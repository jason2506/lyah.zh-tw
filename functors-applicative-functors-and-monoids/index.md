---
layout: page
title: Functor、Applicative Functor 與單子
prev:
    url: functionally-solving-problems
    title: 函數式地解決問題
---

Haskell 是純粹性、高階函數、參數化代數型別、與 typeclass 的結合，其允許我們實作比其它語言更高層級的多型。我們不必思考屬於一個大的型別階層的型別。取而代之的，我們思考型別可以作為什麼、然後以合適的 typeclass 連結它們。一個 `Int` 可以扮演很多東西。它可以扮演一個可以比較相等性的東西、一個有順序的東西、一個可以列舉的東西、等等。

typeclass 是很開放的，這代表我們可以定義我們自己的資料型別，思考它可以作為什麼、並以定義它的行為的 typeclass 來連結它。因為如此、且因為 Haskell 優秀的型別系統──其允許我們僅看一個 function 的型別宣告，就能知道許多關於它的資訊──我們可以定義具有非常一般化與抽象的行為的 typeclass。我們已經看過定義了「檢查兩個值是否相等」、或是「以某種順序比較兩個值的大小」的操作的 typeclass。這些都是非常抽象且確切的行為，但我們不會將它們想成任何特別的東西，因為我們大多數時候都要處理它們。我們最近遇到了 functor，基本上它是可以被 map 的東西。這是個 typeclass 可以描述的有用、但仍然十分抽象的屬性的例子。在這一章，我們要仔細看看 functor，以及稍微強一點、且更有用的 functor 版本，叫作 applicative functor。我們也會看看單子（monoid），它有點像是保險櫃。

## <a name="functors-redux">回歸 Functor</a>

<img src="img/frogtor.png" alt="frogs dont even need money" style="float:right" />
我們已經在 functor [它們自己的小節](making-our-own-types-and-typeclasses#the-functor-typeclass)談過它們了。如果你不曾讀過它，你現在、或者在之後，你有更多時間時，可能需要去看一看。不然你可以假裝你讀過它了。

儘管如此，這裡有個快速的回顧：functor 為可以被映射的東西，像是 list、`Maybe`、tree、等等。在 Haskell 中，它們為 typeclass `Functor` 所描述，它只有一個 typeclass method，即 `fmap`，其型別為 `fmap :: (a -> b) -> f a -> f b`。它表明：給我一個接收一個 `a` 並回傳一個 `b` 的 function、與一個裝著一個（或多個） `a` 的盒子，我將會給你一個裝著一個（或多個） `b` 的盒子。它就像是將 function 應用到盒子中的元素一樣。

<p class="hint">
<em>一點建議。</em>許多時候，盒子類比是用以幫助你得到一些 functor 如何運作的感覺，之後我們可能會為 applicative functor 與單子使用相同的類比。它是個幫助人們瞭解 functor 的不錯的類比，但別把它當真了，因為對於某些 functor，盒子類比必須被過度延伸以符合某些事實。一個代表 functor 的更加正確的詞彙會是計算語境（computational context）。這個語境可能是「計算可能有個值、或者它可能會失敗（<code>Maybe</code> 或是 <code>Either a</code>）」、或是「這裡可能有更多值（list）」、諸如此類。
</p>

若是我們要讓一個型別建構子為一個 `Functor` 的實體，它的 kind 必須為 `* -> *`，代表它必須恰好取一個具體型別作為型別參數。舉例來說，`Maybe` 可以作為一個實體，因為它取一個型別參數以產生一個具體型別，像是 `Maybe Int` 與 `Maybe String`。若是一個型別建構子接收兩個參數，像是 `Either`，我們就必須部分應用型別建構子，直到它只取一個型別參數為止。所以我們不能寫 `instance Functor Either where`，但可以寫 `instance Functor (Either a) where`，然後若是我們想像 `fmap` 只能處理 `Either a`，它的型別宣告就會是 `fmap :: (b -> c) -> Either a b -> Either a c`。如你所見，`Either a` 這個部分是固定的，因為 `Either a` 只接收一個型別參數、而 `Either` 取兩個，所以 `fmap :: (b -> c) -> Either b -> Either c` 就非常不合理。

我們現在已經知道了這麼多為 `Functor` 實體的型別（嗯，其實是型別建構子），像是 `[]`、`Maybe`、`Either a`、與我們自己建立的 `Tree` 型別。我們看過我們能夠如何將 function 映射到它們。在這一節，我們要再多看看兩個 functor 的實體，即 `IO` 與 `(->) r`.

若是某個值的型別為 `IO String`，代表它是個 I/O 動作，其在執行時將會走到真實世界、並為我們取得它將會作為結果產生的字串。我們可以在 <i>do</i> 語法中使用 `<-` 以將這個結果綁定到一個名稱上。我們提過 I/O 動作像是一個長著小腳的盒子走出去、並為我們從外界取回一些值。我們可以檢查它們取回了什麼，但檢查之後，我們必須將這個值包回 `IO` 中。藉由思考這個長著小腳的盒子類比，我們可以發現 `IO` 的行為有多麼像一個 functor。

讓我們看看 `IO` 是怎麼樣的一個 `Functor` 實體。當我們將一個 function `fmap` 到一個 I/O 動作時，我們要取回一個做相同事情、但將我們的 function 應用到其結果值的 I/O 動作。

<pre name="code" class="haskell:hs">
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
</pre>

映射某個東西到 I/O 動作的結果將會是一個 I/O 動作，所以我們立刻使用 <i>do</i> 語法以結合兩個動作、並建立一個新的動作。在 `fmap` 的實作中，我們建立了一個新的 I/O 動作，其先執行原本的 I/O 動作、然後將它的結果稱作 `result`。接著，我們執行 `return (f result)`。如你所知，`return` 為一個建立一個除了提供某值作為它的結果之外、不會做任何事的 I/O 動作的 function。一個 <i>do</i> 區塊產生的動作，總是會得到它最後一個動作的結果值。這就是為什麼我們使用要 `return` 來建立一個實際上不做任何事、僅提供 `f result` 作為新的 I/O 動作結果的 I/O 動作。

我們可以試試它，以得到一點感覺。它其實十分簡單。看看這段程式碼：

<pre name="code" class="haskell:hs">
main = do line <- getLine
          let line' = reverse line
          putStrLn $ "You said " ++ line' ++ " backwards!"
          putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"
</pre>

使用者被提示輸入一行，而我們將它丟回給使用者，只不過是反轉過的。以下是如何使用 `fmap` 來改寫它：

<pre name="code" class="haskell:hs">
main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards!"
          putStrLn $ "Yes, you really said" ++ line ++ " backwards!"
</pre>

<img src="img/alien.png" alt="w00ooOoooOO" style="float:left" />
就像是我們將 `reverse` `fmap` 到 `Just "blah"` 時，會得到 `Just "halb"`，我們也可以將 `reverse` `fmap` 到 `getLine`。`getLine` 是一個型別為 `IO String` 的 I/O 動作，而將 `reverse` 映射到它，會給我們一個將會走到真實世界、取得一行、然後將 `reverse` 應用到其結果的 I/O 動作。像是我們可以將一個 function 應用到一個 `Maybe` 盒子中的某個東西，我們也可以將一個 function 應用到一個 `IO` 盒子中的東西，只是它必須走到真實世界以取得某值。當我們使用 `<-` 將它綁定到一個名稱時，這個名稱將會反映這個已經應用過 `reverse` 的結果。

`fmap (++"!") getLine` 這個 I/O 動作的行為就像是 `getLine`，只是它的結果永遠有個附加於它的 `"!"`！

如果我們看看 `fmap` 被限制在 `IO` 時的型別，它將會是 `fmap :: (a -> b) -> IO a -> IO b`。`fmap` 取一個 function 與一個 I/O 動作、並回傳一個新的 I/O 動作，它就像是原先的那個動作、只是 function 會被應用到它所包含的結果。

如果你曾發覺，你將一個 I/O 動作的結果綁定到一個名稱上，只是為了要應用一個 function、然後以其它什麼東西呼叫它，就考慮使用 `fmap` 吧，因為它看起來更漂亮。若是你想要將多個轉換應用到在一個 functor 之中的某些資料，你可以在最高層級宣告你自己的 function、建立一個 lambda function、或者比較理想地，使用複合函數：

<pre name="code" class="haskell:hs">
import Data.Char
import Data.List

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line
</pre>

<pre name="code" class="plain">
$ runhaskell fmapping_io.hs
hello there
E-R-E-H-T- -O-L-L-E-H
</pre>

大概正如你所知的，`intersperse '-' . reverse . map toUpper` 為一個接收一字串、對其映射 `toUpper`、應用 `reverse` 到其結果、然後應用 `intersperse '-'` 到其結果的 function。它就像是寫下 `(\xs -> intersperse '-' (reverse (map toUpper xs)))`，只是更漂亮。

另一個我們一直在處理、但沒有認出它是一個 `Functor` 的 `Functor` 實體為 `(->) r`。你現在可能有點混亂，因為 `(->) r` 到底代表什麼鬼東西？function 型別 `r -> a` 可以被改寫成 `(->) r a`，很像是我們可以將 `2 + 3` 寫成 `(+) 2 3`。當我們以 `(->) r a` 來看它時，我們能夠以略微不同的觀點來看看 `(->)`，因為我們發現它就像 `Either`，是一個接收兩個型別參數的型別建構子。但要記得，我們說過一個型別建構子必須恰好取一個型別參數，以讓它能作為 `Functor` 的實體。這就是為什麼我們無法讓 `(->)` 為 `Functor` 的實體，但若是我們將它部分應用成 `(->) r`，它就不會產生任何問題。若是語法允許型別建構子以 section 被部分應用（像是我們可以藉由 `(2+)` 來部分應用 `+`，其與 `(+) 2` 相同），你可以將 `(->) r` 寫成 `(r ->)`。function functor 如何呢？嗯，讓我們看看 `Control.Monad.Instances` 中的實作：

<p class="hint">
我們通常會將接收任何值、並回傳任何值的 function 記作 <code>a -> b</code>。<code>r -> a</code> 是一樣的東西，我們只是使用了不同的字母來代表型別變數。
</p>

<pre name="code" class="haskell:hs">
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))
</pre>

若是語法允許的話，它可以被寫成：

<pre name="code" class="haskell:hs">
instance Functor (r ->) where
    fmap f g = (\x -> f (g x))
</pre>

但它不允許，所以我們必須以前者的方式來寫它。

首先，讓我們想想 `fmap` 的型別。其為 `fmap :: (a -> b) -> f a -> f b`。現在我們將要做的，是在心中將所有的 `f`──其為我們 functor 實體所扮演的角色──取代為 `(->) r`。我們要這樣做，以看看 `fmap` 對於這個特定的實體會如何表現。我們得到了 `fmap :: (a -> b) -> ((->) r a) -> ((->) r b)`。現在我們該做的，是將 `(->) r a` 與 `(-> r b)` 寫作前綴的 `(->) r a` 與 `(-> r b)`，像是我們通常會為 function 而做的。我們現在得到的是 `fmap :: (a -> b) -> (r -> a) -> (r -> b)`。

唔，好。將一個 function 映射到一個 function 必須產生一個 function，就像是將一個 function 映射到一個 `Maybe` 必須產生一個 `Maybe`、且將一個 function 映射到一個 list 必須產生一個 list。代表這個實體的型別 `fmap :: (a -> b) -> (r -> a) -> (r -> b)` 告訴了我們什麼呢？嗯，我們看到它接收一個從 `a` 到 `b` 的 function、與一個從 `r` 到 `a` 的 function，並回傳一個從 `r` 到 `b` 的 function。這有讓你回想起任何東西嗎？是的！複合函數！將  `r -> a` 的輸出導向到 `a -> b` 的輸入，以得到一個 function `r -> b`，這正好就是複合函數。若是你看看上面這個實體是如何被定義的，你會發現它僅是個複合函數。另一種撰寫這個實體的方式會是：

<pre name="code" class="haskell:hs">
instance Functor ((->) r) where
    fmap = (.)
</pre>

這顯示了，對 function 使用 `fmap` 顯然就是個複合操作。執行 `:m + Control.Monad.Instances`，因為這是實體被定義之處，然後試著映射到 function。

<pre name="code" class="haskell:ghci">
ghci> :t fmap (*3) (+100)
fmap (*3) (+100) :: (Num a) => a -> a
ghci> fmap (*3) (+100) 1
303
ghci> (*3) `fmap` (+100) $ 1
303
ghci> (*3) . (+100) $ 1
303
ghci> fmap (show . (*3)) (*100) 1
"300"
</pre>

我們可以將 `fmap` 作為一個前綴 function 呼叫，以讓它與 `.` 的相似之處顯而易見。在第二行輸入中，我們將 `(*3)` 映射到 `(+100)`，這會產生一個 function，其會取一個輸入、對它 `(+100)`、然後對這個結果呼叫 `(*3)`。我們以 `1` 呼叫這個 function。

這裡盒子類比還適用嗎？嗯，適用的，如果你要曲解它的話。當我們對 `Just 3` 使用 `fmap (+3)` 時，將 `Maybe` 想像成一個擁有一些我們要應用 function `(+3)` 的內容是很容易的。但當我們做 `fmap (*3) (+100)` 的時候呢？嗯，你可以將 function `(+100)` 想成一個包含它最終結果的盒子。有點像是一個 I/O 動作可以被想成一個將會走到真實世界、並取得一些結果的盒子。將 `fmap (*3)` 使用在 `(+100)` 會建立另一個行為像是 `(+100)` 的 function，只是在產出結果之前，`(*3)` 會被應用到這個結果。現在我們可以發現，`fmap` 對於 function 表現得就像是 `.`。

實際上，`fmap` 在使用到 function 時是一個複合函數，這點在當前並不是非常有用，但至少它非常有趣。
它也稍微扭轉了我們的想法，並讓我們看到行為比起盒子更像計算的東西（`IO` 與 `(->) r`）為何是個 functor。function 被映射到一個計算結果，與這個計算的結果被 function 所修改相同。

<img src="img/lifter.png" alt="lifting a function is easier than lifting a million pounds" style="float:right" />
在我們繼續到 `fmap` 該遵循的規則之前，讓我們再一次想想 `fmap` 的型別。它的型別為 `fmap :: (a -> b) -> f a -> f b`。我們漏掉了類別限制 `(Functor f) =>`，但我們在這裡為求簡潔而省略它，因為我們不管怎樣都在談 functor，所以我們知道 `f` 代表什麼。在我們首次學到 [curried functions](higher-order-functions#curried-functions) 時，我們說過所有的 Haskell function 實際上都取一個參數。一個 function `a -> b -> c` 實際上僅取一個型別 `a` 的參數、然後回傳一個 function `b -> c`，其取一個參數、並回傳一個 `c`。若是我們以太少的參數呼叫一個 function（即，部分應用它），我們就會取回一個接收數個我們遺漏的參數的 function（若是我們再次將 function 想成接收多個參數的話）。所以 `a -> b -> c` 可以被寫作 `a -> (b -> c)`，以讓 curry 更明顯。

同樣的，若是我們寫 `fmap :: (a -> b) -> (f a -> f b)`，我們可以不將 `fmap` 想成一個接收一個 function 與一個 functor、並回傳一個 functor 的 function，而是想成一個接收一個 function、並回傳一個新 function，它就像是原來的那個 function，只是它取一個 functor 作為參數、並回傳一個 functor 作為結果。它取一個 `a -> b` function，並回傳一個 function `f a -> f b`。這被稱為 lift 一個 function。讓我們使用 GHCI 的 `:t` 命令來試試這個概念：

<pre name="code" class="haskell:ghci">
ghci> :t fmap (*2)
fmap (*2) :: (Num a, Functor f) => f a -> f a
ghci> :t fmap (replicate 3)
fmap (replicate 3) :: (Functor f) => f a -> f [a]
</pre>

`fmap (*2)` 這個 expression 是一個取一個基於數字的 functor `f`、並回傳一個基於數字的 functor 的 function。這個 functor 可以是一個 list、一個 `Maybe`、一個 `Either String`、諸如此類。`fmap (replicate 3)` 這個 expression 會取一個基於任何型別的 functor、並回傳一個基於一個基於這種型別的元素的 list 的 functor。

<p class="hint">
當我們說<i>一個基於數字的 functor</i> 的時候，你可以將它想成<i>一個在其中擁有數字的 functor</i>。前者稍微花俏一點、且更為技術正確（technically correct），但後者通常比較容易理解。
</p>

如果我們部分應用 `fmap (++"!")`、然後在 GHCI 中將它綁定到一個名稱，這點甚至會更加明顯。

你可以將 `fmap` 想成一個接收一個 function 與一個 functor、然後將這個 function 映射到 functor 的 function，或者將它想成一個接收一個 function、並 lift 這個 function，以讓它操作 functor 的 function。兩種觀點在 Haskell 中都是正確且等價的。

`fmap (replicate 3) :: (Functor f) => f a -> f [a]` 這個型別代表這個 function 會運作於任何的 functor。它確切會做什麼，則視我們所使用的 functor 為何者而定。若是我們對一個 list 使用 `fmap (replicate 3)` ，list 為 `fmap` 的實作就會被選擇，其即是 `map`。若是我們將它使用在 `Maybe a`，它就會將 `replicate 3` 應用到 `Just` 之中的值、或若是它為 `Nothing`，則它會維持 `Nothing`。

<pre name="code" class="haskell:ghci">
ghci> fmap (replicate 3) [1,2,3,4]
[[1,1,1],[2,2,2],[3,3,3],[4,4,4]]
ghci> fmap (replicate 3) (Just 4)
Just [4,4,4]
ghci> fmap (replicate 3) (Right "blah")
Right ["blah","blah","blah"]
ghci> fmap (replicate 3) Nothing
Nothing
ghci> fmap (replicate 3) (Left "foo")
Left "foo"
</pre>

接下來，我們要看看 *functor 原則*。為了要讓某值為一個 functor，它需要滿足一些原則。所有的 functor 都被預期要展現出某種像是 functor 的屬性與行為。它們應該要如同可以被映射的東西一樣可靠地運作。對一個 functor 呼叫 `fmap` 應該僅將一個 function 映射到 functor，僅此而已。這個行為被描述在 functor 原則之中。還有兩個所有 `Functor` 實體都該遵守的原則。它們並非由 Haskell 自動強制，所以你必須自己測試它們。

*第一條 functor 原則聲明，若是我們將 `id` function 映射到一個 functor，我們取回的 functor 應該要與原來的 functor 相同。*若是我們要將它寫得稍微正式一點，它代表 <code class="label law">fmap id = id</code>。所以本質上，這代表若是我們對一個 functor 進行 `fmap id`，它應該要與對 functor 呼叫 `id` 相同。記住，`id` 為 identity function，其直接回傳它未被修改的參數。它也可以被寫作 `\x -> x`。若是我們將 functor 看成某個可以被映射的東西， <code class="label law">fmap id = id</code> 原則看上去就很明顯了。

讓我們看看這個原則對於一些 functor 的值是否成立。

<pre name="code" class="haskell:ghci">
ghci> fmap id (Just 3)
Just 3
ghci> id (Just 3)
Just 3
ghci> fmap id [1..5]
[1,2,3,4,5]
ghci> id [1..5]
[1,2,3,4,5]
ghci> fmap id []
[]
ghci> fmap id Nothing
Nothing
</pre>

若是我們看看 `fmap` 為 `Maybe` 的實作，我們可以發現為什麼第一 functor 原則成立。

<pre name="code" class="haskell:hs">
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
</pre>

我們想像 `id` 在實作中扮演著 `f` 參數的角色。我們發現若是我們將 `fmap id` 映射到 `Just x`，結果將會是 `Just (id x)`，且因為 `id` 僅回傳它的參數，所以我們可以推論 `Just (id x)` 等於 `Just x`。所以現在我們知道，若是我們將 `id` 映射到一個 `Just` 值建構子的 `Maybe` 值，我們就會取回相同的值。

理解將 `id` 映射到一個 `Nothing` 值會回傳相同的值是很直觀的。所以從這兩個 `fmap` 實作中的等式，我們發現原則 `fmap id = id` 成立。

<img src="img/justice.png" alt="justice is blind, but so is my dog" style="float:left" />
*第二條原則聲明，組合兩個 function、然後將產生的 function 映射到一個 functor，應該與先將一個 function 映射到 functor、然後映射另一個 function 相同。*正式地寫，其代表 <code class="label law">fmap (f . g) = fmap f . fmap g</code>。或者以另一種方式來寫，對於任意 functor <i>F</i>，下式應該成立： <code class="label law">fmap (f . g) F = fmap f (fmap g F)</code>。

若是我們可以證明某個型別遵守這兩個 functor 原則，在涉及映射時，我們就可以依靠它與其它 functor 相同的基礎行為。我們可以知道，當我們對它使用 `fmap` 時，背後不會有任何映射以外的事情發生，且它會作為一個可以被映射的東西，即 functor。你藉由查看 `fmap` 對於某個型別的實作、然後使用我們用來檢查 `Maybe` 是否遵守第一原則的方法，以理解對於這個型別，第二原則為何成立。

若是你想的話，我們可以檢驗第二 functor 原則對於 `Maybe` 是如何成立的。如果我們對 `Nothing` 執行 `fmap (f . g)`，我們會得到 `Nothing`，因為以任何 function 對 `Nothing` 進行 `fmap` 都回傳 `Nothing`。若是我們執行 `fmap f (fmap g Nothing)`，我們會因為相同的原因得到 `Nothing`。好，看到若是 `Maybe` 為一個 `Nothing` 值時，第二原則對於 `Maybe` 如何成立是非常簡單、幾乎是直觀的。

若是它是個 <code>Just <i>something</i></code> 值怎麼樣呢？嗯，若是我們執行 `fmap (f . g) (Just x)`，我們從實作中看到，它以 `Just ((f . g) x)` 實作，即是 `Just (f (g x))`。若是我們執行 `fmap f (fmap g (Just x))`，我們從實作中看到，`fmap g (Just x)` 為 `Just (g x)`。因此，`fmap f (fmap g (Just x))` 等於 `fmap f (Just (g x))`，並且從實作中，我們看到這等於 `Just (f (g x))`。

如果你有點被這個證明搞混了，別擔心。確定你瞭解[複合函數](higher-order-functions#composition)如何運作。
很多時候，因為這些像是容器或 function 的型別，你可以直覺地理解這些原則為何成立。你也能夠以一個型別的不同值來測試它們，並能夠稍微肯定地宣稱，一個型別確實遵守原則。

讓我們看看一個型別建構子為一個 `Functor` 的實體，但並非真的為一個 functor，因為它不滿足原則的病態例子。讓我們假設我們有一個型別：

<pre name="code" class="haskell:hs">
data CMaybe a = CNothing | CJust Int a deriving (Show)
</pre>

這裡的 C 代表計數器。它是個看起來非常像 `Maybe a` 的資料型別，只是 `Just` 部分持有兩個欄位、而非一個。在 `CJust` 值建構子中的第一個欄位永遠是個 `Int` 的型別，且它將會是某種計數器；第二個欄位為型別 `a`，其來自於型別參數，而它的型別當然會視我們為 `CMaybe a` 所選的具體型別而定。讓我們試試我們的新型別以得到一些感覺。

<pre name="code" class="haskell:ghci">
ghci> CNothing
CNothing
ghci> CJust 0 "haha"
CJust 0 "haha"
ghci> :t CNothing
CNothing :: CMaybe a
ghci> :t CJust 0 "haha"
CJust 0 "haha" :: CMaybe [Char]
ghci> CJust 100 [1,2,3]
CJust 100 [1,2,3]
</pre>

若是我們使用 `CNothing` 建構子就沒有欄位；而若是我們使用 `CJust` 建構子，第一個欄位為一個整數、且第二個欄位可以是任意型別。讓我們令它為 `Functor` 的實體，使得每次我們使用 `fmap` 時，讓 function 被應用到第二個欄位，而第一個欄位則增加 1。

<pre name="code" class="haskell:hs">
instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)
</pre>

這有點像是 `Maybe` 的實體實作，除了當我們對一個不代表空盒子的值（一個 `CJust` 值）執行 `fmap` 的時候，我們不僅將 function 應用到內容，我們也將計數器增加 1。到目前為止的每件事看來都很好，我們甚至可以稍微試試看：

<pre name="code" class="haskell:ghci">
ghci> fmap (++"ha") (CJust 0 "ho")
CJust 1 "hoha"
ghci> fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))
CJust 2 "hohahe"
ghci> fmap (++"blah") CNothing
CNothing
</pre>

這遵守 functor 原則嗎？為了看看某值是否遵守一條原則，只要找出一個反例就足夠了。

<pre name="code" class="haskell:ghci">
ghci> fmap id (CJust 0 "haha")
CJust 1 "haha"
ghci> id (CJust 0 "haha")
CJust 0 "haha"
</pre>

阿！我們知道第一個 functor 原則聲明，若是我們將 `id` 映射到一個 functor，它應該要與以相同的 functor 呼叫 `id` 相同，但如同我們在這個例子中看到的，這對於我們的 `CMaybe` functor 並不為真。它不遵守 functor 原則，即使它是 `Functor` typeclass 的一員，它也不是一個 functor。若是某人將我們的 `CMaybe` 作為一個 functor 使用，它們會預期它像個好 functor 一樣遵守 functor 原則。但即使 `CMaybe` 自稱是一個 functor，它作為一個 functor 也失格了，所以將它用作一個 functor 可能會導致一些有缺陷的程式碼。當我們使用一個 functor 時，若是我們先組合一些 function、然後將它們映射到 functor，或者我們僅連續將每個 function 映射到一個 functor 都不該有關係。但使用 `CMaybe` 卻有關係，因為它記錄了它被映射的次數。一點也不好！若是我們想讓 `CMaybe` 遵守 functor 原則，我們就必須在我們使用 `fmap` 時，令 `Int` 欄位維持不變。

首先，functor 原則看起來可能有點令人困擾、且不必要，但若是我們知道一個型別遵守這兩個原則，我們就可以做出某些它會如何行為的假設。若是一個型別遵守 functor 原則，我們就知道對這個型別的值呼叫 `fmap` 只會將 function 映射到它，僅此而已。這使得程式碼更加抽象且可擴充，因為我們可以使用原則來推論任何 functor 都該有的行為，並建立可靠地操作任意 functor 的 function。

所以在標準函式庫中的 `Functor` 都遵守這些原則，但若是你不相信我，你可以自己去檢查。在你下一次令一個型別為 `Functor` 的實體時，花一分鐘確保它遵守 functor 原則。一旦你處理過夠多的 functor，你就能稍微直觀地看出它們共有的屬性與行為，並且直觀地看出一個型別是否遵守 functor 原則並不困難。但即使沒有直覺，你還是可以一行接著一行掃過實作，並看看原則是否成立、或是試著找出一個反例。

我們也可以將 functor 看作在一個語境中輸出值的東西。舉例來說，`Just 3` 在「它可能或不可能輸出任何值」的語境中會輸出 `3`。`[1,2,3]` 在「這裡可能有更多值、或沒有值」的語境中會輸出三個值──`1`、`2`、與 `3`。`(+3)` 這個 function 會輸出一個值，視給它的參數為何者而定。

若是你將 functor 想成輸出值的東西，你可以將映射到 functor 想成對 functor 的輸出附加一個改變值的轉換。當我們執行 `fmap (+3) [1,2,3]` 時，我們將轉換 `(+3)` 附加到 `[1,2,3]` 的輸出，所以無論我們何時看到 list 輸出的數字，`(+3)` 都會被應用到它。另一個例子是映射 function。當我們執行 `fmap (+3) (*3)` 時，我們將轉換 `(+3)` 附加到 `(*3)` 最終的輸出上。以這種方式來看它，給了我們一些為何將 `fmap` 使用在 function 只是個計算（`fmap (+3) (*3)` 等於 `(+3) . (*3)`，其等同於 `\x -> ((x*3)+3)`）的感覺，因為我們取一個像是 `(*3)` 的 function，接著我們將轉換 `(+3)` 附加到它的輸出。結果仍然是一個 function，只是當我們給它一個數字時，它會被乘以三、然後它會通過附加的轉換、其中它會被加上三。這就是使用複合操作所做的了。

## <a name="applicative-functors">Applicative functor</a>

<img src="img/present.png" style="float:right" alt="disregard this analogy" />
在這一節，我們要看看在 Haskell 中，以 `Control.Applicative` 模組的 `Applicative` typeclass 表示的 applicative functor，其改進了 functor。

如你所知，在 Haskell 中的 function 預設是被 curry 的，這代表一個看起來取多個參數的 function，實際上只取一個參數、並回傳一個取下一個參數的 function，以此類推。若是一個 function 的型別為 `a -> b -> c`，我們通常會說，它取兩個參數、並回傳一個 `c`，但它實際上是取一個 `a`、並回傳一個 function `b -> c`。這就是為什麼我們能夠將一個 function 以 `f x y` 或者 `(f x) y` 呼叫。這個機制讓我們藉由僅以較少的參數呼叫 function 來部分應用它們，這會產生我們接著能夠傳遞到其它 function 的 function。

到目前為止，當我們將 function 映射到 functor 時，我們通常會映射只取一個參數的 function。但當我們將一個像是 `*` 的 function──其取兩個參數──映射到一個 functor 時會發生什麼事呢？讓我們看看幾個這種情況的具體例子。若是我們有個 `Just 3`，並執行 `fmap (*) (Just 3)`，我們會得到什麼？從 `Maybe` 為 `Functor` 的實作，我們知道若是它是個 <code>Just <i>something</i></code> 值，它就會將 function 應用到 `Just` 之中的 <code><i>something</i></code>。於是，執行 `fmap (*) (Just 3)` 會產生 `Just ((*) 3)`。若是我們使用 section，這也可以被寫成 `Just (* 3)`。有趣！我們得到了一個包在一個 `Just` 中的 function！

<pre name="code" class="haskell:ghci">
ghci> :t fmap (++) (Just "hey")
fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])
ghci> :t fmap compare (Just 'a')
fmap compare (Just 'a') :: Maybe (Char -> Ordering)
ghci> :t fmap compare "A LIST OF CHARS"
fmap compare "A LIST OF CHARS" :: [Char -> Ordering]
ghci> :t fmap (\x y z -> x + y / z) [3,4,5,6]
fmap (\x y z -> x + y / z) [3,4,5,6] :: (Fractional a) => [a -> a -> a]
</pre>

若是我們將 `compare`──其型別為 `(Ord a) => a -> a -> Ordering`──映射到一個字元 list，我們就得到一個型別為 `Char -> Ordering` 的 function 的 list，因為 `compare` 這個 function 被 list 中的字元部分應用。它不是個 `(Ord a) => a -> Ordering` function 的 list，因為被應用的第一個 `a` 是個 `Char`，所以第二個 `a` 的型別就必定為 `Char`。

我們看到藉由將「多參數」function 映射到 functor，我們得到了在其中包含 function 的 functor。所以我們現在能用它們做什麼呢？首先，我們可以將取這些 function 作為參數的 function 映射到它們，因為無論在 functor 中的是什麼，都會被給予我們作為參數映射的 function。

<pre name="code" class="haskell:ghci">
ghci> let a = fmap (*) [1,2,3,4]
ghci> :t a
a :: [Integer -> Integer]
ghci> fmap (\f -> f 9) a
[9,18,27,36]
</pre>

但若是我們有個 `Just (3 *)` 的 functor 值、以及一個 `Just 5` 的 functor 值，而我們想要從 `Just (3 *)` 取出 function，並將它映射到 `Just 5` 呢？以一般的 functor 來說，我們不大幸運，因為它們支援的只有將一般的 function 映射到現有的 functor。即使在我們將 `\f -> f 9` 映射到一個在其中包含 function 的 functor 時，我們也只是將一個普通的 function 映射到這個 functor。但我們無法以 `fmap` 提供給我們的功能，將一個 functor 之中的 function 映射到另一個 functor。我們可以對 `Just` 建構子模式匹配以取出其中的 function、然後將它映射到 `Just 5`，但我們要尋找一個更通用、且抽象的方法來做到這件事，其適用於各種 functor。

看看 `Applicative` typeclass。它位在 `Control.Applicative` 模組，且它定義了兩個 method，`pure` 與 `<*>`。它不為任何一個 method 提供預設實作，所以若是我們要讓某個東西作為一個 applicative functor，我們必須定義它們。class 像這樣被定義：

<pre name="code" class="haskell:hs">
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
</pre>

這三行簡單的 class 定義告訴我們很多事！讓我們從第一行開始。它開始了 `Applicative` class 的定義，它也引入了一個類別限制。它表明，若是我們想要令一個型別建構子為 `Applicative` typeclass 的一員，它必須先在 `Functor` 中。這就是為什麼我們知道，若是一個型別建構子為 `Applicative` typeclass 的一員，它就也在 `Functor` 中，所以我們可以對它使用 `fmap`。

它定義的第一個 method 叫做 `pure`。它的型別宣告為 `pure :: a -> f a`。在這裡 `f` 扮演著我們的 applicative functor 的角色。因為 Haskell 有個非常棒的型別系統，且因為一個 function 能做的只有取一些參數、並回傳一些值，所以我們可以從一個型別宣告判斷很多事，毫無例外。`pure` 需要取一個任何型別的值，並回傳一個在其中包含這個值的 applicative functor。當我們說<i>在其中（inside it）</i>時，我們再次使用了盒子類比，即使我們發現它並不總是經得起推敲。但 `a -> f a` 型別宣告仍然是十分具有描述性的。我們取一個值、並將它包在一個 applicative functor 中，它在其中擁有這個作為結果的值。

一個思考 `pure` 的更好方式，是假定它取一個值、並將它放在某種預設（或是純粹的）情境──一個仍然產生這個值的最小情境──之中。

`<*>` function 真的很有趣。它的型別宣告為 `f (a -> b) -> f a -> f b`。這有讓你回想起任何東西嗎？當然，`fmap :: (a -> b) -> f a -> f b`。它是一種改良的 `fmap`。`fmap` 取一個 function 與一個 functor，並將 function 應用到 functor 之中，而 `<*>` 則取一個在其中擁有一個 function 的 functor、與另一個 functor，並從第一個 functor 擷取這個 function，然後將它映射到第二個 functor。當我說擷取（extract）時，我所指的實際上是執行然後擷取，或許甚至要<i>串接（sequence）</i>。我們不久就會看到為什麼。

讓我們看看 `Applicative` 為 `Maybe` 的實體實作。

<pre name="code" class="haskell:hs">
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something
</pre>

再一次的，我們從 class 定義看到扮演著 applicative functor 角色的 `f` 應該取一個具體型別作為參數，所以我們寫成 `instance Applicative Maybe where`，而非 `instance Applicative (Maybe a) where`。

首先，`pure`。我們先前說過，它應該取某個值、並將它包在一個 applicative functor 中。我們寫了 `pure = Just`，因為像 `Just` 這種值建構子都是一般的 function。我們也可以寫成 `pure x = Just x`。

接下來，我們有 `<*>` 的定義。我們無法從一個 `Nothing` 取出一個 function，因為在它之中並沒有 function。所以我們表明，若是我們試著從一個 `Nothing` 擷取一個 function，那麼結果就是一個 `Nothing`。若是你看到 `Applicative` 的 class 定義，你會發現這裡有個 `Functor` 型別限制，這代表我們可以假設 `<*>` 的兩個參數皆為 functor。若是第一個參數不是個 `Nothing`，而是一個在其中有某個 function 的 `Just`，我們就指定：我們這時要將這個 function 映射到第二個參數。這也考量到了第二個參數為 `Nothing` 的情況，因為以任何 function 對 `Nothing` 執行 `fmap` 都會回傳一個 `Nothing`。

所以對於 `Maybe`，若是左值是一個 `Just`，`<*>` 就從中擷取 function，並將它映射到右值。若是任何參數為 `Nothing`，`Nothing` 就是結果。

好，非常棒。讓我們來試試看。

<pre name="code" class="haskell:ghci">
ghci> Just (+3) <*> Just 9
Just 12
ghci> pure (+3) <*> Just 10
Just 13
ghci> pure (+3) <*> Just 9
Just 12
ghci> Just (++"hahah") <*> Nothing
Nothing
ghci> Nothing <*> Just "woot"
Nothing
</pre>

我們看到在這種情況中，執行 `pure (+3)` 與 `Just (+3)` 是相同的。若是你要在一個 applicative 的情境中（即，以 `<*>` 使用它）處理 `Maybe` 值就使用 `pure`，除此之外就維持 `Just` 吧。前四行輸入顯示了 function 是如何被擷取、然後映射，但在這種情況中，它可以僅由將未被封裝的 function 映射到 functor 來達成。最後一行很有趣，因為我們試著從一個 `Nothing` 取出一個 function、然後將它映射到某值，這當然會產生一個 `Nothing`。

以一般的 functor，你可以僅將一個 function 映射到一個 functor，但你無法以任何通用的方式來取出結果，即使結果為一個部分應用的 function。在另一方面，applicative functor 則允許你以單一個 function 操作多個 functor。看看這段程式碼：

<pre name="code" class="haskell:ghci">
ghci> pure (+) <*> Just 3 <*> Just 5
Just 8
ghci> pure (+) <*> Just 3 <*> Nothing
Nothing
ghci> pure (+) <*> Nothing <*> Just 5
Nothing
</pre>

<img src="img/whale.png" alt="whaale" style="float:right" />
這是怎麼回事？讓我們一步接著一步來看看。`<*>` 為左結合，這代表 `pure (+) <*> Just 3 <*> Just 5` 等同於 `(pure (+) <*> Just 3) <*> Just 5`。首先，`+` function 被放進一個 functor 中，這在這種情況中是一個包含 function 的 `Maybe` 值。所以首先，我們有 `pure (+)`，即為 `Just (+)`。接著，進行 `Just (+) <*> Just 3`。它的結果為 `Just (3+)`。這是因為部分應用的緣故。只將 `3` 應用到 `+` function 會產生一個取一個參數並加上 3 的 function。最後，`Just (3+) <*> Just 5` 被執行了，其會產生一個 `Just 8`。

這不是很棒嗎？！applicative functor 與 applicative style 地執行 `pure f <*> x <*> y <*> ...`，允許我們取一個預期參數未必被包在 functor 中的 function，並使用這個 function 來操作多個在 functor 情境中的值。這個 function 可以取我們想要的那麼多參數，因為它總是在 `<*>` 之間，一步接著一步地被部分應用。

若是我們考量到 `pure f <*> x` 等於 `fmap f x` 的事實，這會變得更加便利且明顯。這是其中一個 applicative 原則。我們會在之後更仔細地看看它們，但現在，我們可以稍微直覺地將它看作是這樣的東西。思考它，它很合理。像是我們之前說的，`pure` 將一個值放進一個預設情境中。若是我們僅將一個 function 放進一個預設情境中、然後擷取並應用它到一個在另一個 applicative functor 之中的值，我們做的與僅將這個 function 映射到這個 applicative functor 相同。我們可以寫成 `fmap f x <*> y <*> ...`，而非 `pure f <*> x <*> y <*> ...`。這就是為什麼 `Control.Applicative` 要輸出一個叫做 `<$>` 的 function，它僅是一個作為中綴運算子的 `fmap`。它是這樣被定義的：

<pre name="code" class="haskell:hs">
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
</pre>

<p class="hint">
<em>呦！</em>快速提醒：型別變數是與參數名稱、或是其它的值的名稱獨立的。在這裡的 function 宣告中的 <code>f</code> 為一個型別變數，帶著一個表明任何取代 <code>f</code> 的型別建構子都應該在 <code>Functor</code> typeclass 中的類別限制。在 function 主體中的 <code>f</code> 表示一個我們映射到 <code>x</code> 的 function。事實是，我們使用 <code>f</code> 來表示這兩者，並不代表它們在某種層面表示相同的東西。
</p>

藉由使用 `<$>`，applicative style 就十分耀眼了，因為現在若是我們想要將一個 function `f` 應用到三個 applicative functor 之間，我們可以寫成 `f <$> x <*> y <*> z`。若是參數並非 applicative functor，而是一般的值，我們就寫成 `f x y z`。

讓我們更仔細地看看它是如何運作的。我們有個 `Just "johntra"` 的值、與一個 `Just "volta"` 的值，我們想要將它們結合成一個在一個 `Maybe` functor 之中的 `String`。我們這樣做：

<pre name="code" class="haskell:ghci">
ghci> (++) <$> Just "johntra" <*> Just "volta"
Just "johntravolta"
</pre>

在我們看看這是怎麼回事之前，先把上面那行跟這個比較一下：

<pre name="code" class="haskell:ghci">
ghci> (++) "johntra" "volta"
"johntravolta"
</pre>

真棒！要對 applicative functor 使用一般的 function，只要點綴一些 `<$>` 與 `<*>`，這個 function 就會操作 applicative、並回傳一個 applicative。這不是很棒嗎？

總而言之，當我們 `(++) <$> Just "johntra" <*> Just "volta"` 時，`(++)`──其型別為 `(++) :: [a] -> [a] -> [a]`──先被映射到了 `Just "johntra"`，產生一個等同於 `Just ("johntra"++)`、且型別為 `Maybe ([Char] -> [Char])` 的值。注意到 `(++)` 的第一個參數是如何被吃掉、以及 `a` 是如何被轉成 `Char` 的。現在執行 `Just ("johntra"++) <*> Just "volta"`，其從 `Just` 取出 function、並將它映射到 `Just "volta"`，產生 `Just "johntravolta"`。假使這兩個值的任何一個為 `Nothing`，則結果也會是 `Nothing`。

到目前為止，我們在我們的例子中只使用過 `Maybe`，你可能會認為 applicative functor 全都跟 `Maybe` 有關。這裡有一堆其它的 `Applicative` 實體，所以讓我們看看它們吧！

list（實際上是 list 型別建構子，`[]`）為 applicative functor。真令人驚訝！以下是 `[]` 是如何為一個 `Applicative` 實體的：

<pre name="code" class="haskell:hs">
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
</pre>

先前，我們說過 `pure` 取一個值，並將它放進一個預設情境中。或者換句話說，一個仍然產生這個值的最小情境。對於 list 的最小情境為空 list，`[]`，但空 list 代表缺少值，所以它本身不能持有我們使用在 `pure` 的值。這就是為什麼 `pure` 要取一個值、並將它放進一個單一元素的 list 中。同樣的，對於 `Maybe` applicative functor 的最小情境會是個 `Nothing`，但它表示缺少一個值、而非一個值，所以 `pure` 在 `Maybe` 的實體實作中被實作成 `Just`。

<pre name="code" class="haskell:ghci">
ghci> pure "Hey" :: [String]
["Hey"]
ghci> pure "Hey" :: Maybe String
Just "Hey"
</pre>

`<*>` 怎麼樣呢？若是我們看看 `<*>` 在它被限制在只處理 list 時的型別會是什麼，我們會得到 `(<*>) :: [a -> b] -> [a] -> [b]`。它以一個 [list comprehension](starting-out#im-a-list-comprehension) 實作。`<*>` 必須在某種層面上從它的左參數擷取出 function，然後將它映射到右參數。但這裡的情況是，左邊的 list 可能在其中包含零個 function、一個 function、或是多個 function。右邊的 list 可能也持有多個值。這就是為什麼我們要使用 list comprehension 來從兩個 list 取值。我們將每個左 list 中的可能的 function 應用到右 list 的每個可能的值。產生的 list 擁有將左 list 中的 function 應用到右 list 中的值的所有可能組合。

<pre name="code" class="haskell:ghci">
ghci> [(*0),(+100),(^2)] <*> [1,2,3]
[0,0,0,101,102,103,1,4,9]
</pre>

左邊的 list 有三個 function，右邊的 list 有三個值，所以產生的 list 將會有九個元素。每個在左 list 的 function 都被應用到每個在右邊的值。若是我們有個取兩個參數的 function 的 list，我們可以將這些 function 應用到兩個 list。

<pre name="code" class="haskell:ghci">
ghci> [(+),(*)] <*> [1,2] <*> [3,4]
[4,5,5,6,3,4,6,8]
</pre>

因為 `<*>` 是左結合的，所以 `[(+),(*)] <*> [1,2]` 會先執行，產生一個等同於 `[(1+),(2+),(1*),(2*)]` 的 list，因為在左邊的每個 function 都被應用到每個在右邊的值。接著，執行 `[(1+),(2+),(1*),(2*)] <*> [3,4]`，其產生最終的結果。

以 list 使用 applicative style 是很有趣的！看：

<pre name="code" class="haskell:ghci">
ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]
</pre>

再一次，看看我們如何藉由插入適當的 applicative 運算子，以在兩個字串的 applicative functor 之間，使用接收兩個字串的普通 function。

你可以將 list 看成非確定性（non-deterministic）計算。一個像是 `100` 或 `"what"` 的值可以被看作一個只會有一種結果的確定性計算，而一個像是 `[1,2,3]` 的 list 可以被看作一個無法決定它想要的是哪個結果的計算，所以它提供給我們所有可能的結果。所以當你做某個像是 `(+) <$> [1,2,3] <*> [4,5,6]` 這樣的事，你可以將它想成以 `+` 加總兩個非確定性計算，只不過產生了另一個更不確定其結果的非確定計算。

對 list 使用 applicative style，經常是個 list comprehension 的不錯的替代品。在第二章，我們想看看 `[2,5,10]` 與 `[8,10,11]` 的所有可能的乘積，所以我們這樣做：

<pre name="code" class="haskell:ghci">
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]
</pre>

我們從兩個 list 取值，並將一個 function 應用到元素的每個組合之間。這也可以以 applicative style 來做到：

<pre name="code" class="haskell:ghci">
ghci> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]
</pre>

這對我來說看起來比較清楚，因為它更容易看到我們只是在兩個非確定性計算之間呼叫 `*`。若是我們想要這兩個 list 所有大於 50 的可能乘積，我們只要執行：

<pre name="code" class="haskell:ghci">
ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
[55,80,100,110]
</pre>

要理解對於 list，為何 `pure f <*> xs` 等於 `fmap f xs` 是很容易的。`pure f` 僅是 `[f]`，且 `[f] <*> xs` 會將每個在左 list 中的 function 應用到每個在右 list 的值，但這裡只有一個 function 在左 list 中，所以它就像是映射。

我們已經遇過的另一個 `Applicative` 實體是 `IO`。這個實體是這樣被定義的：

<pre name="code" class="haskell:hs">
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)
</pre>

<img src="img/knight.png" alt="ahahahah!" style="float:left" />
由於 `pure` 是將一個值放進一個仍會持有它作為結果的最小情境中，所以 `pure` 僅為 `return` 是很合理的，因為 `return` 正是這麼做的；它建立一個不做任何事的 I/O 動作，它僅產生一些值作為它的結果，但它不會真的做任何像是印到終端機、或是從一個檔案進行讀取這類 I/O 操作。

若 `<*>` 是專門針對 `IO` 的，它的型別將會是 `(<*>) :: IO (a -> b) -> IO a -> IO b`。它會取一個產生一個 function 作為其結果的 I/O 動作、與另一個 I/O 動作，並由這兩個 I/O 動作建立一個新的 I/O 動作，其執行時會先執行第一個 I/O 動作以得到 function、然後執行第二個 I/O 動作以得到值，接著它會產生將這個 function 應用到值的結果作為結果。這裡我們使用 <i>do</i> 語法來實作它。記住，<i>do</i> 語法會取多個 I/O 動作，並將它們結合成一個，這正好就是我們在這裡所做的。

對於 `Maybe` 與 `[]`，我們可以將 `<*>` 想成簡單地從它左邊的參數擷取一個 function，然後將它應用到右邊的參數。對於 `IO`，擷取動作仍然牽涉在其中，但現在我們也有個串接的概念，因為我們取了兩個 I/O 動作，並將它們串接、或是結合成一個。我們必須從第一個 I/O 動作擷取 function，但為了要從一個 I/O 動作擷取結果，它必須被執行。

考慮這個：

<pre name="code" class="haskell:hs">
myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a ++ b
</pre>

這是一個將會提示使用者輸入兩行、並產生串接的這兩行作為它的結果的 I/O 動作。我們藉由將兩個 `getLine` I/O 動作與一個 `return` 結合在一起來達成，因為我們想讓我們新結合成的 I/O 動作持有 `a ++ b` 的結果。另一種撰寫這種功能的方式是使用 applicative style。

<pre name="code" class="haskell:hs">
myAction :: IO String
myAction = (++) <$> getLine <*> getLine
</pre>

我們先前所做的是建立一個 I/O 動作，其將一個 function 應用到其它兩個 I/O 動作的結果之間，而這段程式也是相同的。記得，`getLine` 為一個帶著 `getLine :: IO String` 型別的 I/O 動作。當我們在兩個 applicative functor 之間使用 `<*>` 時，結果就為一個 applicative functor，所以這一切都很合理。

若是我們回歸到盒子類比，我們可以將 `getLine` 想像成一個會走到真實世界、並將一個字串取回給我們的盒子。執行 `(++) <$> getLine <*> getLine` 會建立一個新的、比較大的盒子，它將這兩個盒子送出去以從終端機取得幾行，然後將這兩行的串接作為它的結果呈獻。

`(++) <$> getLine <*> getLine` 這個 expression 的型別為 `IO String`，這代表這個 expression 為一個十分普通的 I/O 動作，就像任何其它的 I/O 動作一樣。它也在其中持有一個結果值，就像是其它的 I/O 動作。這就是為什麼我們可以像這樣做：

<pre name="code" class="haskell:hs">
main = do
    a <- (++) <$> getLine <*> getLine
    putStrLn $ "The two lines concatenated turn out to be: " ++ a
</pre>

若是你曾經發現，你自己將某些 I/O 動作綁定到名稱，然後對它們呼叫某些 function，並使用 `return` 將這作為結果呈獻，就考慮使用 applicative style 吧，因為它可以說是稍微簡潔與精鍊一些。

另一個 `Applicative` 的實體為 `(->) r`，即 function。它除了在 code golf<span class="note">（譯註：<http://codegolf.com/>）</span>之外很少以 applicative style 使用，但它作為 applicative 仍然很有趣，所以讓我們看看 function 實體是如何實作的。

<p class="hint">
如果你被 <code>(->) r</code> 代表什麼給搞混了，就看看先前我們解釋 <code>(->) r</code> 如何作為一個 functor 的那節吧。
</p>

<pre name="code" class="haskell:hs">
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
</pre>

當我們把一個值以一個 `pure` 包進一個 applicative functor 中時，它產生的結果永遠必須是這個值。一個仍然產生這個值作為結果的最小預設情境。這就是為什麼在 function 實體實作中，`pure` 要取一個值，並建立一個忽略其參數、並總是回傳這個值的 function。若是我們看看 `pure` 專門針對 `(->) r` 實體的型別，即為 `pure :: a -> (r -> a)`。

<pre name="code" class="haskell:ghci">
ghci> (pure 3) "blah"
3
</pre>

由於 currying，function application 是左結合的，所以我們可以省略括號。

<pre name="code" class="haskell:ghci">
ghci> pure 3 "blah"
3
</pre>

`<*>` 的實體實作有一點神祕，所以若是我們只看看要如何以 applicative 將 function 用作 applicative functor 是最好的。

<pre name="code" class="haskell:ghci">
ghci> :t (+) <$> (+3) <*> (*100)
(+) <$> (+3) <*> (*100) :: (Num a) => a -> a
ghci> (+) <$> (+3) <*> (*100) $ 5
508
</pre>

以兩個 applicative functor 呼叫 `<*>` 會產生一個 applicative functor，所以若是我們對兩個 function 使用它，我們會取回一個 function。所以這裡是怎麼回事？當我們執行 `(+) <$> (+3) <*> (*100)` 時，我們建立了一個將會對 `(+3)` 與 `(*100)` 的結果使用 `+`、並回傳結果的 function。以實際的例子說明，當我們執行 `(+) <$> (+3) <*> (*100) $ 5` 時，`5` 先被應用到 `(+3)` 與 `(*100)`，產生 `8` 與 `500`。接著，以 `8` 與 `500` 呼叫 `+`，產生 `508`。

<pre name="code" class="haskell:ghci">
ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
[8.0,10.0,2.5]
</pre>

<img src="img/jazzb.png" alt="SLAP" style="float:right" />
這裡也相同。我們建立了一個將會以 `(+3)`、`(*2)` 與 `(/2)` 的最終結果呼叫 function `\x y z -> [x,y,z]` 的 function。`5` 被餵給了這三個 function，然後以這些結果呼叫 `\x y z -> [x, y, z]`。

你可以將 function 想成一個包含其最終結果的盒子，所以執行 `k <$> f <*> g` 會建立一個將會以 `f` 與 `g` 的最終結果呼叫 `k` 的 function。當我們進行像是 `(+) <$> Just 3 <*> Just 5` 時，我們對可能在或可能不在這裡的值使用 `+`，其也會產生一個可能在或可能不在這裡的值。當我們執行 `(+) <$> (+10) <*> (+5)` 時，我們對 `(+10)` 與 `(+5)` 未來的回傳值使用 `+`，結果也是某個只有在以一個參數呼叫它的時候才會產生一個值的東西。

我們通常不會將 function 用作 applicative，但這依然十分有趣。你瞭解對於 `Applicative` 的 `(->) r` 實體如何運作並不是很重要，所以若是你當前無法瞭解它也別氣餒。試著把玩 applicative style 與 function，以建立 functions 作為 applicative 的感覺。

一個我們還不曾遇過的 `Applicative` 實體為 `ZipList`，它位在 `Control.Applicative` 中。

結果證明，實際上 list 還有更多作為 applicative functor 的方式。一種方式是我們已經提過的，其表明以一個 function 的 list 與一個值的 list 呼叫 `<*>` 會產生一個擁有將左 list 的 function 應用到右 list 的值的所有可能組合的 list。若是我們執行 `[(+3),(*2)] <*> [1,2]`，`(+3)` 就會被應用到 `1` 與 `2`，且 `(*2)` 也會被應用到 `1` 與 `2`，產生一個有著四個元素的 list，即 `[4,5,2,4]`。

然而，`[(+3),(*2)] <*> [1,2]` 也能夠以這種方式運作：將左 list 中的第一個 function 應用到右 list 中的第一個值、將左 list 中的第二個 function 應用到右 list 中的第二個值、以此類推。這會產生一個有兩個值的 list，即 `[4,4]`。你可以將它看作 `[1 + 3, 2 * 2]`。

因為一個型別無法有兩個相同 typeclass 的實體，於是 `ZipList a` 就被引入了，其有一個只有一個欄位的建構子 `ZipList`，而這個欄位為一個 list。以下即是這個實體：

<pre name="code" class="haskell:hs">
instance Applicative ZipList where
        pure x = ZipList (repeat x)
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
</pre>

`<*>` 所做的就是我們所說的了。它將第一個 function 應用到第一個值、將第二個 function 應用到第二個值、以此類推。這以 `zipWith (\f x -> f x) fs xs` 來達成。因為 `zipWith` 是如此運作的，所以產生的 list 將會與兩個 list 中較短者一樣長。

這裡的 `pure` 也很有趣。它取一個值、並將它放進一個僅有這個值無限重複的 list 中。`pure "haha"` 產生 `ZipList (["haha","haha","haha"...`。這可能有點令人困惑，因為我們說過 `pure` 應該要把值放進一個仍會產生這個值的最小情境中。你或許認為一個某值的無限 list 根本就不是最小的。但這對 zip list 是合理的，因為它必須在任何位置產生值。這也滿足 `pure f <*> xs` 應該等於 `fmap f xs` 的原則。若是 `pure 3` 僅回傳 `ZipList [3]`，`pure (*2) <*> ZipList [1,5,10]` 就會產生 `ZipList [2]`，因為兩個 zip list 產生的 list 的長度為兩者中較短者的長度。若是我們以一個無限的 list 扣上一個有限的 list，產生的 list 長度就永遠會等於有限的 list 的長度。

所以 zip list 是如何以 applicative style 運作的呢？讓我們看看。喔，`ZipList a` 型別沒有 `Show` 實體，所以我們必須使用 <code class="label function">getZipList</code> function 來從一個 zip list 擷取出一個 raw list。

<pre name="code" class="haskell:ghci">
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
[101,102,103]
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
[101,102,103]
ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
[5,3,3,4]
ghci> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
[('d','c','r'),('o','a','a'),('g','t','t')]
</pre>

<p class="hint">
<code>(,,)</code> function 等同於 <code>\x y z -> (x,y,z)</code>。同樣的，<code>(,)</code> function 等同於 <code>\x y -> (x,y)</code>。
</p>

除了 `zipWith`，標準函式庫也有像是 `zipWith3`、`zipWith4`、一路到 7 的 function。`zipWith` 取一個接收兩個參數的 function，並以它扣上兩個 list。`zipWith3` 取一個接收三個參數的 function，並以它扣上三個 list，以此類推。
藉由以 applicative style 來使用 zip list，對於每個我們想要扣在一起的 list，我們就不必有個個別的 zip function。我們只要使用 applicative style 來將任意數量的 list 以一個 function 扣在一起，這樣非常好。

`Control.Applicative` 定義了一個叫做 <code class="label function">liftA2</code> 的 function，其型別為 `liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c`。它像這樣被定義。

<pre name="code" class="haskell:hs">
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b
</pre>

沒什麼特別的，它僅將一個 function 應用到兩個 applicative 之間，隱藏我們已熟悉的 applicative style。
我們察看它的原因，是因為它清楚地顯示了為什麼 applicative functor 比起普通的 functor 是比較強大的。以普通的 functor，我們只能將 function 映射到一個 functor。但以 applicative functor，我們可以將一個 function 應用到多個 functor 之間。把這個 function 的型別看成 `(a -> b -> c) -> (f a -> f b -> f c)` 也很有趣。當我們像這樣來看它時，我們可以說 `liftA2` 取一個普通的二元 function，並將它升級成一個操作兩個 functor 的 function。

這裡有個有趣的概念：我們可以取兩個 applicative functor，並將它們結合成一個 applicative functor，在其中有著在一個 list 中的這兩個 applicative functor 的結果。舉例來說，我們有 `Just 3` 與 `Just 4`。讓我們假設第二個 applicative functor 之中有一個單一元素的 list，因為要達成非常容易：

<pre name="code" class="haskell:ghci">
ghci> fmap (\x -> [x]) (Just 4)
Just [4]
</pre>

好，所以讓我們假定我們有 `Just 3` 與 `Just [4]`。我們要如何取得 `Just [3,4]` 呢？簡單。

<pre name="code" class="haskell:ghci">
ghci> liftA2 (:) (Just 3) (Just [4])
Just [3,4]
ghci> (:) <$> Just 3 <*> Just [4]
Just [3,4]
</pre>

記住，`:` 為一個取一個元素、與一個 list，並回傳一個帶著這個位在開頭的元素的 list 的 function。現在我們有 `Just [3,4]` 了，我們能夠以 `Just 2` 結合它，以產生 `Just [2,3,4]` 嗎？我們當然可以。我們似乎能夠將任意數量的 applicative 結合成一個其中有著這些 applicative 的結果的 list 的 applicative。讓我們試著實作一個 function，其取一個 applicative 的 list，並回傳一個擁有一個 list 作為它的回傳值的 applicative。我們要稱它為 `sequenceA`。

<pre name="code" class="haskell:hs">
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
</pre>

啊，遞迴！首先，我們看看型別。它會把一個 applicative 的 list 轉成一個帶著一個 list 的 applicative。藉此，我們可以為邊界條件奠定一些基礎。若是我們想要將一個空 list 轉成一個帶著結果的 list 的 applicative，嗯，我們只要將一個空 list 擺進一個預設情境中。現在輪到遞迴了。
若是我們有個有著 head 與 tail 的 list（記住，`x` 為一個 applicative，`xs` 為一個它們的 list），我們就對 tail 呼叫 `sequenceA`，其產生一個帶著一個 list 的 applicative。接著，我們僅將 applicative `x` 之中的值前置在這個帶著一個 list applicative 之前，就這樣！

所以若是我們執行 `sequenceA [Just 1, Just 2]`，即是 `(:) <$> Just 1 <*> sequenceA [Just 2]`。這等於 `(:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])`。啊！
我們知道 `sequenceA []` 最終會變成 `Just []`，所以這個 expression 現在是 `(:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])`，即是 `(:) <$> Just 1 <*> Just [2]`，即是 `Just [1,2]`！

另一種實作 `sequenceA` 的方式是使用折疊。記得，幾乎任何我們用以一個元素接著一個元素走過一個 list、並一路累加結果的 function，都能夠以折疊來實作。

<pre name="code" class="haskell:hs">
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])
</pre>

我們從右邊開始逼近 list，並以 `pure []` 的累加值開始。
我們在累加器與 list 的最後一個元素之間執行 `liftA2 (:)`，其產生一個有個單一元素在其中的 applicative。接著我們以當前最後一個元素與目前的累加器執行 `liftA2 (:)`，以此類推，直到我們只剩下累加器──其持有一個所有 applicative 的結果的 list──為止。

讓我們用一些 applicative 來試試我們的 function。

<pre name="code" class="haskell:ghci">
ghci> sequenceA [Just 3, Just 2, Just 1]
Just [3,2,1]
ghci> sequenceA [Just 3, Nothing, Just 1]
Nothing
ghci> sequenceA [(+3),(+2),(+1)] 3
[6,5,4]
ghci> sequenceA [[1,2,3],[4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
ghci> sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]
[]
</pre>

啊！非常好。當使用在 `Maybe` 值時，`sequenceA` 以所有在其中的結果作為一個 list，建立了一個 `Maybe` 值。若是其中一個值為 `Nothing`，則結果也會是個 `Nothing`。當你有個 `Maybe` 值的 list，且你只有在沒有值是 `Nothing` 時才對這些值有興趣的時候，這是很棒的。

當以 function 使用時，`sequenceA` 會取一個 function 的 list，並回傳一個回傳一個 list 的 function。在我們的例子中，我們建立了一個 function，其取一個數字作為參數、將它應用到 list 中的每個 function、然後回傳結果的 list。`sequenceA [(+3),(+2),(+1)] 3` 將會以 `3` 呼叫 `(+3)`、以 `3` 呼叫 `(+2)`、以 `3` 呼叫 `(+1)`，並將所有結果作為一個 list 呈獻。

執行 `(+) <$> (+3) <*> (*2)` 將會建立一個取一個參數、將它餵給 `(+3)` 與 `(*2)`、然後以這兩個結果呼叫 `+` 的 function。同理，`sequenceA [(+3),(*2)]` 會建立一個將會取一個參數、並將它餵給所有在 list 中的 function 的 function 是很合理的。並非以 function 的結果呼叫 `+`，`:` 與 `pure []` 的結合被用來將這些結果收集到一個 list 中，這即是這個 function 的結果。

當我們有個 function 的 list，而我們想要將相同的輸入餵給它們、然後檢視結果的 list 時，使用 `sequenceA` 是很棒的。舉例來說，我們有一個數字，並且我們很好奇它是否滿足所有在一個 list 中的述部。做到這件事的一種方式會像這樣：

<pre name="code" class="haskell:ghci">
ghci> map (\f -> f 7) [(>4),(<10),odd]
[True,True,True]
ghci> and $ map (\f -> f 7) [(>4),(<10),odd]
True
</pre>

記住，`and` 取一個布林 list，並在它們全都為 `True` 時回傳 `True`。另一個做到相同事情的方式是使用 `sequenceA`：

<pre name="code" class="haskell:ghci">
ghci> sequenceA [(>4),(<10),odd] 7
[True,True,True]
ghci> and $ sequenceA [(>4),(<10),odd] 7
True
</pre>

`sequenceA [(>4),(<10),odd]` 建立一個將會取一個數字、將它餵給 `[(>4),(<10),odd]` 中的所有述部、並回傳一個布林 list。它將一個帶著 `(Num a) => [a -> Bool]` 型別的 list 轉成帶著 `(Num a) => a -> [Bool]` 型別的 function。非常棒，哈？

因為 list 是同質的，所以所有在 list 中的 function 都必須為相同型別的 function，當然。你無法有個像是 `[ord, (+3)]` 的 list，因為 `ord` 取一個字元並回傳一個數字，而 `(+3)` 則取一個數字並回傳一個數字。

當以 `[]` 使用時，`sequenceA` 取一個 list 的 list，並回傳一個 list 的 list。唔，有趣。它實際上建立了一個擁有它們元素的所有可能組合的 list。為了說明，以下是以 `sequenceA`、接著以一個 list comprehension 來達成上面所說的：

<pre name="code" class="haskell:ghci">
ghci> sequenceA [[1,2,3],[4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
ghci> [[x,y] | x <- [1,2,3], y <- [4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
ghci> sequenceA [[1,2],[3,4]]
[[1,3],[1,4],[2,3],[2,4]]
ghci> [[x,y] | x <- [1,2], y <- [3,4]]
[[1,3],[1,4],[2,3],[2,4]]
ghci> sequenceA [[1,2],[3,4],[5,6]]
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
ghci> [[x,y,z] | x <- [1,2], y <- [3,4], z <- [5,6]]
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
</pre>

這可能有點難以領會，但若是你用它把玩了一會，你就會發現它是怎麼運作的了。讓我們假定我們做的是 `sequenceA [[1,2],[3,4]]`。為了看看這是怎麼回事，讓我們使用 `sequenceA` 的 `sequenceA (x:xs) = (:) <$> x <*> sequenceA xs`、以及邊界條件 `sequenceA [] = pure []` 的定義。你不必遵循這個求值方法，但若是你在想像 `sequenceA` 如何運作在 list 的 list 時出了問題，這可能會有用的，因為它可能有點令人費解。

* 我們以 `sequenceA [[1,2],[3,4]]` 開始
* 計算結果為 `(:) <$> [1,2] <*> sequenceA [[3,4]]`
* 進一步求值內部的 `sequenceA`，我們得到 `(:) <$> [1,2] <*> ((:) <$> [3,4] <*> sequenceA [])`
* 我們抵達邊界條件，所以它現在為 `(:) <$> [1,2] <*> ((:) <$> [3,4] <*> [[]])`
* 現在，我們要計算 `(:) <$> [3,4] <*> [[]]` 部分，其會以在左 list 的每個可能值（可能值為 `3` 與 `4`）與在右 list 的所有可能值（唯一的可能值為 `[]`）使用 `:`，這會產生 `[3:[], 4:[]]`，即是 `[[3],[4]]`。所以現在我們有 `(:) <$> [1,2] <*> [[3],[4]]`
* 現在，`:` 被用在左 list 的所有可能值（`1` 與 `2`）與右 list 中的所有可能值（`[3]` 與 `[4]`），其結果為 `[1:[3], 1:[4], 2:[3], 2:[4]]`，即是 `[[1,3],[1,4],[2,3],[2,4]`

執行 `(+) <$> [1,2] <*> [4,5,6]` 會產生一個非確定性計算 `x + y`，其中 `x` 接收每個來自 `[1,2]` 的值、`y` 接收每個來自 `[4,5,6]` 的值。我們將它表示成一個持有所有可能結果的 list。相同的，當我們執行 `sequence [[1,2],[3,4],[5,6],[7,8]]` 時，結果為一個非確定性計算 `[x,y,z,w]`，其中 `x` 接收每個來自 `[1,2]` 的值、`y` 接收每個來自 `[3,4]` 的值，以此類推。為了要表示這個非確定性計算的結果，我們使用一個 list，其中每個在 list 中的元素都是一個可能的 list。這就是為什麼結果是一個 list 的 list。

以 I/O 動作使用時，`sequenceA` 是與 `sequence` 相同的東西！它取一個 I/O 動作的 list，並回傳一個將會執行這些動作、並以這些 I/O 動作的結果的 list 作為它的結果的 I/O 動作。這是因為，為了要將一個 `[IO a]` 值轉成一個 `IO [a]` 值、為了讓一個 I/O 動作在執行時產生結果的 list，所有這些 I/O 動作都必須被串接，以讓它們能在被強迫求值時一個接著一個執行。你無法在不執行一個 I/O 動作的情況下取得它的結果。

<pre name="code" class="haskell:ghci">
ghci> sequenceA [getLine, getLine, getLine]
heyh
ho
woo
["heyh","ho","woo"]
</pre>

如同一般的 functor，applicative functor 也有著一些原則。最重要的一個，是我們已經提過的，即是 <code class="label law">pure f <*> x = fmap f x</code> 成立。作為練習，你可以為一些我們在這一章所遇過的 applicative functor 證明這個原則。其它的原則是：

* <code class="label law">pure id <\*> v = v</code>
* <code class="label law">pure (.) <\*> u <\*> v <\*> w = u <\*> (v <\*> w)</code>
* <code class="label law">pure f <\*> pure x = pure (f x)</code>
* <code class="label law">u <\*> pure y = pure ($ y) <\*> u</code>

我們當前不會仔細察看它們，因為這會花上許多篇幅，而且它或許有點無聊，但若是你能勝任這個任務，你可以更仔細地看看它們，並看看它們對於一些實體是否成立。

總結一下，applicative functor 不只有趣、它們也很有用，因為它們允許我們藉由 applicative style 結合不同的計算，像是 I/O 計算、非確定性計算、可能會失敗的計算、等等。只要使用 `<$>` 與 `<*>`，我們就能使用普通的 function 來一致地操作各種 applicative functor，並充分利用每一個 applicative functor 的語義。

## <a name="the-newtype-keyword">newtype 關鍵字</a>

<img src="img/maoi.png" alt="why_ so serious?" style="float:left" />
到目前為止，我們已經學過如何使用 *data* 關鍵字來建立我們自己的代數資料型別。我們也學過如何以 *type* 關鍵字來給予現存的型別同義詞。在這一節，我們要在一開始看看如何使用 *newtype* 關鍵字來讓現有的資料型別變成一個新型別，以及為什麼我們想要這麼做。

在前一節，我們發現對於 list 型別，實際上有很多作為一個 applicative functor 的方式。一種方式是讓 `<*>` 從它左參數的 list 取出每個 function、並將它應用到右參數 list 中的每個值，產生左 list 中的 function 應用到右 list 中的值的所有可能組合。

<pre name="code" class="haskell:ghci">
ghci> [(+1),(*100),(*5)] <*> [1,2,3]
[2,3,4,100,200,300,5,10,15]
</pre>

第二種方式是取 `<*>` 左邊的第一個 function、並將它應用到右邊的第一個值，然後取左邊 list 的第二個 function、並將它應用到右邊的第二個值，以此類推。最終，它有點像是將兩個 list 扣在一起。但 list 已經為一個 `Applicative` 的實體了，所以我們要如何讓 list 也以第二種方式作為 `Applicative` 的實體呢？如果你還記得，我們說過 `ZipList a` 是為了這個理由而被引入的，其有一個只有一個欄位的值建構子，`ZipList`。我們將要包裝的 list 放進這個欄位。接著，`ZipList` 成為了一個 `Applicative` 的實體，使得我們想要以 zip 的方式將 list 用作 applicative 的時候，我們只要以 `ZipList` 建構子包裝它、然後當我們完成時，以 `getZipList` 來拆解它：

<pre name="code" class="haskell:ghci">
ghci> getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]
[2,200,15]
</pre>

所以，這個 <i>newtype</i> 關鍵字是做什麼用的？嗯，想想我們會怎麼寫我們的 `ZipList a` 型別的 data 宣告。一種方式是像這樣做：

<pre name="code" class="haskell:hs">
data ZipList a = ZipList [a]
</pre>

一個只有一個值建構子的型別，且這個值建構子只有一個為某值的 list 的欄位。我們可能也想使用 record 語法以讓我們自動地取得一個從`ZipList` 擷取 list 的 function：

<pre name="code" class="haskell:hs">
data ZipList a = ZipList { getZipList :: [a] }
</pre>

這看起來不賴，其實也運作得非常好。我們有兩種令現有型別為一個 typeclass 實體的方式，所以我們使用 <i>data</i> 關鍵字來將這個型別包成另一個型別，並讓後者以第二種方式作為實體。

Haskell 中的 <i>newtype</i> 關鍵字，正是為了在我們僅想取一個型別、並將它包在某個將它表示為另一個型別的東西中的這種情況而生的。在實際的函式庫中，`ZipList a` 是像這樣被定義的：

<pre name="code" class="haskell:hs">
newtype ZipList a = ZipList { getZipList :: [a] }
</pre>

並非 <i>data</i> 關鍵字，而是使用 <i>newtype</i> 關鍵字。所以，為什麼要這樣？嗯，首先，<i>newtype</i> 比較快。若是你使用 <i>data</i> 關鍵字來包裝一個型別，在你的程式執行時就會有一些包裝與拆解的額外開銷。但若是你使用 <i>newtype</i>，Haskell 就知道你只是要使用它來將一個現有的型別包成一個新型別（於是有了這個名字），因為你想讓它是相同的內在、但擁有不同的型別。考慮到這一點，Haskell 可以在它解析這個值是什麼型別時，去除掉包裝與拆解的動作。

所以為什麼不要每次都只使用 <i>newtype</i>，而非 <i>data</i> 呢？嗯，當你使用 <i>newtype</i> 關鍵字從一個現有型別建立一個新型別時，你就只能有一個值建構子，且這個值建構子只能有一個欄位。但以 <i>data</i>，你可以建立擁有多個值建構子、且每個建構子都可以有零或多個欄位的型別：

<pre name="code" class="haskell:hs">
data Profession = Fighter | Archer | Accountant

data Race = Human | Elf | Orc | Goblin

data PlayerCharacter = PlayerCharacter Race Profession
</pre>

使用 <i>newtype</i> 時，你就被限制成只有一個帶著一個欄位的建構子。

我們也能夠以 <i>newtype</i> 使用 <i>deriving</i> 關鍵字，就像是我們會以 <i>data</i> 做的。我們可以為 `Eq`、`Ord`、`Enum`、`Bounded`、`Show` 與 `Read` 衍生實體。若是我們為一個 typeclass 衍生實體，我們所包裝的型別原先就必須在這個 typeclass 中。這很合理，因為 <i>newtype</i> 只是包裝一個現有的型別。所以現在，若是我們執行下述這行，我們就可以印出並比較我們的新型別的值：

<pre name="code" class="haskell:hs">
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
</pre>

讓我們試試它：

<pre name="code" class="haskell:ghci">
ghci> CharList "this will be shown!"
CharList {getCharList = "this will be shown!"}
ghci> CharList "benny" == CharList "benny"
True
ghci> CharList "benny" == CharList "oisters"
False
</pre>

在這個特別的 <i>newtype</i> 中，值建構子為下述型別：

<pre name="code" class="haskell:hs">
CharList :: [Char] -> CharList
</pre>

它取一個 `[Char]` 值，像是 `"my sharona"`，並回傳一個 `CharList` 值。
從上面我們使用 `CharList` 值建構子的例子，我們發現確實如此。相反的，`getCharList` function──它是因為我們在我們的 <i>newtype</i> 中使用了 record 語法而產生的──的型別為：

<pre name="code" class="haskell:hs">
getCharList :: CharList -> [Char]
</pre>

它取一個 `CharList` 值，並將它轉成 `[Char]` 值。你可以將這想成包裝與拆解，但你也可以將它想成將值從一個型別轉成另一個型別。

### 使用 newtype 來建立 typeclass 實體

很多時候，我們想要令我們的型別為特定 typeclass 的實體，但型別參數就是不合我們想要的。令 `Maybe` 為一個 `Functor` 實體是很容易的，但 `Functor` typeclass 是像這樣被定義的：

<pre name="code" class="haskell:hs">
class Functor f where
    fmap :: (a -> b) -> f a -> f b
</pre>

所以我們從這開始：

<pre name="code" class="haskell:hs">
instance Functor Maybe where
</pre>

然後實作 `fmap`。所有型別參數都很合理，因為 `Maybe` 取代了 `Functor` typeclass 定義中的 `f`，
所以若是我們把 `fmap` 看成它只運作在 `Maybe` 的話，它最終的行為會像：

<pre name="code" class="haskell:hs">
fmap :: (a -> b) -> Maybe a -> Maybe b
</pre>

<img src="img/krakatoa.png" alt="wow, very evil" style="float:right" />
這不是很好嗎？現在若是我們想要令 tuple 為 `Functor` 的實體，以在我們將一個 function `fmap` 到一個 tuple 時，它就會被應用到 tuple 的第一項？以這種方式，執行 `fmap (+3) (1,1)` 會產生 `(4,1)`。事實證明，為此寫下實體是有點困難的。對於 `Maybe`，我們只要表明 `instance Functor Maybe where`，因為只有恰好取一個參數的型別建構子可以作為 `Functor` 的實體。但看起來好像沒有辦法對 `(a,b)` 這麼做，以讓型別參數 `a` 在我們使用 `fmap` 時作為被改變的那個值。為了解決這個問題，我們可以 <i>newtype</i> 我們的 tuple，使得第二個型別參數代表 tuple 中第一項的型別：

<pre name="code" class="haskell:hs">
newtype Pair b a = Pair { getPair :: (a,b) }
</pre>

現在，我們可以令它為一個 `Functor` 的實體，以讓 function 被映射到第一項：

<pre name="code" class="haskell:hs">
instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)
</pre>

如你所見，我們能夠模式匹配到以 <i>newtype</i> 定義的型別。我們進行模式匹配以取得內部的 tuple，然後將 function `f` 應用 tuple 中的第一項，接著使用 `Pair` 值建構子來將 tuple 轉回我們的 `Pair b a`。若是我們將 `fmap` 的型別想成只運作在我們的新 pair 上，它會是：

<pre name="code" class="haskell:hs">
fmap :: (a -> b) -> Pair c a -> Pair c b
</pre>

再一次，我們表明 `instance Functor (Pair c) where`，於是 `Pair c` 取代了在 `Functor` typeclass 定義中的 `f`：

<pre name="code" class="haskell:hs">
class Functor f where
    fmap :: (a -> b) -> f a -> f b
</pre>

所以現在，若是我們將一個 tuple 轉成一個 `Pair b a`，我們可以對它使用 `fmap`，而 function 將會被映射到第一項：

<pre name="code" class="haskell:ghci">
ghci> getPair $ fmap (*100) (Pair (2,3))
(200,3)
ghci> getPair $ fmap reverse (Pair ("london calling", 3))
("gnillac nodnol",3)
</pre>

### On newtype laziness

我們提過 <i>newtype</i> 通常比 <i>data</i> 還快。<i>newtype</i> 唯一可以做的事情是將一個現有的型別轉成一個新型別，所以在內部，Haskell 可以將以 <i>newtype</i> 定義的型別的值像原先的型別一樣表示，只是它必須記住它現在的型別是不同的。這一事實意味著 <i>newtype</i> 不只比較快，它還比較懶（lazier）。讓我們看看這代表什麼。

如同我們先前說過的，Haskell 預設是惰性的，這意味著只有在我們試著實際印出我們的 function 結果時，才會進行計算。此外，只有對於我們的 function 來說，為了告訴我們結果所必須的那些計算才會被執行。Haskell 中的 `undefined` 值表示一個錯誤計算。若是我們試著藉由將它印到終端機來對它求值（即，強迫 Haskell 真的去計算它），Hakslle 將會拋出一個 hissy fit（技術上簡稱為例外）：

<pre name="code" class="haskell:ghci">
ghci> undefined
*** Exception: Prelude.undefined
</pre>

然而，若是我們建立一個在其中擁有一些 `undefined` 值的 list，但只需要 list 的 head──它並非 `undefined`──那個一切都將順利進行，因為若是我們只想看看第一個元素是什麼，Haskell 並不真的需要對 list 中的其它元素求值：

<pre name="code" class="haskell:ghci">
ghci> head [3,4,5,undefined,2,undefined]
3
</pre>

現在考慮下述型別：

<pre name="code" class="haskell:hs">
data CoolBool = CoolBool { getCoolBool :: Bool }
</pre>

它是個你以 <i>data</i> 關鍵字定義的普通的代數資料型別。它有一個值建構子，其有一個型別為 `Bool` 的欄位。讓我們建立一個對一個 `CoolBool` 進行模式匹配，並且不論在 `CoolBool` 中的 `Bool` 為 `True` 或 `False` 都回傳 `"hello"` 的 function：

<pre name="code" class="haskell:hs">
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"
</pre>

並非將這個 function 應用到一個普通的 `CoolBool`，讓我們給它個難題：將它應用到 `undefined`！

<pre name="code" class="haskell:ghci">
ghci> helloMe undefined
"*** Exception: Prelude.undefined
</pre>

呀！一個例外！這個例外為何會發生呢？以 <i>data</i> 關鍵字定義的型別可以有多個值建構子（即使 `CoolBool` 只有一個）。所以為了要看看給定我們 function 的值是否符合 `(CoolBool _)` 模式，Haskell 必須對它求值，才能看看我們建立值所使用的值建構子是哪個。而當我們試著對一個 `undefined` 值求值時──即使只有一下子──就拋出一個例外了。

並非使用 <i>data</i> 關鍵字來定義 `CoolBool`，讓我們試著使用 <i>newtype</i>：

<pre name="code" class="haskell:hs">
newtype CoolBool = CoolBool { getCoolBool :: Bool }
</pre>

我們不必修改我們的 `helloMe` function，因為若是你使用 <i>newtype</i> 或是 <i>data</i> 來定義你的型別，模式匹配語法都是相同的。在這裡，讓我們做一樣的事情，將 `helloMe` 套用到一個 `undefined` 值：

<pre name="code" class="haskell:ghci">
ghci> helloMe undefined
"hello"
</pre>

<img src="img/shamrock.png" alt="top of the mornin to ya!!!" style="float:right" />
它能動！唔，為何如此？嗯，如同我們說過的，當我們使用 <i>newtype</i> 時，Haskell 可以在內部將新型別的值以表示原來的值的相同方式來表示。它不必加上另一個盒子來包裝它，它只需要知道這個值是不同型別的。且因為 Haskell 知道以 <i>newtype</i> 關鍵字建立的型別只能有一個建構子，它就不必對傳遞到 function 的值求值，以確認它符合 `(CoolBool _)` 模式，因為 <i>newtype</i> 只能有一個可能的值建構子與一個欄位！

這些行為上的差異看似微不足道，但它實際上非常重要，因為它幫助我們瞭解：即使以 <i>data</i> 與 <i>newtype</i> 定義的型別，從程式設計師觀點的表現相同，因為它們都有值建構子與欄位，它們實際上也是兩個不同的機制。<i>data</i> 可以從無到有建立你自己的型別，而 <i>newtype</i> 則是讓一個現有的資料型別變成一個全新型別。對 <i>newtype</i> 值模式匹配並不像是從一個盒子中取出東西（像是 <i>data</i> 一樣），它更像是建立一個從一個型別到另一個型別的直接轉換。

### `type` vs. `newtype` vs. `data`

此時，你可能有點搞混 <i>type</i>、<i>data</i> 與 <i>newtype</i> 之間的差異了，所以讓我們稍微回顧一下。

*type* 關鍵字是用以建立型別同義詞。這意味著我們僅是將另一個名字給予一個已經存在的型別，以讓這個型別更容易參照。假使我們執行下述這行：

<pre name="code" class="haskell:hs">
type IntList = [Int]
</pre>

這所做的是允許我們以 `IntList` 參照 `[Int]` 型別。它們可以被交互使用。我們不會得到一個 `IntList` 值建構子、或是任何類似的東西。因為 `[Int]` 與 `IntList` 只是兩種參照到相同型別的方式，無論我們使用在我們型別註釋的名字是哪個都無所謂：

<pre name="code" class="haskell:ghci">
ghci> ([1,2,3] :: IntList) ++ ([1,2,3] :: [Int])
[1,2,3,1,2,3]
</pre>

當我們想要藉由給定型別名稱──其告訴我們它被使用在 function 情境中的目的──以讓我們的型別更加具有描述性時，我們就使用型別同義詞。舉例來說，當我們使用一個 `[(String,String)]` 型別的關連列表來表示一本電話簿時，我們給它 `PhoneBook` 這個型別同義詞，以讓我們 function 的型別簽名更容易閱讀。

*newtype* 用來取現有的型別，並將它包裝在新型別中，大多是為了更容易讓它成為特定 typeclass 的實體。當我們使用 <i>newtype</i> 來包裝一個現有型別時，我們得到的型別是不同於原先的型別的。若是我們建立下述 <i>newtype</i>：

<pre name="code" class="haskell:hs">
newtype CharList = CharList { getCharList :: [Char] }
</pre>

我們無法使用 `++` 來將一個 `CharList` 與一個 `[Char]` 型別的 list 擺在一起。我們甚至無法使用 `++` 來將兩個 `CharList` 擺在一起，因為 `++` 只能運作在 list 上，而 `CharList` 型別並非一個 list，即使它可以說它包含了一個 list。然而，我們可以將兩個 `CharList` 轉成 list、`++` 它們，然後將它們轉回 `CharList`。

當我們在我們的 <i>newtype</i> 宣告使用 record 語法時，我們便得到用以在新型別與原始型別之間轉換的 function：即是我們 <i>newtype</i> 的值建構子，以及用以在它的欄位擷取值的 function。新型別也不會自動作為原始型別屬於的 typeclass 實體，所以我們必須衍生或手動撰寫它。

實務上，你可以將 <i>newtype</i> 宣告想成只有一個建構子與一個欄位的 <i>data</i> 宣告。若是你發現你自己在寫這樣的 <i>data</i> 宣告時，就考慮使用 <i>newtype</i> 吧。

*data* 關鍵字用來建立你自己的資料型別，並且使用它，你可能會興奮得發狂。它可以有數量如你所想的建構子與欄位，也可以用來實作任何代數資料型別。從 list、類 `Maybe` 型別到 tree 的任何東西。

若是你僅想讓你的型別簽名看起來更清楚且更有描述性，你或許需要型別同義詞。若是你想要取一個現有型別，並將它包在一個新型別中，以讓它為一個 typeclass 的實體，你可能就是在找一個 <i>newtype</i>。假如你想要建立一個全新的型別，你要找的很可能是 <i>data</i> 關鍵字。

## <a name="monoids">單子</a>
