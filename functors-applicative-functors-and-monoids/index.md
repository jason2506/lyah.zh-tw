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

另一個我們一直在處理、但沒有認出它是一個 `Functor` 的 `Functor` 實體為 `(->) r`。你現在可能有點混亂，因為 `(->) r` 到底代表什麼鬼東西？function 型別 `r -> a` 可以被改寫成 `(->) r a`，很像是我們可以將 `2 + 3` 寫成 `(+) 2 3`。當我們以 `(->) r a` 來看它時，我們能夠以略微不同的觀點來看看 `(->)`，因為我們發現它就像 `Either`，是一個接收兩個型別參數的型別建構子。但要記得，我們說過一個型別建構子必須恰好取一個型別參數，以讓它能作為 `Functor` 的實體。這就是為什麼我們無法讓 `(->)` 為 `Functor` 的實體，但若是我們將它部分應用成 `(->) r`，它就不會產生任何問題。若是語法允許型別建構子以片段被部分應用（像是我們可以藉由 `(2+)` 來部分應用 `+`，其與 `(+) 2` 相同），你可以將 `(->) r` 寫成 `(r ->)`。function functor 如何呢？嗯，讓我們看看 `Control.Monad.Instances` 中的實作：

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

若是我們可以證明某個型別遵守這兩個 functor 原則，在涉及映射時，我們就可以依靠它與其他 functor 相同的基礎行為。我們可以知道，當我們對它使用 `fmap` 時，背後不會有任何映射以外的事情發生，且它會作為一個可以被映射的東西，即 functor。你藉由查看 `fmap` 對於某個型別的實作、然後使用我們用來檢查 `Maybe` 是否遵守第一原則的方法，以理解對於這個型別，第二原則為何成立。

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

阿！我們知道第一個 functor 原則聲明，若是我們將 `id` 映射到一個 functor，它應該要與以相同的 functor 呼叫 `id` 相同，但如同我們在這個例子中看到的，這對於我們的 `CMaybe` functor 並不為真。它不遵守 functor 原則，即使它是 `Functor` typeclass 的一員，它也不是一個 functor。若是某人將我們的 `CMaybe` 作為一個 functor 使用，他們會預期它像個好 functor 一樣遵守 functor 原則。但即使 `CMaybe` 自稱是一個 functor，它作為一個 functor 也失格了，所以將它用作一個 functor 可能會導致一些有缺陷的程式碼。當我們使用一個 functor 時，若是我們先組合一些 function、然後將它們映射到 functor，或者我們僅連續將每個 function 映射到一個 functor 都不該有關係。但使用 `CMaybe` 卻有關係，因為它記錄了它被映射的次數。一點也不好！若是我們想讓 `CMaybe` 遵守 functor 原則，我們就必須在我們使用 `fmap` 時，令 `Int` 欄位維持不變。

首先，functor 原則看起來可能有點令人困擾、且不必要，但若是我們知道一個型別遵守這兩個原則，我們就可以做出某些它會如何行為的假設。若是一個型別遵守 functor 原則，我們就知道對這個型別的值呼叫 `fmap` 只會將 function 映射到它，僅此而已。這使得程式碼更加抽象且可擴充，因為我們可以使用原則來推論任何 functor 都該有的行為，並建立可靠地操作任意 functor 的 function。

所以在標準函式庫中的 `Functor` 都遵守這些原則，但若是你不相信我，你可以自己去檢查。在你下一次令一個型別為 `Functor` 的實體時，花一分鐘確保它遵守 functor 原則。一旦你處理過夠多的 functor，你就能稍微直觀地看出它們共有的屬性與行為，並且直觀地看出一個型別是否遵守 functor 原則並不困難。但即使沒有直覺，你還是可以一行接著一行掃過實作，並看看原則是否成立、或是試著找出一個反例。

我們也可以將 functor 看作在一個語境中輸出值的東西。舉例來說，`Just 3` 在「它可能或不可能輸出任何值」的語境中會輸出 `3`。`[1,2,3]` 在「這裡可能有更多值、或沒有值」的語境中會輸出三個值──`1`、`2`、與 `3`。`(+3)` 這個 function 會輸出一個值，視給它的參數為何者而定。

若是你將 functor 想成輸出值的東西，你可以將映射到 functor 想成對 functor 的輸出附加一個改變值的轉換。當我們執行 `fmap (+3) [1,2,3]` 時，我們將轉換 `(+3)` 附加到 `[1,2,3]` 的輸出，所以無論我們何時看到 list 輸出的數字，`(+3)` 都會被應用到它。另一個例子是映射 function。當我們執行 `fmap (+3) (*3)` 時，我們將轉換 `(+3)` 附加到 `(*3)` 最終的輸出上。以這種方式來看它，給了我們一些為何將 `fmap` 使用在 function 只是個計算（`fmap (+3) (*3)` 等於 `(+3) . (*3)`，其等同於 `\x -> ((x*3)+3)`）的感覺，因為我們取一個像是 `(*3)` 的 function，接著我們將轉換 `(+3)` 附加到它的輸出。結果仍然是一個 function，只是當我們給它一個數字時，它會被乘以三、然後它會通過附加的轉換、其中它會被加上三。這就是使用複合操作所做的了。

## <a name="applicative-functors">Applicative functor</a>

## <a name="the-newtype-keyword">newtype 關鍵字</a>

## <a name="monoids">單子</a>
