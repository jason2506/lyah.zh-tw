---
layout: page
title: 模組
prev:
    url: higher-order-functions
    title: 高階函數
---

## <a name="loading-modules">載入模組</a>

<img src="img/modules.png" alt="modules" style="float:right" />
一個 Haskell 模組（module）是個相關 function、型別與 typeclass 的集合。一個 Haskell 程式是個模組的集合，其中是由主（main）模組載入其它的模組，然後使用定義在其中的 function 做某些事。將程式碼切割成數個模組有非常多優點。若是一個模組足夠一般化，其輸出（export）的 function 就能夠被使用在許多不同的程式中。若是你擁有的程式碼被分割成獨立（self-contained）的模組，也就是不會太過依賴其它模組（我們也將這稱為鬆耦合〈loosely coupled〉），你就可以在之後重用它。藉由將程式切割成數個部分，每個部分都有其用途，使得撰寫程式的處理更容易控制。

Haskell 標準函式庫被切割成多個模組，每個都包含某種相關且提供共同目的的 function 與型別。有個處理 list 的模組、有個用作平行（concurrent）程式開發的模組、有個處理複數的模組、等等。所有我們到目前為止處理的 function、型別與 typeclass 都是 `Prelude` 模組──它預設會被引入（import）──的一部分。在這一章，我們要檢驗一些有用的模組與其擁有的 function。但首先，我們要來看看如何引入模組。

在 Haskell 腳本中，引入模組的語法為 `import <module name>`。這必須在定義任何 function 前完成，所以引入通常在檔案的開頭完成。當然，一個腳本可以引入多個模組。只要將引入敘述擺在獨立的一行裡。讓我們引入 `Data.List` 模組──它有一堆處理 list 的有用 function，並使用它輸出的 function 來建立一個告訴我們 list 有多少個不重複元素的 function。

<pre name="code" class="haskell:hs">
import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
</pre>

當你執行 `import Data.List` 時，在全域名稱空間（namespace）中，`Data.List` 輸出的所有 function 都會變得可以取用，意味著你可以在腳本的任何地方呼叫它。`nub` 是個定義在 `Data.List` 的 function，其接收一個 list 並去除重複的元素。藉由 `length . nub` 來組合 `length` 與 `nub` 會產生一個等同於 `\xs -> length (nub xs)` 的 function。

你也可以在使用 GHCI 的時候把模組的 function 擺進全域空間中。若是你在 GHCI 中，且想要能夠呼叫從 `Data.List` 輸出的 function，就這樣做：

<pre name="code" class="haskell:ghci">
ghci> :m + Data.List
</pre>

若是你想要在 GHCI 中載入來自多個模組的名稱，我們不需要多次執行 `:m +`，我們可以一次載入多個模組。

<pre name="code" class="haskell:ghci">
ghci> :m + Data.List Data.Map Data.Set
</pre>

然而，若是你載入了已經載入某個模組的腳本，你就不需要使用 `:m +` 來存取它。

若是你僅需要某個模組中的一部分 function，你可以選擇性的引入這些 function。若是你只想要從 `Data.List` 引入 `nub` 與 `sort` function，我們就這樣做：

<pre name="code" class="haskell:hs">
import Data.List (nub, sort)
</pre>

你也可以選擇引入某個模組的所有的 function，除了少數的幾個之外。這在多個模組輸出相同名字的 function，而你想要避免引起問題時，通常挺有用的。假使我們已經有個叫做 `nub` 的我們自己的 function，而我們想要從 `Data.List` 引入所有除了 `nub` function 以外的 function：

<pre name="code" class="haskell:hs">
import Data.List hiding (nub)
</pre>

另一種處理名稱衝突的方式是執行限制（qualified）引入。`Data.Map` 模組──其提供一個藉由 key 尋找 value 的資料結構──輸出一堆與 `Prelude` 相同名稱的 function，像是 `filter` 或是 `null`。所以當我們引入 `Data.Map` 然後呼叫 `filter` 的時候，Haskell 無法知道要用哪個 function。以下是我們如何解決這個問題：

<pre name="code" class="haskell:hs">
import qualified Data.Map
</pre>

這使得：若是我們想要參照（reference）`Data.Map` 的 `filter` function，我們必須執行 `Data.Map.filter`，而 `filter` 仍然指到我們都熟知喜愛的一般 `filter`。但是在每個來自此模組的 function 前面都要加上 `Data.Map` 有些令人厭煩。這就是我什麼我們可以將限制引入取個比較短的名字：

<pre name="code" class="haskell:hs">
import qualified Data.Map as M
</pre>

現在，要參照 `Data.Map` 的 `filter` function，我們只需要用 `M.filter`。

使用[這個方便的參考手冊](http://www.haskell.org/ghc/docs/latest/html/libraries/)來看看哪些模組在標準函式庫中。拾取新的 Haskell 知識的偉大途徑，即是點開標準函式庫參考手冊，並瀏覽模組與其 function。你也可以看看每個模組的 Haskell 原始碼。閱讀某些模組的原始碼是學習 Haskell 與得到實際體會的非常好的方式。

要搜尋 function 或是尋找其所在，就使用 [Hoogle](http://haskell.org/hoogle)。這是個非常棒的 Haskell 搜尋引擎，你可以透過名稱、模組名稱、甚或是型別簽名來搜尋。

## <a name="data-list">Data.List</a>

顯而易見的，`Data.List` 模組全都與 list 有關。它提供一些處理 list 的非常有用的 function。我們已經看過其中的一些 function（像是 `map` 與 `filter`）了，因為 `Prelude` 為了方便起見，從 `Data.List` 引入了一些 function。你不需要藉由限制引入來引入 `Data.List`，因為除了已經被 `Prelude` 搶先從 `Data.List` 引入的那些名稱以外，它不會與任何 `Prelude` 名稱衝突。讓我們看看一些我們之前未曾看過的 function。

<code class="label function">intersperse</code> 接收一個元素與一個 list，然後將這個元素擺在 list 中的每一對元素之間。這裡有個示範：

<pre name="code" class="haskell:ghci">
ghci> intersperse '.' "MONKEY"
"M.O.N.K.E.Y"
ghci> intersperse 0 [1,2,3,4,5,6]
[1,0,2,0,3,0,4,0,5,0,6]
</pre>

<code class="label function">intercalate</code> 接收一個 list 的 list 與一個 list。此時它將這個 list 插入到 list 的 list 之間，然後展開（flatten）其結果<span class="note">〔譯註：`intercalate x y` 等同於 `concat (intersperse x y)` 等同於 `concat . intersperse x $ y`〕</span>。

<pre name="code" class="haskell:ghci">
ghci> intercalate " " ["hey","there","guys"]
"hey there guys"
ghci> intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]
[1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]
</pre>

<code class="label function">transpose</code> 轉置（transpose）一個 list 的 list。如果你把 list 的 list 看作是個 2D 矩陣，列會變成行，行會變成列。

<pre name="code" class="haskell:ghci">
ghci> transpose [[1,2,3],[4,5,6],[7,8,9]]
[[1,4,7],[2,5,8],[3,6,9]]
ghci> transpose ["hey","there","guys"]
["htg","ehu","yey","rs","e"]
</pre>

假使我們有個多項式 <i>3x^2 + 5x + 9</i>、<i>10x^3 + 9</i> 與 <i>8x^3 + 5x^2 + x - 1</i> 而我們想要把它們加總起來。我們在 Haskell 使用 list `[0,3,5,9]`、`[10,0,0,9]` 與 `[8,5,1,-1]` 來代表它們。現在，要加總它們，我們所要做的是：

<pre name="code" class="haskell:ghci">
ghci> map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
[18,8,6,17]
</pre>

當我們轉置這三個 list，三次方此時就到了第一行，二次方會在第二行，以此類推。將 `sum` 映射到其之上會產生我們所需的結果。

<img src="img/legolists.png" alt="shopping lists" style="float:left" />
<code class="label function">foldl'</code> 與 <code class="label function">foldl1'</code> 是它們各自惰性實作的嚴格版。在將惰性的摺疊使用在大的 list 時，你也許會得到一個堆疊溢出（stack overflow）錯誤。這個問題的罪魁禍首就是因為摺疊的惰性，累加值實際上並不會在摺疊發生的時候更新。實際上發生的是，累加器像是作了一個「它會在要求實際產生結果的時候計算它的值」的承諾（也被稱作 thunk）。這發生在每個中介的累加器上，而這些 thunk 溢出你的堆疊。嚴格版的摺疊不是個懶惰蟲，且真的會在它們執行的時候計算中介值，而不是以 thunk 佔據你的堆疊。所以若是你曾經在執行惰性摺疊時得到一個堆疊溢出錯誤，試著將它轉成嚴格版的。

<code class="label function">concat</code> 將一個 list 的 list 展開成一個元素的 list。

<pre name="code" class="haskell:ghci">
ghci> concat ["foo","bar","car"]
"foobarcar"
ghci> concat [[3,4,5],[2,3,4],[2,1,1]]
[3,4,5,2,3,4,2,1,1]
</pre>

它僅會移除嵌套的一個層級。所以若是我們要完全展開 `[[[2,3],[3,4,5],[2]],[[2,3],[3,4]]]`──它是一個 list 的 list 的 list，你必須串接（concatenate）它們兩次。

進行 <code class="label function">concatMap</code> 與先將一個 function 映射到一個 list，然後以 `concat` 串接 list 相同。

<pre name="code" class="haskell:ghci">
ghci> concatMap (replicate 4) [1..3]
[1,1,1,1,2,2,2,2,3,3,3,3]
</pre>

<code class="label function">and</code> 接收一個布林值的 list，並只在所有 list 中的值為 `True` 的時候回傳 `True`。

<pre name="code" class="haskell:ghci">
ghci> and $ map (>4) [5,6,7,8]
True
ghci> and $ map (==4) [4,4,4,3,4]
False
</pre>

<code class="label function">or</code> 就像是 `and`，只是它會在任何 list 中的布林值為 `True` 的時候回傳 `True`。

<pre name="code" class="haskell:ghci">
ghci> or $ map (==4) [2,3,4,5,6,1]
True
ghci> or $ map (>4) [1,2,3]
False
</pre>

<code class="label function">any</code> 與 <code class="label function">all</code> 分別接收一個述部，然後檢查是否有任何或所有在 list 中的元素符合這個述部。通常我們會使用這兩個 function，而不是映射到一個 list 然後進行 `and` 或 `or`。

<pre name="code" class="haskell:ghci">
ghci> any (==4) [2,3,5,6,1,4]
True
ghci> all (>4) [6,9,10]
True
ghci> all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
False
ghci> any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
True
</pre>

<code class="label function">iterate</code> 接收一個 function 與一個起始值。其套用 function 到起始值上，然後套用這個 function 在產生的結果，然後再一次套用 function 在產生的結果，以此類推。它會以無限 list 的形式回傳所有的結果。

<pre name="code" class="haskell:ghci">
ghci> take 10 $ iterate (*2) 1
[1,2,4,8,16,32,64,128,256,512]
ghci> take 3 $ iterate (++ "haha") "haha"
["haha","hahahaha","hahahahahaha"]
</pre>

<code class="label function">splitAt</code> 接收一個數字與一個 list。此時它會在這麼多元素之處切割 list，回傳 tuple 中的兩個生成的 list。

<pre name="code" class="haskell:ghci">
ghci> splitAt 3 "heyman"
("hey","man")
ghci> splitAt 100 "heyman"
("heyman","")
ghci> splitAt (-3) "heyman"
("","heyman")
ghci> let (a,b) = splitAt 3 "foobar" in b ++ a
"barfoo"
</pre>

<code class="label function">takeWhile</code> 是個非常有用的小 function。它在述部成立的時候從一個 list 中取出元素，然後在遇到一個不滿足述部的元素時截斷。事實證明，這是非常有用的。

<pre name="code" class="haskell:ghci">
ghci> takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]
[6,5,4]
ghci> takeWhile (/=' ') "This is a sentence"
"This"
</pre>

假定我們想要知道所有小於 10,000 的三次方的總和。我們無法映射 `(^3)` 到 `[1..]` 上、套用一個過濾、然後嘗試求其總和，因為過濾一個無限 list 永遠不會完成。你也許知道這裡所有的元素都是升序的，但 Haskell 並不知道。這就是為什麼我們必須這麼做：

<pre name="code" class="haskell:ghci">
ghci> sum $ takeWhile (<10000) $ map (^3) [1..]
53361
</pre>

我們套用 `(^3)` 到一個無限 list，然後一旦碰到一個超過 10,000 的元素，這個 list 就在此截斷。現在我們可以輕易地加總它。

<code class="label function">dropWhile</code> 也很類似，只是它是在述部為真的時候丟棄所有的元素。一旦述部等於 `False`，它就傳回 list 剩下的部份。一個既有用又可愛的 function！

<pre name="code" class="haskell:ghci">
ghci> dropWhile (/=' ') "This is a sentence"
" is a sentence"
ghci> dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]
[3,4,5,4,3,2,1]
</pre>

我們給定一個代表按照日期的股票價值的 list。這個 list 以第一項為股票價值、第二項為年份、第三項為月份、第四項為日期的 tuple 組成。我們想知道何時股票價值首次超過一千元！

<pre name="code" class="haskell:ghci">
ghci> let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
ghci> head (dropWhile (\(val,y,m,d) -> val < 1000) stock)
(1001.4,2008,9,4)
</pre>

<code class="label function">span</code> 有點像是 `takeWhile`，只是它回傳的是一對 list。第一個 list 包含所有若是以相同的述部與相同的 list 呼叫 `takeWhile` 產生的 list 會包含的所有值。第二個 list 包含 list
被丟棄的部分。

<pre name="code" class="haskell:ghci">
ghci> let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest
"First word:
This, the rest:
is a sentence"
</pre>

`span` 在述部為真的時候延展（span） list，而 <code class="label function">break</code> 則在述部首次為真的時候打斷它。進行 `break p` 等同於進行 `span (not . p)`。

<pre name="code" class="haskell:ghci">
ghci> break (==4) [1,2,3,4,5,6,7]
([1,2,3],[4,5,6,7])
ghci> span (/=4) [1,2,3,4,5,6,7]
([1,2,3],[4,5,6,7])
</pre>

在使用 `break` 的時候，結果中的第二個 list 將會從第一個滿足述部的元素開始。

<code class="label function">sort</code> 排序一個 list。list 中的元素型別必須為 `Ord` typeclass 的一員，因為假使一個 list 的元素無法以某種順序排列，這個 list 就無法被排序。

<pre name="code" class="haskell:ghci">
ghci> sort [8,5,3,2,1,6,4,2]
[1,2,2,3,4,5,6,8]
ghci> sort "This will be sorted soon"
"    Tbdeehiillnooorssstw"
</pre>

<code class="label function">group</code> 接收一個 list 並將相同的鄰近元素群組為 sublist。

<pre name="code" class="haskell:ghci">
ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]
</pre>

若是我們在群組 list 前排序它，我們可以找出每個元素在 list 中出現多少次。

<pre name="code" class="haskell:ghci">
ghci> map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
[(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]
</pre>

<code class="label function">inits</code> 與 <code class="label function">tails</code> 很像 `init` 與 `tail`，只是它們會遞迴地套用在 list 上，直到沒有任何值。看：

<pre name="code" class="haskell:ghci">
ghci> inits "w00t"
["","w","w0","w00","w00t"]
ghci> tails "w00t"
["w00t","00t","0t","t",""]
ghci> let w = "w00t" in zip (inits w) (tails w)
[("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]
</pre>

讓我們用摺疊來實作從 list 搜尋 sublist。

<pre name="code" class="haskell:hs">
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
</pre>

首先我們以我們要搜尋的 list 呼叫 `tails`。然後我們走遍每個 tail 並檢查它是否以我們在找的 sublist 開頭。

藉此，我們實際上建立了一個行為像是 <code class="label function">isInfixOf</code> 的 function。`isInfixOf` 在一個 list 中搜尋一個 sublist，並在我們尋找的 sublist 在目標 list 中的某處時回傳 `True`。

<pre name="code" class="haskell:ghci">
ghci> "cat" `isInfixOf` "im a cat burglar"
True
ghci> "Cat" `isInfixOf` "im a cat burglar"
False
ghci> "cats" `isInfixOf` "im a cat burglar"
False
</pre>

<code class="label function">isPrefixOf</code> 與 <code class="label function">isSuffixOf</code> 分別在 list 的開頭與結尾搜尋 sublist。

<pre name="code" class="haskell:ghci">
ghci> "hey" `isPrefixOf` "hey there!"
True
ghci> "hey" `isPrefixOf` "oh hey there!"
False
ghci> "there!" `isSuffixOf` "oh hey there!"
True
ghci> "there!" `isSuffixOf` "oh hey there"
False
</pre>

<code class="label function">elem</code> 與 <code class="label function">notElem</code> 檢查一個元素是否在或不在 list 中。

<code class="label function">partition</code> 接收一個 list 與一個述部，並回傳一對 list。結果中的第一個 list 包含所有滿足述部的元素，第二個 list 包含所有不滿足述部的元素。

<pre name="code" class="haskell:ghci">
ghci> partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
("BOBMORGAN","sidneyeddy")
ghci> partition (>3) [1,3,5,6,3,2,1,0,3,7]
([5,6,7],[1,3,3,2,1,0,3])
</pre>

理解這和 `span` 與 `break` 有何不同是很重要的：

<pre name="code" class="haskell:ghci">
ghci> span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
("BOB","sidneyMORGANeddy")
</pre>

一旦 `span` 與 `break` 遇到第一個不滿足或滿足述部的元素它就執行完成了，而`partition` 會走訪整個 list 並根據述部切割它。

<code class="label function">find</code> 接收一個 list 與一個述部，並回傳第一個滿足述部的元素。不過它傳回的是被包在一個 `Maybe` 值中的元素。我們要在下一章更深入地涵蓋代數（algebraic）資料型別。不過現在，你需要知道的是：一個 `Maybe` 可以為 `Just something` 或是 `Nothing`。非常像是一個 list 可以為一個空 list 或是一個有某些元素的 list，一個 `Maybe` 可以是沒有元素或是一個單一元素。並且像是整數 list 的型別為 `[Int]`，可能包含一個整數的型別為 `Maybe Int`。總而言之，讓我們來試試我們的 `find` function。

<pre name="code" class="haskell:ghci">
ghci> find (>4) [1,2,3,4,5,6]
Just 5
ghci> find (>9) [1,2,3,4,5,6]
Nothing
ghci> :t find
find :: (a -> Bool) -> [a] -> Maybe a
</pre>

注意到 `find` 的型別。它的結果為 `Maybe a`。這有點像是有個 `[a]` 型別，只是型別為 `Maybe` 的值可以包含零個元素或一個元素，而一個 list 可以包含零個元素、一個元素或是多個元素。

記得當我們在搜尋我們的股票第一次超過 $1000 的時候。我們執行 `head (dropWhile (\(val,y,m,d) -> val < 1000) stock)`。記得 `head` 並不是非常安全。若是我們的股票永遠不會超過 $1000 會發生什麼呢？我們的 `dropWhile` application 將會回傳一個空 list，而取得空 list 的 head 將會產生錯誤。然而，若是我們改寫成 `find (\(val,y,m,d) -> val > 1000) stock` 會更加安全。若是我們的股票永遠不會超過 $1000（所以沒有元素滿足述部），我們會得到一個 `Nothing`。但是在這個 list 有個合法的答案，我們會得到 `Just (1001.4,2008,9,4)`。

<code class="label function">elemIndex</code> 有點像是 `elem`，只是它並不是回傳一個布林值。它可能會回傳一個我們尋找的元素索引值。若是元素不在我們的 list 中，它會回傳一個 `Nothing`。

<pre name="code" class="haskell:ghci">
ghci> :t elemIndex
elemIndex :: (Eq a) => a -> [a] -> Maybe Int
ghci> 4 `elemIndex` [1,2,3,4,5,6]
Just 3
ghci> 10 `elemIndex` [1,2,3,4,5,6]
Nothing
</pre>

<code class="label function">elemIndices</code> 就像是 `elemIndex`，只是在我們尋找的元素在 list 多次出現的情況下，它回傳的是一個索引值 list。因為我們使用一個 list 來表示索引值，我們就不需要 `Maybe` 型別了，因為搜尋失敗可以被表示成空 list，而這幾乎就與 `Nothing` 同義。

<pre name="code" class="haskell:ghci">
ghci> ' ' `elemIndices` "Where are the spaces?"
[5,9,13]
</pre>

<code class="label function">findIndex</code> 就像是 `find`，但它會回傳第一個滿足述部的元素索引值。<code class="label function">findIndices</code> 以 list 的形式回傳所有滿足述部的元素索引值。

<pre name="code" class="haskell:ghci">
ghci> findIndex (==4) [5,3,2,1,6,4]
Just 5
ghci> findIndex (==7) [5,3,2,1,6,4]
Nothing
ghci> findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"
[0,6,10,14]
</pre>

我們已經看過 `zip` 與 `zipWith`。我們注意到它們將兩個 list 扣在一個 tuple 中、或是以一個二元 function（代表這種 function 接收兩個參數）扣上兩個 list。不過若是我們要扣上三個 list 呢？或是以一個接收三個參數的 function 扣上三個 list？嗯，為此，我們有 <code class="label function">zip3</code>、<code class="label function">zip4</code>、等等，以及 <code class="label function">zipWith3</code>、<code class="label function">zipWith4</code>、等等。這些變形至多到 7。雖然這看起來可能像個 hack，但是它運作地非常好，因為你不會有很多要將 8 個 list 扣在一起的機會。還有個將無限個 list 扣在一起的非常巧妙的方式，但我們還沒有到足以涵蓋它的水準。

<pre name="code" class="haskell:ghci">
ghci> zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]
[7,9,8]
ghci> zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]
[(2,2,5,2),(3,2,5,2),(3,2,3,2)]
</pre>

就像是一般的扣上動作，比最短的 list 長的 list 會被截斷。

<code class="label function">lines</code> 在處理檔案或是來自某處的輸入時，是個有用的 function。其接收一個字串，並以一個獨立的 list 回傳這個字串的每一行。

<pre name="code" class="haskell:ghci">
ghci> lines "first line\nsecond line\nthird line"
["first line","second line","third line"]
</pre>

`'\n'` 是 unix 換行的符號。在 Haskell 的字串與字元中，反斜線具有特殊意義。

<code class="label function">unlines</code> 是個 `lines` 的反函數（inverse function）。它接收一個字串的 list，並使用 `'\n'` 將它們串起來。

<pre name="code" class="haskell:ghci">
ghci> unlines ["first line", "second line", "third line"]
"first line\nsecond line\nthird line\n"
</pre>

<code class="label function">words</code> 與 <code class="label function">unwords</code> 用以將一行文字（text）切割成單詞（word），或是將單詞 list 串成文字。非常有用。

<pre name="code" class="haskell:ghci">
ghci> words "hey these are the words in this sentence"
["hey","these","are","the","words","in","this","sentence"]
ghci> words "hey these           are    the words in this\nsentence"
["hey","these","are","the","words","in","this","sentence"]
ghci> unwords ["hey","there","mate"]
"hey there mate"
</pre>

我們已經提過 <code class="label function">nub</code> 了。它接收一個 list 並去除重複的元素，回傳所有元素皆為獨一無二的 list。這個 function 有個有點奇怪的名字。"nub" 的意義原本是一小塊，或是某個東西的重要部分。在我看來，它需要用個實際的詞來作為 function 名稱，而不是用過時的詞彙。

<pre name="code" class="haskell:ghci">
ghci> nub [1,2,3,4,3,2,1,2,3,4,3,2,1]
[1,2,3,4]
ghci> nub "Lots of words and stuff"
"Lots fwrdanu"
</pre>

<code class="label function">delete</code> 接收一個元素與一個 list，並在這個元素第一次在 list 中出現時刪除它。

<pre name="code" class="haskell:ghci">
ghci> delete 'h' "hey there ghang!"
"ey there ghang!"
ghci> delete 'h' . delete 'h' $ "hey there ghang!"
"ey tere ghang!"
ghci> delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"
"ey tere gang!"
</pre>

<code class="label function">\\\\</code> 是個 list 差異（difference）function。它基本上就像是集合差異。對於每個在右手邊 list 中的元素，它會刪除左邊 list 對應的元素。

<pre name="code" class="haskell:ghci">
ghci> [1..10] \\ [2,5,9]
[1,3,4,6,7,8,10]
ghci> "Im a big baby" \\ "big"
"Im a  baby"
</pre>

執行 `[1..10] \\ [2,5,9]` 就像是執行 `delete 2 . delete 5 . delete 9 $ [1..10]`。

<code class="label function">union</code> 也像是個集合上的 function。它傳回兩個 list 的聯集。它走遍在第二個 list 的每個元素，若是它還不在第一個 list 中，它就將它附加到第一個 list 上。注意看，來自第二個 list 的重複元素都被刪除了！

<pre name="code" class="haskell:ghci">
ghci> "hey man" `union` "man what's up"
"hey manwt'sup"
ghci> [1..7] `union` [5..10]
[1,2,3,4,5,6,7,8,9,10]
</pre>

<code class="label function">intersect</code> 運作得像是集合的交集。它只回傳在兩個 list 中都有的元素。

<pre name="code" class="haskell:ghci">
ghci> [1..7] `intersect` [5..10]
[5,6,7]
</pre>

<code class="label function">insert</code> 接收一個可以被排序的元素與一個元素 list，並將它插到令它仍然小於或等於下一個元素的最後一個位置。換句話說，`insert`將從 list 的開頭開始，然後繼續前進，直到它找到一個等於或大於我們要插入的元素的元素，並將它插入到這個元素之前。

<pre name="code" class="haskell:ghci">
ghci> insert 4 [3,5,1,2,8,2]
[3,4,5,1,2,8,2]
ghci> insert 4 [1,3,4,4,1]
[1,3,4,4,4,1]
</pre>

在第一個範例 `4` 被插入到 `3` 之後與 `5` 之前，且在第二個範例的 `3` 與 `4` 之間。

若是我們使用 `insert` 來插入到一個已排序的 list，則產生的 list 也會是已排序的<span class="note">〔譯註：list 必須以升序排序才符合這句敘述〕</span>。

<pre name="code" class="haskell:ghci">
ghci> insert 4 [1,2,3,5,6,7]
[1,2,3,4,5,6,7]
ghci> insert 'g' $ ['a'..'f'] ++ ['h'..'z']
"abcdefghijklmnopqrstuvwxyz"
ghci> insert 3 [1,2,4,3,2,1]
[1,2,3,4,3,2,1]
</pre>

`length`、`take`、`drop`、`splitAt`、`!!` 與 `replicate` 的共通點在於，它們都取 `Int` 作為它們的第一個參數（或是傳回一個 `Int`），即使它們取任何為 `Integral` 或 `Num` typeclass（視 function 而定）一員的型別，就能夠令它們更加一般化且便於使用。這麼做是為了歷史因素。然而，修正它可能會破壞許多既有的程式碼。這就是為什麼 `Data.List` 有它們更一般化的版本，叫做 <code class="label function">genericLength</code>、<code class="label function">genericTake</code>、<code class="label function">genericDrop</code>、<code class="label function">genericSplitAt</code>、<code class="label function">genericIndex</code> 與 <code class="label function">genericReplicate</code>。舉例來說，`length` 的型別簽名為 `length :: [a] -> Int`。若是我們嘗試藉由 `let xs = [1..6] in sum xs / length xs` 取一個數字 list 的平均，我們會得到一個錯誤，因為你無法對一個 `Int` 使用 `/`。另一方面，`genericLength` 的型別簽名為 `genericLength :: (Num a) => [b] -> a`。因為 `Num` 可以作為一個浮點數，藉由 `let xs = [1..6] in sum xs / genericLength xs` 取平均運作良好。

`nub`、`delete`、`union`、`intersect` 與 `group` function 全都有它們的一般化對應，叫做 <code class="label function">nubBy</code>、<code class="label function">deleteBy</code>、<code class="label function">unionBy</code>、<code class="label function">intersectBy</code> 與 <code class="label function">groupBy</code>。它們之中的不同在於，第一組 function 使用 `==` 來測試相等性，而 <i>By</i> 那組還接收了一個相等性 function，然後藉由這個相等性 function 來做比較。`group` 等同於 `groupBy (==)`。

舉例來說，假使我們有一個描述每一秒 function 值的 list。我們要基於值在小於零，與大於零的時機將它切割成 sublist。若是我們僅以一般的 `group` 來做，它只會把相同的鄰近值群組在一起。但我們要的是藉由它們是否為負值來群組它們。該是 `groupBy` 表現的時候了！提供給這個 <i>By</i> function 的相等性 function 需要接收兩個相同型別的元素，並在它根據其標準將值視為相等時回傳 `True`。

<pre name="code" class="haskell:ghci">
ghci> let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]
ghci> groupBy (\x y -> (x > 0) == (y > 0)) values
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
</pre>

藉此，我們清楚地看到哪個部分是正的、哪個是負的。提供的相等性 function 接收兩個元素，然後只在它們都為負的或都為正的情況回傳 `True`。這個相等性 function 也可以被寫作 `\x y -> (x > 0) && (y > 0) || (x <= 0) && (y <= 0)`，雖然我想第一種方式更加易讀。若是你從 `Data.Function` 引入 <code class="label function">on</code> function，有一個更清楚的方式來為 <i>By</i> function 撰寫相等性 function。`on` 像這樣被定義：

<pre name="code" class="haskell:hs">
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)
</pre>

所以執行 ``(==) `on` (> 0)`` 回傳一個看起來像是 `\x y -> (x > 0) == (y > 0)` 的相等性 function。`on` 經常與 <i>By</i> function 一同使用，因為搭配它，我們可以：

<pre name="code" class="haskell:ghci">
ghci> groupBy ((==) `on` (> 0)) values
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
</pre>

的確非常好讀！你可以大聲的閱讀它：藉由元素是否大於 0 的結果相等性來群組它（Group this by equality on whether the elements are greater than zero）。

相似的，`sort`、`insert`、`maximum` 與 `minimum` 也有它們更加一般化的版本。像是 `groupBy` 的 function 取一個判斷兩個元素是否相等的 function。<code class="label function">sortBy</code>、<code class="label function">insertBy</code>、<code class="label function">maximumBy</code> 與 <code class="label function">minimumBy</code> 取一個判斷元素是否大於、小於或等於另一個值的 function。`sortBy` 的型別簽名為 `sortBy :: (a -> a -> Ordering) -> [a] -> [a]`。假如你還記得之前提過，`Ordering` 型別可以為 `LT`、`EQ` 或是 `GT`。`sort` 是 `sortBy compare` 的等價物，因為 `compare` 取兩個型別為 `Ord` typeclass 的元素，並回傳它們的順序關係。

list 可以被比較，但當它們被比較的時候，它們是以字典順序比較。若是我們有個 list 的 list，而我們不是基於內部 list 的內容，而是根據它們的長度來排序它呢？嗯，如同你可能已經猜到的，我們要使用 `sortBy` function。

<pre name="code" class="haskell:ghci">
ghci> let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
ghci> sortBy (compare `on` length) xs
[[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]
</pre>

太棒了！``compare `on` length`` ... 嗯，這讀起來幾乎像是真的英文！如果你不確定這裡 `on` 是如何運作的，``compare `on` length`` 等同於 ``\x y -> length x `compare` length y``。當你處理取一個相等性 function 的 <i>By</i> function 的時候，你常會進行 ``(==) `on` something``；而當你處理接收一個次序 function，你常會進行 ``compare `on` something``。

## <a name="data-char">Data.Char</a>

<img src="img/legochar.png" alt="lego char" style="float:right" />
`Data.Char` 模組做的就是它的名稱所暗示的。它輸出處理字元的 function。它在過濾與映射到字串上時也是有用的，因為字串僅是字元的 list。

`Data.Char` 輸出一些處理字元的述部。這即是：接收一個字元，並告訴我們某些關於它的假設是否為真的 function。以下這些即是：

<code class="label function">isControl</code> 檢查一個字元是否是個控制字元。

<code class="label function">isSpace</code> 檢查一個字元是否是個空白字元。這包含了空白、tab 字元、換行、等等。

<code class="label function">isLower</code> 檢查一個字元是否為小寫。

<code class="label function">isUpper</code> 檢查一個字元是否為大寫。

<code class="label function">isAlpha</code> 檢查一個字元是否是個字母（letter）。

<code class="label function">isAlphaNum</code> 檢查一個字元是否是個字母或是個數字。

<code class="label function">isPrint</code> 檢查一個字元是否可印（printable）。舉例來說，控制字元不可印。

<code class="label function">isDigit</code> 檢查一個字元是否是個數字<span class="note">〔譯註：包含 `0..9`〕</span>。

<code class="label function">isOctDigit</code> 檢查一個字元是否是個八進位數字<span class="note">〔譯註：包含 `0..7`〕</span>。

<code class="label function">isHexDigit</code> 檢查一個字元是否是個十六進位數字<span class="note">〔譯註：包含 `0..9`、`a..f`、與 `A..F]`〕</span>。

<code class="label function">isLetter</code> 檢查一個字元是否是個字母<span class="note">〔譯註：等同於 `isAlpha`〕</span>。

<code class="label function">isMark</code> 檢查 Unicode 標記（mark）字元。這些是結合前面的字母以組成帶口音字母的字元。如果你是法國人就用它吧。

<code class="label function">isNumber</code> 檢查一個字元是否為數字<span class="note">〔譯註：除了 `0..9` 之外，亦包含其他 Unicode 的數字符號。請參考 Wikipedia 的 [Numerals in Unicode](http://en.wikipedia.org/wiki/Numerals_in_Unicode) 條目〕</span>。

<code class="label function">isPunctuation</code> 檢查一個字元是否為標點符號。

<code class="label function">isSymbol</code> 檢查一個字元是否是個花俏的數學或貨幣符號。

<code class="label function">isSeparator</code> 檢查 Unicode 空白與分隔字元。

<code class="label function">isAscii</code> 檢查一個字元是否落在 Unicode 字元集的前 128 個字元。

<code class="label function">isLatin1</code> 檢查一個字元是否落在 Unicode 字元集的前 256 個字元。

<code class="label function">isAsciiUpper</code> 檢查一個字元是否為 ASCII 大寫字元。

<code class="label function">isAsciiLower</code> 檢查一個字元是否為 ASCII 小寫字元。

所有這些述部的型別都是 `Char -> Bool`。很多時候你會使用它來過濾字串或是類似的東西。舉例來說，讓我們假設我們要建立一個接收使用者名稱，且使用者名稱只能以字母和數字組成的程式。我們可以使用 `Data.List` 的 `all` function 結合 `Data.Char` 的述部來判斷使用者名稱是否正確。

<pre name="code" class="haskell:ghci">
ghci> all isAlphaNum "bobby283"
True
ghci> all isAlphaNum "eddy the fish!"
False
</pre>

酷。萬一你不記得，`all` 接收一個述部與一個 list，並只在述部對每個 list 中的元素成立時回傳 `True`。

我們也可以使用 `isSpace` 來模擬 `Data.List` 的 `words` function。

<pre name="code" class="haskell:ghci">
ghci> words "hey guys its me"
["hey","guys","its","me"]
ghci> groupBy ((==) `on` isSpace) "hey guys its me"
["hey"," ","guys"," ","its"," ","me"]
ghci>
</pre>

唔，好，有點 `words` 的樣子了，但是我們還留著只有空白的元素。唔，有什麼是我們該做的呢？我知道了，讓我們過濾它們。

<pre name="code" class="haskell:ghci">
ghci> filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"
["hey","guys","its","me"]
</pre>

阿。

`Data.Char` 也輸出一個有點像是 `Ordering` 的資料型別。`Ordering` 型別可以為 `LT`、`EQ` 或是 `GT`。這是一種列舉。它描述了一些比較兩個元素可以出現的可能結果。`GeneralCategory` 型別也是個列舉。它提供給我們一些字元可能落入的分類（category）。要取得一個字元大體分類的主要 function 為 `generalCategory`。它的型別為 `generalCategory :: Char -> GeneralCategory`。大約有 31 種分類，所以我們不會在這裡一一列出它們，但讓我們來玩玩這個 function。

<pre name="code" class="haskell:ghci">
ghci> generalCategory ' '
Space
ghci> generalCategory 'A'
UppercaseLetter
ghci> generalCategory 'a'
LowercaseLetter
ghci> generalCategory '.'
OtherPunctuation
ghci> generalCategory '9'
DecimalNumber
ghci> map generalCategory " \t\nA9?|"
[Space,Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]
</pre>

由於 `GeneralCategory` 型別為 `Eq` typeclass 的一員，所以我們可以做像 `generalCategory c == Space` 這樣的測試。

<code class="label function">toUpper</code> 將一個字元轉成大寫。空白、數字、與類似的字元不會被改變。

<code class="label function">toLower</code> 將一個字元轉成小寫。

<code class="label function">toTitle</code> 將一個字元轉成 title-case。對於大部分的字元，title-case 與大寫相同。

<code class="label function">digitToInt</code> 將一個字元轉成一個 `Int`。要成功轉換，字元必須在 `'0'..'9'`、`'a'..'f'` 或是 `'A'..'F'` 的範圍中。

<pre name="code" class="haskell:ghci">
ghci> map digitToInt "34538"
[3,4,5,3,8]
ghci> map digitToInt "FF85AB"
[15,15,8,5,10,11]
</pre>

<code class="label function">intToDigit</code> 為 `digitToInt` 的反函數。它取一個介在 `0..15` 之間的 `Int`，並將它轉成一個小寫字元。

<pre name="code" class="haskell:ghci">
ghci> intToDigit 15
'f'
ghci> intToDigit 5
'5'
</pre>

<code class="label function">ord</code> 與 <code class="label function">chr</code> function 將字元轉成其對應的數字，或是將數字轉成其對應的字元：

<pre name="code" class="haskell:ghci">
ghci> ord 'a'
97
ghci> chr 97
'a'
ghci> map ord "abcdefgh"
[97,98,99,100,101,102,103,104]
</pre>

兩個字元 `ord` 值之間的差距等同於它們在 Unicode 表格中間隔多遠。

凱薩密碼（Caesar cipher）是個藉由在字母表中以固定數量的位置偏移（shift）訊息中的每個字元，以編碼（encode）訊息的原始方法。我們可以輕易地建立一種我們自己的凱薩密碼，只是我們不會受限於字母表。

<pre name="code" class="haskell:hs">
encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted
</pre>

這裡，我們先將字串轉換成一組數字 list。這時在將數字 list 轉換回字元之前，我們將每個數字加上偏移量。如果你是個組合牛仔，你可以將這個 function 的主體寫成 `map (chr . (+ shift) . ord) msg`。讓我們試著編碼一些訊息。

<pre name="code" class="haskell:ghci">
ghci> encode 3 "Heeeeey"
"Khhhhh|"
ghci> encode 4 "Heeeeey"
"Liiiii}"
ghci> encode 1 "abcd"
"bcde"
ghci> encode 5 "Marry Christmas! Ho ho ho!"
"Rfww~%Hmwnxyrfx&%Mt%mt%mt&"
</pre>

編碼沒問題了。解碼（decode）一個訊息基本上只是以第一次偏移的數量，將它偏移回去。

<pre name="code" class="haskell:hs">
decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg
</pre>

<pre name="code" class="haskell:ghci">
ghci> encode 3 "Im a little teapot"
"Lp#d#olwwoh#whdsrw"
ghci> decode 3 "Lp#d#olwwoh#whdsrw"
"Im a little teapot"
ghci> decode 5 . encode 5 $ "This is a sentence"
"This is a sentence"
</pre>

## <a name="data-map">Data.Map</a>

關聯列表（association list，也被稱為字典）是用以儲存 key-value pair，而順序無關緊要的 list。舉例來說，我們可能會使用一個關聯列表來儲存電話號碼，其中電話號碼會是 value，而人名會是 key。我們並不關心它儲存的順序，我們只想要為正確的人取得正確的電話號碼。

在 Haskell 中，藉由一個 pair list 表達關聯列表是個最明顯的作法。pair 中的第一項會是 key，第二項是 value。這裡有個電話號碼的關聯列表範例：

<pre name="code" class="haskell:hs">
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]
</pre>

儘管這似乎縮排地很怪，但這就是個字串的 pair list。在處理關聯列表時，最常見的任務是藉由 key 來尋找某個 value。讓我們建立一個給定一個 key 以尋找某個 value 的 function。

<pre name="code" class="haskell:hs">
findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs
</pre>

十分簡單。這個 function 接收一個 key 與一個 list、過濾 list 使得只有匹配的 key 被留下來、取出第一個匹配的 key-value 並回傳 value。但如果我們尋找的 key 不在關聯 list 裡頭會發生什麼呢？唔。在這裡，假使一個 key 不在關聯列表中，我們最後會試著取一個空 list 的 head，這會拋出一個執行期錯誤。然而，我們需要避免讓我們的程式這麼簡單就崩潰，所以讓我們使用 `Maybe` 資料型別。若是我們沒有找到 key，我們就回傳一個 `Nothing`。若是我們找到它，我們就回傳 `Just something`，其中 something 為對應到這個 key 的 value。

<pre name="code" class="haskell:hs">
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k
                            then Just v
                            else findKey key xs
</pre>

看看型別宣告。它取一個可以被比較相等性的 key 與一個關聯列表，然後它或許會產生一個值。聽起來是對的。

這是一個處理一個 list 的標準遞迴 function。邊界案例、將一個 list 切割成 head 與 tail、遞迴呼叫，全都有了。這是個典型的摺疊模式，所以讓我們看看如何將它以摺疊實作。

<pre name="code" class="haskell:hs">
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
</pre>

<p class="hint">
<em>註記：</em>為這種標準的 list 遞迴模式使用摺疊，而不是明確寫出遞迴通常是比較好的，因為它更加容易閱讀與辨認。當人們看到 <code>foldr</code> 呼叫的時候，每個人都知道它是個摺疊，但閱讀明確的遞迴需要更多的思考。
</p>

<pre name="code" class="haskell:ghci">
ghci> findKey "penny" phoneBook
Just "853-2492"
ghci> findKey "betty" phoneBook
Just "555-2938"
ghci> findKey "wilma" phoneBook
Nothing
</pre>

<img src="img/legomap.png" alt="legomap" style="float:left" />
太不可思議了！假如我們有女孩的電話號碼，我們「就」得到了這個號碼（we `Just` get the number），否則我們什麼也拿不到（we get `Nothing`）。

我們剛才實作了 `Data.List` 裡的 `lookup` function。假如我們想要尋找對應一個 key 的 value，我們必須要尋訪 list 中的所有元素，直到我們找到它。`Data.Map` 模組提供更快速的關聯列表（因為它以樹進行內部實作），它也提供很多實用的 function。從現在開始，我們將假設我們用的是 map 而非關聯列表。

因為 `Data.Map` 會輸出與 `Prelude` 和 `Data.List` 衝突的 function，所以我們要進行限制引入。

<pre name="code" class="haskell:hs">
import qualified Data.Map as Map
</pre>

將這個引入敘述擺在腳本裡，然後透過 GHCI 載入腳本。

讓我們繼續前進，看看等著我們的 `Data.Map` 有些什麼！以下為其 function 的概要。

<code class="label function">fromList</code> function 接收一個關聯列表（以一個 list 的形式），並回傳具有相同關聯的 map。

<pre name="code" class="haskell:ghci">
ghci> Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
ghci> Map.fromList [(1,2),(3,4),(3,2),(5,5)]
fromList [(1,2),(3,2),(5,5)]
</pre>

若是在原始的關聯列表裡有重複的 key，重複的那個就會被丟棄。這是 `fromList` 的型別簽名：

<pre name="code" class="haskell:hs">
Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v
</pre>

這表明它取一個型別為 `k` 與 `v` 的 pair list，並回傳一個從型別為 `k` 的 key 映射到型別 `v` 的 map。注意到，當你以一般的 list 表示關聯列表時，key 只要可以比較相等性（其型別屬於`Eq` typeclass）即可，但現在它必須要是可以排序的。這是在 `Data.Map` 模組中的一個本質限制。它需要 key 是可以排序的，以讓它能夠在一棵樹中排列它們。

你始終都應該使用 `Data.Map` 表示 key-value 關聯，除非你有個並非為 `Ord` typeclass 一員的 key。

<code class="label function">empty</code> 表示一個空 map。它不接收引數，它僅回傳一個空的 map。

<pre name="code" class="haskell:ghci">
ghci> Map.empty
fromList []
</pre>

<code class="label function">insert</code> 接收一個 key、一個 value 與一個 map，並回傳一個就像是舊的 map，只是插入 key 與 value 的 map。

<pre name="code" class="haskell:ghci">
ghci> Map.empty
fromList []
ghci> Map.insert 3 100 Map.empty
fromList [(3,100)]
ghci> Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))
fromList [(3,100),(4,200),(5,600)]
ghci> Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty
fromList [(3,100),(4,200),(5,600)]
</pre>

我們可以藉由使用空 map、`insert` 與摺疊來實作我們自己的 `fromList`。看：

<pre name="code" class="haskell:hs">
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty
</pre>

這是個非常直觀的摺疊。我們以一個空 map 開始，我們將它從右開始摺疊，並在進行時將 key-value pair 插入到累加器。

<code class="label function">null</code> 檢查 map 是否為空。

<pre name="code" class="haskell:ghci">
ghci> Map.null Map.empty
True
ghci> Map.null $ Map.fromList [(2,3),(5,5)]
False
</pre>

<code class="label function">size</code> 回報 map 的大小。

<pre name="code" class="haskell:ghci">
ghci> Map.size Map.empty
0
ghci> Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]
5
</pre>

<code class="label function">singleton</code> 接收一個 key 與一個 value，並建立一個僅有一組映射的 map。

<pre name="code" class="haskell:ghci">
ghci> Map.singleton 3 9
fromList [(3,9)]
ghci> Map.insert 5 9 $ Map.singleton 3 9
fromList [(3,9),(5,9)]
</pre>

<code class="label function">lookup</code> 如同 `Data.List` `lookup` 那般運作，只是它操作的是 map。它會在它找到對應 key 的 something 時回傳 `Just something`，並在沒找到時回傳 `Nothing`。

<code class="label function">member</code> 為一個接收一個 key 與一個 map，並回報 key 是否在 map 中的述部。

<pre name="code" class="haskell:ghci">
ghci> Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]
True
ghci> Map.member 3 $ Map.fromList [(2,5),(4,5)]
False
</pre>

<code class="label function">map</code> 與 <code class="label function">filter</code> 運作地非常像是它們的 list 版本。

<pre name="code" class="haskell:ghci">
ghci> Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]
fromList [(1,100),(2,400),(3,900)]
ghci> Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]
fromList [(2,'A'),(4,'B')]
</pre>

<code class="label function">toList</code> 為 `fromList` 的反函數。

<pre name="code" class="haskell:ghci">
ghci> Map.toList . Map.insert 9 2 $ Map.singleton 4 3
[(4,3),(9,2)]
</pre>

<code class="label function">keys</code> 與 <code class="label function">elems</code> 分別傳回 key 與 value 的 list。`keys` 等同於 `map fst . Map.toList` 且 `elems` 等同於 `map snd . Map.toList`。

<code class="label function">fromListWith</code> 是個很酷的小 function。它就像 `fromList`，只是它並不丟棄重複的 key，而是使用提供給它的 function 來決定該對它做什麼。讓我們假定一個女孩可以有多個號碼，而我們有個像這樣設置的關聯列表：

<pre name="code" class="haskell:hs">
phoneBook =
    [("betty","555-2938")
    ,("betty","342-2492")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","943-2929")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")
    ]
</pre>

現在若是我們使用 `fromList` 來將它擺進一個 map，我們會丟失一些號碼！所以這裡我們要這麼做：

<pre name="code" class="haskell:hs">
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs
</pre>

<pre name="code" class="haskell:ghci">
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
"827-9162, 943-2929, 493-2928"
ghci> Map.lookup "wendy" $ phoneBookToMap phoneBook
"939-8282"
ghci> Map.lookup "betty" $ phoneBookToMap phoneBook
"342-2492, 555-2938"
</pre>

假如一個重複的 key 被找到，我們傳遞的 function 會被用來將這些 key 的 value 結合成某個別的 value。我們也可以先將關聯列表中所有的 value 建成單一元素的 list，然後我們可以使用 `++` 來結合號碼。

<pre name="code" class="haskell:hs">
phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs
</pre>

<pre name="code" class="haskell:ghci">
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
["827-9162","943-2929","493-2928"]
</pre>

非常好！另一個使用情況是，假如我們要從一個數字的關聯列表建立一個 map，且在一個重複的 key 被找到時，我們要保留這個 key 的最大 value。

<pre name="code" class="haskell:ghci">
ghci> Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
fromList [(2,100),(3,29),(4,22)]
</pre>

或是，我們可以選擇加總相同 key 的 value。

<pre name="code" class="haskell:ghci">
ghci> Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
fromList [(2,108),(3,62),(4,37)]
</pre>

<code class="label function">insertWith</code> 之於 `insert` 猶如 `fromListWith` 之於 `fromList`。它將一個 key-value pair 插入到一個 map 中，但若是這個 map 已經包含這個 key，它會使用傳遞給它的 function 來決定該做什麼。

<pre name="code" class="haskell:ghci">
ghci> Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]
fromList [(3,104),(5,103),(6,339)]
</pre>

還有一些 `Data.Map` 的 function。你可以在這份[文件](http://www.haskell.org/ghc/docs/latest/html/libraries/containers/Data-Map.html#v%3Aassocs)中看到一份完整的清單。

## <a name="data-set">Data.Set</a>

<img src="img/legosets.png" alt="legosets" style="float:right" />
`Data.Set` 模組提供給我們，嗯，set。像是數學中的集合。set 有點像是 list 與 map 之間的過渡。所有在一個 set 裡的元素都是唯一的。且因為它是以樹進行內部實作（非常像是 `Data.Map` 中的 map），所以它們是有順序的。檢查成員關係、插入、刪除等等，比起用 list 做相同的事要快得多。處理 set 時最常見的操作是插入到一個 set、檢查成員關係以及將一個 set 轉換成一個 list。

因為在 `Data.Set` 中的名稱會與許多 `Prelude` 與 `Data.List` 的名稱衝突，所以我們要進行限制引入。

將這個引入敘述擺進腳本中：

<pre name="code" class="haskell:hs">
import qualified Data.Set as Set
</pre>

然後透過 GHCI 載入腳本。

讓我們假設我們有兩段文字。我們想要找出有哪些字元同時被使用在兩者之中。

<pre name="code" class="haskell:hs">
text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"
</pre>

<code class="label function">fromList</code> function 運作地非常像你所預期的。它取一個 list 並將它轉換成一個 set。

<pre name="code" class="haskell:ghci">
ghci> let set1 = Set.fromList text1
ghci> let set2 = Set.fromList text2
ghci> set1
fromList " .?AIRadefhijlmnorstuy"
ghci> set2
fromList " !Tabcdefghilmnorstuvwy"
</pre>

如你所見，元素是有順序的，且每個元素都是唯一的。現在讓我們使用 <code class="label function">intersection</code> function 來看看它們共享的元素。

<pre name="code" class="haskell:ghci">
ghci> Set.intersection set1 set2
fromList " adefhilmnorstuy"
</pre>

我們可以使用 <code class="label function">difference</code> function 來看看哪個字母在第一個 set 中，但不在第二個 set 中，反之亦然。

<pre name="code" class="haskell:ghci">
ghci> Set.difference set1 set2
fromList ".?AIRj"
ghci> Set.difference set2 set1
fromList "!Tbcgvw"
</pre>

或者，我們可以藉由使用 <code class="label function">union</code> 來看看所有被使用在兩個段落中的唯一字母。

<pre name="code" class="haskell:ghci">
ghci> Set.union set1 set2
fromList " !.?AIRTabcdefghijlmnorstuvwy"
</pre>

<code class="label function">null</code>、<code class="label function">size</code>、<code class="label function">member</code>、<code class="label function">empty</code>、<code class="label function">singleton</code>、<code class="label function">insert</code> 與 <code class="label function">delete</code> function 全都如你所預期地運作。

<pre name="code" class="haskell:ghci">
ghci> Set.null Set.empty
True
ghci> Set.null $ Set.fromList [3,4,5,5,4,3]
False
ghci> Set.size $ Set.fromList [3,4,5,3,4,5]
3
ghci> Set.singleton 9
fromList [9]
ghci> Set.insert 4 $ Set.fromList [9,3,8,1]
fromList [1,3,4,8,9]
ghci> Set.insert 8 $ Set.fromList [5..10]
fromList [5,6,7,8,9,10]
ghci> Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]
fromList [3,5]
</pre>

我們也可以檢驗子集（subset）或是嚴格子集（proper subset）。若是集合 B 包含所有集合 A 包含的所有元素，A 就是 B 的子集。若是集合 B 包含所有集合 A 包含的所有元素，但還擁有更多的元素，A 就是 B 的嚴格子集。

<pre name="code" class="haskell:ghci">
ghci> Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
True
ghci> Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
True
ghci> Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]
False
ghci> Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
False
</pre>

我們也可以套用 <code class="label function">map</code> 在 set 上與 <code class="label function">filter</code> 它。

<pre name="code" class="haskell:ghci">
ghci> Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]
fromList [3,5,7]
ghci> Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]
fromList [3,4,5,6,7,8]
</pre>

set 通常藉由先將 list 以 `fromList` 建成一個 set，然後再將它以 <code class="label function">toList</code> 轉回成一個 list 來從一個 list 中去除重複元素。雖然 `Data.List` 的 `nub` function 已經能做到這件事，但如果你將一個大 list 填入 set 中，然後將它轉換回一個 list 來去除重複元素，比起使用 `nub` 來得更快。但使用 `nub` 時 list 的元素型別只要求為 `Eq` typeclass 的一員，而若是你想要將元素填入一個 set，list 的型別則必須屬於 `Ord`。

<pre name="code" class="haskell:ghci">
ghci> let setNub xs = Set.toList $ Set.fromList xs
ghci> setNub "HEY WHATS CRACKALACKIN"
" ACEHIKLNRSTWY"
ghci> nub "HEY WHATS CRACKALACKIN"
"HEY WATSCRKLIN"
</pre>

<code class="label function">setNub</code> 處理大 list 通常比 `nub` 還快，但如你所見，`nub` 會保留 list 元素的順序，而 `setNub` 則否。

## <a name="making-our-own-modules">建立我們自己的模組</a>

<img src="img/making_modules.png" alt="making modules" style="float:right" />
我們至今已經看過許多很酷的模組，但是我們要如何做個我們自己的模組呢？幾乎每個程式語言都使你能夠將你的程式碼切割成多個檔案，Haskell 也不例外。在建立程式時，將有著類似用途的 function 與型別擺進一個模組是個好習慣。這樣一來，你可以在其他程式中藉由引入你的模組輕易地重用這些 function。

讓我們藉由建立一個提供一些計算幾何物件體積與面積的小模組，來看看我們可以如何建立我們自己的模組。我們要從建立一個叫做 `Geometry.hs` 的檔案開始。

我們假設一個模組輸出多個 function。意思是，當我引入一個模組時，我可以使用它所輸出的 function。它可以定義它的 function 在內部呼叫的 function，但我們只能看到並使用它輸出的那些。

在一個模組的開頭，我們指定模組的名稱。若是我們有個叫做 `Geometry.hs` 的檔案，這時我們就需要將我們的模組命名為 `Geometry`。然後，我們指定它所輸出的 function，在這之後我們就可以開始撰寫這些 function。所以我們要從這個開始：

<pre name="code" class="haskell:hs">
module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where
</pre>

如你所見，我們要為球體、立方體與長方體求取面積與體積。繼續向前，接著定義我們的 function：

<pre name="code" class="haskell:ghci">
module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b
</pre>

非常標準的幾何學就在這裡。還有一些需要注意的東西。因為一個立方體僅是長方體的一個特例，所以我們藉由將它視為一個邊長全都相同的長方體來定義它的面積與體積。我們也定義一個叫做 `rectangleArea` 的輔助 function，其基於矩型的邊長計算矩型面積。這比較普通，因為它僅僅是乘法而已。注意到我們將它使用在我們的模組中的 function 裡（即 `cuboidArea` 與 `cuboidVolume`），但我們並不輸出它！因為我們想讓我們的模組只提供處理三維物件的 function，所以我們使用 `rectangleArea` 但不輸出它。

建立一個模組時，我們通常只輸出作為我們模組介面的那些 function 以隱藏實作細節。若是某個人使用了我們的 `Geometry` 模組，他不必關心我們沒有輸出的 function。我們可以決定完全改變這些 function，或是在一個比較新的版本刪除它們（我們可以刪除 `rectangleArea` 並僅以 `*` 取代），且因為我們並沒有輸出它們，所以沒有人會在意這些改變。

要使用我們的模組，我們只需要：

<pre name="code" class="haskell:hs">
import Geometry
</pre>

`Geometry.hs` 必須在與引入它的程式所在的相同目錄中。

模組也可以被給予一階層結構。每個模組可以擁有一些子模組，而它們也可以有它們自己的子模組。讓我們將這些 function 分組，使得 `Geometry` 為一個擁有三個子模組──每個對應一個物件型別──的模組。

首先，我們要建立一個叫做 `Geometry` 的目錄。留心開頭大寫字母 G。在其中，我們要擺進三個檔案：`Sphere.hs`、`Cuboid.hs`、與 `Cube.hs`。以下是檔案將會包含的內容：

`Sphere.hs`

<pre name="code" class="haskell:hs">
module Geometry.Sphere
( volume
, area
) where

volume :: Float -> Float
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)

area :: Float -> Float
area radius = 4 * pi * (radius ^ 2)
</pre>

`Cuboid.hs`

<pre name="code" class="haskell:hs">
module Geometry.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float
volume a b c = rectangleArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b
</pre>

`Cube.hs`

<pre name="code" class="haskell:hs">
module Geometry.Cube
( volume
, area
) where

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volume side = Cuboid.volume side side side

area :: Float -> Float
area side = Cuboid.area side side side
</pre>

非常好！所以首先是 `Geometry.Sphere`。注意到我們將它擺進一個叫做 `Geometry` 的目錄中，然後將模組名稱定義為 `Geometry.Sphere`。我們也對長方體做一樣的事。同樣注意到我們如何在三個子模組中定義有著相同名稱的 function。因為它們為獨立的模組，所以我們可以這麼做。我們要在 `Geometry.Cube` 使用 `Geometry.Cuboid` 的 function，但我們無法直接進行 `import Geometry.Cuboid`，因為它輸出與 `Geometry.Cube` 相同名稱的 function。這就是為什麼我們要進行限制引入，且一切良好的原因。

所以現在假使我們在一個與 `Geometry` 目錄同層級的檔案中，我們可以做：

<pre name="code" class="haskell:hs">
import Geometry.Sphere
</pre>

然後我們呼叫 `area` 與 `volume`，然後它將會告訴我們一個球體的面積與體積。若是我們要使用兩個或更多這些模組，我們就必須要進行限制引入，因為它們輸出具有相同名稱的 function。所以我們像這樣做：

<pre name="code" class="haskell:hs">
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube
</pre>

然後我們可以呼叫 `Sphere.area`、`Sphere.volume`、`Cuboid.area`、等等，而它們將會針對它們對應的物件計算面積或是體積。

下一次你發現你自己寫了一個非常大且擁有許多 function 的檔案，就試著看看有哪些提供某些共同用途的 function，然後看看你是否能將它們擺進它們自己的模組中。在你下一次寫一個需要某些相同功能的程式時，你就能夠引入你的模組。
