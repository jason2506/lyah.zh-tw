---
layout: page
title: 高階函數
prev:
    url: recursion
    title: 遞迴
---

<img src="http://s3.amazonaws.com/lyah/sun.png" alt="sun" style="float:right" />
Haskell function 可以接收 function 作為參數，也可以將 function 作為傳回值回傳。當中的任一種 function 都被稱為高階函數。高階函數不僅是 Haskell 經驗的一部分，它幾乎就是整個 Haskell 經驗。
事實證明，如果你想要藉由定義<i>是</i>什麼，而不是藉由定義改變某狀態的步驟或是循環，來定義計算的方式，高階函數是不可缺少的。它是個真正有威力的解決問題與思考程式的方法。

## <a name="curried-functions">Curried functions</a>

每個在 Haskell 中的 function 實際上只接收一個參數。所以我們迄今是如何定義並使用那幾個接收多於一個參數的 function 呢？嗯，這是個聰明的手法！所有到目前為止接收<i>數個參數</i>的 function 都是個 *curried function*。這代表什麼呢？在一個例子中理解它是最好的。讓我們拿出我們的好朋友，`max` function。它看起來就像是它接收兩個參數，並傳回比較大的那個值。執行 `max 4 5` 首先會建立一個接收一個參數的 function，其傳回 `4` 或這個參數，取決於哪個值比較大。然後，`5` 被套用在這個 function，而這個 function 產生我們需要的結果。這聽起來很拗口，不過它實際上真的是個非常酷的概念。接下來兩個呼叫是相等的：

<pre name="code" class="haskell:ghci">
ghci> max 4 5
5
ghci> (max 4) 5
5
</pre>

<img src="img/curry.png" alt="haskell curry" style="float:left" />
將一個空白放在兩個東西之間是一個簡單地 *function application*。空白有點像是一個運算子，且擁有最高的優先次序。讓我們檢驗 `max` 的型別。其為 `max :: (Ord a) => a -> a -> a`。這也可以被寫成 `max :: (Ord a) => a -> (a -> a)`。這可以被讀作：`max` 接收一個 `a` 並傳回（就是那個 `->`）一個接收一個 `a` 並傳回一個 `a` 的 function。這就是為什麼 function 的回傳型別與參數全都簡單地以箭頭分隔。

所以這對我們又有什麼好處呢？簡單地說，假如我們以不足的參數呼叫一個 function，我們會得到一個*部分應用的（partially applied）* function。使用 partial application（以不足的參數呼叫 function，如果你願意的話）是一個快速建立 function 的俐落方式，所以我們可以將它們傳入另一個 function 或是塞入某些資料。

看看這個十分簡單的 function：

<pre name="code" class="haskell:hs">
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z
</pre>

當我們執行 `multThree 3 5 9` 或是 `((multThree 3) 5) 9` 實際上發生了什麼呢？首先，`3` 被套用到 `multThree` 上，因為它們以一個空白分隔。這建立了一個接收一個參數並傳回一個 function 的 function。所以接下來 `5` 被套用到這個 function，其會建立一個接收一個參數並乘以 15 的 function。`9` 被套用到這個 function 且結果為 135。記得這個 function 的型別也可以被寫作 `multThree :: (Num a) => a -> (a -> (a -> a))`。在 `->` 之前的東西為一個 function 接收的參數，在 `->` 之後的東西則是它所傳回的值。所以我們的 function 接收一個 `a` 並傳回一個型別為 `(Num a) => a -> (a -> a)` 的 function。同樣的，這個 function 接收一個 `a` 並傳回一個型別為 `(Num a) => a -> a` 的 function。而在最後，這個 function 接收一個 `a` 並傳回一個 `a`。看看這個：

<pre name="code" class="haskell:ghci">
ghci> let multTwoWithNine = multThree 9
ghci> multTwoWithNine 2 3
54
ghci> let multWithEighteen = multTwoWithNine 2
ghci> multWithEighteen 10
180
</pre>

藉由以不足的參數呼叫 function，我們可以說是快速建立了新的 function。若是我們想要建立一個接收一個數字並將其與 `100` 相比較的 function 呢？我們可以像這樣做：

<pre name="code" class="haskell:hs">
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x
</pre>

假如我們以 `99` 呼叫它，它會傳回一個 `GT`。簡單的東西。注意到在等式兩邊的 `x` 都擺在右手邊。現在讓我們想想 `compare 100` 會傳回什麼。它會傳回一個接收一個數字並與 `100` 相比較的 function。哇喔！這不就是我們要的 function 嗎？我們可以把它重寫成：

<pre name="code" class="haskell:hs">
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100
</pre>

型別宣告維持不變，因為 `compare 100` 回傳一個 function。`compare` 擁有型別 `(Ord a) => a -> (a -> Ordering)`，而以 `100` 呼叫它傳回一個 `(Num a, Ord a) => a -> Ordering`。躲在這裡的額外的類別限制是因為 `100` 也是 `Num` typeclass 的一員。

<p class="hint">
<em>呦！</em>確保你相當瞭解 curried function 與 partial application 如何運作，因為他們非常重要！
</p>

中綴 function 也可以藉由分段（section）被部分應用。為了要分段一個中綴 function，只要將它用括號包起來，並只提供一邊的參數。這會建立一個接收一個參數，然後將它套用在缺少運算元的那一邊的 function。一個十分簡單的 function 如下：

<pre name="code" class="haskell:hs">
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)
</pre>

呼叫 `divideByTen 200` 等同於執行 `200 / 10`，也就是執行 `(/10) 200`。一個檢查給定字元是否為大寫字母的 function 如下：

<pre name="code" class="haskell:hs">
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])
</pre>

使用 `-` 是分段唯一特別的地方。由分段的定義，`(-4)` 會產生一個接收一個數字並將其減掉 4 的 function。然而，為了方便起見，`(-4)` 代表負四。所以若是你想要建立一個將作為參數的數字減掉 4 的 function，就對 `subtract` function 做部分應用，像這樣：`(subtract 4)`。

若是我們嘗試在 GHCI 中要執行 `multThree 3 4`，而不是用 <i>let</i> 將它綁定到一個名稱上，或是傳遞到另一個 function 會發生什麼呢？

<pre name="code" class="haskell:ghci">
ghci> multThree 3 4
&lt;interactive&gt;:1:0:
    No instance for (Show (t -> t))
      arising from a use of `print' at &lt;interactive&gt;:1:0-12
    Possible fix: add an instance declaration for (Show (t -> t))
    In the expression: print it
    In a 'do' expression: print it
</pre>

GHCI 告訴我們 expression 產生一個型別為 `a -> a` 的 function，但它不知道如何將其印在螢幕上。function 並不是 `Show` typeclass 的實體，所以我們無法取得表示一個 function 的字串。當我們在 GHCI 中執行 `1 + 1` 時，它會先計算出 `2`，然後對 `2` 呼叫 `show` 來取得這個數字的文字表示。而 `2` 的文字表示就是字串 `"2"`，它接下來就會被印在我們的螢幕上。

## <a name="higher-orderism">是時候來點高階的了</a>

function 可以接收 function 作為參數，也能回傳 function。為了說明這一點，我們要建立一個接收一個 function，然後將它套用在某值兩次的 function！

<pre name="code" class="haskell:hs">
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
</pre>

<img src="http://s3.amazonaws.com/lyah/bonus.png" alt="rocktopus" style="float:right" />
首先，注意到型別宣告。先前我們並不需要括號，因為 `->` 本來就是右結合的（right-associative）。然而在這裡它是必要的。它表明第一個參數是一個 function，其接收某個值並傳回相同型別的值。第二個參數為此型別的某個值，而傳回值的型別亦相同。我們可以用 curried 的方式來看這個型別宣告，不過為了省下麻煩，我們只說這個 function 接收兩個參數並傳回一個值。第一個參數是一個（型別為 `a -> a` 的）function，而第二個參數型別則是相同的 `a`。這個 function 也可以為 `Int -> Int`、`String -> String` 或其它什麼的。不過這時，第二個參數也必須為相同型別。

<p class="hint">
<em>註記：</em>從現在開始，我們將會說 function 接收數個參數，儘管每個 function 實際上都只取一個參數並傳回部分應用的 function，直到我們到達一個傳回完整值的 function。所以為了簡單起見，我們會說 <code>a -> a -> a</code> 接收兩個參數，儘管我們知道它實際上到底做了什麼。
</p>

function 主體十分簡單。我們將參數 `f` 作為一個 function 使用，藉由以空白分隔將 `x` 套用進去，然後再一次將結果套進 `f` 中。總而言之，玩玩看這個 function：

<pre name="code" class="haskell:ghci">
ghci> applyTwice (+3) 10
16
ghci> applyTwice (++ " HAHA") "HEY"
"HEY HAHA HAHA"
ghci> applyTwice ("HAHA " ++) "HEY"
"HAHA HAHA HEY"
ghci> applyTwice (multThree 2 2) 9
144
ghci> applyTwice (3:) [1]
[3,3,1]
</pre>

partial application 的迷人與有用是顯而易見的。假如我們的 function 要求我們傳遞一個只接收一個參數的 function，我們可以部分應用一個 function 使它只接收一個參數，然後再傳遞它。

現在我們要使用高階程式設計來實作一個在標準函式庫中非常有用的 function。它叫做 `zipWith`。它接收一個 function 與兩個 list 作為參數，然後藉由將 function 套用在兩個 list 中的個別元素來結合這兩個 list。這裡我們要這樣實作它：

<pre name="code" class="haskell:hs">
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
</pre>

看看型別宣告。第一個參數為一個接收兩個值並產生第三個值的 function。它們不必為相同型別，但即使相同也行。第二與第三個參數為 list。回傳結果也是個 list。第一個 list 必須為一個 `a` 的 list，因為結合 function 以 `a` 作為它第一個引數。第二個 list 必須為一個 `b` 的 list，因為結合 function 的第二個參數型別為 `b`。回傳結果為 `c` 的 list。如果一個 function 的型別宣告表明它接受一個 `a -> b -> c` function 作為參數，它也會接受一個 `a -> a -> a` function，不過反過來就不行了！記得當你要建立 function──尤其是高階函數──且不確定型別的時候，你可以試著忽略型別宣告，然後藉由使用 `:t` 來檢查 Haskell 將它推導為什麼。

這個 function 的行為與一般的 `zip` 十分相似。邊界條件相同，只是有個額外的引數──結合 function，不過這個引數在邊界條件中無關緊要，所以我們僅用一個 `_` 表示它。而在最後一個模式裡的 function 主體也與 `zip` 相似，只是它不是用 `(x,y)`，而是 `f x y`。若是足夠一般化，一個單獨的高階函數可以被用在許多不同的任務中。這裡是我們的 `zipWith'` function 可以做的所有不同工作的一個小示範：

<pre name="code" class="haskell:ghci">
ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]
[6,8,7,9]
ghci> zipWith' max [6,3,2,1] [7,3,1,5]
[7,3,2,5]
ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
["foo fighters","bar hoppers","baz aldrin"]
ghci> zipWith' (*) (replicate 5 2) [1..]
[2,4,6,8,10]
ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
[[3,4,6],[9,20,30],[10,12,12]]
</pre>

如你所見，一個單獨的高階函數能夠以十分多樣的方式使用。命令式程式設計通常使用像是 for 迴圈、while 迴圈、設定變數值、檢測狀態之類的東西來達成某些行為，然後將它包裝成一個介面，像是一個 function。函數式程式設計使用高階函數來抽離常見的模式，像是成對檢查兩個 list 再對這些 pair 做某些事、取得一組解、或是剔除你不需要的值。

我們將實作另一個已經在標準函式庫中的 function，叫做 `flip`。`flip` 僅接收一個 function 並傳回一個像是我們原始的 function，只是前兩個參數被翻轉（flip）了。我們可以像這樣實作它：

<pre name="code" class="haskell:hs">
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x
</pre>

看到型別宣告，我們表明其接收一個接收一個 `a` 與一個 `b` 的 function，並回傳一個接收一個 `b` 與一個 `a` 的 function。不過因為 function 預設為 curried 的，所以實際上不需要第二組括號，因為 `->` 預設為右結合。`(a -> b -> c) -> (b -> a -> c)` 與 `(a -> b -> c) -> (b -> (a -> c))` 相同，其與 `(a -> b -> c) -> b -> a -> c` 相同。我們寫了 `g x y = f y x`。若是如此，則 `f y x = g x y` 一定也行，對吧？謹記於心，我們可以用一個更簡單的方式定義這個 function。

<pre name="code" class="haskell:hs">
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y
</pre>

這裡，我們藉著 curried function 的優勢。當我們缺少了 `y` 與 `x` 來呼叫 `flip' f`，它會回傳一個接收這兩個參數，但將其翻轉呼叫的 `f`。即使翻轉的 function 通常被傳遞到其它的 function 裡，我們也可以藉著 curried function 的優勢，藉由預先想好並寫下它被完整呼叫的最終結果來建立高階函數。

<pre name="code" class="haskell:ghci">
ghci> flip' zip [1,2,3,4,5] "hello"
[('h',1),('e',2),('l',3),('l',4),('o',5)]
ghci> zipWith (flip' div) [2,2..] [10,8,6,4,2]
[5,4,3,2,1]
</pre>

## <a name="maps-and-filters">Map 與 Filter</a>

<code class="label function">map</code> 接收一個 function 與一個 list，並將這個 function 套用在 list 中的所有元素以產生一個新的 list。讓我們看看它的型別簽名是什麼，以及它是如何被定義的。

<pre name="code" class="haskell:hs">
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
</pre>

型別簽名表示它接收一個接收一個 `a` 並回傳一個 `b` 的 function、一個 `a` 的 list，並傳回一個 `b` 的 list。有時候光看 function 的型別簽名，你就能說出它是什麼，挺有趣的。`map` 是能夠被用在數以百萬計不同方法的多用途高階函數之一。以下是它的執行結果：

<pre name="code" class="haskell:ghci">
ghci> map (+3) [1,5,3,1,6]
[4,8,6,4,9]
ghci> map (++ "!") ["BIFF", "BANG", "POW"]
["BIFF!","BANG!","POW!"]
ghci> map (replicate 3) [3..6]
[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
ghci> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
[[1,4],[9,16,25,36],[49,64]]
ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]
[1,3,6,2,2]
</pre>

你或許有注意到其中的每一項都能夠以一個 list comprehension 達成。`map (+3) [1,5,3,1,6]` 等同於寫下 `[x+3 | x <- [1,5,3,1,6]]`。然而，在你只應用某 function 在一個 list 的元素時，使用 `map` 更加易讀，尤其在你處理映射（map）的映射的時候，這時有一大堆括號會變得有些凌亂。

<code class="label function">filter</code> 為一個接收一個述部（一個述部是一個告訴你某值為真或假的 function，所以在我們的情況中，即是一個傳回布林值的 function）與一個 list，然後回傳滿足述部的元素 list。其型別簽名與實作像這樣：

<pre name="code" class="haskell:hs">
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
</pre>

非常簡單的東西。如果 `p x` 被求值為 `True`，此元素就會被包含在新的 list 中。若否，就將它排除在外。幾個有用的範例：

<pre name="code" class="haskell:ghci">
ghci> filter (>3) [1,5,3,2,1,6,4,3,2,1]
[5,6,4]
ghci> filter (==3) [1,2,3,4,5]
[3]
ghci> filter even [1..10]
[2,4,6,8,10]
ghci> let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
[[1,2,3],[3,4,5],[2,2]]
ghci> filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
"uagameasadifeent"
ghci> filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"
"GAYBALLS"
</pre>

這所有的一切也能夠以使用述部的 list comprehension 達成。沒有一個何時使用 `map` 與 `filter` 或是使用 list comprehension 的既定規則，你必須根據程式碼與上下文判斷哪個更加易讀。應用多個述部的 `filter` 等同於在一個 list comprehension 中多次過濾某值，或是以邏輯 `&&` function 結合多個述部。

記得我們在[前一章](/recursion)中的快速排序法 function 嗎？我們使用 list comprehension 來過濾小於（或等於）與大於基準點的 list 元素。我們可以藉由使用 `filter`，以一個更加易讀的方式達成相同的功能：

<pre name="code" class="haskell:hs">
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted
</pre>

<img src="img/map.png" alt="map" style="float:left" />
映射與過濾是每個函數程式設計師工具箱裡的謀生工具（bread and butter）。噢，無論你是使用 `map` 與 `filter` function 或是 list comprehension 來做都沒關係。回想我們如何解決找出特定周長的直角三角形的問題。在命令式語言，我們藉由嵌套三層迴圈，然後測試當前的組合是否為一個直角三角形，且有正確的周長來解決。若是如此，我們會將它印到螢幕上或是其它地方。在函數式程式設計中，這種模式以映射與過濾達成。你建立一個接收一個值並產出某個結果的 function。我們映射這個 function 到一組值之上，然後我們過濾生成的 list 以得到滿足我們搜尋的結果。感謝 Haskell 的惰性，即使你多次映射到一個 list 上，並多次過濾它，它也只會傳遞這個 list 一次。

讓我們*找出可以被 3829 除盡且小於 100,000 的最大數字*。為了做到這件事，我們要過濾一組我們知道答案所在的可能值。

<pre name="code" class="haskell:hs">
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0
</pre>

我們首先建立一個所有小於 100,000 的數字 list，以降序排列。然後我們藉由我們的述部來過濾它，且因為數字以降序排列，滿足我們述部的最大數字即是過濾過 list 的第一個元素。我們甚至不需要使用一個無限的 list 作為我們的起始集合。又一個懶惰的表現。因為我們最終只會使用過濾過 list 的 head，所以過濾過的 list 是有限或是無限並無關緊要。求值會在第一個適當的答案被找到時停止。

接下來，我們要*找出所有小於 10,000 的奇數平方的和*<span class="note">〔譯註：是「小於 10,000 且為奇數的平方數」的和，不是「小於 10,000 的奇數的平方數」的和〕</span>。不過首先，我們要引入 <code class="label function">takeWhile</code> function，因為我們會在我們的解法中用到它。它接收一個述部與一個 list，然後從 list 的開頭開始，並在述部成立時傳回它的元素。一旦找到一個令述部不成立的元素，它就停止了。若是我們要取出字串 `"elephants know how to party"` 中的第一個字，我們可以執行 `takeWhile (/=' ') "elephants know how to party"`，而它會傳回 `"elephants"`。好，所有小於 10,000 的奇數平方的和。首先，我們要從映射 `(^2)` function 到無限 list `[1..]` 開始。然後我們過濾它，以讓我們只得到奇數值。這時，我們要在這個 list 的元素小於 10,000 時從中取出元素。最後，我們要取得這個 list 的總和。我們甚至不必為此定義一個 function，我們可以在 GHCI 中以一行達成：

<pre name="code" class="haskell:ghci">
ghci> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
166650
</pre>

真棒！我們從某個初始資料開始（所有自然數的無限 list），然後我們映射到它之上、過濾它並切割它直到它滿足我們的需求，然後我們將它加總起來。我們也可以使用 list comprehension 來寫：

<pre name="code" class="haskell:ghci">
ghci> sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])
166650
</pre>

你覺得哪種比較漂亮完全是品味的問題。再一次，Haskell 的惰性令其變為可能。我們可以映射與過濾一個無限的 list，因為它不會真的立刻映射與過濾它，它會延遲這些行為。只有在我們強迫 Haskell 告訴我們總和，`sum` function 才會告訴 `takeWhile` 它需要這些數字。`takeWhile` 強制讓過濾與映射發生，但只到遇到一個大於或是等於 10,000 的數字為止。

對於我們的下個問題，我們要處理 Collatz 序列。我們取一個正整數。如果這個數字是偶數，我們將它除以二。如果它是奇數，我們將它乘以 3 再加上 1。我們取結果的數字，再對它做一樣的事，這會產生一個新的數字，以此類推。本質上，我們會取得一個數字鏈（chain）。對於所有起始數字，鏈都會以數字 1 結束。所以若是我們取了起始數字 13，我們會得到這個序列：<i>13, 40, 20, 10, 5, 16, 8, 4, 2, 1</i>。13*3 + 1 等於 40。40 除以 2 為 20，等等。我們看到這條鏈有十個項。

現在我們想要知道的是這個：*對於所有介於 1 到 100 的起始數字，有多少條鏈的長度大於 15？*首先，我們要寫一個產生一條鏈的 function：

<pre name="code" class="haskell:hs">
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)
</pre>

因為鏈以 1 結尾，所以它就是邊界案例。這是個十分基本的遞迴 function。

<pre name="code" class="haskell:ghci">
ghci> chain 10
[10,5,16,8,4,2,1]
ghci> chain 1
[1]
ghci> chain 30
[30,15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]
</pre>

呀！它看起來運作正確。現在，這個 function 會告訴我們問題的答案：

<pre name="code" class="haskell:hs">
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15
</pre>

我們將 `chain` function 映射到 `[1..100]` 以得到鏈的 list，鏈本身就以 list 表示。然後，我們藉由一個僅檢查 list 長度是否大於 15 的述部來過濾它。一旦我們完成過濾，我們就能看到有多少鏈被留在結果的 list 中。

<p class="hint">
<em>註記：</em>這個 function 的型別為 <code>numLongChains :: Int</code>，因為歷史因素，<code>length</code> 會回傳一個 <code>Int</code> 而不是一個 <code>Num a</code>。如果我們想要回傳一個更一般化的 <code>Num a</code>，我們可以將 <code>fromIntegral</code> 使用在產生出來的長度上。
</p>

藉由 `map`，我們也可以做像是 `map (*) [0..]` 這種事，如果不是為了任何說明 currying 是如何運作，以及（部分應用的）function 是如何實際求值──為此你可以將它傳遞到其它 function 或是放進 list 裡（你無法將它們轉成字串）──的理由。到目前為止，我們只將接收一個參數的 function 映射到 list 上，像是 `map (*2) [0..]` 來得到一個型別為 `(Num a) => [a]` 的 list，不過我們也可以毫無問題的執行 `map (*) [0..]`。這裡所發生的是，在 list 中的數字被套用到 function `*`──其型別為 `(Num a) => a -> a -> a`。只套用一個參數到一個接收兩個參數的 function 會回傳一個接收一個參數的 function。若是我們將 `*` 映射到 list `[0..]` 之上，我們會得到一個僅接收一個參數的 function list，所以是 `(Num a) => [a -> a]`。`map (*) [0..]` 會產生一個像是我們藉由寫下 `[(0*),(1*),(2*),(3*),(4*),(5*)..` 而得到的 list。

<pre name="code" class="haskell:ghci">
ghci> let listOfFuns = map (*) [0..]
ghci> (listOfFuns !! 4) 5
20
</pre>

從我們的 list 取得索引 `4` 的元素會回傳一個等同於 `(4*)` 的 function。這時，我們套用 `5` 到這個 function。所以這就像是寫下 `(4*) 5` 或是 `4 * 5`。

## <a name="lambdas">Lambdas</a>

<img src="img/lambda.png" alt="lambda" style="float:right" />
lambda 基本上是個匿名（anonymous）函數，使用它是因為某些 function 我們只需要使用一次。通常，我們建立一個 lambda 都伴隨著要將它傳遞到一個高階函數中的唯一目的。要建立一個 lambda，我們寫作一個 `\`（因為如果你瞇著眼睛看，它看起來就像個希臘字母的 lambda），然後寫下以空白分隔的參數。接著是一個 `->` 接著 function 主體。我們通常將它以括號包起來，否則它會一路向右擴展下去。

如果你向上看大概五英呎左右，你會看到我們在我們的 `numLongChains` function 裡頭使用了一個 <i>where</i> 綁定來建立 `isLong` function，其唯一目的就是要傳遞到 `filter` 之中。嗯，為了代替這種作法，我們可以使用一個 lambda：

<pre name="code" class="haskell:hs">
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))
</pre>

lambda 是 expression，這就是為什麼我們可以像這樣傳遞它。expression `(\xs -> length xs > 15)` 回傳一個告訴我們傳遞進去的 list 長度是否大於 15 的 function。

<img src="img/lamb.png" alt="lamb" style="float:left" />
不熟悉 currying 與 partial application 如何運作的人，時常會在不必要的時候使用 lambda。舉例來說，由於 `(+3)` 與 `(\x -> x + 3)` 都是接收一個數字並加上 3 的 function，所以 expression `map (+3) [1,6,3,2]` 與 `map (\x -> x + 3) [1,6,3,2]` 是等價的。不消說，由於使用 partial application 更加易讀，在這種情況中建立一個 lambda 是很愚蠢的。

如同一般的 function，lambda 可以接收任何數量的參數：

<pre name="code" class="haskell:ghci">
ghci> zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
[153.0,61.5,31.0,15.75,6.6]
</pre>

且如同一般的 function，你可以在 lambda 中進行模式匹配。唯一的不同是，你無法為一個參數定義多個模式，像是為相同的參數建立一個 `[]` 與一個 `(x:xs)` 模式。若是在 lambda 中模式匹配失敗，就會發生一個執行期錯誤，所以在 lambda 裡使用模式匹配時要小心點。

<pre name="code" class="haskell:ghci">
ghci> map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
[3,8,9,8,7]
</pre>

lambda 通常以括號包起來，除非我們要讓它一路向右擴展下去。這裡有些有趣的事情：由於 function 預設是 curried，所以這兩者是等價的：

<pre name="code" class="haskell:hs">
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z
</pre>

<pre name="code" class="haskell:hs">
addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z
</pre>

若是我們像這樣定義一個 function，型別宣告為何如此就很顯而易見了。型別宣告與方程式（equation）兩者都有三個 `->`。但當然，第一種撰寫 function 的方式更加易讀，第二種方式則幾乎是個說明 currying 的噱頭。

然而，有些時候使用這個符號是很酷的。我想像這樣定義 `flip` function 的時候是最好讀的：

<pre name="code" class="haskell:ghci">
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x
</pre>

即使這與撰寫 `flip' f x y = f y x` 相同，但大多時候，我們能明顯看到它會被用來產生一個新的 function。`flip` 最常見的使用情況是僅以 function 參數呼叫它，然後將生成的 function 傳遞到一個 `map` 或是一個 `filter`。所以當你想要明確表示你的 function 主要會被部分應用並作為參數傳遞到一個 function 裡，就以這種方式使用 lambda
。

## <a name="folds">Only folds and horses</a>

<img src="img/origami.png" alt="folded bird" style="float:right" />
回到我們處理遞迴的時候，我們注意到到處都有許多操作在 list 之上的遞迴 function。通常，我們有一個針對空 list 的邊界案例。我們引入 `x:xs` 模式然後做某些涉及單獨的元素與 list 剩下部份的動作。事實證明這是個非常常見的模式，因此有一對非常有用的 function 被引入來封裝它。這些 function 被稱為折疊（fold）。它有點像是 `map` function，只是它將 list 化簡為某個單一值。

一個折疊（fold）接收一個二元 function、一個起始值（我喜歡把它叫做累加器〈accumulator〉）與一個要被折疊的 list。二元 function 本身接收兩個參數。二元 function 會以累加器與第一個（或是最後一個）元素呼叫，並產生一個新的累加器。然後，二元 function 會再一次以新的累加器與當前新的第一個（或最後一個）元素被呼叫，以此類推。一旦我們走遍整個 list，只有累加器留下來，而它就是我們化簡 list 得到的結果。

首先讓我們看看 <code class="label function">foldl</code> function，也被稱為左折疊。它從左邊開始折疊 list。二元 function 被套用在起始值與 list head。這會產生一個新的累加值，而二元 function 會以這個值與下一個元素再次被呼叫，以此類推。

讓我們再一次實作 `sum`，但這時我們要使用一個折疊，而不是顯式的遞迴。

<pre name="code" class="haskell:hs">
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
</pre>

測試看看，一二三：

<pre name="code" class="haskell:ghci">
ghci> sum' [3,5,2,1]
11
</pre>

<img src="img/foldl.png" alt="foldl" style="float:left" />
讓我們深入看看這個折疊是怎麼發生的。`\acc x -> acc + x` 為一個二元 function。`0` 為起始值，而 `xs` 是要被折疊的 list。首先，`0` 被用來當做二元 function 的 `acc` 參數，而 `3` 則被用來當做 `x`（或是當前的元素）參數。`0 + 3` 產生一個 `3`，而它會變成新的累加值。接下來，`3` 被用來當做累加值，而 `5` 作為當前元素，且 `8` 成為了新的累加值。繼續向前，`8` 為累加值、`2` 為當前元素、新的累加值為 `10`。最後，`10` 被用作累加值，而 `1` 作為當前元素，產生 `11`。恭喜，你已經完成折疊了！

左邊的這個專業圖表，一步接著一步（一天接著一天！）說明了折疊是怎麼發生的。綠褐色的數字為累加值。你可以看到 list 如何藉由累加器，從左邊漸漸被消耗。唔！如果我們考慮到 function 會被 curried，我們可以將它實作的更加簡潔，像這樣：

<pre name="code" class="haskell:hs">
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0
</pre>

`(\acc x -> acc + x)` 這個 lambda function 與 `(+)` 相同。我們可以忽略作為參數的 `xs`，因為呼叫 `foldl (+) 0` 會回傳一個接收一個 list 的 function。通常，由於有 currying，所以如果你有一個像是 `foo a = bar b a` 的 function，你可以將它改寫成 `foo = bar b`。

總之，在進入到右折疊之前，讓我們以左折疊實作另一個 function。我相信大家都知道 `elem` 會檢查一個值是否為一個 list 的一部分，所以我不會再提一次（唉呦，剛剛就提了！）。讓我們以左折疊實作它。

<pre name="code" class="haskell:hs">
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys
</pre>

嗯，嗯，嗯，這裡我們有些什麼呢？這裡起始值與累加器為一個布林值。在處理折疊的時候，累加值與最終結果的型別總是相同的。如果你始終不知道該拿什麼來當做起始值，這會給你一些想法。我們從 `False` 開始。使用 `False` 作為起始值是有意義的。我們假設給定的值不存在 list 中。同樣的，如果我們對一個空 list 進行折疊，則結果將會是起始值。然後我們檢查當前的元素是否是我們要尋找的元素。如果是，我們就將累加器設為 `True`。若否，我們就讓累加器維持不變。若是累加器先前為 `False`，它會維持如此，因為當前的元素也不是我們要找的。若它先前為 `True`，我們也因此保留它。

右折疊──<code class="label function">foldr</code> 以類似於左折疊的方式運作，只是累加器從右邊開始取值。同樣的，左折疊的二元 function 有個累計器作為第一個參數，與當前值作為第二個參數（所以是 `\acc x -> ...`），右折疊的二元 function 有個當前值作為第一個參數，與累計器作為第二個參數（所以是 `\x acc -> ...`）。右折疊有個在右邊的累加器這點也有點道理，因為是從右邊折疊的嘛。

折疊的累加值（以及結果）可以是任何型別。它可以是個數字、一個布林值、甚或是一個 list。我們要以右折疊實作 `map` function。累加器將會是個 list，我們將會一個元素接著一個元素累積被映射的 list。根據如此，起始值是個空 list 這點是很顯而易見的。

<pre name="code" class="haskell:hs">
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
</pre>

若是我們要將 `(+3)` 映射到 `[1,2,3]`，要從右邊開始處理 list。我們取得最後一個元素──其為 `3`，並將 function 套用上去，其結果為 `6`。然後，我們將它前置（`:`）在累加器──其為 `[]`──之前。`6:[]` 即是 `[6]`，而這就是現在的累加器。我們將 `(+3)` 套用在 `2`，其為 `5`，且我們將它前置在累加器，所以累計器現在是 `[5,6]`。我們將 `(+3)` 套用到 `1`，並將它前置在累加器，所以最終值為 `[4,5,6]`。

當然，我們也能夠以左折疊來實作這個 function。這將會是 `map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs`，不過 `++` function 比起 `:` 代價更為高昂，所以我們在從一個 list 建立新 list 的時候，通常會使用右折疊。

<img src="img/washmachine.png" alt="fold this up!" style="float:right" />
如果你要反轉一個 list，你能夠以像是左折疊的方式對它進行右折疊，反之亦然。有時你甚至不必這麼做。`sum` function 能夠以十分相似的方式以左折疊與右折疊實作。一個主要的不同是，右折疊能夠運作於無限的 list，而左折疊則不行！如果你在某個點取一個無限的 list，並從右邊開始折疊它，你最終將會到達 list 的開頭。然而，如果你在某個點取一個無限的 list，然後嘗試從左邊開始折疊它，你將永不會到達結尾！

*折疊可以被用來實作任何讓你一個元素接個一個元素巡訪 list 一次，然後基於如此傳回某值的 function。無論何時你想巡訪一個 list 以傳回某值，你可能就需要一個折疊。*這就是為什麼折疊是函數式語言中為最有用的 function 型別之一，與映射與過濾齊名。

<code class="label function">foldl1</code> 與 <code class="label function">foldr1</code> function 運作得非常像是 `foldl` 與 `foldr`，只是你不需要提供它們一個明確的起始值。它假設 list 的第一個（或最後一個）元素為起始值，然後從它的下一個元素開始折疊。考慮到這一點，`sum` function 可以像這樣被實作：`sum = foldl1 (+)`。因為它是以要折疊的 list 有至少一個元素為前提，所以若是以空 list 呼叫它，就會產生執行期錯誤。在另一方面，`foldl` 與 `foldr` 與空 list 運作良好。在建立一個折疊的時候，想想它要怎麼操作空 list。如果 function 在給定空 list 時沒什麼意義，你可能可以用 `foldl1` 或是 `foldr1` 來實作它。

為了顯示折疊的威力，我們要使用折疊來實作一堆標準函式庫 function：

<pre name="code" class="haskell:hs">
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)
</pre>

`head` 以模式匹配實作得更好，不過這只是要顯示，你仍然可以使用折疊來達成。我想我們的 `reverse'` 定義十分高明。我們取一個空 list 作為起始值，然後從左邊開始處理 list，並將它前置在我們的累加器。最後，我們就建出了一個反轉的 list。`\acc x -> x : acc` 看起來有點像是 `:` function，只是參數是顛倒過來的。這就是為什麼我們也可以將我們的 reverse 寫成 `foldl (flip (:)) []`。

另一種描述左與右折疊的方式像是：假定我們有個右折疊，二元 function 為 `f`，而起始值為 `z`。若是我們對 list `[3,4,5,6]` 進行右折疊，我們實際上是這樣做：`f 3 (f 4 (f 5 (f 6 z)))`。`f` 以 list 中的最後一個元素與累加器呼叫，這個值就被視為下一個累加器，以此類推。若是我們以 `+` 作為 `f` 且起始累加值為 `0`，就是 `3 + (4 + (5 + (6 + 0)))`。或是我們將 `+` 寫成前綴 function，就是 `(+) 3 ((+) 4 ((+) 5 ((+) 6 0)))`。類似的，對這個 list 以 `g` 作為二元 function，以 `z`作為累加器進行左折疊，等同於 `g (g (g (g z 3) 4) 5) 6`。若是我們使用 `flip (:)` 作為二元 function，與 `[]` 作為累加器（所以我們會反轉這個 list），這時它就等同於 `flip (:) (flip (:) (flip (:) (flip (:) [] 3) 4) 5) 6`。果然，如果你對此 expression 求值，你就得到了 `[6,5,4,3]`。

<code class="label function">scanl</code> 與 <code class="label function">scanr</code> 就像是 `foldl` 與 `foldr`，只是它會以 list 的形式記錄所有中介的累加器狀態。這裡也有個 `scanl1` 與 `scanr1`，其和 `foldl1` 與 `foldr1` 相似。

<pre name="code" class="haskell:ghci">
ghci> scanl (+) 0 [3,5,2,1]
[0,3,8,10,11]
ghci> scanr (+) 0 [3,5,2,1]
[11,8,3,1,0]
ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
[3,4,5,5,7,9,9,9]
ghci> scanl (flip (:)) [] [3,2,1]
[[],[3],[2,3],[1,2,3]]
</pre>

在使用 `scanl` 時，最終結果將會是其生成 list 的最後一個元素，而 `scanr` 則會把結果擺在 head。

掃描（scan）用以監視能夠以 fold 實作的 function 執行過程。讓我們回答這個問題：*取多少個自然數平方根的總和會大於 1000？*要得到所有自然數的平方根，我們僅需要 `map sqrt [1..]`。現在，要取得總和，我們可以做一次折疊，但因為我們對總和的過程感興趣，所以我們要進行一次掃描。一旦我們完成掃描，我們就看到有多少個總和小於 1000 了。通常，在掃描清單的第一個總和為 1。第二個總和將會是 1 加上 2 的平方根。第三個總和將會是它加上 3 的平方根。若是有 X 個小於 1000 的總和，這時它會取第 X+1 個總和超過 1000 的元素。

<pre name="code" class="haskell:hs">
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
</pre>

<pre name="code" class="haskell:ghci">
ghci> sqrtSums
131
ghci> sum (map sqrt [1..131])
1005.0942035344083
ghci> sum (map sqrt [1..130])
993.6486803921487
</pre>

我們在這裡使用 `takeWhile` 而不是 `filter`，因為 `filter` 無法運作在無限的 list。即使我們知道 list 為升序的，但 `filter` 卻不知道，所以我們使用 `takeWhile` 在第一次發生總和大於 1000 的地方切割掃描清單。

## <a name="function-application">帶著 $ 的 function application</a>

## <a name="composition">Function composition</a>
