---
layout: page
title: 遞迴
prev:
    url: syntax-in-functions
    title: Function 中的語法
---

## <a name="hello-recursion">遞迴，你好！</a>

<img src="img/recursion.png" alt="SOVIET RUSSIA" style="float:left" />
在先前的章節中我們簡略地提到過遞迴。在這一章，我們將會更仔細地看看遞迴、為什麼它對 Haskell 來說是很重要的、以及我們如何藉由遞迴思考，來針對問題想出非常簡潔且優雅的解答。

如果你還是不知道遞迴是什麼，就讀讀這一句。哈哈！開個玩笑而已！遞迴實際上是一種定義函數的方式，其中這個函數被應用在它自己的定義裡頭。數學中的定義往往以遞迴的形式給定。舉例來說，費波那契數列（fibonacci sequence）就被遞迴地定義。首先，我們非遞迴地定義前兩個費波那契數。我們說 <i>F(0) = 0</i> 且 <i>F(1) = 1</i>，代表第 0 個與第 1 個費波那契數分別是 0 和 1。然後我們說，對於任何其他的自然數，此費波那契數為前兩個費波那契數的總和。所以 <i>F(n) = F(n-1) + F(n-2)</i>。如此一來，<i>F(3)</i> 為 <i>F(2) + F(1)</i>，即是 <i>(F(1) + F(0)) + F(1)</i>。因為我們現在拆解到只剩非遞迴定義的費波那契數，所以我們可以有把握地說：<i>F(3)</i> 為 2。在遞迴函數中有一兩個非遞迴定義的元素（像是這裡的 <i>F(0)</i> 與 <i>F(1)</i>）也被稱為*邊界條件*，如果你想要你的遞迴 function 終止，這是很重要的。假使我們沒有非遞迴地定義 <i>F(0)</i> 與 <i>F(1)</i>，對於任何數字你都不會得到一個解答，因為你會到達 0，然後進入負數。突然間，你已經算到 <i>F(-2000)</i> 為 <i>F(-2001) + F(-2002)</i>，卻仍然看不到盡頭。

遞迴對 Haskell 是重要的，因為不若命令式語言，你在 Haskell 中是藉由宣告「某個東西<i>是</i>什麼」而不是宣告「你<i>如何</i>得到它」來進行計算。這就是為什麼在 Haskell 中沒有 while 迴圈（loop）或是 for 迴圈，而我們很多時候都必須要使用遞迴來宣告「某個東西是什麼」。

## <a name="maximum-awesome">Maximum 真棒</a>

`maximum` function 接收某個可以被排序的 list（例如 `Ord` typeclass 的實體〔instance〕），並傳回其中最大的值。想想你要如何以命令式的方式實作它。你可能會設定一個變數以保留到目前為止的最大值，然後你會走遍 list 中的元素，若是某個元素大於當前的最大值，你會以這個元素來取代它。保留到最後的最大值即是結果。呼！用了不少文字來描述這樣一個簡單的演算法！

現在讓我們看看我們要如何遞迴地定義它。我們可以先設定一個邊界條件，表示一個單一元素的 list 的最大值等於其中的唯一元素。然後我們可以說，
若是一個比較長的 list 的 head 大於 tail 的最大值，則最大值為 head。若是 tail 的最大值比較大，嗯，則此 list 的最大值為 tail 的最大值。就這樣了！現在讓我們在 Haskell 中實作它。

<pre name="code" class="haskell:hs">
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs
</pre>

你可以看到，模式匹配與遞迴十分相配！許多命令式語言沒有模式匹配，所以你必須建立許多 if else 敘述來測試邊界條件。這裡我們僅需要把它們寫成模式。所以第一個邊界條件表示，如果 list 為空，崩潰！有道理，一個空 list 的最大值是什麼呢？我不知道。第二個模式也設定了一個邊界條件。它表示如果它是一個單一元素的 list，就傳回唯一的元素。

現在，第三個模式為動作實際發生的地方。我們使用模式匹配來將一個 list 切割為 head 與 tail。這在對 list 執行遞迴時，是個十分常見的慣用寫法，所以習慣它吧。我們使用一個 <i>where</i> 綁定來將 `maxTail` 定義成 list 其餘部分的最大值。然後我們檢查 head 是否大於 list 其餘部分的最大值。若是，我們就回傳 head。否則，我們就傳回 list 其餘部分的最大值。

讓我們取一串數字的範例 list：`[2,5,1]`，並檢查看看它會如何運作。若是我們對它呼叫 `maximum'`，前兩個模式都不會被匹配。而第三個模式會，且 list 被切割成 `2` 與 `[5,1]`。<i>where</i> 子句想知道 `[5,1]` 的最大值，所以我們繼續下去。其再次匹配了第三個模式，且 `[5,1]` 被切割成 `5` 與 `[1]`。再一次的，<i>where</i> 子句想知道 `[1]` 的最大值。因為這是個邊界條件，所以它會回傳 `1`。終於！所以我們回到前一步，比較 `5` 與 `[1]` 的最大值（為 `1`），顯然我們會得到 `5`。所以現在我們知道 `[5,1]` 的最大值為 `5`。我們再回到前一步，那裡我們有 `2` 與 `[5,1]`。比較 `2` 與 `[5,1]` 的最大值，為 `5`，於是我們選擇了 `5`。

撰寫這個 function 的一個更清楚的方式是使用 `max`。如果你還記得，`max` 為一個接收兩個數字，並傳回之中比較大的值的 function。以下是我們如何使用 `max` 改寫 `maximum'`：

<pre name="code" class="haskell:hs">
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
</pre>

如此的優雅！本質上，一個 list 的最大值即為第一個元素與 tail 最大值的最大值。

<img src="img/maxs.png" alt="max" style="margin:10px auto 25px;display:block" />

## <a name="a-few-more-recursive-functions">再來一點遞迴 function</a>

現在我們大體而言知道如何遞迴地思考，讓我們實作一些使用遞迴的 function。首先，我們要實作 `replicate`。`replicate` 接收一個 `Int` 與某個元素，並傳回一個擁有許多相同元素重複的 list。舉例來說，`replicate 3 5` 傳回 `[5,5,5]`。讓我們想想邊界條件。我的猜想是邊界條件小於或等於 0。假如我們試著重複某個值零次，它會傳回一個空 list。對負數來說亦同，因為它實際上沒什麼意義。

<pre name="code" class="haskell:hs">
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x
</pre>

這裡我們使用 guard 而不是模式，因為我們正在測試一個布林條件。假使 `n` 小於或等於 0，傳回一個空字串。否則，傳回一個以 `x` 為第一個元素，然後以 `x` 重複 n-1 次作為 tail 的 list。最後，`(n-1)` 這部份將會令我們的 function 到達邊界條件。

<p class="hint">
<em>註記：</em><code>Num</code> 並非 <code>Ord</code> 的子類別（subclass）。這意味著由數字所構成的某值並非真的必須遵守一個順序。所以這就是為什麼我們在做加減法與大小比較時，必須同時指定 <code>Num</code> 與 <code>Ord</code> 類別約束。
</p>

接著，我們要實作 `take`。它從一個 list 中取出一定數量的元素。舉例來說，`take 3 [5,4,3,2,1]` 將會傳回 `[5,4,3]`。若是我們試著從一個 list 取出 0 個或更少元素，我們會得到一個空 list。若是我們試著從一個空 list 取出任何東西，我們同樣會得到一個空 list。注意到這裡有兩個邊界條件。所以讓我們把它寫下來：

<pre name="code" class="haskell:hs">
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs
</pre>

<img src="img/painter.png" alt="painter" style="float:right" />
第一個模式指定：若是嘗試取出 0 個或負數個元素，我們會得到一個空 list。注意到我們使用 `_` 來匹配 list，因為我們在這種情況下並不真的關心它是什麼。同樣注意到我們使用了一個 guard，但是沒有 `otherwise` 部分。這代表如果 `n` 實際上大於 0，則會落到下一個模式。第二個模式表示：如果我們試著從一個空 list 取出任何東西，我們會得到一個空 list。第三個模式將 list 分割成 head 與 tail。然後我們陳述：從一個 list 取出 `n` 個元素，等於一個以 `x` 作為 head，然後從 `xs` 中取出 `n-1` 個元素而得的 list 作為 tail 的 list。假如我們試著要從 `[4,3,2,1]` 取出 3 個元素，試著使用一張紙來寫下求值的過程看起來會是怎麼樣的。

`reverse` 簡單地反轉一個 list。想想邊界條件。是什麼呢？來吧....它是個空 list！一個反轉的空 list 等於空 list 本身。好。那麼其餘的部份呢？嗯，你可以說，若是我們將一個 list 切割成 head 與 tail，反轉的 list 就等於反轉的 tail 接著結尾的 head。

<pre name="code" class="haskell:hs">
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
</pre>

繼續前進！

因為 Haskell 支援無限 list，我們的遞迴並非真的必須有個邊界條件。不過若是沒有它，它就會無限地持續處理某個值，或是產生一個無限的資料結構，像是一個無限的 list。無限 list 的好處是，我們可以在我們想要的地方切斷它。`repeat` 接收一個元素，並傳回一個僅有這個元素的無限 list。它的遞迴實作非常簡單，看：

<pre name="code" class="haskell:hs">
repeat' :: a -> [a]
repeat' x = x:repeat' x
</pre>

呼叫 `repeat 3` 將會給我們一個從 `3` 開始，然後擁有無限個 `3` 作為 tail 的 list。所以呼叫 `repeat 3` 將會被求值為 `3:repeat 3`，也就是 `3:(3:repeat 3)`，也就是 `3:(3:(3:repeat 3))`，以此類推。
`repeat 3` 將永遠不會完成求值，而 `take 5 (repeat 3)` 將會給我們一個有五個 3 的 list。所以本質上它就像執行 `replicate 5 3`。

`zip` 接收兩個 list，並將它們扣在一起。`zip [1,2,3] [2,3]` 傳回 `[(1,2),(2,3)]`，因為它會截斷比較長的 list 以配合比較短的 list 長度。如果我們要把某個 list 與一個空的 list 扣在一起會怎麼樣呢？嗯，我們會得到一個空的 list。所以這就是我們的邊界條件。然而，`zip` 接收兩個 list 作為參數，所以實際上有兩個邊界條件。

<pre name="code" class="haskell:hs">
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys
</pre>

前兩個模式表示，如果第一個 list 或是第二個 list 為空，我們會得到一個空的 list。第三個模式表示，兩個扣在一起的 list 等於將它們的 head 配成一對，然後接上扣在一起的 tail。將 `[1,2,3]` 與 `['a','b']` 扣在一起最終會試著將 `[3]` 與 `[]` 扣在一起。觸及了邊界條件，所以結果是 `(1,'a'):(2,'b'):[]`，也就等同於 `[(1,'a'),(2,'b')]`。

讓我們再實作一個標準函式庫的 function──`elem`。它接收一個元素與一個 list，並檢查這個元素是否在 list 之中。邊界條件──與 list 的大多數情況相同──為空 list。我們知道一個空 list 不包含元素，所以它當然沒有我們要找的東西。

<pre name="code" class="haskell:hs">
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs
</pre>

如同預期的十分簡單。若是 head 不是這個元素，我們就檢查 tail。假如我們達到一個空 list，則結果為 `False`。

## <a name="quick-sort">快速，排序！</a>

我們有一串可以被排序的 list。它們的型別為 `Ord` typeclass 的實體。而且現在，我們打算要排序它們！有個非常酷的排序演算法，被稱為快速排序法（quicksort）。它以非常高明的方式進行排序。在命令式語言中要用超過十行來實作快速排序法，而在 Haskell 中的實作則更加簡短且優雅。快速排序已經變成 Haskell 的一種典型代表。因此，讓我們在這裡實作它，即使在 Haskell 中實作快速排序法是被視為十分俗氣的，因為每個人藉此以展示 Haskell 有多優雅。

<img src="img/quickman.png" alt="quickman" style="float:left" />
所以，型別簽名將是 `quicksort :: (Ord a) => [a] -> [a]`。沒什麼特別的。邊界條件呢？空 list，正如預期。一個排序過的空 list 是一個空 list。現在這裡看到主要的演算法：*一個排序過的 list 是一個所有小於（或等於）list head 的值在前（且這些值是被排序過的），然後 list 的 head 在中間，再接著所有大於 head 的值（它們也是排序過的）的 list*。注意到我們在此定義中說了兩次<i>排序的</i>，所以我們可能必須進行兩次遞迴呼叫！同樣注意到我們使用動詞<i>是（is）</i>來定義演算法，而不是說<i>做這個、做那個、然後做那個....</i>這就是函數式程式設計之美！我們要如何過濾 list，讓我們只得到小於我們 list head 的元素，與大於我們 list head 的元素呢？list comprehension。所以，讓我們繼續定義這個 function。

<pre name="code" class="haskell:hs">
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted
</pre>

讓我們給它一個小測試執行，看它是否正確地表現。

<pre name="code" class="haskell:ghci">
ghci> quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]
[1,2,2,3,3,4,4,5,6,7,8,9,10]
ghci> quicksort "the quick brown fox jumps over the lazy dog"
"        abcdeeefghhijklmnoooopqrrsttuuvwxyz"
</pre>

這就是我所說的！所以假使我們有 `[5,1,9,4,6,7,3]`，且想要排序它，這個演算法首先會取得 head──`5`，然後把它擺在兩個分別為小於和大於它的 list 中間。所以在此時，你會得到 `[1,4,3] ++ [5] ++ [9,6,7]`。我們知道一旦 list 被完全地排序，數字 `5` 將會留在第四個位置，因為這裡有 3 個數字小於它，且有 3 個數字大於它。現在，若是我們排序 `[1,4,3]` 與 `[9,6,7]`，我們就有了一個排序過的 list！我們使用相同的 function 排序這兩個 list。最終，我們會持續拆解它以達到空 list，而一個空的 list 在某種方面上已經是排序過的了。這裡是個示意圖：

<img src="img/quicksort.png" alt="quicksort" style="margin:10px auto 25px;display:block" />

一個就定位且不會再移動的元素被表示為<span style="color:#FF6600;font-weight:bold;">橘色</span>。假使你從左到右閱讀它，你會看到排序過的 list。雖然這裡我們選擇 head 跟所有元素比較大小，不過我們也可以使用任何元素做比較。在快速排序法中，一個你用來比較大小的元素被稱為一個基準點（pivot）。在這裡它是<span style="color:#009900;font-weight:bold">綠色</span>的。我們選擇 head 是因為藉由模式匹配它很容易取得。小於基準點的元素為<span style="color:#0f0;font-weight:bold">亮綠色</span>，而大於基準點的元素為<span style="color:#030;font-weight:bold">深綠色</span>。黃色漸層的東西表示快速排序的一個 application。

## <a name="thinking-recursively">遞迴地思考</a>

我們到目前為止已經做了不少遞迴了，正如你可能已經注意到的，這裡有個模式：通常你會定義一個邊界案例，然後定義一個 function，其對某些元素做某些事情，再將此 function 套用到其餘的部分。無論它是一個 list、tree 或是任何其他的資料結構。list 的總和為 list 的第一個元素加上 list 其餘部分的總和。list 的乘積是 list 第一個元素與 list 其餘部分乘積的乘積。list 的長度為一加上 list tail 的長度。諸如此類、諸如此類....

<img src="img/brain.png" alt="brain" style="float:left" />
當然，也要有邊界案例。通常邊界案例為某些遞迴 application 不具意義的情況。處理 list 時，最常見的邊界案例為空 list。若是你在處理 tree，邊界情況通常為不具有任何子代（children）的節點（node）。

你在遞迴地處理數字時也很類似。通常它必須對某個數字做些操作，然後再將 function 應用在這個修改過的數字。我們先前看過階乘 function，而它為一個數字與這個數字減一階乘的乘積。這樣的遞迴 application 對於 0 不具意義，因為僅有正整數的階乘有被定義。邊界案例的值往往為一個單位元素（identity）。乘法的單位元素為 1，因為若是你將某值乘以 1，你會得到同樣的值。同樣在計算 list 總和時，我們定義一個空 list 的總和為 0，而 0 是加法的單位元素。在快速排序法中，邊界案例為空 list，且單位元素也是空 list，因為若是你將一個空 list 加到一個 list 上面，你只會得到原本的 list。

所以在試著想一個遞迴方法來解決一個問題時，嘗試思考遞迴解無法應用的情況，然後看看你是否能以此作為一個邊界案例，想想基本元素再想想你是否要拆開 function 的參數（舉例來說，list 通常透過模式匹配拆成 head 與 tail），以及你要在哪個部分使用遞迴呼叫。
