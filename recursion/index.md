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

## <a name="quick-sort">快速，排序！</a>

## <a name="thinking-recursively">遞迴地思考</a>

