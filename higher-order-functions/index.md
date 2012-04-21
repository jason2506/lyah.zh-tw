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

## <a name="maps-and-filters">Map 與 Filter</a>

## <a name="lambdas">Lambdas</a>

## <a name="folds">Only folds and horses</a>

## <a name="function-application">帶著 $ 的 function application</a>

## <a name="composition">Function composition</a>
