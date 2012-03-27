---
layout: page
title: 型別與 Typeclass
prev:
    url: starting-out
    title: 出發
---

## <a name="believe-the-type">相信型別</a>

<img src="img/cow.png" alt="moo" style="float:left" />
先前我們提到 Haskell 擁有個靜態型別系統。所有 expression 的型別在編譯期皆為已知，這使得程式碼更為安全。假使你寫了一個嘗試將布林型別除以某個數字的程式，它將無法編譯。這挺不錯的，因為這些錯誤能夠在編譯期抓到，比起讓你的程式崩潰（crash）要來得好。不像 Java 或是 Pascal，Haskell 擁有型別推導。假如你寫下一個數字，我們不必告知 Haskell 它是個數字。它可以自己推論出來，所以我們不必明確寫下我們 function 中的資料型別，expression 就能夠把這件事情做好。我們已涵蓋了 Haskell 的一些基礎，其中只有非常粗略地看過型別一眼。然而，瞭解型別系統是學習 Haskell 的一個非常重要的部份。

一個型別是一種每個 expression 都有的標記。它告訴我們這個 expression 所屬的類別（category）。`True` 這個 expression 是一個布林值、`"hello"` 為一個字串等等。

現在我們要使用 GHCI 來測試某些 expression 的型別。我們將使用 `:t` 命令緊接著任何合法的 expression 來告訴我們它的型別。讓我們試一試：

<pre name="code" class="haskell: ghci">
ghci> :t 'a'
'a' :: Char
ghci> :t True
True :: Bool
ghci> :t "HELLO!"
"HELLO!" :: [Char]
ghci> :t (True, 'a')
(True, 'a') :: (Bool, Char)
ghci> :t 4 == 5
4 == 5 :: Bool
</pre>

<img src="img/bomb.png" alt="bomb" style="float:right" />
這裡我們看到對一個 expression 執行 `:t`，會印出此 expression 緊接著 `::` 與它的型別。`::` 讀做「型別為（has type of）」。明確的型別總是被表示為首字母大寫。`'a'`──如同它看起來的樣子──型別為 `Char`。要斷定它是個字元並不困難。`True` 為 `Bool` 型別。有道理。不過這個呢？測試 `"HELLO!"` 的型別產生一個 `[Char]`。方括號代表一個 list。所以我們將其讀作一個字元 list。不若 list，每個 tuple 長度都有其型別。所以 `(True, 'a')` 這個 expression 型別為 `(Bool, Char)`，而像是 `('a','b','c')` 這樣的 expression 型別將是 `(Char, Char, Char)`。`4 == 5` 總是傳回 `False`，所以它的型別為 `Bool`。

function 也有型別。當寫下我們自己的 function 時，我們可以選擇給它一個明確的型別宣告（declaration）。這一般被視為一個好的作法，除非是在寫非常短的 function 的時候。從現在開始，我們將會為所有我們建立的 function 加上型別宣告。還記得我們先前建立的，用來過濾一個字串並只留下大寫字母的 list comprehension 嗎？它加上型別宣告看起來像這樣：

<pre name="code" class="haskell: hs">
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
</pre>

`removeNonUppercase` 型別為 `[Char] -> [Char]`，代表它從一個字串映射（map）到一個字串。這是因為其接收一個字串作為參數，並傳回另一個字串作為結果。`[Char]` 與 `String` 是同義的，所以如果我們寫成 `removeNonUppercase :: String -> String` 會更清楚。我們不必給定此 function 一個型別宣告，因為編譯器能夠自己推論出來：這是一個接收字串、回傳字串的 function，不過無論如何我們還是這麼做了。但是我們該如何寫一個接收許多參數的 function 型別呢？這裡有個簡單的 function，其接收三個整數並將它們加總在一起：

<pre name="code" class="haskell: hs">
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
</pre>

參數以 `->` 分隔，且參數與回傳型別並沒有特別的區別。回傳型態為宣告的最後一項，而參數則為前三項。之後我們將會看到為什麼它們全都只以 `->` 分隔，而不是在回傳型態與參數之間有些更明顯的區別，像是 `Int, Int, Int -> Int` 之類的。

假如你想要給定你的 function 一個型別宣告，不過並不確定它應該是什麼，你可以寫一個沒有型別宣告的 function，然後再用 `:t` 來確認它的型別。function 也是 expression，所以 `:t` 能夠毫無問題地運作。

以下是某些常見型別的總覽：

<code class="label type">Int</code> 代表整數。`7` 可以為一個 `Int`，不過 `7.2` 不能。`Int` 是有界限的（bounded），意味著它擁有一個最小值與一個最大值。通常在 32 位元的機器上，`Int` 可能的最大值為 2147483647，而最小值為 -2147483648。

<code class="label type">Integer</code> 代表....呃....也是整數。主要的不同是它是無界限的，所以它可以被用來表示非常非常大的數字。我的意思是，真的非常大。然而，`Int` 的效率是比較高的。

<pre name="code" class="haskell: hs">
factorial :: Integer -> Integer
factorial n = product [1..n]
</pre>

<pre name="code" class="haskell: ghci">
ghci> factorial 50
30414093201713378043612608166064768844377641568960512000000000000
</pre>

<code class="label type">Float</code> 為一個單精度（precision）浮點數。

<pre name="code" class="haskell: hs">
circumference :: Float -> Float
circumference r = 2 * pi * r
ghci> circumference 4.0
25.132742
</pre>

<code class="label type">Double</code> 為一個雙精度（precision）浮點數！

<pre name="code" class="haskell: hs">
circumference' :: Double -> Double
circumference' r = 2 * pi * r
ghci> circumference' 4.0
25.132741228718345
</pre>

<code class="label type">Bool</code> 為一個布林型別。它只能有兩種值：`True` 與 `False`。

<code class="label type">Char</code> 表示一個字元。它以單引號表示。一個字元 list 為一個字串。

tuple 為型別，不過它取決於它們的長度以及元素的型別，所以理論上有無限個 tuple 型別，多到此教學無法涵蓋。注意到空的 tuple <code class="label type">()</code> 也是一個型別，它只能有一個單一值：`()`。

## <a name="type-variables">型別變數</a>

你覺得 `head` function 的型別是什麼？因為 `head` 接收一個任意型別的 list，並傳回第一個元素，所以它會是什麼呢？讓我們確認看看！

<pre name="code" class="haskell: ghci">
ghci> :t head
head :: [a] -> a
</pre>

<img src="img/box.png" alt="box" style="float:left" />
唔！這個 `a` 是什麼？它是一個型別嗎？還記得我們先前所說，型別被寫成首字母大寫，所以它不完全是個型別。因為其並非首字母大寫，所以它實際上是一個*型別變數（type variable）*。這代表 `a` 可以是任何型別。這非常像其他語言中的泛型（generic），只是在 Haskell 中它更為強大，因為它允許我們輕易地撰寫非常一般化的 function，只要這個 function 沒有在其中使用到任何型別的特定行為。擁有型別變數的 function 被稱為*多型（polymorphic）function*。`head` 的型別宣告表達它接收一個任意型別的 list，並傳回一個相同型別的元素。

雖然型別變數可以有一個字元以上的名稱，我們通常還是取名叫做 a、b、c、d....

記得 `fst` 嗎？其傳回一個 pair 的第一個元素。讓我們來檢驗它的型別。

<pre name="code" class="haskell: ghci">
ghci> :t fst
fst :: (a, b) -> a
</pre>

我們看到 `fst` 接收一個包含兩種型別的 tuple，並傳回一個型別與 pair 第一個元素相同的元素。這就是為什麼我們可以將 `fst` 使用在包含任意兩個型別的 pair 的原因。注意到只因為 `a` 與 `b` 是不同的型別變數，不代表它們必須為不同型別。它僅表達第一個元素的型別與回傳型別是相同的。

## <a name="typeclasses-101">Typeclasses 101</a>
