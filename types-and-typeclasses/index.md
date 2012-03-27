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

<img src="img/classes.png" alt="class" style="float:right" />
一個 typeclass 是一種定義某種行為的介面（interface）。假如一個型別屬於某個 typeclass，則意味著它支援並實作了 typeclass 所描述的行為。很多來自於 OOP 的人對 typeclass 感到困惑，因為它就像是物件導向語言（object oriented language）裡的類別（class）。嗯，但它不是。你可以把它想成某種類似 Java interface，只是比它更好的東西。

`==` function 的型別簽名（signature）是什麼呢？

<pre name="code" class="haskell: ghci">
ghci> :t (==)
(==) :: (Eq a) => a -> a -> Bool
</pre>

<p class="hint">
<em>註記：</em>相等運算子──<code>==</code> 為一個 function。<code>+</code>、<code>*</code>、<code>-</code>、<code>/</code> 與幾乎所有的運算子也都是 function。假使一個 function 只由特殊字元組成，其預設被視為中綴 function。假如想要檢驗它們的型別、將它傳遞到另一個 function、或是作為前綴 function 來呼叫它，我們必須將它包在括號裡。
</p>

有意思。這裡我們看到一個新東西，`=>` 符號。在 `=>` 符號之前的所有東西被稱為一個*類別約束（class constraint）*。我們可以像這樣閱讀先前的型別宣告：`==` 接收任兩個相同型別的值，並回傳一個 `Bool`。這兩個值的型別必須是 `Eq` class 的一個成員（這就是類別約束）。

`Eq` typeclass 提供一個測試相等性的介面。任何具有測試兩個值相等性意義的型別都應該是 `Eq` class 的成員。除了 IO（處理輸入與輸出的型別）與 function 之外，所有 Haskell 的標準型別都屬於`Eq` typeclass。

因為 `elem` function 將 `==` 使用在一個 list 之上，以檢查我們正在尋找的某個值是否在此 list 之中，所以它的型別為 `(Eq a) => a -> [a] -> Bool`。

幾個基本的 typeclass 如下：

<code class="label class">Eq</code> 被用在支援相等性測試的型別。其成員實作的 function 為 `==` 與 `/=`。所以如果在一個 function 裡有個針對某型別的 `Eq` 類別約束，代表這個 function 在定義中的某處使用了 `==` 或 `/=`。除了 function 以外，我們先前提到的所有型別皆屬於 `Eq`，所以它們可以被測試相等性。

<pre name="code" class="haskell: ghci">
ghci> 5 == 5
True
ghci> 5 /= 5
False
ghci> 'a' == 'a'
True
ghci> "Ho Ho" == "Ho Ho"
True
ghci> 3.432 == 3.432
True
</pre>

<code class="label class">Ord</code> 代表擁有順序的型別。

<pre name="code" class="haskell: ghci">
ghci> :t (>)
(>) :: (Ord a) => a -> a -> Bool
</pre>

除了 function 以外，我們先前涵蓋的所有型別皆屬於 `Ord`。`Ord` 涵蓋所有標準的比較大小的 function，像是 `>`、`<`、`>=` 與 `<=`。`compare` function 接收兩個相同型別的 `Ord` 成員，並回傳一個比較大小的結果。
<code class="label type">Ordering</code> 為一個可以為 `GT`、`LT` 或 `EQ` 的型別，分別代表<i>大於</i>、<i>小於</i>與<i>等於</i>。

要成為 `Ord` 的成員，一個型別必須先加入著名又時髦的 `Eq` 俱樂部。

<pre name="code" class="haskell: ghci">
ghci> "Abrakadabra" < "Zebra"
True
ghci> "Abrakadabra" `compare` "Zebra"
LT
ghci> 5 >= 2
True
ghci> 5 `compare` 3
GT
</pre>

<code class="label class">Show</code> 的成員可以作為字串表示。除了 function 之外，到目前為止涵蓋的所有型別都屬於 `Show`。最常用來處理 `Show` typeclass 的 function 為 `show`。它接收一個型別為 `Show` 成員的值，並以一個字串表示這個值。

<pre name="code" class="haskell: ghci">
ghci> show 3
"3"
ghci> show 5.334
"5.334"
ghci> show True
"True"
</pre>

<code class="label class">Read</code> 是與 `Show` 相對的 typeclass。`read` function 接收一個字串並傳回一個為 `Read` 成員的型別。

<pre name="code" class="haskell: ghci">
ghci> read "True" || False
True
ghci> read "8.2" + 3.8
12.0
ghci> read "5" - 2
3
ghci> read "[1,2,3,4]" ++ [3]
[1,2,3,4,3]
</pre>

一切都很好。再一次，目前為止涵蓋的所有型別皆在這個 typeclass 中。不過如果我們試著執行 `read "4"` 會發生什麼事？

<pre name="code" class="haskell: ghci">
ghci> read "4"
&lt;interactive&gt;:1:0:
    Ambiguous type variable `a' in the constraint:
      `Read a' arising from a use of `read' at &lt;interactive&gt;:1:0-7
    Probable fix: add a type signature that fixes these type variable(s)
</pre>

這裡 GHCI 告訴我們的是，它不知道我們想要的是什麼傳回值。注意到先前使用 `read` 時，我們會在其結果之後做某些事。經由這種方式，GHCI 可以推論我們想要從 `read` 得到的是哪種結果。假使我們作為一個布林值來使用它，它便知道它必須回傳一個 `Bool`。但是現在，它知道我們想要某個屬於 `Read` class 的型別，只是不知道是哪一個。讓我們看看 `read` 的型別簽名：

<pre name="code" class="haskell: ghci">
ghci> :t read
read :: (Read a) => String -> a
</pre>

看到了嗎？它回傳一個屬於 `Read` 的型別，不過如果我們並未在之後嘗試以某種方式使用它，它就沒有方法知道是哪個型別。這就是為什麼我們要使用明確的*型別註釋（type annotation）*。型別註釋為一種明確表示一個 expression 型別該是什麼的方式。我們藉由在 expression 後面加上 `::` 然後指定一個型別以做到這件事。看看吧：

<pre name="code" class="haskell: ghci">
ghci> read "5" :: Int
5
ghci> read "5" :: Float
5.0
ghci> (read "5" :: Float) * 4
20.0
ghci> read "[1,2,3,4]" :: [Int]
[1,2,3,4]
ghci> read "(3, 'a')" :: (Int, Char)
(3, 'a')
</pre>

大多數 expression 都能夠讓編譯器自己可以推論它們的型別。但有時候，對於一個像是 `read "5"` 這樣的 expression，編譯器無法知道要傳回一個 `Int` 或是 `Float` 型別的值。為了得知是哪種型別，Haskell 必須實際去對 `read "5"` 求值。但由於 Haskell 是一種靜態型別語言，它必須在程式碼被編譯（或是在 GHCI 被求值的情況）之前知道所有的型別。所以我們必須告訴 Haskell：「嘿，這個 expression 應該是這個型別，如果你不知道的話！」

<code class="label class">Enum</code> 成員為照順序排列的型別──它們可以被列舉。`Enum` typeclass 的主要好處是我們可以在 list range 裡使用它的型別。它也定義了後繼子與前置子（predecesor）──你可以用 `succ` 與 `pred` function 取得。在這個 class 的類別有：`()`、`Bool`、`Char`、`Ordering`、`Int`、`Integer`、`Float` 與 `Double`。

<pre name="code" class="haskell: ghci">
ghci> ['a'..'e']
"abcde"
ghci> [LT .. GT]
[LT,EQ,GT]
ghci> [3 .. 5]
[3,4,5]
ghci> succ 'B'
'C'
</pre>

<code class="label class">Bounded</code> 成員擁有一個上界與下界。

<pre name="code" class="haskell: ghci">
ghci> minBound :: Int
-2147483648
ghci> maxBound :: Char
'\1114111'
ghci> maxBound :: Bool
True
ghci> minBound :: Bool
False
</pre>

`minBound` 與 `maxBound` 很有趣，因為它們的型別為 `(Bounded a) => a`。在某種意義上，它們都是多型常數（constants）。

若 tuple 裡的元素屬於 `Bounded`，則此 tuple 也屬於 `Bounded`。

<pre name="code" class="haskell: ghci">
ghci> maxBound :: (Bool, Int, Char)
(True,2147483647,'\1114111')
</pre>

<code class="label class">Num</code> 為一個數字的 typeclass。它的成員擁有能夠作為數字的特性。讓我們來檢驗一個數字的型別：

<pre name="code" class="haskell: ghci">
ghci> :t 20
20 :: (Num t) => t
</pre>

看來所有數字都是多型常數。它們可以作為任何屬於 `Num` typeclass 的型別。

<pre name="code" class="haskell: ghci">
ghci> 20 :: Int
20
ghci> 20 :: Integer
20
ghci> 20 :: Float
20.0
ghci> 20 :: Double
20.0
</pre>

這些都是屬於 `Num` typeclass 的型別。如果我們檢驗 `*` 的型別，我們會看到它接受一切數字：

<pre name="code" class="haskell: ghci">
ghci> :t (*)
(*) :: (Num a) => a -> a -> a
</pre>

它接收兩個相同型別的數字，並傳回此型別的一個數字。這就是為什麼 `(5 :: Int) * (6 :: Integer)` 會產生一個型別錯誤，而 `5 * (6 :: Integer)` 卻能運作良好並產生一個 `Integer` 的原因，因為 `5` 能夠作為一個 `Integer` 或是一個 `Int`。

想加入 `Num`，一個型別必須已經跟 `Show` 與 `Eq` 打好關係。

<code class="label class">Integral</code> 也是一個數字的 typeclass。`Num` 包含所有的數字，包含實數與整數，而 `Integral` 只包含（所有的）整數。`Int` 與 `Integer` 屬於此 typeclass。

<code class="label class">Floating</code> 只包含浮點數，所以是 `Float` 與 `Double`。

有個能處理數字的有用 function 是 <code class="label function">fromIntegral</code>。它的型別宣告為 `fromIntegral :: (Num b, Integral a) => a -> b`。從它的型別簽名，我們發現它接收一個整數，並將它轉成更一般化的數字。這在我們想要讓整數與浮點數型別一起良好運作的時候是有用的。舉例來說，`length` function 的型別宣告為 `length :: [a] -> Int`，而不是 `(Num b) => length :: [a] -> b`。我想這是為了歷史因素或是其他什麼的──但在我看來，這十分愚蠢。總之，假如我們嘗試取得一個 list 的長度再加上 `3.2`，我們將會得到一個錯誤，因為我們嘗試將一個 `Int` 與一個浮點數加在一起。所以為了避開這個錯誤，我們執行 `fromIntegral (length [1,2,3,4]) + 3.2` 就全都解決了。

注意到 `fromIntegral` 在它的型別簽名中有許多類別約束，這是完全合法的。如你所見，類別約束在括號內以逗點隔開。
