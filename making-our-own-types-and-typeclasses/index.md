---
layout: page
title: 建立我們自己的型別與 Typeclass
prev:
    url: modules
    title: 模組
---

在前面的章節，我們談了一些現有的 Haskell 型別與 typeclass。在這一章，我們要學習如何建立我們自己的型別與 typeclass、以及如何讓它們運作！

## <a name="algebraic-data-types">代數資料型別簡介</a>

到目前為止，我們碰上了不少資料型別。`Bool`、`Int`、`Char`、`Maybe`、等等。但我們要如何建立我們自己的呢？嗯，一種方式是使用 *data* 關鍵字來定義一個型別。讓我們看看 `Bool` 型別在標準函式庫中是如何定義的。

<pre name="code" class="haskell:hs">
data Bool = False | True
</pre>

`data` 代表我們正在定義一個新的資料型別。`=` 之前的部份表示型別，即為 `Bool`。`=` 之後的部份為*值建構子（value constructors）*。它指定了這個型別可以擁有的不同值。`|` 讀作<i>或</i>。所以我們可以將這讀作：`Bool` 型別可以擁有一個 `True` 或 `False` 的值。型別名稱與值建構子都必須為首字母大寫。

以類似的方式，我們可以認為 `Int` 型別是像這樣被定義的：

<pre name="code" class="haskell:hs">
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
</pre>

<img src="img/caveman.png" alt="caveman" style="float:left" />
第一個與最後一個值建構子為 `Int` 可能的最小與最大值。它實際上並不是像這樣定義的，這裡有個省略符號，因為我們省略了許多數字，而這只是為了方便說明。

現在，讓我們想想我們要如何在 Haskell 中表示一個形狀。一種方式是使用 tuple。一個圓可以被表示成 `(43.1, 55.0, 10.4)`，其中第一與第二欄（field）為圓心的座標，而第三欄為半徑。聽起來還可以，但它也可以表示一個三維向量或是其它什麼的。一個更好的解法是建立我們自己的型別來表示一個形狀。讓我們假定一個形狀可以是一個圓形或是一個矩形。以下就是：

<pre name="code" class="haskell:hs">
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
</pre>

這是什麼呢？像這樣想。`Circle` 這個值建構子有三個接收浮點數的欄位。所以當我們在撰寫一個值建構子時，我們可以在其後加上一些可選的型別，而這些型別定義了它將會包含的值。這裡，前兩個欄位為其中心座標，第三個欄位為它的半徑。`Rectangle` 這個值建構子有四個接收浮點數的欄位。前兩個欄位為它左上角的座標，而後兩個欄位為它右下角的座標。

當我提到欄位，我實際上指的是參數。值建構子實際上是個最終會回傳一個資料型別的 function。讓我們看看這兩個值建構子的型別簽名。

<pre name="code" class="haskell:ghci">
ghci> :t Circle
Circle :: Float -> Float -> Float -> Shape
ghci> :t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape
</pre>

酷，所以值建構子就像是其它任何的 function。誰想得到？讓我們建立一個接收一個 shape 並回傳其表面積的 functon。

<pre name="code" class="haskell:hs">
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
</pre>

這裡第一個值得注意的東西是型別宣告。它表示這個 function 接收一個 shape 並回傳一個浮點數。我們無法寫個 `Circle -> Float` 這樣的型別宣告，因為 `Circle` 並非一個型別，`Shape` 才是。就像是我們無法寫一個帶著 `True -> Int` 型別宣告的 function。這裡我們注意到的第二件事，是我們可以針對建構子做模式匹配。我們在針對像是 `[]`、`False` 或 `5` 這些沒有任何欄位的值進行模式匹配之前，先對建構子進行模式匹配。我們只要寫一個建構子，然後將其欄位綁定到名稱上。因為我們只對半徑感興趣，實際上並不關心告訴我們圓在何處的前兩個欄位。

<pre name="code" class="haskell:ghci">
ghci> surface $ Circle 10 20 10
314.15927
ghci> surface $ Rectangle 0 0 100 100
10000.0
</pre>

耶，它正常運作！但若是我們試著在命令列印出 `Circle 10 20 5`，我們會得到一個錯誤。這是因為 Haskell（還）不知道該如何將我們的資料型別作為一個字串顯示。記得，當我們試著在命令列中印出一個值，Haskell 首先會執行 `show` function 以得到我們的值的字串表示，然後它會將之印到終端機。為了讓我們的 `Shape` 型別為 `Show` typeclass 的一員，我們像這樣修改它：

<pre name="code" class="haskell:hs">
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
</pre>

我們現在先不深入探討 deriving。讓我們假定我們將 `deriving (Show)` 加在資料宣告的末端，Haskell 就會自動地令這個型別為 `Show` typeclass 的一員。所以現在，我們可以這樣做：

<pre name="code" class="haskell:ghci">
ghci> Circle 10 20 5
Circle 10.0 20.0 5.0
ghci> Rectangle 50 230 60 90
Rectangle 50.0 230.0 60.0 90.0
</pre>

值建構子為 function，所以我們可以映射它、部分應用它等等。若是我們想要一個不同半徑的同心圓 list，我們可以這樣做：

<pre name="code" class="haskell:ghci">
ghci> map (Circle 10 20) [4,5,6,6]
[Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]
</pre>

我們的資料型別還不錯，雖然它可以更好。讓我們建立一個定義一個在二維空間中的點的中介資料型別。然後我們可以使用它來讓我們的 shape 更加易懂。

<pre name="code" class="haskell:hs">
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
</pre>

注意到在定義一個 point 時，我們在資料型別與值建構子使用了相同的名稱。這沒有特殊的意義，若是只有一個值建構子，讓它使用與型別相同的名稱是很常見的。所以現在 `Circle` 有兩個欄位，一個型別為 `Point` 而另一個型別為 `Float`。這使得理解它是什麼更為容易。對於 rectangle 亦同。我們必須調整我們的 `surface` function 以因應這些改變。

<pre name="code" class="haskell:hs">
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
</pre>

我們必須變更的唯一之處為模式。我們無視在 circle 模式中的整個 point。在 rectangle 模式中，我們僅使用一個巢狀的模式匹配來取得 point 的欄位。若是我們為了某些理由想要參考 point 本身，我們就可以使用這種模式。

<pre name="code" class="haskell:ghci">
ghci> surface (Rectangle (Point 0 0) (Point 100 100))
10000.0
ghci> surface (Circle (Point 0 0) 24)
1809.5574
</pre>

一個移動 shape 的 function 怎麼樣？它取一個 shape、在 x 軸移動它的數量與在 y 軸移動它的數量，然後回傳一個有著相同尺寸、但位在其它某處的新 shape。

<pre name="code" class="haskell:hs">
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))
</pre>

十分直觀。我們將偏移量加到表示 shape 位置的 point 上。

<pre name="code" class="haskell:ghci">
ghci> nudge (Circle (Point 34 34) 10) 5 10
Circle (Point 39.0 44.0) 10.0
</pre>

若是你不想直接處理 point，我們可以建立一些輔助 function，在原點座標建立某種大小的形狀，然後移動它。

<pre name="code" class="haskell:hs">
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)
</pre>

<pre name="code" class="haskell:ghci">
ghci> nudge (baseRect 40 100) 60 23
Rectangle (Point 60.0 23.0) (Point 100.0 123.0)
</pre>

當然，你可以在你的模組中輸出你的資料型別。為了做到這件事，只要將你的型別與你輸出的 function 一同寫下，然後加個括號、在其中指定你想要輸出的值建構子，並以逗號分隔。若是你想要為一個給定的型別輸出所有的值建構子，就寫作 `..`。

若是我們想要在一個模組中輸出我們在這裡定義的 function 與型別，我們可以像這樣開始：

<pre name="code" class="haskell:hs">
module Shapes
( Point(..)
, Shape(..)
, surface
, nudge
, baseCircle
, baseRect
) where
</pre>

藉由 `Shape(..)`，我們為 `Shape` 輸出所有的值建構子，以代表任何引入你模組的人都可以藉由 `Rectangle` 與 `Circle` 值建構子來建立 shape。這等同於寫下 `Shape (Rectangle, Circle)`。

我們也可以只在輸出敘述寫下 `Shape`，選擇不為 `Shape` 輸出任何值建構子。這樣一來，引入我們模組的人只能使用輔助 function `baseCircle` 與 `baseRect` 來建立 shape。`Data.Map` 就使用這種方法。你無法藉由 `Map.Map [(1,2),(3,4)]` 建立一個 map，因為它沒有輸出這個值建構子。然而，你可以藉由其中一個輔助 function──像是 `Map.fromList`──來建立一個 map。記得，值建構子只是取欄位為參數、並回傳某個型別（像是 `Shape`）為結果的 function。所以當我們選擇不輸出它，我們僅避免引入我們模組的人使用這些 function，但若是其它被輸出的 function 回傳這個型別，我們就可以使用它來建立我們自定資料型別的值。

不輸出一個資料型別的值建構子，我們得以隱藏它的實作細節，使得它更加抽象。同時，任何使用我們模組的人都無法對值建構子進行模式匹配。

## <a name="record-syntax">Record 語法</a>

## <a name="type-parameters">型別參數</a>

## <a name="derived-instances">衍生實體</a>

## <a name="type-synonyms">型別同義詞</a>

## <a name="recursive-data-structures">遞迴資料結構</a>

## <a name="typeclasses-102">Typeclasses 102</a>

## <a name="a-yes-no-typeclass">一個 yes-no typeclass</a>

## <a name="the-functor-typeclass">Functor typeclass</a>

## <a name="kinds-and-some-type-foo">Kind 與一些 type-foo</a>
