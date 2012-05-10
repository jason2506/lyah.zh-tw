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

<img src="img/record.png" alt="record" style="float:right" />
好，我們要負責建立一個描述一個人的資料型別。我們要儲存的關於人的資訊是：名字、姓氏、年齡、身高、電話號碼、以及最愛的冰淇淋口味。我不知道你怎麼樣，但這就是關於一個人我想知道的全部了。讓我們來試試看吧！

<pre name="code" class="haskell:hs">
data Person = Person String String Int Float String String deriving (Show)
</pre>

好，第一欄為名字、第二欄為姓氏、第三欄為年齡，以此類推。讓我們建立一個 person。

<pre name="code" class="haskell:ghci">
ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
ghci> guy
Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
</pre>

這有點兒酷，雖然稍微不太好讀。若是我們想要建立一個從一個 person 取得獨立資訊的 function 呢？一個取得某個 person 名字的 function、一個取得某個 person 的姓氏、等等。嗯，我們必須像這樣定義它們：

<pre name="code" class="haskell:hs">
firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) = number

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor
</pre>

呼！我當然不喜歡這樣寫！儘管寫起來非常累贅且**無聊**，這種方法還是可以運作的。

<pre name="code" class="haskell:ghci">
ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
ghci> firstName guy
"Buddy"
ghci> height guy
184.2
ghci> flavor guy
"Chocolate"
</pre>

你說，必須有個更好的方法！不，沒有，抱歉。

開個玩笑，有啦。哈哈！Haskell 的創作者都非常聰明，並預料到了這種情況。他們引入了一個撰寫資料型別的替代方法。以下就是我們如何以 record 語法達成上面的功能。

<pre name="code" class="haskell:hs">
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)
</pre>

所以並非一個接著一個寫下欄位名稱並以空白分隔，我們要使用大括號。我們先寫下欄位的名稱──舉例來說，`firstName`，然後我們寫下兩個冒號 `::`（也被稱為 Paamayim Nekudotayim<span class="note">〔譯註：希伯來語，「兩個冒號」的意思〕</span>，哈哈），然後我們指定其型別。產生的資料型別完全相同。這樣的主要好處是，它建立了查詢資料型別中欄位的 function。藉由使用 record 語法來建立這個資料型別，Haskell 會自動建立這些 function：`firstName`、`lastName`、`age`、`height`、`phoneNumber` 與 `flavor`。

<pre name="code" class="haskell:ghci">
ghci> :t flavor
flavor :: Person -> String
ghci> :t firstName
firstName :: Person -> String
</pre>

有另一個使用 record 語法的好處。當我們為型別衍生（derive） `Show` 時，若是我們使用 record 語法來定義並實體化型別，它會以不同方式顯示它。假使我們有個表示一輛車的型別。我們要記錄製造它的公司、型號名稱與它的生產年份。看：

<pre name="code" class="haskell:hs">
data Car = Car String String Int deriving (Show)
</pre>

<pre name="code" class="haskell:ghci">
ghci> Car "Ford" "Mustang" 1967
Car "Ford" "Mustang" 1967
</pre>

若是我們使用 record 語法定義它，我們可以像這樣建一個新的 car：

<pre name="code" class="haskell:ghci">
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
</pre>

<pre name="code" class="haskell:ghci">
ghci> Car {company="Ford", model="Mustang", year=1967}
Car {company = "Ford", model = "Mustang", year = 1967}
</pre>

當建立一個新的 car，我們不必以正確的順序放置欄位，只要我們將它們全部列出來。但若是我們不使用 record 語法，我們則必須照順序指定它們。

在一個建構子有多個欄位，且哪個欄位是哪個並不明顯時使用 record 語法。若是我們藉由 `data Vector = Vector Int Int Int` 建立一個一個三維向量資料型別，欄位為向量的分量是十分顯而易見的。然而，在我們的 `Person` 與 `Car` 型別中的欄位就不是十分明顯，我們便大大受益於使用 record 語法。

## <a name="type-parameters">型別參數</a>

一個值建構子可以接收一些值作為參數，然後產生一個新的值。舉例來說，`Car` 建構子接收三個值並產生一個 car 值。以類似的方式，*型別建構子（type constructor）*可以接收型別作為參數以產生新的型別。這起初聽起來可能有一點太深奧，但是它沒有這麼複雜。如果你熟悉 C++ 中的模版（template），你會看到一些相似之處。為了要搞清楚型別參數實際上如何運作，讓我們看看一個我們已經遇過的型別是如何實作的。

<pre name="code" class="haskell:hs">
data Maybe a = Nothing | Just a
</pre>

<img src="img/yeti.png" alt="yeti" style="float:left" />
這裡的 `a` 為型別參數。且因為有個型別參數，所以我們將 `Maybe` 稱作一個型別建構子。根據我們想要在它並非為 `Nothing` 時持有的資料型別，這個型別建構子最終可以產生一個 `Maybe Int`、`Maybe Car`、`Maybe String`、等等型別。沒有值的型別可以為 `Maybe`，因為這本身並非一個型別，它是個型別建構子。為了令其作為一個可以具有值的真的型別，它必須填滿它所有的型別參數。

所以若是我們將 `Char` 作為型別參數傳遞給 `Maybe`，我們就得到了一個 `Maybe Char` 型別。舉例來說，`Just 'a'` 的型別就為 `Maybe Char`。

你也許不知道，但我們在使用 `Maybe` 之前使用過另一個具有一個型別參數的型別。這個型別即是 list 型別。儘管有些語法糖衣，list 型別實際上是取一個參數以產生一個具體的（concrete）型別。值可以為 `[Int]` 型別、`[Char]` 型別、`[[String]]` 型別，但你無法有個型別為 `[]` 的值。

讓我們試試這個 `Maybe` 型別。

<pre name="code" class="haskell:ghci">
ghci> Just "Haha"
Just "Haha"
ghci> Just 84
Just 84
ghci> :t Just "Haha"
Just "Haha" :: Maybe [Char]
ghci> :t Just 84
Just 84 :: (Num t) => Maybe t
ghci> :t Nothing
Nothing :: Maybe a
ghci> Just 10 :: Maybe Double
Just 10.0
</pre>

型別參數是很有用的，因為我們可以根據我們想要包含在我們資料型別中的型別種類來建立不同的型別。當我們執行 `:t Just "Haha"` 時，型別推導引擎發現它的型別為 `Maybe [Char]`，因為若是 `Just a` 中的 `a` 為一個字串，則 `Maybe a` 中的 `a` 也必須是個字串。

注意到 `Nothing` 的型別為 `Maybe a`。它的型別是多型的。若是某些 function 要求一個 `Maybe Int` 作為參數，我們可以給它一個不包含任何值的 `Nothing` 而不會有問題。`Maybe a` 型別在必要時可以視為一個 `Maybe Int`，就像是 `5` 可以作為一個 `Int` 或是一個 `Double`。相似的，空 list 的型別為 `[a]`。一個空 list 可以視為一個任意型別的 list。這就是為什麼我們可以執行 `[1,2,3] ++ []` 與 `["ha","ha","ha"] ++ []`。

使用型別參數是非常有益的，但只有在你合理使用它的時候。我們通常在無論我們的資料型別內部的值的型別為何都會運作時使用型別參數，像是我們的 `Maybe a` 型別。若是我們的型別像是某種盒子，使用型別參數就還不錯。我們可以將我們的 `Car` 資料型別從這樣：

<pre name="code" class="haskell:hs">
data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)
</pre>

改成這樣：

<pre name="code" class="haskell:hs">
data Car a b c = Car { company :: a
                     , model :: b
                     , year :: c
                     } deriving (Show)
</pre>

但我們真的得到好處了嗎？答案是：或許沒有，因為我們最終定義的 function 只會運作在 `Car String String Int` 型別。舉例來說，給定我們的 `Car` 的第一個定義，我們可以建立一個以簡短文字顯示汽車屬性的 function。

<pre name="code" class="haskell:hs">
tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
</pre>

<pre name="code" class="haskell:ghci">
ghci> let stang = Car {company="Ford", model="Mustang", year=1967}
ghci> tellCar stang
"This Ford Mustang was made in 1967"
</pre>

一個可愛的小 function！型別宣告很可愛，且其運作良好。現在若是 `Car` 為 `Car a b c` 怎麼樣？

<pre name="code" class="haskell:hs">
tellCar :: (Show a) => Car String String a -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
</pre>

我們必須迫使這個 function 接收一個型別為 `(Show a) => Car String String a` 的 `Car` 型別。你可以看到型別簽名更加複雜，而我們實際上得到的唯一的好處是，我們可以使用任何為 `Show` typeclass 實體的型別作為 `a` 的型別。

<pre name="code" class="haskell:ghci">
ghci> tellCar (Car "Ford" "Mustang" 1967)
"This Ford Mustang was made in 1967"
ghci> tellCar (Car "Ford" "Mustang" "nineteen sixty seven")
"This Ford Mustang was made in \"nineteen sixty seven\""
ghci> :t Car "Ford" "Mustang" 1967
Car "Ford" "Mustang" 1967 :: (Num t) => Car [Char] [Char] t
ghci> :t Car "Ford" "Mustang" "nineteen sixty seven"
Car "Ford" "Mustang" "nineteen sixty seven" :: Car [Char] [Char] [Char]
</pre>

<img src="img/meekrat.png" alt="meekrat" style="float:right" />
在現實生活中，我們大多數時候都是使用 `Car String String Int`，所以看起來並不是很值得參數化（parameterize）`Car` 型別。我們通常在包含於資料型別內部的型別的各個值建構子，對於型別運作並不是非常重要的時候使用型別參數。某個東西的 list 就是一串東西的列表，不管這個東西的型別為何，它都仍然可以運作。若是我們想要加總一個數字 list，我們可以之後在加總 function 中指定我們明確地想要一個數字 list。`Maybe` 亦同。`Maybe` 表示一個「沒有值」或是「有某一個值」的選項。無論這某一個值的型別為何。

另一個我們已經遇過的參數化型別例子是 `Data.Map` 中的 `Map k v`。`k` 為在一個 map 中的 kye 的型別，而 `v` 為 value 的型別。這是型別參數非常有用的一個很好的例子。map 的參數化使我們能夠從任意型別映射到其它任意型別，只要 key 的型別為 `Ord` typeclass 的一員。
若是我們要定義一個 map 型別，我們可以在 <i>data</i> 宣告中可以加上一個 typeclass 限制：

<pre name="code" class="haskell:hs">
data (Ord k) => Map k v = ...
</pre>

然而，*絕對不要在 <i>data</i> 宣告中加上 typeclass 限制*是個在 Haskell 中的非常奇怪的約定。為什麼？嗯，因為我們並不會得到很多好處，但我們最終會寫下更多類別限制，即使是在我們不需要它們的時候。無論我們在 `Map k v` 的 <i>data</i> 宣告中擺或不擺 `Ord k` 限制，我們都必須將這個限制擺進假定 map 中的 key 可以被排序的 function 中。但若是我們不將這個限制擺進 <i>data</i> 宣告中，我們就不必將 `(Ord k) =>` 擺進不在乎 key 是否可以被排序的 function 的型別宣告中。一個這種 function 的範例是 `toList`，它取一個 map 並將它轉成一個關聯列表。它的型別簽名為 `toList :: Map k a -> [(k, a)]`。若是 `Map k v` 在它的 <i>data</i> 宣告中有一個型別限制，`toList` 的型別就必須為 `toList :: (Ord k) => Map k a -> [(k, a)]`，即使這個 function 並不會對 key 做任何比較。

所以別將型別限制擺進 <i>data</i> 宣告中──即使它看起來很合理，因為無論哪種方式你都必須將它擺進 function 的型別宣告中。

讓我們實作一個三維向量型別，並為它加上一些操作。我們要使用一個參數化型別，因為即使它通常會包含數字型別，
它仍然會支援多個不同的數字類型。

<pre name="code" class="haskell:hs">
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n
</pre>

`vplus` 是用來將兩個向量加在一起。兩個向量藉由加總它們對應的分量來相加。`scalarMult` 是用來求兩個向量的內積，`vectMult` 是用來將一個向量乘上一個純量。這些 function 可以操作 `Vector Int`、`Vector Integer`、`Vector Float`、等等，只是 `Vector a` 的 `a` 要為 `Num` typeclass。同樣的，若是你為這些 function 檢驗型別宣告，你就會看到它們只可以操作相同型別的向量，而涉及的數字也必須是包含在向量中的型別。注意到我們沒有將一個 `Num` 類別限制擺進 <i>data</i> 宣告中，因為我們無論如何都必須在 function 中重複它。

再一次，區分型別建構子與值建構子是非常重要的。宣告一個資料型別時，在 `=` 之前的部份為型別建構子，而在它之後的建構子（或許會以 `|` 分隔）為值建構子。為一個 function 給定一個 `Vector t t t -> Vector t t t -> t` 型別是不對的，因為我們必須將型別擺進型別宣告中，且 vector *型別*建構子只取一個參數，而值建構子則是取三個。讓我們試試我們的 vector：

<pre name="code" class="haskell:ghci">
ghci> Vector 3 5 8 `vplus` Vector 9 2 8
Vector 12 7 16
ghci> Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
Vector 12 9 19
ghci> Vector 3 9 7 `vectMult` 10
Vector 30 90 70
ghci> Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0
74.0
ghci> Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)
Vector 148 666 222
</pre>

## <a name="derived-instances">衍生實體</a>

## <a name="type-synonyms">型別同義詞</a>

## <a name="recursive-data-structures">遞迴資料結構</a>

## <a name="typeclasses-102">Typeclasses 102</a>

## <a name="a-yes-no-typeclass">一個 yes-no typeclass</a>

## <a name="the-functor-typeclass">Functor typeclass</a>

## <a name="kinds-and-some-type-foo">Kind 與一些 type-foo</a>
