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

<img src="img/gob.png" alt="gob" style="float:right" />
在 [Typeclasses 101](types-and-typeclasses#typeclasses-101) 這一節，我們解釋過 typeclass 的基礎。我們解釋過一個 typeclass 是一種定義某種行為的介面。若是一個型別支援這些動作，它就可以作為一個 typeclass 的*實體（instance）*。例如：`Int` 型別為 `Eq` typeclass 的實體，因為 `Eq` typeclass 定義了值可以被比較相等性的行為。且因為整數可以被比較相等性，所以 `Int` 即為 `Eq` typeclass 的一員。實際用途在於作為 `Eq` 介面的 function，叫做 `==` 與 `/=`。如果一個型別為 `Eq` typeclass 的一員，我們就能夠以這種型別的值使用 `==` function。這就是為什麼像是 `4 == 4` 與 `"foo" /= "bar"` 這種 expression 要做型別檢查。

我們也提過它通常會跟像是 Java、Python、C++ 以及類似語言中的類別搞混，這使得不少人感到困惑。在這些語言中，類別是一張藍圖，接著我們從中建立包含狀態（state）且能夠做某些行為的物件。typeclass 更像是介面。我們不會從 typeclass 建立資料。取而代之的，我們首先建立我們的資料型別，然後我們思考它可以作為什麼。如果它可以作為某個可以被比較相等性的值，我們就令它為 `Eq` typeclass 的實體。如果它可以作為某個可以被排序的值，我們就令它為 `Ord` typeclass 的實體。

在下一節，我們將看看我們可以如何藉由實作由 typeclass 定義的 function，手動地建立我們的 typeclass 型別實體。但現在，讓我們看看 Haskell 可以如何自動地令我們自己的型別為下列任何 typeclass 的實體：`Eq`、`Ord`、`Enum`、`Bounded`、`Show`、`Read`。若是我們在建立我們的資料型別時使用 <i>deriving</i> 關鍵字，Haskell 就能夠以這些 context 衍生我們型別的行為。

考慮到這個資料型別：

<pre name="code" class="haskell:hs">
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     }
</pre>

它描述了一個人。讓我們假設沒有兩個人有相同名字、姓氏與年齡的組合。現在，若是我們有兩個人的紀錄，看看它們是否表示同一個人是否合理？當然囉。我們可以試著比較兩者的相等性，再看看它們是否相等。這就是為什麼這個型別為 `Eq` typeclass 一員是合理的。我們要衍生這個實體。

<pre name="code" class="haskell:ghci">
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq)
</pre>

當我們為一個型別衍生 `Eq` 實體，然後嘗試以 `==` 或是 `/=` 比較兩個此型別的值，Haskell 將會看看值建構子是否匹配（雖然這裡只有一個值建構子），然後它會藉由 `==` 測試每一對欄位，檢查所有包含在內部的資料是否匹配。只有一個問題，所有欄位的型別也必須為 `Eq` typeclass 的一員。但由於 `String` 與 `Int` 皆是，所以是沒問題的。讓我們測試我們的 `Eq` 實體。

<pre name="code" class="haskell:ghci">
ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
ghci> let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
ghci> let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}
ghci> mca == adRock
False
ghci> mikeD == adRock
False
ghci> mikeD == mikeD
True
ghci> mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}
True
</pre>

當然，由於 `Person` 目前屬於 `Eq`，所以我們可以將它用作所有在其型別簽名中有個 `Eq a` 類別限制的 function 中的 `a`，像是 `elem`。

<pre name="code" class="haskell:ghci">
ghci> let beastieBoys = [mca, adRock, mikeD]
ghci> mikeD `elem` beastieBoys
True
</pre>

`Show` 與 `Read` typeclass 分別是可以被轉成字串或是由字串轉成的值。如同 `Eq`，若是我們想要令我們的型別為 `Show` 與 `Read` typeclass 的實體，且型別的建構子擁有欄位，它們的型別也必須為 `Show` 或是 `Read` 的一員。讓我們令我們的 `Person` 資料型別也為 `Show` 與 `Read` 的一員。

<pre name="code" class="haskell:hs">
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)
</pre>

現在我們可以在終端機印出一個 person。

<pre name="code" class="haskell:ghci">
ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
ghci> mikeD
Person {firstName = "Michael", lastName = "Diamond", age = 43}
ghci> "mikeD is: " ++ show mikeD
"mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"
</pre>

如果我們在令 `Person` 資料型別為 `Show` 的一員前嘗試在終端機印出 person，Haskell 將會向我們抱怨、聲稱它不知道該如何以一個字串表達一個 person。但現在我們已經為它衍生了一個 `Show` 實體，它就知道了。

`Read` 幾乎是與 `Show` 相反的 typeclass。`Show` 是為了將我們的型別的值轉成字串，`Read` 則是為了將字串轉成我們的型別的值。記住，當我們使用 `read` function 時，我們必須使用一個顯式的型別註釋（type annotation）以告訴 Haskell 我們想要作為結果得到的是哪個型別。若是你不明確表示我們想要作為結果的型別，Haskell 就不知道我們想要的是哪個型別。

<pre name="code" class="haskell:ghci">
ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person
Person {firstName = "Michael", lastName = "Diamond", age = 43}
</pre>

如果我們在之後使用我們的 `read` 結果，Haskell 就能夠推論它應該將其讀作一個 person，我們就不必使用型別註釋了。

<pre name="code" class="haskell:ghci">
ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" == mikeD
True
</pre>

我們也可以 read 參數化型別，但我們必須填入型別參數。所以我們無法執行 `read "Just 't'" :: Maybe a`，但我們可以執行 `read "Just 't'" :: Maybe Char`。

我們可以為 `Ord` typeclass 衍生實體──其代表擁有可以被排序的值的型別。如果我們比較兩個相同型別、但使用不同建構子建立的值，以先定義的建構子建立的值會被視為比較小的。舉例來說，考慮 `Bool` 型別，它可以擁有 `False` 或是 `True` 任一個值。為了看看它在被比較時的行為，我們可以想成它像這樣實作：

<pre name="code" class="haskell:hs">
data Bool = False | True deriving (Ord)
</pre>

因為 `False` 值建構子先被指定，而 `True` 值建構子在其之後被指定，所以我們可以將 `True` 視為比 `False` 還大。

<pre name="code" class="haskell:ghci">
ghci> True `compare` False
GT
ghci> True > False
True
ghci> True < False
False
</pre>

在 `Maybe a` 資料型別中，`Nothing` 值建構子在 `Just` 值建構子之前被指定，所以一個 `Nothing` 的值總是小於 `Just something` 的值，即使這個 something 為非常小的值。但如果我們比較兩個 `Just` 值，這時它就比較在它們之中的值。

<pre name="code" class="haskell:ghci">
ghci> Nothing < Just 100
True
ghci> Nothing > Just (-49999)
False
ghci> Just 3 `compare` Just 2
GT
ghci> Just 100 > Just 50
True
</pre>

但我們無法做像是 `Just (*3) > Just (*2)` 這樣的事，因為 `(*3)` 與 `(*2)` 為 function，它們都不是 `Ord` 的實體。

我們可以輕易地使用代數資料型別來建立列舉，而 `Enum` 與 `Bounded` typeclass 則幫助我們做到。考慮下列資料型別：

<pre name="code" class="haskell:hs">
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
</pre>

因為所有的值建構子都是 nullary 的（不接收參數──即欄位），所以我們可以讓它為 `Enum` typeclass 的一員。`Enum` typeclass 代表擁有前置子與後繼子的型別。我們也可以令它為 `Bounded` typeclass 的一員──其代表擁有一個最小可能值與最大可能值的型別。既然如此，讓我們同時令它為一個所有其它可衍生的 typeclass 實體，並看看我們能夠用它來做什麼。

<pre name="code" class="haskell:hs">
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
</pre>

因為它是 `Show` 與 `Read` typeclass 的一員，所以我們可以將這個型別的值轉換成字串，以及從字串轉換成這個型別的值。

<pre name="code" class="haskell:ghci">
ghci> Wednesday
Wednesday
ghci> show Wednesday
"Wednesday"
ghci> read "Saturday" :: Day
Saturday
</pre>

因為它是 `Eq` 與 `Ord` typeclass 的一員，所以我們可以比較 day 的大小或是相等性。

<pre name="code" class="haskell:ghci">
ghci> Saturday == Sunday
False
ghci> Saturday == Saturday
True
ghci> Saturday > Friday
True
ghci> Monday `compare` Wednesday
LT
</pre>

它也是 `Bounded` 的一員，所以我們可以取得最小與最大的 day。

<pre name="code" class="haskell:ghci">
ghci> minBound :: Day
Monday
ghci> maxBound :: Day
Sunday
</pre>

它也是 `Enum` 的實體。我們可以取得 day 的前置子與後繼子，且我們可以用它建立 list range！

<pre name="code" class="haskell:ghci">
ghci> succ Monday
Tuesday
ghci> pred Saturday
Friday
ghci> [Thursday .. Sunday]
[Thursday,Friday,Saturday,Sunday]
ghci> [minBound .. maxBound] :: [Day]
[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]
</pre>

這個超級棒的。

## <a name="type-synonyms">型別同義詞</a>

先前，我們在撰寫型別時提過 `[Char]` 與 `String` 型別是相等且可以互換的。這是以*型別同義詞（type synonyms）*實作的。型別同義詞本身不會真的做什麼事，它們只是給予某些型別不同的名字，以讓它對於閱讀我們程式與文件的某人來說更加有意義。以下是標準函式庫如何將 `String` 定義為 `[Char]` 同義詞的方式。

<pre name="code" class="haskell:hs">
type String = [Char]
</pre>

<img src="img/chicken.png" alt="chicken" style="float:left" />
我們引入了 <i>type</i> 關鍵字。這個關鍵字可能會誤導某些人，因為我們實際上不會建立任何新的型別（我們以 <i>data</i> 關鍵字來做到），我們僅為一個已經存在的型別建立一個同義詞。

如果我們建立一個將字串轉成大寫的 function，並稱它為 `toUpperString` 或什麼的，我們可以給它一個 `toUpperString :: [Char] -> [Char]` 或是 `toUpperString :: String -> String` 型別宣告。這兩者本質上是相同的，只是後者更好讀。

當我們處理 `Data.Map` 模組時，我們在將它轉成一個 map 之前，首先將 phonebook 以一個關連列表表示。如同我們已經發現的，一個關連列表為一個 key-value pair 的 list。讓我們看看我們的 phonebook。

<pre name="code" class="haskell:hs">
phoneBook :: [(String,String)]
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]
</pre>

我們看到 `phoneBook` 的型別為 `[(String,String)]`。這告訴我們它是個從字串映射到字串的關連列表，但僅此而已。讓我們建立一個型別同義詞，以在型別宣告中傳達更多的訊息。

<pre name="code" class="haskell:hs">
type PhoneBook = [(String,String)]
</pre>

現在我們 phonebook 的型別宣告可以是 `phoneBook :: PhoneBook`。讓我們也為 `String` 建立一個型別同義詞。

<pre name="code" class="haskell:hs">
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]
</pre>

Haskell 程式設計師會在他們想要傳達更多關於在他們 function 中的字串應該被用作什麼、以及它們代表什麼的資訊時，給予 `String` 型別同義詞。

所以現在，當我們實作一個接收一個名字與一個數字、並看看這個名字與號碼組合是否在我們的 phonebook 中的 function，我們可以給它一個非常漂亮且具有描述性的型別宣告。

<pre name="code" class="haskell:hs">
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook
</pre>

若是我們決定不要使用型別同義詞，我們的 function 型別將會是 `String -> String -> [(String,String)] -> Bool`。在這種情況中，利用型別同義詞的型別宣告更加容易理解。我們引入型別同義詞以描述在我們 function 中的某個已有型別表示什麼，或是在某值有一個重複多次又有點長的型別（像是 `[(String,String)]`）、但某值表達在我們 function 中的前後文更加具體的時候。

型別同義詞也可以被參數化。如果我們想要一個表達關連列表的型別，但仍然想讓它一般化以令它得以使用任何型別作為 key 與 value，我們可以這麼做：

<pre name="code" class="haskell:hs">
type AssocList k v = [(k,v)]
</pre>

現在，一個在關連列表中以 key 取得 value 的 function 型別可以是 `(Eq k) => k -> AssocList k v -> Maybe v`。`AssocList` 是一個接收兩個型別並產生一個具體型別的型別建構子──舉例來說，像是 `AssocList Int String`。

<p class="hint">
<em>Fonzie 說：</em>Aaay！當我談論具體型別時，我指的像是完全套用型別，如 <code>Map Int String</code>；或是我們在處理其中一個它們的多型 function，如<code>[a]</code> 或是 <code>(Ord a) => Maybe a</code> 之類的。就像是，有時候我跟哥兒們說 <code>Maybe</code> 是個型別，但我們指的不是字面上的意思，因為每個傻瓜都知道 <code>Maybe</code> 是個型別建構子嘛。當我應用一個額外的型別到 <code>Maybe</code> 時，像是 <code>Maybe String</code>，這時我就有個具體型別。你知道的，值的型別只可以是具體型別！總而言之，及時行樂、小心至上（live fast, love hard and don't let anybody else use your comb）！<span class="note">〔譯註：Fonzie 為 1980 年代美國喜劇《Happy Days》中的虛構人物。開頭的 Aaay 與文末結論皆為他在劇中比較有名的台詞。〕</span>
</p>

就像是我們可以部分應用 function 以得到新的 function，我們也可以部分應用型別參數以從中得到新的型別建構子。就像是我們以很少的參數呼叫 function 以得到一個新的 function，我們也能夠以很少的參數指定一個型別建構子並取得一個部分應用的型別建構子。若是我們想要一個表達一個從整數映射到某個型別的 map（來自於 `Data.Map`），我們可以這樣做：

<pre name="code" class="haskell:hs">
type IntMap v = Map Int v
</pre>

或者，我們可以像這樣做：

<pre name="code" class="haskell:hs">
type IntMap = Map Int
</pre>

任何一種方式，`IntMap` 型別建構子都取一個參數，而這就是整數將會映射到的型別。

<p class="hint">
<em>喔耶。</em>如果你要試著實作它，你將可能要進行 <code>Data.Map</code> 的限制引入。當你進行限制引入時，型別建構子也必須以模組名稱開頭。所以你要寫成 <code>type IntMap = Map.Map Int</code>。
</p>

確定你真的理解型別建構子與值建構子之間的分別。僅因為我們建立一個叫做 `IntMap` 或是 `AssocList` 的型別同義詞，不代表我們可以做像是 `AssocList [(1,2),(4,5),(7,9)]` 的事。它所代表的是我們可以使用不同名字參照到它的型別。
我們可以做 `[(1,2),(3,5),(8,9)] :: AssocList Int Int`，這將會令內部的數字型別假定為 `Int`，但我們仍然可以使用這個 list 作為任何在內部擁有一對整數的一般 list。型別同義詞（與一般化型別）只可以被使用在 Haskell 的型別部分。無論是我們在定義新型別（對 <i>data</i> 與 <i>type</i> 宣告都是）的任何時候，或是在我們位在一個 `::` 之後的時候，我們都在 Haskell 的型別部分中。`::` 是在型別宣告或是在型別註釋中。

另一個接收兩個型別做為其參數的酷資料型別是 `Either a b` 型別。它大概是這樣定義的：

<pre name="code" class="haskell:hs">
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
</pre>

它有兩個值建構子。若是 `Left` 被使用，則其內容型別為 `a`；若是 `Right` 被使用，則其內容型別為 `b`。所以我們可以使用這個型別來封裝一個型別的值或是另一個型別的值，然後當我們取得一個型別為 `Either a b` 的值時，我們通常會對 `Left` 與 `Right` 進行模式匹配，我們會根據它是哪一種來做出不同的行為。

<pre name="code" class="haskell:ghci">
ghci> Right 20
Right 20
ghci> Left "w00t"
Left "w00t"
ghci> :t Right 'a'
Right 'a' :: Either a Char
ghci> :t Left True
Left True :: Either Bool b
</pre>

到目前為止，我們看到 `Maybe a` 是最常被用來表達可以是失敗或成功的計算結果。但有時候，`Maybe a` 並不是足夠好的，因為 `Nothing` 沒有真的傳遞許多除了「有什麼失敗了」以外的資訊。這對於只能有一種失敗方式、或是我們對如何與為什麼失敗不感興趣的 function 來說是很好的。`Data.Map` 只有在我們檢索的 key 不在 map 中的時候會檢索失敗，所以我們確實地知道發生了什麼。然而，當我們對某個 function 如何或是為什麼失敗感興趣時，我們通常會使用型別為 `Either a b` 的結果，其中 `a` 為某種可以告訴我們關於可能的失敗資訊的型別、而 `b` 則是成功計算的型別。於是，錯誤使用 `Left` 值建構子，而結果使用 `Right`。

一個例子：一所高中擁有置物櫃以讓學生有地方放它們的 Guns'n'Roses<span class="note">〔譯註：美國 1980-1990 年代的搖滾樂團〕</span>海報。每個置物櫃都有一個密碼組合。當一個學生想要一個新的置物櫃時，他要告訴置物櫃管理員他想要的置物櫃號碼，管理員就會給予他密碼。然而，若是某人已經使用了這個置物櫃，管理員就不能告訴他置物櫃的密碼，而他必須挑選不同的置物櫃。我們要使用一個 `Data.Map` 的 map 來表示置物櫃。它將會從置物櫃號碼映射到一對置物櫃是否被使用與置物櫃的密碼。

<pre name="code" class="haskell:hs">
import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)
</pre>

簡單的東西。我們引入一個新的資料型別以表示一個置物櫃是否被佔用，我們為置物櫃密碼建立一個型別同義詞。我們也為從整數映射到一對置物櫃狀態與密碼的型別建立一個型別同義詞。而現在，我們要建立一個在置物櫃 map 搜尋密碼的 function。我們要使用 `Either String Code` 來表達我們的結果，因為我們的檢索可以有兩種失敗方式──置物櫃被佔用，在這種情況我們無法告知密碼；或是置物櫃號碼可能根本不存在。若是檢索失敗，我們就要使用一個 `String` 來告知發生了什麼。

<pre name="code" class="haskell:hs">
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
</pre>

我們在 map 進行一個普通的檢索。若是我們得到一個 `Nothing`，我們就回傳一個型別為 `Left String` 的值，表明置物櫃根本不存在。若是我們找到它，我們就做一個額外的檢查以看看置物櫃是否被佔用。如果是，就回傳一個 `Left` 表明它已經被佔用。若否，就傳回一個型別為 `Right Code` 的值，其中我們給予這個學生置物櫃的正確密碼。它實際上是一個 `Right String`，但我們引入這個型別同義詞以將一些額外的說明引入到型別宣告中。以下是個範例 map：

<pre name="code" class="haskell:hs">
lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]
</pre>

現在讓我們試著檢索一些置物櫃密碼。

<pre name="code" class="haskell:ghci">
ghci> lockerLookup 101 lockers
Right "JAH3I"
ghci> lockerLookup 100 lockers
Left "Locker 100 is already taken!"
ghci> lockerLookup 102 lockers
Left "Locker number 102 doesn't exist!"
ghci> lockerLookup 110 lockers
Left "Locker 110 is already taken!"
ghci> lockerLookup 105 lockers
Right "QOTSA"
</pre>

我們可以使用一個 `Maybe a` 來表達結果，但這時我們將不會知道我們為什麼無法取得密碼。但現在，我們在我們的結果型別中有了關於失敗的資訊。

## <a name="recursive-data-structures">遞迴資料結構</a>

<img src="img/thefonz.png" alt="the fonz" style="float:right" />
如同我們見過的，一個在代數資料型別中的建構子可以有多個（或是根本沒有）欄位，且每個欄位都必須為某個具體型別。考慮到這一點，我們可以建立其建構子擁有相同型別欄位的型別！藉此，我們可以建立遞迴資料結構，其中某個型別的值包含這個型別的值，接著這個值也包含更多相同型別的值等等。

想想這個 list：`[5]`。這僅是 `5:[]` 的語法糖衣。在 `:` 左邊為一個值，而在右邊為一個 list。在這個例子中，它是個空 list。現在 `[4,5]` 這個 list 如何呢？嗯，它被去糖（desugar）為 `4:(5:[])`。看看第一個 `:`，我們看到在它的左邊也有一個元素、在它的右邊也有一個 list（`5:[]`）。同樣適用於像是 `3:(4:(5:6:[]))` 這樣的 list，它可以被寫成像是這樣、像是 `3:4:5:6:[]`（因為 `:` 是右結合）或是 `[3,4,5,6]`。

我們可以說，一個 list 可以為一個空 list，或者它可以為一個元素以 `:` 結合另一個 list（它可以是或不是空 list）。

接著讓我們使用代數資料型別來實作我們自己的 list！

<pre name="code" class="haskell:hs">
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
</pre>

這讀起來就像是來自於前面段落之一的我們的 list 定義。它是一個空 list 或是一個帶著某值的 head 與一個 list 的組合。若是你對此感到困惑，你也許會發現它以 record 語法比較容易理解。

<pre name="code" class="haskell:hs">
data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
</pre>

你也許也會對這裡的 `Cons` 建構子感到困惑。cons 為 `:` 的另一個說法。你看到在 list 中的 `:` 實際上是一個接收一個值與另一個 list、並回傳一個 list 的建構子。我們已經可以使用我們的新 list 型別了！換句話說，它有兩個欄位。一個欄位的型別為 `a`，而另一個型別為 `[a]`。

<pre name="code" class="haskell:ghci">
ghci> Empty
Empty
ghci> 5 `Cons` Empty
Cons 5 Empty
ghci> 4 `Cons` (5 `Cons` Empty)
Cons 4 (Cons 5 Empty)
ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))
Cons 3 (Cons 4 (Cons 5 Empty))
</pre>

我們以中綴的方式呼叫我們的 `Cons` 建構子，所以你可以看到它就像是 `:`。`Empty` 就像是 `[]`，而 ``4 `Cons` (5 `Cons` Empty)`` 就像是 `4:(5:[])`.

我們可以讓我們的 function 僅以特殊字元組成，將它定義成自動中綴呼叫。由於建構子只是回傳一個資料型別的 function，所以我們也可以對它做一樣的事。所以看看這個：

<pre name="code" class="haskell:hs">
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
</pre>

首先，我們注意到一個新的語法結構──fixity 宣告。當我們以運算子定義 function 時，我們可以使用這個以給予它一個 fixity（但我們不必如此）。一個 fixity 描述了運算子綁定有多緊密、以及它是否為左結合或右結合。舉例來說，`*` 的 fixity 為 `infixl 7 *`，而 `+` 的 fixity 為 `infixl 6`。這意味著它們都是左結合（`4 * 3 * 2` 為 `(4 * 3) * 2`），但 `*` 綁定緊密於 `+`，因為它有比較高的 fixity，所以 `5 * 4 + 3` 為 `(5 * 4) + 3`。

於是，我們就可以寫成 `a :-: (List a)` 以取代 `Cons a (List a)`。現在，我們能夠以我們的 list 型別像這樣寫下 list：

<pre name="code" class="haskell:ghci">
ghci> 3 :-: 4 :-: 5 :-: Empty
(:-:) 3 ((:-:) 4 ((:-:) 5 Empty))
ghci> let a = 3 :-: 4 :-: 5 :-: Empty
ghci> 100 :-: a
(:-:) 100 ((:-:) 3 ((:-:) 4 ((:-:) 5 Empty)))
</pre>

在為我們的型別衍生 `Show` 的時候，Haskell 仍然會將建構子當作前綴 function 來顯示它，於是就有個括號包住運算子（記住，`4 + 3` 為 `(+) 4 3`）。

讓我們建立一個將兩個我們的 list 加起來的 function。這即是針對一般 list 的 `++` 是如何被定義的：

<pre name="code" class="haskell:hs">
infixr 5  ++
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
</pre>

所以我們要把它偷來給我們自己的 list 用。我們要把 function 命名為 `.++`。

<pre name="code" class="haskell:hs">
infixr 5  .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)
</pre>

讓我們看看它是否正常運作...

<pre name="code" class="haskell:ghci">
ghci> let a = 3 :-: 4 :-: 5 :-: Empty
ghci> let b = 6 :-: 7 :-: Empty
ghci> a .++ b
(:-:) 3 ((:-:) 4 ((:-:) 5 ((:-:) 6 ((:-:) 7 Empty))))
</pre>

好。很不錯。如果我們想要，我們可以為我們自己的 list 型別實作所有操作在 list 上的操作。

注意到我們如何在 `(x :-: xs)` 上進行模式匹配。這可以正常運作，因為模式匹配實際上是匹配建構子。我們可以在 `:-:` 上做匹配，因為它是個我們自己的 list 型別的建構子；且我們也可以在 `:` 上做匹配，因為它是個內建 list 型別的建構子。同樣適用於 `[]`。因為模式匹配（只能）運作在建構子上，所以我們可以匹配像這樣的東西、一般的前綴建構子或像是 `8` 或 `'a'` 這樣的東西──它們基本上分別是數字與字元型別的建構子。

<img src="img/binarytree.png" alt="binary search tree" style="float:left" />
現在，我們要實作一棵*二元搜尋樹（binary search tree）*。
如果你不熟悉像是 C 這種語言的二元搜尋樹，這就是：一個元素指到兩個元素，一個在它左邊，一個在它右邊。在左邊的元素比較小，在右邊的元素比較大。每個這些元素也可以指到兩個元素（或是一個、或是沒有）。實際上，每個元素擁有至多兩個子樹。關於二元搜尋樹的一個很酷的事情是，我們知道所有在──假定為 5──的左子樹的元素都是比 5 小的。在它右子樹的元素都是比較大的。所以若是我們需要找找 8 是否在我們的樹裡頭，我們就從 5 開始，然後因為 8 大於 5，我們就往右走。我們現在到了 7，且因為 8 比 7 大，所以我們再次往右走。我們以三步找到了我們的元素！現在若是這是個一般的 list（或是一棵樹，但是非常不平衡〈unbalanced〉），它就會耗費我們七步而不是三步來看看 8 是否在這裡。

`Data.Set` 與 `Data.Map` 的 set 與 map 都以樹來實作，只是它們使用的不是一般的二元搜尋樹，而是平衡的（balanced）二元搜尋樹──它總是平衡的。但現在，我們只要實作一般的二元搜尋樹。

這裡我們要假定：一棵樹要不是一棵空的樹，就是一個包含某值與兩棵樹的元素。聽起來與代數資料型別十分契合！

<pre name="code" class="haskell:hs">
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
</pre>

好的，好，非常好。並非手動地建立一棵樹，我們要建立一個取一棵樹與一個元素、並插入這個元素的 function。我們藉由比較我們要插入的值與根節點（root note）的大小來做到，然後若是它比較小，我們就往左走；若是它比較大，我們就向右走。我們對每個子樹節點做一樣的事情，直到我們到達一棵空的樹。一旦我們到達一棵空的樹，我們將以這個值插入一個節點取代這棵空樹。

在像是 C 這種語言中，我們藉由修改指標（pointer）與樹內部的值來做到這件事。在 Haskell 中，我們無法真的修改我們的樹，所以我們必須在每次我們決定往左或往右走的時候建立一棵新的子樹，到最後這個插入 function 會回傳一棵完全嶄新的樹，因為 Haskell 並非真的擁有指標的概念，只有值而已。於是，我們的插入 function 型別會像是 `a -> Tree a - > Tree a` 這樣。它取一個元素與一棵樹，並傳回一棵內部擁有這個元素的新樹。這聽起來似乎很沒效率，但惰性處理了這個問題。

所以，這裡有兩個 function。一個是建立一棵獨體（singleton）樹（只有一個節點的樹）的 function，一個是將一個元素插入到一棵樹中的 function。

<pre name="code" class="haskell:hs">
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)
</pre>

`singleton` 這個 function 只是一個建立一個擁有某值與兩顆空子樹的節點的便捷方法。在插入 function 中，我們首先有個邊界條件作為模式。若是我們到達一棵空子樹，這代表我們到了我們想去的地方，並且我們以帶著這個元素的獨體樹取代這棵空樹。若是我們不是要插入到一棵空樹中，我們就必須檢查別的東西。首先，若是我們要插入的元素等於根元素，就回傳相同的一棵樹。若是它比較小，就回傳一棵擁有相同根元素、相同右子樹，但以一棵擁有我們插入的值到左子樹中的樹來替代其左子樹。若是我們的值大於根元素亦同（但左右相反過來）。

接下來，我們要建立一個檢查某個元素是否在樹中的 function。首先，讓我們定義邊界條件。若是我們在一棵空樹中尋找元素，那它當然不在這裡。好。注意到這與在 list 中搜尋元素時的邊界條件是相同的。若是我們在一個空 list 中尋找一個元素，它就不在這裡。總而言之，若是你不在一棵空樹中尋找一個元素，我們就檢查其它的東西。若是在根節點的元素就是我們要找的，很好！如果不是，那怎麼辦？嗯，我們可以運用瞭解所有左邊的元素都小於根節點的優勢。所以若是我們在找的元素小於根節點，就檢查看看它是否在左子樹。如果它比較大，就檢查看看它是否在右子樹中。

<pre name="code" class="haskell:hs">
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right
</pre>

我們必須去做的是以程式碼中寫下前面的段落。讓我們與我們的樹愉快相處！我們要使用一個摺疊來從一個 list 建立一棵樹，而非手動地建立一棵樹（雖然我們可以）。記住，幾乎每個一個接著一個元素尋訪 list、然後回傳某種值的東西都能夠以摺疊實作！我們要從空的樹開始，然後從右邊逼近一個 list，並一個元素接著一個元素插入到我們的累加樹。

<pre name="code" class="haskell:ghci">
ghci> let nums = [8,6,4,1,7,3,5]
ghci> let numsTree = foldr treeInsert EmptyTree nums
ghci> numsTree
Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))
</pre>

在這個 `foldr` 中，`treeInsert` 為摺疊 function（它取一棵樹與一個 list 元素，並產生一棵新的樹），`EmptyTree` 為起始累加器。當然，`nums` 是我們要摺疊的 list。

當我們將我們的樹印到命令列時，它是非常不好讀的，但若是我們嘗試看看，我們還是可以識別它的結構。我們看到根節點為 5，然後它有兩棵子樹，其中一棵有個 3 的根節點，而另一棵為 7，等等。

<pre name="code" class="haskell:ghci">
ghci> 8 `treeElem` numsTree
True
ghci> 100 `treeElem` numsTree
False
ghci> 1 `treeElem` numsTree
True
ghci> 10 `treeElem` numsTree
False
</pre>

檢查成員關係也運作良好。酷。

所以如你所見，代數資料結構在 Haskell 中是個非常酷且有威力的概念。我們可以使用它來建立從布林值、weekday 列舉到二元搜尋樹與更多的任何東西！

## <a name="typeclasses-102">Typeclasses 102</a>

<img src="img/trafficlight.png" alt="tweet" style="float:right" />
到目前為止，我們學過一些標準的 Haskell typeclass，我們也看過屬於它們的型別了。我們也學過如何藉由要求 Haskell 為我們衍生實體來自動地建立我們自己的標準 typeclass 型別實體。在這一節，我們要學學如何建立我們自己的 typeclass，以及如何手動建立它們的型別實體。

快速回顧一下 typeclass：typeclass 就像是介面。一個 typeclass 定義一些行為（像是比較相等性、比較順序、列舉），然後令能夠以這種方式運作的型別為這個 typeclass 的實體。typeclass 的行為藉由定義 function 或我們接著要實作的型別宣告來達成。所以當我們說一個型別為一個 typeclass 的實體時，我們指的是我們能夠以這個型別使用 typeclass 定義的 function。

typeclass 與像是 Java 或是 Python 這類語言的類別幾乎沒什麼關係。這令許多人感到困惑，所以我要你現在就忘記在命令式語言中，你所知道關於類別的所有東西。

舉例來說，`Eq` typeclass 代表可以被比較相等性的東西。它定義了 `==` 與 `/=` function。如果我們有個型別（假定為 `Car`），並且以相等性 function `==` 比較兩個 car 很合理，那麼 `Car` 作為 `Eq` 的實體就很合理了。

這就是在標準函式庫中定義 `Eq` 的方式：

<pre name="code" class="haskell:hs">
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
</pre>

哇，哇，哇！這裡有一些新的奇怪語法與關鍵字！別擔心，這一切立刻就會明瞭。首先，當你寫下 `class Eq a where` 時，這就代表我們定義了一個新的 typeclass，且它叫做 `Eq`。`a` 為型別變數，它代表 `a` 將會扮演我們不久將會令它作為 `Eq` 實體的型別。它不必被叫做 `a`，它甚至不必為一個字母，但它必須為小寫的字。接著，我們定義了多個 function。它並不強制去實作 function 主體本身，我們僅需為 function 指定型別宣告。

<p class="hint">
有些人可能認為，如果我們寫 <code>class Eq equatable where</code>，並像 <code>(==) :: equatable -> equatable -> Bool</code> 指定型別宣告是比較好的。
</p>

總而言之，我們為 `Eq` 定義的 function 實作了 function 主體，只是我們以交互遞迴（mutual recursion）的方式來定義它們。我們表明，如果兩個 `Eq` 實體沒有不同，它們就是相等的；如果它們不相等，它們就是不同的。其實我們不必去做這件事，但我們做了，且我們不久將會看到這對我們有何幫助。

<p class="hint">
若是我們已經說 <code>class Eq a where</code>，然後在這個 class 中像 <code>(==) :: a -> -a -> Bool</code> 定義一個型別宣告，當我們之後檢驗這個 function 的型別時，我們將會得到的型別為 <code>(Eq a) => a -> a -> Bool</code>。
</p>

所以一旦我們有一個 class，我們可以用它來做什麼？嗯，其實，沒有很多。但一旦我們開始建立這個 class 的型別實體，我們就開始得到一些不錯的功能。所以看看這個型別：

<pre name="code" class="haskell:hs">
data TrafficLight = Red | Yellow | Green
</pre>

它定義了一個紅綠燈的狀態。注意到我們沒有為它衍生任何 class 實體。這是因為我們要手動寫下一些實體，即使我們可以為它衍生像是 `Eq` 與 `Show`。以下是我們如何令它為 `Eq` 的實體：

<pre name="code" class="haskell:hs">
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
</pre>

我們使用 <i>instance</i> 關鍵字來做到這件事。所以 <i>class</i> 是代表定義新的 typelclass，<i>instance</i> 是代表建立我們的 typeclass 型別實體。當我們定義 `Eq` 時，我們寫了 `class Eq a where`，並表示 `a` 扮演稍後將會作為實體的任何型別。這裡我們可以清楚地看到這件事，因為當我們建立一個實體時，我們寫了 `instance Eq TrafficLight where`。我們將 `a` 以實際的型別取代。

因為在 <i>class</i> 宣告中，`==` 以 `/=` 的方式定義、反之亦然，所以我們在 <i>instance</i> 宣告只要覆寫其中一個。這被稱為 typeclass 的最小完整定義──我們必須實作的最低限度的 function，以讓我們的型別能讓 class 所說的那樣運作。為了要為 `Eq` 滿足最小完整定義，我們必須覆寫 `==` 或是 `/=`。若是 `Eq` 只像這樣被定義：

<pre name="code" class="haskell:hs">
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
</pre>

在令一個型別為其實體的時候，我們就必須都實作這些 function，因為 Haskell 並不知道這兩個 function 是相關的。這時最小完整定義就會是： `==` 與 `/=`。

你可以看到我們簡單地藉由模式匹配來實作 `==`。由於有很多兩個燈號不相等的情況，所以我們指定了它們相等的情況，然後指定一個捕獲所有情況的模式，表明如果它沒有前面的組合，則兩個燈號就是不相等的。

讓我們也手動令它為 `Show` 的實體。要為 `Show` 滿足最小完整定義，我們必須實作它的 `show` function，它接收一個值並將它轉成字串。

<pre name="code" class="haskell:hs">
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
</pre>

再一次，我們使用模式匹配來達成我們的目標。讓我們看看它如何實際運作：

<pre name="code" class="haskell:ghci">
ghci> Red == Red
True
ghci> Red == Yellow
False
ghci> Red `elem` [Red, Yellow, Green]
True
ghci> [Red, Yellow, Green]
[Red light,Yellow light,Green light]
</pre>

不錯。我們可以直接衍生 `Eq`，且它會擁有相同的效果（但我們為了教育目的而不這麼做）。然而，衍生 `Show` 將會直接將值建構子轉成字串。但若是我們想要讓燈號像 `"Red light"` 這樣顯示，那我們就必須手動建立 instance 宣告。

你也可以讓 typeclass 為其它 typeclass 的子類別。`Num` 的 <i>class</i> 宣告有一點長，但這是它最前面的部份：

<pre name="code" class="haskell:hs">
class (Eq a) => Num a where
   ...
</pre>

如同我們先前提到的，有很多我們可以填進類別限制的地方。所以這就像是寫下 `class Num a where`，只是我們表示我們的型別 `a` 必須為 `Eq` 的實體。我們本質上是表明，我們必須在我們令一個型別為 `Num` 實體之前，令它為 `Eq` 的實體。在某型別可以被視為一個數字之前，我們可以判斷這個型別的值是否可以比較相等性是合理的。子類別化其實就這麼簡單，它僅是一個在 <i>class</i> 宣告上的一個類別限制！當我們在 <i>class</i> 宣告、或是在 <i>instance</i> 宣告中定義 function 主體的時候，我們可以假定 `a` 為 `Eq` 的一員，所以我們可以將 `==` 使用在這個型別的值上。

但是要如何令 `Maybe` 或是 list 型別為 typeclass 的實體呢？`Maybe` 與 `TrafficLight` 的不同在於，`Maybe` 本身不是一個具體型別──它是一個接收一個型別參數（像是 `Char` 之類的）以產生一個具體型別（像是 `Maybe Char`）的型別建構子。讓我們再看一次 `Eq` typeclass：

<pre name="code" class="haskell:hs">
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
</pre>

從型別宣告，我們看到 `a` 被用作一個具體型別，因為所有在 function 中的型別都必須是具體的（記住，你無法擁有一個型別為 `a -> Maybe` 的 function，但你可以有一個 `a -> Maybe a` 或是 `Maybe Int -> Maybe String` 的 function）。這就是為什麼我們不能像這樣做：

<pre name="code" class="haskell:hs">
instance Eq Maybe where
    ...
</pre>

因為就像是我們所看到的，`a` 必須為一個具體型別，但 `Maybe` 不是個具體型別。它是一個接收一個參數、然後產生一個具體型別的型別建構子。為每個型別寫 `instance Eq (Maybe Int) where`、`instance Eq (Maybe Char) where`、等等也令人厭煩。所以我們可以像這樣寫：

<pre name="code" class="haskell:hs">
instance Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
</pre>

這就像是說，我們想要讓所有 `Maybe something` 形式的型別為 `Eq` 的實體。我們實際上可以寫成 `(Maybe something)`，但我們通常選擇單一字母以符合 Haskell 風格。這裡的 `(Maybe m)` 扮演著 `class Eq a where` 的 `a` 的角色。`Maybe` 不是一個具體型別，而 `Maybe m` 是。藉由指定一個型別參數（`m`，它是小寫的），我們表明我們想要令所有 `Maybe m`──其中 `m` 為任意型別──形式的型別為 `Eq` 的實體。

雖然這有一個問題。你能找出它嗎？我們將 `==` 使用在 `Maybe` 的內容上，但我們必須保證 `Maybe` 包含的值可以被 `Eq` 使用！這就是為什麼我們必須像這樣修改我們的 instance 宣告：

<pre name="code" class="haskell:hs">
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
</pre>

我們必須加入一個型別限制！以這個 <i>instance</i> 宣告，我們表明：我們想要讓所有 `Maybe m` 形式的型別為 `Eq` typeclass 的一員，但只有 `m`（也就是包含在 `Maybe` 內部的型別）也為 `Eq` 一員的那些型別。這實際上也是 Haskell 衍生型別的方式。

許多時候，在 <i>class</i> 宣告中的型別限制會被用來讓一個 typeclass 為另一個 typeclass 的子類別，而在 <i>instance</i> 宣告中會被用以表達某型別內容的要求。舉例來說，這裡我們需要 `Maybe` 的內容也為 `Eq` typeclass 的一員。

在建立一個實體時，若是你看到一個型別在型別宣告中被用作一個具體型別（像是 `a -> a -> Bool` 中的 `a`），你就必須提供型別參數並加上括號，以讓你最終得到一個具體型別。

<p class="hint">
考慮到你嘗試令它為實體的型別將會取代 <i>class</i> 宣告中的參數。<code>class Eq a where</code> 的 <code>a</code> 在你建立一個實體時將會被取代成一個真實型別，所以也試著假想將你的型別擺進 function 的型別宣告中。<code>(==) :: Maybe -> Maybe -> Bool</code> 並不合理，但 <code>(==) :: (Eq m) => Maybe m -> Maybe m -> Bool</code> 是合理的。不過這只是一種思考方式，因為無論我們建立的型別為何，<code>==</code> 的型別總是為 <code>(==) :: (Eq a) => a -> a -> Bool</code>。
</p>

喔，還有一件事，聽我說！如果你想要看看一個 typeclass 有哪些實體，只要在 GHCI 執行 `:info YourTypeClass`。所以輸入 `:info Num` 會顯示 typeclass 定義的 function，且它會給你一個屬於這個 typeclass 的型別列表。`:info` 也作用在型別與型別限制上。如果你執行 `:info Maybe`，它將會將所有 `Maybe` 為實體的 typeclass 顯示給你。`:info` 也可以將一個 function 的型別宣告顯示給你。我想這非常酷。

## <a name="a-yes-no-typeclass">一個 yes-no typeclass</a>

<img src="img/yesno.png" alt="yesno" style="float:left" />
在 JavaScript 與一些其它的弱型別（weakly typed）語言中，你幾乎可以把任何東西擺進一個 if expression 中。舉例來說，你可以做下述所有的操作： `if (0) alert("YEAH!") else alert("NO!")` 、 `if ("") alert ("YEAH!") else alert("NO!")` 、 `if (false) alert("YEAH") else alert("NO!)` 、等等，而它們全都會丟出一個 `NO!` 的 alert。若是你執行 `if ("WHAT") alert ("YEAH") else alert("NO!")`，它將會 alert 一個 `"YEAH!"`，因為 JavaScript 將非空字串視為一種真值。

雖然僅使用 `Bool` 代表布林語義（semantic）在 Haskell 中運作地比較好，但無論如何，讓我們試著實作這個 JavaScript 式的行為。為了好玩！讓我們以一個 <i>class</i> 宣告開始。

<pre name="code" class="haskell:hs">
class YesNo a where
    yesno :: a -> Bool
</pre>

十分簡單。`YesNo` typeclass 定義了一個 function。這個 function 接收一個可以被視為擁有某種真值概念、並肯定地告訴我們它是否為真的型別的值。注意到我們將 `a` 使用在 function 的方式，`a` 必須為一個具體型別。

接下來，讓我們定義一些實體。對於數字，我們要（如同在 JavaScript 中）假設任何非 0 數字為真，而 0 為假。

<pre name="code" class="haskell:hs">
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
</pre>

空 list（以及字串）為假，而非空 list 為一個真值。

<pre name="code" class="haskell:hs">
instance YesNo [a] where
    yesno [] = False
    yesno _ = True
</pre>

注意到我們僅將一個型別參數 `a` 擺在這裡，以讓 list 為一個具體型別，即使我們不會做任何關於包含在 list 中的型別的假設。還有什麼，唔....我知道了，`Bool` 本身也持有真值與假值，而哪個是哪個是十分顯而易見的。

<pre name="code" class="haskell:hs">
instance YesNo Bool where
    yesno = id
</pre>

阿？`id` 是什麼？這只是一個接收一個參數並回傳同一個值的標準函式庫 function，總之這就是我們要寫在這裡的東西。

讓我們也令 `Maybe a` 作為實體。

<pre name="code" class="haskell:hs">
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False
</pre>

我們不需要類別限制，因為我們不做任何關於 `Maybe` 內容的假設。我們只表明若是它為 `Just` 值它就為真，若是它是一個 `Nothing` 就為假。我們仍然必須寫成 `(Maybe a)` 而不僅是 `Maybe`，因為若是你仔細想想，一個 `Maybe -> Bool` function 無法存在（因為 `Maybe` 不是個具體型別），而 `Maybe a -> Bool` 則很正常。這依然非常酷，因為現在不管 `something` 是什麼，任何 `Maybe something` 形式的型別都是 `YesNo` 的一員。

先前，我們定義了一個 `Tree a` 型別，它表示一棵二元搜尋樹。我們可以假定空樹為假，而任何不是一棵空樹的值都為真。

<pre name="code" class="haskell:hs">
instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True
</pre>

一個紅綠燈可以為 yes 或 no 值嗎？當然。如果它是紅燈，你就停。如果它是綠燈，你就走。如果它是黃燈呢？呃，我通常會闖黃燈，因為我為腎上腺素而活。

<pre name="code" class="haskell:hs">
instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True
</pre>

酷，現在我們有一些實體了，讓我們試試！

<pre name="code" class="haskell:ghci">
ghci> yesno $ length []
False
ghci> yesno "haha"
True
ghci> yesno ""
False
ghci> yesno $ Just 0
True
ghci> yesno True
True
ghci> yesno EmptyTree
False
ghci> yesno []
False
ghci> yesno [0,0,0]
True
ghci> :t yesno
yesno :: (YesNo a) => a -> Bool
</pre>

是的，它正常運作。讓我們建立一個模仿 if 敘述的 function，但它與 `YesNo` 值一同運作。

<pre name="code" class="haskell:hs">
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult
</pre>

十分直觀。它取一個 yes-no 值與兩個值。如果 yes-no 值為 yes，它就回傳兩個值的第一個值，否則它就回傳其中的第二個值。

<pre name="code" class="haskell:ghci">
ghci> yesnoIf [] "YEAH!" "NO!"
"NO!"
ghci> yesnoIf [2,3,4] "YEAH!" "NO!"
"YEAH!"
ghci> yesnoIf True "YEAH!" "NO!"
"YEAH!"
ghci> yesnoIf (Just 500) "YEAH!" "NO!"
"YEAH!"
ghci> yesnoIf Nothing "YEAH!" "NO!"
"NO!"
</pre>

## <a name="the-functor-typeclass">Functor typeclass</a>

## <a name="kinds-and-some-type-foo">Kind 與一些 type-foo</a>
