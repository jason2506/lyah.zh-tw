---
layout: page
title: Function 中的語法
prev:
    url: types-and-typeclasses
    title: 型別與 Typeclass
---

## <a name="pattern-matching">模式匹配</a>

<img src="img/pattern.png" alt="four!" style="float:right" />
這個章節將會涵蓋一些 Haskell 酷炫的語法結構，我們將從模式匹配（pattern matching）開始。模式匹配由指定一些資料應符合的模式，然後檢查資料是否符合，並根據模式解構資料所構成。

定義 function 時，你可以為不同的模式定義個別的 function 主體（body）。這使得程式碼十分乾淨、簡單且可讀。你可以對任何資料型別──數字、字元、list、tuple 等等──進行模式匹配。讓我們建立一個非常沒用的 function，它會檢查我們提供給它的數字是否為七。

<pre name="code" class="haskell: hs">
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"
</pre>

當你呼叫 `lucky` 的時候，模式將會由上而下被檢查，一旦符合某個模式，對應的 function 主體就會被使用。只有在某個數字為 7 的情況符合第一個模式。假如它不是 7，它就屬於第二個模式──其匹配任何值，並將它綁定到 `x`。這個 function 也能夠使用 if 敘述來實作。不過若是我們想要一個表明從 1 到 5 的數字，對於其他的任何數字則表明 `"Not between 1 and 5"` 的 function 呢？少了模式匹配，我們就必須建立一個令人費解的 if then else 樹。然而，藉由這樣：

<pre name="code" class="haskell: hs">
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"
</pre>

注意到假使我們將最後一個模式（捕獲所有情況的那個）移到最上頭，它將總是說 `"Not between 1 and 5"`，因為它將會捕獲所有數字，且它不會有機會往下檢查任何其他的模式。

還記得我們先前實作的階乘 function 嗎？我們將一個數字 `n` 的階乘定義為 `product [1..n]`。我們也可以遞迴地（recursively）定義一個階乘 function，這通常是在數學中被定義的方法。我們從表明 0 的階乘為 1 開始。然後我們將任何正整數的階乘陳述為這個整數乘上其前置子的階乘。如下是翻譯成 Haskell 看起來的樣子：

<pre name="code" class="haskell: hs">
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
</pre>

這是我們第一次遞迴地定義一個 function。遞迴（recursion）在 Haskell 中很重要，而我們將會在之後更深入地探討它。不過概括而言，如果我們嘗試取得 3 的階乘，它會嘗試計算 `3 * factorial 2`。2 的階乘為 `2 * factorial 1`，所以我們現在得到 `3 * (2 * factorial 1)`。1 的階乘為 `1 * factorial 0`，所以我們現在得到 `3 * (2 * (1 * factorial 0))`。這裡有個小把戲──我們已經定義了 0 的階乘為 1，且因為它遇到捕獲所有情況的那個模式之前的模式，所以它僅會傳回 1。所以最後的結果等於 `3 * (2 * (1 * 1))`。若是我們將第二個模式寫在第一個模式上面，它將會捕獲所有的數字──包含 0，而我們的計算將永遠不會終止。這就是為什麼指定模式時的順序很重要。先指定最特定的模式，之後才指定比較一般化的模式是最好的。

模式匹配也會失敗。假如我們定義了一個像這樣的 function：

<pre name="code" class="haskell: hs">
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
</pre>

然後試著用我們不預期的輸入來呼叫它，會出現這種情況：

<pre name="code" class="haskell: ghci">
ghci> charName 'a'
"Albert"
ghci> charName 'b'
"Broseph"
ghci> charName 'h'
"*** Exception: tut.hs:(53,0)-(55,21): Non-exhaustive patterns in function charName
</pre>

它抱怨道：我們有個不詳盡的模式，這是理所當然的。建立模式時，我們總是需要包含一個捕獲所有情況的模式，所以我們的程式不會在取得某些未預期的輸入時崩潰。

模式匹配也可以被用在 tuple 上。假如我們想要建立一個接收兩個平面空間中的向量（它是 pair 的形式），並將它們加總起來的 function 呢？為了要加總兩個向量，我們要獨立加總它們的 x 分量（component），然後獨立加總它們的 y 分量。如果我們還不知道模式匹配的話，我們將會這樣建立它：

<pre name="code" class="haskell: hs">
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)
</pre>

嗯，這能行，不過有個更好的方法來達成。讓我們使用模式匹配修改這個 function。

<pre name="code" class="haskell: hs">
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
</pre>

好了！好多了。注意到它已經是個捕獲所有情況的模式了。（在兩種情況中的）`addVectors` 的型別都是 `addVectors :: (Num a) => (a, a) -> (a, a) - > (a, a)`，所以我們保證能取得兩個 pair 作為參數。

`fst` 與 `snd` 擷取 pair 中的元素。不過 triple 怎麼辦呢？嗯，這裡沒有現成的 function 能做到，不過我們可以建立我們自己的。

<pre name="code" class="haskell: hs">
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z
</pre>

這裡的 `_` 與它在 list comprehension 中的意義相同。它代表我們完全不關心這個部分是什麼，所以我們只寫作一個 `_`。

這提醒了我，你也可以在 list comprehension 中使用模式匹配。看看它吧：

<pre name="code" class="haskell: ghci">
ghci> let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
ghci> [a+b | (a,b) <- xs]
[4,7,6,8,11,4]
</pre>

萬一模式匹配失敗了，它就只會移到下一個元素。

list 本身也可以被用在模式匹配中。你可以匹配空 list `[]` 或是任何包括 `:` 與空 list 的模式。但由於 `[1,2,3]` 只是個 `1:2:3:[]` 的語法糖衣，所以你也可以使用前者的形式。一個像是 `x:xs` 這樣的模式會將 list 的 head 綁定為 `x`，將剩下的綁定為 `xs`──即使 list 只有一個元素，而使得 `xs` 最終為一個空的 list。

<p class="hint">
<em>註記：</em><code>x:xs</code> 模式經常被使用，尤其是遞迴 function。不過擁有 <code>:</code> 的模式只能匹配長度大於 1 的 list。
</p>

假如你想要將 list 的前三個元素綁定到變數，剩下的綁定到另一個變數，你可以使用像是 `x:y:z:zs` 的形式。它將只能匹配擁有三個以上元素的 list。

現在我們知道如何匹配 list，讓我們建立我們自己的 `head` function 實作。

<pre name="code" class="haskell: ghci">
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x
</pre>

看看它是否能運作：

<pre name="code" class="haskell: ghci">
ghci> head' [4,5,6]
4
ghci> head' "Hello"
'H'
</pre>

很好！注意到如果你想要綁定多個變數（即使其中之一只是個 `_`，而沒有真正地被綁定），我們必須將它們包在括號中。同時注意到我們使用的 `error` function。它接收一個字串，產生一個執行期錯誤（runtime error），並使用此字串作為發生的錯誤類型資訊。這會導致程式崩潰，所以使用得太多是不太好的。不過對一個空 list 呼叫 `head` 沒有任何意義。

讓我們建立一個沒什麼用的 function，它以（不）便捷的英語形式告訴我們 list 中的前幾個元素。

<pre name="code" class="haskell: hs">
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
</pre>

這個 function 是安全的，因為它考慮了空 list、單一元素的 list、兩個元素的 list 以及多於兩個元素的 list。注意到 `(x:[])` 與 `(x:y:[])` 可以被改寫成 `[x]` 與 `[x,y]` （因為它是個語法糖衣，所以我們不需要括號）。我們無法將 `(x:y:_)` 以方括號改寫，因為它匹配任何長度大於 2 的 list。

我們已經利用 list comprehension 實作了我們自己的 `length` function。現在我們將使用模式匹配與一點點遞迴來實作它：

<pre name="code" class="haskell: hs">
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs
</pre>

這很像是我們先前撰寫的階乘 function。首先我們定義了一個已知輸入的結果──空 list。這也被稱為邊界條件（edge condition）。然後在第二個模式我們接收一個被切成一個 head 與一個 tail 的 list。我們表明長度等於 1 加上 tail 的長度。我們使用 `_` 來匹配 head，因為我們完全不關心它是什麼。同時注意到我們考慮了所有 list 的可能模式。第一個模式匹配一個空的 list，第二個模式則匹配任何非空的 list。

讓我們看看如果我們對 `"ham"` 呼叫 `length'` 會發生什麼。首先，它會檢查它是否是個空的 list。因為它不是，所以它就落到了第二個模式。它會匹配於第二個模式，而這表示它的長度為 `1 + length' "am"`，因為我們把它切割成一個 head 與一個 tail，並丟棄這個 head。好。同樣的，`"am"` 的 `length'` 為 `1 + length' "m"`。所以現在我們得到 `1 + (1 + length' "m")`。`length' "m"` 為 `1 + length' ""`（也可以被寫作 `1 + length' []`）。因為我們將 `length' []` 定義為 `0`，所以最後我們得到 `1 + (1 + (1 + 0))`。

讓我們來實作 `sum`。我們知道一個空的 list 的總和為 0。我們將這寫下來作為一個模式。我們也知道一個 list 的總和為它的 head 加上 list 其餘值的總和。所以如果我們將這寫下來，我們會得到：

<pre name="code" class="haskell: hs">
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs
</pre>

還有個被稱為 <i>as 模式</i>的東西。這是一種根據一個模式切割某個值並綁定到名稱上，而仍然保持整個值的參考的方法。藉由將一個名稱與一個 `@` 放置在一個模式前面，你就能做到了。舉例來說，`xs@(x:y:ys)` 這個模式。這個模式將會精確匹配與 `x:y:ys` 相同的值，不過你可以藉由 `xs` 輕易地取得整個 list，而不是在 function 主體中重複地打入 `x:y:ys`。這裡是個應急的範例：

<pre name="code" class="haskell: hs">
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
</pre>

<pre name="code" class="haskell: ghci">
ghci> capital "Dracula"
"The first letter of Dracula is D"
</pre>

當我們必須在 function 主體中一再使用整個值的時候，我們通常使用 as 模式以避免在匹配一個較大的模式時進行重複撰寫。

還有一件事──你不能在模式匹配中使用 `++`。假如你嘗試要匹配 `(xs ++ ys)`，第一個 list 應該要是什麼，而第二個 list 應該是什麼？它並沒有太多意義。匹配 `(xs ++ [x,y,z])` 或僅是 `(xs ++ [x])` 可能有意義，不過因為 list 的本質，你不能這樣做。

## <a name="guards-guards">Guards，guards！</a>

<img src="img/guards.png" alt="guards" style="float:left" />
模式是一種確定某個值符合某種形式，並將其解構的方式；而 guard 則是測試一個值（或很多值）的某些屬性是否為真的方式。這聽起來非常像一個 if 敘述，它們也十分相似。事實是當你有許多條件時，guard 更加容易閱讀，而且與模式搭配良好。

讓我們直接深入並建立一個使用 guard 的 function，而不是解釋它的語法。我們打算建立一個根據你的 [BMI](http://en.wikipedia.org/wiki/Body_mass_index)（body mass index）來斥責你的簡單 function。你的 BMI 等於你的體重除以你身高的平方。假如你的 BMI 小於 18.5，你就被視為過輕。假如它介於 18.5 到 25 之間，則你被視為適中。25 到 30 為過重，而超過 30 就是肥胖。所以這就是這個 function（我們現在還不想計算 BMI 值，這個 function 僅取得這個值並告訴你結果）：

<pre name="code" class="haskell: hs">
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
</pre>

guard 以緊跟著 function 名稱與其參數之後的 `|` 表示。它們通常都向右縮排一格並排成一列。一個 guard 基本上是一個布林 expression。假如它被求值為 `True`，則對應的 function 主體就會被使用。假如它被求值為 `False`，則繼續檢查下一個 guard，以此類推。假如你以 `24.3` 呼叫這個 function，它首先會檢查它是否小於或等於 `18.5`。並非如此，所以它落到下一個 guard。檢查進行到第二個 guard，並且因為 24.3 小於 25.0，所以傳回了第二個字串。

這不禁讓人聯想到命令式語言中的一棵大的 if else 樹，只不過這樣更好且更易讀。而大的 if else 樹通常令人皺眉，有時候一個問題被定義在這樣一個鬆散的方法中，而你無法避開它。guard 則是針對此的一個非常好的替代方案。

最後一個 guard 往往是 `otherwise`。`otherwise` 被簡單地定義成 `otherwise = True` 並捕捉任何情況。這與模式十分相似，只是後者檢查的是輸入是否滿足某個模式，而 guard 檢查的則是布林條件。假如一個 function 的所有 guard 都被求值為 `False`（而我們沒有提供一個捕捉所有情況的 `otherwise` guard），則會落到下一個*模式*求值。這即是模式與 guard 搭配良好的方式。如果沒有找到合適的 guard 或是模式，則會拋出一個錯誤。

我們當然可以在接收的參數數量如同我們所需的 function 使用 guard。讓我們修改這個 function，讓它接收一身高與體重，並為我們計算 BMI 值，而不是讓使用者在呼叫 function 前自行計算它。

<pre name="code" class="haskell: hs">
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise                 = "You're a whale, congratulations!"
</pre>

讓我們看看我是不是很胖....

<pre name="code" class="haskell: ghci">
ghci> bmiTell 85 1.90
"You're supposedly normal. Pffft, I bet you're ugly!"
</pre>

呀！我並不胖！不過 Haskell 說我很醜。無所謂啦！

注意到在 function 名稱與其參數之後、在第一個 guard 之前並沒有 `=`。許多新手有時候會因為多加了這個而得到語法錯誤。

另一個十分簡單的例子：讓我們實作我們自己的 `max` function。假如你還記得，其接收可以被比較的兩個值，並傳回比較大的那個。

<pre name="code" class="haskell: hs">
max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b
</pre>

guard 也可以被寫成一行，雖然我並不建議這樣，因為即使是非常短的 function，它也不太易讀。不過為了證明，我們可以像這樣撰寫 `max'`：

<pre name="code" class="haskell: hs">
max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b
</pre>

噁！一點也不好讀！繼續前進：讓我們使用 guard 實作我們自己的 `compare`。

<pre name="code" class="haskell: hs">
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT
</pre>

<pre name="code" class="haskell: ghci">
ghci> 3 `myCompare` 2
GT
</pre>

<p class="hint">
<em>註記：</em>我們不只可以用反引號以中綴形式呼叫 function，我們也可以用反引號定義它。有時候這種方式更容易閱讀。
</p>

## <a name="where">Where!?</a>

在前一節中，我們定義了一個像這樣的 BMI 計算器 function：

<pre name="code" class="haskell: hs">
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise                   = "You're a whale, congratulations!"
</pre>

注意到我們這裡重複了三次。我們重複了三次。在程式撰寫時重複（三次）有如頭部中了一腳。既然我們重複了三次相同的 expression，如果我們能夠將它一次算好、綁定到某個名稱，然後使用這個名稱而不是 expression 是比較理想的。嗯，我們可以像這樣修改我們的 function：

<pre name="code" class="haskell: hs">
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
</pre>

我們在 guard 後面放置關鍵字 `where`（通常將它的縮排與 `|` 一致是最好的），然後我們定義多個名稱或是 function。這些名稱對所有的 guard 都是可見的，並給了我們不必重複撰寫的優點。假使我們決定以些許不同的方式計算 BMI 值，我們只需要修改它一次。它也藉由命名提昇了可讀性，並能夠因為像是我們的 `bmi` 變數在這裡只被計算一次，使我們的程式更加快速。我們可以更進一步，像這樣呈現我們的 function：

<pre name="code" class="haskell: hs">
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0
</pre>

我們定義在 function 中 where 部分的名稱只在這個 function 可見，所以我們不必擔心它污染其他 function 的名稱空間（namespace）。注意到所有的名稱都對齊同一欄。假如我們沒有好好對齊它們，Haskell 會被搞混，因為它無法知道它們全都是同個區塊（block）的一部分。

<i>where</i> 綁定並不會共享於不同模式的 function 主體。如果你想要一個 function 的多個模式存取某些共享的名稱，你必須將它定義為全域。

你也可以在 where 綁定使用*模式匹配*！我們可以改寫我們先前 function 的 where 部分為：

<pre name="code" class="haskell: hs">
...
where bmi = weight / height ^ 2
      (skinny, normal, fat) = (18.5, 25.0, 30.0)
</pre>

讓我們建立另一個相當沒用的 function，它接收一個姓氏與一個名字，並傳回給某人他的縮寫。

<pre name="code" class="haskell: hs">
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname
</pre>

我們可以直接在 function 的參數裡達成這個模式匹配（它實際上會更短也更清楚），不過這只是想證明在 where 綁定做到這件事也是可能的。

如同我們在 where 區塊裡定義常數，你也可以定義 function。繼續健康的程式題材，讓我們建立一個接收一串體重－身高 pair 的 list，並傳回一串 BMI list 的 function。

<pre name="code" class="haskell: hs">
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2
</pre>

就只有這樣而已！在這個範例中，我們必須將 `bmi` 引入為一個 function 的原因，是因為我們無法僅從 function 的參數計算出一個 BMI 值。我們必須檢驗傳遞到 function 的 list，而對於其中的每個 pair 都有不同的 BMI 值。

<i>where</i> 綁定也可以是巢狀的。建立一個 function 並在其 where 子句（clause）中定義某些輔助 function，然後也給這些 function 一些輔助 function，每個 function 都有各自的 where 子句，這是一個常見的慣用寫法。

## <a name="let-it-be">Let it be</a>

## <a name="case-expressions">Case expressions</a>

