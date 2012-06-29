---
layout: page
title: 函數式地解決問題
prev:
    url: input-and-output
    title: 輸入與輸出
---

在這一章，我們要看看一些有趣的問題，以及要如何函數式地思考以盡可能優雅地解決它們。我們大概不會引入任何新概念，我們只是要伸展我們新練成的 Haskell 肌肉、並實踐我們的程式撰寫技能。每一節都會提出一個不同的問題。首先，我們會描述問題，然後我們會試著找出解決它的最好（或最差）方式。

## <a name="reverse-polish-notation-calculator">逆波蘭表示法計算機</a>

通常我們在學校寫數學表達式的時候，都是以中綴的方式撰寫的。舉例來說，我們會寫 `10 - (4 + 3) * 2`。`+`、`*` 與 `-` 為中綴運算子，就像是我們在 Haskell 中看過的中綴 function（`+`、`` `elem` ``、等等）。這使得它很方便，因為我們──作為人──可以藉由看著這種表達式，在我們心中輕易地解析它。它的缺點是，我們必須使用括號來表示優先次數。

[逆波蘭表示法](http://zh.wikipedia.org/wiki/%E9%80%86%E6%B3%A2%E5%85%B0%E8%A1%A8%E7%A4%BA%E6%B3%95)（Reverse Polish Notation，RPN）<span class="note">〔譯註：實際上就是後綴表示法〕</span>是另一種寫下數學表達式的方式。它最初看起來有點怪異，但它真的十分容易瞭解與使用，因為這裡不需要括號，而且打進計算機十分容易。雖然大部分的現代計算機都使用中綴表示法，但有些人仍然效忠於 RPN 計算機。先前表達式的 RPN 形式看起來就像這樣：`10 4 3 + 2 * -`。我們要如何計算它的結果是什麼呢？嗯，想像有個堆疊（stack）吧。你從左到右掃過這個表達式。每遇到一個數字，就將它 push 到堆疊上。當我們碰到一個運算子，就取出堆疊頂端的兩個數字（也就是說，我們將它們 pop 出來）、使用運算子與這兩個數字、然後把產生出來的數字 push 回堆疊上。當你到達表達式的結尾時，如果表達式是格式正確的，你應該會剩下一個單一的數字，而這個數字就表示結果。

<img src="img/rpn.png" alt="this expression" style="margin:10px auto 25px;display:block">
讓我們一起看看 `10 4 3 + 2 * -` 吧！首先，我們將 `10` push 到堆疊上，堆疊當前為 `10`。下一項為 `4`，所以我們也將它 push 到堆疊。堆疊當前為 `10, 4`。我們對 `3` 做相同的事，堆疊當前為 `10, 4, 3`。現在，我們遇到了一個運算子，就是 `+`！我們從堆疊 pop 出前兩個數字（所以現在堆疊只有 `10`）、將這些數字加在一起、再將這個結果 push 到堆疊。堆疊當前為 `10, 7`。我們將 `2` push 到堆疊，堆疊目前為 `10, 7, 2`。我們再次遇到一個運算子，所以讓我們將 `7` 與 `2` pop 出堆疊、將它們相乘、再將這個結果 push 到堆疊。將 `7` 與 `2` 相乘產生 `14`，所以現在我們的堆疊為 `10, 14`。最後，是一個 `-`。我們從堆疊 pop 出 `10` 與 `14`、從 `10` 減掉 `14`、並將它 push 回去。現在在堆疊中的數字為 `-4`，且因為在我們的表達式中沒有更多的數字或運算子，所以這就是我們的結果！

現在我們知道要如何手動計算 RPN 表達式了，讓我們想想我們要如何建立一個 Haskell function，其以一個包含 RPN 表達式的字串作為參數，像是 `"10 4 3 + 2 * -"`，並給我們它的結果。

這個 function 的型別將會是什麼呢？我們想讓它取一個字串作為參數，並產生一個數字作為其結果。所以它可能會是像這樣的東西：`solveRPN :: (Num a) => String -> a`。

<p class="hint">
<em>Protip:</em>
在我們自己涉及到實作、然後寫下它之前，先思考「一個 function 的型別宣告應該是什麼」是非常有幫助的。在 Haskell 中，由於有非常強大的型別系統，一個 function 的型別宣告就能告訴我們許多關於 function 的訊息。
</p>

<img src="img/calculator.png" alt="HA HA HA" style="float:left" />
酷。當在 Haskell 中實作一個問題的解法時，回頭思考你如何手動達成也是不錯的，也許嘗試看看你是否可以從中得到任何見解。這裡我們發現，我們將每個以空白分隔的數字或運算子視為一個單一 item。所以若是我們從將像是 `"10 4 3 + 2 * -"` 這樣的字串，斷開成像是 `["10","4","3","+","2","*","-"]` 這樣的 item list 開始，可能會幫助我們。

接下來，我們在我們腦中對這個 item list 做了什麼？我們從左到右掃過它，並如同我們曾經做的那樣管理一個堆疊。先前的章節有讓你想起任何東西嗎？還記得，在關於[折疊](higher-order-functions#folds)的那一節中，我們說過：幾乎任何一個元素接著一個元素、從左到右或從右到左巡訪一個 list、並建立（累加）某個結果（無論它是個數字、list、或堆疊）的 function 都能夠以一個折疊來實作。

在這種情況中，我們要使用一個左折疊，因為我們是從左到右掃過 list。累加值將會是我們的堆疊，於是折疊的結果也會是個堆疊，只是如我們所見到的，它只會有一個 item。

還有一件要考慮的事情是，嗯，我們要如何表示堆疊呢？我打算使用一個 list。我也打算讓我們堆疊的頂端維持在 list 的 head。這是因為，在一個 list 的 head（開頭）加入元素，比起在它的結尾加入元素還來得快。所以若是我們有個 `10, 4, 3` 的堆疊，我們會以 list `[3,4,10]` 來表示它。

現在我們有足夠的資訊來粗略地描繪我們的 function。它會取一個字串，像是 `"10 4 3 + 2 * -"`，並使用 `words` 將它斷成 item 的 list，以得到 `["10","4","3","+","2","*","-"]`。接著，我們要對這個 list 進行左折疊，最後會得到一個有一個單一 item 的堆疊，即 `[-4]`。我們從 list 取出這個單一 item，而這就是我們的最終結果！

所以以下是這個 function 大概樣貌：

<pre name="code" class="haskell:hs">
import Data.List

solveRPN :: (Num a) => String -> a
solveRPN expression = head (foldl foldingFunction [] (words expression))
    where   foldingFunction stack item = ...
</pre>

我們接收表達式，並將它轉成 item 的 list。接著我們以折疊 function 對這個 list 進行折疊。留意這個 `[]`，它表示起始累加器。累加器是我們的堆疊，所以 `[]` 表示我們用以開始的一個空堆疊。在取得帶著一個單一 item 的最終堆疊之後，我們對這個 list 呼叫 `head` 以取出這個 item，然後我們應用 `read`。

所以目前所剩下的，只有去實作一個折疊 function，其將會取一個堆疊──像是 `[4,10]`、與一個 item──像是 `"3"`，並回傳一個新的堆疊 `[3,4,10]`。若是堆疊為 `[4,10]`，且 item 為 `"*"`，那麼它就必須回傳 `[40]`。但在此之前，讓我們將我們的 function 轉成 [point-free style](higher-order-functions#composition)，因為它有許多稍微擾亂我的括號：

<pre name="code" class="haskell:hs">
import Data.List

solveRPN :: (Num a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction stack item = ...
</pre>

阿，好了。好多了。所以，這個折疊 function 將會取一個堆疊與一個 item，並回傳一個新的堆疊。我們要使用模式匹配，以取得一個堆疊的頂端 item，並對像是 `"*"` 與 `"-"` 這種運算子進行模式匹配。

<pre name="code" class="haskell:hs">
solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction xs numberString = read numberString:xs
</pre>

我們安排了四個模式。這些模式會被由上而下地嘗試。首先，折疊 function 將會看看當前的 item 是否為 `"*"`。如果是，它就會取一個像是 `[3,4,9,3]` 的 list，並將它的前兩個元素分別稱作 `x` 與 `y`。所以在這種情況中，`x` 會是 `3`、而 `y` 會是 `4`。`ys` 會是 `[9,3]`。它將會回傳一個就像是 `ys` 的 list，只是它有 `x` 與 `y` 的乘積作為它的 head。因此，我們從堆疊 pop 出兩個最頂端的數字、將它們相乘、並將結果 push 回堆疊上。若是這個 item 不是 `"*"`，模式匹配將會往下進行，並會檢查 "+"，以此類推。

若是這個 item 不是任何一種運算子，我們就假定它是個表示一個數字的字串。若它是個數字，我們就對這個字串呼叫 `read` 以從中取得一個數字，並回傳先前的堆疊，但有著這個被 push 到頂端的數字。

就這樣！同樣注意到，因為我們為了得到數字而對字串呼叫了 `read`，所以我們在 function 宣告加了一個額外的 `Read a` 類別限制。所以這個宣告代表，結果可以是任何為 `Num` 與 `Read` typeclass 一員的型別（像是 `Int`、`Float`、等等）。

對於 item 的 list `["2","3","+"]`，我們的 function 將會從左邊開始進行折疊。起始堆疊將會是 `[]`。它將會以 `[]` 作為堆疊（累加器）、`"2"` 作為 item 來呼叫折疊 function。所以現在的新堆疊為 `[2]`，並且折疊 function 將會以 `[2]` 作為堆疊、`"3"` 作為 item 呼叫，產生一個 `[3,2]` 的新堆疊。接著，第三次它以 `[3,2]` 作為堆疊、`"+"` 作為 item 呼叫。這使得這兩個數字從堆疊被 pop 出來、加總、再 push 回去。最終的堆疊為 `[5]`，這就是我們回傳的數字。

讓我們試試我們的 function：

<pre name="code" class="haskell:ghci">
ghci> solveRPN "10 4 3 + 2 * -"
-4
ghci> solveRPN "2 3 +"
5
ghci> solveRPN "90 34 12 33 55 66 + * - +"
-3947
ghci> solveRPN "90 34 12 33 55 66 + * - + -"
4037
ghci> solveRPN "90 34 12 33 55 66 + * - + -"
4037
ghci> solveRPN "90 3 -"
87
</pre>

酷，它正常運作！一個關於這個 function 的好消息是，它可以被輕易地修改、以支持各種不同的運算子。它們甚至不必是二元運算子。舉例來說，我們可以建立一個運算子 `"log"`，它僅從堆疊 pop 一個數字、並將它的對數（logarithm）push 回去。我們也可以建立一個三元（ternary）運算子，它從堆疊 pop 三個數字、並 push 回一個結果；或者像是 `"sum"` 的運算子，它 pop 出所有的數字、並 push 回它們的總和。

讓我們修改我們的 function，以接受更多種運算子。為了簡單起見，我們要改變它的型別宣告，使得它回傳一個 `Float` 型別的數字。

<pre name="code" class="haskell:hs">
import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction (x:y:ys) "/" = (y / x):ys
            foldingFunction (x:y:ys) "^" = (y ** x):ys
            foldingFunction (x:xs) "ln" = log x:xs
            foldingFunction xs "sum" = [sum xs]
            foldingFunction xs numberString = read numberString:xs
</pre>

哇喔，真不錯！`/` 理所當然是除法，而 `**` 為浮點數冪次（exponentiation）。對於對數運算子，我們僅對「一個單一元素與堆疊的其餘部分」進行模式匹配，因為我們只需要一個元素來求取它的自然對數。對於總和運算子，我們就回傳一個僅有一個元素的堆疊，其為到目前為止的堆疊的總和。

<pre name="code" class="haskell:ghci">
ghci> solveRPN "2.7 ln"
0.9932518
ghci> solveRPN "10 10 10 10 sum 4 /"
10.0
ghci> solveRPN "10 10 10 10 10 sum 4 /"
12.5
ghci> solveRPN "10 2 ^"
100.0
</pre>

注意到，我們可以在我們的表達式中包含浮點數，因為 `read` 知道該如何讀取它。

<pre name="code" class="haskell:ghci">
ghci> solveRPN "43.2425 0.5 ^"
6.575903
</pre>

我想，建立一個可以計算隨意的浮點數 RPN 表達式、且擁有在 10 行內輕易擴充的選擇性是非常棒的。

關於這個 function，有一件要注意的事情是，它並不是非常容錯的（fault tolerant）。當給予不合理的輸入時，它就會直接當掉。一旦我們瞭解單子（monad，它並不可怕，相信我！），我們就要以 `solveRPN :: String -> Maybe Float` 型別宣告建立一個它的容錯版本。我們現在就可以建立一個，但它會有點乏味，因為它會在每一個步驟中涉及到許多 `Nothing` 的檢查。若是你覺得你想接受這個挑戰，你可以繼續前進，去試試吧！提示：你可以使用 `reads` 來看看讀取是否成功。

## <a name="heathrow-to-london">希思羅機場到倫敦</a>

我們的下一個問題是這個：你的飛機剛剛在英格蘭降落，而你租了一輛車。你待會就有個會議，而你必須儘快（但平安地！）從希思羅機場趕到倫敦。

這裡有兩條從希思羅機場到倫敦的主要道路，還有許多橫跨它們的區域道路。你從一個十字路口行駛到另一個十字路口要耗費固定的時間。由你來找出一條能讓你儘快到達倫敦的最佳化路徑！你從左邊開始，並且能夠橫跨到另一條主要道路、或是往前進。

<img src="img/roads.png" alt="Heathrow - London" style="margin:10px auto 25px;display:block" />
如同你在圖片上看到的，在這種情況中，從希思羅機場到倫敦的最短路徑是從主要道路 B 開始、橫跨道路、在 A 上前進、再次橫跨道路、然後在 B 上前進兩次。如果我們走這條路徑，我們就要耗費 75 分鐘。假使我們選擇任何其他路徑，都會花費比這還多的時間。

我們的工作是建立一個程式，其接收表示一條道路系統的輸入，並印出穿越它的最短路徑是什麼。以下是這個案例中，輸入看起來的樣子：

<pre name="code" class="plain">
50
10
30
5
90
20
40
2
25
10
8
0
</pre>

為了在心中解析輸入檔，就以三行為一組來讀取它，並在心中將道路系統切成路段。每個路段由道路 A、道路 B 與一條橫跨道路組成。為了要剛好符合三行，我們假設要行駛最後一個橫跨路段得花 0 分鐘。這是因為我們並不在意我們是在倫敦的何處抵達，只要我們在倫敦就行了。

就像是我們在解決 RPN 計算機問題時所做的，我們要以三個步驟來解決這個問題：

* 暫時忘記 Haskell，想想我們要如何手動解決這個問題。
* 思考我們要如何在 Haskell 中表達我們的資料。
* 想出如何在 Haskell 中操作這個資料，以讓我們得出解答。

在 RPN 計算機那節中，我們首先想到了在我們手動計算一個表達式時，我們在我們心中存放著一個堆疊，然後每次一個 item 地掃過表達式。我們決定使用一個字串 list 來表示我們的表達式。最後，我們在維持堆疊時，使用一個左折疊來掃過字串 list，以產生一個解。

好，所以我們要如何手動算出從希思羅機場到倫敦的最短路徑呢？嗯，我們可以試著去猜最短的路徑是哪條，但願我們猜的是對的。這個解法對於非常小的輸入行得通，但如果我們有條有 10,000 個路段的道路呢？呀！我們也不能肯定地說我們的解是最佳解，我們只能在某種程度上，表明我們十分確定。

這不是個很好的解法。這裡有個我們道路系統的簡化圖：

<img src="img/roads_simple.png" alt="roads" style="margin:10px auto 25px;display:block" />
很好，你可以想出到達道路 A 上第一個十字路口（A 上的第一個藍點，記作 <i>A1</i>）的最短路徑是哪條嗎？這十分直觀。
我們就看看是從 A 直接往前走比較短，還是從 B 往前走、然後橫跨道路比較短。很顯然，從 B 往前走、然後橫跨道路是比較短的，因為它要花 40 分鐘，而直接從 A 走要花 50 分鐘。十字路口 <i>B1</i> 怎麼樣？一樣的事情。我們發現直接從 B 走（導致 10 分鐘的成本）是短得多的，因為從 A 走、然後橫跨道路要花費我們整整 80 分鐘！

現在我們知道到 <i>A1</i> 的最短路徑是什麼（從 B 出發、然後橫跨道路，所以我們會說是成本為 40 的 `B, C`），我們也知道到 <i>B1</i> 的最短路徑是什麼（直接從 B 出發，所以即是 `B`，成本 10）。若是我們想要知道到兩條主要道路的下一個十字路口的最短路徑，這點知識對我們有任何幫助嗎？天啊，肯定有的！

讓我們看看到 <i>A2</i> 的最短路徑會是什麼。要到 <i>A2</i>，我們要不是直接從 <i>A1</i> 到 <i>A2</i>，就是從 <i>B1</i> 往前走、然後橫跨道路（記住，我們只能往前移動、或橫跨到另一邊）。且因為我們知道到 <i>A1</i> 與 <i>B1</i> 的成本，所以我們可以輕易地想出到 <i>A2</i> 的最佳路徑是哪條。到達 <i>A1</i> 要耗費 40 分鐘，然後從 <i>A1</i> 到 <i>A2</i> 要花 5 分鐘，也就是 `B, C, A` 的成本為 45 分鐘。到達 <i>B1</i> 只要花 10 分鐘，但它要耗費額外的 110 分鐘以前往 <i>B2</i>、然後橫跨道路！所以顯然地，到 <i>A2</i> 的最短路徑為 `B, C, A`。以相同的方式，到 <i>B2</i> 的最短路徑是直接從 <i>B2</i> 往前走、然後橫跨道路。

<p class="hint">
<em>或許你會自問：</em>但首先在 <i>B1</i> 橫跨道路，然後往前走到達 <i>B2</i> 怎麼樣呢？嗯，我們已經在我們尋找抵達 <i>A1</i> 的最佳路徑時，涵蓋了從 <i>B1</i> 橫跨到 <i>A1</i> 的情況，所以我們不必也在下一步考慮這種情況。
</p>

現在我們有到達 <i>A2</i> 與 <i>B2</i> 的最佳路徑，所以我們可以無限重複下去，直到我們抵達目的地。一旦我們得到 <i>A4</i> 與 <i>B4</i> 的最佳路徑，比較短的那條就是最佳路徑了！

所以在本質上，對於第二個路段，我們僅重複了我們在第一個路段所做的步驟，只是我們考慮了先前在 A 與 B 上的最佳路徑是哪條。我們可以假定，我們在第一步中已經考慮了在 A 與 B 上的最佳路徑，只不過它們都是成本為 0 的空路徑。

這裡是個總結。要得到從希思羅機場到倫敦的最短路徑，我們這麼做：首先我們看看到達主要道路 A 上、下一個十字路口的最短路徑是哪條。兩個選擇是，直接往前走、或者從對面那條路開始往前走，然後橫跨道路。我們記住其成本與路徑。我們使用相同的方法看看到達主要道路 B 上、下一個十字路口的最短路徑是哪條，然後記住它。然後，我們看看我們要到達 A 上的下一個十字路口，是從前一個 A 的十字路口出發比較快，還是從前一個 B 的十字路口出發、然後橫跨道路比較快。我們記住最短的路徑，然後對它對面的十字路口做一樣的事。我們對每個路段這樣做，直到抵達盡頭。一旦我們抵達盡頭，兩條路徑中最短的就是我們的最佳路徑了！

所以在本質上，我們保存了在道路 A 上的最短路徑以及道路 B 上的最短路徑，且在我們到達盡頭時，兩者中比較短的即是我們的路徑。我們現在知道如何手動算出最短路徑了。如果你有足夠的時間、紙以及筆，你可以得出通過一個有著任何數量個路段的道路系統的最短路徑。

下一步！我們要如何以 Haskell 的資料型別表達這個道路系統呢？一種方式是將起始點與十字路口想成一張圖上、指向其它十字路口的節點。如果我們想像，起始點實際上是以一條長度為一的道路指向到其他節點，我們發現每個十字路口（或者節點）都指向另一邊的節點、以及它這邊的下一個節點。除了最後的節點，它們僅指向另一邊。

<pre name="code" class="haskell:hs">
data Node = Node Road Road | EndNode Road
data Road = Road Int Node
</pre>

一個節點要不是一個有著「通到另一個主要道路的道路」以及「通到下一個節點的道路」資訊的一般節點，就是個只有「通到另一個主要道路的道路」資訊的終端節點。一條路保存了「它有多長」以及「它指向哪個節點」的資訊。舉例來說，主要道路 A 上的第一個路段將會是 `Road 50 a1`，其中 `a1` 將會是節點 `Node x y`，其中 `x` 與 `y` 為指向 <i>B1</i> 與 <i>A2</i> 的道路。

另一種方式會是使用 `Maybe` 代表往前指的路段。每個節點都有個指到對面道路的路段，但只有那些並非終端節點的節點有往前指的路段。

<pre name="code" class="haskell:hs">
data Node = Node Road (Maybe Road)
data Road = Road Int Node
</pre>

這是個在 Haskell 表示道路系統的好方法，我們當然能夠用它來解決這個問題，但也許我們能想出更簡單的東西？若是我們回想起我們的手動解法，我們總是會一次確認三個部分的長度：在道路 A 上的部分、在道路 B 上的相對部分、以及連接這兩個部分的 C 部分。當我們在尋找到達 <i>A1</i> 與 <i>B1</i> 的最短路徑時，我們只需要處理前三個部分的長度，其長度為 50、10 與 30。我們將這稱作一個路段。所以我們在這個例子中使用的道路系統可以被簡單地表達成四個路段：`50, 10, 30`、`5, 90, 20`、`40, 2, 25`、與 `10, 8, 0`。

讓我們的資料型別盡可能地維持簡單總是很好的，雖然這一點也不簡單！

<pre name="code" class="haskell:hs">
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]
</pre>

這真是太完美了！它就如它所做的一樣簡單，而我感覺它會在實作我們的解法時完美地運作。`Section` 是個簡單的代數資料型別，其持有代表其三個道路部分的三個整數。我們也引入一個型別同義詞，將 `RoadSystem` 指定為 section 的 list。

<p class="hint">
我們也可以使用一個 <code>(Int, Int, Int)</code> 的 triple 來表示一個路段。使用 tuple，而非建立你自己的代數資料型別，對於某些區域性的小東西是很好的，但像這樣建立一個新的型別通常是比較好的。它給予型別系統更多關於「它是什麼」的資訊。我們可以使用 <code>(Int, Int, Int)</code> 來表示一條路段、或是一個 3D 空間的向量，我們可以操作這兩者，但它允許我們混用它們。若是我們使用 <code>Section</code> 與 <code>Vector</code> 資料型別，我們就沒辦法不小心將一個向量加到一個道路系統的路段上。
</p>

我們從希思羅機場到倫敦的道路系統現在可以像這樣表示：

<pre name="code" class="haskell:hs">
heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]
</pre>

我們現在所需要做的，只剩下在 Haskell 中實作我們先前想到的解法了。為任何給定的道路系統計算一條最短路徑的 function，其型別宣告應該要是什麼呢？它應該接收一個道路系統作為參數，並回傳一條路徑。我們也會以一個 list 表示一條路徑。讓我們引入一個 `Label` 型別，其僅是一個 `A`、`B` 或 `C` 的列舉。我們也會建立一個型別同義詞：`Path`。

<pre name="code" class="haskell:hs">
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]
</pre>

於是，我們的 function──我們將它叫做 `optimalPath`──的型別宣告應該為 `optimalPath :: RoadSystem -> Path`。若是以道路系統 `heathrowToLondon` 呼叫它，它應該要回傳下述路徑：

<pre name="code" class="haskell:hs">
[(B,10),(C,30),(A,5),(C,20),(B,2),(B,8)]
</pre>

我們必須要從左到右走過 section 的 list，並在我們進行時記住在 A 上的最佳路徑、以及在 B 上的最佳路徑。我們要在我們走過 list 時，累加最佳路徑、從左到右。這聽起來像什麼？叮、叮、叮！答對了，一個左折疊！

在手動求解時，有一個步驟是我們不斷重複的。它涉及迄今在 A 與 B 上的最佳化路徑、以及當前路段的檢驗，以產生在 A 與 B 上的新的最佳路徑。舉例來說，剛開始對於 A 與 B 的最佳路徑分別為 `[]` 與 `[]`。我們檢查路段 `Section 50 10 30`，並推斷到達 <i>A1</i> 的新的最佳路徑為 `[(B,10),(C,30)]`，而到達 <i>B1</i> 的最佳路徑為 `[(B,10)]`。若是我們將這一步看作一個 function，它就是取一對 path 與 section 的 pair，並產生一對 path 的新 pair。其型別為 `(Path, Path) -> Section -> (Path, Path)`。讓我們繼續實作這個 function，因為它一定會有用的。

<p class="hint">
<em>提示：</em>它會有用是因為 <code>(Path, Path) -> Section -> (Path, Path)</code> 可以被用作左折疊的二元 function──其型別必須為 <code>a -> b -> a</code>。
</p>

<pre name="code" class="haskell:hs">
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let priceA = sum $ map snd pathA
        priceB = sum $ map snd pathB
        forwardPriceToA = priceA + a
        crossPriceToA = priceB + b + c
        forwardPriceToB = priceB + b
        crossPriceToB = priceA + a + c
        newPathToA = if forwardPriceToA <= crossPriceToA
                        then (A,a):pathA
                        else (C,c):(B,b):pathB
        newPathToB = if forwardPriceToB <= crossPriceToB
                        then (B,b):pathB
                        else (C,c):(A,a):pathA
    in  (newPathToA, newPathToB)
</pre>

<img src="img/guycar.png" alt="this is you" style="float:right" />
這裡發生了什麼事？首先，基於迄今在 A 上的最小代價，計算出在 A 上的最小代價。我們對 B 做相同的事。我們執行 `sum $ map snd pathA`，所以若是 `pathA` 像是 `[(A,100),(C,20)]`，`priceA` 就會變成 `120`。若是我們要從 A 上的前一個十字路口直接前往 A 上的下一個十字路口，`forwardPriceToA` 為我們將會付出的代價。它等於我們先前 A 的最小成本、加上當前路段的 A 部分的長度。若是我們要從前一個 B 往前走、然後橫跨道路到下一個 A，`crossPriceToA` 便為我們將會付出的代價。它為前一個 B 迄今的最小成本、加上路段 B 部分的長度、加上路段 C 部分的長度。我們以相同的方式決定 `forwardPriceToB` 與 `crossPriceToB`。

現在我們知道到達 A 與 B 的最佳路徑是哪條，我們只需要基於此建立到達 A 與 B 的新路徑。若是藉由往前走來走到 A 比較短，我們就將 `newPathToA` 設為 `(A,a):pathA`。基本上，我們即是將 `Label` `A` 與路段長度 `a` 前置在到目前為止的最佳路徑之前。基本上，我們表明到下一個 A 十字路口的最佳路徑，是到達前一個 A 十字路口的路徑、然後通過 A 往前一個路段。記住，`A` 僅是一個標籤，而 `a` 的型別為 `Int`。為什麼我們要執行前置操作，而非進行 `pathA ++ [(A,a)]`？嗯，將一個元素加到一個 list 的開頭（也稱為 consing）比起加到結尾還要快。這代表一旦我們以這個 function 折疊一個 list，這條路徑將會是錯的，但之後反轉這個 list 很簡單。若是藉由從道路 B 往前走、然後橫跨道路到達下一個 A 十字路口比較短，則 `newPathToA` 就是到達 B 的既有路徑、然後往前走、並橫跨到 A。我們為 `newPathToB` 做一樣的事，只是每個東西都反過來<span class="note">〔譯註：A 變成 B、B 變成 A〕</span>。

最後，我們在一個 pair 中回傳 `newPathToA` 與 `newPathToB`。

讓我們以 `heathrowToLondon` 的第一個路段執行這個 function。因為它是第一個路段，所以 A 與 B 上的最佳路徑參數將會是一對空 list 的 pair。

<pre name="code" class="haskell:ghci">
ghci> roadStep ([], []) (head heathrowToLondon)
([(C,30),(B,10)],[(B,10)])
</pre>

記住，路徑是被反轉的，所以要從右到左閱讀它。由此，我們可以讀作：到達下一個 A 的最佳路徑，是從 B 開始、然後橫跨到 A；到達下一個 B 的最佳路徑是直接從 B 上的起始點往前走。

<p class="hint">
<em>最佳化提示：</em>當我們進行 <code>priceA = sum $ map snd pathA</code> 時，我們在每一步都要從路徑計算成本。若是我們將 <code>roadStep</code> 實作成一個 <code>(Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int)</code> function──其中的整數代表 A 與 B 上的最小代價，我們就不必做這件事了。
</p>

現在我們有個接收一對 path 的 pair、以及一個 section、並產生一個新的最佳路徑的 function，我們可以輕易地對 section 的 list 進行左折疊。`roadStep` 以 `([],[])` 與第一個 section 呼叫，並回傳一對到達這個 section 的最佳路徑的 pair。接著，它以這對 path 的 pair 與下一個 section 呼叫，以此類推。當我們走過所有的 section，我們會剩下一對最佳路徑的 pair，它們之中比較短的就是我們的答案。考慮到這點，我們可以實作 `optimalPath`。

<pre name="code" class="haskell:hs">
optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)
            then reverse bestAPath
            else reverse bestBPath
</pre>

我們以一對空 path 的 pair 作為起始累加器，左折疊 `roadSystem`（還記得，它是一個 section 的 list）。這個折疊的結果為一對 path 的 pair，所以我們對這個 pair 進行模式匹配，以得到它們各自的路徑。接著，我們檢查它們哪一個是比較短的，並回傳它。在回傳它之前，我們還要反轉它，因為我們選擇了 consing 而非附加，所以到目前為止的最佳路徑是被反轉的。

讓我們測試看看！

<pre name="code" class="haskell:ghci">
ghci> optimalPath heathrowToLondon
[(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]
</pre>

這是我們應該要得到的結果！真棒！它與我們預期的結果有一點不同，因為結尾有一個步驟 `(C,0)`，其代表我們在倫敦時橫跨到了另一條路，但因為橫跨道路並沒有花費任何成本，所以這仍然是正確的結果。

我們有個基於此尋找最佳路徑的 function，現在我們只需要從標準輸入讀取一個道路系統的文字表示、將它轉成 `RoadSystem` 型別、以 `optimalPath` function 執行它、並印出路徑。

首先，讓我們建立一個接收一個 list、並將它切成相同大小的群組。我們要稱它為 `groupsOf`。對於參數 `[1..10]`，`groupsOf 3` 應該要回傳 `[[1,2,3],[4,5,6],[7,8,9],[10]]`。

<pre name="code" class="haskell:hs">
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
</pre>

一個標準的遞迴 function。對於 `xs` 為 `[1..10]`、`n` 為 `3`，這等同於 `[1,2,3] : groupsOf 3 [4,5,6,7,8,9,10]`。當遞迴結束時，我們就得到我們以三個為一組的 list。以下是我們的 `main` function，其從標準輸入進行讀取、用它建立一個 `RoadSystem`、並印出最短路徑：

<pre name="code" class="haskell:hs">
import Data.List

main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathPrice = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "The price is: " ++ show pathPrice
</pre>

首先，我們從標準輸入取得所有的內容。接著，我們以我們的內容呼叫 `lines`，以將像是 `"50\n10\n30\n...` 的值轉成 `["50","10","30"..`，然後我們將 `read` 映射到它，以將它轉成一個數字的 list。我們對它呼叫 `groupsOf 3`，以讓我們將它轉成一個長度為 3 的 list 的 list。我們將 lambda `(\[a,b,c] -> Section a b c)` 映射到這個 list 的 list。如你所見，這個 lambda 僅取一個長度為 3 的 list，並將它轉成一個 section。所以 `roadSystem` 現在是我們的道路系統，而它甚至有正確的型別，即 `RoadSystem`（或是 `[Section]`）。我們以它呼叫 `optimalPath`，然後取得以文字表示的路徑與代價，並將它印出。

我們將下列文字

<pre name="code" class="plain">
50
10
30
5
90
20
40
2
25
10
8
0
</pre>

存在叫做 `paths.txt` 的檔案中，然後將它餵給我們的程式。

<pre name="code" class="plain">
$ cat paths.txt | runhaskell heathrow.hs
The best path to take is: BCACBBC
The price is: 75
</pre>

不可思議地運作了！你可以使用 `Data.Random` 模組的知識，以產生一條更長的路，接著你可以將它餵給你方才寫好的程式。若是你得到堆疊溢出（stack overflow），就試著使用`foldl'` 代替 `foldl` 吧，因為 `foldl'` 是 strict 的。
