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

這個 function 的型別將會是什麼呢？我們想讓它取一個字串作為參數，並產生一個數字作為其結果。所以他可能會是像這樣的東西：`solveRPN :: (Num a) => String -> a`。

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
