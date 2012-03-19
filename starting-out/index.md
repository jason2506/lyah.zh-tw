---
layout: page
title: 出發
prev:
    url: introduction
    title: 引言
---

## <a name="ready-set-go">就定位，預備，開始！</a>

<img src="img/startingout.png" alt="egg" style="float:right" />
好吧，讓我們開始！假如你是那種直接跳過介紹不看的討厭鬼，你可能還是需要去閱讀前言的最後一節，因為那裡面講了此教學所需要的東西，以及我們該如何載入 function。我們所要做的第一件事是執行 ghc 的互動模式，然後呼叫一些 function 以對 haskell 有個十分基礎的體驗。開啟你的終端機（terminal）並輸入 `ghci`。你將會看到像這樣的歡迎訊息：

<pre name="code" class="haskell: ghci">
GHCi, version 6.8.2: http://www.haskell.org/ghc/  :? for help
Loading package base ... linking ... done.
Prelude>
</pre>

恭喜，你已經進入 GHCI 了！這裡的命令提示字元是 `Prelude>`，不過因為它會在你載入什麼東西時變長，所以我們想要改成 `ghci>`。假如你想要有一樣的提示字元，只需要輸入 `:set prompt "ghci> "`。

這裡是一些簡單的算術：

<pre name="code" class="haskell: ghci">
ghci> 2 + 15
17
ghci> 49 * 100
4900
ghci> 1892 - 1472
420
ghci> 5 / 2
2.5
ghci>
</pre>

不言自明。我們也可以在一行裡使用多個運算子（operator），其遵守所有常見的優先次序規則（precedence rule）。我們可以使用括號來讓優先次序清楚些，或是改變原本的優先次序。

<pre name="code" class="haskell: ghci">
ghci> (50 * 100) - 4999
1
ghci> 50 * 100 - 4999
1
ghci> 50 * (100 - 4999)
-244950
</pre>

非常酷，對吧？好吧，我知道這不酷，不過耐心聽我說。負數在是這裡一個需要小心的陷阱。假如你想要一個負數，把它用括號包起來是最好的。執行 `5 * -3` 將會使 GHCI 對你大叫，不過 `5 * (-3)` 會運作地很好。

布林（boolean）代數也非常簡單。如同你大概知道的：`&&` 代表布林值 <i>and</i>，`||` 代表布林值 <i>or</i>。`not` 反轉（negate）一個 `True` 或是 `False`。

<pre name="code" class="haskell: ghci">
ghci> True && False
False
ghci> True && True
True
ghci> False || True
True
ghci> not False
True
ghci> not (True && True)
False
</pre>

相等性的測試像這樣：

<pre name="code" class="haskell: ghci">
ghci> 5 == 5
True
ghci> 1 == 0
False
ghci> 5 /= 5
False
ghci> 5 /= 4
True
ghci> "hello" == "hello"
True
</pre>

執行 `5 + "llama"` 或是 `5 == True` 會怎麼樣呢？嗯，假如我們嘗試第一種情況，我們將會得到一個非常可怕的的錯誤訊息：

<pre name="code" class="haskell: ghci">
No instance for (Num [Char])
arising from a use of `+' at &lt;interactive&gt;:1:0-9
Possible fix: add an instance declaration for (Num [Char])
In the expression: 5 + "llama"
In the definition of `it': it = 5 + "llama"
</pre>

呀！這裡 GHCI 告訴我們的是：`"llama"` 並不是一個數字，所以它不知道該如何將它加上 5。即使這裡不是 `"llama"` 而是 `"four"` 或是 `"4"`，Haskell 仍然不能將它辨別成數字。`+` 預期它左右兩邊都是數字。假如我們嘗試執行 `True == 5`，GHCI 會告訴我們型別不相符。`+` 只能在被判別為數字的值上運作，`==` 只能在兩個可以被比較的值上運作。不過要點是它們必須擁有相同的型別：你無法把蘋果拿來跟橘子比。我們將會在稍後細看型別。記住：你可以執行 `5 + 4.0`，因為 `5` 可以作為一個整數（integer）或是一個浮點數（floating-point number）操作。`4.0` 無法作為整數操作，所以 `5` 必須配合它當作浮點數來運算。

也許你還不知道，不過我們現在一直都在使用 function。舉例來說，`*` 是一個將兩個數相乘的 function。如你所見，我們把它夾在兩個數字之間來呼叫它。這被我們稱為<i>中綴（infix）</i> function。大部分不是用在數字的 function 為<i>前綴（prefix）</i> function。讓我們來看看它們。

<img src="img/ringring.png" alt="phone" style="float:right" />
function 通常都是前綴的。所以從現在開始，我們都假設一個 function 是前綴形式，而不會明確陳述這點。在多數命令式語言中，function 藉由寫成 function 名稱接著括號中的參數——通常以逗點隔開——來呼叫的。在 Haskell 裡，function 則是藉由寫成 function 名稱、空白、然後是以空白隔開的參數來呼叫。一開始，我們嘗試呼叫 Haskell 其中一個最無聊的 function：

<pre name="code" class="haskell: ghci">
ghci> succ 8
9
</pre>

`succ` 這個 function 接收任何擁有已定義後繼子（successor）的值，並傳回此後繼子。如你所見，我們僅將 function 名稱與參數以空白來隔開。呼叫多個參數的 function 也很簡單。`min` 與 `max` 這兩個 function 接收兩個可以被排列順序的值（像是數字！）。`min` 傳回比較小的那個，而 `max` 傳回比較大的那個。你自己試看看吧：

<pre name="code" class="haskell: ghci">
ghci> min 9 10
9
ghci> min 3.4 3.2
3.2
ghci> max 100 101
101
</pre>

function application（在一個 function 後面接著空白，然後輸入參數以呼叫它）擁有最高的優先次序。我們所說的意思是，這兩個敘述（statement）是相等的：

<pre name="code" class="haskell: ghci">
ghci> succ 9 + max 5 4 + 1
16
ghci> (succ 9) + (max 5 4) + 1
16
</pre>

然而，假如我們想要取得 9 與 10 乘積的後繼子，我們無法寫作 `succ 9 * 10`，因為這將會取得 9 的後繼子再乘以 10，所以是 100。我們必須寫成 `succ (9 * 10)` 以得到 91。

假如一個 function 接收兩個參數，我們也可以將它用反引號（backtick）包起來，如同前綴 function 一樣呼叫它。舉例來說，`div` 這個 function 接收兩個整數，然後對他們進行整數相除（integral division）。執行 `div 92 10` 結果是 9。不過當我們像這樣呼叫它，可能會對哪個數字是除數、哪個數字是被除數有所混淆。所以我們藉由執行 ``92 `div` 10`` 把它當做中綴 function 來呼叫，立刻變得清楚許多。

許多來自於命令式語言的人往往堅持 function application 需要用括號來表示。例如在 C 語言中，你採用括號來呼叫 function，如： `foo()`、`bar(1)` 或是 `baz(3, "haha")`。如同我們所說，在 Haskell 中的 function application 是使用空白的。所以在 Haskell 中，這些 function 將會是 `foo`、`bar 1` 與 `baz 3 "haha"`。所以你看到像是 `bar (bar 3)` 並不代表 `bar` 被 `bar` 與 `3` 作為參數呼叫。它代表的是：我們首先以 `3` 作為參數呼叫 `bar` 以得到某個數字，然後我們以這個數字再次呼叫 `bar`。在 C 語言中，這就如同 `bar(bar(3))`。

## <a name="babys-first-functions">我們的第一個 function</a>

在前一節我們已經對呼叫 function 有個基本的體驗。現在讓我們來嘗試建立我們自己的 function！開啟你最喜歡的文字編輯器，並輸入這個接收一個數字並將之乘以兩倍的 function。

<pre name="code" class="haskell: hs">
doubleMe x = x + x
</pre>

function 被定義得像是它被呼叫的方式：function 名稱後緊跟著以空白隔開的參數。不過在定義 function 時，還有一個 `=` 接著我們所定義的 function 行為。將它儲存成 `baby.hs` 或是其他名稱。現在導覽到檔案儲存的位置，並在此執行 `ghci`。一旦進入 GHCI，就執行 `:l baby`。現在我們的腳本已經被載入了，我們可以來試試這個我們定義的 function。

<pre name="code" class="haskell: ghci">
ghci> :l baby
[1 of 1] Compiling Main             ( baby.hs, interpreted )
Ok, modules loaded: Main.
ghci> doubleMe 9
18
ghci> doubleMe 8.3
16.6
</pre>

因為 `+` 能夠在整數與浮點數上運作（事實上，任何能被視為數字的值都可以），我們的 function 也能在任何數字上運作。讓我們來建立一個接收兩個數字，將它們各自乘以二，然後再加總起來的 function。

<pre name="code" class="haskell: hs">
doubleUs x y = x*2 + y*2
</pre>

很簡單。我們也可以把它定義成 `doubleUs x y = x + x + y + y`。測試看看，你會得到這個相當顯而易見的結果（記得將這個 function 附加到 `baby.hs`，儲存之後在 GHCI 執行 `:l baby`）：

<pre name="code" class="haskell: ghci">
ghci> doubleUs 4 9
26
ghci> doubleUs 2.3 34.2
73.0
ghci> doubleUs 28 88 + doubleMe 123
478
</pre>

正如預期，你可以從你建立的別的 function 呼叫你自己的 function。考慮到這一點，我們可以像這樣重新定義 `doubleUs`：

<pre name="code" class="haskell: hs">
doubleUs x y = doubleMe x + doubleMe y
</pre>

這是一個非常簡單的範例，此種模式你會經常在整個 Haskell 見到。建立一個明顯是正確的基礎 function，然後將它們結合成更複雜的 function。這種方法也讓你免於重複撰寫。假如某些數學家發覺 2 其實是 3，讓你必須去修改你的程式怎麼辦？你可以只將 `doubleMe` 重新定義成 `x + x + x`，同時因為 `doubleUs` 呼叫了 `doubleMe`，它自動而然地能夠在這個 2 是 3 的奇怪新世界上運作。

在 Haskell 中的 function 並沒有任何特殊的順序，所以它並不在乎你是先定義 `doubleMe` 才定義 `doubleUs`，還是採用了其他定義的方式。

現在我們準備要建立一個將某數乘以二的 function，不過只有在這個數字小於或等於 100 的情況，因為大於 100 的數字已經足夠大了！

<pre name="code" class="haskell: hs">
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
</pre>

<img src="img/baby.png" alt="this is you" style="float:left" />

在這裡我們引入了 Haskell 的 if 敘述。你可能對其他語言裡的 if 敘述很熟悉。Haskell 的 if 敘述與命令式語言中的 if 敘述的不同在於：else 的部份在 Haskell 中是強制的。在命令式語言中，你可以在條件（condition）不滿足的情況下跳過幾個步驟，不過在 Haskell 中，每個 expression 與 function 都必須傳回某值。我們也可以把 if 敘述寫在同一行，不過我發現現在這樣更好讀。另外一件關於 Haskell 中的 if 敘述的事，就是它是一個 expression。一個 expression 基本上是一段傳回一個值的程式碼。`5` 是一個 expression，因為他傳回 5、`4 + 8` 是一個 expression、`x + y` 也是一個 expression，因為它傳回 `x` 與 `y` 的和。因為 else 是強制性的，一個 if 敘述總是會傳回某個值，這就是為什麼它是一個 expression 的原因。假如我們想要將先前 function 產生的每個數字加上一，我們可以把它寫成這樣：

<pre name="code" class="haskell: hs">
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
</pre>

若是我們省略了括號，這個 function 就只會在 `x` 大於 100 的時候加上一。注意在 function 名稱後面的 `'`。這一撇在 Haskell 的語法中並沒有任何特殊的意義。它是一個用在 function 名稱的合法字元。我們有時候會使用 `'` 來表示一個嚴格版的（非惰性的）function，或是一個 function 或是變數（variable）稍作修改的版本。因為 `'` 是一個在 function 中的合法字元，我們可以建立一個像這樣的 function：

<pre name="code" class="haskell: hs">
conanO'Brien = "It's a-me, Conan O'Brien!"
</pre>

這裡有兩件值得注意的事情。第一件是在這個 function 名稱中，我們並沒有將 Conan 的名字寫成大寫。這是因為 function 不能以大寫字母開頭。我們將會在稍後看到原因。第二件事是這個 function 並不接收任何參數。當一個 function 不接收任何參數，我們通常稱它是一個 definition（或是一個 name）。因為我們無法改變 name（與 function），意味著一旦我們定義了 `conanO'Brien` 與字串（string） `"It's a-me, Conan O'Brien!"`，它們兩者就是等價的。

## <a name="an-intro-to-lists">List 簡介</a>

## <a name="texas-ranges">Texas ranges</a>

## <a name="im-a-list-comprehension">我是一個 list comprehension</a>

## <a name="tuples">Tuples</a>
