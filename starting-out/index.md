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

## <a name="an-intro-to-lists">List 簡介</a>

## <a name="texas-ranges">Texas ranges</a>

## <a name="im-a-list-comprehension">我是一個 list comprehension</a>

## <a name="tuples">Tuples</a>
