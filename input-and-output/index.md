---
layout: page
title: 輸入與輸出
prev:
    url: making-our-own-types-and-typeclasses
    title: 建立我們自己的型別與 Typeclass
---

<img src="img/dognap.png" alt="poor dog" style="float:right" />
我們已經提過 Haskell 是一個純函數式語言。在命令式語言中，你通常藉由給予電腦一連串的步驟來執行以將事情完成，而函數式語言則較多「東西是什麼」的定義。在 Haskell 中，一個 function 無法改變狀態，像是改變一個變數的內容（當一個 function 會改變狀態，我們就說這個 function 有<i>副作用〈side-effects〉</i>）。在 Haskell 中的 function 唯一能做的事，就是基於我們給它的參數，將結果丟回來給我們。若是一個 function 以相同參數被呼叫兩次，它就必須回傳相同的結果。雖然這在你自命令式的世界而來的時候看起來可能有一點限制，但我們已經看到這實際上非常酷。在命令式語言中，你必須保證一個僅需琢磨某些數字的簡單 function，不會在琢磨這些數字的同時燒了你的房子、綁架你的狗並以一顆馬鈴薯刮傷你的車子。舉例來說，當我們在建立一個二元搜尋樹的時候，我們不會藉由就地修改某棵樹，來將一個元素插入到一棵樹中。我們用來插入一棵二元搜尋樹的 function 實際上會回傳一棵新的樹，因為它不會改變舊的那一棵。

儘管 function 不能夠被改變狀態是有益的，因為它幫助我們思考我們的程式，但這樣卻有一個問題。若是一個 function 無法改變世界上的任何東西，它應該要如何告訴我們它計算出什麼呢？為了要告訴我們它計算出了什麼，它必須改變一個輸出裝置的狀態（通常是螢幕的狀態），然後發射傳導到我們腦中的光子，並改變我們心智的狀態。

別失望，我們還沒有失去一切。事實證明，Haskell 實際上有一個非常巧妙的系統來處理擁有副作用 function，清楚地分割了我們純粹的程式部分以及不純粹的程式部分，它做了所有像是與鍵盤和螢幕溝通這類的骯髒工作。由於這兩個被分割的部份，我們仍然可以在與外界有效溝通的同時思考我們的純粹程式，並得益於所有純粹提供的東西──像是惰性、強健性與模組化。

## <a name="hello-world">Hello, world!</a>

<img src="img/helloworld.png" alt="HELLO!" style="float:left" />
到目前為止，我們總是將我們的 function 載入到 GHCI 中，以測試並使用它們。我們也以這種方式探索過標準函式庫的 function。但現在，經過八個章節之後，我們終於要寫下第一個真正的 Haskell 程式！耶！當然，我們要做個 `"hello, world"` 的老把戲。

<p class="hint">
<em>嘿!</em>為了本章的目的，我要假設你是使用一個 unix 環境來學習 Haskell。如果你在 Windows 中，我建議你下載 <a href="http://www.cygwin.com/">Cygwin</a>，它是一個 Windows 上的 Linux-like 環境，也正是你所需要的。
</p>

所以作為開始，把接下來的文字打進你喜歡的文字編輯器裡頭：

<pre name="code" class="haskell:hs">
main = putStrLn "hello, world"
</pre>

我們定義了一個叫做 `main` 的名字，在其中我們以參數 `"hello, world"` 呼叫一個叫做 `putStrLn` 的 function。看起來非常普通，但並非如此，就如同我們不久後將會看到的。將這個檔案存成 `helloworld.hs`。

現在，我們要做某些我們以前未曾做過的事。其實我們要編譯（compile）我們的程式！我好興奮！打開你的終端機、移動到 `helloworld.hs` 所在的目錄，並像這樣做：

<pre name="code" class="plain">
$ ghc --make helloworld
[1 of 1] Compiling Main             ( helloworld.hs, helloworld.o )
Linking helloworld ...
</pre>

好！幸運的話，你就會得到像這樣的訊息，現在你可以藉由 `./helloworld` 來執行你的程式。

<pre name="code" class="haskell:plain">
$ ./helloworld
hello, world
</pre>

這樣就對了，我們的第一個將某些東西印在終端機上的編譯程式。怎麼這麼無聊！

讓我們看看我們寫了什麼。首先，讓我們看看 `putStrLn` 這個 function 的型別。

<pre name="code" class="haskell:ghci">
ghci> :t putStrLn
putStrLn :: String -> IO ()
ghci> :t putStrLn "hello, world"
putStrLn "hello, world" :: IO ()
</pre>

我們可以像這樣讀 `putStrLn` 的型別：`putStrLn` 取一個字串，並回傳一個擁有 `()`（即空 tuple，也被稱為 unit）回傳型別的 *I/O 動作（action）*。一個 I/O 動作是在執行時會帶有副作用（這通常不是從輸入讀取東西，就是在螢幕印出東西）的動作，它也會在其中包含某些回傳值。將一個字串印在終端機完全不具有任何有意義的回傳值，所以會使用一個 `()` 的假值。

<p class="hint">
空 tuple 為 <code>()</code> 的值，它的型別也是 <code>()</code>。
</p>

所以一個 I/O 動作會在何時執行呢？嗯，這就是 `main` 的功用所在。一個 I/O 動作會在我們給它一個 `main` 的名字、然後執行我們程式的時候執行。

令你整個程式僅有一個 I/O 動作似乎是個限制。這就是為什麼我們必須使用 <i>do</i> 語法來將多個 I/O 動作結合成一個。看看接下來的例子：

<pre name="code" class="haskell:hs">
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
</pre>

阿，有趣，新的語法！而這讀起來十分像是一個命令式程式。如果你編譯它並執行它，它大概會如你預期的方式運作。注意到我們寫了 <i>do</i> 然後擺了一系列步驟，就像是我們會在一個命令式程式中做的。每個步驟都是一個 I/O 動作。藉由將它們以 <i>do</i> 語法擺在一塊兒，我們將它們結合成了一個 I/O 動作。我們得到的動作的型別為 `IO ()`，因為這是其中最後一個 I/O 動作的型別。

因此，`main` 總是有個 <code>main :: IO <i>something</i></code> 的型別簽名，其中 <code><i>something</i></code> 是某個具體型別。依照慣例，我們通常不會為 `main` 指定一個型別宣告。

一個我們先前不曾看過的有趣事情是第三行，其指定 `name <- getLine`。它看起來像是它從輸入讀取一行，並將它存進一個叫做 `name` 的變數中。真的是這樣嗎？嗯，讓我們檢驗 `getLine` 的型別。

<pre name="code" class="haskell:ghci">
ghci> :t getLine
getLine :: IO String
</pre>

<img src="img/luggage.png" alt="luggage" style="float:left" />
哈，好。`getLine` 是一個包含一個結果為 `String` 型別的 I/O 動作。這很合理，因為它將會等待使用者在終端機輸入某些東西，然後這個東西將會以字串表達。所以 `name <- getLine` 有什麼用？你可以像這樣讀這段程式碼：*執行 I/O 動作 `getLine`，然後將它的結果綁定到 `name`*。`getLine` 的型別為 `IO String`，所以 `name` 的型別將是 `String`。你可以把 I/O 動作想成一個長著小腳的盒子，它會跑到真實世界並做某些事（像是在牆上塗鴉），並可能帶回某些資料。一旦它為你取得這些資料，打開盒子並取得其中資料的唯一方法即是使用 `<-` 結構。若是我們從一個 I/O 動作取出資料，我們就只能在另一個 I/O 動作中取出資料。這就是 Haskell 如何管理以清楚分割我們程式碼的純粹與不純粹部分的方式。`getLine` 在某種意義上是不純粹的，因為它不保證在兩次執行它時的結果值是相同的。這就是為什麼它稍微被 `IO` 型別建構子污染的原因，而我們只能夠在 I/O 程式碼中取得這個資料。且因為 I/O 程式碼也被污染了，所以任何依賴被污染的 I/O 資料的計算都會有個被污染的結果。

當我說<i>被污染（tainted）</i>，我並不是指以這種方式被污染、我們就永遠不能在之後的純粹的程式碼中使用包含在 I/O 動作中的結果。不，當我們將它綁定到一個名字上的時候，我們便暫時地去除在 I/O 動作中資料的汙染。當我們進行 `name <- getLine` 時，`name` 僅是一個普通的字串，因為它表示在盒子裡的東西。我們可以有個非常複雜的 function，假定它取你的名字（一個普通字串）作為參數，並基於你的名字告訴你你的命運與你整個人生的未來。我們可以這樣做：

<pre name="code" class="haskell:hs">
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name
</pre>

而 `tellFortune`（或是任何傳遞 `name` 進去地 function）不必知道任何關於 I/O 的東西，它僅是一個普通的 `String -> String` function！

看看這段程式碼。它是合法的嗎？

<pre name="code" class="haskell:hs">
nameTag = "Hello, my name is " ++ getLine
</pre>

如果你說不，吃塊餅乾吧。如果你說是，就喝下一碗岩漿。開個玩笑，別這麼做。它不能運作的原因是 `++` 要求它的參數都要為相同型別的 list。左邊的參數型別為 `String`（或是 `[Char]`，如果你想的話），而 `getLine` 的型別為 `IO String`。你無法串接一個字串與一個 I/O 動作。我們必須先取出 I/O 動作的結果以得到一個 `String` 型別的值，唯一可以做到如此的方法是在某個另外的 I/O 動作中寫下像 `name <- getLine` 這樣的東西。若是我們要處理不純粹的資料，我們必須在一個不純粹的環境中做這件事。所以不純粹的汙染十分像是不死天譴在各地蔓延，而令我們程式碼的 I/O 部分盡可能地小則對我們最有利。

每個被執行的 I/O 動作都有一個封裝在其中的結果。這就是為什麼我們先前的範例程式也可以像這樣寫：

<pre name="code" class="haskell:hs">
main = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
</pre>

然而，`foo` 僅有一個 `()` 的值，所以這樣做沒什麼意義。注意到我們沒將 `putStrLn` 綁定到任何東西上。這是因為在 <i>do</i> 區塊中，*最後一個動作無法被綁定到一個名稱上*。你會在不久之後，當我們冒險前進單子（monad）的世界時，看到它為何如此。現在，你可以以這種方式思考它：<i>do</i> 區塊會自動從最後一個動作擷取值，並將它綁定到它自己的結果。

除了最後一行，在 <i>do</i> 區塊中沒有綁定的每一行也都能夠以綁定寫成。所以 `putStrLn "BLAH"` 可以被寫成 `_ <- putStrLn "BLAH"`。但這沒什麼用，所以我們為沒有包含重要結果的 I/O 動作──<code>putStrLn <i>something</i></code>──省去 `<-`。

初學者有時候會認為執行

<pre name="code" class="haskell:hs">
name = getLine
</pre>

將會讀取輸入，然後將它的值綁定到 `name`。嗯，它並不會，它所做的只有給予 `getLine` I/O 動作一個叫做，嗯，`name` 的不同名字。記住，為了要取出一個 I/O 動作的值，你必須在另一個 I/O 動作，藉由以 `<-` 將它綁定到一個名字來執行它。

I/O 動作只會在它被給予一個 `main` 的名字、或是它在一個我們以 <i>do</i> 區塊組成的比較大的 I/O 動作中時被執行。我們也能夠使用一個 <i>do</i> 區塊來將一些 I/O 動作結合在一起，然後我們可以在另一個 <i>do</i> 區塊中使用這個 I/O 動作。無論哪種方式，它都只會在它最終落在 `main` 之中的時候才會被執行。

喔，對了，還有一個在 I/O 動作將要執行時候的案例。當我們在 GHCI 輸入一個 I/O 動作並按下 return，它就會被執行。

<pre name="code" class="haskell:ghci">
ghci> putStrLn "HEEY"
HEEY
</pre>

甚至在我們只是在 GHCI 中輸入一個數字或是呼叫一個 function 並按下 return 時，它就會（如它所需的）對它求值，然後對它呼叫 `show`，而這會立即使用 `putStrLn` 來印出這個字串。

記得 <i>let</i> 綁定嗎？如果你不記得，就讀讀[這一節](syntax-in-functions#let-it-be)以喚起你對它的記憶。它必須為 <code>let <i>bindings</i> in <i>expression</i></code> 的形式，其中 <code><i>bindings</i></code> 為給予 expression 的名稱，而 <code><i>expression</i></code> 是要使用它來求值的 expression。我們也表明在 list comprehension 中，<i>in</i> 這個部分是不需要的。嗯，你可以在 <i>do</i> 區塊中使用它，就像是你在 list comprehension 中使用它一樣。看看這個吧：

<pre name="code" class="haskell:hs">
import Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
</pre>

看看在 <i>do</i> 區塊中的 I/O 動作是如何排列的？也注意到 <i>let</i> 如何與 I/O 動作一同排列、以及 <i>let</i> 的名稱如何與其它名稱一同排列？這是個好習慣，因為在 Haskell 中的縮排（indentation）是很重要的。現在，我們執行 `map toUpper firstName`，它會把像是 `"John"` 這樣的東西轉成像 `"JOHN"` 這樣更酷的字串。我們將這個大寫字串綁定在一個名稱上，然後將它使用在之後我們要印到終端機上的字串中。

或許你會不知道該在何時使用 `<-` 以及 <i>let</i> 綁定？嗯，記住，`<-`（當前）是用以執行 I/O 動作，並將其結果綁定到名稱上。然而，`map toUpper firstName` 並非一個 I/O 動作。它是 Haskell 中的純 expression。所以，在你要綁定 I/O 動作的結果到 name 的時候使用 `<-`，你可以使用 <i>let</i> 綁定來將純 expression 綁定到名稱上。假使我們做了像 `let firstName = getLine` 這樣的事，我們僅是有個呼叫 `getLine` 這個 I/O 動作的不同名稱，且我們仍要透過執行一個 `<-` 來執行它。

現在我們要建立一個持續讀取一行、並印出反轉單詞的同一行的程式。程式執行將會在我們輸入一個空行的時候停止。這就是這個程式：

<pre name="code" class="haskell:hs">
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
</pre>

你可以在我們檢閱程式碼之前執行看看，以瞭解它在做什麼。

<p class="hint">
<em>提點：</em>要執行一個程式，你可以藉由 <code>ghc --make helloworld</code> 編譯它，然後藉由 <code>./helloworld</code> 執行生成的執行檔、或是使用 <code>runhaskell</code> 命令，像是：<code>runhaskell helloworld.hs</code>，而你的程式將會立刻被執行。
</p>

首先，讓我們看看 `reverseWords` function。它僅是一個取一個像是 `"hey there man"` 的字串、然後以它呼叫 `words` 以產生一個像是 `["hey","there","man"]` 的單詞 list 的普通 function。然後我們將 `reverse` 映射到 list 上、得到 `["yeh","ereht","nam"]`，然後我們使用 `unwords` 將它擺回字串中，最終結果為 `"yeh ereht nam"`。看看在這裡我們是如何使用 function composition 的。少了 function composition，我們就必須寫下像是 `reverseWords st = unwords (map reverse (words st))` 這樣的東西。

`main` 怎麼樣？首先，我們藉由執行 `getLine` 來從終端機取得一行，並將這行叫做 `line`。然後，我們有個條件式 expression。記得在 Haskell 中，每個 <i>if</i> 必須有個對應的 <i>else</i>，因為每個 expression 都必須擁有某個值。我們弄了個 <i>if</i>，以在條件為真的時候（在我們的情況裡，就是我們輸入的是空行）執行其 I/O 動作；而在條件不為真的時候，在 <i>else</i> 下的 I/O 動作就會被執行。這就是為什麼在一個 I/O <i>do</i> 區塊中，<i>if</i> 必須為 <code>if <i>condition</i> then <i>I/O action</i> else <i>I/O action</i></code> 的形式。

讓我們先看看 <i>else</i> 子句下會發生什麼事。因為在 <i>else</i> 之後我們只能有一個 I/O 動作，所以我們使用一個 <i>do</i> 區塊來將兩個 I/O 動作結合成一個。你也可以將這個部分寫成：

<pre name="code" class="haskell:hs">
else (do
    putStrLn $ reverseWords line
    main)
</pre>

這使得 <i>do</i> 區塊可以被視為一個 I/O 動作這點更加明確，但這樣比較醜。總而言之，在 <i>do</i> 區塊之中，我們對我們從 `getLine` 取得到那行呼叫 `reverseWords`，然後將它印到終端機。在這之後，我們僅執行了 `main`。它被遞迴地呼叫，而這是沒問題的，因為 `main` 本身也是個 I/O 動作。所以在某種意義上，我們回到了程式的起始處。

現在，當 `null line` 為真時會發生什麼呢？在這種情況中，<i>then</i> 後面的東西會被執行。若是我們看一看，我們會發現它是 `then return ()`。若是你寫過像是 C、Java 或是 Python 這類命令式語言，你大概會認為你知道這個 `return` 做的是什麼，或許你已經略過這個非常長的段落了。嗯，事實是：*Haskell 中的 `return` 真的一點也不像大多數其它語言中的 `return`！*它有一樣的名字，這混淆了許多人，但實際上它是完全不同的。在命令式語言中，`return` 通常會中止一個方法（method）或是一個子程式（subroutine）的執行，並令它將某個值回傳給它的呼叫者。在 Haskell 中（尤其是在 I/O 動作中），它會以一個純粹值建立一個 I/O 動作。若是你想作先前的盒子類比，即是它取一個值，並將它包在一個盒子中。產生的 I/O 動作實際上不會作任何事，它僅有這個被封裝的值作為其結果。所以在一個 I/O 情境中，`return "haha"` 的型別將會是 `IO String`。僅將一個純粹值轉成一個不作任何事的 I/O 動作的目的是什麼呢？為什麼要以 `IO` 比起其所必須的更加污染我們的程式呢？嗯，我們需要某個 I/O 動作來處理輸入空行的情況。這就是為什麼我們要藉由寫下 `return ()` 來建立一個不作任何事的假的 I/O 動作。

使用 `return` 並不會導致 I/O <i>do</i> 區塊停止執行、或是類似於此的任何事情。舉例來說，這個程式將會相當順利地一路完成到最後一行：

<pre name="code" class="haskell:hs">
main = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line
</pre>

所有這些 `return` 所做的是，它建立了一個除了擁有一個被封裝的結果以外，不會真的做任何事的 I/O 動作，且因為它沒有被綁定到一個名稱上，所以這個結果會被丟棄。我們可以將 `return` 使用在與 `<-` 的結合中，以綁定東西到名稱上。

<pre name="code" class="haskell:hs">
main = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b
</pre>

所以你發現，`return` 大致上與 `<-` 相反。`return` 取一個值並將它包在一個盒子中，而 `<-` 則是取一個盒子（並執行它）並取出它的值、綁定它到一個名稱上。但這麼做有點多餘，主要是因為你可以在 <i>do</i> 區塊中使用 <i>let</i> 綁定來綁定到名稱上，像這樣：

<pre name="code" class="haskell:hs">
main = do
    let a = "hell"
        b = "yeah"
    putStrLn $ a ++ " " ++ b
</pre>

在處理 I/O <i>do</i> 區塊的時候，我們使用 `return` 大多是因為我們需要建立一個不做任何事的 I/O 動作、或是因為我們不想要由一個 <i>do</i> 建立的 I/O 動作擁有其最後一個動作的結果值，而是想要令它有個不同的結果值，所以我們使用 `return` 來建立一個總是有我們所需結果的 I/O 動作，並將它擺在最後面。

<p class="hint">
一個 <i>do</i> 區塊也可以僅有一個 I/O 動作。在這種情況中，它就與僅寫下這個 I/O 動作相同。有些人在這種情況中比較偏好寫成 <code>then do return ()</code>，因為 <i>else</i> 也有個 <i>do</i>。
</p>

在前進到檔案之前，讓我們看看一些處理 I/O 時的有用 function。

<code class="label function">putStr</code> 在其取一個字串作為參數、並回傳一個將會把這個字串印到終端機的 I/O 動作這點十分像是 `putStrLn`，只是 `putStr` 不會在印出字串之後跳到新的一行，而 `putStrLn` 會。

<pre name="code" class="haskell:hs">
main = do   putStr "Hey, "
            putStr "I'm "
            putStrLn "Andy!"
</pre>

<pre name="code" class="plain">
$ runhaskell putstr_test.hs
Hey, I'm Andy!

</pre>

它的型別簽名為 `putStr :: String -> IO ()`，所以封裝在產生的 I/O 動作中的結果為 unit。一個啞值（dud value），所以綁定它並無意義。

<code class="label function">putChar</code> 取一個字元並回傳一個將會將它印出到終端機的 I/O 動作。

<pre name="code" class="haskell:hs">
main = do   putChar 't'
            putChar 'e'
            putChar 'h'
</pre>

<pre name="code" class="plain">
$ runhaskell putchar_test.hs
teh
</pre>

`putStr` 實際上是以 `putChar` 的協助被遞迴地定義的。`putStr` 的邊界條件為空字串，所以若是我們要印出一個空字串，就使用 `return ()` 回傳一個不做任何事的 I/O 動作。若是它並不是空的，則藉由執行 `putChar` 來印出字串的第一個字元，然後使用 `putStr` 印出其餘的部份。

<pre name="code" class="haskell:hs">
putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do
    putChar x
    putStr xs
</pre>

看到我們能夠如何在 I/O 中使用遞迴，就如同我們可以在純粹的程式碼中使用它。就像在純粹的程式碼中，我們定義邊界案例，然後思考結果實際上是什麼。它是個先印出第一個字元、然後輸出字串其餘部分的動作。

<code class="label function">print</code> 取一個為 `Show` 實體的任意型別的值（意味著我們知道如何以一個字串來表示它）、以這個值呼叫 `show` 來字串化（stringify）它、然後將這個字串輸出到終端機。基本上，它就是 `putStrLn . show`。它首先對一個值執行 `show`，然後將這餵給 `putStrLn`──其回傳一個將會印出我們的值的 I/O 動作。

<pre name="code" class="haskell:hs">
main = do   print True
            print 2
            print "haha"
            print 3.2
            print [3,4,3]
</pre>

<pre name="code" class="haskell:hs">
$ runhaskell print_test.hs
True
2
"haha"
3.2
[3,4,3]
</pre>

如你所見，這是個非常方便的 function。記得我們談過 I/O 動作只有在它們落在 `main`、或是我們試著在 GHCI 提示字元中對它求值時才會被執行嗎？當我們輸入一個值（像是 `3` 或是 `[1,2,3]`）並按下 return 鍵時，GHCI 實際上是對這個值使用 `print` 來將它顯示在我們的終端機上！

<pre name="code" class="haskell:ghci">
ghci> 3
3
ghci> print 3
3
ghci> map (++"!") ["hey","ho","woo"]
["hey!","ho!","woo!"]
ghci> print (map (++"!") ["hey","ho","woo"])
["hey!","ho!","woo!"]
</pre>

當我們想要印出字串時，我們通常會使用 `putStrLn`，因為我們不想要包著它的引號，但要將其它型別的值印到終端機，`print` 才是被使用得最多的。

<code class="label function">getChar</code> 是個從輸入讀取一個字元的 I/O 動作。於是，它的型別簽名為 `getChar :: IO Char`，因為包含在 I/O 動作之中的結果是一個 `Char`。注意到，由於緩衝（buffer）的緣故，直到使用者按下 return 鍵，字元的讀取都不會真的發生。

<pre name="code" class="haskell:hs">
main = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()
</pre>

這個程式看起來像是，它需要讀取一個字元，然後檢查它是否是個空白。若是，就中斷執行；若否，就將它印到終端機，然後再做一遍相同的事情。嗯，它大致上是這樣做的，只是並非是以你所預期的方式。檢查看看：

<pre name="code" class="haskell:hs">
$ runhaskell getchar_test.hs
hello sir
hello
</pre>

第二行為輸入。我們輸入 `hello sir` 然後按下 return。由於緩衝的緣故，程式的執行只有在我們按下 return 之後才會開始，而不是在每次輸入字元之後。但一旦我們按下 return，它就會對我們至今輸入的東西進行操作。試著用看看這個程式以瞭解它！

<code class="label function">when</code> 這個 function 能在 `Control.Monad` 中找到（執行 `import Control.Monad` 來使用它）。這很有趣，因為在一個 <i>do</i> 區塊中，它看起來就像是一個控制流程（control flow）敘述，但它實際上是個普通的 function。它接收一個布林值與一個 I/O 動作，若是這個布林值為 `True`，它就回傳我們提供給它的相同 I/O 動作。然而，若是它為 `False`，它就回傳 `return ()` 動作，即是個不做任何事的 I/O 動作。這裡我們可以使用 `when` 來改寫我們先前示範 `getChar` 的那段程式碼：

<pre name="code" class="haskell:hs">
import Control.Monad

main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main
</pre>

所以如你所見，它對於封裝 <code>if <i>something</i> then do <i>some I/O action</i> else return ()</code> 模式是很有用的。

<code class="label function">sequence</code> 接收一個 I/O 動作的 list，並回傳一個將會一個接著一個執行這些動作的 I/O 動作。I/O 動作中所包含的結果將會是所有被執行的 I/O 動作的結果的 list。它的型別簽名為 `sequence :: [IO a] -> IO [a]`。這樣做：

<pre name="code" class="haskell:hs">
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]
</pre>

完全等同於這樣做：

<pre name="code" class="haskell:hs">
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
</pre>

所以 `sequence [getLine, getLine, getLine]` 會建立一個將三次執行 `getLine` 的 I/O 動作。若是我們將這個動作綁定到一個名稱上，結果即是所有結果的 list，所以在我們的例子中，即是使用者在提示字元輸入的三個值的 list。

一個帶著 `sequence` 的常見模式，是在我們將像是 `print` 或是 `putStrLn` 這類 function 映射到 list 上的時候。執行 `map print [1,2,3,4]` 並不會建立一個 I/O 動作。它會建立一個 I/O 動作的 list，因為這就像是寫下 `[print 1, print 2, print 3, print 4]`。若是我們想要將 I/O 動作的 list 轉成一個 I/O 動作，我們必須 sequence 它。

<pre name="code" class="haskell:ghci">
ghci> sequence (map print [1,2,3,4,5])
1
2
3
4
5
[(),(),(),(),()]
</pre>

最後的 `[(),(),(),(),()]` 是怎麼了？嗯，當我們在 GHCI 對一個 I/O 動作求值，它就會被執行、然後其結果會被印出，除非這個結果為 `()`──在這種情況它不會被印出來。這就是為什麼在 GHCI 對 `putStrLn "hehe"` 求值僅印出 `hehe`（因為包含在 `putStrLn "hehe"` 的結果為 `()`）。但當我們在 GHCI 執行 `getLine` 的時候，這個 I/O 動作的結果會被印出來，因為 `getLine` 的型別為 `IO String`。

因為映射一個回傳 I/O 動作的 function 到一個 list 上、然後再 sequence 它是如此地常見，所以引入了通用 function <code class="label function">mapM</code> 與 <code class="label function">mapM_</code>。`mapM` 接收一個 function 與一個 list，並映射這個 function 到 list 上，然後 sequence 它。`mapM_` 亦同，只是它會在之後丟棄結果。我們通常會在我們不關心我們的 sequenced I/O 動作為何時使用 `mapM_`。

<pre name="code" class="haskell:ghci">
ghci> mapM print [1,2,3]
1
2
3
[(),(),()]
ghci> mapM_ print [1,2,3]
1
2
3
</pre>

<code class="label function">forever</code> 接收一個 I/O 動作，並回傳一個永遠重複它得到的 I/O 動作的 I/O 動作。它位在 `Control.Monad` 中。這個小程式將會不斷地要求使用者輸入東西，並將它轉成大寫吐回去給他：

<pre name="code" class="haskell:hs">
import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l
</pre>

<code class="label function">forM</code>（位在 `Control.Monad` 中）就像是 `mapM`，只是它的參數左右對調。第一個參數為 list、第二個參數為映射到這個接著要被 sequence 的 list 的 function。這為何有用？嗯，藉由有創意地使用 lambda 與 <i>do</i> 標記，我們可以像這樣做：

<pre name="code" class="haskell:hs">
import Control.Monad

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
</pre>

`(\a -> do ... )` 為一個接收一個數字並回傳一個 I/O 動作的 function。我們必須以括號包住它，否則 lambda 會認為最後兩個 I/O 動作該被歸入其中。注意到我們在 <i>do</i> 區塊執行了 `return color`。我們藉此以令 <i>do</i> 區塊定義的 I/O 動作擁有我們包含在其中的顏色的結果。實際上我們不必這樣做，因為 `getLine` 已經在其中包含這個結果了。執行 `color <- getLine` 接著 `return color` 僅是取出 `getLine` 的結果然後再重新包裝一次，所以這就與僅執行 `getLine` 相同。（以它的兩個參數呼叫的）`forM` 產生一個 I/O 動作，我們將它的結果綁定到 `colors` 上。`colors` 僅是一個持有字串的普通 list。最後，我們藉由 `mapM putStrLn colors` 印出所有的顏色。

你可以將 `forM` 想成是：為這個 list 中的每個元素建立一個 I/O 動作。每個 I/O 動作會做的事必須視被用來建立動作的元素而定。最後，執行這些動作並將其結果綁定到某個東西上。我們不必綁定它，我們也可以將它丟棄。

<pre name="code" class="plain">
$ runhaskell form_test.hs
Which color do you associate with the number 1?
white
Which color do you associate with the number 2?
blue
Which color do you associate with the number 3?
red
Which color do you associate with the number 4?
orange
The colors that you associate with 1, 2, 3 and 4 are:
white
blue
red
orange
</pre>

實際上我們可以不採用 `forM` 來做到這件事，只是利用 `forM` 它會更加易讀。通常我們會在我們要映射、並 sequence 某些使用 <i>do</i> 標記的地方定義的動作時寫下 `forM`。同樣的，我們能夠以 `forM colors putStrLn` 取代最後一行。

在這一節，我們學到了輸入與輸出的基礎。我們也發現了 I/O 動作是什麼、它們如何令我們能夠進行輸入與輸出、以及它們會在何時真正地被執行。再次聲明，在 Haskell 中的 I/O 動作是十分近似於其它任何值的值。我們可以將它作為參數傳遞到 function、function 可以回傳 I/O 動作作為結果。它們的特別之處在於，若是它們落在 `main` function（或是為 GHCI 中的結果），它們就會被執行。而這就是它們把東西印在你的螢幕上、或是透過你的麥克風喇叭播放 Yakety Sax 的時候。每個 I/O 動作也可以封裝一個告訴你它從真實世界得到了什麼的結果。

別將像是 `putStrLn` 這樣的 function 想成是一個接收一個字串並將它印在螢幕上的 function。將它想成一個接收一個字串並回傳一個 I/O 動作的 function。這個 I/O 動作將會──在執行的時候──在你的終端機上印出美妙的詩句。

## <a name="files-and-streams">檔案與串流</a>

<img src="img/streams.png" alt="streams" style="float:right" />
`getChar` 是一個從終端機讀取單一字元的 I/O 動作。`getLine` 是一個從終端機讀取一行的 I/O 動作。這兩者十分直觀，許多程式語言都有類似於此的 function 或敘述。但現在，讓我們看看 <code class="label function">getContents</code>。`getContents` 是一個從標準輸入（standard input）讀取任何東西、直到它遇到檔案結尾（end-of-file，eof）字元為止的 I/O 動作。它的型別為 `getContents :: IO String`。`getContents` 酷的地方在於它做的是惰性 I/O。當我們執行 `foo <- getContents` 時，它不會立即讀取所有的輸入、將它儲存在記憶體、然後將它綁定到 `foo`。不會的，它很懶惰！它會說：<i>「是是，我晚點就會從終端機讀取輸入，在你真的需要它的時候！」</i>.

`getContents` 在我們要將一個程式的輸出導向（pipe）到我們程式的輸入時十分有用。假如你不知道導向在 unix 系統中是如何運作的，這裡有個快速入門。讓我們建立一個包含下述小俳句的文字檔：

<pre name="code" class="plain">
I'm a lil' teapot
What's with that airplane food, huh?
It's so small, tasteless
</pre>

是阿，這個俳句很爛，有什麼關係呢？如果哪個人知道任何不錯的俳句教學，讓我知道吧。

現在，回想起我們在介紹 `forever` function 時寫的小程式吧。它要求使用者輸入一行、以大寫形式回傳給他，然後再做一遍同樣的事情，無限進行下去。為了讓你不必一路捲回去，這裡再寫一次：

<pre name="code" class="haskell:hs">
import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l
</pre>

我們要將這個程式存成 `capslocker.hs` 或是什麼的，然後編譯它。然後，我們要使用一個 unix 導向，來將我們的文字檔直接餵給我們的小程式。我們要使用 GNU <i>cat</i> 的幫助，其會印出作為參數給予它的檔案。檢查看看，booyaka！

<pre name="code" class="plain">
$ ghc --make capslocker
[1 of 1] Compiling Main             ( capslocker.hs, capslocker.o )
Linking capslocker ...
$ cat haiku.txt
I'm a lil' teapot
What's with that airplane food, huh?
It's so small, tasteless
$ cat haiku.txt | ./capslocker
I'M A LIL' TEAPOT
WHAT'S WITH THAT AIRPLANE FOOD, HUH?
IT'S SO SMALL, TASTELESS
capslocker &lt;stdin&gt;: hGetLine: end of file
</pre>

如你所見，將一個程式（在我們的例子中是 <i>cat</i>）的輸出導向到另一個程式（<i>capslocker</i>）的輸入是以 `|` 來做到。我們所做的幾乎等同於直接執行 <i>capslocker</i>、在終端機輸入我們的俳句、然後送出檔案結尾符號（這通常藉由按下 Ctrl-D 來做到）。它就像是執行 <i>cat haiku.txt</i>，然後說：「等等，別把這個印到終端機上，而是把它告訴 <i>capslocker</i>！」

所以我們使用 `forever` 所做的，基本上是取得輸入、並將它轉換成某個輸出。這就是為什麼我們必須使用 `getContents` 來讓我們的程式更短、更好：

<pre name="code" class="haskell:hs">
import Data.Char

main = do
    contents <- getContents
    putStr (map toUpper contents)
</pre>

我們執行 `getContents` 這個 I/O 動作，並將它產生的字串命名為 `contents`。然後，我們將 `toUpper` 映射到這個字串，並將它印在終端機上。謹記於心，由於字串基本上是個惰性的 list，且 `getContents` 是個惰性 I/O，所以在印出大寫的版本之前，它不會立即試著讀取全部內容、並將它儲存到記憶體中。更確切地說，它會在它讀取它的時候印出大寫的版本，因為它只有在它真的需要的時候才會從輸入讀取一行。

<pre name="code" class="plain">
$ cat haiku.txt | ./capslocker
I'M A LIL' TEAPOT
WHAT'S WITH THAT AIRPLANE FOOD, HUH?
IT'S SO SMALL, TASTELESS
</pre>

酷，它正常運作。若是我們直接執行 <i>capslocker</i>，並試著輸入幾行呢？

<pre name="code" class="plain">
$ ./capslocker
hey ho
HEY HO
lets go
LETS GO
</pre>

我們藉由按下 Ctrl-D 來離開程式。非常好！如你所見，它會一行接著一行將我們的大寫輸入印出來給我們。當 `getContents` 的結果被綁定到 `contents` 上時，它在記憶體中並不是以真正的字串被表示，而比較像是一個「它最終將會產生字串」的承諾。當我們將 `toUpper` 映射到 `contents` 時，這同樣是個「這個 function 會映射到最終結果」的承諾。最後當 `putStr` 發生時，它會要求先前的承諾：<i>「嘿，我需要一行大寫字串！」</i>它還不擁有任何一行，所以它對 `contents` 說：<i>「嘿，真的去從終端機取出一行怎麼樣？」</i>所以這就是 `getContents` 真的從終端機讀取一行、並將之給予要求它產生某些明確東西的程式的時候。這個程式接著將 `toUpper` 映射到這一行，並將它交給 `putStr` 來印出它。然後，`putStr` 說：<i>「嘿，我需要下一行，來吧！」</i>，而這會重複到沒有更多輸入的時候，其被表示為檔案結尾字元。

讓我們建立一個接收某個輸入、並只印出短於十個字元的那幾行的程式。看看吧：

<pre name="code" class="haskell:hs">
main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in  result
</pre>

我們已經讓我們程式的 I/O 部分盡可能地短。因為我們的程式應該接收某個輸入、並基於輸入印出某個輸出，所以我們可以藉由讀取輸入內容、以它執行一個 function、然後印出 function 傳回的東西的方式來實作它。

`shortLinesOnly` function 像這樣運作：它取一個字串，像是 `"short\nlooooooooooooooong\nshort again"`。這個字串有三行，其中兩行很短，而中間那行很長。它以這個字串執行 `lines` function，其會將它轉成 `["short", "looooooooooooooong", "short again"]`，然後我們將它綁定到 `allLines` 這個名稱上。這個字串 list 接著被過濾，使得只有短於十個字元的那幾行留在 list 中，產生 `["short", "short again"]`。最後，`unlines` 會將這個 list 結合成一個換行分隔的單一字串，得到 `"short\nshort again"`。讓我們試一試。

<pre name="code" class="plain">
i'm short
so am i
i am a loooooooooong line!!!
yeah i'm long so what hahahaha!!!!!!
short line
loooooooooooooooooooooooooooong
short
</pre>

<pre name="code" class="plain">
$ ghc --make shortlinesonly
[1 of 1] Compiling Main             ( shortlinesonly.hs, shortlinesonly.o )
Linking shortlinesonly ...
$ cat shortlines.txt | ./shortlinesonly
i'm short
so am i
short
</pre>

我們將 <i>shortlines.txt</i> 的內容導向到 <i>shortlinesonly</i> 的輸入。且如同輸出，我們只取短的那幾行。

這種從輸入取得某字串、以一個 function 轉換它、然後輸出它的模式是如此的常見，所以這裡有個叫做 <code class="label function">interact</code> 的 function 能令此更加容易。`interact` 取一個型別為 `String -> String` 的 function 作為參數，並回傳一個將取某個輸入、對它執行 function、然後印出 function 結果的 I/O 動作。讓我們使用它來修改我們的程式。

<pre name="code" class="haskell:hs">
main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in  result
</pre>

為了表現它能夠以更短的程式碼達成（即使它會比較不易讀），並顯示我們的 function composition 技能，我們要稍微進一步地修改它。

<pre name="code" class="haskell:hs">
main = interact $ unlines . filter ((<10) . length) . lines
</pre>

哇，我們真的將它減少到只有一行，這真的好酷！

`interact` 可以被用來建立將某些內容導向到它、然後倒出某個結果的程式，或是被用來建立從使用者取得一行輸入、基於這一行回傳某個結果、然後再取另一行、並如此進行下去的程式。這兩者之間實際上沒有真正的區別，它僅是視使用者應該如何用它而定的。

讓我們建立一個持續讀取一行、然後告訴我們這行是否是個回文（palindrome）的程式。我們可以僅用 `getLine` 來讀取一行、告訴使用者它是否是個回文、然後再一次執行 `main`。但若是我們使用 `interact` 會更加簡單。使用 `interact` 的時候，想一想要將某個輸入轉換成所需的輸出，你所需要的是什麼。在我們的情況中，我們必須將輸入的每一行以 `"palindrome"` 或是 `"not a palindrome"` 取代。所以我們必須寫一個將某個像是 `"elephant\nABCBA\nwhatever"` 的值轉成 `"not a palindrome\npalindrome\nnot a palindrome"` 的 function。讓我們動手做吧！

<pre name="code" class="haskell:hs">
respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))
    where   isPalindrome xs = xs == reverse xs
</pre>

讓我們以 point-free 的形式寫它。

<pre name="code" class="haskell:hs">
respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
    where   isPalindrome xs = xs == reverse xs
</pre>

十分直觀。首先它將某個像是 `"elephant\nABCBA\nwhatever"` 的值轉成 `["elephant", "ABCBA", "whatever"]`、然後它將這個 lambda 映射到它之上，得到 `["not a palindrome", "palindrome", "not a palindrome"]`、然後 `unlines` 將這個 list 結合成一個單一的、以換行分隔的字串。現在我們可以做：

<pre name="code" class="haskell:hs">
main = interact respondPalindromes
</pre>

讓我們測試看看：

<pre name="code" class="plain">
$ runhaskell palindromes.hs
hehe
not a palindrome
ABCBA
palindrome
cookie
not a palindrome
</pre>

即使我們建立一個將一個輸入的大字串轉換成另一個字串的程式，它表現得依然像是我們建立的是一行接著一行做的程式。這是因為 Haskell 是惰性的，且它想要印出結果字串的第一行，但因為它仍未擁有輸入的第一行，所以它無法如此。所以只要我們給它輸入的第一行，它就會印出輸出的第一行。我們藉由送出檔案結尾字元來離開這個程式。

我們也可以僅藉由將一個檔案導向它來使用這個程式。讓我們假定我們有這個檔案：

<pre name="code" class="haskell:hs">
dogaroo
radar
rotor
madam
</pre>

並且將它存成 `words.txt`。這即是我們藉由將它導向到我們程式而得的：

<pre name="code" class="plain">
$ cat words.txt | runhaskell palindromes.hs
not a palindrome
palindrome
palindrome
palindrome
</pre>

再一次，我們得到如同我們執行我們的程式、並由我們自己在標準輸入中輸入文字而得的相同輸出。我們沒看到 `palindromes.hs` 的輸入，因為輸入來自於檔案，而不是來自於我們打字進去。

所以現在你大概瞭解惰性 I/O 如何運作、以及我們能如何用它來做為我們的優勢了。你可以僅從對於某個給定輸入的輸出應該是什麼的角度來思考，並寫下一個 function 來達成這個轉換。在惰性 I/O 中，沒有任何東西會從輸入被吃掉，直到它確實要如此的時候，因為我們現在就要依賴此輸入來印出東西。

到目前為止，我們已經藉由將東西印到終端機上、以及從終端機讀取東西與 I/O 一同運作過了。但要怎麼樣讀取並寫入檔案呢？嗯，在某種程度上，我們已經做到這件事了。一種思考從終端機進行讀取的方式，是去想像它就像是從一個（有點特殊的）檔案進行讀取。同樣適用於寫入到終端機，它有點像是寫入到一個檔案。我們可以將這兩個檔案叫做 `stdout` 與 `stdin`，分別代表<i>標準輸出（standard output）</i>與<i>標準輸入（standard input）</i>。謹記於心，我們會發現從檔案進行寫入與讀取，非常近似於寫入到標準輸出與讀取自標準輸入。

我們要從一個開啟一個叫做 <i>girlfriend.txt</i> 的檔案──其包含來自 Avril Lavigne 第一首熱門單曲 <i>Girlfriend</i> 的一節，並僅將它印到終端機上的非常簡單的程式開始。這即是 <i>girlfriend.txt</i>：

<pre name="code" class="plain">
Hey! Hey! You! You!
I don't like your girlfriend!
No way! No way!
I think you need a new one!
</pre>

而這是我們的程式：

<pre name="code" class="haskell:hs">
import System.IO

main = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
</pre>

執行它，我們得到預期的結果：

<pre name="code" class="plain">
$ runhaskell girlfriend.hs
Hey! Hey! You! You!
I don't like your girlfriend!
No way! No way!
I think you need a new one!
</pre>

讓我們一行接著一行檢閱它。第一行僅是四個感嘆詞，以引起我們的注意。在第二行，Avril 告訴我們她並不喜歡我們當前的浪漫伴侶。第三行旨在強調不贊成這點、而第四行則建議我們應該尋找一個新的女朋友。

讓我們也一行接著一行檢閱程式！我們的程式是以一個 <i>do</i> 區塊結合的數個 I/O 動作。在 <i>do</i> 區塊的第一行，我們注意到一個叫做 <code class="label function">openFile</code> 的新 function。這是它的型別簽名：`openFile :: FilePath -> IOMode -> IO Handle`。若是你大聲地將它讀出來，它說明：`openFile` 接收一個檔案路徑與一個 `IOMode`，並回傳一個將會打開一個檔案、並擁有封裝為其結果的檔案的關聯 handle
的 I/O 動作。

`FilePath` 僅是一個 `String` 的[型別別名](making-our-own-types-and-typeclasses#type-synonyms)，被簡單地定義成：

<pre name="code" class="haskell:hs">
type FilePath = String
</pre>

`IOMode` 是一個像這樣被定義的型別：

<pre name="code" class="haskell:hs">
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
</pre>

<img src="img/file.png" alt="A FILE IN A CAKE!!!" style="float:left" />
就像是我們可以表達代表一週日子的七個可能的值的型別，這個型別是一個表達我們要對我們打開的檔案做什麼的列舉。非常簡單。注意到這個型別是 `IOMode` 而非 `IO Mode`。`IO Mode` 會是一個擁有某個型別 `Mode` 的值作為其結果的 I/O 動作的型別，但 `IOMode` 僅是一個簡單的列舉。

最後，它回傳一個將會以指定模式開啟指定檔案 I/O 動作。若是我們將這個動作綁定到某個東西上，我們就會得到一個 `Handle`。一個 `Handle` 型別的值代表我們檔案在哪裡。我們要使用這個 handle，以讓我們知道要讀取的是哪個檔案。讀取一個檔案、卻不將它綁定到一個 handle 是很愚蠢的，因為我們不能夠對這個檔案做任何事情。所以在我們的情況中，我們將這個 handle 綁定到 `handle`。

在下一行，我們看到一個叫做 <code class="label function">hGetContents</code> 的 function。它接收一個 `Handle`，所以它知道要取得內容的檔案是哪個、並回傳一個 `IO String`──一個持有檔案內容作為其結果的 I/O 動作。這個 function 十分像是 `getContents`。唯一的不同點在於，`getContents` 會自動地從標準輸入（即是從終端機）進行讀取，而 `hGetContents` 則是接收一個告訴它「要讀取的檔案是哪個」的檔案 handle。除此之外，它們的運作原理相同。就如同 `getContents`，`hGetContents` 不會試圖立即讀取檔案、並將它儲存在記憶體中，而是會在需要時讀取它。這實在很酷，因為我們可以把 `contents` 看作是檔案的完整內容，但它其實沒有被載入到記憶體中。所以若是這是個很大的檔案，執行 `hGetContents` 將不會阻塞我們的記憶體，而是只在它需要的時候、從檔案中讀取它需要的東西。

注意到用以識別檔案的 handle 與檔案內容的差別，它們在我們的程式中分別被綁定到 `handle` 與 `contents`。handle 僅是我們能藉此得知「我們的檔案是什麼」的東西。若是你將你的整個檔案系統想像成一本非常大的書，而每個檔案是書中的一章，handle 即是一個顯示你當前正在書中的哪一章讀取（或寫入）的書籤，而內容則是實際的章節。

我們使用 `putStr contents` 來將內容印到標準輸出，接著執行 <code class="label function">hClose</code>──它接收一個 handle，並回傳一個關閉檔案的 I/O 動作。你必須在以 `openFile` 開啟檔案之後，自己關閉它。

做到如此的另一種方式是使用 <code class="label function">withFile</code> function，其型別簽名為 `withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a`。它接收一個檔案的路徑、一個 `IOMode`，以及一個接收一個 handle 並回傳某個 I/O 動作的 function。它所回傳的是一個將會開啟檔案、做某些我們想要對檔案做的事、並關閉它的 I/O 動作。封裝在被回傳的最終 I/O 動作中的結果，與我們給它的 function 回傳的 I/O 動作相同。這聽起來可能有一點複雜，但它其實很簡單，尤其是 lambda。這裡有個我們使用 `withFile` 重寫過的先前的範例：

<pre name="code" class="haskell:hs">
import System.IO

main = do
    withFile "girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
</pre>

如你所見，它非常近似於先前那段程式碼。`(\handle -> ... )` 為接收一個 handle 並回傳一個 I/O 動作的 function，且它通常像這樣──以一個 lambda──來做到。它必須取一個回傳 I/O 動作的 function，而不是僅取一個 I/O 動作來操作、然後關閉檔案的理由，是因為我們傳遞給它的 I/O 動作不會知道要操作的是哪個檔案。藉由這種方式，`withFile` 會開啟檔案、然後將 handle 傳遞給我們給定的 function。它會從這個 function 取回一個 I/O 動作，然後建立一個如同於此的 I/O 動作，只是它會在之後關閉檔案。以下是我們能夠如何建立我們自己的 `withFile` function：

<pre name="code" class="haskell:hs">
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result
</pre>

<img src="img/edd.png" alt="butter toast" style="float:right" />
由於我們知道結果將會是一個 I/O 動作，所以我們可以僅以一個 <i>do</i> 開始。首先我們開啟檔案，並從其中取得一個 handle。然後，我們將 `handle` 應用到我們的 function 以取回完成所有工作的 I/O 動作。我們將這個動作綁定到 `result`、關閉 handle、然後執行 `return result`。藉由 `return` 封裝在我們從 `f` 取得的 I/O 動作中的結果，我們令我們的 I/O 動作封裝與我們從 `f handle` 取得的相同結果。所以若是 `f handle` 回傳一個將會從標準輸入讀取數行、將它寫入到一個檔案中、並封裝它讀取的行數作為結果的動作，假使我們以 `withFile'` 使用它，產生出來的 I/O 動作也會以讀取的行數作為結果。

就像是我們有個運作地像是 `getContents`、只不過是針對一個特定檔案的 `hGetContents`，我們也有 <code class="label function">hGetLine</code>、<code class="label function">hPutStr</code>、<code class="label function">hPutStrLn</code>、<code class="label function">hGetChar</code>、等等。它們就如同它們沒有 h 的對應版本般運作，只是它們都接收一個 handle 作為參數，並操作它指定的檔案，而不是操作在標準輸入或標準輸出上。例如：`putStrLn` 是一個接收一個字串，並回傳一個將會在終端機印出字串與其後換行的 I/O 動作。`hPutStrLn` 接收一個 handle 與一個字串，並回傳一個將會將這個字串寫入到關聯到 handle 的檔案、然後在其後擺一個換行的 I/O 動作。同理，`hGetLine` 接收一個 handle，並回傳一個從它的檔案讀取一行的 I/O 動作。

載入檔案、然後將它的內容視為字串是如此地常見，所以我們有這三個不錯的小 function 來讓我們的工作更加容易：

<code class="label function">readFile</code> 的型別簽名為 `readFile :: FilePath -> IO String`。記住，`FilePath` 僅是一個 `String` 的化名。`readFile` 接收一個檔案路徑，並回傳一個將會讀取這個檔案（當然，惰性地）、並將其內容作為字串綁定到某個東西上的 I/O 動作。這通常比執行 `openFile` 並將它綁定到一個 handle、然後執行 `hGetContents` 來得方便。以下是我們如何以 `readFile` 來寫下我們先前的範例：

<pre name="code" class="haskell:hs">
import System.IO

main = do
    contents <- readFile "girlfriend.txt"
    putStr contents
</pre>

因為我們沒有取得用以識別我們檔案的 handle，因此我們無法手動關閉它，所以當我們使用 `readFile` 的時候，Haskell 會為我們做這件事。

<code class="label function">writeFile</code> 的型別為 `writeFile :: FilePath -> String -> IO ()`。它取一個檔案路徑以及要寫到這個檔案的字串，並回傳一個將會進行寫入的 I/O 動作。若是這個檔案已經存在，它就會在寫入前被清空。以下即是如何將 <i>girlfriend.txt</i> 轉成大寫的版本，並將它寫入到 <i>girlfriendcaps.txt</i>：

<pre name="code" class="haskell:hs">
import System.IO
import Data.Char

main = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriendcaps.txt" (map toUpper contents)
</pre>

<pre name="code" class="plain">
$ runhaskell girlfriendtocaps.hs
$ cat girlfriendcaps.txt
HEY! HEY! YOU! YOU!
I DON'T LIKE YOUR GIRLFRIEND!
NO WAY! NO WAY!
I THINK YOU NEED A NEW ONE!
</pre>

<code class="label function">appendFile</code> 的型別簽名就如同 `writeFile`，不過若是檔案已經存在，`appendFile` 並不會將檔案清空，而是把東西附加上去。

讓我們假定我們有個每一行都有一個我們必須要做的任務的檔案 <i>todo.txt</i>。現在讓我們建立一個從標準輸入取一行、並將它加到待辦清單的程式。

<pre name="code" class="haskell:hs">
import System.IO

main = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")
</pre>

<pre name="code" class="plain">
$ runhaskell appendtodo.hs
Iron the dishes
$ runhaskell appendtodo.hs
Dust the dog
$ runhaskell appendtodo.hs
Take salad out of the oven
$ cat todo.txt
Iron the dishes
Dust the dog
Take salad out of the oven
</pre>

我們需要將 `"\n"` 加到每一行的結尾，因為 `getLine` 並不會給我們在結尾的換行字元。

喔，還有一件事。我們談過執行 `contents <- hGetContents handle` 不會導致整個檔案立即被讀取、並儲存在記憶體中。這即是惰性 I/O，所以這樣做：

<pre name="code" class="haskell:hs">
main = do
    withFile "something.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
</pre>

實際上就像是連接一條從檔案到輸出的管道（pipe）。就如同你可以將 list 想成串流（stream），你也可以將檔案想成串流。這將會一次讀取一行，並將它印到終端機上。所以你可能會問，那麼這條管子有多寬呢？硬碟多久會被存取一次呢？嗯，對於文字檔，預設的緩衝通常是行緩衝（line-buffering）。這代表檔案要被一次讀取的最小部分為一行。這就是為什麼在這種情況中，它實際上是讀取一行、將它印到輸出、讀取下一行、印出它、以此類推。對於二元（binary）檔，預設的緩衝通常是區塊緩衝（block-buffering）。這代表它會一個 chunk 接著一個 chunk 讀取檔案。chunk 大小是你的作業系統覺得不錯的大小。

你可以使用 `hSetBuffering` function 來控制緩衝究竟是如何完成的。它取一個 handle 與一個 `BufferMode`，並回傳一個將會設定緩衝的 I/O 動作。`BufferMode` 是一個簡單的列舉資料型別，它可以持有的可能值為：`NoBuffering`、`LineBuffering` 或 `BlockBuffering (Maybe Int)`。`Maybe Int` 是以位元組（bytes）為單位，代表 chunk 大小應該多大。若是它是 `Nothing`，則由作業系統決定 chunk 大小。`NoBuffering` 代表它將會一次讀取一個字元。`NoBuffering` 作為緩衝模式通常很爛，因為它必須多次存取硬碟。

以下是我們先前那段程式碼，只是它不是一行接著一行讀取，而是以 2048 個位元組的 chunk 讀取整個檔案。

<pre name="code" class="haskell:hs">
main = do
    withFile "something.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)
</pre>

若是我們要最小化硬碟存取、或是當我們的檔案實際上是個緩慢的網路資源時，以比較大的 chunk 讀取檔案是有幫助的。

我們也可以使用 <code class="label function">hFlush</code>，它是個接收一個 handle 並回傳一個將會刷新（flush）handle 關聯的檔案緩衝的 I/O 動作。當我們進行行緩衝時，緩衝會在每一行後刷新。當我們進行區塊緩衝時，它會在我們讀取一個 chunk 之後刷新。它在關閉一個 handle 之後也會被刷新。這代表當我們遇到一個換行字元時，讀取（或寫入）機制會回報到目前為止的所有資料。但我們可以使用 `hFlush` 來強制回報到目前為止已經被讀取的資料。刷新之後，資料對同時在執行的其它程式就是可用的了。

像這樣思考讀取一個區塊緩衝的檔案：你的馬桶會在它擁有一加崙的水之後自己沖水（flush）。所以你開始灌水，且一旦達到一加崙，這些水就會自動被沖掉，而迄今你灌在水中的資料就被讀取到了。但你也可以藉由按下馬桶上的按鈕來沖馬桶。這會令馬桶沖水，而所有在馬桶之中的水（資料）就被讀到了。如果你不曾注意到，手動沖馬桶是個 `hFlush` 的比喻。這以程式比喻的標準來看，並不是個非常好的類比，但我想要一個可以被沖水的現實世界物件作為妙語。

我們已經建立一個程式以將一個新項目加到我們在 <i>todo.txt</i> 中的待辦清單，現在讓我們建立一個程式來移除一個項目。我要貼上程式碼，然後我們要一同檢閱程式，因而你會看到它非常簡單。我們要使用一些來自 `System.Directory` 的新 function，以及來自 `System.IO` 的一個新 function，但它們全都會被解釋。

總而言之，以下是用以從 <i>todo.txt</i> 移除一個項目的程式：

<pre name="code" class="haskell:hs">
import System.IO
import System.Directory
import Data.List

main = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
</pre>

首先，我們開啟以讀取模式 <i>todo.txt</i>，並將它的 handle 綁定到 `handle`。

接下來，我們使用來自於 `System.IO`、我們先前未曾見過的 function──<code class="label function">openTempFile</code>。其名稱不言自明。它取一個暫存資料夾的路徑、與一個檔案的樣板（template）名稱，並開啟一個暫存檔。我們使用 `"."` 作為暫存資料夾，因為 `.` 在幾乎任何作業系統都代表當前的資料夾。我們使用 `"temp"` 作為暫存檔的樣板名稱，這意味著暫存檔案將會被命名為 <i>temp</i> 加上一些隨機字元。它回傳一個建立暫存檔的 I/O 動作，且在這個 I/O 動作中的結果是一對值：暫存檔的名稱與一個 handle。我們可以開啟一個叫做 <i>todo2.txt</i> 或是其它類似於此的名稱的普通檔案，但使用 `openTempFile` 以讓你知道你大概不會覆寫任何東西是比較好的習慣。

我們不使用 `getCurrentDirectory` 來取得當前資料夾、然後將它傳遞給 `openTempFile`，而是僅將 `"."` to `openTempFile` 的理由，是因為 `.` 在 unix-like 系統與 Windows 指的都是當前的資料夾。

接下來，我們將 <i>todo.txt</i> 的內容綁定到 `contents`。然後，將這個字串切割成字串的 list，一個字串代表一行。所以 `todoTasks` 現在是像 `["Iron the dishes", "Dust the dog", "Take salad out of the oven"]` 這樣的東西。我們以接收一個數字──像是 3、以及一個字串──像是 `"hey"`，並回傳 `"3 - hey"` 的 function，將從 0 開始的數字與這個 list 扣在一起，所以 `numberedTasks` 為 `["0 - Iron the dishes", "1 - Dust the dog" ...`。我們以 `unlines` 將這個字串 list 結合成一個以換行分隔的單一字串，並將這個字串印到終端機上。注意到，作為這樣做的替代，我們也可以執行 `mapM putStrLn numberedTasks`。

我們詢問使用者它想要刪除的是哪一個，並等待他輸入一個數字。讓我們假定他想要刪除 1 號，其為 `Dust the dog`，所以他輸入 1。`numberString` 現在是 `"1"`，且因為我們想要一個數字、而不是個字串，所以我們對它執行 `read` 以得到 `1`，並將它綁定到 `number`。

記得來自於 `Data.List` 的 `delete` 與 `!!` function。`!!` 回傳一個 list 中帶有某個索引值的元素；`delete` 刪除在 list 中的一個元素的首次出現之處，並回傳沒有這個元素的新 list。`(todoTasks !! number)`（`number` 目前是 `1`）回傳 `"Dust the dog"`。我們將去除 `"Dust the dog"` 第一次出現之處的 `todoTasks` 綁定到 `newTodoItems`，然後在將它寫入我們開啟的暫存檔之前，將它以 `unlines` 結合成一個單一字串。舊的檔案當前未被改變，而暫存檔包含舊的檔案包含的每一行，除了我們刪掉的那行以外。

在我們關閉原始檔案與暫存檔案之後，我們以 <code class="label function">removeFile</code>──如你所見，它接收一個檔案路徑，並移除它──刪除原始的檔案。在刪除舊的 <i>todo.txt</i> 之後，我們使用 <code class="label function">renameFile</code> 以將暫存檔重新命名為 <i>todo.txt</i>。要小心，`removeFile` 與 `renameFile`（順帶一提，它們都在 `System.Directory` 中）是取檔案路徑作為其參數，而非 handle。

僅此而已！我們可以用更少行做到這件事，但我們要非常小心不要覆寫任何存在的檔案，並禮貌地請求作業系統告訴我們可以在哪裡擺我們的暫存檔。讓我們試看看吧！

<pre name="code" class="plain">
$ runhaskell deletetodo.hs
These are your TO-DO items:
0 - Iron the dishes
1 - Dust the dog
2 - Take salad out of the oven
Which one do you want to delete?
1

$ cat todo.txt
Iron the dishes
Take salad out of the oven

$ runhaskell deletetodo.hs
These are your TO-DO items:
0 - Iron the dishes
1 - Take salad out of the oven
Which one do you want to delete?
0

$ cat todo.txt
Take salad out of the oven
</pre>

## <a name="command-line-arguments">命令列引數</a>

<img src="img/arguments.png" alt="COMMAND LINE ARGUMENTS!!! ARGH" style="float:right" />
若是你想要建立一個執行在終端機上的腳本或應用程式，處理命令列引數（command line argument）是十分必要的。幸運地，Haskell 的標準函式庫有個取得程式命令列引數的好方法。

在前一節，我們建立了一個用以將待辦事項加到我們的待辦清單中的程式、以及一個用以刪除待辦事項的程式。我們採取的方式有兩個問題。第一個是在我們的程式碼中，我們將待辦清單的檔案名稱寫死了。我們僅決定檔案將會被命名為 <i>todo.txt</i>，且使用者永遠不會有處理多個待辦清單的需求。

一種解決這個問題的方式，是每次都詢問使用者他們想要用來當做待辦清單的是哪個檔案。這行得通，但不是非常好，因為它要求使用者去執行程式、等待程式詢問、然後才將之告訴程式。這被稱為一個互動式程式，而互動式命令列程式的困難之處在於──若是你想要自動執行這個程式，像是用一個批次（batch）腳本呢？建立一個與一個程式互動的批次腳本，是比建立一個僅呼叫一個或多個程式的批次腳本還困難的。

這就是為什麼讓使用者在他們執行程式的時候，告訴程式他們要的是什麼、而不是讓程式在執行時詢問使用者，有時候是比較好的。要讓使用者在他們執行程式的時候，告訴它他們想要它去做什麼，有什麼比透過命令列引數更好的方法呢！

`System.Environment` 模組有兩個很酷的 I/O 動作。一個是 <code class="label function">getArgs</code>，它的型別為 `getArgs :: IO [String]`，並且是個將會取得被執行程式的引數、並以帶著引數的 list 作為其包含結果的 I/O 動作。<code class="label function">getProgName</code> 型別為 `getProgName :: IO String`，且是個包含程式名稱的 I/O 動作。

這裡有個示範它們兩個如何運作的小程式：

<pre name="code" class="haskell:hs">
import System.Environment
import Data.List

main = do
   args <- getArgs
   progName <- getProgName
   putStrLn "The arguments are:"
   mapM putStrLn args
   putStrLn "The program name is:"
   putStrLn progName
</pre>

我們將 `getArgs` 與 `progName` 綁定到 `args` 與 `progName` 上。我們印出 `The arguments are:`，然後我們對每個在 `args` 中的引數執行 `putStrLn`。最後，我們也印出程式名稱。讓我們將它編譯成 `arg-test`。

<pre name="code" class="plain">
$ ./arg-test first second w00t "multi word arg"
The arguments are:
first
second
w00t
multi word arg
The program name is:
arg-test
</pre>

很好。有了這些知識，你就可以創造一些很酷的命令列應用程式。事實上，就讓我們繼續前進、並建立一個吧。在前一節，我們為了新增任務建立了一個獨立的程式，並為了刪除它們建立了一個獨立的程式。現在，我們要將之結合成一個程式，而它所做的將會視命令列引數而定。我們也要讓它可以操作在不同的檔案上，而不是僅有 <i>todo.txt</i>。

我們要簡單地稱它為 <i>todo</i>，且它將能夠去做（哈哈！<span class="note">〔譯註：「去做」即為 to do，作者大概覺得這很幽默。〕</span>）三件不同的事：

* 檢視任務
* 新增任務
* 刪除任務

我們當前並不打算過於關心可能的不良輸入。

我們的程式使得，若是我們想要加入 `Find the magic sword of power` 這個任務到檔案 <i>todo.txt</i> 中時，我們必須在我們的終端機中敲入 `todo add todo.txt "Find the magic sword of power"`。為了檢視任務，我們要執行 `todo view todo.txt`；而為了刪除索引值 2 的任務，我們要執行 `todo remove todo.txt 2`。

我們將藉由建立一個 dispatch 關連列表開始。它會是一個命令列引數作為 key、function 作為它們對應的 value 的簡單的關聯列表。所有 function 的型別將會是 `[String] -> IO ()`。它們會取引數 list 作為參數，並回傳一個進行檢視、新增、刪除等等的 I/O 動作。

<pre name="code" class="haskell:hs">
import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            ]
</pre>

我們尚未定義 `main`、`add`、`view` 與 `remove`，所以讓我們以 `main` 開始：

<pre name="code" class="haskell:hs">
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args
</pre>

首先，我們取得引數、並將它們綁定到 `(command:args)`。若是你還記得模式匹配，這意謂著第一個引數將會被綁定到 `command`，而剩下的部份會被綁定到 `args`。若是我們像 `todo add todo.txt "Spank the monkey"` 這樣呼叫我們的程式，`command` 將會是 `"add"`，而 `args` 將會是 `["todo.xt", "Spank the monkey"]`。

在下一行，我們在 dispatch list 中尋找我們的命令。因為 `"add"` 指到 `add`，所以我們取得 `Just add` 作為結果。我們再次使用模式匹配，以從 `Maybe` 取出我們的 function。若是我們的命令不在 dispatch list 中會發生什麼呢？那麼 `lookup` 將會回傳 `Nothing`，但我們表明我們不會太過關心失敗的情況，所以模式匹配將會失敗，而我們的程式就會鬧彆扭。

最後，我們以引數 list 的剩餘部分呼叫我們的 `action` function。這將會回傳一個新增一個項目、顯示一個項目清單、或是刪除一個項目的 I/O 動作，且因為這個動作是 `main` <i>do</i> block 的一部分，所以它將會被執行。若是我們遵循我們到目前為止的具體例子，且我們的 `action` function 為 `add`，它將會以 `args`（即 `["todo.txt", "Spank the monkey"]`）呼叫，並回傳一個將 `Spank the monkey` 新增到 <i>todo.txt</i> 的 I/O 動作。

很好！現在所剩下的，就是去實作 `add`、`view` 與 `remove`。讓我們以 `add` 開始：

<pre name="code" class="haskell:hs">
add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
</pre>

若是我們像 `todo add todo.txt "Spank the monkey"` 這樣呼叫我們的程式，`"add"` 將會在 `main` 區塊中的第一個模式匹配中被綁定到 `command`，而 `["todo.txt", "Spank the monkey"]` 將會被傳遞到我們從 dispatch list 取出的 function 中。因此，由於我們現在並不處理不良輸入，所以我們就立即以這兩個元素針對 list 進行模式匹配，並回傳一個將這行附加到檔案結尾、帶著一個換行字元的 I/O 動作。

接著，讓我們實作清單檢視功能。若是我們想要檢視一個檔案中的項目，我們就會執行 `todo view todo.txt`。所以在第一個模式匹配中，`command` 將會是 `"view"`，而 `args` 將會是 `["todo.txt"]`。

<pre name="code" class="haskell:hs">
view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks
</pre>

在僅有刪除任務的程式中，我們在顯示任務以便使用者可以選擇一個來刪除的時候，已經做了幾乎一樣的事情，只是在這裡我們僅顯示任務。

最後，我們要實作 `remove`。這會十分相似於僅有刪除任務的程式，所以若是你不瞭解在這裡刪除一個項目是如何運作的，就看看這個程式的解釋吧。主要的不同在於，我們並不把 <i>todo.txt</i> 寫死，而是作為一個引數來取得它。我們也不會提示使用者要刪除的任務編號，我們會作為引數來取得它。

<pre name="code" class="haskell:hs">
remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
</pre>

我們基於 `fileName` 開啟檔案，並開啟一個暫存檔案、刪除使用者想要刪除的索引值的那行、將之寫入到暫存檔、移除原始檔案、並將暫存檔重新命名回 `fileName`。

以下即是這個光彩奪目的完整程式！

<pre name="code" class="haskell:hs">
import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            ]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
</pre>

<img src="img/salad.png" alt="fresh baked salad" style="float:left" />
總結我們的解法：我們建立一個從命令映射到接收某些命令列引數、並回傳一個 I/O 動作的 function 的 dispatch 關聯。我們發現命令為何，並基於此、從 dispatch list 取得適合的 function。我們以剩下的命令列引數呼叫這個 function，並取回一個將會做適當操作的 I/O 動作，然後就執行這個動作！

在其他語言中，我們也許會以一個很大的 switch case 敘述或什麼的來實作它，但使用高階函數則允許我們要求 dispatch list 給我們合適的 function，然後要求這個 function 對於某些命令列引數給我們一個 I/O 動作。

讓我們試試我們的應用程式！

<pre name="code" class="plain">
$ ./todo view todo.txt
0 - Iron the dishes
1 - Dust the dog
2 - Take salad out of the oven

$ ./todo add todo.txt "Pick up children from drycleaners"

$ ./todo view todo.txt
0 - Iron the dishes
1 - Dust the dog
2 - Take salad out of the oven
3 - Pick up children from drycleaners

$ ./todo remove todo.txt 2

$ ./todo view todo.txt
0 - Iron the dishes
1 - Dust the dog
2 - Pick up children from drycleaners
</pre>

另一個關於此的酷事是，加入額外功能是很容易的。只要在 dispatch 關聯列表中加入一個進入點（entry），並實作對應的 function！作為練習，你可以試著實作一個 `bump` function，其將會接收一個檔案與一個任務編號，並回傳一個把這個任務擠到待辦清單頂端的 I/O 動作。

你也可以藉由建立一個僅回報有個錯誤的 I/O 動作（假定為 `errorExit :: IO ()`）、然後檢查可能的錯誤輸入、若是有錯誤輸入，便執行錯誤回報的 I/O 動作，讓這個程式在不良輸入的情況中（舉例來說，若是某個人執行 `todo UP YOURS HAHAHAHA`）失敗得更優雅一點。另一種方式是使用例外，我們不久就會遇到它。

## <a name="randomness">隨機性</a>

<img src="img/random.png" alt="this picture is the ultimate source of randomness and wackiness" style="float:right" />
很多時候，你都需要在寫程式的時候取得一些隨機資料。或許你正在寫一個需要擲骰子的遊戲，或者你需要產生一些測試資料來測試你的程式。程式撰寫的時候有許多隨機資料的用途。嗯，事實上，是假隨機（pseudo-random），因為我們所知道的唯一真實的隨機性（randomness）來源是在一輛獨輪車上、一隻手拿著起司、另一隻手抓著屁股的一隻猴子上<span class="note">〔譯註：[無限猴子定理](http://zh.wikipedia.org/wiki/%E7%84%A1%E9%99%90%E7%8C%B4%E5%AD%90%E5%AE%9A%E7%90%86)〕</span>。在這一節，我們要看看如何使 Haskell 產生看似隨機的資料。

在許多其它程式語言中，你會有個給予你某個隨機數的 function。每當你呼叫這個 function，你就取回一個（但願是）不同的隨機數。Haskell 又如何呢？嗯，還記得，Haskell 是個純函數式語言。這所代表的是它擁有參考透明性。這所代表的是，若是給予相同的參數兩次，一個 function 必須產生相同的結果兩次。這確實很酷，因為它允許我們以不同方式思考程式，且它讓我們延緩求值、直到我們真的需要它為止。若是我呼叫一個 function，我可以確定它不會在給我結果前做任何有趣的事情。重要的只有它的結果。然而，這使得它在取得隨機數時有些棘手。若是我有個像這樣的 function：

<pre name="code" class="haskell:hs">
randomNumber :: (Num a) => a
randomNumber = 4
</pre>

這並不如一個隨機數 function 一般地有用，因為它將總是回傳 `4`，即使我能夠向你保證：因為我使用一個骰子來決定這個數字，所以這個 4 是完全隨機的。

其它語言是如何建立看似隨機的數字呢？嗯，它會從你的電腦取得不同的資訊，像是當前時間、你的滑鼠移動的多寡與位置、以及你在你的電腦後面製造出來的是哪種噪音，並基於此給予一個看起來非常隨機的數字。這些因子（隨機性）的組合可能在任何特定時刻都不同，所以你就得到一個不同的隨機數。

阿。所以在 Haskell 中，若是我們建立一個取隨機性作為其參數、並基於此回傳某個數字（或是其他資料型別）的 function，我們就可以產生一個隨機數了。

進入到 `System.Random` 模組。它擁有滿足我們為了隨機性所需的所有 function。讓我們來研究它輸出的其中一個 function，即 <code class="label function">random</code>。這是它的型別：`random :: (RandomGen g, Random a) => g -> (a, g)`。哇！在這裡的這個型別宣告中有一些新的 typeclass！<code class="label class">RandomGen</code> typeclass 代表可以作為隨機性來源的型別。<code class="label class">Random</code> typeclass 代表可以具有隨機值的東西。一個布林值可以具有一個隨機值，即 `True` 或 `False`。一個數字也可以有許多不同的隨機值。一個 function 可以具有一個隨機值嗎？我不這麼認為，或許不行！若是我們試著將 `random` 的型別宣告翻譯成中文，我們得到的結果就像是：它接收一個隨機產生器（即是我們的隨機性來源），並回傳一個隨機值與一個新的隨機產生器。為什麼它不但回傳一個隨機值，而且還會回傳一個新的產生器呢？嗯，我們待會就會看到。

為了要使用我們的 `random` function，我們必須得到其中一個隨機產生器。`System.Random` 模組輸出一個很酷的型別，即 <code class="label type">StdGen</code>，其為一個 `RandomGen` typeclass 的實體。我們可以手動建立一個 `StdGen`，或者我們可以要求系統給我們一個基於多種隨機因子的 `StdGen`。

要手動建立一個隨機產生器，就使用 <code class="label function">mkStdGen</code> function 吧。它的型別為 `mkStdGen :: Int -> StdGen`。它取一個整數，並基於此給予我們一個隨機產生器。好，那麼讓我們串聯使用 `random` 與 `mkStdGen` 來取得一個（幾乎不隨機的）數字。

<pre name="code" class="haskell:ghci">
ghci> random (mkStdGen 100)
</pre>

<pre name="code" class="plain">
&lt;interactive&gt;:1:0:
    Ambiguous type variable `a' in the constraint:
      `Random a' arising from a use of `random' at &lt;interactive&gt;:1:0-20
    Probable fix: add a type signature that fixes these type variable(s)
</pre>

這是什麼？阿，對，`random` function 可以回傳任何為 `Random` typeclass 一員的型別的值，所以我們必須告訴 Haskell 我們想要的是哪種型別。也別忘了它回傳的是一對 pair 中的一個回傳值與一個隨機產生器。

<pre name="code" class="haskell:ghci">
ghci> random (mkStdGen 100) :: (Int, StdGen)
(-1352021624,651872571 1655838864)
</pre>

終於！一個看似隨機的數字！tuple 的第一項是我們的數字，而第二項我們新的隨機產生器的文字表示。若是我們再次以相同的隨機產生器呼叫 `random` 會發生什麼呢？

<pre name="code" class="haskell:ghci">
ghci> random (mkStdGen 100) :: (Int, StdGen)
(-1352021624,651872571 1655838864)
</pre>

當然。以相同的參數得到相同的結果。所以，讓我們試著給它一個不同的隨機產生器作為參數。

<pre name="code" class="haskell:ghci">
ghci> random (mkStdGen 949494) :: (Int, StdGen)
(539963926,466647808 1655838864)
</pre>

很好，很酷，太好了，一個不同的數字。我們可以使用型別註釋來從這個 function 取回不同型別的值。

<pre name="code" class="haskell:ghci">
ghci> random (mkStdGen 949488) :: (Float, StdGen)
(0.8938442,1597344447 1655838864)
ghci> random (mkStdGen 949488) :: (Bool, StdGen)
(False,1485632275 40692)
ghci> random (mkStdGen 949488) :: (Integer, StdGen)
(1691547873,1597344447 1655838864)
</pre>

讓我們建立一個模擬丟三次硬幣的 function。若是 `random` 不與隨機值一起回傳一個新的產生器，我們就必須令這個 function 接收三個隨機產生器作為參數，然後回傳每一個產生器的硬幣投擲結果。但這聽起來是錯的，因為若是一個產生器可以建立一個 `Int` 型別（其可以具有大量不同的值）的隨機值，它就應該能夠產生三個擲硬幣的結果（其恰好具有八種組合）。所以這就是 `random` 與值一同回傳一個新的產生器確實派上用場之處了。

我們要以一個簡單的 `Bool` 來表示一個硬幣。`True` 為反面，`False` 為正面。

<pre name="code" class="haskell:hs">
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)
</pre>

我們以我們得到的產生器作為參數呼叫 `random`，以得到一枚硬幣與一個新的產生器。然後我們再次呼叫它──只是這時是以新的產生器呼叫──以得到第二枚硬幣。對於第三枚硬幣亦同。假使我們每次都以相同的產生器呼叫它，所有的硬幣都會有相同的值，而我們只能夠得到 `(False, False, False)` 或是 `(True, True, True)` 作為結果。

<pre name="code" class="haskell:ghci">
ghci> threeCoins (mkStdGen 21)
(True,True,True)
ghci> threeCoins (mkStdGen 22)
(True,False,True)
ghci> threeCoins (mkStdGen 943)
(True,False,True)
ghci> threeCoins (mkStdGen 944)
(True,True,True)
</pre>

注意到我們不必去做 `random gen :: (Bool, StdGen)`。這是因為我們已經在 function 的型別宣告中表明我們想要布林值。這就是為什麼在這種情況中，Haskell 能夠推論我們想要一個布林值。

所以若是我們想要擲四個硬幣呢？或是五個？嗯，有個叫做 <code class="label function">randoms</code> 的 function，其接收一個產生器並基於這個產生器回傳一個無窮的值的序列。

<pre name="code" class="haskell:ghci">
ghci> take 5 $ randoms (mkStdGen 11) :: [Int]
[-1807975507,545074951,-1015194702,-1622477312,-502893664]
ghci> take 5 $ randoms (mkStdGen 11) :: [Bool]
[True,True,True,True,False]
ghci> take 5 $ randoms (mkStdGen 11) :: [Float]
[7.904789e-2,0.62691015,0.26363158,0.12223756,0.38291094]
</pre>

為什麼 `randoms` 不跟著 list 一起回傳一個新產生器呢？我們可以像這樣十分輕易地實作 `randoms` function：

<pre name="code" class="haskell:hs">
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen
</pre>

一個遞迴定義。我們從當前的產生器取得一個隨機值與一個新的產生器，然後建立一個以這個值作為它的 head、以基於新的產生器的隨機數字作為它的 tail 的 list。因為我們有可能需要能夠產生無限個數字，所以我們無法丟回新的隨機產生器。

我們可以像這樣建立一個產生有限的數字串流以及一個新的產生器的 function：

<pre name="code" class="haskell:hs">
finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in  (value:restOfList, finalGen)
</pre>

再一次的，一個遞迴定義。我們表明，若是我們想要 0 個數字，我們就回傳一個空的 list 和我們給予的產生器。對於其它任何數量的隨機值，我們首先取得一個隨機數與一個新的產生器。這將會是 head。然後我們表明，tail 將會是以新產生器產生的 <i>n - 1</i> 個數字。接著我們回傳 head 接上 list 的剩餘部分、以及我們從 <i>n - 1</i> 個隨機數得到的最後的產生器。

若是我們想要一個在某個範圍中的隨機值呢？到目前為止的所有隨機整數不是很大就是很小。若是我們想要丟一顆骰子呢？嗯，我們為了這個目的而使用 <code class="label function">randomR</code>。它的型別為 `randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)`，代表它有點像是 `random`，只是它取一對設定上限與下限的值的 pair 作為它的第一個參數，而產生的最終值將會落在這個範圍中。

<pre name="code" class="haskell:ghci">
ghci> randomR (1,6) (mkStdGen 359353)
(6,1494289578 40692)
ghci> randomR (1,6) (mkStdGen 35935335)
(3,1250031057 40692)
</pre>

還有個 <code class="label function">randomRs</code>，其產生一個在我們定義區間中的隨機值串流。看看吧：

<pre name="code" class="haskell:ghci">
ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]
"ndkxbvmomg"
</pre>

很好，看起來像是一個超級機密的密碼之類的。

或許你會自問，這一節有什麼無論如何都必須以 I/O 來做的嗎？我們迄今都還沒做任何有關 I/O 的事。嗯，到目前為止我們總是以某個隨意的整數來手動建立我們的隨機數產生器。問題是，若是我們在我們的真實程式這樣做，它們將總是回傳相同的隨機數，這對我們來說並不是很好。這即是 `System.Random` 為什麼要提供 <code class="label function">getStdGen</code> I/O 動作，其型別為 `IO StdGen`。當你的程式開始時，它會跟系統要一個好的隨機數產生器，並將它儲存在一個所謂的全域（global）產生器中。`getStdGen` 會在你將它綁定到某個東西上的時候，替你取得全域隨機產生器。

以下是個產生隨機字串的簡單程式：

<pre name="code" class="haskell:hs">
import System.Random

main = do
    gen <- getStdGen
    putStr $ take 20 (randomRs ('a','z') gen)
</pre>

<pre name="code" class="plain">
$ runhaskell random_string.hs
pybphhzzhuepknbykxhe
$ runhaskell random_string.hs
eiqgcxykivpudlsvvjpg
$ runhaskell random_string.hs
nzdceoconysdgcyqjruo
$ runhaskell random_string.hs
bakzhnnuzrkgvesqplrx
</pre>

不過要小心，將 `getStdGen` 執行兩次將會跟系統要兩次相同的全域產生器。若是你這樣做：

<pre name="code" class="haskell:hs">
import System.Random

main = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a','z') gen)
    gen2 <- getStdGen
    putStr $ take 20 (randomRs ('a','z') gen2)
</pre>

你會得到兩次被印出來的相同字串。一種取得兩個長度 20 的不同字串的方式，是建立一個無限串流、然後取前 20 個字元，在一行中將它們印出來、然後取第二組 20 個字元，再在第二行中將它們印出來。對此，我們可以使用 `Data.List` 的 `splitAt` function，其在某個索引值上切割一個 list，並回傳一個第一部分作為第一項、第二部分作為第二項的 tuple。

<pre name="code" class="haskell:hs">
import System.Random
import Data.List

main = do
    gen <- getStdGen
    let randomChars = randomRs ('a','z') gen
        (first20, rest) = splitAt 20 randomChars
        (second20, _) = splitAt 20 rest
    putStrLn first20
    putStr second20
</pre>

另一種方式是使用 <code class="label function">newStdGen</code> 動作，其將我們當前的隨機產生器切成兩個產生器。它以其中一個更新全域隨機產生器，並將另一個封裝成它的結果。

<pre name="code" class="haskell:hs">
import System.Random

main = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a','z') gen)
    gen' <- newStdGen
    putStr $ take 20 (randomRs ('a','z') gen')
</pre>

當我們將 `newStdGen` 綁定到某個東西上時，不僅我們取得了一個新的隨機產生器、全域的產生器也被更新了，所以若是我們再次執行 `getStdGen` 並將它綁定到某個東西上，我們會得到一個與 `gen` 不同的產生器。

這裡有個小程式，會讓使用者猜猜它想的數字是什麼。

<pre name="code" class="haskell:hs">
import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
        askForNumber newGen
</pre>

<img src="img/jackofdiamonds.png" alt="jack of diamonds" style="float:left" />
我們建立一個 `askForNumber` function，其取一個隨機數產生器，並回傳一個將會提示使用者輸入數字、並告知他猜的數字是否正確的 I/O 動作。在這個 function 中，我們先基於我們作為參數取得的產生器，產生一個隨機數與一個新的產生器，並將它們叫做 `randNumber` 與 `newGen`。讓我們假定產生出來的數字為 `7`。這時我們請使用者猜猜我們想的數字是什麼。我們執行 `getLine` 並將其結果綁定到 `numberString`。當使用者輸入 `7`，`numberString` 就變成了 `"7"`。接著，我們使用 `when` 來檢查使用者輸入的字串是否為空字串。若是，一個 `return ()` 的空 I/O 動作會被執行，這實際上就是結束程式。若否，那裡的 <i>do</i> 區塊組成的動作就會被執行。我們對 `numberString` 使用 `read` 以將它轉成數字，所以 `number` 當前為 `7`。

<p class="hint">
<em>對不起！</em>若是這裡使用者給了我們 <code>read</code> 無法讀取的輸入（像是 <code>"haha"</code>），我們的程式將會帶著一個醜陋的錯誤訊息崩潰。若是你不想要你的程式在錯誤輸入時崩潰，就使用 <code class="label function">reads</code> 吧，它會在它字串讀取失敗的時候回傳一個空的 list。當它成功的時候，它會回傳一個帶著一個 tuple 的單一元素 list，tuple 以我們所需的值作為一項、以它無法讀取的字串作為另一項<span class="note">〔譯註：所以執行 <code>reads "123abc456" :: [(Int, String)]</code> 得到的結果為 <code>[(123, "abc456")]</code>〕</span>。
</p>

我們檢查我們輸入的數字是否與隨機產生的數字相等，並給予使用者合適的訊息。接著我們遞迴地呼叫 `askForNumber`，只是這時是以我們得到的新產生器呼叫，其給予我們一個就像是我們執行的 I/O 動作，只是它取決於一個不同的產生器，接著我們執行它。

`main` 僅以「從系統得到的一個隨機產生器，並以它來呼叫 `askForNumber` 以得到初始動作」組成。

以下是我們程式的運作狀況！

<pre name="code" class="plain">
$ runhaskell guess_the_number.hs
Which number in the range from 1 to 10 am I thinking of? 4
Sorry, it was 3
Which number in the range from 1 to 10 am I thinking of? 10
You are correct!
Which number in the range from 1 to 10 am I thinking of? 2
Sorry, it was 4
Which number in the range from 1 to 10 am I thinking of? 5
Sorry, it was 10
Which number in the range from 1 to 10 am I thinking of?
</pre>

另一種建立相同程式的方式就像這樣：

<pre name="code" class="haskell:hs">
import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
        newStdGen
        main
</pre>

這非常相似於先前的版本，只是我們並非是建立一個接收一個產生器、然後以更新的產生器遞迴地呼叫自身的 function，而是在 `main` 中完成所有的工作。在告訴使用者他們的猜測是否正確之後，我們就更新全域產生器，然後再一次呼叫 `main`。兩種方式都是合法的，但我更喜歡第一種，因為它在 `main` 中做的事比較少，也提供給我們一個可以輕易重用的 function。

## <a name="bytestrings">Bytestrings</a>

<img src="img/chainchomp.png" alt="like normal string, only they byte ... what a pedestrian pun this is" style="float:right" />
list 是個很酷也很有用的資料結構。到目前為止，我們已經幾乎無處不用它。這裡有許多操作它們的 function，且 Haskell 的惰性允許我們將其他語言的 for 與 while 迴圈替換成對 list 的過濾與映射。因為求值只會在它真的需要的時候才會發生，所以像是無限 list（甚至是無限 list 的無限 list！）這類東西，對我們來說都不成問題。這就是為什麼 list 也可以在從標準輸入讀取、或是從檔案進行讀取的時候，被用來表示串流。我們可以開啟一個檔案，並將它讀取為字串，即使它只有在出現需求的時候才會被存取。

然而，將檔案作為字串處理有一個缺點：它往往很慢。如你所知，`String` 是個 `[Char]` 的型別同義詞。`Char` 沒有固定的大小，因為它會取數個位元組來表示一個──比如說，Unicode 的──字元。此外，list 是十分惰性的。若是你有個像 `[1,2,3,4]` 這樣的 list，它將只在非常必要的時候才會被求值。所以整個 list 就像是一個「一個 list」的承諾。還記得 `[1,2,3,4]` 是個 `1:2:3:4:[]` 的語法糖衣。當 list 的第一個元素被強迫求值（比如說印出它），list 的其餘部分 `2:3:4:[]` 仍然只是個「一個 list」的承諾，以此類推。所以你可以將 list 想成下一個元素將會在真的必要時被釋出、並帶著其後元素承諾的承諾。不難推論，將一個簡單的數字 list 作為一系列的承諾處理，可能並不是很有效率。

大多時候這種開銷並不會太讓我們困擾，但在讀取大檔案並操作它們的時候，它就是一個不利條件了。這就是為什麼 Haskell 會有 *bytestring*。bytestring 有點像是 list，但每個元素大小都是一個位元組（或 8 位元〈bit〉）。它們處理惰性的方式也不同。

bytestring 有兩種類型：strict 與 lazy。strict bytestring 屬於 `Data.ByteString`，且它完全廢除惰性。沒有牽涉在內的承諾；一個 strict bytestring 表示在一個陣列（array）中的一系列位元組。你無法有個像是無限 strict bytestring 這樣的東西。若是你對一個 strict bytestring 的第一個位元組求值，你就必須對整個 bytestring 求值。好的一面是開銷比較小，因為沒有涉及在內的 thunk（<i>承諾</i> 的術語）。壞的一面是它可能會快速塞滿你的記憶體，因為它被一次讀取到記憶體中。

另一種 bytestring 類型屬於 `Data.ByteString.Lazy`。它是惰性的，但不若 list 惰性得那般徹底。就像是我們先前說的，一個 list 中有多少個元素，就有多少個 thunk。這就是讓它對於某些用途有點慢的原因。lazy bytestring 採取不同的方法──它被儲存在 chunk（別跟 thunk 搞混了！）中，每個 chunk 的大小為 64K。所以若是我們對一個 lazy bytestring 中的一個位元組求值（印出它之類的），第一個 64K 就會被求值。在這之後，它僅是一個其餘 chunk 的承諾。lazy bytestring 有點像是大小為 64K 的 strict bytestring 的 list。當你以 lazy bytestring 處理一個檔案時，它就會一個 chunk 接著一個 chunk 被讀取。這很棒，因為它不會導致記憶體使用量飆高，且 64K 大概恰好適合裝入你 CPU 的 L2 快取中。

如果你看過 `Data.ByteString.Lazy` 的[文件](http://www.haskell.org/ghc/docs/latest/html/libraries/bytestring/Data-ByteString-Lazy.html)，你會看到它有很多與 `Data.List` 中的 function 同名的 function，只是型別簽名中的是 `ByteString` 而不是 `[a]`、是 `Word8` 而不是 `a`。具有相同名稱的 function 大多與運作在 list 上的 function 的行為相同。因為名稱是相同的，所以我們要在一個腳本中進行限制引入，然後將這個腳本載入到 GHCI 中以試試 bytestring。

<pre name="code" class="haskell:hs">
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
</pre>

`B` 擁有 lazy bytestring 的型別與 function，而 `S` 擁有 strict 的型別與 function。我們大多會使用 lazy 版本。

<code class="label function">pack</code> 這個 function 的型別簽名為 `pack :: [Word8] -> ByteString`。這所代表的是，它取一個 `Word8` 型別的位元組的 list，並回傳一個 `ByteString`。你可以將它想成接收一個 lazy 的 list，並讓它比較不 lazy 一點，以讓它只有在 64K 的間隔上是 lazy 的。

這個 `Word8` 型別是什麼呢？嗯，它就像是 `Int`，只是它的範圍比較小，即 0-255。它表示一個 8 位元的數字。就如同 `Int`，它也在 `Num` typeclass 之中。舉例來說，我們知道 `5` 是多型的，它可以作為任何數值型別操作。嗯，它也可以看作 `Word8` 型別。

<pre name="code" class="haskell:ghci">
ghci> B.pack [99,97,110]
Chunk "can" Empty
ghci> B.pack [98..120]
Chunk "bcdefghijklmnopqrstuvwx" Empty
</pre>

如你所見，你通常不必太過擔心 `Word8`，因為型別系統可以讓數字選擇這個型別。若是你試著將一個很大的數字──像是 `336`──用作一個 `Word8`，它就會被包成 `80`<span class="note">〔譯註：因為發生溢位〈overflow〉，所以 `336` 捨去溢出的位元，就變成 `80` 了〕</span>。

我們只將一點點值包裝成一個 `ByteString`，所以它塞得進一個 chunk 中。`Empty` 就像是對於 list 的 `[]`。

<code class="label function">unpack</code> 為 `pack` 的反函數。它取一個 bytestring 並將它轉成一個位元組的 list。

<code class="label function">fromChunks</code> 接收一個 strict bytestring 的 list，並將它轉成一個 lazy bytestring。<code class="label function">toChunks</code> 接收一個 lazy bytestring 的 list，並將它轉成一個 strict bytestring。

<pre name="code" class="haskell:ghci">
ghci> B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]
Chunk "()*" (Chunk "+,-" (Chunk "./0" Empty))
</pre>

若是你有許多很小的 strict bytestring，且你想要有效率地處理它們，但不想先將它們在記憶體中結合成一個大的 strict bytestring 的話，這樣是很好的。

bytestring 版本的 `:` 被叫做 <code class="label function">cons</code>。它取一個位元組與一個 bytestring，並將這個位元組擺在開頭。但它是惰性的，所以即便 bytestring 中的第一個 chunk 沒滿，它也會建立一個新的 chunk。這就是為什麼若是你要在一個 bytestring 的開頭插入很多位元組的時候，使用 strict 版本的 `cons`──<code class="label function">cons'</code>──是比較好的。

<pre name="code" class="haskell:ghci">
ghci> B.cons 85 $ B.pack [80,81,82,84]
Chunk "U" (Chunk "PQRT" Empty)
ghci> B.cons' 85 $ B.pack [80,81,82,84]
Chunk "UPQRT" Empty
ghci> foldr B.cons B.empty [50..60]
Chunk "2" (Chunk "3" (Chunk "4" (Chunk "5" (Chunk "6" (Chunk "7" (Chunk "8" (Chunk "9" (Chunk ":" (Chunk ";" (Chunk "<"
Empty))))))))))
ghci> foldr B.cons' B.empty [50..60]
Chunk "23456789:;<" Empty
</pre>

如你所看到的，<code class="label function">empty</code> 建立了一個空的 bytestring。看到 `cons` 與 `cons'` 之間的不同了嗎？藉著 `foldr`，我們已一個空的 bytestring 開始，然後從右邊走遍數字的 list、將每個數字加到 bytestring 的開頭。當我們使用 `cons` 時，最後會是每個位元組一個 chunk，這有點違背了目的。

除此之外，bytestring 模組有許多近似於 `Data.List` 中的 function 的 function，包含、但不限於 `head`、`tail`、`init`、`null`、`length`、`map`、`reverse`、`foldl`、`foldr`、`concat`、`takeWhile`、`filter`、等等。

它也有具有相同名稱、且表現得跟 `System.IO` 中的某些 function 一樣的 function，只是 `String` 都被以 `ByteString` 取代。舉例來說， `System.IO` 中的 `readFile` function 的型別為 `readFile :: FilePath -> IO String`，而 bytestring 模組中的 <code class="label function">readFile</code> 的型別為 `readFile :: FilePath -> IO ByteString`。小心，若是你使用 strict bytestring，且試圖讀取一個檔案，它將會一次將它讀取到記憶體中！以 lazy bytestring，它將會將它讀入整齊的 chunk 中。

讓我們建立一個作為命令列引數接收兩個檔案名稱、並將第一個檔案複製到第二個檔案的簡單程式。注意到 `System.Directory` 已經有個叫做 `copyFile` 的 function，但我們無論如何都要實作我們自己的檔案複製 function 與程式。

<pre name="code" class="haskell:hs">
import System.Environment
import qualified Data.ByteString.Lazy as B

main = do
    (fileName1:fileName2:_) <- getArgs
    copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
    contents <- B.readFile source
    B.writeFile dest contents
</pre>

我們建立我們自己的 function，其接收兩個 `FilePath`（記住，`FilePath` 僅是個 `String` 的型別同義詞），並回傳一個將會使用 bytestring 將一個檔案複製到另一個檔案的 I/O 動作。在 `main` function 中，我們僅取得引數，並以它們來呼叫我們的 function 以取得 I/O 動作，它接著會被執行。

<pre name="code" class="plain">
$ runhaskell bytestringcopy.hs something.txt ../../something.txt
</pre>

注意到不使用 bytestring 的程式看起來可能就像這樣，唯一的不同是我們使用了 `B.readFile` 與 `B.writeFile`，而非 `readFile` 與 `writeFile`。許多時候，你可以藉由進行必要的引入、然後將限制的模組名稱放在某些 function 的前面，以把使用一般字串的程式轉成使用 bytestring 的程式。有時候，你必須轉換你寫來運作在字串上的 function，以讓它運作在 bytestring 上，但這並不困難。

每當你需要讓一個將許多資料讀取成字串的程式有更好的效能時，給 bytestring 一個機會吧，或許你會以很小的努力得到不錯的效能提昇。我通常使用一般的字串來撰寫程式，然後若是效能並不令人滿意，就將它改成使用 bytestring。

## <a name="exceptions">例外</a>

<img src="img/timber.png" style="float:left" alt="timberr!!!!" />
所有的語言都有可能會有以某種方式失敗的 procedure、function、與一段程式碼。這是個不爭的事實。不同的語言有不同處理失敗的方法。在 C 裡面，我們通常使用一些異常的回傳值（像是 `-1`，或是一個空指標〈null pointer〉），來表示一個 function 回傳的值不該如同普通的值一般看待。在另一方面，Java 與 C# 往往會使用例外（exception）來處理失敗的情況。當例外被拋出（throw）時，控制流程就會跳到我們定義來做一些清理、接著或許會重新拋出例外以讓其它錯誤處理程式碼可以處理其他東西的某段程式碼。

Haskell 有個非常棒的型別系統。代數資料型別允許像是 `Maybe` 與 `Either` 之類的型別，我們可以使用這些型別的值來表示可能有或沒有的結果。在 C 裡頭，在失敗時回傳 `-1` 完全是慣例的問題。它只對人有特殊意義。如果我們不小心，我們可能會把這些異常的值作為一般的值對待，然後它們就會導致我們程式碼中的浩劫與驚愕。Haskell 的型別系統在這方面，給了我們非常需要的安全性。一個 `a -> Maybe b` 的 function 清楚地表示它可能會產生一個包在 `Just` 中的 `b`，或者回傳 `Nothing`。這個型別與只有簡單的 `a -> b` 不同，若是我們試著交替使用這兩個 function，編譯器就會向我們抱怨。

縱使有富有表達能力的型別來做為計算失敗的依據，Haskell 依舊擁有對例外的支持，因為它們在 I/O 情境中更加合理。在處理外部世界時，很多東西都會出問題的，因為它就是這麼不可信任。舉例來說，在開啟一個檔案時，一堆東西都可能出問題。檔案可能被鎖住了、它可能根本不在那裏、或者硬碟裝置什麼的可能根本不在那裏。所以，在這類錯誤發生的時候，能夠跳去我們程式碼中某個錯誤處理的部份是很有用的。

好，所以 I/O 程式碼（即不純粹的程式碼）可以拋出例外。這很合理。但純粹的程式碼怎麼樣呢？嗯，它也可以拋出例外。想想 `div` 與 `head` function。它們的型別分別為 `(Integral a) => a -> a -> a` 與 `[a] -> a`。在它們的回傳型別中沒有 `Maybe` 或是 `Either`，但它們都有可能失敗！當你試著除以零，`div` 會在你的臉上爆炸；當你給 `head` 一個空 list，它會大發脾氣。

<pre name="code" class="haskell:ghci">
ghci> 4 `div` 0
*** Exception: divide by zero
ghci> head []
*** Exception: Prelude.head: empty list
</pre>

<img src="img/police.png" alt="Stop right there, criminal scum!
Nobody breaks the law on my watch!
Now pay your fine or it's off to jail." style="float:left" />
純粹的程式碼可以拋出例外，但它只能夠（在我們進入 `main` 的 <i>do</i> 區塊中的時候）在我們程式碼中的 I/O 部分被攔截（catch）。這是因為你不知道在純粹程式碼中的值會在何時（或者，是否會）被計算，因為它是惰性的、且沒有被明確定義的執行順序，而 I/O 程式碼則有。

稍早，我們談過為什麼我們花在我們程式 I/O 部分的時間應該要盡可能地少。我們程式的邏輯應該大多要屬於純粹的 function 中，因為它們的結果只視用以呼叫 function 的參數而定。處理純粹的 function 時，你只需要思考 function 回傳的東西，因為它不能做這之外的任何事情。這會讓你的日子好過點。即使在 I/O 之中處理某些邏輯是必要的（像是開啟檔案之類的），它還是最好要保持在最低限度。純粹的 function 預設是惰性的，這代表我們不知道它會在何時被求值，而且這也無關緊要。然而，一旦純粹的 function 開始拋出例外，它何時會被求值就很重要了。這就是為什麼我們只能在我們程式碼中的 I/O 部分攔截由純粹 function 拋出的例外。這很糟糕，因為我們想要讓 I/O 部分盡可能地小。然而，若是我們不在我們程式碼中的 I/O 部分攔截它，我們的程式就會當掉。解決方法呢？不要混用例外與純粹的程式碼。藉著 Haskell 強大的型別系統的優勢，並使用像是 `Either` 與 `Maybe` 型別來表示可能會失敗的結果。

這就是為什麼我們現在要來看看如何使用 I/O 例外。I/O 例外是我們在屬於 `main` 一部分的一個 I/O 動作中、與外界溝通時發生問題所造成的例外。舉例來說，我們可以試著開啟一個檔案，結果是檔案已經被刪除或怎麼了。看看這個開啟一個檔案、其名稱作為命令列引數給定、並告訴我們檔案有多少行的程式。

<pre name="code" class="haskell:hs">
import System.Environment
import System.IO

main = do (fileName:_) <- getArgs
          contents <- readFile fileName
          putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
</pre>

一個非常簡單的程式。我們執行 `getArgs` I/O 動作，並將它產生的 list 中的第一個字串綁定到 `fileName`。然後我們把檔案的內容叫做 `contents`。最後，我們將 `lines` 應用到這些內容上以得到每一行的 list，然後我們取得這個 list 的長度、並將它丟給 `show` 以得到這個數字的字串表示。它如同預期般運作，但當我們給它一個不存在的檔案名稱時會發生什麼呢？

<pre name="code" class="plain">
$ runhaskell linecount.hs i_dont_exist.txt
linecount.hs: i_dont_exist.txt: openFile: does not exist (No such file or directory)
</pre>

阿，我們從 GHC 得到一個錯誤，告訴我們檔案不存在。我們的程式當了。如果檔案不存在，而我們想要印出一個比較好的訊息呢？一種方式是在試著開啟檔案前，使用 `System.Directory` 的 <code class="label function">doesFileExist</code> function 來檢查檔案是否存在。

<pre name="code" class="haskell:hs">
import System.Environment
import System.IO
import System.Directory

main = do (fileName:_) <- getArgs
          fileExists <- doesFileExist fileName
          if fileExists
              then do contents <- readFile fileName
                      putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
              else do putStrLn "The file doesn't exist!"
</pre>

我們進行 `fileExists <- doesFileExist fileName`因為 `doesFileExist` 的型別為 `doesFileExist :: FilePath -> IO Bool`，這意謂著它會回傳一個以布林值作為其結果、以告訴我們檔案是否存在的 I/O 動作。我們不能直接在 <i>if</i> expression 中使用 `doesFileExist`。

這裡的另一個解法是使用例外。它在這種情境中使用它是完全可以接受的。一個檔案不存在是一個由 I/O 產生的例外，所以在 I/O 中攔截它是非常好的。

為了要使用例外來處理這個，我們要利用 `System.IO.Error` 的 <code class="label function">catch</code> function。它的型別為 `catch :: IO a -> (IOError -> IO a) -> IO a`。它接收兩個參數。第一個參數為一個 I/O 動作。舉例來說，它可以是個企圖開啟一個檔案的 I/O 動作。第二個參數就是所謂的 handler。若是傳遞到 `catch` 的第一個 I/O 動作拋出一個 I/O 例外，這個例外就會被傳遞到 handler 中，它會決定該做什麼。所以最終的結果即是一個 I/O 動作，其要不是與第一個參數做相同的動作、就是在第一個 I/O 動作拋出了一個例外時，去做 handler 所做的事情。

<img src="img/puppy.png" alt="non sequitor" style="float:right" />
如果你很熟悉像是 Java 或 Python 這些語言中的 <i>try-catch</i> 區塊，`catch` function 就與此相似。第一個參數為企圖去做的事，有點像是在其他命令式語言中、在 <i>try</i> 區塊的東西。第二個參數是接收例外的 handler，就像是大部分接收例外的 <i>catch</i> 區塊，你可以接著檢查看看發生了什麼事。handler 會在例外被拋出時執行。

handler 取一個 `IOError` 型別的值，這是個代表「一個 I/O 例外已經發生」的值。它也帶有關於被拋出的例外型別的資訊。這個型別如何實作視語言自身的實作而定，這代表我們不能藉由對它進行模式匹配來檢查 `IOError` 型別的值，就像是我們不能對 <code>IO <i>something</i></code> 型別的值進行模式匹配一樣。我們可以用一堆有用的述部來查明關於 `IOError` 型別的值，如同我們不久將會學到的。

所以讓我們來試試我們的新朋友 `catch`！

<pre name="code" class="haskell:hs">
import System.Environment
import System.IO
import System.IO.Error

main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e = putStrLn "Whoops, had some trouble!"
</pre>

首先，你會發現我們以反引號包住它，以讓我們可以將它作為前綴 function 來使用，因為它取兩個參數。將它作為一個前綴 function 使用使得它更加易讀。所以 ``toTry `catch` handler`` 與 `catch toTry handler` 相同，這非常符合它的類型。`toTry` 為一個我們企圖執行的 I/O 動作，而 `handler` 為接收一個 `IOError`、並回傳一個在例外的情況中會被執行的動作的 function。

讓我們執行看看：

<pre name="code" class="plain">
$ runhaskell count_lines.hs i_exist.txt
The file has 3 lines!

$ runhaskell count_lines.hs i_dont_exist.txt
Whoops, had some trouble!
</pre>

在 handler 中，我們沒有檢查我們得到的 `IOError` 是哪一種。我們對於任何種類的錯誤，都只會說 `"Whoops, had some trouble!"`。就像在大多數其他語言一樣，在 Haskell 中，只有一個攔截所有類型例外的 handler 是個糟糕的作法。如果有某個我們不想要攔截的其它例外發生，像是我們中斷程式之類的呢？這就是為什麼我們要做在其他語言中通常也會做的相同事情：我們要檢查我們取得的例外是哪一種。如果它是我們等著攔截的例外，我們就做處理。若否，我們就把這個例外拋回荒野中。讓我們修改我們的程式，只攔截檔案不存在造成的例外。

<pre name="code" class="haskell:hs">
import System.Environment
import System.IO
import System.IO.Error

main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e
</pre>

所有東西都維持不變，除了 handler，我們將它修改成只攔截某類 I/O 例外。這裡我們使用了兩個新的 `System.IO.Error` function──<code class="label function">isDoesNotExistError</code> 與 <code class="label function">ioError</code>。`isDoesNotExistError` 為一個對於 `IOError` 的述部，代表它是一個取一個 `IOError`、並回傳一個 `True` 或 `False` 的 function，也就是它的型別為 `isDoesNotExistError :: IOError -> Bool`。我們將它使用在被傳遞到我們的 handler 的例外，以檢查它是否是個由檔案不存在所產生的錯誤。我們在這裡使用了 [guard](syntax-in-functions#guards-guards) 語法，但我們也可以使用 <i>if else</i>。若是它不是由檔案不存在所造成的，我們就以 `ioError` function 重新拋出被傳遞到 handler 的例外。它的型別為 `ioError :: IOException -> IO a`，所以它取一個 `IOError`，並產生一個將會拋出它的 I/O 動作。這個 I/O 動作的型別為 `IO a`，因為它永遠不會真的產生一個結果，所以它可以被視為 <code>IO <i>anything</i></code>。

所以若是我們以一個 <i>do</i> 區塊結合在一起的 `toTry` I/O 動作拋出的例外不是由檔案存在所造成的，``toTry `catch` handler`` 就會攔截它、然後重新拋出它。很酷，對吧？

還有許多作用在 `IOError` 上的述部，而若是一個 guard 不被求值為 `True`，求值就會落到下一個 guard。作用在 `IOError` 的述部為：

* <code class="label function">isAlreadyExistsError</code>
* <code class="label function">isDoesNotExistError</code>
* <code class="label function">isAlreadyInUseError</code>
* <code class="label function">isFullError</code>
* <code class="label function">isEOFError</code>
* <code class="label function">isIllegalOperation</code>
* <code class="label function">isPermissionError</code>
* <code class="label function">isUserError</code>

其中大部分是不言自明的。`isUserError` 在我們使用 <code class="label function">userError</code> function──它被用來建立帶有字串的例外──來建立例外時，它會被求值為 `True`。舉例來說，你可以執行 `ioError $ userError "remote computer unplugged!"`，雖然它寧願你使用像是 `Either` 與 `Maybe` 型別來陳述可能的錯誤，而不是你自己用 `userError` 來拋出例外。

所以你可以有個看起來像這樣的 handler：

<pre name="code" class="haskell:hs">
handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | isFullError e = freeSomeSpace
    | isIllegalOperation e = notifyCops
    | otherwise = ioError e
</pre>

其中 `notifyCops` 與 `freeSomeSpace` 為某個你定義的 I/O 動作。如果它不匹配你的任何一個準則，一定要重新拋出例外，否則你會導致你的程式在某些不該如此的情況中靜靜地失敗。

`System.IO.Error` 也輸出了允許我們向例外要求某些屬性──像是造成錯誤的檔案 handle 是什麼、檔案名稱是什麼──的 function。它們都以 `ioe` 開頭，你可以在文件中看看[它們的完整清單](http://www.haskell.org/ghc/docs/6.10.1/html/libraries/base/System-IO-Error.html#3)。假使我們想要印出造成錯誤的檔案名稱。我們無法印出我們從 `getArgs` 得到的 `fileName`，因為只有 `IOError` 被傳遞到 handler 中，而 handler 不知道其它的東西。一個 function 只視呼叫它的參數而定。這就是為什麼我們要使用 <code class="label function">ioeGetFileName</code> function，其型別為 `ioeGetFileName :: IOError -> Maybe FilePath`。它取一個 `IOError` 作為參數，且可能傳回一個 `FilePath`（記住，這只是個 `String` 的型別同義詞，所以這是一樣的東西）。基本上，它所做的是從 `IOError` 擷取出檔案路徑，如果它可以的話。讓我們修改我們的程式，以印出作為例外發生原因的檔案路徑。

<pre name="code" class="haskell:hs">
import System.Environment
import System.IO
import System.IO.Error

main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e =
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"
    | otherwise = ioError e
</pre>

在 `isDoesNotExistError` 為 `True` 的 guard 中，我們使用一個 <i>case</i> expression 以 `e` 呼叫 `ioeGetFileName`，然後對它回傳的 `Maybe` 值進行模式匹配。當你想要在不引進新的 function 的情況下、對某個值進行模式匹配時，<i>case</i> expression 是很常被使用的。

你不必在整個 I/O 部分中使用 handler 來 `catch` 例外。你可以僅以 `catch` 涵蓋 I/O 程式碼的特定部分，或者你可以以 `catch` 涵蓋多個部分、並為它們使用不同的 handler，像這樣：

<pre name="code" class="haskell:hs">
main = do toTry `catch` handler1
          thenTryThis `catch` handler2
          launchRockets
</pre>

在這裡，`toTry` 使用 `handler1` 作為 handler，而 `thenTryThis` 使用 `handler2`。`launchRockets` 並非 `catch` 的參數，所以它可能拋出的任何例外都會讓我們的程式當掉，除非 `launchRockets` 在內部使用 `catch` 來處理它自己的例外。當然，`toTry`、`thenTryThis` 與 `launchRockets` 都是使用 <i>do</i> 語法結合而成的 I/O 動作，且被假設定義在其它地方。這有點像是其它語言的 <i>try-catch</i> 區塊，其中你可以把你整個程式包在一個單一的 <i>try-catch</i> 中；或者你可以使用更細粒度的方法，在程式碼的不同部分使用不同的 <i>try-catch</i>，以控制哪種錯誤處理要發生在何處。

現在你知道如何處理 I/O 動作了！從純粹的程式碼拋出例外、再處理它們並沒有涵蓋在這裡。主要是因為，如同我們所說的，Haskell 提供了比起用 I/O 攔截錯誤更好的方法來表示錯誤。即使在將 I/O 動作結合在一起時可能會失敗，我也寧願讓它們的型別像是 `IO (Either a b)`，代表它們為一般的 I/O 動作、但它們在執行時產生的結果型別為 `Either a b`，也就代表它是 `Left a` 或 `Right b`。
