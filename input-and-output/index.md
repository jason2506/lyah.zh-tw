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

## <a name="bytestrings">Bytestrings</a>

## <a name="exceptions">例外</a>
