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

首先，讓我們看看 `reverseWords` function。它僅是一個取一個像是 `"hey there man"` 的字串、然後以它呼叫 `words` 以產生一個像是 `["hey","there","man"]` 的單詞 list 的普通 function。然後我們將 `reverse` 映射到 list 上、得到 `["yeh","ereht","nam"]`，然後我們使用 `unwords` 將它擺回字串中，最終結果為 `"yeh ereht nam"`。看看在這裡我們是如何使用複合函數的。少了複合函數，我們就必須寫下像是 `reverseWords st = unwords (map reverse (words st))` 這樣的東西。

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

## <a name="command-line-arguments">命令列引數</a>

## <a name="randomness">隨機性</a>

## <a name="bytestrings">Bytestrings</a>

## <a name="exceptions">例外</a>
