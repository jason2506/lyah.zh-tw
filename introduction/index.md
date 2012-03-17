---
layout: page
title: 引言
next:
    url: starting-out
    title: 出發
---

## <a name="about-this-tutorial">關於此教學</a>

歡迎來到 *Learn You a Haskell for Great Good*！如果你正在閱讀此教學，可能代表你想要學習 Haskell 吧。嗯，你來對地方了，不過先讓我們稍微談談這篇教學。

我決定撰寫此教學，是因為我想要鞏固我自身對 Haskell 的理解，也因為我想我能夠幫助剛開始接觸 Haskell 的人透過我的觀點來學習它。網路上已有不少 Haskell 的教學。當我開始學習 Haskell 的時候，我無法只從單一資源學習它。我是從多個不同的教學與文章學習 Haskell，因為它們描述事物的方式各不相同。藉由多種資源，我能夠將多個片段拼湊在一起。所以這是一次加入另一個 Haskell 學習資源的嘗試，你也多了個找到你喜歡的學習資源的機會。

<img src="img/bird.png" alt="bird" style="float:left" />

此教學針對已經有命令式（imperative）程式語言（C、C++、Java、Python....）經驗，但不曾接觸過函數式（functional）語言（Haskell、ML、OCaml....）的讀者。雖然我敢打賭，即使你不曾有過任何實質的程式經驗，像你這樣的聰明小伙子也能夠跟著學習 Haskell。

如果你覺得卡住了，freenode 網路上的 #haskell 頻道是個讓你問問題的好地方。那裡的人都非常友善，對新手相當有耐心且通情達理。

在我掌握 Haskell 前大概有兩次學習失敗的經驗，因為對我來說它看起來實在太過怪異，令我難以理解。不過當你被突然點醒，並跨越了最初的障礙之後，學習將會變得一帆風順。我猜我想說的是：Haskell 很棒，假如你對寫程式很有興趣，你真的該學學它，儘管乍看之下它很不可思議。學習 Haskell 非常像第一次學寫程式──十分有趣！它迫使你用不同的方式思考，將我們帶到下一個階段....。

## <a name="so-whats-haskell">所以，Haskell 是什麼？</a>

<img src="img/fx.png" alt="f of x" style="float:right" />
Haskell 是一種*純函數式程式語言（purely functional programming language）*。在命令式語言中，你藉由給定電腦一系列的任務（task），並執行它們以完成某件事。在執行時，狀態（state）也會隨之改變。舉例來說，你將變數 `a` 設為 5，並在做某些事之後將它設為其他值。你有控制流程結構（control flow structure）以多次執行某些動作。在純函數式語言中，你不必告訴電腦該做什麼，而是告訴它某個東西<i>是</i>什麼：某個數字的階乘（factorial）是從 1 到這個數字間所有數字的乘積、一組數字序列的總和是第一個數字加上其他數字的總和，諸如此類。你藉由 function 的形式表達它們。你也不能將一個變數設成某值之後，再把它設成另一個值。如果你說 `a` 是 5，你就不能在之後說它是其他的值，因為你已經表明它是 5 了。你不是個騙子，對吧？所以在純函數式語言，一個 function 是沒有副作用（side-effects）的。function 唯一能做的就是求值，然後將它作為結果回傳。剛開始，這看起來似乎是種限制，不過它實際上帶來一些很不錯的結果：假如一個 function 被相同的參數（parameter）呼叫兩次，它能保證傳回一樣的結果。這被稱為參考透明性（referential transparency），其不只讓編譯器（compiler）理解程式的行為，它也使你能夠簡單的推斷（甚至是證明）一個 function 是正確的，並藉由結合簡單的 function 來建構更複雜的 function。

<img src="img/lazy.png" alt="lazy" style="float:right" />
Haskell 是*惰性的（lazy）*。這代表，除非明確指定，否則 Haskell 在真的需要將結果顯示給你之前，並不會執行 function 並計算結果。這個特性能夠與參考透明性良好搭配，使你能夠將程式想成一系列*資料上的轉換（transformations on data）*。這同樣也允許像是無限資料的結構這樣的酷東西。假使你有一個不可變的（immutable）數字序列（list）`xs = [1,2,3,4,5,6,7,8]` 與一個 function `doubleMe`──它會將每個元素（element）乘以 2，並傳回一個新的 list。在命令式語言中，假如我們想要將我們的 list 乘以 8，執行 `doubleMe(doubleMe(doubleMe(xs)))` 大概會走遍整個 list 一次、建立一個副本、然後再回傳它，然後它會再額外走訪兩次 list，之後再傳回結果。在一個惰性語言中，對一個 list 呼叫 `doubleMe` 而沒有要求他顯示結果，程式會告訴你：「好好，待會再做！」。不過一旦你想要看結果，第一個 `doubleMe` 會對第二個 `doubleMe` 說：它現在就要結果！第二個 `doubleMe` 便會對第三個 `doubleMe` 說同樣的話，然後第三個 `doubleMe` 只好傳回 1 <span class="note">〔譯註：`xs` 的第一個元素〕</span>的兩倍，結果是 2。第二個 `doubleMe` 收到結果後傳回 4 給第一個 `doubleMe`。第一個 `doubleMe` 看到後會告訴你第一個元素是 8。所以它只有在你真的需要結果的時候才會走訪整個 list。當你想要從惰性語言中獲取結果，你可以取得一些初始資料，並且有效率地轉換與改良它，它會因此像你最終所想要的。

<img src="img/boat.png" alt="boat" style="float:right" />
Haskell 是*靜態型別的（statically typed）*。當你編譯你的程式，編譯器知道程式的哪個部分是數字、哪個部分是字串等等。這意味著許多可能的錯誤會在編譯時期（compile time）被抓出來。如果你嘗試將一個數字與一個字串相加，編譯器將會向你抱怨。Haskell 採用一種非常好的型別系統，其具有*型別推導（type inference）*。這代表你不必明確為程式的每個部分標記型別，因為型別系統能夠聰明的斷定大部分的情況。假如你說 `a = 5 + 4`，你不需要告訴 Haskell `a` 是一個數字──它可以自己推測出來。型別推導也使得你的程式更加一般化（general）。假如你建立的一個 function 會加總它的兩個參數，你不需要明確陳述它們的型別，這個 function 將會對任何與數字行為相同的兩個參數進行操作<span class="note">〔譯註：假如傳入的兩個參數支援相加這個操作，這個 function 就能夠運作了〕</span>。

Haskell 是*優雅且簡潔的*。因為它使用了許多高階的概念，與等值的命令式程式相比，Haskell 程式通常更短。對比於較長的程式，更短的程式更容易被維護，錯誤也比較少。

Haskell 由一些（擁有博士學位的）*真正的聰明人*所創。Haskell 的研發始於 1987 年，當時有一個委員會的研究人員聚在一起，以設計一個了不起的語言。2003 年，Haskell Report 發表，定義了這個語言的一個穩定版本。

## <a name="what-you-need">你所需要的是....</a>

一個文字編輯器與 Haskell 編譯器。或許你已經裝好了愛用的文字編輯器，因此我們並不在此多加著墨。此教學中我們採用 GHC──它是最常被使用的 Haskell 編譯器。最好的上手方式是下載 [Haskell Platform](http://hackage.haskell.org/platform/)，它基本上是一個可立即使用的 Haskell 環境（Haskell with batteries included）。

GHC 可以編譯一個 Haskell 腳本（通常有個 .hs 附檔名），但它同時擁有互動模式（interactive mode）使你能夠跟腳本互動。你可以呼叫載入腳本內的 function，其結果會立即被顯示出來。對學習來說，相對於每次改變程式都需要編譯，再從提示字元（prompt）執行程式，這是非常簡單且快速的。互動模式可以藉由在提示字元後輸入 `ghci` 來呼叫。假使你已經在 `myfunctions.hs` 這個檔案中定義了一些 function，你可以輸入 `:l myfunctions` 以載入並使用它們。其中 `myfunctions.hs` 是擺在與 `ghci` 被呼叫的相同目錄下。假如你更動了 .hs 腳本，只要再一次執行 `:l myfunctions` 或是 `:r`──它們基本上是相同的，因為 `:r` 會重新載入當前的腳本。通常我的工作流程是在 .hs 檔中定義數個 function，載入並隨意操作它們、然後修改 .hs 檔，並再次載入它們。這也是我們往後將會做的。
