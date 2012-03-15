---
layout: page
title: 引言
next:
    url: starting-out
    title: Starting Out
---

## <a name="about-this-tutorial">關於此教學</a>

歡迎來到 *Learn You a Haskell for Great Good*！如果你正在閱讀此教學，可能代表你想要學習 Haskell 吧。嗯，你來對地方了，不過先讓我們稍微談談這篇教學。

我決定撰寫此教學，是因為我想要鞏固我自身對 Haskell 的理解，也因為我想我能夠幫助剛開始接觸 Haskell 的人透過我的觀點來學習它。網路上已有不少 Haskell 的教學。當我開始學習 Haskell 的時候，我無法只從單一資源學習它。我是從多個不同的教學與文章學習 Haskell，因為它們描述事物的方式各不相同。藉由多種資源，我能夠將多個片段拼湊在一起。所以這是一次加入另一個 Haskell 學習資源的嘗試，你也多了個找到你喜歡的學習資源的機會。

<img src="img/bird.png" alt="bird" style="float:left" />

此教學針對已經有命令式（imperative）程式語言（C、C++、Java、Python....）經驗，但不曾接觸過函數式（functional）語言（Haskell、ML、OCaml....）的讀者。雖然我敢打賭，即使你不曾有過任何實質的程式經驗，像你這樣的聰明小伙子也能夠跟著學習 Haskell。

如果你覺得卡住了，freenode 網路上的 #haskell 頻道是個讓你問問題的好地方。那裡的人都非常友善，對新手相當有耐心且通情達理。

在我掌握 Haskell 前的大概有兩次學習失敗的經驗，因為對我來說它看起來實在太過怪異，令我難以理解。不過當你被突然點醒，並跨越了最初的障礙之後，學習將會變得一帆風順。我猜我想說的是：Haskell 很棒，假如你對寫程式很有興趣，你真的該學學它，儘管乍看之下它很不可思議。學習 Haskell 非常像第一次學寫程式──十分有趣！它迫使你用不同的方式思考，將我們帶到下一個階段....。

## <a name="so-whats-haskell">所以，Haskell 是什麼？</a>

<img src="img/fx.png" alt="f of x" style="float:right" />
Haskell is a *purely functional programming language*. In imperative languages you get things done by giving the computer a sequence of tasks and then it executes them. While executing them, it can change state. For instance, you set variable `a` to 5 and then do some stuff and then set it to something else. You have control flow structures for doing some action several times. In purely functional programming you don't tell the computer what to do as such but rather you tell it what stuff is.
The factorial of a number is the product of all the numbers from 1 to that number, the sum of a list of numbers is the first number plus the sum of all the other numbers, and so on. You express that in the form of functions. You also can't set a variable to something and then set it to something else later. If you say that `a` is 5, you can't say it's something else later because you just said it was 5. What are you, some kind of liar? So in purely functional languages, a function has no side-effects. The only thing a function can do is calculate something and return it as a result. At first, this seems kind of limiting but it actually has some very nice consequences: if a function is called twice with the same parameters, it's guaranteed to return the same result. That's called referential transparency and not only does it allow the compiler to reason about the program's behavior, but it also allows you to easily deduce (and even prove) that a function is correct and then build more complex functions by gluing simple functions together.

<img src="img/lazy.png" alt="lazy" style="float:right" />
Haskell is *lazy*. That means that unless specifically told otherwise, Haskell won't execute functions and calculate things until it's really forced to show you a result. That goes well with referential transparency and it allows you to think of programs as a series of *transformations on data*. It also allows cool things such as infinite data structures. Say you have an immutable list of numbers `xs = [1,2,3,4,5,6,7,8]` and a function `doubleMe` which multiplies every element by 2 and then returns a new list. If we wanted to multiply our list by 8 in an imperative language and did `doubleMe(doubleMe(doubleMe(xs)))`, it would probably pass through the list once and make a copy and then return it. Then it would pass through the list another two times and return the result. In a lazy language, calling `doubleMe` on a list without forcing it to show you the result ends up in the program sort of telling you "Yeah yeah, I'll do it later!". But once you want to see the result, the first `doubleMe` tells the second one it wants the result, now! The second one says that to the third one and the third one reluctantly gives back a doubled 1, which is a 2. The second one receives that and gives back 4 to the first one. The first one sees that and tells you the first element is 8. So it only does one pass through the list and only when you really need it. That way when you want something from a lazy language you can just take some initial data and efficiently transform and mend it so it resembles what you want at the end.

<img src="img/boat.png" alt="boat" style="float:right" />
Haskell is *statically typed*. When you compile your program, the compiler knows which piece of code is a number, which is a string and so on. That means that a lot of possible errors are caught at compile time. If you try to add together a number and a string, the compiler will whine at you. Haskell uses a very good type system that has *type inference*. That means that you don't have to explicitly label every piece of code with a type because the type system can intelligently figure out a lot about it. If you say `a = 5 + 4`, you don't have to tell Haskell that `a` is a number, it can figure that out by itself. Type inference also allows your code to be more general. If a function you make takes two parameters and adds them together and you don't explicitly state their type, the function will work on any two parameters that act like numbers.

Haskell is *elegant and concise*. Because it uses a lot of high level concepts, Haskell programs are usually shorter than their imperative equivalents. And shorter programs are easier to maintain than longer ones and have less bugs.

Haskell was made by some *really smart guys* (with PhDs). Work on Haskell began in 1987 when a committee of researchers got together to design a kick-ass language. In 2003 the Haskell Report was published, which defines a stable version of the language.

## <a name="what-you-need">你所需要的是....</a>

一個文字編輯器與 Haskell 編譯器。或許你已經裝好了愛用的文字編輯器，因此我們並不在此多加著墨。此教學中我們採用 GHC──它是最常被使用的 Haskell 編譯器。最好的上手方式是下載 [Haskell Platform](http://hackage.haskell.org/platform/)，它基本上是一個可立即使用的 Haskell 環境（Haskell with batteries included）。

GHC 可以編譯一個 Haskell 腳本（通常有個 .hs 附檔名），但它同時擁有互動模式（interactive mode）使你能夠跟腳本互動。你可以呼叫載入腳本內的函式（function），其結果會立即被顯示出來。對學習來說，相對於每次改變程式都需要編譯，再從提示字元（prompt）執行程式，這是非常簡單且快速的。互動模式可以藉由在提示字元後輸入 `ghci` 來呼叫。假使你已經在 `myfunctions.hs` 這個檔案中定義了一些函式，你可以輸入 `:l myfunctions` 以載入並使用它們。其中 `myfunctions.hs` 是擺在與 `ghci` 被呼叫的相同目錄下。假如你更動了 .hs 腳本，只要再一次執行 `:l myfunctions` 或是 `:r`──它們基本上是相同的，因為 `:r` 會重新載入當前的腳本。通常我的工作流程是在 .hs 檔中定義數個函式，載入並隨意操作它們、然後修改 .hs 檔，並再次載入它們。這也是我們往後將會做的。