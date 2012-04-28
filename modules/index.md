---
layout: page
title: 模組
prev:
    url: higher-order-functions
    title: 高階函數
---

## <a name="loading-modules">載入模組</a>

<img src="img/modules.png" alt="modules" style="float:right" />
一個 Haskell 模組（module）是個相關 function、型別與 typeclass 的集合。一個 Haskell 程式是個模組的集合，其中是由主（main）模組載入其它的模組，然後使用定義在其中的 function 做某些事。將程式碼切割成數個模組有非常多優點。若是一個模組足夠一般化，其輸出（export）的 function 就能夠被使用在許多不同的程式中。若是你擁有的程式碼被分割成獨立（self-contained）的模組，也就是不會太過依賴其它模組（我們也將這稱為鬆耦合〈loosely coupled〉），你就可以在之後重用它。藉由將程式切割成數個部分，每個部分都有其用途，使得撰寫程式的處理更容易控制。

Haskell 標準函式庫被切割成多個模組，每個都包含某種相關且提供共同目的的 function 與型別。有個處理 list 的模組、有個用作平行（concurrent）程式開發的模組、有個處理複數的模組、等等。所有我們到目前為止處理的 function、型別與 typeclass 都是 `Prelude` 模組──它預設會被引入（import）──的一部分。在這一章，我們要檢驗一些有用的模組與其擁有的 function。但首先，我們要來看看如何引入模組。

在 Haskell 腳本中，引入模組的語法為 `import <module name>`。這必須在定義任何 function 前完成，所以引入通常在檔案的開頭完成。當然，一個腳本可以引入多個模組。只要將引入敘述擺在獨立的一行裡。讓我們引入 `Data.List` 模組──它有一堆處理 list 的有用 function，並使用它輸出的 function 來建立一個告訴我們 list 有多少個不重複元素的 function。

<pre name="code" class="haskell:hs">
import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
</pre>

當你執行 `import Data.List` 時，在全域名稱空間（namespace）中，`Data.List` 輸出的所有 function 都會變得可以取用，意味著你可以在腳本的任何地方呼叫它。`nub` 是個定義在 `Data.List` 的 function，其接收一個 list 並去除重複的元素。藉由 `length . nub` 來組合 `length` 與 `nub` 會產生一個等同於 `\xs -> length (nub xs)` 的 function。

你也可以在使用 GHCI 的時候把模組的 function 擺進全域空間中。若是你在 GHCI 中，且想要能夠呼叫從 `Data.List` 輸出的 function，就這樣做：

<pre name="code" class="haskell:ghci">
ghci> :m + Data.List
</pre>

若是你想要在 GHCI 中載入來自多個模組的名稱，我們不需要多次執行 `:m +`，我們可以一次載入多個模組。

<pre name="code" class="haskell:ghci">
ghci> :m + Data.List Data.Map Data.Set
</pre>

然而，若是你載入了已經載入某個模組的腳本，你就不需要使用 `:m +` 來存取它。

若是你僅需要某個模組中的一部分 function，你可以選擇性的引入這些 function。若是你只想要從 `Data.List` 引入 `nub` 與 `sort` function，我們就這樣做：

<pre name="code" class="haskell:hs">
import Data.List (nub, sort)
</pre>

你也可以選擇引入某個模組的所有的 function，除了少數的幾個之外。這在多個模組輸出相同名字的 function，而你想要避免引起問題時，通常挺有用的。假使我們已經有個叫做 `nub` 的我們自己的 function，而我們想要從 `Data.List` 引入所有除了 `nub` function 以外的 function：

<pre name="code" class="haskell:hs">
import Data.List hiding (nub)
</pre>

另一種處理名稱衝突的方式是執行限制（qualified）引入。`Data.Map` 模組──其提供一個藉由鍵（key）尋找值的資料結構──輸出一堆與 `Prelude` 相同名稱的 function，像是 `filter` 或是 `null`。所以當我們引入 `Data.Map` 然後呼叫 `filter` 的時候，Haskell 無法知道要用哪個 function。以下是我們如何解決這個問題：

<pre name="code" class="haskell:hs">
import qualified Data.Map
</pre>

這使得：若是我們想要參照（reference）`Data.Map` 的 `filter` function，我們必須執行 `Data.Map.filter`，而 `filter` 仍然指到我們都熟知喜愛的一般 `filter`。但是在每個來自此模組的 function 前面都要加上 `Data.Map` 有些令人厭煩。這就是我什麼我們可以將限制引入取個比較短的名字：

<pre name="code" class="haskell:hs">
import qualified Data.Map as M
</pre>

現在，要參照 `Data.Map` 的 `filter` function，我們只需要用 `M.filter`。

使用[這個方便的參考手冊](http://www.haskell.org/ghc/docs/latest/html/libraries/)來看看哪些模組在標準函式庫中。拾取新的 Haskell 知識的偉大途徑，即是點開標準函式庫參考手冊，並瀏覽模組與其 function。你也可以看看每個模組的 Haskell 原始碼。閱讀某些模組的原始碼是學習 Haskell 與得到實際體會的非常好的方式。

要搜尋 function 或是尋找其所在，就使用 [Hoogle](http://haskell.org/hoogle)。這是個非常棒的 Haskell 搜尋引擎，你可以透過名稱、模組名稱、甚或是型別簽名來搜尋。

## <a name="data-list">Data.List</a>

## <a name="data-char">Data.Char</a>

## <a name="data-map">Data.Map</a>

## <a name="data-set">Data.Set</a>

## <a name="making-our-own-modules">建立我們自己的模組</a>
