---
layout: page
title: Functor、Applicative Functor 與單子
prev:
    url: functionally-solving-problems
    title: 函數式地解決問題
---

Haskell 是純粹性、高階函數、參數化代數型別、與 typeclass 的結合，其允許我們實作比其它語言更高層級的多型。我們不必思考屬於一個大的型別階層的型別。取而代之的，我們思考型別可以作為什麼、然後以合適的 typeclass 連結它們。一個 `Int` 可以扮演很多東西。它可以扮演一個可以比較相等性的東西、一個有順序的東西、一個可以列舉的東西、等等。

typeclass 是很開放的，這代表我們可以定義我們自己的資料型別，思考它可以作為什麼、並以定義它的行為的 typeclass 來連結它。因為如此、且因為 Haskell 優秀的型別系統──其允許我們僅看一個 function 的型別宣告，就能知道許多關於它的資訊──我們可以定義具有非常一般化與抽象的行為的 typeclass。我們已經看過定義了「檢查兩個值是否相等」、或是「以某種順序比較兩個值的大小」的操作的 typeclass。這些都是非常抽象且確切的行為，旦我們不會將它們想成任何特別的東西，因為我們大多數時候都要處理它們。我們最近遇到了 functor，基本上它是可以被 map 的東西。這是個 typeclass 可以描述的有用、旦仍然十分抽象的屬性的例子。在這一章，我們要仔細看看 functor，以及稍微強一點、且更有用的 functor 版本，叫作 applicative functor。我們也會看看單子（monoid），它有點像是保險櫃。

## <a name="functors-redux">回歸 Functor</a>

## <a name="applicative-functors">Applicative functor</a>

## <a name="the-newtype-keyword">newtype 關鍵字</a>

## <a name="monoids">單子</a>
