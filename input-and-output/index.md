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

## <a name="files-and-streams">檔案與串流</a>

## <a name="command-line-arguments">命令列引數</a>

## <a name="randomness">隨機性</a>

## <a name="bytestrings">Bytestrings</a>

## <a name="exceptions">例外</a>
