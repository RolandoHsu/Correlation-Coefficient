### 使用時機
在分析時，時常需要使用相關係數觀察變數之間的相關程度，此時我們可能會選擇利用 stats :: cor 函數，將資料喂進去後產生相關係數表格，甚至可以利用 corrplot 套件來製作精美的圖表。
然而，當變數過多時，若是使用上述之方法，回傳的表格將會非常大且繁雜，非常難以閱讀，因此，製作此function，可以直接排序出相關係數程度較高的變數，讓我們可以較為直接的觀察變數之間的相關程度。

### 使用方法
使用此 function 前需要先載入 tidyverse 以及 data.table，因應function 中需要用到的 pipeline 以及 data.table。 
此 funciton 僅使用兩個引數，分別為 Data 以及 CorrLevel， Data 為要計算相關係數的資料，須注意的是此 Data中僅能保留數值型變數。
CorrLevel 代表的是相關係數的閥值，只有當某兩個相關係數的絕對值大於此閥值時，才會被顯示在最後產生的表格中。

底下為使用 iris 資料的範例，只需要將資料以及 相關係數閥值 CorrLevel 設定好，即可便利的獲取相關程度較高的變數，方便後續做分析。
