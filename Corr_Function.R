library(tidyverse)
library(data.table)

GetHighCorr_Result <- function(Data, CorrLevel){
  
  ## 建立相關係數Data
  CorrData <- cor(Data) %>% 
    as.data.table() %>% 
    .[, Variable := names(Data)] %>% 
    dplyr::select(., "Variable", everything())
  
  ## Function 用於找出每一個變數的高相關係數關係變數
  GetHighCorr <- function(Variable_Name){
    
    Data <- CorrData %>% 
      .[, .SD, .SDcols = c("Variable", Variable_Name)] %>% 
      .[abs(get(Variable_Name))>= CorrLevel] %>% 
      .[, Variable2 := names(.)[2]] %>% 
      setnames(., Variable_Name, "Corr") %>% 
      .[, .SD, .SDcols = c("Variable", "Variable2", "Corr")]
    
    return(Data)
    
  }
  
  ## 利用迴圈針對每一個變數找到高相關係數關係並疊合
  HighCorr_UnClean <- NULL
  for (i in names(CorrData)[-1]) {
    
    Data <- GetHighCorr(i)
    
    if (nrow(Data) == 1) next
    
    HighCorr_UnClean <- rbind(HighCorr_UnClean, Data) %>% 
      setDT() %>% 
      .[Corr != 1]
  }
  
  ## 去掉重複值
  HighCorr <- HighCorr_UnClean %>% 
    .[order(-Corr)] %>% 
    .[, head(.SD, 1), by = "Corr"] %>% 
    .[, .SD, .SDcols = c("Variable", "Variable2", "Corr")]
  
  return(HighCorr)
  
}

data("iris")

test <- iris %>% 
  .[, -5] %>% # 去除非數值型的變數
  GetHighCorr_Result(., 0.7)

