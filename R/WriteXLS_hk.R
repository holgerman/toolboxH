### write xlsx with better defaults
WriteXLS_hk = function(x, ExcelFileName = "R.xls", SheetNames = x, AutoFilter = T, BoldHeaderRow = T, FreezeRow = 1, FreezeCol = 1, AdjWidth = F, ...){
  library(WriteXLS)
  WriteXLS( x=x, ExcelFileName = ExcelFileName, SheetNames = SheetNames, AutoFilter = AutoFilter, BoldHeaderRow = BoldHeaderRow, FreezeRow = FreezeRow, FreezeCol = FreezeCol, AdjWidth = AdjWidth,... )}
