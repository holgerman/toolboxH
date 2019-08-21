### write xlsx with better defaults
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param ExcelFileName PARAM_DESCRIPTION, Default: 'R.xls'
#' @param SheetNames PARAM_DESCRIPTION, Default: x
#' @param AutoFilter PARAM_DESCRIPTION, Default: T
#' @param BoldHeaderRow PARAM_DESCRIPTION, Default: T
#' @param FreezeRow PARAM_DESCRIPTION, Default: 1
#' @param FreezeCol PARAM_DESCRIPTION, Default: 1
#' @param AdjWidth PARAM_DESCRIPTION, Default: F
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname WriteXLS_hk
#' @export
WriteXLS_hk = function(x, ExcelFileName = "R.xls", SheetNames = x, AutoFilter = T, BoldHeaderRow = T, FreezeRow = 1, FreezeCol = 1, AdjWidth = F, ...){

  WriteXLS::WriteXLS( x=x, ExcelFileName = ExcelFileName, SheetNames = SheetNames, AutoFilter = AutoFilter, BoldHeaderRow = BoldHeaderRow, FreezeRow = FreezeRow, FreezeCol = FreezeCol, AdjWidth = AdjWidth,... )}
