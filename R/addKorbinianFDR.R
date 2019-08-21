#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pvals PARAM_DESCRIPTION
#' @param mystatistic PARAM_DESCRIPTION, Default: 'pvalue'
#' @param showplot PARAM_DESCRIPTION, Default: F
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  
#' @rdname addKorbinianFDR
#' @export 
#' @import fdrtool
addKorbinianFDR = function(pvals, mystatistic = "pvalue", showplot=F, ...){

  tempres =fdrtool::fdrtool(pvals, statistic = mystatistic, plot = showplot, ...)
  stopifnot(identical(pvals, tempres$pval))
  tempres$qval
}
