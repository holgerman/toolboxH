
### plotte mean und se oder sd df im long format als boesen dynamite plot
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param categs PARAM_DESCRIPTION
#' @param werte PARAM_DESCRIPTION
#' @param plotteSD PARAM_DESCRIPTION, Default: F
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname plotMeanSE
#' @export
plotMeanSE= function(categs, werte, plotteSD=F, ...) {

  if(plotteSD==F) sciplot::bargraph.CI(x.factor = categs, response = werte,...)
  if(plotteSD==T) sciplot::bargraph.CI(x.factor = categs, response = werte, ci.fun = function(x) {
    fun = function(x) mean(x, na.rm=TRUE)
    c(fun(x)-stats::sd(x), fun(x)+stats::sd(x))
  },...)
}
