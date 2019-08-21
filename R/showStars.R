#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param p PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname showStars
#' @export 
showStars = function (p)
{
  if (inherits(p, c("matrix", "data.frame")) && length(dim(p)) ==
      2) {
    apply(p, c(1, 2), showStars)
  }
  else {
    if (length(p) > 1) {
      sapply(p, showStars)
    }
    else {
      s <- ifelse(p > 0.05, "", ifelse(p > 0.01, "*",
                                       ifelse(p > 0.001, "**", "***")))
      s
    }
  }
}
