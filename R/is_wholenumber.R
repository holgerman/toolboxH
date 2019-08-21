## test auf ganzezahlen
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param tol PARAM_DESCRIPTION, Default: .Machine$double.eps^0.5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname is.wholenumber
#' @export 
is.wholenumber <-   function(x, tol = .Machine$double.eps^0.5) {
  x <- stats::na.omit(x)
  if(is.numeric(x) ==F) return(F)
  if(is.numeric(x) ==T)  abs(x - round(x)) < tol
}
