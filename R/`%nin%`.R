#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname %nin%
#' @export

##..................................................................................
## data manipulation and handling
##..................................................................................
## Returns a logical vector TRUE for elements of X not in Y
`%nin%` <- function(x, y) !(x %in% y)
