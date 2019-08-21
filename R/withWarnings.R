#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fn PARAM_DESCRIPTION
#' @param sheet PARAM_DESCRIPTION, Default: sheet
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
#' @rdname withWarnings
#' @export 
#' @import readxl
  withWarnings <- function(fn,sheet = sheet) {
    myWarnings <- NULL
    wHandler <- function(w) {
      myWarnings <<- c(myWarnings, list(w))
      invokeRestart("muffleWarning")
    }
    val <- withCallingHandlers(readxl::read_excel(fn,sheet , skip =skip, na = na,...), warning = wHandler)
    list(value = val, warnings = myWarnings)
  }
