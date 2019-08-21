##..................................................................................
## reporting inkl formatieren mit knitr & co
##..................................................................................
### copy to the clipboard
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
#' @rdname ccc
#' @export 
ccc <- function(x)utils::write.table(x, "clipboard",sep="\t",row.names=F)
