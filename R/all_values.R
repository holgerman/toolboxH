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
#' @rdname all_values
#' @export 
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- mtc[mtc$id225847815 == x$id225847815,c(tooltipcols) , drop = F]
    paste0(names(row), ": ", format(row), collapse = "<br />")
  }
