### im Text als potenz schreiben, so dass es von knitr umgewandelt werden kann
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param showdigits PARAM_DESCRIPTION, Default: 1
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
#' @rdname potenzFormate
#' @export 
#' @import stringr
potenzFormate = function(x, showdigits=1) {paste0(stringr::str_replace(formatC(x, digits=showdigits, format = "e"), "e", "x10^"),"^")}
