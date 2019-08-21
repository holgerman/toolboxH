
### uhrzeit inkl. einheit
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param timediff PARAM_DESCRIPTION
#' @param mydigits PARAM_DESCRIPTION, Default: 3
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname formateTimediff
#' @export 
formateTimediff = function(timediff, mydigits = 3) paste0(format(unclass(timediff), digits = mydigits), " ", attr(timediff, "units"))
