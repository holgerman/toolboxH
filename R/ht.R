### Show the first an last  rows  of a data frame or matrix
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION
#' @param myrows PARAM_DESCRIPTION, Default: 10
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ht
#' @export 
ht <- function ( d, myrows=10 )
{ ## updated 11.3. to show all if dim 1 smaller than myrows*2
  rows2show = min(dim(d)[1],myrows)
  if(dim(d)[1] <= 2*rows2show) return(d)
  rbind ( head ( d ,  rows2show ), tail ( d ,  rows2show ))
}
