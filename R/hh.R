#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION
#' @param mydims PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname hh
#' @export 
hh = function ( d, mydims=5 ) {
  # 29.1.15 data.table included
  if("data.table" %in% class(d)) {
    d[1 : min(dim(d)[1],mydims), names(d)[ 1 : min(dim(d)[2],mydims)], with = F]
  } else   d [ 1 : min(dim(d)[1],mydims) , 1 : min(dim(d)[2],mydims) , drop =F]
}
