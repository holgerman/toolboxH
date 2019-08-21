### Takes a dataframe and a column name, and moves that column to the front of the DF.
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION, Default: dataframe
#' @param colname PARAM_DESCRIPTION, Default: 'colname'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname moveColFront
#' @export 
moveColFront <- function ( d = dataframe , colname = "colname" ) {
  ## 15.6.15 data.table auf setcolorder umgestellt
  ## multiple ohne warning
  stopifnot(all(colname %in% names(d)))
  index <- match ( colname , names ( d ))
  old_order <- 1:ncol(d)
  new_order <- c(index, old_order[old_order %nin% index])
  if(data.table::is.data.table(d)) data.table::setcolorder(d, new_order)
  if(data.table::is.data.table(d)==F)  d= d[,new_order]
  return(d)
}
