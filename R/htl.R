#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param laenge PARAM_DESCRIPTION, Default: 2
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname htl
#' @export 
htl = function(df, laenge =2) {### show the first and last row transposed
  dim1 = dim(df)[1]
  zeilen = c(1:laenge,(dim1 -laenge+1) :dim1)
  res = t(df[zeilen,])
  colnames(res) = zeilen
  res
}
