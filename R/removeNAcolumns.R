#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname removeNAcolumns
#' @export 
removeNAcolumns = function(df) {
  nacol = sapply(df, function(x) sum(is.na(x)) == dim(df)[1])
  if("data.table" %in% class(df)) return(df[,nacol ==F, with = F]) else return(df[,nacol ==F])
}
