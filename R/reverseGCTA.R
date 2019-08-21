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
#' @rdname reverseGCTA
#' @export 
reverseGCTA = function(x) {
  x = toupper(x)
  # print(x)
  y = ifelse(x =="A", "T", ifelse(x=="C", "G", ifelse(x=="G", "C", ifelse(x=="T", "A",NA)))) #  stop("alle must be any of a c g t A C G T")
  y
}
