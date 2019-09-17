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
#' @rdname readPlinkFam
#' @export
readPlinkFam = function(x)
{
  file.fam <- read.table(x, sep="")
  names(file.fam) <- c("pid", "id", "fid", "mid" , "sex", "disease")
  file.fam
}
