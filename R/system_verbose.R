
### system with capturing the output

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname system_verbose
#' @export 
system_verbose = function(...) {
  report = system(..., intern = T)
  message(paste0("\n\n----------Output of function start:\n\n",paste(report, collapse = "\n"), "\n\n----------Output of function finished...\n\n"))

}
