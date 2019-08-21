### active waiting so that RStudio server does not initiate suspend mode (usefull for huge workspaces)
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname wait4me
#' @export 
wait4me = function() {
  while(T){
    message(paste0(Sys.time(), ": ...still missing you so much...hit ESC when you are back.."))
    Sys.sleep(time = 1800 )
  }
}
