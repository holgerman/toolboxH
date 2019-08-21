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
#' @rdname xtabs_hk
#' @export 
xtabs_hk = function(...) xtabs(... , exclude = NULL, na.action= "na.pass") # 12.6.15 zweites komma weggemacht, in .env verschoben
