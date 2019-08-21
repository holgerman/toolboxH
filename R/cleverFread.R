#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name PARAM_DESCRIPTION
#' @param path PARAM_DESCRIPTION
#' @param myenvir PARAM_DESCRIPTION, Default: .GlobalEnv
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname cleverFread
#' @export
cleverFread = function(name, path, myenvir = .GlobalEnv, ...) {

  if(exists(name)==F) assign(name, data.table::fread(path, ...), envir = myenvir) else message("using object '", name, "' from workspace, saving loading-time :)")
  return(get(name))
}
