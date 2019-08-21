#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param f PARAM_DESCRIPTION
#' @param number_or_name PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname load_obj
#' @export 
load_obj <- function(f, number_or_name = 1) ## load object number_or_name with numberin new environment to allow that it does not overwrite something with the same name
{
  envextra <- new.env()
  nm <- load(f, envextra)
  message("found following objects:\n", paste(nm, collapse = "\n"))
  if (is.numeric(number_or_name)) {
    message("\nimported object ", nm[number_or_name])
    return(envextra[[nm[number_or_name]]])
  }
  else if (is.character(number_or_name)) {
    obj_number = which(nm == number_or_name)
    if (length(obj_number) == 0)
      stop(paste("did not found object named ", number_or_name))
    message("\nimported object ", nm[which(nm == number_or_name)])
    return(envextra[[nm[obj_number]]])
  } else stop("number or name must be either a valid number or a valid character naming one of the objects above" )
}
