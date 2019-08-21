#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x1 PARAM_DESCRIPTION
#' @param y1 PARAM_DESCRIPTION
#' @param x2 PARAM_DESCRIPTION
#' @param y2 PARAM_DESCRIPTION
#' @param color PARAM_DESCRIPTION, Default: col.shade
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname shade
#' @export 
  shade <- function(x1, y1, x2, y2, color=col.shade) {
    n <- length(x2)
    graphics::polygon(c(x1, x2[n:1]), c(y1, y2[n:1]), border=NA, col=color)
  }
