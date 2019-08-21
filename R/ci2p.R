### confidence intervalls to p values see http://www.bmj.com/content/343/bmj.d2304
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param Est PARAM_DESCRIPTION
#' @param u PARAM_DESCRIPTION
#' @param l PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ci2p
#' @export 
ci2p <- function (Est,u,l) {
  SE = (u - l)/(2*1.96)
  z = Est/SE
  P = exp(-0.717*z - 0.416*z^2)
  P
}
