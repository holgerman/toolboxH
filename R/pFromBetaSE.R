#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param beta PARAM_DESCRIPTION
#' @param se PARAM_DESCRIPTION
#' @param two.sided PARAM_DESCRIPTION, Default: T
#' @param returnXminIf0 PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pFromBetaSE
#' @export 
pFromBetaSE = function(beta, se, two.sided = T, returnXminIf0 = F) {
  # beta = 0.5; se = 0.005;two.sided = T;returnXminIf0 = F
  z = beta/se
  if(two.sided==T) p = 2*stats::pnorm(-abs(z)) else  p = stats::pnorm(-abs(z))
  if(returnXminIf0 ==T) p = ifelse(p>1,1, ifelse(p==0, .Machine$double.xmin, p)) else p = ifelse(p>1,1,p)

  p

}
