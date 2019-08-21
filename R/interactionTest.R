


## interaction test see http://www.bmj.com/content/326/7382/219.long
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mean1 PARAM_DESCRIPTION
#' @param se1 PARAM_DESCRIPTION
#' @param mean2 PARAM_DESCRIPTION
#' @param se2 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  
#' @rdname interactionTest
#' @export 
#' @import data.table
interactionTest  = function(mean1, se1, mean2, se2) {
  meandiff_se = sqrt(se1^2 + se2^2)
  meandiff = mean2 - mean1
  meandiff_cilow = meandiff - 1.96*meandiff_se
  meandiff_cihigh = meandiff + 1.96*meandiff_se
  meandiff_z = meandiff/meandiff_se
  meandiff_p = pnorm(abs(meandiff_z), lower.tail = F)*2
  if(meandiff_p>1) meandiff_p = 1
  data.table::data.table(mean1, se1, mean2, se2, meandiff, meandiff_se, meandiff_cilow, meandiff_cihigh, meandiff_z, meandiff_p)

}
