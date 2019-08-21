#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param mydigits PARAM_DESCRIPTION, Default: 1
#' @param doprint PARAM_DESCRIPTION, Default: T
#' @param do_science_output PARAM_DESCRIPTION, Default: F
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
#' @rdname mytable
#' @export 
#' @import stringr
mytable = function (x, mydigits = 1, doprint = T, do_science_output = F) {
  res = c()
  res$num <- table(x, useNA="always")
  res$output = data.frame(res$num)
  names(res$output) = c("category" , "freq")
  res$output$percent = res$num /sum(res$num)
  res$output$observed = paste0(format(res$output$freq, big.mark=",", scientific=do_science_output), " (", round(res$output$percent*100,mydigits), "%)")

  if('stringr' %in% installed.packages()[,"Package"]) res$output$observed = stringr::str_trim(res$output$observed)
  zeilennamen = as.character(res$output$category)
  zeilennamen[is.na(zeilennamen)] = "NA"
  rownames(res$output) = zeilennamen
  res$output$category = zeilennamen
  if(doprint) print(res$output[,c('observed'), drop= F])
  invisible(res$output)

}
