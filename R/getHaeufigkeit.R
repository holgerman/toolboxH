


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param var PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname getHaeufigkeit
#' @export 
getHaeufigkeit = function(var){

  require(data.table)
  dt = data.table( var = var, key = "var")

  dt[,oldorder := 1:nrow(dt)]

  tabled = dt[,table(var)]

  tabled = data.table(num = as.numeric(tabled), var = names(tabled), key = 'var')
  dt = tabled[dt]
  setorder(dt, oldorder)
  #   print(dt)
  #   print(dt[,num])

  return(dt$num)
}
