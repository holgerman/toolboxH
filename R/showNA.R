### show NAs within a data.frame
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param showAllNoNA PARAM_DESCRIPTION, Default: T
#' @param returnAsDataTable PARAM_DESCRIPTION, Default: F
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
#' @rdname showNA
#' @export 
#' @import data.table
showNA <- function(x, showAllNoNA = T, returnAsDataTable = F) {
  ## 15.6.15 als data.frame
  ## 7.7.15 apply statt sapply damit auch mit matrix funzend
  ## 24.7.18 as data.table version
x_is_datatable = data.table::is.data.table(x)
  if(x_is_datatable==F) {
    x = data.table::copy(x)
    data.table::setDT(x)

  }

  resi = unlist(x[,lapply(.SD, function(y) sum(is.na(y)))])
  resi2 = data.table::data.table(var = names(resi), NAs = as.vector(resi), vals = nrow(x)-as.vector(resi))
  if(showAllNoNA) {

    # test = microbenchmark('matrixstat' = {rowsNoNA = x[,matrixStats::rowAnyNAs(as.matrix(.SD))]},
    #                       'byvari' = {rowsNoNA = x[,all(is.na(unlist(.SD))==F), by= row.names(x)]},
    #                       'apply' = {rowsNoNA = apply(x, 1, function(x) all(is.na(x)==F))},times = 100)
    # test
    #
    # ggplot2::autoplot(test)

    # >     test
    # Unit: seconds
    # expr       min        lq      mean    median        uq       max neval
    # matrixstat 124.87354 124.87354 124.87354 124.87354 124.87354 124.87354     1
    # byvari  20.97202  20.97202  20.97202  20.97202  20.97202  20.97202     1
    # apply 136.37191 136.37191 136.37191 136.37191 136.37191 136.37191     1
    # > dim(x)
    # [1] 1500000      96


    rowsNoNA = x[,all(is.na(unlist(.SD))==F), by= list(row.names(x))][,sum(V1)]

    resi2 = rbind(resi2, data.frame(var = 'ROWS_NO_NAs', NAs = nrow(x)-rowsNoNA, vals = rowsNoNA))
  }

  if(returnAsDataTable==F) data.table::setDF(resi2)

  return(resi2)

}
