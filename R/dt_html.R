### schlaue html table
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df2 PARAM_DESCRIPTION
#' @param zeileninitial PARAM_DESCRIPTION, Default: 20
#' @param maxstringlength PARAM_DESCRIPTION, Default: 25
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
#' @rdname dt_html
#' @export
#' @import stringr
dt_html <- function (df2, zeileninitial=20, maxstringlength = 25) {


  if(sum(showNA(df2)$NAs) ==0){
    DT::datatable((df2),
              class =  'cell-border stripe',
              filter = 'bottom',
              extensions = c( "ColReorder", 'KeyTable','Scroller'),
              options = list(
                pageLength = zeileninitial,
                autoWidth = TRUE,
                dom = 'T<"clear">CRlfrtip',
                columnDefs = list(list(
                  targets = 1:ncol(df2),
                  render = DT::JS(
                    "function(data, type, row, meta) {",
                    stringr::str_replace("return type === 'display' && data.length > maxstringlength ?","maxstringlength", as.character(maxstringlength)) ,
                    stringr::str_replace("'<span title=\"' + data + '\">' + data.substr(0, maxstringlength) + '...</span>' : data;","maxstringlength", as.character(maxstringlength)) ,
                    "}")
                )),
                scrollY = 1000,
                scrollCollapse = TRUE
              ),
              callback = DT::JS('table.page(3).draw(false);'))
  } else {
    DT::datatable(df2,
              class =  'cell-border stripe',
              filter = 'bottom',
              options = list(
                dom = 'CRlfrtip',
                columnDefs = list(list(
                  render = DT::JS(
                    "function(data, type, row, meta) {",
                    stringr::str_replace("return type === 'display' && data.length > maxstringlength ?","maxstringlength", as.character(maxstringlength)) ,
                    stringr::str_replace("'<span title=\"' + data + '\">' + data.substr(0, maxstringlength) + '...</span>' : data;","maxstringlength", as.character(maxstringlength)) ,
                    "}")
                ))
              ),
              callback = DT::JS('table.page(3).draw(false);')
    )
  }
}
