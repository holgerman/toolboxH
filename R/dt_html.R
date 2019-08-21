### schlaue html table
dt_html <- function (df2, zeileninitial=20, maxstringlength = 25) {
  library(DT)

  if(sum(showNA(df2)$NAs) ==0){
    datatable((df2),
              class =  'cell-border stripe',
              filter = 'bottom',
              extensions = c( "ColReorder", 'KeyTable','Scroller'),
              options = list(
                pageLength = zeileninitial,
                autoWidth = TRUE,
                dom = 'T<"clear">CRlfrtip',
                columnDefs = list(list(
                  targets = 1:ncol(df2),
                  render = JS(
                    "function(data, type, row, meta) {",
                    stringr::str_replace("return type === 'display' && data.length > maxstringlength ?","maxstringlength", as.character(maxstringlength)) ,
                    stringr::str_replace("'<span title=\"' + data + '\">' + data.substr(0, maxstringlength) + '...</span>' : data;","maxstringlength", as.character(maxstringlength)) ,
                    "}")
                )),
                scrollY = 1000,
                scrollCollapse = TRUE
              ),
              callback = JS('table.page(3).draw(false);'))
  } else {
    datatable(df2,
              class =  'cell-border stripe',
              filter = 'bottom',
              options = list(
                dom = 'CRlfrtip',
                columnDefs = list(list(
                  render = JS(
                    "function(data, type, row, meta) {",
                    stringr::str_replace("return type === 'display' && data.length > maxstringlength ?","maxstringlength", as.character(maxstringlength)) ,
                    stringr::str_replace("'<span title=\"' + data + '\">' + data.substr(0, maxstringlength) + '...</span>' : data;","maxstringlength", as.character(maxstringlength)) ,
                    "}")
                ))
              ),
              callback = JS('table.page(3).draw(false);')
    )
  }
}
