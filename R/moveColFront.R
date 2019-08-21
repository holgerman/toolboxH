### Takes a dataframe and a column name, and moves that column to the front of the DF.
moveColFront <- function ( d = dataframe , colname = "colname" ) {
  ## 15.6.15 data.table auf setcolorder umgestellt
  ## multiple ohne warning
  stopifnot(all(colname %in% names(d)))
  index <- match ( colname , names ( d ))
  old_order <- 1:ncol(d)
  new_order <- c(index, old_order[old_order %nin% index])
  if(is.data.table(d)) setcolorder(d, new_order)
  if(is.data.table(d)==F)  d= d[,new_order]
  return(d)
}
