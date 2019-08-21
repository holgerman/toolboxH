
### Aus mehrfacheintraegen einzelne zeilen machen
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dfori PARAM_DESCRIPTION
#' @param idToSplit PARAM_DESCRIPTION
#' @param separator PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname fastermultipleEntries2multipleRows
#' @export
fastermultipleEntries2multipleRows = function(dfori,idToSplit,separator) {
  #15/1/14 grundlegender umbau, um clasen der felder zu erhalten. das hatte bis dahin logicals zerschossen...., weil es vor dies bei textumwandlung leerzeichen einfuegt. allerdings war wahrsheinlich fastmultipleEntries2multipleRows richtig
  # 5/2/15 eigene Methode fuer data.tabel gebaut und insgesamt the data.table way gebaut
  classfound = class(dfori)

  stopifnot(classfound %in% c("data.table", "data.frame"))

  if( "data.table" %nin% classfound) dfori = data.table::data.table(dfori)

  #aufteilen auf 2 table, je nachdem ob Felder den separator enthalten
  time1 = Sys.time()
  call0 = parse(text = paste0('df0 = dfori[grep(separator, ', idToSplit,', invert=T),]'))
  eval(call0)

  call1 = parse(text = paste0('df1 = dfori[grep(separator, ', idToSplit,'),]'))
  eval(call1)

  spaltenzahl = which(names(df1) == idToSplit)
  spaltennamen = names(df1)
  #    spaltennamen = names(df1)[1:6]
  if(nrow(df1)==0) {
    message("Separator kommt nicht im angegebenen Feld vor!")
    return(df0)
  }

  time1 = Sys.time()
  call2 = parse(text = paste0('splitvar = df1[, unique(as.character(',idToSplit,'))]'))
  eval(call2)

  message("\nsplitte ", nrow(df1), " eintraege (entsprechen ", length(splitvar)," unique Eintraegen) ...")

  tosplit = data.table::data.table(splitvar)

  call3 = parse(text = paste0('referenz = tosplit[,list(splitted = unlist(strsplit(splitvar, split= "',separator,'"))), by = list(splitvar)]'))
  eval(call3)

  data.table::setnames(referenz, "splitvar", idToSplit)
  data.table::setkeyv(referenz, idToSplit)

  data.table::setkeyv(df1,idToSplit)

  df1 = referenz[df1,allow.cartesian=TRUE]

  time1 = Sys.time() - time1

  message("done in ", time1, attr(time1, which="units"))

  time1 = Sys.time()
  message("\ncombiniere eintraege...")

  data.table::setnames(df1,  idToSplit, "loesche")
  df1$loesche = NULL

  data.table::setnames(df1, "splitted", idToSplit)

  data.table::setcolorder(df1, names(df0))
  if(nrow(df0) >0) df2 = data.table::rbindlist(list(df0, df1)) else df2 = df1

  time1 = Sys.time() - time1
  message("done in ", time1, attr(time1, which="units"))

  if( "data.table" %nin% classfound)  return(data.frame(df2)) else  return(df2)

}
