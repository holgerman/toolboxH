

## barplot with labels
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param shift PARAM_DESCRIPTION, Default: 'calc'
#' @param srt PARAM_DESCRIPTION, Default: 0
#' @param dotable PARAM_DESCRIPTION, Default: T
#' @param cext PARAM_DESCRIPTION, Default: 1
#' @param textrotate PARAM_DESCRIPTION, Default: 0
#' @param myround PARAM_DESCRIPTION, Default: 3
#' @param showproz PARAM_DESCRIPTION, Default: T
#' @param colplot PARAM_DESCRIPTION, Default: 'darkred'
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname barplot_hk
#' @export 
barplot_hk = function(x,shift="calc", srt=0, dotable=T,cext=1, textrotate = 0, myround=3,showproz = T, colplot="darkred",...){   #x Vektor fuer Barplot, shift: abstand label und balken, dotable: tablefunktion fuer datenvektor, cext: verkleinerung x -achse,
  library(plotrix)
  if(dotable ==T) {mytable <- table(x, useNA="always");namesarg = names( table(x, useNA="always"))} else {mytable <- (x) ;namesarg = names(mytable)}
  barX <- barp(mytable,col=colplot, staxx=T, srt=srt, names.arg=namesarg, ...)
  if(shift=="calc") shift <- max(mytable)/30 else shift <- shift
  if(showproz) mylabel <- paste(as.character(mytable), " = ", as.character(round(mytable/sum(mytable), myround)*100), "%", sep="") else mylabel <- as.character(mytable)
  text(x=barX$x, y=mytable+shift, label=mylabel, cex=cext, srt = textrotate)
}
