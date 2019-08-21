

## barplot with labels
barplot_hk = function(x,shift="calc", srt=0, dotable=T,cext=1, textrotate = 0, myround=3,showproz = T, colplot="darkred",...){   #x Vektor fuer Barplot, shift: abstand label und balken, dotable: tablefunktion fuer datenvektor, cext: verkleinerung x -achse,
  library(plotrix)
  if(dotable ==T) {mytable <- table(x, useNA="always");namesarg = names( table(x, useNA="always"))} else {mytable <- (x) ;namesarg = names(mytable)}
  barX <- barp(mytable,col=colplot, staxx=T, srt=srt, names.arg=namesarg, ...)
  if(shift=="calc") shift <- max(mytable)/30 else shift <- shift
  if(showproz) mylabel <- paste(as.character(mytable), " = ", as.character(round(mytable/sum(mytable), myround)*100), "%", sep="") else mylabel <- as.character(mytable)
  text(x=barX$x, y=mytable+shift, label=mylabel, cex=cext, srt = textrotate)
}
