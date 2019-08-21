### matching und dabeiaufpassen, dass matchvariable unique ist
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @param testunique PARAM_DESCRIPTION, Default: T
#' @param makeunique PARAM_DESCRIPTION, Default: F
#' @param importcol PARAM_DESCRIPTION, Default: NULL
#' @param showMessages PARAM_DESCRIPTION, Default: T
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname match_hk
#' @export 
match_hk = function(x, y, testunique =T, makeunique = F,importcol = NULL,showMessages = T, ...) {
  ##150122 makeunique = F statt T, na.omit bei duplicated y, fehlenden ok fall includiert
  ##160328 check auf gleiche laenge x und improtcol
  ##160616 match hk zeigt die duplikated zeilen statt mytabl falls ein Fehler kommt
  #   x = transkripte_eqtl$nuid
  #   y = ilmnAnnot013$nuid
  ##180530 data.table aware

  yname = deparse(substitute(y))

  # 150119 unique check auf schnelles duplicated umgestellt, auto makeuniuq
  if(testunique ==T){
    check = as.numeric(sum(duplicated(stats::na.omit(y))))
    if(identical(check, 0)) return(match(x, y, incomparables=c(NA, NaN),...))

    if(identical(check, 0)==F  & makeunique == F) {
      if(showMessages ==T) message("Duplicated entries:\n", paste(y[duplicated(y)], collapse = "\n"))
      stop(paste(yname ,"ist nicht unique"))
    }

    if(identical(check, 0)==F  & makeunique == T) {

      ## try to make it nunique
      if(is.null(importcol)) stop("When asking for make unique, please provide vector with values to be imported")
      if(length(importcol) != length(y)) stop("When asking for make unique, please provide vector with values to be imported")

      datatable_da = "data.table" %in%  rownames(utils::installed.packages())
      datatable_da
      if(datatable_da) {
        matcher = unique(data.table::data.table(index = y, importcol = importcol))
        matcher = matcher[ index %in% x]
        matchercheck = matcher[,as.numeric(sum(duplicated(stats::na.omit(index))))]
        if(identical(matchercheck, 0)==F  ) {
          if(showMessages ==T) print(matcher[allDuplicatedEntries(matcher$index)])
          stop(paste(yname ,"ist nicht unique after trying to make index and importcol unique..."))
        }
      }

      if(datatable_da==F) {
        matcher = unique(data.frame(index = y, importcol = importcol))
        matcher = matcher[ matcher$index %in% x,]
        matchercheck = as.numeric(sum(duplicated(stats::na.omit(matcher$index))))
        if(identical(matchercheck, 0)==F  ) {
          if(showMessages ==T) print(matcher[allDuplicatedEntries(matcher$index),])
          stop(paste(yname ,"ist nicht unique after trying to make index and importcol unique..."))
        }
      }
      return(match(x, y, incomparables=c(NA, NaN),...))

    }

  }
  if(testunique ==F)  return(match(x, y, incomparables=c(NA, NaN),...))
}
