### remove duplicated columns
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mydf PARAM_DESCRIPTION
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
#' @rdname removeDuplicatedColumns
#' @export 
#' @import stringr
removeDuplicatedColumns <- function (mydf) {

  namesdf= data.frame(original = names(mydf))
  namesdf$final = sapply(stringr::str_split(namesdf$original, pattern='\\.x\\.*[0-9]*$|\\.y\\.*[0-9]*$'), "[",1)
  tab = table(namesdf$final)
  namesdf$num_categ = tab[ match_hk(namesdf$final, names(tab))]
  finalnames = namesdf$final[duplicated(namesdf$final)==F]

  tounify = unique(namesdf$final[ namesdf$num_categ >1])
  for(i in tounify){
    message("merge ", i)
    mysubset = namesdf[ namesdf$final == i,]
    print(mysubset)
    tomerge = as.matrix(mydf[ , mysubset$original])
    idenditycheck = apply(tomerge,1,function(x) identical(x[[1]], unique(x)))
    #   idenditycheck = apply(tomerge,1,function(x) {print(x[[1]]); print(unique(x));message("-----\n");identical(x[[1]], unique(x))})
    if(all(idenditycheck) != T) stop("probleme gefunden bei obiger kategorie, bitte selber drumm kuemmern")
    mydf[[i]] = tomerge[,1]

  }

  mydf = mydf[,finalnames]
  mydf
}
