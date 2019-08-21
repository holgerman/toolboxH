### write so that snptest accepts the file
###
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param filename PARAM_DESCRIPTION
#' @param samplefile PARAM_DESCRIPTION
#' @param vartypes PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname writeSnptestSamplefile
#' @export 
writeSnptestSamplefile <- function (filename, samplefile, vartypes) {
  stopifnot(dim(samplefile)[2] == length(vartypes))
  stopifnot(identical(as.character(vartypes[1:3]),as.character(c(0,0,0))))
  stopifnot(all(vartypes %in% c("0", "C", "D", "B", "P")))
  stopifnot(identical(names(samplefile)[1:3],c("ID1","ID2","missing")) | identical(names(samplefile)[1:3],c("ID_1","ID_2","missing") ))
  f <- file(filename, open="wb")
  write.table(t(names(samplefile)),file=f,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
  write.table(t(as.character(vartypes)),file=f,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
  write.table(samplefile,file=f,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
  close(f)
}
