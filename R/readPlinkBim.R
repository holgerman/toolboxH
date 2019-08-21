#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param useData.table PARAM_DESCRIPTION, Default: T
#' @param clever PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname readPlinkBim
#' @export
readPlinkBim = function(x, useData.table=T, clever = NULL)
{
  #150303 datatable import bei plink 1.9
  if(useData.table & is.null(clever)) {

    file.bim <- data.table::fread(x, header = F)
    data.table::setnames(file.bim, names(file.bim),  c("chr", "snp", "lod", "pos", "a1", "a2"))
    return(file.bim)
  }

  if(useData.table & is.null(clever)==F) {

    stopifnot(is.character(clever))
    file.bim <- cleverFread(name = clever, x, header = F)
    data.table::setnames(file.bim, names(file.bim),  c("chr", "snp", "lod", "pos", "a1", "a2"))
    return(file.bim)
  }

  file.bim <- utils::read.delim(x, header = F)
  names(file.bim) <- c("chr", "snp", "lod", "pos", "a1", "a2")
  return(file.bim)

}
