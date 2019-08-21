readPlinkBim = function(x, useData.table=T, clever = NULL)
{
  #150303 datatable import bei plink 1.9
  if(useData.table & is.null(clever)) {
    library(data.table)
    file.bim <- fread(x, header = F)
    setnames(file.bim, names(file.bim),  c("chr", "snp", "lod", "pos", "a1", "a2"))
    return(file.bim)
  }

  if(useData.table & is.null(clever)==F) {
    library(data.table)
    stopifnot(is.character(clever))
    file.bim <- cleverFread(name = clever, x, header = F)
    setnames(file.bim, names(file.bim),  c("chr", "snp", "lod", "pos", "a1", "a2"))
    return(file.bim)
  }

  file.bim <- read.delim(x, header = F)
  names(file.bim) <- c("chr", "snp", "lod", "pos", "a1", "a2")
  return(file.bim)

}
