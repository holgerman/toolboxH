### write delimeted tab
write.delim = function(x, y, writeColnames=T,writeRownames = F, createDir = F, ...) {
  ## create Dir option hinyugefuegt
  # 8.2. rownameparameter hinzugefuegt
  if(createDir ==T ){

    oldwd = getwd()
    pathname = unlist(stringr::str_split(y, pattern="/"))
    pathname = paste(pathname[1:(length(pathname)-1)], collapse="/")
    vortest = try(setwd(pathname), silent=T)
    test = identical(vortest , oldwd)
    setwd(oldwd)
    if (test == F) {
      dir.create(pathname,recursive=T)
      message("\n...created directory ", pathname)

    } else message("\n... directory ", pathname, " already exists...")

  }

  write.table(x, y, quote=F, col.names=writeColnames, row.names= writeRownames, sep="\t")
}
