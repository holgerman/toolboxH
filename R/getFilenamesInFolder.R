### read all files from dir
getFilenamesInFolder <- function (erkennungsstring = "\\.txt$", on_server = Sys.info()['sysname']=="Linux", wd = getwd()) {
  # 10.6 zerhacken bei unix bei tab

  initial_wd = getwd()
  setwd(wd)
  if(on_server) {system("dir > verzeichnisinfo");system("rm verzeichnisinfo");system("dir > verzeichnisinfo")}
  if(on_server==F) {shell("dir > verzeichnisinfo");shell("DEL verzeichnisinfo");shell("dir > verzeichnisinfo")}  # falls von win gestartet

  options(encoding="WINDOWS-1252")
  files <- readLines(con="verzeichnisinfo")
  options(encoding="UTF-8")

  files

  if(on_server == F){
    files <- files[grep(erkennungsstring, x=files)]
    viertes = sapply(stringr::str_split(files, " +"), "[", 4)
    mystart <- stringr::str_locate(files, pattern=viertes)[,"start"]
    myend <- stringr::str_length(files)

    files <- stringr::str_sub(string=files, start=mystart, end=myend)
    files <- files[grep(erkennungsstring, x=files)]
    setwd(initial_wd)
    return(files)
  }
  if(on_server ){
    files <- unlist(stringr::str_split(files,"\t"))
    files <- files[grep(erkennungsstring, x=files)]
    files = stringr::str_replace_all(files, "\\\\", "")
    setwd(initial_wd)
    return(files)
  }

}
