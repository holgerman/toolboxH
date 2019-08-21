### read all files from dir
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param erkennungsstring PARAM_DESCRIPTION, Default: '\.txt$'
#' @param on_server PARAM_DESCRIPTION, Default: Sys.info()["sysname"] == "Linux"
#' @param wd PARAM_DESCRIPTION, Default: getwd()
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
#' @rdname getFilenamesInFolder
#' @export 
#' @import stringr
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
