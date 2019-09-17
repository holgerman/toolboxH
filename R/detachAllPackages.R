
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname detachAllPackages
#' @export
detachAllPackages <- function() {
  message("not yet the desired replace for restarting R for fully solving related problems of incompatible packages frequently found on computeservers... in work")

  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")

  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]

  package.list <- setdiff(package.list,basic.packages)

  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)

  a = sessionInfo()
  loadedonly = setdiff(names(a[["loadedOnly"]]), c('compiler', 'tools',    'yaml', 'grid'))

  while(length(loadedonly)>0) {
    message("Packages attached via namespace still to unload : ", length(loadedonly))
    try(unloadNamespace(sample(loadedonly, 1)),silent = T)
    a = sessionInfo()
    loadedonly = setdiff(names(a[["loadedOnly"]]), c('compiler' ,'tools',    'yaml', 'grid'))
  }

  print(sessionInfo())

}
