#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param datatable_lines PARAM_DESCRIPTION, Default: 3
#' @param datatable_nrow_allshow PARAM_DESCRIPTION, Default: 10
#' @param datatable_colwidth PARAM_DESCRIPTION, Default: 40
#' @param datatable_cores PARAM_DESCRIPTION, Default: 10
#' @param computer PARAM_DESCRIPTION, Default: 'amanMRO'
#' @param add_libpath PARAM_DESCRIPTION, Default: T
#' @param lib_location PARAM_DESCRIPTION, Default: '/net/ifs1/san_projekte/projekte/genstat/07_programme/rpackages/'
#' @param myfilename PARAM_DESCRIPTION, Default: NULL
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
#' @rdname initializeSkript
#' @export 
#' @import data.table
initializeSkript <-  function(datatable_lines = 3,datatable_nrow_allshow = 10,datatable_colwidth = 40L,datatable_cores = 10,  computer="amanMRO",add_libpath = T,  lib_location = "/net/ifs1/san_projekte/projekte/genstat/07_programme/rpackages/", myfilename =NULL){
  #clear memory cache

  gc()
  gc()


  ### Do you want to automatically convert strings to factor variables in a data.frame? WARNING!!! This makes your code less portable/reproducible.
  message("setting options(stringsAsFactors=FALSE)")
  options(stringsAsFactors=FALSE)

  ## display large integer in data.table as R base does
  message('setting options(datatable.integer64= "numeric")')
  options(datatable.integer64= "numeric")

  ### Don't ask me for my CRAN mirror every time
  message('setting options("repos" = c(CRAN = "http://cran.rstudio.com/"))')
  options("repos" = c(CRAN = "http://cran.rstudio.com/"))


  ### Warning bei partial matchign immer an
  message('setting options ( warnPartialMatchAttr = T ) and options ( warnPartialMatchDollar = T )')
  options ( warnPartialMatchAttr = T )
  options ( warnPartialMatchDollar = T )
  options(nwarnings = 100000)

  # initiate time measuring
  time0 <<- Sys.time()

  message('setting options for data.table( datatable.prettyprint.char = ',datatable_colwidth,'\n)',
          'setting options for data.table( datatable.print.topn = ',datatable_lines,'\n)',
          'setting options for data.table( datatable.print.nrows = ',datatable_nrow_allshow,'\n)',
          'setting options for data.table::setDTthreads() = ',datatable_cores,'\n)')

  data.table::setDTthreads(datatable_cores)
  options(datatable.prettyprint.char= datatable_colwidth )
  options(datatable.print.topn=datatable_lines)
  options(datatable.print.nrows =datatable_nrow_allshow)


  # define where packages are found
  if(add_libpath) .libPaths(unique(c(paste0(lib_location,computer), .libPaths()))) else .libPaths(paste0(lib_location,computer))
  message("using libpath: ", paste(.libPaths(), collapse = "\n"))

}
