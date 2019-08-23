### write with fwrite tabbed gz file

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param filename PARAM_DESCRIPTION
#' @param gzip PARAM_DESCRIPTION, Default: T
#' @param delim PARAM_DESCRIPTION, Default: '	'
#' @param overwriteGz PARAM_DESCRIPTION, Default: T
#' @param ... PARAM_DESCRIPTION
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
#' @rdname fwriteGz
#' @export
#' @import R.utils
fwriteGz = function(df, filename, gzip = T, delim = "\t", overwriteGz = T, ...) {
  data.table::fwrite(df, filename, sep = delim, ...)
  message('Now creating gz from file ,' ,filename )
  if(gzip ==T) R.utils::gzip(filename,overwrite=overwriteGz)
  message('wrote ', paste0(filename, ".gz"))
}
