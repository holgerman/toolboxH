### write with fwrite tabbed gz file

#' @title Combining fwrite() with gz
#' @description Wrapper for fwrite and gz from R.utils
#' @param df Dataframe or data.table to save
#' @param filename Filename without gz suffix
#' @param gzip Whether to gz after writing, Default: T
#' @param delim Delimiter, Default is tab: `\t`
#' @param overwriteGz Overwrite an existing gz file Default: T
#' @param ... passed to fwrite()
#' @return The written and gz compressed file
#' @details Should work on Linux and Windows
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
