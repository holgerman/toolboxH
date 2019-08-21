### write with fwrite tabbed gz file

fwriteGz = function(df, filename, gzip = T, delim = "\t", overwriteGz = T, ...) {
  fwrite(df, filename, sep = delim, ...)
  R.utils::gzip(filename,overwrite=overwriteGz)
  message('wrote ', paste0(filename, ".gz"))
}
