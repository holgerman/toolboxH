showStartcode = function(os = "unix", pathwd = getwd(), myfilename = filename){

  # create code for command - line start of the script:
  if(exists('myfilename') ==F) myfilename = "string_myfilenme_not_defined"

  if(! os %in% c("unix", "win")) stop("parameter `os` must be either `unix` or `win` ")
  if(os == "unix") message(paste0("\ncd ", pathwd, "\n", "R CMD BATCH --vanilla ", myfilename, ".R ", myfilename, "R.out 2>&1 &"), "\n", "tail -f ",myfilename, "R.out\n")
  if(os == "win") message(paste0("\ncd ", pathwd, "\n", "'c:/Program Files/R/R-3.1.2/bin/x64/Rterm.exe' --vanilla < '", myfilename, ".R' '",paste0(myfilename, "R.out' 2>&1 &"), "\n", "tail -f ",myfilename, "R.out\n"))

}
