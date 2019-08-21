
### system with capturing the output

system_verbose = function(...) {
  report = system(..., intern = T)
  message(paste0("\n\n----------Output of function start:\n\n",paste(report, collapse = "\n"), "\n\n----------Output of function finished...\n\n"))

}
