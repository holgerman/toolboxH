cleverFread = function(name, path, myenvir = .GlobalEnv, ...) {
  library(data.table)
  if(exists(name)==F) assign(name, fread(path, ...), envir = myenvir) else message("using object '", name, "' from workspace, saving loading-time :)")
  return(get(name))
}
