cleverLoadinGlobalEnv = function(name, path, myenvir = .GlobalEnv, number_or_name = NULL,...) {

  if(is.null(number_or_name)) {
    if(exists(name)==F) {
      loadedname = load(path, envir = myenvir, ...)
      if(identical(loadedname, name)==F) stop("When loading, found object named `", loadedname, "`` instead of `", name, "`")
      return(loadedname)
    } else message("using object '", name, "' from workspace, saving loading-time :)") # fix 16.4.18
    return(name)
  }

  if(exists(name)==F)  assign(name, load_obj(path, number_or_name = number_or_name), envir = .GlobalEnv) else message("using object '", name, "' from workspace, saving loading-time :)")
  message("assigning ", name, " to loaded object...")
  return(name)

}
