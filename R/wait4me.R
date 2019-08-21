### active waiting so that RStudio server does not initiate suspend mode (usefull for huge workspaces)
wait4me = function() {
  while(T){
    message(paste0(Sys.time(), ": ...still missing you so much...hit ESC when you are back.."))
    Sys.sleep(time = 1800 )
  }
}
