bauePlinkCall = function(on_server = as.character(Sys.info()['sysname']) =="Linux", showMessage1 = T, showMessage2 = T){
  if(on_server==F) {
    basicpath = "R:/genstat/"
    callPlink19 <<-  paste0(basicpath, "07_programme/plink1.9/vs180103_stable_beta_51/win64/plink.exe" )
    callPlink20 <<-  paste0(basicpath, "07_programme/plink2.0/20180103/win64/plink2.exe" )

  } else {
    basicpath = "/net/ifs1/san_projekte/projekte/genstat/"
    callPlink19 <<-  paste0(basicpath, "07_programme/plink1.9/vs180103_stable_beta_51/unix64/plink" )
    callPlink20 <<-  paste0(basicpath, "07_programme/plink2.0/20180103/unix64/plink2" )
  }

  if(showMessage1) message("`callPlink19` will call plink from here: ", callPlink19)
  if(showMessage2) message("`callPlink20` will call plink from here: ", callPlink20)

}
