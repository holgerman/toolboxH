parallelisiere = function(proc=3, on_server = Sys.info()['sysname']=="Linux", mkl_threads = 1) {
  # mkl_threads: Revolution R aka MRO uses internal multithreading via Intel Math Kernel Libraries. This sometimes seames to conflict with parallizing by foreach
  if(on_server ==T) {
    for(i in c( "doMC", "foreach")) {
      suppressPackageStartupMessages(library(i, character.only = TRUE))
    }

    registerDoMC(cores=proc)
    if("RevoUtilsMath" %in% installed.packages()) {
      RevoUtilsMath::setMKLthreads(1)
      message("MKL threads set to ", RevoUtilsMath::getMKLthreads())
    }

  }

  if(on_server ==F) {
    for(i in c( "doSNOW", "foreach")) {
      suppressPackageStartupMessages(library(i, character.only = TRUE))
    }

    registerDoSNOW(makeCluster(proc, type = "SOCK"))
    if("RevoUtilsMath" %in% installed.packages()) {
      RevoUtilsMath::setMKLthreads(1)
      message("MKL threads set to ", RevoUtilsMath::getMKLthreads())
    }

  }
  message("Parallel backbone initiated for ",proc, " parallel processes....")

}
