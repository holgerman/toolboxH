### end up a skript
finalizeSkript <- function(myfilename=filename, saveTheImage=F, dostr=F,mypathwd=pathwd){
  # 130418 variante do pdf entfernt, da man compile notebook in RStudio machen kann
  if(exists('myfilename') ==F) myfilename = "string_myfilenme_not_defined"
  if(exists('mypathwd') ==F) pathwd = getwd()

  message("==================================================================================")
  message("\n\nWarnings found so far:\n\n")

  print(table(names(warnings() )))

  message("==================================================================================")
  message("\n\nSession Info::\n\n")

  print(sessionInfo())

  if(dostr==T) {
    message("==================================================================================")
    message("\n\nInfos about R-object included in R Session:\n\n")

    for(i in ls())   {
      if(mode(get(i)) != "function"){
        print("_________________________________")
        print(i)
        str(get(i))
      }

    }
  }
  if(saveTheImage==T) {
    setwd(mypathwd)
    save_filename <- paste0("obj/", myfilename, ".RData")
    message(paste("image saved under :\n", save_filename))
    save.image(file=save_filename)
    ## ZU TESTZWECKEN   load(
  }

  message("==================================================================================")
  message("\n\nTotal Time:\n\n")

  if(exists("time0")) print(Sys.time() -time0)

}
