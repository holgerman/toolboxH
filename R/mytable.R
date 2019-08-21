mytable = function (x, mydigits = 1, doprint = T, do_science_output = F) {
  res = c()
  res$num <- table(x, useNA="always")
  res$output = data.frame(res$num)
  names(res$output) = c("category" , "freq")
  res$output$percent = res$num /sum(res$num)
  res$output$observed = paste0(format(res$output$freq, big.mark=",", scientific=do_science_output), " (", round(res$output$percent*100,mydigits), "%)")

  if('stringr' %in% installed.packages()[,"Package"]) res$output$observed = stringr::str_trim(res$output$observed)
  zeilennamen = as.character(res$output$category)
  zeilennamen[is.na(zeilennamen)] = "NA"
  rownames(res$output) = zeilennamen
  res$output$category = zeilennamen
  if(doprint) print(res$output[,c('observed'), drop= F])
  invisible(res$output)

}
