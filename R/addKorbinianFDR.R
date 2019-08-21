addKorbinianFDR = function(pvals, mystatistic = "pvalue", showplot=F, ...){

  tempres =fdrtool::fdrtool(pvals, statistic = mystatistic, plot = showplot, ...)
  stopifnot(identical(pvals, tempres$pval))
  tempres$qval
}
