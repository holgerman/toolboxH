
### plotte mean und se oder sd df im long format als boesen dynamite plot
plotMeanSE= function(categs, werte, plotteSD=F, ...) {
  library(sciplot)
  if(plotteSD==F) bargraph.CI(x.factor = categs, response = werte,...)
  if(plotteSD==T) bargraph.CI(x.factor = categs, response = werte, ci.fun = function(x) {
    fun = function(x) mean(x, na.rm=TRUE)
    c(fun(x)-sd(x), fun(x)+sd(x))
  },...)
}
