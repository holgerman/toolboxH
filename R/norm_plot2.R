### normality check
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vektor PARAM_DESCRIPTION
#' @param x_lim PARAM_DESCRIPTION, Default: c(min(vektor) - min(vektor) * 0.5, max(vektor) + max(vektor) * 
#'    0.1)
#' @param titelgroesse PARAM_DESCRIPTION, Default: 1
#' @param mybreaks PARAM_DESCRIPTION, Default: NULL
#' @param vektorname PARAM_DESCRIPTION, Default: NULL
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname norm_plot2
#' @export 
norm_plot2 <- function(vektor,  x_lim=c(min(vektor)-min(vektor)*.5, max(vektor)+max(vektor)*.1), titelgroesse = 1, mybreaks = NULL,vektorname = NULL , ...) {
  #27.8.15 parameter vektorname
  graphics::par(mfrow=c(1,2))
  if(is.null(mybreaks)) mybreaks = min(c(as.integer(length(vektor)/5), 20))
  # print(mybreaks)
  varname <- ifelse(length(vektorname)==0, deparse(substitute(vektor)),vektorname)
  graphics::hist(vektor,freq=T, breaks=mybreaks, labels=T, col="antiquewhite", xlim=x_lim, main = paste0("Histogram of ", varname), cex.main = titelgroesse, ...)
  graphics::mtext(paste0("ks-test pval = ", signif(stats::ks.test(vektor,"pnorm", mean(vektor), stats::sd(vektor))$p.value,3)))
  stats::qqnorm(vektor,main = paste0("QQ-plot of ", varname), cex.main = titelgroesse,...); stats::qqline(vektor, col = 2)
  if(length(vektor) >5000) shapvektor = sample(vektor, size=5000, replace=F) else shapvektor = vektor
  graphics::mtext(paste0("shapiro wilk pval = ",signif(stats::shapiro.test(shapvektor)$p.value,3) ))
  graphics::par(mfrow = c(1,1))
}
