
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param punktcol PARAM_DESCRIPTION, Default: rgb(0, 0, 0, 0.3)
#' @param punktform PARAM_DESCRIPTION, Default: 4
#' @param cortype PARAM_DESCRIPTION, Default: 'spearman'
#' @param smoothness PARAM_DESCRIPTION, Default: 0.95
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  
#' @rdname nicepairs
#' @export 
#' @import RColorBrewer
#' @import stats
nicepairs = function (x, punktcol = rgb(0, 0, 0, 0.3), punktform = 4, cortype = "spearman",smoothness = 0.95, ...)
{
  #24.7.18 can handle single values with lots NA
  panel.cor = function(x, y, digits = 2, prefix = "") {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    isna <- is.na(x) == F & is.na(y) == F
    r <- cor(x[isna], y[isna], method = cortype)

    pval = try(cor.test(x[isna], y[isna], method = cortype)$p.value, silent = T)
    if("try-error" %in% class(pval)) {pval = NA}
    if(is.na(r)==F) pvalstars = showStars(pval) else  pvalstars =""
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, pvalstars, sep = "")
    if(is.na(r)==F) cex_todo = 1 + abs(r)  else  cex_todo = 1
    colorrule = data.frame(myvalue = seq(0, 1, 0.125), mycolor = mypalette <- RColorBrewer::brewer.pal(9,
                                                                                                       "YlOrRd"))
    if(is.na(r)==F) bg = colorrule[abs(r) > colorrule$myvalue & abs(r) <=
                                     (colorrule$myvalue + 0.125), "mycolor"] else bg = "grey55"
    ll <- par("usr")
    rect(ll[1], ll[3], ll[2], ll[4], col =  as.character(bg))
    text(0.5, 0.5, txt, cex = cex_todo, col = ifelse(is.na(r), "grey99", ifelse(r <
                                                                                  0, "dodgerblue4", "black")))
  }
  panel.smooth2 = function(x, y, bg = NA, pch = punktform,
                           cex = 1, col = punktcol, col.smooth = "red", span = smoothness,
                           iter = 3, ...) {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
      lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = col.smooth)
  }
  # pairs(x, lower.panel = panel.cor, upper.panel = panel.smooth2)
  pairs(x, lower.panel = panel.cor, upper.panel = panel.smooth2, ...)
}
