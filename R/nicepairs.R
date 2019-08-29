
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param punktcol PARAM_DESCRIPTION, Default: rgb(0, 0, 0, 0.3)
#' @param punktform PARAM_DESCRIPTION, Default: 4
#' @param punktgroesse PARAM_DESCRIPTION, Default: NULL
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
nicepairs2 = function (x, punktcol = grDevices::rgb(0, 0, 0, 0.3), punktform = 4, punktgroesse = NULL,
                       cortype = "spearman", smoothness = 0.95, ...)
{
  panel.cor = function(x, y, digits = 2, prefix = "") {
    usr <- graphics::par("usr")
    on.exit(graphics::par(usr))
    graphics::par(usr = c(0, 1, 0, 1))
    isna <- is.na(x) == F & is.na(y) == F
    r <- stats::cor(x[isna], y[isna], method = cortype)
    pval = try(stats::cor.test(x[isna], y[isna], method = cortype)$p.value,
               silent = T)
    if ("try-error" %in% class(pval)) {
      pval = NA
    }
    if (is.na(r) == F)
      pvalstars = showStars(pval)
    else pvalstars = ""
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, pvalstars, sep = "")

    if(is.null(punktgroesse)){
      if (is.na(r) == F)
        cex_todo = 1 + abs(r)
      else cex_todo = 1
    } else cex_todo = punktgroesse
    colorrule = data.frame(myvalue = seq(0, 1, 0.125), mycolor = mypalette <- RColorBrewer::brewer.pal(9,
                                                                                                       "YlOrRd"))
    if (is.na(r) == F)
      bg = colorrule[abs(r) > colorrule$myvalue & abs(r) <=
                       (colorrule$myvalue + 0.125), "mycolor"]
    else bg = "grey55"
    ll <- graphics::par("usr")
    graphics::rect(ll[1], ll[3], ll[2], ll[4], col = as.character(bg))
    graphics::text(0.5, 0.5, txt, cex = cex_todo, col = ifelse(is.na(r),
                                                               "grey99", ifelse(r < 0, "dodgerblue4", "black")))
  }
  panel.smooth2 = function(x, y, bg = NA, pch = punktform,
                           cex = 1, col = punktcol, col.smooth = "red", span = smoothness,
                           iter = 3, ...) {
    graphics::points(x, y, pch = pch, col = col, bg = bg,
                     cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
      graphics::lines(stats::lowess(x[ok], y[ok], f = span,
                                    iter = iter), col = col.smooth)
  }
  graphics::pairs(x, lower.panel = panel.cor, upper.panel = panel.smooth2,
                  ...)
}
