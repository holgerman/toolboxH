
#' @title Plot a sankey plot
#' @description FUNCTION_DESCRIPTION
#' @param comparematrix data.frame or data.table with plotting data, IMPORTANT Restrict data.frame to columns that should be plotted, NAs currently NOT SUPPORTED
#' @param spalte4color column name used for coloring the sankey
#' @param gap.width PARAM_DESCRIPTION, Default: 0.5
#' @param colorlines PARAM_DESCRIPTION, Default: T
#' @param alpha PARAM_DESCRIPTION, Default: 0.65
#' @param showblocks PARAM_DESCRIPTION, Default: F
#' @param x_axis_size PARAM_DESCRIPTION, Default: 0.1
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#' head(iris)
#' iris$Petal.Width_categ=cut(iris$Petal.Width, breaks = quantile(iris$Petal.Width,c(0, 0.5,1)), include.lowest=T)
#' plotSankey(comparematrix = iris[,c('Species','Petal.Width_categ')], spalte4color = 'Species' )
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_sub}},\code{\link[stringr]{str_length}}
#'  \code{\link[scales]{alpha}}
#' @rdname plotSankey
#' @export
#' @importFrom stringr str_sub str_length
#' @importFrom scales alpha

plotSankey <- function(comparematrix, spalte4color, gap.width = 0.5,colorlines =T,alpha = 0.65, showblocks = F, x_axis_size = 0.1,...) {
  # require('alluvial')
  # vs. 21-02-28
  comparematrix = data.table::as.data.table(data.table::copy(comparematrix))
  matr_names = names(comparematrix)
  matr_count = comparematrix[,.N, mget(matr_names)]
  for(i in matr_names) {
    matr_count[is.na(get(i)) ,(i) := "NA"]

  }

  # some ugly renaming to reorder
  matr_count[,orinum := .I]
  # matr_count[,col1_factor_level := NULL]
  matr_count[,col1_factor_level := as.numeric(as.factor(get(matr_names[1])))  %>% formatC(., width = 6, format = "d", flag = "0")]
  data.table::setorder(matr_count, -N)

  for(colnum in seq(along =matr_names)) {
    # colnum = 2
    colname = matr_names[colnum]
    renaming2 = matr_count[duplicated(get(colname))==F]

    colname_v2=paste0(colname, "_v2")
    renaming2[,(colname_v2) := paste(col1_factor_level, get(colname)) ]

    matr_count[,(colname_v2) := renaming2[match_hk(matr_count[,get(colname)], renaming2[,get(colname)]),get(colname_v2)]]


  }

  alluvial2 = function (..., freq, col = "gray", border = 0, layer, hide = FALSE,
                        alpha = 0.5, gap.width = 0.05, xw = 0.1, cw = 0.1, blocks = TRUE,
                        ordering = NULL, axis_labels = NULL, cex = par("cex"), cex.axis = par("cex.axis"), leadingLetters2remove = 6+1)
  {
    # original alluvial function with reordering by renaming
    p <- data.frame(..., freq = freq, col, alpha, border, hide,
                    stringsAsFactors = FALSE)
    np <- ncol(p) - 5
    if (!is.null(ordering)) {
      stopifnot(is.list(ordering))
      if (length(ordering) != np)
        stop("'ordering' argument should have ", np, " components, has ",
             length(ordering))
    }
    n <- nrow(p)
    if (missing(layer)) {
      layer <- 1:n
    }
    p$layer <- layer
    d <- p[, 1:np, drop = FALSE]
    p <- p[, -c(1:np), drop = FALSE]
    p$freq <- with(p, freq/sum(freq))
    col <- col2rgb(p$col, alpha = TRUE)
    if (!identical(alpha, FALSE)) {
      col["alpha", ] <- p$alpha * 256
    }
    p$col <- apply(col, 2, function(x) do.call(rgb, c(as.list(x),
                                                      maxColorValue = 256)))
    isch <- sapply(d, is.character)
    d[isch] <- lapply(d[isch], as.factor)
    if (length(blocks) == 1) {
      blocks <- if (!is.na(as.logical(blocks))) {
        rep(blocks, np)
      }
      else if (blocks == "bookends") {
        c(TRUE, rep(FALSE, np - 2), TRUE)
      }
    }
    if (is.null(axis_labels)) {
      axis_labels <- names(d)
    }
    else {
      if (length(axis_labels) != ncol(d))
        stop("`axis_labels` should have length ", names(d),
             ", has ", length(axis_labels))
    }
    getp <- function(i, d, f, w = gap.width) {
      a <- c(i, (1:ncol(d))[-i])
      if (is.null(ordering[[i]])) {
        o <- do.call(order, d[a])
      }
      else {
        d2 <- d
        d2[1] <- ordering[[i]]
        o <- do.call(order, d2[a])
      }
      x <- c(0, cumsum(f[o])) * (1 - w)
      x <- cbind(x[-length(x)], x[-1])
      gap <- cumsum(c(0L, diff(as.numeric(d[o, i])) != 0))
      mx <- max(gap)
      if (mx == 0)
        mx <- 1
      gap <- gap/mx * w
      (x + gap)[order(o), ]
    }
    dd <- lapply(seq_along(d), getp, d = d, f = p$freq)
    rval <- list(endpoints = dd)
    op <- par(mar = c(2, 1, 1, 1))
    plot(NULL, type = "n", xlim = c(1 - cw, np + cw), ylim = c(0,
                                                               1), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i",
         xlab = "", ylab = "", frame = FALSE)
    ind <- which(!p$hide)[rev(order(p[!p$hide, ]$layer))]
    for (i in ind) {
      for (j in 1:(np - 1)) {
        xspline(c(j, j, j + xw, j + 1 - xw, j + 1, j + 1,
                  j + 1 - xw, j + xw, j) + rep(c(cw, -cw, cw),
                                               c(3, 4, 2)), c(dd[[j]][i, c(1, 2, 2)], rev(dd[[j +
                                                                                                1]][i, c(1, 1, 2, 2)]), dd[[j]][i, c(1, 1)]),
                shape = c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0), open = FALSE,
                col = p$col[i], border = p$border[i])
      }
    }
    for (j in seq_along(dd)) {
      ax <- lapply(split(dd[[j]], d[, j]), range)
      names(ax) = stringr::str_sub(string = names(ax), start = leadingLetters2remove , end = stringr::str_length(names(ax)))
      if (blocks[j]) {
        for (k in seq_along(ax)) {
          rect(j - cw, ax[[k]][1], j + cw, ax[[k]][2])
        }
      }
      else {
        for (i in ind) {
          x <- j + c(-1, 1) * cw
          y <- t(dd[[j]][c(i, i), ])
          w <- xw * (x[2] - x[1])
          xspline(x = c(x[1], x[1], x[1] + w, x[2] - w,
                        x[2], x[2], x[2] - w, x[1] + w, x[1]), y = c(y[c(1,
                                                                         2, 2), 1], y[c(2, 2, 1, 1), 2], y[c(1, 1),
                                                                                                           1]), shape = c(0, 0, 1, 1, 0, 0, 1, 1, 0,
                                                                                                                          0), open = FALSE, col = p$col[i], border = p$border[i])
        }
      }
      for (k in seq_along(ax)) {
        text(j, mean(ax[[k]]), labels = names(ax)[k], cex = cex)
      }
    }
    axis(1, at = rep(c(-cw, cw), ncol(d)) + rep(seq_along(d),
                                                each = 2), line = 0.5, col = "white", col.ticks = "black",
         labels = FALSE)
    axis(1, at = seq_along(d), tick = FALSE, labels = axis_labels,
         cex.axis = cex.axis)
    par(op)
    invisible(rval)
  }

  data.table::setorder(matr_count, -N)

  for(i in matr_names) matr_count[,(i) := NULL]
  data.table::setnames(matr_count, paste0(matr_names, "_v2"), matr_names)

  if(colorlines ==F)   alluvial2(matr_count[,matr_names, with = F], freq=matr_count$N,col = rainbow(matr_count[,uniqueN(get(spalte4color))])[factor(matr_count[,get(spalte4color)]) %>% as.numeric], gap.width = gap.width, border = 'grey90',alpha =alpha, blocks = showblocks, cw = x_axis_size, ...)

  if(colorlines ==T)   alluvial2(matr_count[,matr_names, with = F], freq=matr_count$N,col = rainbow(matr_count[,uniqueN(get(spalte4color))])[factor(matr_count[,get(spalte4color)]) %>% as.numeric], gap.width = gap.width, border = scales::alpha(rainbow(matr_count[,uniqueN(get(spalte4color))]),alpha = min(0.25, alpha-0.55))[factor(matr_count[,get(spalte4color)]) %>% as.numeric], alpha =alpha, blocks = showblocks, cw = x_axis_size,...)



}
