
## qq plot fuer minimum p
qqPlotMinP = function (x, number_of_tests = 3, xlab = expression(Expected ~
                                                                   ~-log[10](italic(p[min]))), ylab = expression(Observed ~
                                                                                                                   ~-log[10](italic(p[min]))), main = deparse(substitute(x)),
                       las = par("las"), subtitle = paste0("(Considering minimum of ",
                                                           number_of_tests, " independent p-value-series)"), envelope = 0.95,
                       col = palette()[1], col.lines = palette()[2], lwd = 2, pch = 16,
                       cex = par("cex"), labels = if (!is.null(names(x))) names(x) else seq(along = x),
                       id.method = "y", id.n = if (id.method[1] == "identify") Inf else 0,
                       id.cex = 1, id.col = palette()[1], id.location = "lr", grid = TRUE,
                       ...)
{
  #  inspired by car::qqPlot and qq_conf

  #rows = 10000
  # bsp = data.table(a = runif(rows), b = runif(rows), c = runif(rows),
  #                  num = 1:rows)
  # bsp[, `:=`(mini, min(.SD)), by = num]
  # bsp
  # bsp[, qqPlotMinP(mini, ylim = c(0, 7))]
  line <- "identity"
  good <- !is.na(x)
  ord <- order(x[good])
  if (length(col) == length(x))
    col <- col[good][ord]
  if (length(pch) == length(x))
    pch <- pch[good][ord]
  if (length(cex) == length(x))
    cex <- cex[good][ord]
  ord.x <- x[good][ord]
  ord.lab <- labels[good][ord]
  n <- length(ord.x)
  P <- ppoints(n)
  z <- qbeta(P, shape1 = 1, shape2 = number_of_tests)
  z_plot = -log10(z)
  ord.x_plot = -log10(ord.x)
  plot(z_plot, ord.x_plot, type = "n", xlab = xlab, ylab = ylab,
       main = main, las = las, ...)
  # plot(z_plot, ord.x_plot, type = "n", xlab = xlab, ylab = ylab, main = main, las = las)
  if (grid) {
    grid(lty = 1, equilogs = F)
    box()
  }
  points(z_plot, ord.x_plot, col = col, pch = pch, cex = cex)
  mtext(subtitle)
  if (line == "identity") {
    a = 0
    b = 1
    abline(0, 1, col = col.lines, lwd = lwd)
  }
  conf <- if (envelope == FALSE) .95 else envelope
  upper = qbeta(p = qbeta(p = (1 + conf)/2, 1:n, n:1), shape1 = 1, shape2 = number_of_tests)
  lower = qbeta(p = qbeta(p = (1 - conf)/2, 1:n, n:1), shape1 = 1, shape2 = number_of_tests)
  lowergr0 = lower > 0
  table(lowergr0)
  if (envelope != FALSE) {
    lines(z_plot, -log10(upper), lty = 2, lwd = lwd, col = col.lines)
    lines(z_plot[lowergr0], -log10(lower[lowergr0]), lty = 2,
          lwd = lwd, col = col.lines)
  }

  showLabels = function (x, y, labels = NULL, id.method = "identify", id.n = length(x),
                         id.cex = 1, id.col = palette()[1], id.location = "lr", ...)
  {
    res <- NULL
    id.method <- if (is.list(id.method))
      id.method
    else list(id.method)
    for (meth in id.method) res <- c(res, showLabels1(x, y, labels,
                                                      meth, id.n, id.cex, id.col, id.location, ...))
    return(if (is.null(res)) invisible(res) else res)
  }

  invisible(data.table::data.table(z, ord.x, upper, lower))
}
