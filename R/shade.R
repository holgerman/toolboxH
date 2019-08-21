  shade <- function(x1, y1, x2, y2, color=col.shade) {
    n <- length(x2)
    polygon(c(x1, x2[n:1]), c(y1, y2[n:1]), border=NA, col=color)
  }
