showStars = function (p)
{
  if (inherits(p, c("matrix", "data.frame")) && length(dim(p)) ==
      2) {
    apply(p, c(1, 2), showStars)
  }
  else {
    if (length(p) > 1) {
      sapply(p, showStars)
    }
    else {
      s <- ifelse(p > 0.05, "", ifelse(p > 0.01, "*",
                                       ifelse(p > 0.001, "**", "***")))
      s
    }
  }
}
