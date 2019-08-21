## test auf ganzezahlen
is.wholenumber <-   function(x, tol = .Machine$double.eps^0.5) {
  x <- na.omit(x)
  if(is.numeric(x) ==F) return(F)
  if(is.numeric(x) ==T)  abs(x - round(x)) < tol
}
