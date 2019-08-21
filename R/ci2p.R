### confidence intervalls to p values see http://www.bmj.com/content/343/bmj.d2304
ci2p <- function (Est,u,l) {
  SE = (u - l)/(2*1.96)
  z = Est/SE
  P = exp(-0.717*z - 0.416*z^2)
  P
}
