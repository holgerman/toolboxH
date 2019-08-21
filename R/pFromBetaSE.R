pFromBetaSE = function(beta, se, two.sided = T, returnXminIf0 = F) {
  # beta = 0.5; se = 0.005;two.sided = T;returnXminIf0 = F
  z = beta/se
  if(two.sided==T) p = 2*pnorm(-abs(z)) else  p = pnorm(-abs(z))
  if(returnXminIf0 ==T) p = ifelse(p>1,1, ifelse(p==0, .Machine$double.xmin, p)) else p = ifelse(p>1,1,p)

  p

}
