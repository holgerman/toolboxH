venn4 = function(x1,y1,z1,w1, mytitle="4-Way Venn Diagram", mylabels = NA,  plotte =T, venntype = '4el')
{
  # 13/07/03
  # 150119 vector check
  if(venntype=="4map") venntype = "4elmap"
  if(all(is.vector(x1)|is.factor(x1),is.vector(y1)|is.factor(y1),is.vector(z1)|is.factor(z1),is.vector(w1)|is.factor(w1))==F) stop("All input data must be vectors...")

  if(is.na(mylabels[1])) mylabels = c(deparse(substitute(x1)), deparse(substitute(y1)), deparse(substitute(z1)), deparse(substitute(w1)))
  qlist <- venndiagram(x=x1, y=y1, z=z1, w=w1, unique=T, title=mytitle, labels=mylabels,
                       plot=plotte, lines=c(2,3,4,6), lcol=c(2,3,4,6), tcol=1, lwd=3, cex=1, printsub=T, type=venntype)
  qlist
}
