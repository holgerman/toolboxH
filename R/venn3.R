#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x1 PARAM_DESCRIPTION
#' @param y1 PARAM_DESCRIPTION
#' @param z1 PARAM_DESCRIPTION
#' @param mytitle PARAM_DESCRIPTION, Default: '3-Way Venn Diagram'
#' @param mylabels PARAM_DESCRIPTION, Default: NA
#' @param plotte PARAM_DESCRIPTION, Default: T
#' @param venntype PARAM_DESCRIPTION, Default: '3'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname venn3
#' @export 
venn3 = function(x1,y1,z1, mytitle="3-Way Venn Diagram", mylabels = NA,  plotte =T, venntype = '3')
{
  # 28/2/13 plotte par
  # 150119 vector check
  if(all(is.vector(x1)|is.factor(x1),is.vector(y1)|is.factor(y1),is.vector(z1)|is.factor(z1))==F) stop("All input data must be vectors...")

  if(is.na(mylabels[1])) mylabels = c(deparse(substitute(x1)), deparse(substitute(y1)), deparse(substitute(z1)))
  qlist <- venndiagram(x=x1, y=y1, z=z1, unique=T, title=mytitle, labels= mylabels, plot=plotte, lines=c(2,3,4), lcol=c(2,3,4), tcol=c(1,1,1,1,1,1,1), lwd=3, cex=1.3, printsub=T, type=venntype)
  qlist
}
