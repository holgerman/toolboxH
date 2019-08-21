## faerbe die zahlen einer gegebenen quadratisch -symmetrischen matrix nach ihrem wertm im 2 colordesign
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param m_psy PARAM_DESCRIPTION
#' @param relaxissize PARAM_DESCRIPTION, Default: 2
#' @param rellabelsize PARAM_DESCRIPTION, Default: 15
#' @param textangle PARAM_DESCRIPTION, Default: 30
#' @param maxcolor PARAM_DESCRIPTION, Default: 'darkred'
#' @param mincolor PARAM_DESCRIPTION, Default: 'white'
#' @param rundestellen PARAM_DESCRIPTION, Default: 2
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname plotCorMatrix
#' @export 
plotCorMatrix <- function (m_psy, relaxissize = 2,rellabelsize = 15,textangle = 30,maxcolor = "darkred",mincolor = "white",rundestellen = 2) {
  library(ggplot2)
  library(reshape2)
  m_psy_melt = melt(m_psy)
  names(m_psy_melt) = c('Var1', 'Var2', 'value')

  labelcolor = maxcolor
  m_psy_melt$value = round(m_psy_melt$value,rundestellen)
  p_psybind <- ggplot(m_psy_melt, aes(Var1, Var2, label=value)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_gradient(low = mincolor, high = maxcolor) + geom_text(size  = rel(rellabelsize)) + xlab("") + ylab("") + theme(axis.text.x = element_text(angle=textangle, hjust=1, vjust=1), axis.text = element_text(color = labelcolor, size=rel(relaxissize))) + guides(fill=FALSE)
  p_psybind
}
