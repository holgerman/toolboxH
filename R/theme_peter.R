#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param beschriftungszahlengroesse PARAM_DESCRIPTION, Default: 20
#' @param relfak PARAM_DESCRIPTION, Default: 2
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname theme_peter
#' @export 
theme_peter = function(beschriftungszahlengroesse = 20, relfak = 2) {
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param beschriftungszahlengroesse PARAM_DESCRIPTION, Default: 20
#' @param relfak PARAM_DESCRIPTION, Default: 2
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname theme_peter
#' @export 
  theme_peter = ggplot2::theme_grey() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=0, hjust=0, size = beschriftungszahlengroesse), axis.text.y = ggplot2::element_text(size = beschriftungszahlengroesse), axis.title= ggplot2::element_text(size = beschriftungszahlengroesse), legend.title=ggplot2::element_text(size = ggplot2::rel(relfak)), legend.text=ggplot2::element_text(size = ggplot2::rel(relfak)), strip.text = ggplot2::element_text(face="bold", size=ggplot2::rel(relfak)))
  theme_peter
}
