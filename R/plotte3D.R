#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @param z PARAM_DESCRIPTION
#' @param farbe PARAM_DESCRIPTION
#' @param mylabels PARAM_DESCRIPTION, Default: ''
#' @param mysize PARAM_DESCRIPTION, Default: 0.6
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname plotte3D
#' @export 
plotte3D = function (x, y, z, farbe, mylabels="", mysize = 0.6)
{
  require(threejs)
  farbentopf = grDevices::rainbow(length(unique(farbe)))
  farbe = as.character(factor(farbe, labels = farbentopf))
  try(threejs::scatterplot3js(cbind(x, y, z), color = farbe,
                     labels = mylabels, size = mysize, renderer = "auto"))
}
