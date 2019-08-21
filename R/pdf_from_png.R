

### pdf file as graphical png in order to avoid long startup time if 100 of thousends points are included

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param code2parseOrPlot PARAM_DESCRIPTION
#' @param pdf_filename PARAM_DESCRIPTION
#' @param temp_pngfile PARAM_DESCRIPTION, Default: tempfile()
#' @param resolution PARAM_DESCRIPTION, Default: 300
#' @param weite PARAM_DESCRIPTION, Default: 13
#' @param laenge PARAM_DESCRIPTION, Default: 13
#' @param einheiten PARAM_DESCRIPTION, Default: 'cm'
#' @param as1file PARAM_DESCRIPTION, Default: T
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  
#' @rdname pdf_from_png
#' @export 
#' @import png
pdf_from_png = function (code2parseOrPlot, pdf_filename, temp_pngfile = tempfile(),
                         resolution = 300, weite = 13, laenge = 13, einheiten = "cm",
                         as1file = T, ...)
{ # fixed 18.8.17
  grDevices::png(temp_pngfile, weite, laenge, units = einheiten,
      res = resolution)
  if (is.character(code2parseOrPlot))
    eval(parse(text = code2parseOrPlot))
  else graphics::plot(code2parseOrPlot)
  grDevices::dev.off()
  grDevices::pdf(pdf_filename, weite, laenge, onefile = as1file, ...)
  graphics::par(mai = c(0, 0, 0, 0))
  plotPNG = png::readPNG(temp_pngfile)
  graphics::plot(c(0, 1), c(0, 1), type = "n")
  graphics::rasterImage(plotPNG, 0, 0, 1, 1)
  grDevices::dev.off()
}
