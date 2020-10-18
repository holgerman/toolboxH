#' @title Save Plot in multiple Formats
#' @description Saves Plot as jpg, pdf, svg, eps the latter two for Adobe Illustrator or freeware Inkscape
#' @param corefilename filename (optional incl. path) without .*** file extension
#' @param weite width in inches, Default: 7
#' @param hoehe hight in inches, Default: 7
#' @param plotecode Any code used for plotting a graphic, as String
#' @param mymfrow mfrow parameter, relevant for base graphic only, Default: c(1, 1)
#' @param saveSVG save as SVG, Default: T
#' @param savePDF save as PDF, Default: T
#' @param saveEPS save as EPS, Default: T
#' @param saveJPG save as JPEG, Default: T
#' @param jpg_res REsoluation of Jpec, Default: 300
#' @return Files in the specified location
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  data(cars)
#'  savePlotInMultiFormats("Carsplot", "plot(cars$speed ~ cars$dist)")
#'
#'  #EXAMPLE1
#'  data(cars)
#'  require(ggplot2)
#'  p1 = ggplot(cars, aes(dist, speed)) + geom_point()
#'  savePlotInMultiFormats("Carsplot", 'plot(p1)')
#'
#'  }
#' }
#' @rdname savePlotInMultiFormats
#' @export
savePlotInMultiFormats <- function(corefilename, plotecode, weite=7, hoehe=7, mymfrow  = c(1,1), saveSVG=T, savePDF=T, saveEPS=T, saveJPG=T, jpg_res=300) {
  if(saveJPG) {
    myfilename = paste0(corefilename, ".jpg")
    message('saving ', myfilename)

    jpeg(filename = myfilename,
         width = weite,height = hoehe,  unit = "in",quality  = 100,
         res = jpg_res)

    par(mfrow = mymfrow)
    eval(parse(text = plotecode))

    dev.off()
  }

  if(saveSVG) {
    myfilename = paste0(corefilename, ".svg")
    message('saving ', myfilename)
    svg(myfilename, weite, hoehe )
    par(mfrow = mymfrow)
    eval(parse(text = plotecode))

    dev.off()
  }

  if(savePDF){
    myfilename = paste0(corefilename, ".pdf")
    message('saving ', myfilename)

    pdf(myfilename, weite, hoehe )
    par(mfrow = mymfrow)
    eval(parse(text = plotecode))

    dev.off()}

  if(saveEPS){
    myfilename = paste0(corefilename, ".eps")
    message('saving ', myfilename)

    cairo_ps(filename = myfilename,
             width = weite,height = hoehe,  pointsize = 12,
             fallback_resolution = 1200)
    par(mfrow = mymfrow)
    eval(parse(text = plotecode))

    dev.off()
  }


}
