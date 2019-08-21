

### pdf file as graphical png in order to avoid long startup time if 100 of thousends points are included

pdf_from_png = function (code2parseOrPlot, pdf_filename, temp_pngfile = tempfile(),
                         resolution = 300, weite = 13, laenge = 13, einheiten = "cm",
                         as1file = T, ...)
{ # fixed 18.8.17
  png(temp_pngfile, weite, laenge, units = einheiten,
      res = resolution)
  if (is.character(code2parseOrPlot))
    eval(parse(text = code2parseOrPlot))
  else plot(code2parseOrPlot)
  dev.off()
  pdf(pdf_filename, weite, laenge, onefile = as1file, ...)
  par(mai = c(0, 0, 0, 0))
  plotPNG = png::readPNG(temp_pngfile)
  plot(c(0, 1), c(0, 1), type = "n")
  rasterImage(plotPNG, 0, 0, 1, 1)
  dev.off()
}
