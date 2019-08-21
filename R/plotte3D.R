plotte3D = function (x, y, z, farbe, mylabels="", mysize = 0.6)
{
  require(threejs)
  farbentopf = rainbow(length(unique(farbe)))
  farbe = as.character(factor(farbe, labels = farbentopf))
  try(scatterplot3js(cbind(x, y, z), color = farbe,
                     labels = mylabels, size = mysize, renderer = "auto"))
}
