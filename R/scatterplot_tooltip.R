scatterplot_tooltip = function(xx,yy,tooltip, mycolor = "",mysize=5,  tooltipprefix ="", ...)
{
  #   xx = 1:1000
  #   yy = asinh(1:1000)
  #   tooltip = paste0("label", 1:1000)
  # library(data.table)
  library(metricsgraphics)

  dat = data.frame(xx,yy,tooltip, mycolor,mysize)
  names(dat) = c("myxx", 'myyy', 'mytooltip', 'mycolor', 'mysize')

  mystring = "function(d) {
  $('{{ID}} svg .mg-active-datapoint')
  .text('custom text : ' + d.point.mytooltip );
}"
  mystring = stringr::str_replace(mystring, "custom text :", tooltipprefix)

  dat %>%
    mjs_plot(x = myxx, y = myyy) %>%
    mjs_point(color_accessor = "mycolor",size_accessor='mysize', ...) %>%
    mjs_add_mouseover(mystring)
}
