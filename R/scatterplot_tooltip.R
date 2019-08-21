#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param xx PARAM_DESCRIPTION
#' @param yy PARAM_DESCRIPTION
#' @param tooltip PARAM_DESCRIPTION
#' @param mycolor PARAM_DESCRIPTION, Default: ''
#' @param mysize PARAM_DESCRIPTION, Default: 5
#' @param tooltipprefix PARAM_DESCRIPTION, Default: ''
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
#' @rdname scatterplot_tooltip
#' @export 
#' @import stringr
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
