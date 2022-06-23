
## faerbe die zahlen einer gegebenen matrix nach ihrem oder einem externen WErt im 2 color schemewert

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param plotdat PARAM_DESCRIPTION
#' @param mycolors PARAM_DESCRIPTION, Default: c("dodgerblue2", "white", "red")
#' @param lowest_colorval PARAM_DESCRIPTION, Default: 'minimum'
#' @param middle_colorval PARAM_DESCRIPTION, Default: 'median'
#' @param highest_colorval PARAM_DESCRIPTION, Default: 'maximum'
#' @param xlabel PARAM_DESCRIPTION, Default: ''
#' @param ylabel PARAM_DESCRIPTION, Default: ''
#' @param x_axis_pos PARAM_DESCRIPTION, Default: 'top'
#' @param myround PARAM_DESCRIPTION, Default: 0
#' @param userdefined_labels PARAM_DESCRIPTION, Default: NULL
#' @param row_names PARAM_DESCRIPTION, Default: NULL
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
#' @rdname colplotLikeExcel
#' @export
#' @import scales
colplotLikeExcel = function(plotdat, mycolors = c("dodgerblue2", "white", "red"),lowest_colorval = "minimum", middle_colorval = "median", highest_colorval = "maximum", xlabel = "", ylabel = "", x_axis_pos = "top", myround = 0, userdefined_labels = NULL, row_names = NULL, sort_via_value = T){
  plotdat_m = reshape2::melt(plotdat) %>% data.table()
  var1name = names(plotdat_m)[1]
  var2name = names(plotdat_m)[2]

  if(sort_via_value==T) plotdat_m = plotdat_m[order(value, decreasing = T)]
  setnames(plotdat_m, c(var1name, var2name), c("Var1", "Var2"))

  if (sort_via_value==T) {
    plotdat_m$Var1 = factor(plotdat_m$Var1, levels = rev(unique(plotdat_m$Var1)))
  }  else plotdat_m$Var1 = factor(plotdat_m$Var1, levels = rev(unique(rownames(plotdat))))

  sort(unique(plotdat_m$Var1))


  if (sort_via_value==T) {
    plotdat_m$Var2 = factor(plotdat_m$Var2, levels = unique(plotdat_m$Var2))
  }  else plotdat_m$Var2 = factor(plotdat_m$Var2, levels = unique(colnames(plotdat)))

  if (is.numeric(plotdat_m$value) == F) {
    stop("Need numeric matrix as `plotdat` argument in order to know coloring according to provided numeric data! Stoping.\nConsider providing a userdefined matrix with names via parameter `userdefined_labels` in addition to providing numeric values via parameter `plotdat`.")}
  plotdat_m$value = round(plotdat_m$value, myround)
  if (lowest_colorval == "minimum") {
    lowest_colorval = min(plotdat_m$value, na.rm = T)
  }  else lowest_colorval = lowest_colorval
  if (middle_colorval == "median") {
    middle_colorval = stats::median(plotdat_m$value, na.rm = T)
  }  else middle_colorval = middle_colorval
  if (highest_colorval == "maximum") {
    highest_colorval = max(plotdat_m$value, na.rm = T)
  }  else highest_colorval = highest_colorval

  if (is.null(userdefined_labels)) {
    plot1 = ggplot2::ggplot(plotdat_m, ggplot2::aes(Var2,
                                                    Var1, label = value)) + ggplot2::geom_tile(ggplot2::aes(fill = value),
                                                                                               colour = "white") + ggplot2::scale_fill_gradientn(colours = mycolors,
                                                                                                                                                 values = scales::rescale(c(lowest_colorval, middle_colorval,
                                                                                                                                                                            highest_colorval)), guide = FALSE) + ggplot2::geom_text(show.legend = FALSE) +
      ggplot2::scale_x_discrete(position = x_axis_pos) +
      ggplot2::xlab(xlabel) + ggplot2::ylab(ylabel)
  }  else     {

    beschriftdat = as.matrix(userdefined_labels)
    beschriftdat = beschriftdat[rev(rownames(beschriftdat)),
    ]
    stopifnot(identical(dim(plotdat), dim(beschriftdat)))
    stopifnot(identical(rownames(plotdat), rownames(beschriftdat)))
    stopifnot(identical(colnames(plotdat), colnames(beschriftdat)))
    beschriftdat_m = data.table::melt(beschriftdat)
    plot1 = ggplot2::ggplot(plotdat_m, ggplot2::aes(Var2,
                                                    Var1)) + ggplot2::geom_tile(ggplot2::aes(fill = value),
                                                                                colour = "white") + ggplot2::scale_fill_gradientn(colours = mycolors,
                                                                                                                                  values = scales::rescale(c(lowest_colorval, middle_colorval,
                                                                                                                                                             highest_colorval)), guide = "none") + ggplot2::geom_text(label = beschriftdat_m$value,
                                                                                                                                                                                                                      show.legend = FALSE) + ggplot2::scale_x_discrete(position = x_axis_pos) +
      ggplot2::xlab(xlabel) + ggplot2::ylab(ylabel)
  }
  plot1 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                             hjust = 0)) + xlab(var2name) + ylab(var1name)
}
