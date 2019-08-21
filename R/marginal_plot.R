
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @param group PARAM_DESCRIPTION, Default: NULL
#' @param data PARAM_DESCRIPTION, Default: NULL
#' @param lm_formula PARAM_DESCRIPTION, Default: y ~ x
#' @param bw PARAM_DESCRIPTION, Default: 'nrd0'
#' @param alpha PARAM_DESCRIPTION, Default: 1
#' @param plot_legend PARAM_DESCRIPTION, Default: T
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
#' @rdname marginal_plot
#' @export 
#' @import scales
marginal_plot = function(x, y, group = NULL, data = NULL, lm_formula = y ~ x, bw = "nrd0", alpha = 1, plot_legend = T, ...){
  # https://github.com/ChrKoenig/R_marginal_plot
  require(scales)
  ###############
  # Plots a scatterplot with marginal probability density functions for x and y.
  # Data may be grouped or ungrouped.
  # For each group, a linear fit is plotted. The model can be modified using the 'lm_formula' argument. Setting 'lm_formula' to NULL prevents plotting model fits.
  # The 'bw' argument specifies the bandwidth rule used for estimating probability density functions. See ?density for more information.
  # For large datasets, opacity may be decreased by setting alpha to a value between 0 and 1.
  # Additional graphical parameters are passed to the main plot, so you can customize axis labels, titles etc.
  ###############
  moreargs = eval(substitute(list(...)))

  # prepare consistent df
  if(missing(group)){
    if(missing(data)){
      if(length(x) != length(y)){stop("Length of arguments not equal")}
      data = data.frame(x = as.numeric(x), y = as.numeric(y))
    } else {
      data = data.frame(x = as.numeric(data[,deparse(substitute(x))]),
                        y = as.numeric(data[,deparse(substitute(y))]))
    }
    if(sum(!stats::complete.cases(data)) > 0){
      warning(sprintf("Removed %i rows with missing data", sum(!stats::complete.cases(data))))
      data = data[stats::complete.cases(data),]
    }
    group_colors = "black"
  } else {
    if(missing(data)){
      if(length(x) != length(y) | length(x) != length(group)){stop("Length of arguments not equal")}
      data = data.frame(x = as.numeric(x), y = as.numeric(y), group = as.factor(group))
    } else {
      data = data.frame(x = as.numeric(data[,deparse(substitute(x))]),
                        y = as.numeric(data[,deparse(substitute(y))]),
                        group = as.factor(data[,deparse(substitute(group))]))
    }
    if(sum(!stats::complete.cases(data)) > 0){
      warning(sprintf("Removed %i rows with missing data", sum(!stats::complete.cases(data))))
      data = data[stats::complete.cases(data),]
    }
    data = subset(data, group %in% names(which(table(data$group) > 5)))
    data$group = droplevels(data$group)
    group_colors = grDevices::rainbow(length(unique(data$group)))
  }

  # log-transform data (this is need for correct plotting of density functions)
  if(!is.null(moreargs$log)){
    if(!moreargs$log %in% c("y", "x", "yx", "xy")){
      warning("Ignoring invalid 'log' argument. Use 'y', 'x', 'yx' or 'xy.")
    } else {
      data = data[apply(data[unlist(strsplit(moreargs$log, ""))], 1, function(x) !any(x <= 0)), ]
      data[,unlist(strsplit(moreargs$log, ""))] = log10(data[,unlist(strsplit(moreargs$log, ""))])
    }
    moreargs$log = NULL # remove to prevent double logarithm when plotting
  }

  # Catch unwanted user inputs
  if(!is.null(moreargs$col)){moreargs$col = NULL}
  if(!is.null(moreargs$type)){moreargs$type = "p"}

  # get some default plotting arguments
  if(is.null(moreargs$xlim)){moreargs$xlim = range(data$x)}
  if(is.null(moreargs$ylim)){moreargs$ylim = range(data$y)}
  if(is.null(moreargs$xlab)){moreargs$xlab = deparse(substitute(x))}
  if(is.null(moreargs$ylab)){moreargs$ylab = deparse(substitute(y))}
  if(is.null(moreargs$las)){moreargs$las = 1}

  # plotting
  tryCatch(expr = {
    ifelse(!is.null(data$group), data_split <- split(data, data$group), data_split <- list(data))
    orig_par = graphics::par(no.readonly = T)
    graphics::par(mar = c(0.25,5,1,0))
    graphics::layout(matrix(1:4, nrow = 2, byrow = T), widths = c(10,3), heights = c(3,10))

    # upper density plot
    graphics::plot(NULL, type = "n", xlim = moreargs$xlim, ylab = "density",
         ylim = c(0, max(sapply(data_split, function(group_set) max(stats::density(group_set$x, bw = bw)$y)))), main = NA, axes = F)
    graphics::axis(2, las = 1)
    mapply(function(group_set, group_color){graphics::lines(stats::density(group_set$x, bw = bw), col = group_color, lwd = 2)}, data_split, group_colors)

    # legend
    graphics::par(mar = c(0.25,0.25,0,0))
    graphics::plot.new()
    if(!missing(group) & plot_legend){
      graphics::legend("center", levels(data$group), fill = group_colors, border = group_colors, bty = "n", title = deparse(substitute(group)), title.adj = 0.1)
    }

    # main plot
    graphics::par(mar = c(4,5,0,0))
    if(missing(group)){
      do.call(plot, c(list(x = quote(data$x), y = quote(data$y), col = quote(scales::alpha("black", alpha))), moreargs))
    } else {
      do.call(plot, c(list(x = quote(data$x), y = quote(data$y), col = quote(scales::alpha(group_colors[data$group], alpha))), moreargs))
    }
    graphics::axis(3, labels = F, tck = 0.01)
    graphics::axis(4, labels = F, tck = 0.01)
    graphics::box()

    if(!is.null(lm_formula)){
      mapply(function(group_set, group_color){
        lm_tmp = stats::lm(lm_formula, data = group_set)
        x_coords = seq(min(group_set$x), max(group_set$x), length.out = 100)
        y_coords = stats::predict(lm_tmp, newdata = data.frame(x = x_coords))
        graphics::lines(x = x_coords, y = y_coords, col = group_color, lwd = 2.5)
      }, data_split, grDevices::rgb(t(ceiling(grDevices::col2rgb(group_colors)*0.8)), maxColorValue = 255))
    }

    # right density plot
    graphics::par(mar = c(4,0.25,0,1))
    graphics::plot(NULL, type = "n", ylim = moreargs$ylim, xlim = c(0, max(sapply(data_split, function(group_set) max(stats::density(group_set$y, bw = bw)$y)))), main = NA, axes = F, xlab = "density")
    mapply(function(group_set, group_color){graphics::lines(x = stats::density(group_set$y, bw = bw)$y, y = stats::density(group_set$y, bw = bw)$x, col = group_color, lwd = 2)}, data_split, group_colors)
    graphics::axis(1)
  }, finally = {
    graphics::par(orig_par)
  })
}
