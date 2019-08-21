##..................................................................................
## data plotting
##..................................................................................
### grayplot na
#  grayplot_na
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION, Default: NULL
#' @param type PARAM_DESCRIPTION, Default: 'p'
#' @param bgcolor PARAM_DESCRIPTION, Default: 'gray90'
#' @param v_over_h PARAM_DESCRIPTION, Default: FALSE
#' @param pch PARAM_DESCRIPTION, Default: 21
#' @param bg PARAM_DESCRIPTION, Default: 'lightblue'
#' @param col PARAM_DESCRIPTION, Default: 'black'
#' @param force PARAM_DESCRIPTION, Default: c("none", "x", "y", "both")
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname grayplot_na
#' @export 
grayplot_na <-
  function(x, y=NULL, type="p", bgcolor="gray90", v_over_h=FALSE,
           pch=21, bg="lightblue", col="black",
           force=c("none", "x", "y", "both"), ...)
  {
    # from https://github.com/kbroman/broman/blob/master/R/grayplot_na.R

    if(missing(x) || is.null(x)) stop("x unspecified")
    force <- match.arg(force)

    # this is to deal with varying inputs (did "..." include xaxt or not?)
    hidegrayplot_na <-
      function(x, y, ..., type="p",
               hlines=NULL, hlines.col="white", hlines.lty=1, hlines.lwd=1,
               vlines=NULL, vlines.col="white", vlines.lty=1, vlines.lwd=1,
               xat=NULL, yat=NULL, bgcolor="gray90", xaxt="n", yaxt="n",
               col.lab=graphics::par("col.lab"),
               xlim=NULL, ylim=NULL,
               xlab=NULL, ylab=NULL, xname, yname,
               xaxs="i", yaxs="i",
               pch=21, bg="lightblue", col="black",
               las=1, mgp.x=c(2.6, 0.5, 0), mgp.y=c(2.6, 0.5, 0),
               force=c("none", "x", "y", "both"),
               v_over_h=FALSE, na.width=0.06, na.gap=0.01,
               main="")
      {
        force <- match.arg(force)
        dots <- list(...)

        if(na.width >= 1 || na.width <= 0)
          stop("na.width must be between 0 and 1")
        if(na.gap >= na.width || na.gap <= 0)
          stop("na.gap must be between 0 and na.width")

        if("mgp" %in% names(dots) && missing(mgp.x))
          mgp.x <- dots$mgp
        if("mgp" %in% names(dots) && missing(mgp.y))
          mgp.y <- dots$mgp

        if(is.null(y)) {
          if(is.null(xlab)) xlab <- "Index"
          if(is.null(ylab)) ylab <- xname
          y <- x
          x <- seq(along=x)
        }
        else {
          if(is.null(xlab)) xlab <- xname
          if(is.null(ylab)) ylab <- yname
        }

        if(is.null(ylim)) {
          ylim <- range(y, na.rm=TRUE)
          # make sure ylim[1] < ylim[2]
          if(diff(ylim) < 2e-6) ylim <- ylim + c(-1,1)*1e-6
          ylim[1] <- ylim[1]-diff(ylim)*0.02
          ylim[2] <- ylim[2]+diff(ylim)*0.02
        }
        if(is.null(hlines)) {
          if(!is.null(yat))
            hlines <- yat
          else
            hlines <- pretty(ylim)
        }
        else if(length(hlines)==1 && is.na(hlines))
          hlines <- NULL

        if(is.null(xlim)) {
          xlim <- range(x, na.rm=TRUE)
          # make sure xlim[1] < xlim[2]
          if(diff(xlim) < 2e-6) xlim <- xlim + c(-1,1)*1e-6
          xlim[1] <- xlim[1]-diff(xlim)*0.02
          xlim[2] <- xlim[2]+diff(xlim)*0.02
        }
        if(is.null(vlines)) {
          if(!is.null(xat))
            vlines <- xat
          else
            vlines <- pretty(xlim)
        }
        else if(length(vlines)==1 && is.na(vlines))
          vlines <- NULL
        if(is.null(xat)) {
          xat <- pretty(xlim)
          xat <- xat[xat >= xlim[1] & xat <= xlim[2]]
        }
        if(is.null(yat)) {
          yat <- pretty(ylim)
          yat <- yat[yat >= ylim[1] & yat <= ylim[2]]
        }
        if(!is.null(vlines)) vlines <- vlines[vlines >= xlim[1] & vlines <= xlim[2]]
        if(!is.null(hlines)) hlines <- hlines[hlines >= ylim[1] & hlines <= ylim[2]]


        pin <- graphics::par("pin")
        xna.width <- pin[1]*na.width
        yna.width <- pin[2]*na.width
        width <- max(c(xna.width, yna.width))
        xna.width <- width/pin[1]
        yna.width <- width/pin[2]
        xna.gap <- pin[1]*na.gap
        yna.gap <- pin[2]*na.gap
        gap <- max(c(xna.gap, yna.gap))
        xna.gap <- gap/pin[1]
        yna.gap <- gap/pin[2]

        # whether to include the x and y NA boxes
        x_na <- y_na <- FALSE
        if(any(is.na(x)) || force %in% c("x", "both")) x_na <- TRUE
        if(any(is.na(y)) || force %in% c("y", "both")) y_na <- TRUE

        if(x_na) {
          xlim_expand <- c(xlim[2]-diff(xlim)/(1-xna.width), xlim[2])
          xlim_na <- c(xlim_expand[1], xlim_expand[1]+(xlim[1]-xlim_expand[1])*(1-xna.gap/xna.width))
          if(!is.null(vlines)) vlines <- c(vlines, mean(xlim_na))
        } else {
          xlim_expand <- xlim
        }
        if(y_na) {
          ylim_expand <- c(ylim[2]-diff(ylim)/(1-yna.width), ylim[2])
          ylim_na <- c(ylim_expand[1], ylim_expand[1]+(ylim[1]-ylim_expand[1])*(1-yna.gap/yna.width))
          if(!is.null(hlines)) hlines <- c(hlines, mean(ylim_na))
        } else {
          ylim_expand <- ylim
        }

        graphics::plot(x, y, type="n", xaxt="n", yaxt="n", xlab="", ylab="",
             xlim=xlim_expand, ylim=ylim_expand, xaxs="i", yaxs="i", bty="n",
             main=main)
        graphics::rect(xlim[1], ylim[1], xlim[2], ylim[2], col=bgcolor, border="black")
        if(x_na) graphics::rect(xlim_na[1], ylim[1], xlim_na[2], ylim[2],
                      col=bgcolor, border="black", xpd=TRUE)
        if(y_na) graphics::rect(xlim[1], ylim_na[1], xlim[2], ylim_na[2],
                      col=bgcolor, border="black", xpd=TRUE)
        if(x_na && y_na) graphics::rect(xlim_na[1], ylim_na[1], xlim_na[2], ylim_na[2],
                              col=bgcolor, border="black", xpd=TRUE)

        # axis titles
        graphics::title(xlab=xlab, mgp=mgp.x, col.lab=col.lab)
        graphics::title(ylab=ylab, mgp=mgp.y, col.lab=col.lab)

        # x axis: if adding white lines, skip the tick marks and move the numbers closer
        if(!(length(xat)==1 && is.na(xat))) { # if a single NA, skip x-axis
          if(!is.null(vlines)) {
            graphics::axis(side=1, at=xat, mgp=mgp.x, tick=FALSE, las=las)
            if(x_na) graphics::axis(side=1, at=mean(xlim_na), "NA", mgp=mgp.x, tick=FALSE, las=las)
          } else {
            graphics::axis(side=1, at=xat, las=las)
            if(x_na) graphics::axis(side=1, at=mean(xlim_na), "NA", las=las)
          }
        }

        # y axis: like the x-axis
        if(!(length(yat)==1 && is.na(yat))) { # if a single NA, skip y-axis
          if(!is.null(hlines)) {
            graphics::axis(side=2, at=yat, mgp=mgp.y, tick=FALSE, las=las)
            if(y_na) graphics::axis(side=2, at=mean(ylim_na), "NA", mgp=mgp.y, tick=FALSE, las=las)
          } else {
            graphics::axis(side=2, at=yat, las=las)
            if(y_na) graphics::axis(side=2, at=mean(ylim_na), "NA", las=las)
          }
        }

        if(!is.null(vlines) && !v_over_h)
          graphics::abline(v=vlines, col=vlines.col, lty=vlines.lty, lwd=vlines.lwd)
        if(!is.null(hlines))
          graphics::abline(h=hlines, col=hlines.col, lty=hlines.lty, lwd=hlines.lwd)
        if(!is.null(vlines) && v_over_h)
          graphics::abline(v=vlines, col=vlines.col, lty=vlines.lty, lwd=vlines.lwd)

        graphics::points(x, y, pch=pch, bg=bg, col=col, type=type, ...)
        if(x_na) {
          n_na <- sum(is.na(x) & !is.na(y))
          if(n_na > 0) {
            xnapos <- stats::runif(n_na, xlim_na[1]+diff(xlim_na)*0.2, xlim_na[2]-diff(xlim_na)*0.2)
            graphics::points(xnapos, y[is.na(x) & !is.na(y)],
                   pch=pch, bg=bg, col=col, ...)
          }
        }
        if(y_na) {
          n_na <- sum(is.na(y) & !is.na(x))
          if(n_na > 0) {
            ynapos <- stats::runif(n_na, ylim_na[1]+diff(ylim_na)*0.2, ylim_na[2]-diff(ylim_na)*0.2)
            graphics::points(x[!is.na(x) & is.na(y)], ynapos,
                   pch=pch, bg=bg, col=col, ...)
          }
        }
        if(x_na & y_na) {
          n_na <- sum(is.na(y) & is.na(x))
          if(n_na > 0) {
            xnapos <- stats::runif(n_na, xlim_na[1]+diff(xlim_na)*0.2, xlim_na[2]-diff(xlim_na)*0.2)
            ynapos <- stats::runif(n_na, ylim_na[1]+diff(ylim_na)*0.2, ylim_na[2]-diff(ylim_na)*0.2)
            graphics::points(xnapos, ynapos, pch=pch, bg=bg, col=col, ...)
          }

        }

        # add black borders
        graphics::rect(xlim[1], ylim[1], xlim[2], ylim[2], col=NULL, border="black")
        if(x_na) graphics::rect(xlim_na[1], ylim[1], xlim_na[2], ylim[2],
                      col=NULL, border="black", xpd=TRUE)
        if(y_na) graphics::rect(xlim[1], ylim_na[1], xlim[2], ylim_na[2],
                      col=NULL, border="black", xpd=TRUE)
        if(x_na && y_na) graphics::rect(xlim_na[1], ylim_na[1], xlim_na[2], ylim_na[2],
                              col=NULL, border="black", xpd=TRUE)
      }

    hidegrayplot_na(x=x, y=y, type=type, bgcolor=bgcolor,
                    xname=substitute(x), yname=substitute(y),
                    pch=pch, bg=bg, col=col,
                    v_over_h=v_over_h, force=force, ...)
    invisible()
  }
