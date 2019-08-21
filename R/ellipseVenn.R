#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param lines PARAM_DESCRIPTION, Default: lines
#' @param olDF PARAM_DESCRIPTION
#' @param title PARAM_DESCRIPTION, Default: title
#' @param labels PARAM_DESCRIPTION, Default: labels
#' @param sub PARAM_DESCRIPTION, Default: mysub
#' @param main PARAM_DESCRIPTION
#' @param lcol PARAM_DESCRIPTION, Default: lcol
#' @param tcex PARAM_DESCRIPTION, Default: 1.3
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ellipseVenn
#' @export 
      ellipseVenn <- function(lines=lines, olDF, title=title, labels=labels, sub=mysub, main, lcol=lcol, tcex=1.3, ...) {
        graphics::split.screen(c(1,1))
        plotellipse(center=c(3.5,3.6), radius=c(2,4), rotate=-35, segments=360, xlab="", ylab="", col=lines[1], axes=FALSE, main=title, sub=mysub, ...)
        graphics::screen(1, new=FALSE)
        plotellipse(center=c(4.7,4.4), radius=c(2,4), rotate=-35, segments=360, xlab="", ylab="", col=lines[2], axes=FALSE, ...)
        graphics::screen(1, new=FALSE)
        plotellipse(center=c(5.3,4.4), radius=c(2,4), rotate=35, segments=360, xlab="", ylab="", col=lines[3], axes=FALSE, ...)
        graphics::screen(1, new=FALSE)
        plotellipse(center=c(6.5,3.6), radius=c(2,4), rotate=35, segments=360, xlab="", ylab="", col=lines[4], axes=FALSE, ...)
        graphics::text(olDF[1:15,1], olDF[1:15,2], olDF[1:15,3], col=tcol, ...)
        graphics::text(c(0.4, 2.8, 7.5, 9.4), c(7.3, 8.3, 8.3, 7.3), labels, col=lcol, ...)
        graphics::close.screen(all=TRUE)
      }
