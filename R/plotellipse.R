#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param center PARAM_DESCRIPTION, Default: c(1, 1)
#' @param radius PARAM_DESCRIPTION, Default: c(1, 2)
#' @param rotate PARAM_DESCRIPTION, Default: 1
#' @param segments PARAM_DESCRIPTION, Default: 360
#' @param xlab PARAM_DESCRIPTION, Default: ''
#' @param ylab PARAM_DESCRIPTION, Default: ''
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname plotellipse
#' @export 
      plotellipse <- function (center=c(1,1), radius=c(1,2), rotate=1, segments=360, xlab="", ylab="", ...) {
        angles <- (0:segments) * 2 * pi/segments
        rotate <- rotate*pi/180
        ellipse <- cbind(radius[1] * cos(angles), radius[2] * sin(angles))
        ellipse <- cbind( ellipse[,1]*cos(rotate) + ellipse[,2]*sin(rotate), ellipse[,2]*cos(rotate) - ellipse[,1]*sin(rotate) )
        ellipse <- cbind(center[1]+ellipse[,1], center[2]+ellipse[,2])
        plot(ellipse, type = "l", xlim = c(0, 10), ylim = c(0, 10), xlab = "", ylab = "", ...)
      }
