
### qqplot from getting genetics done
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pvector PARAM_DESCRIPTION
#' @param maxx PARAM_DESCRIPTION, Default: NULL
#' @param maxy PARAM_DESCRIPTION, Default: NULL
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
#' @rdname qq.ggd
#' @export
#' @import stats
#' @import graphics

qq.ggd = function ( pvector ,maxx = NULL, maxy = NULL, ... ) {
  #   #23/4/13 p = 0 abgefangen
  #   #06/05/2013 fehler korrigiert p <=1 statt p < 1
  # 3.2.15 p = 0 auf min p statt .Machine$double.xmin

  if ( !is.numeric ( pvector )) stop ( "D'oh! P value vector is not numeric." )

  if(any(pvector >1 | pvector <0 )) stop("Neg. values or values larger 1 found in pvector...Please remove them!")

  if(any(is.na(pvector))) {
    message("ooops, NA found in pvector...removing ", sum(is.na(pvector)), " entries...")
    pvector = pvector[ is.na(pvector) ==F]
  }

  pvector[pvector ==0] = min(pvector[ pvector >0])

  o = - log10 ( sort ( pvector , decreasing = F ))
  #e = -log10( 1:length(o)/length(o) )
  e = - log10 ( stats::ppoints ( length ( pvector ) ))

  if(is.null(maxx)) maxx = max ( e )
  if(is.null(maxy)) maxy =  max ( o )

  graphics::plot ( e , o , pch = 19 , cex = 1 , xlab = expression ( Expected~~ - log [ 10 ]( grDevices::italic ( p ))), ylab = expression ( Observed~~ - log [ 10 ]( grDevices::italic ( p ))), xlim = c ( 0 , maxx), ylim = c ( 0 ,maxy), ... )
  graphics::abline ( 0 , 1 , col = "red" )
}
