
### QQPLOT von j morrison
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param df PARAM_DESCRIPTION, Default: 1
#' @param x.max PARAM_DESCRIPTION, Default: 'auto'
#' @param main PARAM_DESCRIPTION, Default: 'QQ plot'
#' @param plotType PARAM_DESCRIPTION, Default: 'pval'
#' @param sub PARAM_DESCRIPTION, Default: paste("")
#' @param xlab PARAM_DESCRIPTION, Default: 'Expected'
#' @param ylab PARAM_DESCRIPTION, Default: 'Observed'
#' @param conc PARAM_DESCRIPTION, Default: c(0.025, 0.975)
#' @param overdisp PARAM_DESCRIPTION, Default: FALSE
#' @param trim PARAM_DESCRIPTION, Default: 0.5
#' @param slope.one PARAM_DESCRIPTION, Default: T
#' @param slope.lambda PARAM_DESCRIPTION, Default: FALSE
#' @param thin PARAM_DESCRIPTION, Default: c(0.25, 100)
#' @param oor.pch PARAM_DESCRIPTION, Default: 24
#' @param col.shade PARAM_DESCRIPTION, Default: 'gray'
#' @param ofname PARAM_DESCRIPTION, Default: 'qqchi.pdf'
#' @param h PARAM_DESCRIPTION, Default: 6
#' @param w PARAM_DESCRIPTION, Default: 6
#' @param printpdf PARAM_DESCRIPTION, Default: F
#' @param myxlim PARAM_DESCRIPTION, Default: 'auto'
#' @param point_cex PARAM_DESCRIPTION, Default: 0.5
#' @param point_col PARAM_DESCRIPTION, Default: 1
#' @param point_pch PARAM_DESCRIPTION, Default: 1
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname qq_conf
#' @export
qq_conf = function(x, df=1, x.max = "auto",
                   main="QQ plot",plotType="pval",
                   sub=paste(""),
                   xlab="Expected", ylab="Observed",
                   conc=c(0.025, 0.975), overdisp=FALSE, trim=0.5,
                   slope.one=T, slope.lambda=FALSE,
                   thin=c(0.25,100), oor.pch=24, col.shade="gray", ofname="qqchi.pdf",
                   h=6,w=6,printpdf=F,myxlim="auto",point_cex=0.5,point_col = 1, point_pch = 1,...) {
  # From
  #   Jean Morrison[SMTP:MORRISON.JEANV@GMAIL.COM]
  #   Gesendet: Montag, 13. Dezember 2010 19:26:20
  #
  #   Hi Kirsten,
  #   Here is the script. The first chunk is all comments that you might want to read for best results - you should be able to just open it with a text editor to see the comments. One of the arguments is a thinning argument which reduces the number of points on the plot so that your file sizes won't get too huge. I like to use c(0.5, 500) just as a data point.
  # Jean

  # CHANGELOG
  # ...............
  # Added by Anna P. to allow for plotting log(P-value) instead of
  # 2*log(P-value) to make it compatible with chisq distribution with 2 df

  # 13/04/18
  # included auto x.max *das ist inder logik dieses skripts

  # 2013/06/05
  # x.max umgestellt, da sonst manch -inf berechnet wurde fuer x.max

  # 27.1.15 sub parameter i.e.  x achse untertitel raus
  #
  #
  # 9.3.15 myxlim auf auto umgestellt, damit per default alles geplottet
  #   # output as data.frame to improve behaviour in schleifen
  # 25.4.15 coloring and pch option included (point_col, point_pch..) and inflation from mean changed to median

  # 2.2.18 umgestellt lambda auf median chi mit 1 df analog GenABEL::estlambda(p, plot= F, method = "median", filter = F) auch analog http://genometoolbox.blogspot.de/2014/08/how-to-calculate-genomic-inflation.html  um classische lambda definition zu haben

  x_ori = x

  if(x.max == "auto") x.max = max(-log10(x))+2
  if(any(is.na(x))) stop("Please remove NA from p val vector....")

  if (plotType == "pval") {
    if(any(x==0)) stop("Please remove 0 from p val vector....")
    x <- -2*log(x)
    scale <- 0.5/log(10)
    x.max <- x.max/scale
    df <- 2
  } else {
    scale <- 1
  }

  # Function to shade concentration band

  # Sort values and see how many out of range  #hk ergaenzt um mit zu sortierende attribute mitzufuehren

  daten = data.table::data.table(x = x, point_cex = point_cex, point_col = point_col, point_pch = point_pch)
  daten = daten[is.na(x)==F]
  #   print(head(daten,9))
  data.table::setorder(daten, x)
  # print(head(daten,19))
  point_cex = if(length(point_cex)==1) point_cex else daten$point_cex
  point_col = if(length(point_col)==1) point_col else daten$point_col
  point_pch = if(length(point_pch)==1) point_pch else daten$point_pch

  #   obsvd <- sort(x, na.last=NA)
  x = daten$x
  obsvd = x

  N <- length(obsvd)
  if (missing(x.max)) {
    Np <- N
  }
  else {
    Np <- sum(obsvd<=x.max)
  }
  if(Np==0)
    stop("Nothing to plot")

  # Expected values

  if (df==2) {
    expctd <- 2*cumsum(1/(N:1))
  }
  else {
    expctd <- stats::qchisq(p=(1:N)/(N+1), df=df)
  }

  # Concentration bands

  if (!is.null(conc)) {
    if(conc[1]>0) {
      e.low <- stats::qchisq(p=stats::qbeta(conc[1], 1:N, N:1), df=df)
    }
    else {
      e.low <- rep(0, N)
    }
    if (conc[2]<1) {
      e.high <- stats::qchisq(p=stats::qbeta(conc[2], 1:N, N:1), df=df)
    }
    else {
      e.high <- 1.1*rep(max(x),N)
    }
  }

  # Plot outline

  if (Np < N)
    top <- x.max
  else
    top <- obsvd[N]
  right <- expctd[N]
  if(myxlim=="auto") myxlim = ceiling(log10(N))+0.5
  if (printpdf) {grDevices::pdf(ofname,h,w)}
  graphics::plot(c(0, right*scale), c(0, top*scale), type="n", xlab=xlab, ylab=ylab, bty="l",
       # main=main, sub=sub, cex.lab=1.25, # 27.1.15 sub raus
       main=main,  cex.lab=1.25,
       cex.axis=1.1,ylim=c(0,x.max*scale), xlim=c(0,myxlim))

  # Thinning

  if (is.na(thin[1])) {
    show <- 1:Np
  }
  else if (length(thin)!=2 || thin[1]<0 || thin[1]>1 || thin[2]<1) {
    warning("invalid thin parameter; no thinning carried out")
    show <- 1:Np
  }
  else {
    space <- right*thin[1]/floor(thin[2])
    iat <- round((N+1)*stats::pchisq(q=(1:floor(thin[2]))*space, df=df))
    if (max(iat)>thin[2])
      show <- unique(c(iat, (1+max(iat)):Np))
    else
      show <- 1:Np
  }
  Nu <- floor(trim*N)
  #    Nl <- floor(0.3*N)
  if (Nu>0)
    lambda <- stats::median(obsvd[1:Nu])/stats::median(expctd[1:Nu])
  #      lambda <- mean(obsvd[Nl:Nu])/mean(expctd[Nl:Nu])
  if (!is.null(conc)) {
    if (Np<N)
      vert <- c(show, (Np+1):N)
    else
      vert <- show
    if (overdisp)
      shade(expctd[vert]*scale, lambda*e.low[vert]*scale,
            expctd[vert]*scale, lambda*e.high[vert]*scale)
    else
      shade(expctd[vert]*scale, e.low[vert]*scale, expctd[vert]*scale, e.high[vert]*scale)
  }
  point_cex = if(length(point_cex)==1) point_cex else point_cex[show]
  point_col = if(length(point_col)==1) point_col else point_col[show]
  point_pch = if(length(point_pch)==1) point_pch else point_pch[show]
  graphics::points(expctd[show]*scale, obsvd[show]*scale,cex=point_cex,col = point_col, pch = point_pch, ...)
  # Overflow
  if (Np<N) {
    over <- (Np+1):N
    graphics::points(expctd[over]*scale, rep(x.max, N-Np)*scale, pch=oor.pch)
  }
  # Lines
  line.types <- c("solid", "dashed", "dotted")
  key <- NULL
  txt <- NULL
  if (slope.one) {
    key <- c(key, line.types[1])
    txt <- c(txt, "y = x")
    graphics::abline(a=0, b=1, lty=line.types[1])
  }
  if (slope.lambda && Nu>0) {
    key <- c(key, line.types[2])
    txt <- c(txt, paste("y = ", format(lambda, digits=4), "x", sep=""))
    if (!is.null(conc)) {
      if (Np<N)
        vert <- c(show, (Np+1):N)
      else
        vert <- show
    }
    graphics::abline(a=0, b=lambda, lty=line.types[2])
  }
  if (printpdf) {grDevices::dev.off()}
  # Returned value

  #    if (!is.null(key))
  #       legend(0, top*scale, legend=txt, lty=key, bty="n")

  lambda_coxlab = lambda

  chisq_blog <- stats::qchisq(1-x_ori,1) # http://genometoolbox.blogspot.de/2014/08/how-to-calculate-genomic-inflation.html

  lambda_blog = stats::median(chisq_blog)/stats::qchisq(0.5,1)
  lambda_blog

  data.frame(N=N, omitted=N-Np, lambda=lambda_blog, lambda_coxlab=lambda_coxlab)

}
