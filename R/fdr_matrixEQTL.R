#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param p PARAM_DESCRIPTION
#' @param N PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname fdr_matrixEQTL
#' @export
fdr_matrixEQTL <- function(p, N) {
  K = length(p)
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3348564/   section 3.6 False discovery rate
  # Matrix eQTL calculates FDR only for the gene-SNP pairs that passed user-defined significance threshold. The calculations follow Benjamini and Hochberg (1995) procedure, adapted for the situation when not all p-values are recorded.

  # K
  if(any(is.na(p))) stop("NAs in p values not allowed...")

  dt = data.table::data.table(p = p)
  dt[,initialorder := 1:.N]
  data.table::setorder(dt, p)
  fdr = dt[,p*N/(1:K)]
  fdr[fdr>1]=1

  for(i in rev(seq(along=fdr))[-1]) {
    if(fdr[i]>fdr[i+1]) fdr[i] = fdr[i+1]

  }
  dt[,fdr := fdr]
  data.table::setorder(dt, initialorder)
  dt[,fdr]
}
