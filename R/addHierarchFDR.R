#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pvalues PARAM_DESCRIPTION
#' @param categs PARAM_DESCRIPTION
#' @param fdr2control PARAM_DESCRIPTION, Default: 0.05
#' @param fdrmethod_level1 PARAM_DESCRIPTION, Default: 'BH'
#' @param fdrmethod_level2 PARAM_DESCRIPTION, Default: 'BH'
#' @param correctionLevel1 PARAM_DESCRIPTION, Default: 'BB'
#' @param quiet PARAM_DESCRIPTION, Default: FALSE
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
#' @rdname addHierarchFDR
#' @export
#' @import data.table

addHierarchFDR <- function(pvalues, categs, fdr2control = 0.05, fdrmethod_level1 = "BH", fdrmethod_level2 = "BH", correctionLevel1 = "BB", quiet=FALSE ) {

  # correctionLevel1 == "BB" means  from http://bioinformatics.org/treeqtl/Peterson_GenEpi_2016.pdf citing  Benjamini and Bogomolov  [2014] doing  q2 × Number families with FDR level2 <= fdr2control / Number of all families ("appropriate adjustment for the selection bias introduced in Stage 1" page 5) or
  # correctionLevel1 == "listlookup" using "the locally adjusted minimum p-value corresponding to the globally adjusted p-value threshold of 0.05." as decribed in https://www.biorxiv.org/content/biorxiv/early/2017/10/25/209171.full.pdf

  # pvalues = c(runif(500, 0,1 ), runif(500, 0,1 )/100)
  # categs = sort(rep(letters[1:10], 100))
  #


  stopifnot(all(is.numeric(pvalues)))
  stopifnot(is.vector(categs))
  stopifnot(length(categs)==length(pvalues))
  stopifnot(length(categs)>0)

  data = data.table::data.table(p = pvalues, cats = categs)

  data[,fdr_level1 := stats::p.adjust(p,method =fdrmethod_level1), by = list(cats)] #  &

  level2 = data[ ,.(min_level1 = min(fdr_level1)), by = list(cats)]
  level2

  level2[,fdr_level2 := stats::p.adjust(min_level1,method =fdrmethod_level2)]
  level2

  data[,fdr_level2 := level2[match_hk(data$cats, level2$cats),fdr_level2]]

  global_fdr5_level_table = level2[fdr_level2 <= fdr2control]

  if(nrow(global_fdr5_level_table)==0)  {
    hierarch_fdrspalte_name = paste0('hierarch_fdr', 100*fdr2control, "proz")
    data[,(hierarch_fdrspalte_name):=FALSE]
    data.table::setnames(data, "cats", "category")
    stopifnot(identical(data$p, pvalues))
    stopifnot(identical(data$category, categs))
    return(data)

  } else {

    if( correctionLevel1 == "BB") {
      if(quiet==F) message("using for level 1 ",fdrmethod_level1,"; using for level 2 ",fdrmethod_level2, "; adjustment of multiple testing level of intersection hypotheses acc. to n Benjamini and Bogomolov [2014]...")
      n_families_surviving_multipletesting = global_fdr5_level_table[,data.table::uniqueN(cats)]
      n_families_all = data[,data.table::uniqueN(cats)]
      global_fdr5_level = fdr2control * n_families_surviving_multipletesting/n_families_all
      global_fdr5_level

    } else   if( correctionLevel1 == "listlookup") {
      if(quiet==F) message("using for level 1 ",fdrmethod_level1," using for level 2 ",fdrmethod_level2, " adjustment of multiple testing level of intersection hypotheses acc. to Huang 2017 https://www.biorxiv.org/content/biorxiv/early/2017/10/25/209171.full.pdf...")
      global_fdr5_level = global_fdr5_level_table[, max(min_level1)]



      if(quiet==F) message("less significant accepted group (fdr_level2 should be close to ",fdr2control," ):")
      zeile1 = level2[min_level1==global_fdr5_level]
      if(quiet==F) print(zeile1)
      if(quiet==F) message("most significant non-accepted group (fdr_level2 should be also close to ",fdr2control," ):")

      zeile2 = level2[min_level1 >global_fdr5_level][min_level1==min(min_level1)]
      if(quiet==F) print(zeile2)
      if(nrow(zeile2)==0) warning("All groups were significant at the chosen level. Think about it -Is this expected?", immediate. = TRUE)
      if( zeile1$fdr_level2 < fdr2control*0.9 ) warning("No FDR close to desired global cut-off found (FDR cut-off: ",fdr2control, " vs. level2 - FDR corresponding as closest as possible to this globally threshold of 0.05: ",zeile1$fdr_level2, " ). This might be if only few but strong groups are present. FDR is only reliable on group - level, and too conservative on association level 1")


    } else stop('parameter `correctionLevel1` must be either "BB" or "listlookup"')

    if(quiet==F) message("Using as adjustment of multiple testing level of intersection hypotheses FDR ", global_fdr5_level, " instead of ", fdr2control)

    good_categ = level2[fdr_level2 <= fdr2control, unique(cats)]
    # message('A total of ', huebsch(length(unique(good_categ))), " / ",huebsch(n_total_categs) ," (", proz(length(unique(good_categ))/n_total_categs),") categories fullfill FDR criterium...")


    data[ , fdr5proz := cats %in% good_categ & fdr_level1 <= global_fdr5_level]

    data[ , fdr5proz := ifelse(is.na(fdr5proz), F, fdr5proz)] # weil ohnehin kleiner als global_fdr5_level. daher check vorher dass dem auch so ist
    data

    ## fertigmachen
    hierarch_fdrspalte_name = paste0('hierarch_fdr', 100*fdr2control, "proz")

    data.table::setnames(data, "fdr5proz", hierarch_fdrspalte_name)
    data.table::setnames(data, "cats", "category")
    stopifnot(identical(data$p, pvalues))
    stopifnot(identical(data$category, categs))
    return(data)
  }

}
