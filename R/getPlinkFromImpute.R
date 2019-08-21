#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param snps PARAM_DESCRIPTION
#' @param chr PARAM_DESCRIPTION
#' @param geno_fn PARAM_DESCRIPTION
#' @param sample_fn PARAM_DESCRIPTION
#' @param outfile PARAM_DESCRIPTION
#' @param n_threads PARAM_DESCRIPTION, Default: 1
#' @param createPlinkCommandOnly PARAM_DESCRIPTION, Default: F
#' @param snps_fn PARAM_DESCRIPTION, Default: tempfile()
#' @param use_ids PARAM_DESCRIPTION, Default: 'id2'
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
#' @rdname getPlinkFromImpute
#' @export
#' @import data.table
getPlinkFromImpute = function (snps, chr, geno_fn, sample_fn, outfile, n_threads = 1, createPlinkCommandOnly = F,  snps_fn = tempfile(),use_ids = "id2")
{

  # snps = myinfo$markername
  # chr = unique(myinfo$chr)
  # geno_fn = myinfo[,unique(geno_fn)]
  # sample_fn =  myinfo[,unique(sample_fn)]
  # outfile = "/net/ifs1/san_projekte/projekte/genstat/09_nutzer/holger/___owncloud2/imise/DissFelix_SHARED/07_publikation/1606_berechnungen/03_pipeline_candGene/s214_1_plink_alle_genessnps"

  stopifnot(all(is.character(snps)))
  stopifnot(all(is.na(snps)) == F)
  stopifnot(length(geno_fn) == 1)
  stopifnot(file.exists(geno_fn))
  stopifnot(length(sample_fn) == 1)
  stopifnot(file.exists(sample_fn))
  stopifnot(length(chr) == 1)
  stopifnot(chr %in% 1:25)
  snps = unique(snps)
  stopifnot(use_ids %in% c("id2", "id1", "id1_2"))
  write.delim(snps, snps_fn, writeColnames = F)
  if (exists("callPlink20") == F)
    bauePlinkCall(showMessage1 = F) else message("Using plink ", callPlink20)
  samples = data.table::fread(sample_fn, colClasses = "character")
  samples

  if(all(c("ID_1", "ID_2") %nin% names(samples)) & all(c("ID1", "ID2") %in% names(samples))) {
    sample_fntemp = tempfile()
    myvartypes = unlist(samples[1])
    data.table::setnames(samples, names(samples)[1:2], c("ID_1", "ID_2"))
    writeSnptestSamplefile(filename = sample_fntemp,
                                     samplefile = samples[-1], vartypes = myvartypes)
    sample_fn = sample_fntemp

  }

  if ("sex" %in% names(samples)) {
    sex_code = samples[1, sex]
    if (sex_code != "D") {
      message("Found sex code ", sex_code, " but D is expected from plink. Recoding in temporary file")
      sample_fntemp = tempfile()
      myvartypes = unlist(samples[1])
      myvartypes["sex"] = "D"
      writeSnptestSamplefile(filename = sample_fntemp,
                                       samplefile = samples[-1], vartypes = myvartypes)
      sample_fn = sample_fntemp
    }
  }
  mycall = paste(callPlink20, "--make-bed --extract ",
                 snps_fn, "--gen ", geno_fn, "--out ", outfile, "--oxford-single-chr ",
                 chr, "--sample ", sample_fn, "--threads ", n_threads)
  if (createPlinkCommandOnly == T)
    return(mycall)
  message("running plink via\n", mycall)
  syscall = system(mycall)

  if(syscall==0) {message("Successfully created \n", paste(paste0(outfile, c(".bed", ".bim", ".fam")), collapse = "\n"))} else message("Plink file creation failed....")
  outfile

}
