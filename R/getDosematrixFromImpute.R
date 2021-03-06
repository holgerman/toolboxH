#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param snps PARAM_DESCRIPTION
#' @param chr PARAM_DESCRIPTION
#' @param geno_fn PARAM_DESCRIPTION
#' @param sample_fn PARAM_DESCRIPTION
#' @param n_threads PARAM_DESCRIPTION, Default: 1
#' @param createPlinkCommandOnly PARAM_DESCRIPTION, Default: F
#' @param additionallyTransposeFile PARAM_DESCRIPTION, Default: T
#' @param outfile PARAM_DESCRIPTION, Default: tempfile()
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
#' @rdname getDosematrixFromImpute
#' @export
#' @import data.table
#' @import stringr
getDosematrixFromImpute = function(snps,chr,  geno_fn, sample_fn, n_threads = 1, createPlinkCommandOnly = F, additionallyTransposeFile = T, outfile = tempfile(),   snps_fn = tempfile(), use_ids = 'id2') {

  stopifnot(all(is.character(snps)))
  stopifnot(all(is.na(snps))==F)

  stopifnot(length(geno_fn)==1)
  stopifnot(file.exists(geno_fn))

  stopifnot(length(sample_fn)==1)
  stopifnot(file.exists(sample_fn))

  stopifnot(length(chr)==1)
  stopifnot(chr %in% 1:25)

  snps = unique(snps)

  stopifnot(use_ids %in% c('id2', 'id1', 'id1_2'))

  write.delim(snps, snps_fn,writeColnames = F)
  if(exists('callPlink20')==F) bauePlinkCall(showMessage1 = F)

  ## checke .sample sex column to be if existent type 'D'.
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

  if("sex" %in%names(samples)) {
    sex_code = samples[1,sex]
    if(sex_code != "D") {
      message("Found sex code ",sex_code," but D is expected from plink. Recoding in temporary file")
      sample_fntemp = tempfile()
      myvartypes = unlist(samples[1])
      myvartypes["sex"] = "D"
      writeSnptestSamplefile(filename = sample_fntemp, samplefile = samples[-1], vartypes = myvartypes)
      sample_fn = sample_fntemp

    }

  }

  mycall = paste(callPlink20,   '--export A-transpose --extract ', snps_fn, '--gen ', geno_fn, '--out ', outfile, '--oxford-single-chr ', chr, '--sample ', sample_fn, '--threads ', n_threads)

  if(createPlinkCommandOnly==T) return(mycall)
  message('running plink via\n' , mycall)

  syscall = system(mycall)

  additiveFile = data.table::fread(paste0(outfile, ".traw"), colClasses=list(character=c("COUNTED","ALT")))
  # hh(additiveFile,11)

  ids = data.table::data.table(id_combined = names(additiveFile)[-1:-6])
  ids[,id1 := sapply(stringr::str_split(id_combined, "_"), "[", 1)]
  ids[,id2 := stringr::str_replace(id_combined, paste0("^",id1,"_"), "")]

  if(use_ids=="id2") data.table::setnames(additiveFile, ids$id_combined, ids$id2)
  if(use_ids=="id1") data.table::setnames(additiveFile, ids$id_combined, ids$id1)

  if(additionallyTransposeFile == T) {
    additiveFile_t = data.table::data.table(id = names(additiveFile)[-1:-6], data.table::transpose(additiveFile[,-c("CHR","SNP", "(C)M", "POS", "COUNTED", "ALT"), with = F]))
    names(additiveFile_t) = c("id", additiveFile$SNP)
    # hh(additiveFile_t)
  } else additiveFile_t = NULL
  message("Recoded ", nrow(additiveFile), " SNPs of ", ncol(additiveFile)-6, " individuals")
  res = c()
  res$call = mycall
  res$errorcode = syscall
  res$additiveFile =additiveFile
  res$additiveFile_t =additiveFile_t
  res
}
