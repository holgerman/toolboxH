## read SNPTEST sample file in R as data.table
readSNPTESTsamplefileASdata.table <- function (snptest_sample_fn, makeBinaryOutcomeNumeric12 = T) {
  library(data.table)
  samplefile = fread(snptest_sample_fn)

  samplefile_types = unlist(samplefile[1])
  samplefile_types
  samplefile = samplefile[-1]

  for(i in names(samplefile_types[samplefile_types %in% c("C","P")])) {
    samplefile[,(i) := as.numeric(get(i))]
  }

  for(i in names(samplefile_types[samplefile_types %in% c("D","B")])) {
    samplefile[,(i) := factor(get(i))]
  }


  ## binary in 1 2 recodieren
  if(makeBinaryOutcomeNumeric12 == T) {for(i in names(samplefile_types[samplefile_types %in% c("B")])) {

    samplefile[,(i) := as.numeric(factor(get(i)))]
    samplefile[,if(any(na.omit(get(i)) %nin% c(1,2))) stop(paste0("Case/control phenotype " ,i, " is not encoded in exactly two levels...")) ]

  }}

  # str(samplefile)
  samplefile
}
