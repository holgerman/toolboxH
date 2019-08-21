## An T F index for alle duplicated entries, not only the duplicated one
allDuplicatedEntries <- function (vektor) {
  ## 150303 umgestellt auf datatable
  if(length(vektor)==0) return(0)
  library(data.table)
  #   library(reshape2)
  vektab = data.table(myvektor = vektor, num = 1:length(vektor))
  duplicated_vals = vektab[duplicated(myvektor),myvektor]
  duplicated_entries = vektab[ myvektor %in% duplicated_vals]
  setkey(duplicated_entries, myvektor)
  duplicated_entries$num

}
