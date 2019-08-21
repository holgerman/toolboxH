removeNAcolumns = function(df) {
  nacol = sapply(df, function(x) sum(is.na(x)) == dim(df)[1])
  if("data.table" %in% class(df)) return(df[,nacol ==F, with = F]) else return(df[,nacol ==F])
}
