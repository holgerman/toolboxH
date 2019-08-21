removeEmptyCols = function(df) {

  leercols = apply(df,2, function(x) sum(x =="",na.rm = T ) + sum(is.na(x)) == dim(df)[1])
  message('Removed ',sum(leercols), '  columns where entries are only NA or ""...')
  if("data.table" %in% class(df)) return(df[,leercols ==F, with = F]) else return(df[,leercols ==F])
}
