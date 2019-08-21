removeEmptyRow = function(df) {
  nrow1 = nrow(df)

  leerrows = apply(df,1, function(x) sum(x =="",na.rm = T ) + sum(is.na(x)) == dim(df)[2])
  message('Removed ',sum(leerrows), '  rows where entries are only NA or ""...')
  df[leerrows ==F,]
}
