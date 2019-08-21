htl = function(df, laenge =2) {### show the first and last row transposed
  dim1 = dim(df)[1]
  zeilen = c(1:laenge,(dim1 -laenge+1) :dim1)
  res = t(df[zeilen,])
  colnames(res) = zeilen
  res
}
