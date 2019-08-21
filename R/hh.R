hh = function ( d, mydims=5 ) {
  # 29.1.15 data.table included
  if("data.table" %in% class(d)) {
    d[1 : min(dim(d)[1],mydims), names(d)[ 1 : min(dim(d)[2],mydims)], with = F]
  } else   d [ 1 : min(dim(d)[1],mydims) , 1 : min(dim(d)[2],mydims) , drop =F]
}
