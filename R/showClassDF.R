### quickly-find-class-of-dataframe
}                         #http://gettinggeneticsdone.blogspot.com/2010/08/quickly-find-class-of-dataframe-vectors.html
showClassDF <- function(x) {
  ## 12.6.15 als data.frame
  ## 21.11. collapsing

  tmp = lapply(unclass(x), class)
  tmp = lapply(tmp, function(x) paste(x, collapse = ", "))
  resi = unlist(tmp)
  resi = data.frame(column = names(resi), class = as.vector(resi))
  if(any(is.na(resi$column))) resi$column[is.na(resi$column)] = "NA"
  rownames(resi) = as.character(resi$column)
  resi$column = NULL
  resi

}                         #http://gettinggeneticsdone.blogspot.com/2010/08/quickly-find-class-of-dataframe-vectors.html
