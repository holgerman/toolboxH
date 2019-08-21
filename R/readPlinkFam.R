readPlinkFam = function(x)
{
  file.fam <- read.table(x, sep="")
  names(file.fam) <- c("pid", "id", "fid", "mid" , "sex", "disease")
  file.fam
}
