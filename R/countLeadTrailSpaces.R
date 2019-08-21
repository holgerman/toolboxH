countLeadTrailSpaces = function(df) sapply(df,function(x) sum(grepl("^ | $", x)))
countNonalphanum = function(df) sapply(df,function(x) sum(grepl("[[:punct:] ]", na.omit(x))))
gridNonalphanum = function(df) sapply(df,function(x) grepl("[[:punct:] ]", x)==F & is.na(x))
doBasicCheck = function(df) {
  ## 181013 update laenge beispiel
  ## 14/01/14 laenge abhaengig von klasse
  ### schnelles anschauen der daten muss noch optimiert werden

  message("berechne laenge und NAs...\n")

  zeilen = dim(df)
  NAs = showNA(df)$NAs
  NAs_proz = NAs/zeilen
  Vals = zeilen - NAs
  Vals_proz = Vals/zeilen

  message("Berechne klassen und laengen...\n")

  klassen = showClassDF(df)
  numerisch = sapply(df,is.numeric)

  laenge = {sapply(df[,numerisch ==F , drop = F], stringr::str_length)}

  message("Berechne minima maxima...\n")
  maxl = apply(laenge,2,function(xx) max(xx, na.rm=T))
  minl = apply(laenge,2,function(xx) min(xx, na.rm=T))

  max_num = apply(df[, numerisch, drop = F],2,function(xx) max(as.numeric(xx), na.rm=T))
  min_num = apply(df[,numerisch, drop =F],2,function(xx) min(as.numeric(xx), na.rm=T))

  maxx = c(maxl, max_num)
  minn = c(minl, min_num)
  maxx = maxx[names(klassen)]
  minn = minn[names(klassen)]

  message("Berechne leading / trailing spaces...\n")
  lt_spaces = countLeadTrailSpaces(df[, numerisch ==F, drop=F])
  lt_spaces_proz = lt_spaces/zeilen

  lt_spaces = lt_spaces[names(klassen)]
  lt_spaces_proz = lt_spaces_proz[names(klassen)]

  message("Berechne alphanum...\n")
  non_alphanum = countNonalphanum(df[,numerisch ==F, drop=F])
  non_alphanum_proz = non_alphanum/zeilen

  non_alphanum = non_alphanum[names(klassen)]
  non_alphanum_proz = non_alphanum_proz[names(klassen)]

  firstentry = unlist(apply(df,2,function(xx) if(all(is.na(xx))) NA else na.omit(xx)[1]))
  lastentry = unlist(apply(df,2,function(xx) if(all(is.na(xx))) NA else na.omit(xx)[length(na.omit(xx))]))

  res = data.frame(colname = names(klassen),Vals, Vals_proz, NAs, NAs_proz, lt_spaces, lt_spaces_proz, non_alphanum, non_alphanum_proz, klassen, minn, maxx, firstentry, lastentry)

  anheubschcat = c("Vals_proz", "NAs_proz", "lt_spaces_proz", "non_alphanum_proz")
  res[,anheubschcat] = sapply(res[,anheubschcat], function(x) round(100*x,2))
  names(res) = c("Colnames","Vals","V.pr","NAs", "NA.pr", "lt_spaces", "sp.pr", "non_alphanum", "nona.pr", "class", "min", "max", "firstentry", "lastentry")
  row.names(res) = NULL
  res
}
