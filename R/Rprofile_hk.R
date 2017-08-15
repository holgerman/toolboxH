#' @export
initializeSkript <-  function(computer="amanMRO",add_libpath = T,  lib_location = "/mnt/ifs1_projekte/genstat/07_programme/rpackages/"){
  #clear memory cache

  gc()
  gc()


  ### Do you want to automatically convert strings to factor variables in a data.frame? WARNING!!! This makes your code less portable/reproducible.
  message("setting options(stringsAsFactors=FALSE)")
  options(stringsAsFactors=FALSE)

  ## display large integer in data.table as R base does
  message('setting options(datatable.integer64= "numeric")')
  options(datatable.integer64= "numeric")

  ### Don't ask me for my CRAN mirror every time
  message('setting options("repos" = c(CRAN = "http://cran.rstudio.com/"))')
  options("repos" = c(CRAN = "http://cran.rstudio.com/"))


  ### Warning bei partial matchign immer an
  message('setting options ( warnPartialMatchAttr = T ) and options ( warnPartialMatchDollar = T )')
  options ( warnPartialMatchAttr = T )
  options ( warnPartialMatchDollar = T )

  # initiate time measuring
  time0 <<- Sys.time()

  # define where packages are found
  if(add_libpath) .libPaths(unique(c(paste0(lib_location,computer)), .libPaths())) else .libPaths(paste0(lib_location,computer))
  message("using libpath: ", paste(.libPaths(), collapse = "\n"))

}



showStartcode = function(os = "unix", pathwd = getwd(), myfilename = filename){


  # create code for command - line start of the script:
  if(exists('myfilename') ==F) myfilename = "string_myfilenme_not_defined"


  if(! os %in% c("unix", "win")) stop("parameter `os` must be either `unix` or `win` ")
  if(os == "unix") message(paste0("\ncd ", pathwd, "\n", "R CMD BATCH --vanilla ", myfilename, ".R ", myfilename, "R.out 2>&1 &"), "\n", "tail -f ",myfilename, "R.out\n")
  if(os == "win") message(paste0("\ncd ", pathwd, "\n", "'c:/Program Files/R/R-3.1.2/bin/x64/Rterm.exe' --vanilla < '", myfilename, ".R' '",paste0(myfilename, "R.out' 2>&1 &"), "\n", "tail -f ",myfilename, "R.out\n"))

}


parallelisiere = function(proc=3, on_server = Sys.info()['sysname']=="Linux") {
  if(on_server ==T) {
    for(i in c( "doMC", "foreach")) {
      suppressPackageStartupMessages(library(i, character.only = TRUE))
    }

    registerDoMC(cores=proc)

  }

  if(on_server ==F) {
    for(i in c( "doSNOW", "foreach")) {
      suppressPackageStartupMessages(library(i, character.only = TRUE))
    }

    registerDoSNOW(makeCluster(proc, type = "SOCK"))
  }
  message("Parallel backbone initiated for ",proc, " parallel processes....")

}

bauePlinkCall = function(on_server = Sys.info()['sysname']=="Linux"){
  if(on_server =="Linux") {
    callPlink <<-  paste0(basicpath, "07_programme/plink1.9/vs150715stable_beta3v/unix64/plink" )
  } else callPlink <<-  paste0(basicpath, "07_programme/plink1.9/vs150715stable_beta3v/win64/plink.exe" )

  message("`callPlink` will call plink from here: ", callPlink)

}

##..................................................................................
## data exploration
##..................................................................................




### load data if not already there, useful for large dat
cleverFread = function(name, path, myenvir = .GlobalEnv, ...) {
  library(data.table)
  if(exists(name)==F) assign(name, fread(path, ...), envir = myenvir) else message("using object '", name, "' from workspace, saving loading-time :)")
  return(get(name))
}

cleverLoadinGlobalEnv = function(name, path, myenvir = .GlobalEnv, ...) {
  if(exists(name)==F)  load(path, envir = myenvir, ...) else message("using object '", name, "' from workspace, saving loading-time :)")
  return(name)
}
## load object number with numberin new environment to allow that it does not overwrite something with the same name
load_obj <- function(f, number = 1)
{
  envextra <- new.env()

  nm <- load(f, envextra)
  message("found following objects:\n", paste(nm, collapse = "\n"))
  message("\nimported object ", nm[number])
  envextra[[nm[number]]]
}


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

## read plink family file
readPlinkFam = function(x)
{
  file.fam <- read.table(x, sep="")
  names(file.fam) <- c("pid", "id", "fid", "mid" , "sex", "disease")
  file.fam
}

## read plink snp file
readPlinkBim = function(x, useData.table=T, clever = NULL)
{
  #150303 datatable import bei plink 1.9
  if(useData.table & is.null(clever)) {
    library(data.table)
    file.bim <- fread(x, header = F)
    setnames(file.bim, names(file.bim),  c("chr", "snp", "lod", "pos", "a1", "a2"))
    return(file.bim)
  }

  if(useData.table & is.null(clever)==F) {
    library(data.table)
    stopifnot(is.character(clever))
    file.bim <- cleverFread(name = clever, x, header = F)
    setnames(file.bim, names(file.bim),  c("chr", "snp", "lod", "pos", "a1", "a2"))
    return(file.bim)
  }



  file.bim <- read.delim(x, header = F)
  names(file.bim) <- c("chr", "snp", "lod", "pos", "a1", "a2")
  return(file.bim)

}


### quickly-find-class-of-dataframe
showClassDF <- function(x) {
  ## 12.6.15 als data.frame
  resi = unlist(lapply(unclass(x),class))
  resi = data.frame(column = names(resi), class = as.vector(resi))
  resi$column[is.na(resi$column)] = "NA"
  rownames(resi) = as.character(resi$column)
  resi$column = NULL
  resi
}                         #http://gettinggeneticsdone.blogspot.com/2010/08/quickly-find-class-of-dataframe-vectors.html

### show allwahys NA when using xtabs
xtabs_hk = function(...) xtabs(... , exclude = NULL, na.action= "na.pass") # 12.6.15 zweites komma weggemacht, in .env verschoben


### show NAs within a data.frame
showNA <- function(x) {
  ## 15.6.15 als data.frame
  ## 7.7.15 apply statt sapply damit auch mit matrix funzend
  resi = apply(x,2, function(y) sum(is.na(y)))
  resi2 = data.frame(var = names(resi), NAs = as.vector(resi), vals = nrow(x)-as.vector(resi))
  resi2
  # if(is.data.table(x)) {
    # setDT(resi2)
    # return(resi2)} else return(resi2)

}





### table categs
mytable = function (x, mydigits = 1, doprint = T, do_science_output = F) {
  res = c()
  res$num <- table(x, useNA="always")
  res$output = data.frame(res$num)
  names(res$output) = c("category" , "freq")
  res$output$percent = res$num /sum(res$num)
  res$output$observed = paste0(format(res$output$freq, big.mark=",", scientific=do_science_output), " (", round(res$output$percent*100,mydigits), "%)")
  library(stringr)
  res$output$observed = str_trim(res$output$observed)
  zeilennamen = as.character(res$output$category)
  zeilennamen[is.na(zeilennamen)] = "NA"
  rownames(res$output) = zeilennamen
  res$output$category = zeilennamen
  if(doprint) print(res$output[,c('observed'), drop= F])
  invisible(res$output)

}


## test auf ganzezahlen
is.wholenumber <-   function(x, tol = .Machine$double.eps^0.5) {
  x <- na.omit(x)
  if(is.numeric(x) ==F) return(F)
  if(is.numeric(x) ==T)  abs(x - round(x)) < tol
}


### Summmary eines data.frames
countLeadTrailSpaces = function(df) sapply(df,function(x) sum(grepl("^ | $", x)))
countNonalphanum = function(df) sapply(df,function(x) sum(grepl("[[:punct:] ]", na.omit(x))))
gridNonalphanum = function(df) sapply(df,function(x) grepl("[[:punct:] ]", x)==F & is.na(x))
doBasicCheck = function(df) {
  ## 181013 update laenge beispiel
  ## 14/01/14 laenge abhaengig von klasse
  ### schnelles anschauen der daten muss noch optimiert werden

  message("berechne laenge und NAs...\n")

  zeilen = dim(df)
  NAs = showNA(df)
  NAs_proz = NAs/zeilen
  Vals = zeilen - NAs
  Vals_proz = Vals/zeilen


  message("Berechne klassen und laengen...\n")

  klassen = showClassDF(df)
  numerisch = sapply(df,is.numeric)


  laenge = {library(stringr); sapply(df[,numerisch ==F , drop = F], str_length)}

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


### Show the first 5 rows and first 5 columns of a data frame or matrix
hh = function ( d, mydims=5 ) {
  # 29.1.15 data.table included
  if("data.table" %in% class(d)) {
    d[1 : min(dim(d)[1],mydims), names(d)[ 1 : min(dim(d)[2],mydims)], with = F]
  } else   d [ 1 : min(dim(d)[1],mydims) , 1 : min(dim(d)[2],mydims) , drop =F]
}

### Show the first an last  rows  of a data frame or matrix
ht <- function ( d, myrows=10 )
{ ## updated 11.3. to show all if dim 1 smaller than myrows*2
  rows2show = min(dim(d)[1],myrows)
  if(dim(d)[1] <= 2*rows2show) return(d)
  rbind ( head ( d ,  rows2show ), tail ( d ,  rows2show ))
}

### show the first an last row transposed
htl = function(df, laenge =2) {
  dim1 = dim(df)[1]
  zeilen = c(1:laenge,(dim1 -laenge+1) :dim1)
  res = t(df[zeilen,])
  colnames(res) = zeilen
  res
}

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

getHaeufigkeit = function(var){

  require(data.table)
  dt = data.table( var = var, key = "var")

  dt[,oldorder := 1:nrow(dt)]

  tabled = dt[,table(var)]

  tabled = data.table(num = as.numeric(tabled), var = names(tabled), key = 'var')
  dt = tabled[dt]
  setorder(dt, oldorder)
  #   print(dt)
  #   print(dt[,num])

  return(dt$num)
}


##..................................................................................
## data manipulation and handling
##..................................................................................

## Returns a logical vector TRUE for elements of X not in Y
"%nin%" <- function(x, y) !(x %in% y)


### Aus mehrfacheintraegen einzelne zeilen machen
fastermultipleEntries2multipleRows = function(dfori,idToSplit,separator) {
  #15/1/14 grundlegender umbau, um clasen der felder zu erhalten. das hatte bis dahin logicals zerschossen...., weil es vor dies bei textumwandlung leerzeichen einfuegt. allerdings war wahrsheinlich fastmultipleEntries2multipleRows richtig
  # 5/2/15 eigene Methode fuer data.tabel gebaut und insgesamt the data.table way gebaut
  classfound = class(dfori)

  stopifnot(classfound %in% c("data.table", "data.frame"))

  library(data.table)
  library(reshape2)

  if( "data.table" %nin% classfound) dfori = data.table(dfori)

  #aufteilen auf 2 table, je nachdem ob Felder den separator enthalten
  time1 = Sys.time()
  call0 = parse(text = paste0('df0 = dfori[grep(separator, ', idToSplit,', invert=T),]'))
  eval(call0)

  call1 = parse(text = paste0('df1 = dfori[grep(separator, ', idToSplit,'),]'))
  eval(call1)

  spaltenzahl = which(names(df1) == idToSplit)
  spaltennamen = names(df1)
  #    spaltennamen = names(df1)[1:6]
  if(nrow(df1)==0) {
    message("Separator kommt nicht im angegebenen Feld vor!")
    return(df0)
  }

  time1 = Sys.time()
  call2 = parse(text = paste0('splitvar = df1[, unique(as.character(',idToSplit,'))]'))
  eval(call2)

  message("\nsplitte ", nrow(df1), " eintraege (entsprechen ", length(splitvar)," unique Eintraegen) ...")

  tosplit = data.table(splitvar)

  call3 = parse(text = paste0('referenz = tosplit[,list(splitted = unlist(strsplit(splitvar, split= "',separator,'"))), by = splitvar]'))
  eval(call3)

  setnames(referenz, "splitvar", idToSplit)
  setkeyv(referenz, idToSplit)

  setkeyv(df1,idToSplit)

  df1 = referenz[df1,allow.cartesian=TRUE]

  time1 = Sys.time() - time1

  message("done in ", time1, attr(time1, which="units"))

  time1 = Sys.time()
  message("\ncombiniere eintraege...")

  setnames(df1,  idToSplit, "loesche")
  df1$loesche = NULL

  setnames(df1, "splitted", idToSplit)

  setcolorder(df1, names(df0))
  if(nrow(df0) >0) df2 = rbindlist(list(df0, df1)) else df2 = df1

  time1 = Sys.time() - time1
  message("done in ", time1, attr(time1, which="units"))

  if( "data.table" %nin% classfound)  return(data.frame(df2)) else  return(df2)

}

### remove duplicated columns
removeDuplicatedColumns <- function (mydf) {
  library(stringr)
  namesdf= data.frame(original = names(mydf))
  namesdf$final = sapply(str_split(namesdf$original, pattern='\\.x\\.*[0-9]*$|\\.y\\.*[0-9]*$'), "[",1)
  tab = table(namesdf$final)
  namesdf$num_categ = tab[ match_hk(namesdf$final, names(tab))]
  finalnames = namesdf$final[duplicated(namesdf$final)==F]

  tounify = unique(namesdf$final[ namesdf$num_categ >1])
  for(i in tounify){
    message("merge ", i)
    mysubset = namesdf[ namesdf$final == i,]
    print(mysubset)
    tomerge = as.matrix(mydf[ , mysubset$original])
    idenditycheck = apply(tomerge,1,function(x) identical(x[[1]], unique(x)))
    #   idenditycheck = apply(tomerge,1,function(x) {print(x[[1]]); print(unique(x));message("-----\n");identical(x[[1]], unique(x))})
    if(all(idenditycheck) != T) stop("probleme gefunden bei obiger kategorie, bitte selber drumm kuemmern")
    mydf[[i]] = tomerge[,1]

  }

  mydf = mydf[,finalnames]
  mydf
}


### remove empty data
removeNAcolumns = function(df) {
  nacol = sapply(df, function(x) sum(is.na(x)) == dim(df)[1])
  if("data.table" %in% class(df)) return(df[,nacol ==F, with = F]) else return(df[,nacol ==F])
}

removeEmptyRow = function(df) {
  leerrows = apply(df,1, function(x) sum(x =="") == dim(df)[2])
  df[leerrows ==F,]
}

removeEmptyCols = function(df) {
  leercols = apply(df,2, function(x) sum(x =="") == dim(df)[1])
  if("data.table" %in% class(df)) return(df[,leercols ==F, with = F]) else return(df[,leercols ==F])
}


### Takes a dataframe and a column name, and moves that column to the front of the DF.
moveColFront <- function ( d = dataframe , colname = "colname" ) {
  ## 15.6.15 data.table auf setcolorder umgestellt
  ## multiple ohne warning
  stopifnot(all(colname %in% names(d)))
  index <- match ( colname , names ( d ))
  old_order <- 1:ncol(d)
  new_order <- c(index, old_order[old_order %nin% index])
  if(is.data.table(d)) setcolorder(d, new_order)
  if(is.data.table(d)==F)  d= d[,new_order]
  return(d)
}


### matching und dabeiaufpassen, dass matchvariable unique ist
match_hk = function(x, y, testunique =T, makeunique = F,importcol = NULL, ...) {
  ##150122 makeunique = F statt T, na.omit bei duplicated y, fehlenden ok fall includiert
  ##160328 check auf gleiche laenge x und improtcol
  ##160616 match hk zeigt die duplikated zeilen statt mytabl falls ein Fehler kommt
  #   x = transkripte_eqtl$nuid
  #   y = ilmnAnnot013$nuid

  yname = deparse(substitute(y))

  # 150119 unique check auf schnelles duplicated umgestellt, auto makeuniuq
  if(testunique ==T){
    check = as.numeric(sum(duplicated(na.omit(y))))
    if(identical(check, 0)) return(match(x, y, incomparables=c(NA, NaN),...))

    if(identical(check, 0)==F  & makeunique == F) {
      print(y[duplicated(y)])
      stop(paste(yname ,"ist nicht unique"))
    }

    if(identical(check, 0)==F  & makeunique == T) {

      ## try to make it nunique
      if(is.null(importcol)) stop("When asking for make unique, please provide vector with values to be imported")
      if(length(importcol) != length(y)) stop("When asking for make unique, please provide vector with values to be imported")

      matcher = unique(data.frame(index = y, importcol = importcol))
      matcher = matcher[ matcher$index %in% x,]
      matchercheck = as.numeric(sum(duplicated(na.omit(matcher$index))))
      if(identical(matchercheck, 0)==F  ) {
        print(matcher[allDuplicatedEntries(matcher$index),])
        stop(paste(yname ,"ist nicht unique after trying to make index and importcol unique..."))
      }
      return(match(x, y, incomparables=c(NA, NaN),...))
      indinfo[match(x, y, incomparables=c(NA, NaN)),id_prepro_ge1]

    }



  }
  if(testunique ==F)  return(match(x, y, incomparables=c(NA, NaN),...))
}


### read all files from dir
getFilenamesInFolder <- function (erkennungsstring = "\\.txt$", on_server = Sys.info()['sysname']=="Linux", wd = getwd()) {
  # 10.6 zerhacken bei unix bei tab
  library(stringr)
  initial_wd = getwd()
  setwd(wd)
  if(on_server) {system("dir > verzeichnisinfo");system("rm verzeichnisinfo");system("dir > verzeichnisinfo")}
  if(on_server==F) {shell("dir > verzeichnisinfo");shell("DEL verzeichnisinfo");shell("dir > verzeichnisinfo")}  # falls von win gestartet

  options(encoding="WINDOWS-1252")
  files <- readLines(con="verzeichnisinfo")
  options(encoding="UTF-8")

  files

  if(on_server == F){
    files <- files[grep(erkennungsstring, x=files)]
    viertes = sapply(str_split(files, " +"), "[", 4)
    mystart <- str_locate(files, pattern=viertes)[,"start"]
    myend <- str_length(files)

    files <- str_sub(string=files, start=mystart, end=myend)
    files <- files[grep(erkennungsstring, x=files)]
    setwd(initial_wd)
    return(files)
  }
  if(on_server ){
    files <- unlist(str_split(files,"\t"))
    files <- files[grep(erkennungsstring, x=files)]
    files = str_replace_all(files, "\\\\", "")
    setwd(initial_wd)
    return(files)
  }

}


##..................................................................................
## data analysis
##..................................................................................

### odds ratio
# http://www.r-bloggers.com/computing-odds-ratios-in-r/

oddsratioWald <- function(n00, n01, n10, n11, alpha = 0.05){
  #
  #  Compute the odds ratio between two binary variables, x and y,
  #  as defined by the four numbers nij:
  #
  #    n00 = number of cases where x = 0 and y = 0
  #    n01 = number of cases where x = 0 and y = 1
  #    n10 = number of cases where x = 1 and y = 0
  #    n11 = number of cases where x = 1 and y = 1
  #
  OR <- (n00 * n11)/(n01 * n10)
  #
  #  Compute the Wald confidence intervals (1-alpha)%CI):
  #
  siglog <- sqrt((1/n00) + (1/n01) + (1/n10) + (1/n11))
  zalph <- qnorm(1 - alpha/2)
  logOR <- log(OR)
  loglo <- logOR - zalph * siglog
  loghi <- logOR + zalph * siglog
  #
  ORlo <- exp(loglo)
  ORhi <- exp(loghi)
  #
  oframe <- data.frame(LowerCI = ORlo, OR = OR, UpperCI = ORhi, alpha = alpha)
  oframe
}


### hypergeometric enrichment testing
hypertest2 <- function(besondere,gezogene,hintergrund, more=T, unique=T){   #besondere  zu testen auf anreicherung(more=T) oder abreicherung (more=F) in gezogene bei gegebenem Hintergrund hintergrund. besondere,gezogene und hintergrund koennen die laengen der listen sein oder die listen selber
  #http://r.789695.n4.nabble.com/hypergeometric-vs-fisher-test-tt2324223.html
  #in diesem www-Beispiel sind die weissen Kugeln a+b und entsprechen allen Elemente der eingabe "besondere", z.b. allen eQTL SNPs
  #alle gezogenen Kugeln sind a+c und entsprechen der Eingabe "gezogene" und sind z.b. alle GWAS SNPs, egal ob eQTL oder nich eQTL
  #alle Elemente der Urne sind a+b+c+d und entsprechen dem Hintergrund "hintergrund", e.g. allen dbSNP eintraegen mit dem gleichen unspezifischen Filtern wie die Liste (e.g. alle dbSNP SNPs, die auf eQTL getestet wurden)
  if(all(is.character(besondere),is.character(gezogene), is.character(hintergrund))==F) stop("input nicht vom typ chr")
  if(all(besondere %in% hintergrund)==F) stop("nicht alle 'besondere' (alle weissen Kugeln) in 'hintergrund' (der Hintergrund)")
  if(all(gezogene %in% hintergrund)==F) stop("nicht alle 'gezogene' (alle gezogenen Kugeln) in 'hintergrund' (der Hintergrund)")
  if(any(is.na(besondere),is.na(besondere),is.na(besondere))==T) stop("NA in den daten versteckt!")
  if(unique==T){
    besondere <- unique(besondere)
    gezogene <- unique(gezogene)
    hintergrund <- unique(hintergrund)
  }
  aa <- sum(besondere %in% gezogene) #alle weissen gezogenen
  bb <- length(besondere) -aa # alle weissen nicht gezogenen
  cc <- length(gezogene) - aa # alle schwarzen gezogenen
  dd <- length(hintergrund) - cc - bb -aa ## alle schwarzen nicht gezogen
  pval= phyper(aa-1,aa+bb,cc+dd,aa+cc,lower.tail=!more)
  in_gezogen <- round((aa/(aa+cc))*100,3)
  in_bk <-  round(((aa+bb)/(aa+bb+cc+dd))*100,3)
  enr <- round(in_gezogen/in_bk,3)

  mymatrix =  matrix(c(aa,bb,cc,dd),nrow = 2)
  or = fisher.test(mymatrix)
  pvalfisher  = or$p.value
  message(paste(in_gezogen,"% vs. ", in_bk,"% Enrichment:",  enr ,"OR (95%CI) =", signif(or$estimate,3), paste0("(", signif(or$conf.int[1],3), "-", signif(or$conf.int[2]), ")"), sep=" "))
  message(paste("p hypergeomtrisch=", signif(pval,3), 'p fisher', signif(pvalfisher,3)))
  message(paste(aa, "in", aa+cc, "gezogenen vs.", aa+bb, "in", aa+bb+cc+dd, "(grundgesamtheit)", sep=" "))
  res = list(in_gezogen = in_gezogen, in_bk = in_bk, enrichment = enr, pval = pval, or = or$estimate, or_lower = or$conf.int[1], or_upper = or$conf.int[2], matrix = mymatrix)
}


### confidence intervalls to p values see http://www.bmj.com/content/343/bmj.d2304
ci2p <- function (Est,u,l) {
  SE = (u - l)/(2*1.96)
  z = Est/SE
  P = exp(-0.717*z - 0.416*z^2)
  P
}




##..................................................................................
## data plotting
##..................................................................................

### marginal plot

marginal_plot = function(x, y, group = NULL, data = NULL, lm_formula = y ~ x, bw = "nrd0", alpha = 1, plot_legend = T, ...){
  # https://github.com/ChrKoenig/R_marginal_plot
  require(scales)
  ###############
  # Plots a scatterplot with marginal probability density functions for x and y.
  # Data may be grouped or ungrouped.
  # For each group, a linear fit is plotted. The model can be modified using the 'lm_formula' argument. Setting 'lm_formula' to NULL prevents plotting model fits.
  # The 'bw' argument specifies the bandwidth rule used for estimating probability density functions. See ?density for more information.
  # For large datasets, opacity may be decreased by setting alpha to a value between 0 and 1.
  # Additional graphical parameters are passed to the main plot, so you can customize axis labels, titles etc.
  ###############
  moreargs = eval(substitute(list(...)))

  # prepare consistent df
  if(missing(group)){
    if(missing(data)){
      if(length(x) != length(y)){stop("Length of arguments not equal")}
      data = data.frame(x = as.numeric(x), y = as.numeric(y))
    } else {
      data = data.frame(x = as.numeric(data[,deparse(substitute(x))]),
                        y = as.numeric(data[,deparse(substitute(y))]))
    }
    if(sum(!complete.cases(data)) > 0){
      warning(sprintf("Removed %i rows with missing data", sum(!complete.cases(data))))
      data = data[complete.cases(data),]
    }
    group_colors = "black"
  } else {
    if(missing(data)){
      if(length(x) != length(y) | length(x) != length(group)){stop("Length of arguments not equal")}
      data = data.frame(x = as.numeric(x), y = as.numeric(y), group = as.factor(group))
    } else {
      data = data.frame(x = as.numeric(data[,deparse(substitute(x))]),
                        y = as.numeric(data[,deparse(substitute(y))]),
                        group = as.factor(data[,deparse(substitute(group))]))
    }
    if(sum(!complete.cases(data)) > 0){
      warning(sprintf("Removed %i rows with missing data", sum(!complete.cases(data))))
      data = data[complete.cases(data),]
    }
    data = subset(data, group %in% names(which(table(data$group) > 5)))
    data$group = droplevels(data$group)
    group_colors = rainbow(length(unique(data$group)))
  }

  # log-transform data (this is need for correct plotting of density functions)
  if(!is.null(moreargs$log)){
    if(!moreargs$log %in% c("y", "x", "yx", "xy")){
      warning("Ignoring invalid 'log' argument. Use 'y', 'x', 'yx' or 'xy.")
    } else {
      data = data[apply(data[unlist(strsplit(moreargs$log, ""))], 1, function(x) !any(x <= 0)), ]
      data[,unlist(strsplit(moreargs$log, ""))] = log10(data[,unlist(strsplit(moreargs$log, ""))])
    }
    moreargs$log = NULL # remove to prevent double logarithm when plotting
  }

  # Catch unwanted user inputs
  if(!is.null(moreargs$col)){moreargs$col = NULL}
  if(!is.null(moreargs$type)){moreargs$type = "p"}

  # get some default plotting arguments
  if(is.null(moreargs$xlim)){moreargs$xlim = range(data$x)}
  if(is.null(moreargs$ylim)){moreargs$ylim = range(data$y)}
  if(is.null(moreargs$xlab)){moreargs$xlab = deparse(substitute(x))}
  if(is.null(moreargs$ylab)){moreargs$ylab = deparse(substitute(y))}
  if(is.null(moreargs$las)){moreargs$las = 1}

  # plotting
  tryCatch(expr = {
    ifelse(!is.null(data$group), data_split <- split(data, data$group), data_split <- list(data))
    orig_par = par(no.readonly = T)
    par(mar = c(0.25,5,1,0))
    layout(matrix(1:4, nrow = 2, byrow = T), widths = c(10,3), heights = c(3,10))

    # upper density plot
    plot(NULL, type = "n", xlim = moreargs$xlim, ylab = "density",
         ylim = c(0, max(sapply(data_split, function(group_set) max(density(group_set$x, bw = bw)$y)))), main = NA, axes = F)
    axis(2, las = 1)
    mapply(function(group_set, group_color){lines(density(group_set$x, bw = bw), col = group_color, lwd = 2)}, data_split, group_colors)

    # legend
    par(mar = c(0.25,0.25,0,0))
    plot.new()
    if(!missing(group) & plot_legend){
      legend("center", levels(data$group), fill = group_colors, border = group_colors, bty = "n", title = deparse(substitute(group)), title.adj = 0.1)
    }

    # main plot
    par(mar = c(4,5,0,0))
    if(missing(group)){
      do.call(plot, c(list(x = quote(data$x), y = quote(data$y), col = quote(scales::alpha("black", alpha))), moreargs))
    } else {
      do.call(plot, c(list(x = quote(data$x), y = quote(data$y), col = quote(scales::alpha(group_colors[data$group], alpha))), moreargs))
    }
    axis(3, labels = F, tck = 0.01)
    axis(4, labels = F, tck = 0.01)
    box()

    if(!is.null(lm_formula)){
      mapply(function(group_set, group_color){
        lm_tmp = lm(lm_formula, data = group_set)
        x_coords = seq(min(group_set$x), max(group_set$x), length.out = 100)
        y_coords = predict(lm_tmp, newdata = data.frame(x = x_coords))
        lines(x = x_coords, y = y_coords, col = group_color, lwd = 2.5)
      }, data_split, rgb(t(ceiling(col2rgb(group_colors)*0.8)), maxColorValue = 255))
    }

    # right density plot
    par(mar = c(4,0.25,0,1))
    plot(NULL, type = "n", ylim = moreargs$ylim, xlim = c(0, max(sapply(data_split, function(group_set) max(density(group_set$y, bw = bw)$y)))), main = NA, axes = F, xlab = "density")
    mapply(function(group_set, group_color){lines(x = density(group_set$y, bw = bw)$y, y = density(group_set$y, bw = bw)$x, col = group_color, lwd = 2)}, data_split, group_colors)
    axis(1)
  }, finally = {
    par(orig_par)
  })
}




### nice 3d plot interactive

plotte3D <- function (x, y,z, tocolor, dataframe, mylabels, mysize= 1.5) {
  require(threejs)
  require(data.table)
  setDT(dataframe)
  farbe = dataframe[, factor(get(tocolor))]
  farbentopf = rainbow(length(unique(farbe)))
  farbe = as.character(factor(farbe, labels = farbentopf) )
  try(scatterplot3js(dataframe[,c(x, y, z), with  = F], color=farbe, labels=mylabels, size=mysize, renderer="canvas" ))
}


### Function for arranging ggplots. example:   multiplot(p1, p2, p3, p4, cols=2). It can take any number of plot objects as arguments, or if it can take a list of plot objects passed to plotlist.

multiplot <- function(..., plotlist=NULL,  cols=1, layout=NULL) {
  ## http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


### plotte mean und se oder sd df im long format als boesen dynamite plot
plotMeanSE= function(categs, werte, plotteSD=F, ...) {
  library(sciplot)
  if(plotteSD==F) bargraph.CI(x.factor = categs, response = werte,...)
  if(plotteSD==T) bargraph.CI(x.factor = categs, response = werte, ci.fun = function(x) {
    fun = function(x) mean(x, na.rm=TRUE)
    c(fun(x)-sd(x), fun(x)+sd(x))
  },...)
}

## barplot with labels
barplot_hk = function(x,shift="calc", srt=0, dotable=T,cext=1, textrotate = 0, myround=3,showproz = T, colplot="darkred",...){   #x Vektor fuer Barplot, shift: abstand label und balken, dotable: tablefunktion fuer datenvektor, cext: verkleinerung x -achse,
  library(plotrix)
  if(dotable ==T) {mytable <- table(x, useNA="always");namesarg = names( table(x, useNA="always"))} else {mytable <- (x) ;namesarg = names(mytable)}
  barX <- barp(mytable,col=colplot, staxx=T, srt=srt, names.arg=namesarg, ...)
  if(shift=="calc") shift <- max(mytable)/30 else shift <- shift
  if(showproz) mylabel <- paste(as.character(mytable), " = ", as.character(round(mytable/sum(mytable), myround)*100), "%", sep="") else mylabel <- as.character(mytable)
  text(x=barX$x, y=mytable+shift, label=mylabel, cex=cext, srt = textrotate)
}


### qqplot from getting genetics done
qq.ggd = function ( pvector ,maxx = NULL, maxy = NULL, ... ) {
  #   #23/4/13 p = 0 abgefangen
  #   #06/05/2013 fehler korrigiert p <=1 statt p < 1
  # 3.2.15 p = 0 auf min p statt .Machine$double.xmin

  if ( !is.numeric ( pvector )) stop ( "D'oh! P value vector is not numeric." )

  if(any(pvector >1 | pvector <0 )) stop("Neg. values or values larger 1 found in pvector...Please remove them!")

  if(any(is.na(pvector))) {
    message("ooops, NA found in pvector...removing ", sum(is.na(pvector)), " entries...")
    pvector = pvector[ is.na(pvector) ==F]
  }

  pvector[pvector ==0] = min(pvector[ pvector >0])

  o = - log10 ( sort ( pvector , decreasing = F ))
  #e = -log10( 1:length(o)/length(o) )
  e = - log10 ( ppoints ( length ( pvector ) ))

  if(is.null(maxx)) maxx = max ( e )
  if(is.null(maxy)) maxy =  max ( o )

  plot ( e , o , pch = 19 , cex = 1 , xlab = expression ( Expected~~ - log [ 10 ]( italic ( p ))), ylab = expression ( Observed~~ - log [ 10 ]( italic ( p ))), xlim = c ( 0 , maxx), ylim = c ( 0 ,maxy), ... )
  abline ( 0 , 1 , col = "red" )
}


qq_conf = function(x, df=1, x.max = "auto",
                        main="QQ plot",plotType="pval",
                        sub=paste(""),
                        xlab="Expected", ylab="Observed",
                        conc=c(0.025, 0.975), overdisp=FALSE, trim=0.5,
                        slope.one=T, slope.lambda=FALSE,
                        thin=c(0.25,100), oor.pch=24, col.shade="gray", ofname="qqchi.pdf",
                        h=6,w=6,printpdf=F,myxlim="auto",point_cex=0.5,point_col = 1, point_pch = 1,...) {
  # From
  #   Jean Morrison[SMTP:MORRISON.JEANV@GMAIL.COM]
  #   Gesendet: Montag, 13. Dezember 2010 19:26:20
  #
  #   Hi Kirsten,
  #   Here is the script. The first chunk is all comments that you might want to read for best results - you should be able to just open it with a text editor to see the comments. One of the arguments is a thinning argument which reduces the number of points on the plot so that your file sizes won't get too huge. I like to use c(0.5, 500) just as a data point.
  # Jean

  # CHANGELOG
  # ...............
  # Added by Anna P. to allow for plotting log(P-value) instead of
  # 2*log(P-value) to make it compatible with chisq distribution with 2 df

  # 13/04/18
  # included auto x.max *das ist inder logik dieses skripts

  # 2013/06/05
  # x.max umgestellt, da sonst manch -inf berechnet wurde fuer x.max

  # 27.1.15 sub parameter i.e.  x achse untertitel raus
  #
  #
  # 9.3.15 myxlim auf auto umgestellt, damit per default alles geplottet
  #   # output as data.frame to improve behaviour in schleifen
  # 25.4.15 coloring and pch option included (point_col, point_pch..) and inflation from mean changed to median
  if(x.max == "auto") x.max = max(-log10(x))+2
  if(any(is.na(x))) stop("Please remove NA from p val vector....")

  if (plotType == "pval") {
    if(any(x==0)) stop("Please remove 0 from p val vector....")
    x <- -2*log(x)
    scale <- 0.5/log(10)
    x.max <- x.max/scale
    df <- 2
  } else {
    scale <- 1
  }

  # Function to shade concentration band

  shade <- function(x1, y1, x2, y2, color=col.shade) {
    n <- length(x2)
    polygon(c(x1, x2[n:1]), c(y1, y2[n:1]), border=NA, col=color)
  }

  # Sort values and see how many out of range  #hk ergaenzt um mit zu sortierende attribute mitzufuehren
  library(data.table)
  daten = data.table(x = x, point_cex = point_cex, point_col = point_col, point_pch = point_pch)
  daten = daten[is.na(x)==F]
  #   print(head(daten,9))
  setorder(daten, x)
  # print(head(daten,19))
  point_cex = if(length(point_cex)==1) point_cex else daten$point_cex
  point_col = if(length(point_col)==1) point_col else daten$point_col
  point_pch = if(length(point_pch)==1) point_pch else daten$point_pch


  #   obsvd <- sort(x, na.last=NA)
  x = daten$x
  obsvd = x

  N <- length(obsvd)
  if (missing(x.max)) {
    Np <- N
  }
  else {
    Np <- sum(obsvd<=x.max)
  }
  if(Np==0)
    stop("Nothing to plot")

  # Expected values

  if (df==2) {
    expctd <- 2*cumsum(1/(N:1))
  }
  else {
    expctd <- qchisq(p=(1:N)/(N+1), df=df)
  }

  # Concentration bands

  if (!is.null(conc)) {
    if(conc[1]>0) {
      e.low <- qchisq(p=qbeta(conc[1], 1:N, N:1), df=df)
    }
    else {
      e.low <- rep(0, N)
    }
    if (conc[2]<1) {
      e.high <- qchisq(p=qbeta(conc[2], 1:N, N:1), df=df)
    }
    else {
      e.high <- 1.1*rep(max(x),N)
    }
  }

  # Plot outline

  if (Np < N)
    top <- x.max
  else
    top <- obsvd[N]
  right <- expctd[N]
  if(myxlim=="auto") myxlim = ceiling(log10(N))+0.5
  if (printpdf) {pdf(ofname,h,w)}
  plot(c(0, right*scale), c(0, top*scale), type="n", xlab=xlab, ylab=ylab, bty="l",
       # main=main, sub=sub, cex.lab=1.25, # 27.1.15 sub raus
       main=main,  cex.lab=1.25,
       cex.axis=1.1,ylim=c(0,x.max*scale), xlim=c(0,myxlim))

  # Thinning

  if (is.na(thin[1])) {
    show <- 1:Np
  }
  else if (length(thin)!=2 || thin[1]<0 || thin[1]>1 || thin[2]<1) {
    warning("invalid thin parameter; no thinning carried out")
    show <- 1:Np
  }
  else {
    space <- right*thin[1]/floor(thin[2])
    iat <- round((N+1)*pchisq(q=(1:floor(thin[2]))*space, df=df))
    if (max(iat)>thin[2])
      show <- unique(c(iat, (1+max(iat)):Np))
    else
      show <- 1:Np
  }
  Nu <- floor(trim*N)
  #    Nl <- floor(0.3*N)
  if (Nu>0)
    lambda <- median(obsvd[1:Nu])/median(expctd[1:Nu])
  #      lambda <- mean(obsvd[Nl:Nu])/mean(expctd[Nl:Nu])
  if (!is.null(conc)) {
    if (Np<N)
      vert <- c(show, (Np+1):N)
    else
      vert <- show
    if (overdisp)
      shade(expctd[vert]*scale, lambda*e.low[vert]*scale,
            expctd[vert]*scale, lambda*e.high[vert]*scale)
    else
      shade(expctd[vert]*scale, e.low[vert]*scale, expctd[vert]*scale, e.high[vert]*scale)
  }
  point_cex = if(length(point_cex)==1) point_cex else point_cex[show]
  point_col = if(length(point_col)==1) point_col else point_col[show]
  point_pch = if(length(point_pch)==1) point_pch else point_pch[show]
  points(expctd[show]*scale, obsvd[show]*scale,cex=point_cex,col = point_col, pch = point_pch, ...)
  # Overflow
  if (Np<N) {
    over <- (Np+1):N
    points(expctd[over]*scale, rep(x.max, N-Np)*scale, pch=oor.pch)
  }
  # Lines
  line.types <- c("solid", "dashed", "dotted")
  key <- NULL
  txt <- NULL
  if (slope.one) {
    key <- c(key, line.types[1])
    txt <- c(txt, "y = x")
    abline(a=0, b=1, lty=line.types[1])
  }
  if (slope.lambda && Nu>0) {
    key <- c(key, line.types[2])
    txt <- c(txt, paste("y = ", format(lambda, digits=4), "x", sep=""))
    if (!is.null(conc)) {
      if (Np<N)
        vert <- c(show, (Np+1):N)
      else
        vert <- show
    }
    abline(a=0, b=lambda, lty=line.types[2])
  }
  if (printpdf) {dev.off()}
  # Returned value

  #    if (!is.null(key))
  #       legend(0, top*scale, legend=txt, lty=key, bty="n")
  data.frame(N=N, omitted=N-Np, lambda=lambda)

}



### normality check
norm_plot2 <- function(vektor,  x_lim=c(min(vektor)-min(vektor)*.5, max(vektor)+max(vektor)*.1), titelgroesse = 1, mybreaks = NULL,vektorname = NULL , ...) {
  #27.8.15 parameter vektorname
  par(mfrow=c(1,2))
  if(is.null(mybreaks)) mybreaks = as.integer(length(vektor)/5)
  # print(mybreaks)
  varname <- ifelse(length(vektorname)==0, deparse(substitute(vektor)),vektorname)
  hist(vektor,freq=T, breaks=mybreaks, labels=T, col="antiquewhite", xlim=x_lim, main = paste0("Histogram of ", varname), cex.main = titelgroesse, ...)
  mtext(paste0("ks-test pval = ", signif(ks.test(vektor,"pnorm", mean(vektor), sd(vektor))$p.value,3)))
  qqnorm(vektor,main = paste0("QQ-plot of ", varname), cex.main = titelgroesse,...); qqline(vektor, col = 2)
  if(length(vektor) >5000) shapvektor = sample(vektor, size=5000, replace=F) else shapvektor = vektor
  mtext(paste0("shapiro wilk pval = ",signif(shapiro.test(shapvektor)$p.value,3) ))
  par(mfrow = c(1,1))
}


### ggplot costum theme
theme_peter = function(beschriftungszahlengroesse = 20, relfak = 2) {
  theme_peter = theme_grey() + theme(axis.text.x = element_text(angle=0, hjust=0, size = beschriftungszahlengroesse), axis.text.y = element_text(size = beschriftungszahlengroesse), axis.title= element_text(size = beschriftungszahlengroesse), legend.title=element_text(size = rel(relfak)), legend.text=element_text(size = rel(relfak)), strip.text = element_text(face="bold", size=rel(relfak)))
  theme_peter
}

### nice correlation plots
nicepairs = function(x, punktcol = rgb(0,0,0,0.6), punktform = 4,cortype = "spearman", ...)  {
  ##19/1/16
  library(RColorBrewer)
  #
  panel.cor = function(x, y, digits=2, prefix="")
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    isna <- is.na(x)==F & is.na(y)==F
    r <- cor(x[isna], y[isna], method = cortype)
    pval = cor.test(x[isna], y[isna], method = cortype)$p.value
    pvalstars = showStars(pval)
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, pvalstars, sep="")
    cex_todo = 1+abs(r)

    colorrule = data.frame(myvalue  =seq(0,1,0.125), mycolor = mypalette<-brewer.pal(9,"YlOrRd"))
    bg = colorrule[ abs(r)>colorrule$myvalue & abs(r) <= (colorrule$myvalue +0.125), "mycolor"]
    ll <- par("usr")
    rect(ll[1], ll[3], ll[2], ll[4], col=bg)

    text(0.5, 0.5, txt, cex = cex_todo,  col = ifelse(r <0, "red", "black"))
  }
  panel.smooth2 = function (x, y,  bg = NA, pch =punktform,
                            cex = 1, col = punktcol, col.smooth = "red", span = 2/3, iter = 3, ...)
  {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex, ...)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
      lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = col.smooth)
  }
  pairs(x,lower.panel=panel.cor, upper.panel=panel.smooth2)
}



### Plot a non-proportional 2-Way, 3-Way or 4-Way Venn Diagram
### http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/vennDia.R
### NOTE from t.girke: This script has been replaced by overLapper.R, which provides much more  powerful and scalable utilities. The new overLapper.R script is available at:   http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/R_BioCondManual.html#R_graphics_venn


## Define venndiagram function and three wrappers
venndiagram <- function(x=x, y=y, z=z, w=w, unique=T, title="Venn Diagram", labels=c("x", "y", "z", "w"), lines=1, lcol=1, tcol=1, diacol=1, plot=T, type="3", printsub=TRUE, ...) {
  ## Remove duplicates and NA fields in x, y, z and w
  if(unique==T) {
    x <- unique(x); x <- as.vector(na.omit(x))
    y <- unique(y); y <- as.vector(na.omit(y))
    if(!missing("z")) {
      z <- unique(z); z <- as.vector(na.omit(z))
    }
    if(!missing("w")) {
      w <- unique(w); w <- as.vector(na.omit(w))
    }
  }

  ## Check valid type selection
  if(!type %in% c("2", "2map", "3", "3map", "4", "4map", "4el", "4elmap")) {
    return("Error: the 'type' argument can only be set to one of these values: 2, 2map, 3, 3map, 4, 4map, 4el, 4elmap.")
  }

  ## Plot a 2-way venn diagram
  if(type=="2") {
    # Define ovelap queries
    q1 <- x[x %in% y]
    q2 <- x[!x %in% y]
    q3 <- y[!y %in% x]

    ## Store query vectors in list
    qlist <- list(q1=q1, q2=q2, q3=q3)

    ## Perfom query counts
    count <- unlist(lapply(qlist, length))
    countDF <- data.frame(query=names(count) , count=as.vector(count))
    olDF <- data.frame(x=c(5.0, 3.1, 7.0), y=c(6.1, 6.1, 6.1), count=countDF$count)
    if(printsub==TRUE) {mysub <- paste(paste("N unique: xy =", length(unique(c(x,y)))), paste("; x =", length(unique(x))), paste("; y =", length(unique(y))), sep="")} else {mysub <- ""}
    if(plot==T) {
      ## Plot the 2-way venn diagram
      symbols(x=c(4, 6), y = c(6, 6), circles=c(2, 2), xlim=c(0, 10), ylim=c(0, 10), inches=F, main=title, sub=mysub, xlab="", ylab="",  xaxt="n", yaxt="n", bty="n", fg=lines, ...);
      text(olDF$x, olDF$y, olDF$count, col=tcol, ...); text(c(2.0, 8.0), c(8.8, 8.8), labels[1:2], col=lcol, ...)
    }

    ## Return query list
    return(qlist)
  }

  ## Plot 2-way mapping venn diagram
  if(type=="2map") {
    olDFdebug <- data.frame(x=c(5.0, 3.1, 7.0), y=c(6.1, 6.1, 6.1), count=paste("q", 1:3, sep=""), ...)
    symbols(x=c(4, 6), y = c(6, 6), circles=c(2, 2), xlim=c(0, 10), ylim=c(0, 10), inches=F, main="Mapping Venn Diagram", xlab="", ylab="",  xaxt="n", yaxt="n", bty="n", fg=lines, ...);
    text(olDFdebug$x, olDFdebug$y, olDFdebug$count, col=tcol, ...); text(c(2.0, 8.0), c(8.8, 8.8), paste(labels[1:2], "=", c("x","y")), col=lcol, ...)
  }

  ## Plot a 3-way venn diagram
  if(type=="3") {
    ## Define ovelap queries
    q1 <- x[x %in% y & x %in% z]
    q2 <- x[x %in% z]; q2 <- q2[!q2 %in% y]
    q3 <- y[y %in% z]; q3 <- q3[!q3 %in% x]
    q4 <- x[x %in% y]; q4 <- q4[!q4 %in% z]
    q5 <- x[!x %in% y]; q5 <- q5[!q5 %in% z]
    q6 <- y[!y %in% z]; q6 <- q6[!q6 %in% x]
    q7 <- z[!z %in% x]; q7 <- q7[!q7 %in% y]

    ## Store query vectors in list
    qlist <- list(q1=q1, q2=q2, q3=q3, q4=q4, q5=q5, q6=q6, q7=q7)

    ## Perfom query counts
    count <- unlist(lapply(qlist, length))
    countDF <- data.frame(query=names(count) , count=as.vector(count))
    olDF <- data.frame(x=c(5.0, 3.8, 6.3, 5.0, 3.0, 7.0, 5.0), y=c(5.6, 4.6, 4.6, 6.9, 6.5, 6.5, 3.0), count=countDF$count)
    if(printsub==TRUE) {mysub <- paste(paste("N unique: xyz =", length(unique(c(x,y,z)))), paste("; x =", length(unique(x))), paste("; y =", length(unique(y))), paste("; z =", length(unique(z))), sep="")} else { mysub <- "" }
    if(plot==T) {
      ## Plot the 3-way venn diagram
      symbols(x=c(4, 6, 5), y = c(6, 6, 4), circles=c(2, 2, 2), xlim=c(0, 10), ylim=c(0, 10), inches=F, main=title, sub=mysub, xlab="", ylab="",  xaxt="n", yaxt="n", bty="n", fg=lines, ...);
      text(olDF$x, olDF$y, olDF$count, col=tcol, ...); text(c(2.0, 8.0, 6.0), c(8.8, 8.8, 1.1), labels[1:3], col=lcol, ...)
    }

    ## Return query list
    return(qlist)
  }

  ## Plot 3-way mapping venn diagram
  if(type=="3map") {
    olDFdebug <- data.frame(x=c(5.0, 3.8, 6.3, 5.0, 3.0, 7.0, 5.0), y=c(5.6, 4.6, 4.6, 6.9, 6.5, 6.5, 3.0), count=paste("q", 1:7, sep=""), ...)
    symbols(x=c(4, 6, 5), y = c(6, 6, 4), circles=c(2, 2, 2), xlim=c(0, 10), ylim=c(0, 10), inches=F, main="Mapping Venn Diagram", xlab="", ylab="",  xaxt="n", yaxt="n", bty="n", fg=lines, ...);
    text(olDFdebug$x, olDFdebug$y, olDFdebug$count, col=tcol, ...); text(c(2.0, 8.0, 6.0), c(8.8, 8.8, 1.1), paste(labels[1:3], "=", c("x","y","z")), col=lcol, ...)
  }

  ## Overlap queries for 4-way venn diagram
  if(type=="4" | type=="4el" | type=="4elmap") {
    ## Define ovelap queries
    xy <- x[x %in% y]; xz <-x[x %in% z]; xw <- x[x %in% w]; yz <- y[y %in% z]; yw <- y[y %in% w]; zw <- z[z %in% w]
    q1 <- xy[xy %in% zw]
    q2 <- xw[xw %in% z]; q2 <- q2[!q2 %in% y]
    q3 <- yz[yz %in% w]; q3 <- q3[!q3 %in% x]
    q4 <- yz[yz %in% x]; q4 <- q4[!q4 %in% w]
    q5 <- xw[xw %in% y]; q5 <- q5[!q5 %in% z]
    q6 <- xy[!xy %in% z]; q6 <- q6[!q6 %in% w]
    q7 <- zw[!zw %in% x]; q7 <- q7[!q7 %in% y]
    q8 <- xz[!xz %in% y]; q8 <- q8[!q8 %in% w]
    q9 <- yw[!yw %in% x]; q9 <- q9[!q9 %in% z]
    q10 <- x[!x %in% c(y,z,w)]
    q11 <- y[!y %in% c(x,z,w)]
    q12 <- z[!z %in% c(x,y,w)]
    q13 <- w[!w %in% c(x,y,z)]
    q14 <- xw[!xw %in% y]; q14 <- q14[!q14 %in% z]
    q15 <- yz[!yz %in% x]; q15 <- q15[!q15 %in% w]

    ## Store query vectors in list
    qlist <- list(q1=q1, q2=q2, q3=q3, q4=q4, q5=q5, q6=q6, q7=q7, q8=q8, q9=q9, q10=q10, q11=q11, q12=q12, q13=q13, q14=q14, q15=q15)

    ## Perfom query counts
    count <- unlist(lapply(qlist, length))
    countDF <- data.frame(query=names(count) , count=as.vector(count))
    olDF <- data.frame(x=c(4.8, 3.9, 5.7, 3.9, 5.7, 4.8, 4.8, 3.0, 6.5, 3.0, 6.5, 3.0, 6.5, 4.8, 4.8), y=c(5.2, 4.2, 4.2, 6.3, 6.3, 7.2, 3.2, 5.2, 5.2, 7.2, 7.2, 3.2, 3.2, 1.0, 0.4), count=countDF$count)

    if(printsub==TRUE) {mysub <- paste(paste("N unique: xyzw =", length(unique(c(x,y,z,w)))), paste("; x =", length(unique(x))), paste("; y =", length(unique(y))), paste("; z =", length(unique(z))), paste("; w =", length(unique(w))), sep="") } else { mysub <- "" }

    ## Plot 4-way venn diagram as circles
    if(plot==T & type=="4") {
      symbols(x=c(4, 5.5, 4, 5.5), y = c(6, 6, 4.5, 4.5), circles=c(2, 2, 2, 2), xlim=c(0, 10), ylim=c(0, 10), inches=F, main=title, sub=mysub, xlab="", ylab="",  xaxt="n", yaxt="n", bty="n", fg=lines, ...);
      text(olDF$x[1:13], olDF$y[1:13], olDF$count[1:13], col=tcol, ...) # rows 14-15 of olDF are printed in last step
      text(c(2.0, 7.5, 2.0, 7.5), c(8.3, 8.3, 2.0, 2.0), labels, col=lcol, ...)
      text(c(3.8, 3.8), c(1.0, 0.4), c(paste("Only in ", labels[1], " & ", labels[4], ": ", olDF$count[14], sep=""), paste("Only in ", labels[2], " & ", labels[3], ": ", olDF$count[15], sep="")), col=diacol, ...)
    }

    ## Plot 4-way venn diagram as ellipses
    if(plot==T & (type=="4el" | type=="4elmap")) {
      olDF <- data.frame(x=c(5.0, 4.2, 6.4, 3.6, 5.8, 2.9, 7.1, 3.1, 6.9, 1.5, 3.5, 6.5, 8.5, 5.0, 5.0), y=c(2.8, 1.4, 4.0, 4.0, 1.4, 5.9, 5.9, 2.2, 2.2, 4.8, 7.2, 7.2, 4.8, 0.7, 6.0), count=countDF$count)
      ## Plot ellipse
      plotellipse <- function (center=c(1,1), radius=c(1,2), rotate=1, segments=360, xlab="", ylab="", ...) {
        angles <- (0:segments) * 2 * pi/segments
        rotate <- rotate*pi/180
        ellipse <- cbind(radius[1] * cos(angles), radius[2] * sin(angles))
        ellipse <- cbind( ellipse[,1]*cos(rotate) + ellipse[,2]*sin(rotate), ellipse[,2]*cos(rotate) - ellipse[,1]*sin(rotate) )
        ellipse <- cbind(center[1]+ellipse[,1], center[2]+ellipse[,2])
        plot(ellipse, type = "l", xlim = c(0, 10), ylim = c(0, 10), xlab = "", ylab = "", ...)
      }
      ## Plot ellipse as 4-way venn diagram
      ellipseVenn <- function(lines=lines, olDF, title=title, labels=labels, sub=mysub, main, lcol=lcol, tcex=1.3, ...) {
        split.screen(c(1,1))
        plotellipse(center=c(3.5,3.6), radius=c(2,4), rotate=-35, segments=360, xlab="", ylab="", col=lines[1], axes=FALSE, main=title, sub=mysub, ...)
        screen(1, new=FALSE)
        plotellipse(center=c(4.7,4.4), radius=c(2,4), rotate=-35, segments=360, xlab="", ylab="", col=lines[2], axes=FALSE, ...)
        screen(1, new=FALSE)
        plotellipse(center=c(5.3,4.4), radius=c(2,4), rotate=35, segments=360, xlab="", ylab="", col=lines[3], axes=FALSE, ...)
        screen(1, new=FALSE)
        plotellipse(center=c(6.5,3.6), radius=c(2,4), rotate=35, segments=360, xlab="", ylab="", col=lines[4], axes=FALSE, ...)
        text(olDF[1:15,1], olDF[1:15,2], olDF[1:15,3], col=tcol, ...)
        text(c(0.4, 2.8, 7.5, 9.4), c(7.3, 8.3, 8.3, 7.3), labels, col=lcol, ...)
        close.screen(all=TRUE)
      }
      ## Plot 4-way ellipse venn diagram
      if(type=="4el") {
        ellipseVenn(olDF=olDF, lcol=lcol, lines=lines, labels=labels, title=title, ...)
      }

      ## Plot 4-way ellipse mapping venn diagram
      if(type=="4elmap") {
        olDFdebug <- data.frame(x=c(5.0, 4.2, 6.4, 3.6, 5.8, 2.9, 7.1, 3.1, 6.9, 1.5, 3.5, 6.5, 8.5, 5.0, 5.0), y=c(2.8, 1.4, 4.0, 4.0, 1.4, 5.9, 5.9, 2.2, 2.2, 4.8, 7.2, 7.2, 4.8, 0.7, 6.0), count=paste("q", 1:15, sep=""), ...)
        ellipseVenn(olDF=olDFdebug, lcol=lcol, lines=lines, labels=paste(labels, "=", c("x","y","z","w")), title="Mapping Venn Diagram", ...)
      }
    }

    ## Return query list
    return(qlist)
  }

  ## Plot 4-way circle mapping venn diagram
  if(type=="4map") {
    olDFdebug <- data.frame(x=c(4.8, 3.9, 5.7, 3.9, 5.7, 4.8, 4.8, 3.0, 6.5, 3.0, 6.5, 3.0, 6.5, 4.8, 4.8), y=c(5.2, 4.2, 4.2, 6.3, 6.3, 7.2, 3.2, 5.2, 5.2, 7.2, 7.2, 3.2, 3.2, 1.0, 0.4), count=paste("q", 1:15, sep=""), ...)
    symbols(x=c(4, 5.5, 4, 5.5), y = c(6, 6, 4.5, 4.5), circles=c(2, 2, 2, 2), xlim=c(0, 10), ylim=c(0, 10), inches=F, main="Mapping Venn Diagram", xlab="", ylab="",  xaxt="n", yaxt="n", bty="n", fg=lines, ...);
    text(olDFdebug$x[1:13], olDFdebug$y[1:13], olDFdebug$count[1:13], col=tcol, ...); text(c(2.0, 7.5, 2.0, 7.5), c(8.3, 8.3, 2.0, 2.0), paste(labels, "=", c("x","y","z","w")), col=lcol, ...)
    text(c(3.8, 3.8), c(0.97, 0.36), c(paste("Only in ", labels[1], " & ", labels[4], ": ", olDFdebug$count[14], sep=""), paste("Only in ", labels[2], " & ", labels[3], ": ", olDFdebug$count[15], sep="")), col=tcol, ...)
  }

}


venn2 = function(x1,y1, mytitle="2-Way Venn Diagram", mylabels = NA, plotte =T)
{
  # 28/2/13 plotte par
  # 150119 vector check
  if(all(is.vector(x1) | is.factor(x1),is.vector(y1)|is.factor(y1))==F) stop("All input data must be vectors...")
  if(is.na(mylabels[1])) mylabels = c(deparse(substitute(x1)), deparse(substitute(y1)))
  qlist <- venndiagram(x=x1, y=y1, unique=T, title=mytitle, labels= mylabels, plot=plotte, lines=c(2,3), lcol=c(2,3), tcol=c(1,1,1), lwd=3, cex=1.3, printsub=T, type="2")
  qlist
}


venn3 = function(x1,y1,z1, mytitle="3-Way Venn Diagram", mylabels = NA,  plotte =T)
{
  # 28/2/13 plotte par
  # 150119 vector check
  if(all(is.vector(x1)|is.factor(x1),is.vector(y1)|is.factor(y1),is.vector(z1)|is.factor(z1))==F) stop("All input data must be vectors...")

  if(is.na(mylabels[1])) mylabels = c(deparse(substitute(x1)), deparse(substitute(y1)), deparse(substitute(z1)))
  qlist <- venndiagram(x=x1, y=y1, z=z1, unique=T, title=mytitle, labels= mylabels, plot=plotte, lines=c(2,3,4), lcol=c(2,3,4), tcol=c(1,1,1,1,1,1,1), lwd=3, cex=1.3, printsub=T, type="3")
  qlist
}

venn4 = function(x1,y1,z1,w1, mytitle="4-Way Venn Diagram", mylabels = NA,  plotte =T)
{
  # 13/07/03
  # 150119 vector check
  if(all(is.vector(x1)|is.factor(x1),is.vector(y1)|is.factor(y1),is.vector(z1)|is.factor(z1),is.vector(w1)|is.factor(w1))==F) stop("All input data must be vectors...")

  if(is.na(mylabels[1])) mylabels = c(deparse(substitute(x1)), deparse(substitute(y1)), deparse(substitute(z1)), deparse(substitute(w1)))
  qlist <- venndiagram(x=x1, y=y1, z=z1, w=w1, unique=T, title=mytitle, labels=mylabels,
                       plot=plotte, lines=c(2,3,4,6), lcol=c(2,3,4,6), tcol=1, lwd=3, cex=1, printsub=T, type="4el")
  qlist
}

## faerbe die zahlen einer gegebenen quadratisch -symmetrischen matrix nach ihrem wertm im 2 colordesign
plotCorMatrix <- function (m_psy, relaxissize = 2,rellabelsize = 15,textangle = 30,maxcolor = "darkred",mincolor = "white",rundestellen = 2) {
  library(ggplot2)
  library(reshape2)
  m_psy_melt = melt(m_psy)
  names(m_psy_melt) = c('Var1', 'Var2', 'value')

  labelcolor = maxcolor
  m_psy_melt$value = round(m_psy_melt$value,rundestellen)
  p_psybind <- ggplot(m_psy_melt, aes(Var1, Var2, label=value)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_gradient(low = mincolor, high = maxcolor) + geom_text(size  = rel(rellabelsize)) + xlab("") + ylab("") + theme(axis.text.x = element_text(angle=textangle, hjust=1, vjust=1), axis.text = element_text(color = labelcolor, size=rel(relaxissize))) + guides(fill=FALSE)
  p_psybind
}

## faerbe die zahlen einer gegebenen matrix nach ihrem oder einem externen WErt im 2 color schemewert

colplotLikeExcel = function(plotdat, mycolors = c("dodgerblue2", "white", "red"),lowest_colorval = "minimum", middle_colorval = "median", highest_colorval = "maximum", xlabel = "", ylabel = "", x_axis_pos = "top", myround = 0, userdefined_labels = NULL){

  # mycolors = c("dodgerblue2", "white", "red");lowest_colorval = "minimum"; middle_colorval = "median"; highest_colorval = "maximum"; xlabel = ""; ylabel = ""; x_axis_pos = "top"; myround = 0; userdefined_labels = NULL

  plotdat_ori = plotdat
  hh(plotdat_ori,9)
  plotdat = as.matrix(plotdat)
  # plotdat = plotdat[rev(rownames(plotdat)),]
  hh(plotdat)
  plotdat_m = reshape2::melt(plotdat)
  # ht(plotdat_m)
  if (is.null(rownames(plotdat_ori))) plotdat_m$Var1 = factor(plotdat_m$Var1) else plotdat_m$Var1 = factor(plotdat_m$Var1, levels = rev(unique(rownames(plotdat_ori))))
  if (is.null(colnames(plotdat_ori))) plotdat_m$Var2 = factor(plotdat_m$Var2) else  plotdat_m$Var2 = factor(plotdat_m$Var2, levels = unique(colnames(plotdat_ori)))
  if(is.numeric(plotdat_m$value)==F ) stop("Need numeric matrix as `plotdat` argument in order to know coloring according to provided numeric data! Stoping.\nConsider providing a userdefined matrix with names via parameter `userdefined_labels` in addition to providing numeric values via parameter `plotdat`.")



  plotdat_m$value =  round(plotdat_m$value, myround)
  if(lowest_colorval == "minimum")  lowest_colorval = min(plotdat_m$value, na.rm = T) else lowest_colorval = lowest_colorval
  if(middle_colorval == "median")  middle_colorval = median(plotdat_m$value, na.rm = T) else middle_colorval = middle_colorval
  if(highest_colorval == "maximum")  highest_colorval = max(plotdat_m$value, na.rm = T) else highest_colorval = highest_colorval

  if(is.null(userdefined_labels)) {
    plot1 = ggplot(plotdat_m, aes(Var2, Var1, label = value)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_gradientn(colours=mycolors, values=scales::rescale(c(lowest_colorval, middle_colorval, highest_colorval)), guide=FALSE) + geom_text(show.legend = FALSE) + scale_x_discrete(position = x_axis_pos) + xlab(xlabel)  + ylab(ylabel)
  } else {
    beschriftdat = as.matrix(userdefined_labels)
    beschriftdat = beschriftdat[rev(rownames(beschriftdat)),]
    stopifnot(identical(dim(plotdat), dim(beschriftdat)))
    stopifnot(identical(rownames(plotdat), rownames(beschriftdat)))
    stopifnot(identical(colnames(plotdat), colnames(beschriftdat)))



    beschriftdat_m = reshape2::melt(beschriftdat)
    plot1 = ggplot(plotdat_m, aes(Var2, Var1)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_gradientn(colours=mycolors, values=scales::rescale(c(lowest_colorval, middle_colorval, highest_colorval)), guide=FALSE) + geom_text(label = beschriftdat_m$value, show.legend = FALSE) + scale_x_discrete(position = x_axis_pos) + xlab(xlabel)  + ylab(ylabel)
  }
  plot1 +  theme(axis.text.x = element_text(angle = 90, hjust = 0))
}




## scatterplot mit tooltip
scatterplot_tooltip = function(xx,yy,tooltip, mycolor = "",mysize=5,  tooltipprefix ="", ...)
{
  #   xx = 1:1000
  #   yy = asinh(1:1000)
  #   tooltip = paste0("label", 1:1000)
  # library(data.table)
  library(metricsgraphics)
  library(stringr)
  dat = data.frame(xx,yy,tooltip, mycolor,mysize)
  names(dat) = c("myxx", 'myyy', 'mytooltip', 'mycolor', 'mysize')

  mystring = "function(d) {
  $('{{ID}} svg .mg-active-datapoint')
  .text('custom text : ' + d.point.mytooltip );
}"
  mystring = str_replace(mystring, "custom text :", tooltipprefix)


  dat %>%
    mjs_plot(x = myxx, y = myyy) %>%
    mjs_point(color_accessor = "mycolor",size_accessor='mysize', ...) %>%
    mjs_add_mouseover(mystring)
}



## scatterplot mit tooltip via ggvis (nicht interactive in knitr, aber in RStudio / R)


ggvis_tooltip <- function (mtc,xcol, ycol, tooltipcols, on_click = T) {
  library(ggvis)
  ## example
  # mtc <- mtcars
  # ggvis_tooltip(mtc, 'mpg', 'wt', tooltipcols= c("am", "gear"), on_click = F)

  stopifnot("id225847815" %in% names(mtc)==F)
  mtc$id225847815 <- 1:nrow(mtc)

  if("data.table" %in% class(mtc)) {
    library(data.table)
    setDF(mtc)
  }


  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- mtc[mtc$id225847815 == x$id225847815,c(tooltipcols) , drop = F]
    paste0(names(row), ": ", format(row), collapse = "<br />")
  }

  if(on_click) modus = "click" else modus = "hover"

  mtc %>% ggvis(x = ~get(xcol), y = ~get(ycol), key := ~id225847815) %>%
    layer_points() %>%    add_tooltip(all_values,modus)

}




##..................................................................................
## reporting inkl formatieren mit knitr & co
##..................................................................................

### copy to the clipboard
ccc <- function(x)write.table(x, "clipboard",sep="\t",row.names=F)
ccr <- function(x)write.table(x, "clipboard",sep="\t",row.names=T)

### signifikanzsterne
showStars = function (p)
{
  if (inherits(p, c("matrix", "data.frame")) && length(dim(p)) ==
      2) {
    apply(p, c(1, 2), showStars)
  }
  else {
    if (length(p) > 1) {
      sapply(p, showStars)
    }
    else {
      s <- ifelse(p > 0.05, "", ifelse(p > 0.01, "*",
                                       ifelse(p > 0.001, "**", "***")))
      s
    }
  }
}

### uhrzeit inkl. einheit
formateTimediff = function(timediff, mydigits = 3) paste0(format(unclass(timediff), digits = mydigits), " ", attr(timediff, "units"))

###  reverse gcta
reverseGCTA = function(x) {
  x = toupper(x)
  # print(x)
  y = ifelse(x =="A", "T", ifelse(x=="C", "G", ifelse(x=="G", "C", ifelse(x=="T", "A",NA)))) #  stop("alle must be any of a c g t A C G T")
  y
  }

### im Text als potenz schreiben, so dass es von knitr umgewandelt werden kann
potenzFormate = function(x, showdigits=1) {library(stringr);paste0(str_replace(formatC(x, digits=showdigits, format = "e"), "e", "x10^"),"^")}

### numbers as nice strings
huebsch = function(x, stellen =1) format(round(x,stellen), big.mark = ",")

### numbers as nice percentages
proz = function(x, stellen = 1) paste0(round(100*x, stellen), "%")



### wrapper, um ein RMD file von commandline aus zu starten
knitrHelper <- function(myfilename=filename, mypathwd=pathwd, computer = "amanMRO"){
  #version 25/6/12  6/8/12  7/8/12 21/8  15/01/19

  if(exists('filename') ==F) myfilename = "string_myfilenme_not_defined"
  if(exists('pathwd') ==F) mypathwd = getwd() ## neu 10.3.15

  message("Initialize knitting in RStudio!")
  message("-------------------------------------------------\n")
  message("push 'Knit HTML' button")

  message("\nInitialize knitting from command line")
  message("-------------------------------------------------\n")
  message(paste0("cd  ",mypathwd, ""))
  message("R")

  message('options(encoding="WINDOWS-1252")')
  message('options(encoding="UTF-8")')

  message('.libPaths("/mnt/ifs1_projekte/genstat/07_programme/rpackages/',computer,'")')
  message('require(knitr)')
  message('sessionInfo()')
  message(paste0("knit('",myfilename, ".Rmd')"))


  message("\nAfter this, you may have go tho this directory")
  message("and change file format of output using pandoc: \n")
  message(paste0("cd  ",mypathwd, ""))

  if(computer %in% c("forostar", "amanMRO", "dunhargRRO")) {
    pandoc_call = paste0("/usr/lib/rstudio-server/bin/pandoc/pandoc ",myfilename,".md --to html --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash-implicit_figures --output ",myfilename,".html --smart --email-obfuscation none --self-contained --standalone --section-divs --table-of-contents --toc-depth 3 --template /mnt/ifs1_projekte/genstat/07_programme/rpackages/",computer,"/rmarkdown/rmd/h/default.html --number-sections --css custom.css --variable 'theme:bootstrap' --mathjax --variable 'mathjax-url:https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --no-highlight --variable highlightjs=/mnt/ifs1_projekte/genstat/07_programme/rpackages/",computer,"/rmarkdown/rmd/h/highlight")
  } else {

    if(computer == "windows") {
      pandoc_call = paste0('"C:/userprograms/RStudio/bin/pandoc/pandoc" ',myfilename,'.md   --to html --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash-implicit_figures --output ',myfilename,'.html --smart --email-obfuscation none --self-contained --standalone --section-divs --table-of-contents --toc-depth 3 --template "C:\\PROGRA~1\\R\\R-32~1.0\\library\\RMARKD~1\\rmd\\h\\DEFAUL~1.HTM" --number-sections --css custom.css --variable "theme:united" --mathjax --variable "mathjax-url:https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML" --no-highlight --variable "highlightjs=C:\\PROGRA~1\\R\\R-32~1.0\\library\\RMARKD~1\\rmd\\h\\HIGHLI~1"')
    } else {

      pandoc_call = paste0('pandoc ', myfilename, '.md  -o ',  myfilename, '.html --self-contained --highlight-style=pygments \n')


    }


  }

  message(pandoc_call)
  message(paste0('\npandoc ', myfilename, '.md  -o ',  myfilename, '.docx --standalone --highlight-style=pygments\n')) # espresso waere schwarzer hintergrund

}

### end up a skript
finalizeSkript <- function(myfilename=filename, saveTheImage=F, dostr=F,mypathwd=pathwd){
  # 130418 variante do pdf entfernt, da man compile notebook in RStudio machen kann
  if(exists('myfilename') ==F) myfilename = "string_myfilenme_not_defined"
  if(exists('mypathwd') ==F) pathwd = getwd()

  message("==================================================================================")
  message("\n\nWarnings found so far:\n\n")

  print(warnings() )

  message("==================================================================================")
  message("\n\nSession Info::\n\n")

  print(sessionInfo())

  if(dostr==T) {
    message("==================================================================================")
    message("\n\nInfos about R-object included in R Session:\n\n")

    for(i in ls())   {
      if(mode(get(i)) != "function"){
        print("_________________________________")
        print(i)
        str(get(i))
      }

    }
  }
  if(saveTheImage==T) {
    setwd(mypathwd)
    save_filename <- paste0("obj/", myfilename, ".RData")
    message(paste("image saved under :\n", save_filename))
    save.image(file=save_filename)
    ## ZU TESTZWECKEN   load(
  }

  message("==================================================================================")
  message("\n\nTotal Time:\n\n")

  if(exists("time0")) print(Sys.time() -time0)

}

### write delimeted tab
write.delim = function(x, y, writeColnames=T,writeRownames = F, createDir = F, ...) {
  ## create Dir option hinyugefuegt
  # 8.2. rownameparameter hinzugefuegt
  if(createDir ==T ){
    library(stringr)
    oldwd = getwd()
    pathname = unlist(str_split(y, pattern="/"))
    pathname = paste(pathname[1:(length(pathname)-1)], collapse="/")
    vortest = try(setwd(pathname), silent=T)
    test = identical(vortest , oldwd)
    setwd(oldwd)
    if (test == F) {
      dir.create(pathname,recursive=T)
      message("\n...created directory ", pathname)

    } else message("\n... directory ", pathname, " already exists...")

  }

  write.table(x, y, quote=F, col.names=writeColnames, row.names= writeRownames, sep="\t")
}

### write so that snptest accepts the file
###
writeSnptestSamplefile <- function (filename, samplefile, vartypes) {
    stopifnot(dim(samplefile)[2] == length(vartypes))
    stopifnot(identical(as.character(vartypes[1:3]),as.character(c(0,0,0))))
    stopifnot(all(vartypes %in% c("0", "C", "D", "B", "P")))
    stopifnot(identical(names(samplefile)[1:3],c("ID1","ID2","missing")) | identical(names(samplefile)[1:3],c("ID_1","ID_2","missing") ))
    f <- file(filename, open="wb")
    write.table(t(names(samplefile)),file=f,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
    write.table(t(as.character(vartypes)),file=f,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
    write.table(samplefile,file=f,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
    close(f)
  }


### write xlsx with better defaults
WriteXLS_hk = function(x, ExcelFileName = "R.xls", SheetNames = x, AutoFilter = T, BoldHeaderRow = T, FreezeRow = 1, FreezeCol = 1, AdjWidth = F, ...){
  library(WriteXLS)
  WriteXLS( x=x, ExcelFileName = ExcelFileName, SheetNames = SheetNames, AutoFilter = AutoFilter, BoldHeaderRow = BoldHeaderRow, FreezeRow = FreezeRow, FreezeCol = FreezeCol, AdjWidth = AdjWidth,... )}



### active waiting so that RStudio server does not initiate suspend mode (usefull for huge workspaces)
wait4me = function() {
  while(T){
    message(paste0(Sys.time(), ": ...still missing you so much...hit ESC when you are back.."))
    Sys.sleep(time = 1800 )
  }
}

### system with capturing the output

system_verbose = function(...) {
  report = system(..., intern = T)
  message(paste0("\n\n----------Output of function start:\n\n",paste(report, collapse = "\n"), "\n\n----------Output of function finished...\n\n"))

}

### pdf file as graphical png in order to avoid long startup time if 100 of thousends points are included

pdf_from_png = function(myplot, pdf_filename, temp_pngfile = tempfile(), resolution = 300, weite = 7, laenge = 7, einheiten = "cm", as1file = T, ... ){
  library(png)
  png(temp_pngfile, weite, laenge, units = einheiten, res = resolution)
  plot(myplot)
  dev.off()


  pdf(pdf_filename, weite, laenge, onefile=as1file, ...)
  par(mai=c(0,0,0,0))
  plotPNG = readPNG(temp_pngfile)
  plot(c(0,1),c(0,1),type="n")
  rasterImage(plotPNG,0,0,1,1)
  dev.off()
}

### schlaue html table
dt_html <- function (df2, zeileninitial=20, maxstringlength = 25) {
  library(DT)
  library(stringr)
  if(sum(showNA(df2)) ==0){
    datatable((df2),
              class =  'cell-border stripe',
              filter = 'bottom',
              extensions = c("ColVis", "ColReorder", 'KeyTable','Scroller'),
              options = list(
                pageLength = zeileninitial,
                autoWidth = TRUE,
                dom = 'T<"clear">CRlfrtip',
                columnDefs = list(list(
                  targets = 1:ncol(df2),
                  render = JS(
                    "function(data, type, row, meta) {",
                    str_replace("return type === 'display' && data.length > maxstringlength ?","maxstringlength", maxstringlength) ,
                    str_replace("'<span title=\"' + data + '\">' + data.substr(0, maxstringlength) + '...</span>' : data;","maxstringlength", maxstringlength) ,
                    "}")
                )),
                scrollY = 1000,
                scrollCollapse = TRUE,
                tableTools = list(sSwfPath = copySWF())
              ),
              callback = JS('table.page(3).draw(false);'))
  } else {
    datatable(df2,
              class =  'cell-border stripe',
              filter = 'bottom',
              extensions = c("ColVis"),
              options = list(
                dom = 'CRlfrtip',
                columnDefs = list(list(
                  render = JS(
                    "function(data, type, row, meta) {",
                    str_replace("return type === 'display' && data.length > maxstringlength ?","maxstringlength", maxstringlength) ,
                    str_replace("'<span title=\"' + data + '\">' + data.substr(0, maxstringlength) + '...</span>' : data;","maxstringlength", maxstringlength) ,
                    "}")
                ))
              ),
              callback = JS('table.page(3).draw(false);')
    )
  }
}



fdr_matrixEQTL <- function(p, N) {
  K = length(p)
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3348564/   section 3.6 False discovery rate
  # Matrix eQTL calculates FDR only for the gene-SNP pairs that passed user-defined significance threshold. The calculations follow Benjamini and Hochberg (1995) procedure, adapted for the situation when not all p-values are recorded.

  # K
  if(any(is.na(p))) stop("NAs in p values not allowed...")
  library(data.table)
  dt = data.table(p = p)
  dt[,initialorder := 1:.N]
  setorder(dt, p)
  fdr = dt[,p*N/(1:K)]
  fdr[fdr>1]=1

  for(i in rev(seq(along=fdr))[-1]) {
    if(fdr[i]>fdr[i+1]) fdr[i] = fdr[i+1]

  }
  dt[,fdr := fdr]
  setorder(dt, initialorder)
  dt[,fdr]
}


##..................................................................................
# Finalyzing RProfile
##..................................................................................


message( "\n******************************\nSuccessfully loaded toolboxH version 0.1.0 ")

# Inspired from http://gettinggeneticsdone.blogspot.com/2013/06/customize-rprofile.html
