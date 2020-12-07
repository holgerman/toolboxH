require(toolboxH)

todofile = data.table(fn = paste0("plot", 1:11, ".pdf"),
                      plotvar = 100:110, 
                      num  = 1:11)

todofile %>% data.frame
stopifnot(sum(duplicated(todofile$num))==0)

# 1 einzelpdfs plotten

for(mynum in todofile$num) {
  # mynum =todofile$num[1]
  
  myrow = todofile[num == mynum]
  message('plotting ', myrow$fn)
  pdf_from_png(code2parseOrPlot = paste0("plot(", myrow$plotvar, ")"), 
               pdf_filename = myrow$fn,
               weite = 6,
               laenge = 8,
               einheiten = "in",
               resolution = 150)
  
  
}


pdf_name_combined = "plots-combined.pdf"
mycall.used = paste0('gs -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE=',pdf_name_combined,' -dBATCH ', paste(todofile$fn, collapse = " "))
mycall.used

system(mycall.used)
