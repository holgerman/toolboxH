### wrapper, um ein RMD file von commandline aus zu starten
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param myfilename PARAM_DESCRIPTION, Default: filename
#' @param mypathwd PARAM_DESCRIPTION, Default: pathwd
#' @param computer PARAM_DESCRIPTION, Default: 'amanMRO'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname knitrHelper
#' @export 
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

  message('.libPaths("/net/ifs1/san_projekte/projekte/genstat/07_programme/rpackages/',computer,'")')
  message('require(knitr)')
  message('sessionInfo()')
  message(paste0("knit('",myfilename, ".Rmd')"))


  message("\nAfter this, you may have go tho this directory")
  message("and change file format of output using pandoc: \n")
  message(paste0("cd  ",mypathwd, ""))

  if(computer %in% c("forostar", "amanMRO", "dunhargRRO")) {
    pandoc_call = paste0("/usr/lib/rstudio-server/bin/pandoc/pandoc ",myfilename,".md --to html --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash-implicit_figures --output ",myfilename,".html --smart --email-obfuscation none --self-contained --standalone --section-divs --table-of-contents --toc-depth 3 --template /net/ifs1/san_projekte/projekte/genstat/07_programme/rpackages/",computer,"/rmarkdown/rmd/h/default.html --number-sections --css custom.css --variable 'theme:bootstrap' --mathjax --variable 'mathjax-url:https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --no-highlight --variable highlightjs=/net/ifs1/san_projekte/projekte/genstat/07_programme/rpackages/",computer,"/rmarkdown/rmd/h/highlight")
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
