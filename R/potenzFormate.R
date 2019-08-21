### im Text als potenz schreiben, so dass es von knitr umgewandelt werden kann
potenzFormate = function(x, showdigits=1) {paste0(stringr::str_replace(formatC(x, digits=showdigits, format = "e"), "e", "x10^"),"^")}
