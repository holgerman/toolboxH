
### uhrzeit inkl. einheit
formateTimediff = function(timediff, mydigits = 3) paste0(format(unclass(timediff), digits = mydigits), " ", attr(timediff, "units"))
