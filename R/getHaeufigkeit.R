


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
