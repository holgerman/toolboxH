round2 = function(x, n) {
  # https://stackoverflow.com/questions/12688717/round-up-from-5
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}
