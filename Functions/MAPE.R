
MAPE = function(a, f) {
#  x = ifelse( a==0, 0, abs(a-f)/a)
#  m = sum(x)/length(a)
  return( sum(abs(a-f)/a)/length(a) )
}

