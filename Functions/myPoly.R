myPoly = function(x, alpha) {
#  for (i in 1:length(x)) {
#    if (x[i] > 1/(2*alpha) )  x[i] = 1/(2*alpha)
#  }
  x = ifelse(x <= 1/(2*alpha), x, 1/(2*alpha) )
  return(x - alpha*x^2)

}

     