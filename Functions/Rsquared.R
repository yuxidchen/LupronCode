##########################################
# this function calculates R-squared given 
# a : actual
# b : back-caste
##########################################
Rsquared = function(a, b) {
  if(length(a) != length(b)) stop("The length of vectors of actual and predicted are not the same.")
  a = as.vector(a)
  b = as.vector(b)
  residual = a-b
  sse <- sum(residual^2)
  sst <- sum((a-mean(a))^2)
  R2 <- as.numeric(1 - sse/sst)

  return(R2)
  
}

