###################################################
# This function calculates t-stat and P-value given
# m : coefficient estimate
# e : coefficient standard error
# nobs : number of observations
###################################################
tStatPValue = function(m, e, nobs) {

  m = as.vector(m)
  e = as.vector(e)

  tStat = as.vector(m)/as.vector(e)
  P = sapply(abs(tStat), function(x) 2*(1-pt(x,df=nobs-length(m)+1)))
  return(list(tStat, P))

}

