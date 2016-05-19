#############################################################################
#  This function does adstock transformation
#  Inputs: 
#    x - a vector. Contains values that need to be adstock transformed
#    r - decay/carry-over rate
#    MaxPeriod - max number of time units included in the adstock transformation
#    peak - has to be less or equal to MaxPeriod.    
#############################################################################
Adstock <- function(x, peak, r, MaxPeriod){
  N <- length(x)
  norm <- rep(0, N)
  if(peak > MaxPeriod) {
    cat("The adstock peak =", peak, ". It should be less than ", MaxPeriod, "\n")
    stop("The program stopped because the adstock peak is more than MaxPeriod.")
  }
  adstk <- x/(peak+1)
  norm <- norm + 1/(peak+1)
  nperiod <- min(MaxPeriod, N)	# in case the nobs is less than MaxPeriod
  for (i in 1:nperiod) {
     if(i <= peak) {
        adstk <- adstk + (i+1)/(peak+1)*MyLag(x,i)
        norm[(i+1):N] <- norm[(i+1):N] + (i+1)/(peak+1)
     } else {
       adstk <- adstk + r^(i-peak)*MyLag(x,i)
       norm[(i+1):N] <- norm[(i+1):N] + r^(i-peak)
     }
  }
  adstk <- adstk/norm
  return(adstk)  
}
