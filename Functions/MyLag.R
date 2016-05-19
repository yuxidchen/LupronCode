MyLag <- function(x,n) {
   l <- length(x)
   if(!floor(n) == n) {
      cat("The shifting parameter is not an integer. It is set to ", round(n), "\n")
      n <- round(n)
   } else if(n < 1 ) {
      cat("The shifting parameter is less than 1. It is set to 1. \n")
      n <- 1
   } else if(n > l-1) {
     cat("The shifting parameter is more than the length. \n")
     n <- l-1
   }
   return(c(rep(0, n), x[1:(l-n)]))
}

