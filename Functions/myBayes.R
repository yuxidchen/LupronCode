#########################################################################
# Bayesian Linear Regression
#
# Data : list that contains data - y (dependent), X (regressors)
# Prior : list that contains prior - A (precision matrix), 
#                                    sig2 (model sigma sqared)
#                                    betabar (the prior mean of the betas)
# It returns a list that contains the estimate (btilde) and error (error).
# Notes :
#   Julia Liu 2015/04/21: added "tol = 1e-19" in solve() to
#    to make the function more tolerant of linear dependency
##########################################################################

myBayes = function(Data, Prior)
{
	y = Data$y
	X = Data$X
	A = Prior$A
	sig2 = Prior$sig2
	betabar = Prior$betabar

#  cat("A=", A, "\n")
#  cat("sig2=", sig2, "\n")
#  cat("betabar=", betabar, "\n")

	ivar = ncol(X)
	U=chol(A)         # cholesky root of precision matrix
	W=rbind(X,U)      # combine the U matrix with design matrix
	nu = c(y, (U %*% betabar))
	WpW = t(W) %*% W
	IWpW = solve(WpW, tol = 1e-19)
	btilde = IWpW %*% t(W) %*% nu    # basicaly OLS 
	var.cov <- sig2*solve(t(X) %*% X +A, tol = 1e-19)   # this is the error
	return(list(btilde=btilde, error=sqrt(diag(var.cov))))
}

