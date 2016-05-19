##########################################################
# Simuation : this function does physician-level (NB model) simulation given:
#         1) x : data set in data frame;
#         2) b : coefficients in data frame;
#         3) delta : simulation scenarios;
#         4) SimuVar : the variables to be simulated;
#         5) spec : model variables transformation specification
#         6) IV : independent variables.
#         7) DepVar : dependent varialbes
#         8) ID : physician ID
#
# The function returns simulation results in a data frame.
# Notes :
#   Yuxi Chen 2016/03/20: add the choice of whether or not adstock
#   Julia Liu 2014/07/23: made compatible with segment varied Alpha
##########################################################

# Simulation = function(ID, TimeVar, DepVar, x, b, spec, IV, SimuVar, delta, deltaname) {
SimulationYC = function(ID, TimeVar, DepVar, x, b, spec, IV, SimuVar, delta, deltaname) {

xsplit = split(x, x[[ID]])
SimuResult = x[,c(ID, TimeVar, DepVar)]
SimuResult = split(SimuResult, SimuResult[[ID]])
N = length(xsplit)
itime = proc.time()[3]
for (i in 1:length(xsplit) ) {    # loop through units/physicians
  beta = b[b[[ID]] == xsplit[[i]][[ID]][1], IV]
  tmp = xsplit[[i]][, IV]

  X = as.matrix(tmp)
  beta = as.numeric(beta)

  SimuResult[[i]]$yhat = as.vector(exp(X %*% beta))
  X[,2:(ncol(X)-1)] = 0
  SimuResult[[i]]$OffAllTP = as.vector(exp(X %*% beta))

  for ( j in 1:length(SimuVar)  ) {    # loop through simulated variables
    transpec = spec[spec$TransformedVarName == SimuVar[j],]

    for (k in 1:length(delta)) {       # loop through scenarios
      tmp = xsplit[[i]]
      tmp[[transpec$OrigVarName]] = tmp[[transpec$OrigVarName]]*(1+delta[k])      # over-write the raw spending based on scenario

  if (transpec$Type == "Adstock"){  # Add the choice to whether or not Adstock	
# do the transformations
      a = Adstock(x=tmp[[transpec$OrigVarName]], peak=transpec$peak , r=transpec$r, MaxPeriod=transpec$MaxPeriod)
# 2014/07/23  
#      tmp[[transpec$TransformedVarName]] = myPoly(a, transpec$Alpha)/transpec$Scalor
      v = paste(transpec$OrigVarName, "_Alpha", sep="")
      tmp[[transpec$TransformedVarName]] = myPoly(a, tmp[[v]][1])/transpec$Scalor
  } else if (transpec$Type == "None") {
	v <- paste(transpec$OrigVarName, "_Alpha", sep="")
	tmp[[transpec$TransformedVarName]] <- tmp[[transpec$OrigVarName]]
  }

# create the design matrix and predict
      X = as.matrix(tmp[, IV])
      SimuResult[[i]][[paste(transpec$TransformedVarName, deltaname[k], sep="") ]] = as.vector(exp(X %*% beta))
    }

  }

  if (i%%200 == 0) {        # print out progress every 200th unit
    cat("completed", i, "physicians. \n")
    ctime = proc.time()[3]
    timetoend = ((ctime - itime)/i) * (N - i)
    cat(" ", i, " (", round(timetoend/60, 1), ")", fill = TRUE)
    fsh()

  }
}

y = data.frame(rbindlist(SimuResult))    # combine the list into a whole data frame

return(y)
}


