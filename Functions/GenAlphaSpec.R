GenAlphaSpec <- function(alpha_spec)
{
  alpha_spec2 = data.frame(unique(alpha_spec$VaryBy))
  names(alpha_spec2) = c("VaryBy")
  v_alpha = unique(alpha_spec$OrigVarName)
  for (i in 1:length(v_alpha)){
    alpha_spec2[[paste(v_alpha[i], "_Alpha", sep="")]] = alpha_spec$Alpha[alpha_spec$OrigVarName == v_alpha[i]]
  }
  return(alpha_spec2)
}

