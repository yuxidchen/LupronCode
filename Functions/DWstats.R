DWstat <- function(residuals) {

   LagResiduals = MyLag(residuals, 1)
#   DWData$numerator <- ifelse(DWData$Index>1 & !is.na(DWData$Residuals),(DWData$Residuals - DWData$LagResiduals)^2,0)
   numerator = (residuals - LagResiduals)^2
   result = sum(numerator)/sum(residuals^2)
#   return(as.numeric(sum(DWData$numerator)/sum(res^2)))
   return(result)
}

