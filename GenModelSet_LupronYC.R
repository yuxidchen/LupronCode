###############################################################################
# GenModelSet:Generates physician-level model set. 
#             It transforms physician-level variables based on specifications in ModelVariables.csv
#
# ProjectName : name of the model sub-directory 
#
# Inputs:  Raw data file: raw data file
#          ModelVariables.csv: model specification file
#          ModelDrs.csv: The output file from SelectDrs.R. It is physician information
#                        data for ONLY the physicians who are chosen by SelectDrs for 
#                        physician-level model.
# Outputs: ModelSet.csv
# It returns the physician-level model data set in data frame 
# Notes:
# Yuxi Chen 2016/04/23 : For Synthroid, Separate some variable into two b/w the two years
#                           now hard-coded only emeail (pnpActiv)
# Yuxi Chen 2016/03/09 : Added keyword RawDataFile
# Julia Liu 2014/07/11 : add Alpha_spec (Alpha parameter by physician segment)
# Julia Liu 2014/07/22 : improve speed, made it backward compatible
# Julia Liu 2014/07/27 : make it be able to read the output from AlphaSearch()
# Julia Liu 2014/09/26 : fixed a big : check alpha_spec file
# Julia Liu 2014/10/16 : changed spec = spec[spec$Level != "National",] to 
#                                spec = spec[spec$Level == "Physician",]
###############################################################################


library(data.table)
source("Functions/ModelParameters.R")
source("Functions/MyLag.R")
source("Functions/Adstock.R")
source("Functions/myPoly.R")
source("Functions/GenAlphaSpec.R")

# GenModelSetYC = function(ProjectName, RawDataFile) {
GenModelSet_LupronYC = function(ProjectName, RawDataFile) {
    
# file names
ModFolderPath = paste("../", ProjectName, sep="")
ModControlFile = paste(ModFolderPath, "/", ProjectName, "_ModelControl.csv", sep="")

modctrl = read.csv(ModControlFile, header=T, stringsAsFactor=F)

modpar = ModelParameters(modctrl)
DepVar = modpar$DepVar
ID = modpar$ID
TimeVar = modpar$TimeVariable
Segment = modpar$Segment
if (length(RawDataFile) == 0) RawDataFile = paste(ModFolderPath, "/", ProjectName, "_", modpar$RawDataFile, sep="")
ModelDrsFile = paste(ModFolderPath, "/", ProjectName, "_", modpar$ModelDrsFile, sep="")
ModelVariablesFile = paste(ModFolderPath, "/", ProjectName, "_", modpar$ModelVariablesFile, sep="")
AlphaSpecFile = paste(ModFolderPath, "/", ProjectName, "_", modpar$AlphaSpecFile, sep="")
ParameterVaryBy = modpar$ParameterVaryBy
MinNrx = modpar$MinimumNrx
ModelDataFile = paste(ModFolderPath, "/", ProjectName, "_", modpar$ModelDataFile, sep="")

# read the input files
x = read.csv(RawDataFile, header=T, stringsAsFactor=F)
# rename some of the variables
names(x)[names(x)=="APPROVED"] <- "absapproved"
names(x)[names(x)=="SPECIALTY"] <- "specialty"


doc = read.csv(ModelDrsFile, header=T, stringsAsFactor=F)
spec = read.csv(ModelVariablesFile, header=T, stringsAsFactor=F)
AlphaFileExists = file.exists(AlphaSpecFile)
if(AlphaFileExists) {
  alpha_spec = read.csv(AlphaSpecFile, header=T, stringsAsFactor=F)
  alpha_spec = alpha_spec[alpha_spec$VaryBy != "National",]
  alpha_spec2 = GenAlphaSpec(alpha_spec)
}

# select only the modeled physicians and sort
x = x[x[[ID]] %in% doc[[ID]],]
if (AlphaFileExists) {
  doc = doc[,c(ID, Segment, ParameterVaryBy)]
  x = merge(doc, x, by=ID, all.x=F, all.y=T)
  x = merge(x, alpha_spec2, by.x=ParameterVaryBy, by.y="VaryBy")
}

x = x[order(x[[ID]], x[[TimeVar]]),]



# transform physician level variables based on the specifications in the ModelVariables file
spec = spec[spec$Level == "Physician",]

splitX = split(x, x[[ID]])
for (k in 1:nrow(spec)) {

  if (spec$Transform[k] == "Y") {
  if (spec$OrigVarName[k] %in% names(x)) {
    cat("Transforming ", spec$OrigVarName[k], "...\n")
    if(spec$Type[k] == "Adstock") {
      for (i in 1:length(splitX)) {
        tmp = Adstock(x=splitX[[i]][[spec$OrigVarName[k] ]], peak=spec$peak[k], r=spec$r[k], MaxPeriod=spec$MaxPeriod[k])
        if(AlphaFileExists) {
          v = paste(spec$OrigVarName[k], "_Alpha", sep="")
          splitX[[i]][[spec$TransformedVarName[k]]] <- myPoly(tmp, splitX[[i]][[v]][1])/spec$Scalor[k]
        } else {
          splitX[[i]][[spec$TransformedVarName[k]]] <- myPoly(tmp, spec$Alpha[k])/spec$Scalor[k]
        }
      }
    } else if(spec$Type[k]== "Log") {
      for (i in 1:length(splitX)) {
        splitX[[i]][[spec$TransformedVarName[k]]] <- log(splitX[[i]][[spec$OrigVarName[k] ]])
      }
    } else if(spec$Type[k]== "Scale") {
      for (i in 1:length(splitX)) {
        splitX[[i]][[spec$TransformedVarName[k]]] <- splitX[[i]][[spec$OrigVarName[k] ]]/spec$Scalor
      }

    } else if(spec$Type[k] == "None") {
      for (i in 1:length(splitX)) {
        splitX[[i]][[spec$TransformedVarName[k]]] <- splitX[[i]][[spec$OrigVarName[k] ]]
      }      
    } else {
      stop("Invalid transformation type", spec$Type[k])
    }
  } else {
    stop("Variable ", spec$OrigVarName[k], "is not in raw dataset. GenModSet terminates.\n")
  }
  }

}    # for (k in 1:nrow(spec)) 




y = data.frame(rbindlist(splitX))
if(AlphaFileExists) {
v_alpha = unique(alpha_spec$OrigVarName)
for (i in 1:length(v_alpha)){
  y[[paste(v_alpha[i], "_Alpha", sep="")]] = NULL
}
}

# ######## added by YC 4/23/16 to separate emails      ####
# # The separation should be done AFTER transformation ####
# 
# if (sepEmail == T){
#   timeSep <- 22
#   y$pnpActiv14 <- rep(0,nrow(y))
#   y$pnpActiv14[y[[TimeVar]] < timeSep] <- y$pnpActiv[y[[TimeVar]] < timeSep]
#   y$pnpActiv15 <- rep(0,nrow(y))
#   y$pnpActiv15[y[[TimeVar]] >= timeSep] <- y$pnpActiv[y[[TimeVar]] >= timeSep]
#   y$Email14 <- rep(0,nrow(y))
#   y$Email14[y[[TimeVar]] < timeSep] <- y$Email[y[[TimeVar]] < timeSep]
#   y$Email15 <- rep(0,nrow(y))
#   y$Email15[y[[TimeVar]] >= timeSep] <- y$Email[y[[TimeVar]] >= timeSep]
#   
# }



write.csv(y, file=ModelDataFile, row.names=F, quote=F)

print("GenModelSet() done.")
return(y)
}



