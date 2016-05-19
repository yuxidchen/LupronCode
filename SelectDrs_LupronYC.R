# tailored for Lupron -- Yuxi Chen -- 5/19/16 -- While watching Kobe Bryant's last game!

#
# Modified from Julia Liu's SelectDrs.R
# For the original header documentation and revision history, please refer to SelectDrs.R
# 
# Purpose: select physicians for physician-level model.
#          The selection criteria:
#           1) the physician has to be targt at least once by at least one physician level tactics;
#           2) the physician wrote at least MinNrx Rx (specified in ModelControl.csv)
#           3) absapproved == "Y" (this year)
# To call:
#           SelectDr <- SelectDrsYC(ProjectName)
#           ProjectName: name of the model sub-directory
# Input (defined within the function):
#           Raw data file (physician level model data)
#           ProjectName_PhysicianDB:  list of all the physicians with attributes
#           ProjectName_ModelVariables: model specification file      
# Output:
#           ProjectName_ModelDrs.csv: It contains only the physicians who satisfy the selection criteria.
#                                     It does not support the ModGrp output as in SelectDrs.R, because doctors in different 
#                                     deciles tend to have different likelyhood functions, and therefore
#                                     randomly mix them will lead to unstable results.
# Revision hisotry
#         
#         3/14/2016 : Added keyword ignoreVar to remove certain marketing strategy when selecting doctors; these strategies were
#                     mareked as physician level, needed to be included in the physician level, but are not targeted toward each physician;
#                     e.g. because the stratSpend was matched down from higher level (territory/DMA), remove this when selecting doctors
#         3/5/2016 : created by Yuxi Chen


library(data.table)
source("Functions/ModelParameters.R")
# source("Functions/AssignModGrpYC.R") #do not use this
source("Functions/AssignModGrp.R")

SelectDrsYC = function(ProjectName, RawDataFile) {

# file names
ModFolderPath = paste("../", ProjectName, sep="")
ModControlFile = paste(ModFolderPath, "/", ProjectName, "_ModelControl.csv", sep="")
if(file.exists(ModControlFile)) {
  modctrl = read.csv(ModControlFile, header=T, stringsAsFactor=F)
} else {
  stop(ModControlFile, "does not exist. RunPhysicianModel terminates. \n")
}

modpar = ModelParameters(modctrl)
DepVar = modpar$DepVar
ID = modpar$ID
TimeVar = modpar$TimeVariable
Segment = modpar$Segment
if (length(RawDataFile) == 0) RawDataFile = paste(ModFolderPath, "/", ProjectName, "_", modpar$RawDataFile, sep="")
PhysicianDBFile = paste(ModFolderPath, "/", ProjectName, "_", modpar$PhysicianDBFile, sep="")
ModelDrsFile = paste(ModFolderPath, "/", ProjectName, "_", modpar$ModelDrsFile, sep="")
ModelVariablesFile = paste(ModFolderPath, "/", ProjectName, "_", modpar$ModelVariablesFile, sep="")
MinNrx = modpar$MinimumNrx
NumDrs = modpar$NumDrs

if(file.exists(RawDataFile)) {
  x = read.csv(RawDataFile, header=T, stringsAsFactor=F)
} else {
  stop(RawDataFile, "does not exist. SelectDrs terminates. \n")
}
if(file.exists(PhysicianDBFile)) {
  doc = read.csv(PhysicianDBFile, header=T, stringsAsFactor=F)
} else {
  stop(PhysicianDBFile, "does not exist. SelectDrs terminates. \n")
}
if(file.exists(ModelVariablesFile)) {
  spec = read.csv(ModelVariablesFile, header=T, stringsAsFactor=F)
} else {
  stop(ModelVariablesFile, "does not exist. RunPhysicianModel terminates. \n")
}

# filter out the approved physicians (for Abbvie)
if( "absapproved" %in% names(doc)) {
  doc = doc[toupper(substring(doc$absapproved, 1, 1)) == "Y",]
}
x = x[x[[ID]] %in% doc[[ID]],]


var_phy = spec$OrigVarName[toupper(spec$Level) == "PHYSICIAN" & toupper(spec$VariableType) == "MARKETING"]
var_nat = spec$OrigVarName[toupper(spec$Level) == "NATIONAL" & toupper(spec$VariableType) == "MARKETING"]

# # remove those market strategies that are marked as physician, but not on the physician level (e.g. stratSpend)
# cat("var before dropping:",var_phy,"\n")
# if (length(ignoreVar) != 0) var_phy <- var_phy[!(var_phy %in% ignoreVar)]
# cat("var after dropping:",var_phy,"\n")

y = data.table(x[,c(ID, DepVar, var_phy)])

sd_id <- as.data.frame(y[,lapply(.SD, sd), by=ID])
sum_id <- as.data.frame(y[,lapply(.SD, sum), by=ID])
sum_id <- sum_id[,c(ID, DepVar)]

# sd_id contains the DV and IV activities on the physician level during the model period
# we use it to filter out physicians who has zero scripts and/or has not been targeted at the pyscian level 
sd_id$sum = 0
for (i in 1:length(var_phy)) {
  sd_id$sum = sd_id$sum + sd_id[[var_phy[i]]]
}

# filter out physicians who has 0 nrx during the model period
grp_nrx0 = sd_id[[ID]][sd_id[[DepVar]]==0]
grp_nrx  = sd_id[[ID]][sd_id[[DepVar]]!=0]
grp_M0 = sd_id[[ID]][sd_id$sum ==0 ]
grp_M = sd_id[[ID]][sd_id$sum !=0 ]
grp_3 = sum_id[[ID]][sum_id[[DepVar]] >= MinNrx]   # drs with at least MinNrx scripts written during the model period
grp_low = sum_id[[ID]][sum_id[[DepVar]] >0 & sum_id[[DepVar]] < MinNrx]

# create physician table that summarize the group
group = matrix(nrow=4, ncol=4)
group[1,1] = length(grp_nrx0)
group[2,2] = length(grp_nrx)
group[3,3] = length(grp_M0)
group[4,4] = length(grp_M)
group[1,2] = group[2,1] = length(intersect(grp_nrx0, grp_nrx))
group[1,3] = group[3,1] = length(intersect(grp_nrx0, grp_M0))
group[1,4] = group[4,1] = length(intersect(grp_nrx0, grp_M))
group[2,3] = group[3,2] = length(intersect(grp_nrx, grp_M0))
group[2,4] = group[4,2] = length(intersect(grp_nrx, grp_M))
group[3,4] = group[4,3] = length(intersect(grp_M0, grp_M))
group = data.frame(group)
names(group) = c("nrx0", "nrx1+", "M0", "M1+")
row.names(group) = c("nrx0", "nrx1+", "M0", "M1+")
cat("===========================\n")
cat("physician activity table: \n")
cat("---------------------------\n")
print(group)
cat("===========================\n")

# select physicians who has nrx and one of the physician level driver
grp_nrx3 = intersect(grp_nrx, grp_3)   # drs with at least MinNrx scripts and have nrx variation during the model period
grp_mod = intersect(grp_M, grp_nrx3)   
if( "absapproved" %in% names(doc)) {
  grp_yes = doc[[ID]]  [toupper(substring(doc$absapproved, 1, 1)) == "Y"]
  mod_doc = sd_id[sd_id[[ID]] %in% intersect(grp_mod, grp_yes),]   # the intersect of these are in the physician-level model and absapproved

} else {
  mod_doc = sd_id[sd_id[[ID]] %in% grp_mod,]   # the intersect of these are in the physician-level model
}

# define the phyician model group based on the which tactics were on
for (i in 1:nrow(mod_doc) ) {

  mod_doc[i,var_phy] = ifelse(mod_doc[i,var_phy] !=0, 1, 0)

  a = paste(mod_doc[i,var_phy], sep="", collapse="")
  mod_doc$group[i] = paste("V", a, sep="")

  if (i%%500 == 0) {        # print out progress every 500th unit
    cat("completed", i, "physicians. \n")
  }
}

mod_doc[[DepVar]] = NULL
mod_doc = merge(sum_id, mod_doc, by=ID, all.x=F, all.y=T)
mod_doc = mod_doc[,c(ID, DepVar, "group")]
mod_doc = merge(mod_doc, doc, by=ID, all.x=T, all.y=F)

if( "specialty" %in% names(mod_doc)) {
  cat("==========================================================\n")
  cat("There are ", nrow(mod_doc), "physicians in physician-level model. \n")
  cat("----------------------------------------------------------\n")
  cat("The number of physicians by specialty: ")
  print(table(mod_doc$specialty))
  cat("----------------------------------------------------------\n")
}

if ( Segment %in% names(mod_doc)) {
  cat("The number of physicians by doctor segment(", Segment, "):\n")
  print(table(mod_doc[[Segment]]))
  cat("==========================================================\n")
} else {
  cat("PhysicianDB file does not contain segment(", Segment, ")\n")
}

mod_doc = AssignModGrp(ID=ID, df=mod_doc, size=NumDrs)
write.csv(mod_doc, file=ModelDrsFile, row.names=F, quote=F)

return(mod_doc)
}  # end of SelectDrs



