# DataPrepWrapper

ProjectName <- "Lupron201601"
# source("./MapSPECIALTY2SPECGRP.R") # map the physician specialty to SPECGRP
source("./GenPhyDB_Lupron.R")            # generate the physician database
source("./SelectDrs_LupronYC.R")
# moddoc <- SelectDrsYC(ProjectName, "../Creon201601/moddata_Creon_Phys_20160303.csv")
# moddoc <- SelectDrsYC(ProjectName, "../Creon201601/moddata_Creon_Phys_20160313.csv", ignoreVar = "stratSpend")
moddoc <- SelectDrsYC(ProjectName, "../Lupron201601/ModelData_Lupron_Physician_20160511.csv")
source("./GenModelSet_LupronYC.R")
# genmod <- GenModelSetYC(ProjectName, "../Creon201601/moddata_Creon_Phys_20160303.csv")
# genmod <- GenModelSetYC(ProjectName, "../Synthroid201601/ModelData_Synthroid_Physician_20160401.csv")
# genmod <- GenModelSet_SynthroidYC(ProjectName, "../Synthroid201601/ModelData_Synthroid_Physician_20160401.csv",sepEmail=T)
genmod <- GenModelSet_LupronYC(ProjectName, "../Lupron201601/ModelData_Lupron_Physician_20160511.csv")
# cut the model data file into chunks according to the physcian's decile, for later parallel computing
# source("./MakeDecileModelSet.R")