#####################################################################
# 
# Note:
#   Julia Liu: 2014-5-12  modified to unpack newly added variables.
#   Julia Liu: 2014-7-15  added AlphaSpecFile and ParameterVaryBy
#   Julia Liu: 2015-8-1   added more parameters
#   Julia Liu: 2015-8-4   more parameters
#####################################################################

ModelParameters = function(modsetup) {
  
  DepVar = gsub(" ","",modsetup$Value[modsetup$Parameter=="DependentVariable"])
  DepVar1 = gsub(" ","",modsetup$Value[modsetup$Parameter=="DependentVariable1"])
  ID = gsub(" ", "", modsetup$Value[modsetup$Parameter=="PhysicianID"])
  TimeVariable = gsub(" ", "", modsetup$Value[modsetup$Parameter=="TimeVariable"])
  Segment = gsub(" ", "", modsetup$Value[modsetup$Parameter=="Segment"])
  RawDataFile = modsetup$Value[modsetup$Parameter=="RawDataFile"]
  ModelDataFile = modsetup$Value[modsetup$Parameter=="ModelDataFile"] 
  NationalDataFile = modsetup$Value[modsetup$Parameter=="NationalDataFile"]
  PhysicianDBFile = modsetup$Value[modsetup$Parameter=="PhysicianDBFile"]
  ModelDrsFile = modsetup$Value[modsetup$Parameter=="ModelDrsFile"]
  ModelVariablesFile = modsetup$Value[modsetup$Parameter=="ModelVariablesFile"]
  AlphaSpecFile = modsetup$Value[modsetup$Parameter == "AlphaSpecFile"]
  ParameterVaryBy = modsetup$Value[modsetup$Parameter == "ParameterVaryBy"]

  MinimumNrx = as.numeric(modsetup$Value[modsetup$Parameter=="MinimumNrx"])
  NumDrs = as.numeric(modsetup$Value[modsetup$Parameter=="NumDrs"])

# HB MCMC related
  MCMC_MaxIter = as.numeric(modsetup$Value[modsetup$Parameter=="MCMC_MaxIteration"])
  MCMC_Burnin = as.numeric(modsetup$Value[modsetup$Parameter=="MCMC_Burnin"])

# simulation related
  SimVariablesFile = modsetup$Value[modsetup$Parameter=="SimVariablesFile"]
  SimPhyDataFile =   modsetup$Value[modsetup$Parameter=="SimPhysicianDataFile"]
  SimNatDataFile = modsetup$Value[modsetup$Parameter=="SimNationalDataFile"]
  SimStart = as.numeric(modsetup$Value[modsetup$Parameter=="SimStartPeriod"])
  SimEnd = as.numeric(modsetup$Value[modsetup$Parameter=="SimEndPeriod"])
  SimDeltaLow = as.numeric(modsetup$Value[modsetup$Parameter=="SimDeltaLow"])
  SimDeltaHigh = as.numeric(modsetup$Value[modsetup$Parameter=="SimDeltaHigh"])
  SimStep = as.numeric(modsetup$Value[modsetup$Parameter=="SimStep"])

#
#  PhysicianDBFile2 = modsetup$Value[modsetup$Parameter=="PhysicianDBFile2"]
#  GLAB = modsetup$Value[modsetup$Parameter=="GLAB"]

  CSID = gsub(" ", "", modsetup$Value[modsetup$Parameter=="PanelID"])
  CSNAME = gsub(" ", "", modsetup$Value[modsetup$Parameter=="PanelName"])
  ProductID = gsub(" ", "", modsetup$Value[modsetup$Parameter=="ProductID"])
  ProductName = gsub(" ", "", modsetup$Value[modsetup$Parameter=="ProductName"])
  LkupFile= gsub(" ", "", modsetup$Value[modsetup$Parameter=="LkupFile"])

  ModParameters = list(DepVar=DepVar, DepVar1 = DepVar1, ID = ID, TimeVariable=TimeVariable, Segment=Segment, 
                       RawDataFile=RawDataFile, ModelDataFile=ModelDataFile, PhysicianDBFile=PhysicianDBFile,
                       ModelDrsFile = ModelDrsFile, ModelVariablesFile = ModelVariablesFile, 
                       AlphaSpecFile = AlphaSpecFile, ParameterVaryBy=ParameterVaryBy, MinimumNrx=MinimumNrx,
                       NationalDataFile=NationalDataFile, NumDrs = NumDrs, MCMC_MaxIter = MCMC_MaxIter, MCMC_Burnin=MCMC_Burnin,
                       SimVariablesFile = SimVariablesFile, SimPhyDataFile = SimPhyDataFile, SimNatDataFile =SimNatDataFile,
                       SimStart=SimStart, SimEnd=SimEnd,SimDeltaLow = SimDeltaLow,SimDeltaHigh=SimDeltaHigh, SimStep=SimStep,
                       csid = CSID,csname=CSNAME, pid = ProductID, pname=ProductName, lkupFile=LkupFile)

  return(ModParameters)
}

