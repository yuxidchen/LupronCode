# To generate a physician database, by adding the correspondong SPECGRP in the database
# ignore below -- as of 4/13, no Decile info available
##################### ignore comments below ##################################
# Input:
#       datadict_creon_20160330b.csv 
#       CREON Physcian Universe_Q3'13.csv
#       Creon201601_PhysSpec2bucketTbl.csv
# Output:
#       ProjectName_PhysicianDBb.csv
#           rename some of the columns, and add corresponding SPECGRP for each physician
# The matching algorithm is as follows:
#       1. if the IMSNUM from physician file matches the IMS_ID in the phys universe, 
#           AND the specialty matches that in the phys universe, use the matched SPECGRP from the phys universe.
#       2. if there's no match of IMS_ID for IMSNUM, leave the SPECGRP as "unknown" first.
#       3. if there is a match of IMS_ID for IMSNUM, but the specialty of the two files dont match, leave the SPECGRP
#           as "reclassified", because the physician's specialty has been reclassified since 2013.
#       4. for those "unknown" and "reclassified" inputs, AND if there's matching entry in the speialty to SPECGRP 
#           lookup table (generated from the 2013 phys universe) to perform a match.
#       5. the rest of the physicians have new specialty which is not in the physician universe. set all as unknown.
################### end of ignored comments ####################################

ProjectName <- "Lupron201601"
# PhysFile <- paste("../",ProjectName,"/datadict_creon_20160303b.csv",sep="")
# PhysFile <- paste("../",ProjectName,"/datadict_creon_20160313b.csv",sep="")
PhysFile <- paste("../",ProjectName,"/Lupron201601_PhysicianList.csv",sep="")
# as of 4/13/16, no physician universe file
# PhysUniv <- paste("../",ProjectName,"/CREON Physcian Universe_Q3'13.csv",sep="") # physician universe
# PhysMappingFile <- paste("../",ProjectName,"/",ProjectName,"_PhysSpec2SPECGRP.csv",sep="")  # Mapping specialty to specgrp
PhysDBFile <- paste("../",ProjectName,"/",ProjectName,"_PhysicianDB.csv",sep="")
# as of 4/13/2016, no physician buckets file
# PhysSpec2bucketFile <- paste("../",ProjectName,"/",ProjectName,"_PhysSpec2bucketTbl.csv",sep="")
# look up table; only used in case the physician's specialty has been reclassified since the 2013 phys univ

phys <- read.csv(PhysFile,header=T,stringsAsFactors = F)
# PhysUniv <- read.csv(PhysUniv,header=T,stringsAsFactors = F)
# PhysSpec2bucket <- read.csv(PhysSpec2bucketFile,header=T,stringsAsFactors = F)
# rename columns
varnames <- names(phys)
# varnames[which(varnames == "mpzAPPROVED")] <- "absapproved"
varnames[which(varnames == "SPECIALTY")] <- "specialty"
varnames[which(varnames == "APPROVED")] <- "absapproved"
# varnames[which(varnames == "PERT_RX_MATTY_DEC15")] <- "PERT_RX"
# varnames[which(varnames == "PERT_DECILE_MATTY_DEC15")] <- "PERT_DECILE"
# varnames[which(varnames == "CREON_RX_MATTY_DEC15")] <- "CREON_RX"
# varnames[which(varnames == "CREON_DECILE_MATTY_DEC15")] <- "CREON_DECILE"
names(phys) <- varnames

############ as of 4/13/16, no specialty/decile info available #####################
# # add mapped specgrp to each specialty, according to the IMSNUM/IMS_ID
# # load the look up table between specialty and SPECGRP
# # the unmatched is set to a group of "unknown"
# 
# common_imsnum <- intersect(PhysUniv$IMS_ID,phys$IMSNUM)
# unique_imsnum <- setdiff(phys$IMSNUM, PhysUniv$IMS_ID)
# 
# phys$SPECGRP <- rep("unknown",nrow(phys))
# phys$SPECGRP[phys$IMSNUM %in% common_imsnum] <- PhysUniv$SPECGRP[PhysUniv$IMS_ID %in% common_imsnum]
# 
# # save the specialty from phys univ for later check and match
# phys$SpecialtyFromPhysUniv <- rep("NA",nrow(phys))
# phys$SpecialtyFromPhysUniv[phys$IMSNUM %in% common_imsnum] <- PhysUniv$SPECIALTY[PhysUniv$IMS_ID %in% common_imsnum]
# 
# # for those specialty from physician db do not match those from the phys univ, AND have common_imsnum (reclassified)
# # set the SPECGRP to "reclassified", and then use the lookup table to make a new match, if possible
# phys$SPECGRP[phys$specialty != phys$SpecialtyFromPhysUniv & phys$IMSNUM %in% common_imsnum] <- "reclassified"
# # for "reclassified" and "unknown" SPECGRP, use the updated table to put them in the buckets, if possible
# ind <- which(phys$SPECGRP == "reclassified" | phys$SPECGRP == "unknown" & phys$specialty %in% PhysSpec2bucket$specialty)
# for (i in ind){
#   spec <- phys$specialty[i]
#   if (spec %in% PhysSpec2bucket$specialty) 
#     phys$SPECGRP[i] <- PhysSpec2bucket$SPECGRP[PhysSpec2bucket$specialty == spec]
# }
# 
# # set all reclassified as unknown -- no way to find out what they belong to!
# phys$SPECGRP[which(phys$SPECGRP == "reclassified")] <- "unknown"
# # drop the SpecialtyFromPhysUniv column
# phys <- phys[,!(names(phys) %in% c("SpecialtyFromPhysUniv"))]

# only keep the approved physicians
phys <- phys[phys$absapproved=="Y",]
write.csv(phys,PhysDBFile,row.names=F)
