#  Madeleine Flint, 2015-06-10
#  Script to compare the distribution of failed bridges with all NBI bridges over water

require(plyr)
require(reshape2)

load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))

# year of construction
allBldYrs <- table(df.Fail.NBI.Gage[rowsToView,"YR_BLT_EST"])
allBldYrs <- allBldYrs[order(names(allBldYrs))]
VICbldYrs <- table(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"YR_BLT_EST"])
missingVIC <- names(allBldYrs)[!(names(allBldYrs) %in% names(VICbldYrs))]
VICbldYrs[missingVIC] <- 0
VICbldYrs <- VICbldYrs[order(names(VICbldYrs))]
BldYrChisQtest <- chisq.test(allBldYrs,VICbldYrs, simulate.p.value = TRUE) # p is  0.01549 from 2000 MC's, so we can reject the
# null hypothesis (that they are the same) at alpha = 0.10 or 0.05 -> they're probably different

# year of construction
allFailYrs <- table(df.Fail.NBI.Gage[rowsToView,"YR_FAIL_EST"])
allFailYrs <- allFailYrs[order(names(allFailYrs))]
VICFailYrs <- table(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"YR_FAIL_EST"])
missingVIC <- names(allFailYrs)[!(names(allFailYrs) %in% names(VICFailYrs))]
VICFailYrs[missingVIC] <- 0
VICFailYrs <- VICFailYrs[order(names(VICFailYrs))]
FailYrChisQtest <- chisq.test(allFailYrs,VICFailYrs, simulate.p.value = TRUE) # p-value = 0.0004998, definiely different

# drainage area
allDrainG <- table(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"])
allDrainG <- allDrainG[order(names(allDrainG))]
VICDrainG <- table(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])
missingVIC <- names(allDrainG)[!(names(allDrainG) %in% names(VICDrainG))]
VICDrainG[missingVIC] <- 0
VICDrainG <- VICDrainG[order(names(VICDrainG))]
DrainGYrChisQtest <- chisq.test(allDrainG,VICDrainG, simulate.p.value = TRUE) #  p-value = 0.03198, different at 5,10%

# drainage area
allDrainB <- table(df.Fail.NBI.Gage[rowsToView,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])
allDrainB <- allDrainB[order(names(allDrainB))]
VICDrainB <- table(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])
missingVIC <- names(allDrainB)[!(names(allDrainB) %in% names(VICDrainB))]
VICDrainB[missingVIC] <- 0
VICDrainB <- VICDrainB[order(names(VICDrainB))]
DrainBYrChisQtest <- chisq.test(allDrainB,VICDrainB, simulate.p.value = TRUE) # p-value = 0.03098, different at 5,10%

# distance to gauge
allDists <- table(round(df.Fail.NBI.Gage[rowsToView,"DIST_TO_GAGE"]))
allDists <- allDists[order(names(allDists))]
VICDists <- table(round(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DIST_TO_GAGE"]))
missingVIC <- names(allDists)[!(names(allDists) %in% names(VICDists))]
VICDists[missingVIC] <- 0
VICDists <- VICDists[order(names(VICDists))]
DistChisQtest <- chisq.test(allDists,VICDists, simulate.p.value = TRUE) # p-value = 0.3178 -> not different

chiSqTestAllvsVIC <- list(BldYrChisQtest,
                          FailYrChisQtest,
                          DrainGYrChisQtest,
                          DrainBYrChisQtest,
                          DistChisQtest)
savefile <-  paste(gsub("-","",Sys.Date()),"ChiSquaredTestForAll36andVICbridges.RData",sep="_")
save(chiSqTestAllvsVIC,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

