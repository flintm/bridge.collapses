#  Madeleine Flint, 2015-06-10
#  Script to compare the distribution of failed bridges with all NBI bridges over water

require(plyr)
require(reshape2)
require(ggplot2)
require(gridExtra)

# load(file.path(dirs$DataDirFrequent,"nbiDataFramePreProcessedAllFields.RData"))
# load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
# load(file.path(dirs$DataDirAnalysis,"20150615_df.Fail.NBI.GageWithNBImatAndTypeCodes.RData"))
load(file.path(dirsGit$Data,"nbiDataFramePreProcessedAllFields.RData"))
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))

colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage)=="ITEM43A"] <- "ITEM_43A_FAIL"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage)=="ITEM43B"] <- "ITEM_43B_FAIL"

BridgesDataFrame <- df.Fail.NBI.Gage[rowsToView,]
# BridgesDataFrame <- join(BridgesDataFrame[,!(colnames(BridgesDataFrame) %in% c("TYPE"))],df.Fail.NBI.Gage[,c("ID","TYPE","MATER","ITEM_43A_FAIL","ITEM_43B_FAIL")],by="ID")


nbiDataFrame <- nbiDataFrame[,c("ITEM8","STFIPS","ITEM27","ITEM43A","ITEM43B","ITEM106","LATDD","LONGDD")]

df.age.data <- df.Fail.NBI.Gage$YR_BLT_EST 
# hist_age <- ggplot(df.Fail.NBI.Gage, aes(x=price, fill=cut))
# hist_cut + geom_bar() # defaults to stacking


## NOW COMPARE HISTOGRAM OF ALL FAILED BRIDGES AND NBI -------------------
countsFail <- table(df.Fail.NBI.Gage[,c("ITEM_43A_FAIL","ITEM_43B_FAIL")])
df.mats                <- data.frame(MAT=c(levels(factor(nbiDataFrame$ITEM43A)), levels(factor(df.Fail.NBI.Gage$ITEM_43A_FAIL))),COUNT=NA,DATA=NA)
df.mats[1:10,"COUNT"]  <- sapply(levels(factor(nbiDataFrame$ITEM43A)), function(t) sum(nbiDataFrame[!is.na(nbiDataFrame$ITEM43A),"ITEM43A"]==t))
df.mats[1:10,"DATA"]   <- "NBI_WATER"
df.mats[11:20,"COUNT"] <- sapply(levels(factor(df.Fail.NBI.Gage$ITEM_43A_FAIL)), function(t) sum(df.Fail.NBI.Gage[!is.na(df.Fail.NBI.Gage$ITEM_43A_FAIL),"ITEM_43A_FAIL"]==t))
df.mats[11:20,"DATA"]  <- "FAIL_WATER"
df.mats[1:10,"FREQ"]   <-  df.mats[1:10,"COUNT"]/sum(df.mats[1:10,"COUNT"])
df.mats[11:20,"FREQ"]  <-  df.mats[11:20,"COUNT"]/sum(df.mats[11:20,"COUNT"])
hist2 <- ggplot(df.mats,aes(x=MAT,y=FREQ,fill=factor(DATA))) + geom_bar(stat="identity",position="dodge")

countsNBI         <- table(nbiDataFrame[,c("ITEM43A","ITEM43B")])
countsFail        <- table(df.Fail.NBI.Gage[,c("ITEM_43A_FAIL","ITEM_43B_FAIL")])
countsMatNBI      <- table(nbiDataFrame$ITEM43A)
countsMatFail     <- table(df.Fail.NBI.Gage$ITEM_43A_FAIL)

# chi-squared for mats
missingMats <- names(countsMatNBI)[!(names(countsMatNBI) %in% names(countsMatFail))]
countsMatFail[missingMats] <- 0
countsMatFail <- countsMatFail[order(as.numeric(names(countsMatFail)))]
nCountsMatNBI <- sum(countsMatNBI)
nCountsMatFail <- sum(countsMatFail)
matChiSquareYates  <- sum(sapply(names(countsMatNBI), function(i) (countsMatFail[i] - nCountsMatFail*(countsMatNBI[i]/nCountsMatNBI)-0.5)^2/(nCountsMatFail*countsMatNBI[i]/nCountsMatNBI)))

testMat <- chisq.test(countsMatFail,p = countsMatNBI/nCountsMatNBI, simulate.p.value = TRUE) # p-value = 0.0004998, definitely different

# see how types compares
countsTypeNBI     <- table(nbiDataFrame$ITEM43B)
countsTypeFail    <- table(df.Fail.NBI.Gage$ITEM_43B_FAIL)
df.types                <- data.frame(TYPE=c(levels(factor(nbiDataFrame$ITEM43B)), levels(factor(df.Fail.NBI.Gage$ITEM_43B_FAIL))),COUNT=NA,DATA=NA)
df.types[1:22,"COUNT"]  <- sapply(levels(factor(nbiDataFrame$ITEM43B)), function(t) sum(nbiDataFrame[!is.na(nbiDataFrame$ITEM43B),"ITEM43B"]==t))
df.types[1:22,"DATA"]   <- "NBI_WATER"
df.types[23:36,"COUNT"] <- sapply(levels(factor(df.Fail.NBI.Gage$ITEM_43B_FAIL)), function(t) sum(df.Fail.NBI.Gage[!is.na(df.Fail.NBI.Gage$ITEM_43B_FAIL),"ITEM_43B_FAIL"]==t))
df.types[23:36,"DATA"]  <- "FAIL_WATER"
df.types[1:22,"FREQ"]   <-  df.types[1:22,"COUNT"]/sum(df.types[1:22,"COUNT"])
df.types[23:36,"FREQ"]  <-  df.types[23:36,"COUNT"]/sum(df.types[23:36,"COUNT"])
hist1 <- ggplot(df.types,aes(x=TYPE,y=FREQ,fill=factor(DATA))) + geom_bar(stat="identity",position="dodge")


# chi-squared for types
missingTypes <- names(countsTypeNBI)[!(names(countsTypeNBI) %in% names(countsTypeFail))]
countsTypeFail[missingTypes] <- 0
countsTypeFail <- countsTypeFail[order(as.numeric(names(countsTypeFail)))]
nCountsTypeNBI <- sum(countsTypeNBI)
nCountsTypeFail <- sum(countsTypeFail)
typeChiSquareYates  <- sum(sapply(names(countsTypeNBI), function(i) (countsTypeFail[i] - nCountsTypeFail*(countsTypeNBI[i]/nCountsTypeNBI)-0.5)^2/(nCountsTypeFail*countsTypeNBI[i]/nCountsTypeNBI)))

testType <- chisq.test(countsTypeFail,p = countsTypeNBI/nCountsTypeNBI, simulate.p.value = TRUE) #, p-value = 0.0004998, definitely different

# both material and types
nbiDataFrame$ITEM43A <- factor(nbiDataFrame$ITEM43A)
nbiDataFrame$ITEM43B <- factor(nbiDataFrame$ITEM43B)

df.Fail.NBI.Gage$ITEM_43B_FAIL <- factor(df.Fail.NBI.Gage$ITEM_43B_FAIL, levels = levels(nbiDataFrame$ITEM43B))
df.Fail.NBI.Gage$ITEM_43A_FAIL <- factor(df.Fail.NBI.Gage$ITEM_43A_FAIL, levels = levels(nbiDataFrame$ITEM43A))

countMatTypeNBI <- table(nbiDataFrame[,c("ITEM43A","ITEM43B")])
countMatTypeFail <- table(df.Fail.NBI.Gage[,c("ITEM_43A_FAIL","ITEM_43B_FAIL")])

## COMPARE HISTOGRAM OF ALL 36 FAILED BRIDGES AND NBI -------------
countsNBI         <- table(nbiDataFrame[,c("ITEM43A","ITEM43B")])
countsFail        <- table(BridgesDataFrame[,c("ITEM_43A_FAIL","ITEM_43B_FAIL")])
countsMatNBI      <- table(nbiDataFrame$ITEM43A)
countsMatFail     <- table(BridgesDataFrame$ITEM_43A_FAIL)

# chi-squared for mats
missingMats <- names(countsMatNBI)[!(names(countsMatNBI) %in% names(countsMatFail))]
countsMatFail[missingMats] <- 0
countsMatFail <- countsMatFail[order(as.numeric(names(countsMatFail)))]
nCountsMatNBI <- sum(countsMatNBI)
nCountsMatFail <- sum(countsMatFail)
matChiSquareYates  <- sum(sapply(names(countsMatNBI), function(i) (countsMatFail[i] - nCountsMatFail*(countsMatNBI[i]/nCountsMatNBI)-0.5)^2/(nCountsMatFail*countsMatNBI[i]/nCountsMatNBI)))

testMat <- chisq.test(countsMatFail,p = countsMatNBI/nCountsMatNBI, simulate.p.value = TRUE) #p-value = 0.01999, can reject at 5%, 10%, not at 1%

# see how types compares
countsTypeNBI     <- table(nbiDataFrame$ITEM43B)
countsTypeFail    <- table(BridgesDataFrame$ITEM_43B_FAIL)


# chi-squared for types
missingTypes <- names(countsTypeNBI)[!(names(countsTypeNBI) %in% names(countsTypeFail))]
countsTypeFail[missingTypes] <- 0
countsTypeFail <- countsTypeFail[order(as.numeric(names(countsTypeFail)))]
nCountsTypeNBI <- sum(countsTypeNBI)
nCountsTypeFail <- sum(countsTypeFail)
typeChiSquareYates  <- sum(sapply(names(countsTypeNBI), function(i) (countsTypeFail[i] - nCountsTypeFail*(countsTypeNBI[i]/nCountsTypeNBI)-0.5)^2/(nCountsTypeFail*countsTypeNBI[i]/nCountsTypeNBI)))

testType <- chisq.test(countsTypeFail,p = countsTypeNBI/nCountsTypeNBI, simulate.p.value = TRUE) #, p-value = 0.002999, definitely different

# test power
library(pwr)
# Use Cohen's w for effect size (w)
w = sqrt(sum((countsTypeNBI/nCountsTypeNBI-countsTypeFail/nCountsTypeFail)^2/(countsTypeNBI/nCountsTypeNBI)))
pwrType <- pwr.chisq.test(w = w, N = nCountsTypeFail, df = length(countsTypeNBI)-1, sig.level = 0.05) # 1.0 power, 0.8 standard
# A test's power is the probability of correctly rejecting the null hypothesis when it is false
# So very low chance of false negative, 
w = sqrt(sum((countsMatNBI/nCountsMatNBI-countsMatFail/nCountsMatFail)^2/(countsMatNBI/nCountsMatNBI)))
pwrMat <- pwr.chisq.test(w = w, N = nCountsMatFail, df = length(countsMatNBI)-1, sig.level = 0.05) # 0.9994

## COMPARE HISTOGRAM OF BRIDGES IN TOP THREE STATES (VA, MD, NY) -------------
mainStates <- c("VA", "MD", "NY")
mainSTFIPS <- c(51, 36, 24)
countsNBI         <- table(nbiDataFrame[nbiDataFrame$STFIPS %in% mainSTFIPS,c("ITEM43A","ITEM43B")])
countsFail        <- table(BridgesDataFrame[BridgesDataFrame$STATE %in% mainStates,c("ITEM_43A_FAIL","ITEM_43B_FAIL")])
countsMatNBI      <- table(nbiDataFrame[nbiDataFrame$STFIPS %in% mainSTFIPS,"ITEM43A"])
countsMatFail     <- table(BridgesDataFrame[BridgesDataFrame$STATE %in% mainStates,"ITEM_43A_FAIL"])

# chi-squared for mats
missingMats <- names(countsMatNBI)[!(names(countsMatNBI) %in% names(countsMatFail))]
countsMatFail[missingMats] <- 0
countsMatFail <- countsMatFail[order(as.numeric(names(countsMatFail)))]
nCountsMatNBI <- sum(countsMatNBI)
nCountsMatFail <- sum(countsMatFail)
matChiSquareYates  <- sum(sapply(names(countsMatNBI), function(i) (countsMatFail[i] - nCountsMatFail*(countsMatNBI[i]/nCountsMatNBI)-0.5)^2/(nCountsMatFail*countsMatNBI[i]/nCountsMatNBI)))

testMatMainStates <- chisq.test(countsMatFail,p = countsMatNBI/nCountsMatNBI, simulate.p.value = TRUE) #p-value = 0.2224, cannot reject at 10, 5, or 1%

# see how types compares
countsTypeNBI     <- table(nbiDataFrame[nbiDataFrame$STFIPS %in% mainSTFIPS,"ITEM43B"])
countsTypeFail    <- table(BridgesDataFrame[BridgesDataFrame$STATE %in% mainStates,"ITEM_43B_FAIL"])

# chi-squared for types
missingTypes <- names(countsTypeNBI)[!(names(countsTypeNBI) %in% names(countsTypeFail))]
countsTypeFail[missingTypes] <- 0
countsTypeFail <- countsTypeFail[order(as.numeric(names(countsTypeFail)))]
nCountsTypeNBI <- sum(countsTypeNBI)
nCountsTypeFail <- sum(countsTypeFail)
typeChiSquareYates  <- sum(sapply(names(countsTypeNBI), function(i) (countsTypeFail[i] - nCountsTypeFail*(countsTypeNBI[i]/nCountsTypeNBI)-0.5)^2/(nCountsTypeFail*countsTypeNBI[i]/nCountsTypeNBI)))

testTypeMainStates <- chisq.test(countsTypeFail,p = countsTypeNBI/nCountsTypeNBI, simulate.p.value = TRUE) #, p-value = 0.08396, can reject at 10% but not 5 or 1%

# statistical power
w = sqrt(sum((countsTypeNBI/nCountsTypeNBI-countsTypeFail/nCountsTypeFail)^2/(countsTypeNBI/nCountsTypeNBI)))
pwrTypeMainStates <- pwr.chisq.test(w = w, N = nCountsTypeFail, df = length(countsTypeNBI)-1, sig.level = 0.05) # 0.9997 power, 0.8 standard
# A test's power is the probability of correctly rejecting the null hypothesis when it is false
w = sqrt(sum((countsMatNBI/nCountsMatNBI-countsMatFail/nCountsMatFail)^2/(countsMatNBI/nCountsMatNBI)))
pwrMatMainStates <- pwr.chisq.test(w = w, N = nCountsMatFail, df = length(countsMatNBI)-1, sig.level = 0.05) # 0.645
