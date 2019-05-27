# 2015-08-17 Determine failure and max events, and estimate return period of failure and max events 
# from MLE parameters of Daymet-VIC distributions
# 215-10-07 from Bulletin 17B analysis using HEC-SSP

require(stats)
require(nsRFA)
require(Hmisc)

# Load and source ------------------
source(file.path(dirsGit$ScriptsDir,"hydrologyDistrAssess.R"))

load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))

# load(file.path(dirs$DataDirAnalysis,"20150909_Gage_Daymet_VIC_Annual_Max_P3_LP3_GEV_Param.RData"))
# load(file.path(dirs$DataDirAnalysis,"20150813_Bridge_Daymet_VIC_Annual_Max_P3_LP3_GEV_Param.RData"))
load(file.path(dirs$DataDirFrequent,"20151203_GageRoutingVICDaymet.RData"))
load(file.path(dirs$DataDirFrequent,"20151203_BridgeRoutingVICDaymet.RData"))
load(file.path(dirs$DataDirAnalysis,"20151203_BridgeRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirs$DataDirAnalysis,"20151203_GageRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirs$DataDirFrequent,"20160107_HEC_PFA_VICG.RData"))
load(file.path(dirs$DataDirFrequent,"20160107_PeakFQ_PFA_VICg_UserRegSkews.RData"))
load(file.path(dirs$DataDirFrequent,"20160107_PeakFQ_PFA_VICb_UserRegSkews.RData"))

# ESTIMATE RETURN PERIOD OF FAILURE FLOW USING MLE PARAMETERS FROM DAYMET-VIC ANNUAL PEAKS -----------------------
rowsToAnalyzeVIC  <- rownames(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView & df.Fail.NBI.Gage$YR_FAIL_EST >= "1980-01-01",])
IDsToAnalyzeVIC   <- as.character(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"ID"])
STAIDtoAnalyzeVIC <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"STAID"]

problemSTAID <- which(STAIDtoAnalyzeVIC=="01090800") # don't have VIC data for one site
rowsToAnalyzeVIC <- rowsToAnalyzeVIC[-problemSTAID]
IDsToAnalyzeVIC   <- as.character(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"ID"])
STAIDtoAnalyzeVIC <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"STAID"]

nIDsVIC           <- length(IDsToAnalyzeVIC)

# BRIDGES  --------
n                       <- 2 # window of days before or after estimated fail date to check for max value of Daymet-VIC flow
FailDateIndex           <- rep(NA_integer_,nIDsVIC)
names(FailDateIndex)    <- IDsToAnalyzeVIC
FailDateIndexBridgePMn        <- rep(NA_integer_,nIDsVIC)
names(FailDateIndexBridgePMn) <- IDsToAnalyzeVIC
QfailBridge             <- rep(NA_real_,nIDsVIC)
names(QfailBridge)      <- IDsToAnalyzeVIC
QfailBridgePMn          <- rep(NA_real_,nIDsVIC)
names(QfailBridgePMn)   <- IDsToAnalyzeVIC
FailDateBridgePMn       <- rep(NA_real_,nIDsVIC)
names(FailDateBridgePMn)<- IDsToAnalyzeVIC
YrBltIndex              <- rep(NA_integer_,nIDsVIC)
names(YrBltIndex)       <- IDsToAnalyzeVIC
pExcBridge              <- list()
pExcBridgePMn           <- list()
MaxIndexBridge          <- rep(NA_integer_,nIDsVIC)
names(MaxIndexBridge)   <- IDsToAnalyzeVIC
QmaxBridge              <- rep(NA_real_,nIDsVIC)
names(QmaxBridge)       <- IDsToAnalyzeVIC
QmaxDateBridge          <- rep(NA_real_,nIDsVIC)
names(QmaxDateBridge)   <- IDsToAnalyzeVIC
FailYearIndex           <- list()
MaxFailYearIndexBridge         <- rep(NA_integer_,nIDsVIC)
names(MaxFailYearIndexBridge ) <- IDsToAnalyzeVIC
QmaxFailYearBridge             <- rep(NA_real_,nIDsVIC)
names(QmaxFailYearBridge)      <- IDsToAnalyzeVIC
QmaxFailYearDateBridge         <- rep(NA_real_,nIDsVIC)
names(QmaxFailYearDateBridge)  <- IDsToAnalyzeVIC
MaxPreFailIndexBridge          <- rep(NA_integer_,nIDsVIC)
names(MaxPreFailIndexBridge)   <- IDsToAnalyzeVIC
QmaxPreFailBridge              <- rep(NA_real_,nIDsVIC)
names(QmaxPreFailBridge)       <- IDsToAnalyzeVIC
QmaxPreFailDateBridge          <- rep(NA_real_,nIDsVIC)
names(QmaxPreFailDateBridge)   <- IDsToAnalyzeVIC

# QFail from Daymet-VIC Bridge site, distribution from Daymet-VIC bridge
for (ID in IDsToAnalyzeVIC){
  if (ID=="1164") next()
  FailDateIndex[ID]      <- which.min(abs( ls.BridgeRouted[[ID]]$day$Date - as.Date(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"DATE_FAIL_EST_USGS"]) )) # checks out
  FailDatesIndexPMn      <- c(FailDateIndex[ID]-n,FailDateIndex[ID]+n)
  FailYearIndex[[ID]][1] <- which.min(abs( ls.BridgeRouted[[ID]]$day$Date - 
                                       as.Date(paste(substr(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"DATE_FAIL_EST_USGS"],1,4),"01","01",sep="-")) )) # checks out
  FailYearIndex[[ID]][2] <- which.min(abs( ls.BridgeRouted[[ID]]$day$Date - 
                                       as.Date(paste(substr(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"DATE_FAIL_EST_USGS"],1,4),"12","31",sep="-")) )) # checks out
  YrBltIndex[ID]         <- ifelse(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"YR_BLT_EST"] <= ls.BridgeRouted[[ID]]$day$Date[1] |
                                     is.na(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"YR_BLT_EST"]),
                                   1,
                                   which.min(abs(as.numeric(ls.BridgeRouted[[ID]]$day$Date - df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"YR_BLT_EST"])))
                                   )
  
#   print(paste("For ID ",ID," Fail date est:",df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"DATE_FAIL_EST_USGS"],", From Daymet-VIC:",ls.BridgeRouted[[ID]]$day$Date[FailDateIndex[ID]]))
#   
  QfailBridge[ID]  <- ls.BridgeRouted[[ID]]$day$RUNOFF_CFS[FailDateIndex[ID]]
#   FGEV             <- F.GEV(QfailBridge[ID],mlBridgesAnnualMax[["GEV"]][[ID]][1],mlBridgesAnnualMax[["GEV"]][[ID]][2],mlBridgesAnnualMax[["GEV"]][[ID]][3])
#   FLP3             <- F.gamma(log(QfailBridge[ID]),mlBridgesAnnualMax[["LP3"]][[ID]][1],mlBridgesAnnualMax[["LP3"]][[ID]][2],mlBridgesAnnualMax[["LP3"]][[ID]][3])
#   FP3              <- F.gamma(QfailBridge[ID],mlBridgesAnnualMax[["P3"]][[ID]][1],mlBridgesAnnualMax[["P3"]][[ID]][2],mlBridgesAnnualMax[["P3"]][[ID]][3])
#   pExcBridge[[ID]] <- c(GEV = 1-FGEV, LP3 = 1-FLP3, P3 = 1-FP3)
#   
  FailDateIndexBridgePMn[ID]   <- which.max(ls.BridgeRouted[[ID]]$day$RUNOFF_CFS[FailDatesIndexPMn[1]:FailDatesIndexPMn[2]])
  QfailBridgePMn[ID]     <- ls.BridgeRouted[[ID]]$day$RUNOFF_CFS[FailDateIndexBridgePMn[ID] + FailDatesIndexPMn[1] - 1]
  FailDateBridgePMn[ID]  <- ls.BridgeRouted[[ID]]$day$Date[FailDateIndexBridgePMn[ID]+ FailDatesIndexPMn[1] - 1]
#   FGEV             <- F.GEV(QfailBridgePMn[ID],mlBridgesAnnualMax[["GEV"]][[ID]][1],mlBridgesAnnualMax[["GEV"]][[ID]][2],mlBridgesAnnualMax[["GEV"]][[ID]][3])
#   FLP3             <- F.gamma(log(QfailBridgePMn[ID]),mlBridgesAnnualMax[["LP3"]][[ID]][1],mlBridgesAnnualMax[["LP3"]][[ID]][2],mlBridgesAnnualMax[["LP3"]][[ID]][3])
#   FP3              <- F.gamma(QfailBridgePMn[ID],mlBridgesAnnualMax[["P3"]][[ID]][1],mlBridgesAnnualMax[["P3"]][[ID]][2],mlBridgesAnnualMax[["P3"]][[ID]][3])
#   pExcBridgePMn[[ID]] <- c(GEV = 1-FGEV, LP3 = 1-FLP3, P3 = 1-FP3)
#   
#   
#   
  MaxIndexBridge[ID] <- which.max(ls.BridgeRouted[[ID]]$day$RUNOFF_CFS)
  QmaxBridge[ID]     <- ls.BridgeRouted[[ID]]$day$RUNOFF_CFS[MaxIndexBridge[ID]]
  QmaxDateBridge[ID] <- ls.BridgeRouted[[ID]]$day$Date[MaxIndexBridge[ID]]
#   
  MaxFailYearIndexBridge[ID] <- which.max(ls.BridgeRouted[[ID]]$day$RUNOFF_CFS[c(FailYearIndex[[ID]][1]:FailYearIndex[[ID]][2])]) + FailYearIndex[[ID]][1] - 1
  QmaxFailYearBridge[ID]     <- ls.BridgeRouted[[ID]]$day$RUNOFF_CFS[MaxFailYearIndexBridge[ID]]
  QmaxFailYearDateBridge[ID] <- ls.BridgeRouted[[ID]]$day$Date[MaxFailYearIndexBridge[ID]]
#   
  MaxPreFailIndexBridge[ID]  <- which.max(ls.BridgeRouted[[ID]]$day$RUNOFF_CFS[c(YrBltIndex[ID]:FailDateIndex[ID])])
  QmaxPreFailBridge[ID]      <- ls.BridgeRouted[[ID]]$day$RUNOFF_CFS[MaxPreFailIndexBridge[ID] + YrBltIndex[ID] - 1]
  QmaxPreFailDateBridge[ID]  <- ls.BridgeRouted[[ID]]$day$Date[MaxPreFailIndexBridge[ID] + YrBltIndex[ID] - 1]
}
df.Fail.NBI.Gage$Q_FAIL_DVICB <- NA_real_
df.Fail.NBI.Gage$EXCEED_GEV_DAYMET_VIC_BRIDGE <- NA_real_
df.Fail.NBI.Gage$EXCEED_LP3_DAYMET_VIC_BRIDGE <- NA_real_
df.Fail.NBI.Gage$EXCEED_P3_DAYMET_VIC_BRIDGE <- NA_real_
df.Fail.NBI.Gage$Q_MAX_DVICB <- NA_real_
df.Fail.NBI.Gage$Q_MAXPREFAIL_DVICB <- NA_real_
df.Fail.NBI.Gage$Q_FAILYRMAX_DVICB <- NA_real_
df.Fail.NBI.Gage$DATE_MAX_DVICB <- NA_real_
df.Fail.NBI.Gage$DATE_MAXPREFAIL_DVICB <- NA_real_
df.Fail.NBI.Gage$DATE_FAILYRMAX_DVICB <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q_FAIL_DVICB"]         <- QfailBridge 
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"EXCEED_GEV_DAYMET_VIC_BRIDGE"]     <- sapply(IDsToAnalyzeVIC, function(ID) pExcBridge[[ID]][paste("GEV",ID,sep=".")]) # no change
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"EXCEED_LP3_DAYMET_VIC_BRIDGE"]     <- sapply(IDsToAnalyzeVIC, function(ID) pExcBridge[[ID]][paste("LP3",ID,sep=".")]) # no change
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"EXCEED_P3_DAYMET_VIC_BRIDGE"]      <- sapply(IDsToAnalyzeVIC, function(ID) pExcBridge[[ID]][paste("P3",ID,sep=".")]) # no change
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q_MAX_DVICB"]          <- QmaxBridge 
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q_MAXPREFAIL_DVICB"] <- QmaxPreFailBridge 
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q_FAILYRMAX_DVICB"]  <- QmaxFailYearBridge
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DATE_MAX_DVICB"]          <- QmaxDateBridge
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DATE_MAXPREFAIL_DVICB"] <- QmaxPreFailDateBridge
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DATE_FAILYRMAX_DVICB"]  <- QmaxFailYearDateBridge
df.Fail.NBI.Gage$T_FAIL_GEV_DAYMET_VIC_BRIDGE <- 1/df.Fail.NBI.Gage$EXCEED_GEV_DAYMET_VIC_BRIDGE
df.Fail.NBI.Gage$T_FAIL_LP3_DAYMET_VIC_BRIDGE <- 1/df.Fail.NBI.Gage$EXCEED_LP3_DAYMET_VIC_BRIDGE
df.Fail.NBI.Gage$T_FAIL_P3_DAYMET_VIC_BRIDGE  <- 1/df.Fail.NBI.Gage$EXCEED_P3_DAYMET_VIC_BRIDGE

df.Fail.NBI.Gage$Q_FAILPM2_DVICB <- NA_real_
df.Fail.NBI.Gage$DATE_FAILPM2_DVICB <- NA_real_
df.Fail.NBI.Gage$EXCEED_GEV_PM2_DAYMET_VIC_BRIDGE <- NA_real_
df.Fail.NBI.Gage$EXCEED_LP3_PM2_DAYMET_VIC_BRIDGE <- NA_real_
df.Fail.NBI.Gage$EXCEED_P3_PM2_DAYMET_VIC_BRIDGE <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q_FAILPM2_DVICB"]         <- QfailBridgePMn
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DATE_FAILPM2_DVICB"]      <- FailDateBridgePMn
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"EXCEED_GEV_PM2_DAYMET_VIC_BRIDGE"]     <- sapply(IDsToAnalyzeVIC, function(ID) pExcBridgePMn[[ID]][paste("GEV",ID,sep=".")])
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"EXCEED_LP3_PM2_DAYMET_VIC_BRIDGE"]     <- sapply(IDsToAnalyzeVIC, function(ID) pExcBridgePMn[[ID]][paste("LP3",ID,sep=".")])
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"EXCEED_P3_PM2_DAYMET_VIC_BRIDGE"]      <- sapply(IDsToAnalyzeVIC, function(ID) pExcBridgePMn[[ID]][paste("P3",ID,sep=".")])
df.Fail.NBI.Gage$T_FAIL_GEV_PM2_DAYMET_VIC_BRIDGE <- 1/df.Fail.NBI.Gage$EXCEED_GEV_PM2_DAYMET_VIC_BRIDGE
df.Fail.NBI.Gage$T_FAIL_LP3_PM2_DAYMET_VIC_BRIDGE <- 1/df.Fail.NBI.Gage$EXCEED_LP3_PM2_DAYMET_VIC_BRIDGE
df.Fail.NBI.Gage$T_FAIL_P3_PM2_DAYMET_VIC_BRIDGE  <- 1/df.Fail.NBI.Gage$EXCEED_P3_PM2_DAYMET_VIC_BRIDGE

# Find return period of max events
pExcMaxBridge              <- list()
pExcMaxPreFailBridge       <- list()
for (i in 1:nIDsVIC){
  ID                  <- IDsToAnalyzeVIC[i]
  row                 <- rowsToAnalyzeVIC[i]
  
  FGEV                <- F.GEV(df.Fail.NBI.Gage[row,"Q_MAXPREFAIL_DVICB"],mlBridgesAnnualMax[["GEV"]][[ID]][1],mlBridgesAnnualMax[["GEV"]][[ID]][2],mlBridgesAnnualMax[["GEV"]][[ID]][3])
  FLP3                <- F.gamma(log(df.Fail.NBI.Gage[row,"Q_MAXPREFAIL_DVICB"]),mlBridgesAnnualMax[["LP3"]][[ID]][1],mlBridgesAnnualMax[["LP3"]][[ID]][2],mlBridgesAnnualMax[["LP3"]][[ID]][3])
  FP3                 <- F.gamma(df.Fail.NBI.Gage[row,"Q_MAXPREFAIL_DVICB"],mlBridgesAnnualMax[["P3"]][[ID]][1],mlBridgesAnnualMax[["P3"]][[ID]][2],mlBridgesAnnualMax[["P3"]][[ID]][3])
  pExcMaxPreFailBridge[[ID]] <- c(GEV = 1-FGEV, LP3 = 1-FLP3, P3 = 1-FP3)
  
  FGEV                       <- F.GEV(df.Fail.NBI.Gage[row,"Q_MAX_DVICB"],mlBridgesAnnualMax[["GEV"]][[ID]][1],mlBridgesAnnualMax[["GEV"]][[ID]][2],mlBridgesAnnualMax[["GEV"]][[ID]][3])
  FLP3                       <- F.gamma(log(df.Fail.NBI.Gage[row,"Q_MAX_DVICB"]),mlBridgesAnnualMax[["LP3"]][[ID]][1],mlBridgesAnnualMax[["LP3"]][[ID]][2],mlBridgesAnnualMax[["LP3"]][[ID]][3])
  FP3                        <- F.gamma(df.Fail.NBI.Gage[row,"Q_MAX_DVICB"],mlBridgesAnnualMax[["P3"]][[ID]][1],mlBridgesAnnualMax[["P3"]][[ID]][2],mlBridgesAnnualMax[["P3"]][[ID]][3])
  pExcMaxBridge[[ID]] <- c(GEV = 1-FGEV, LP3 = 1-FLP3, P3 = 1-FP3)
}
df.Fail.NBI.Gage$T_MAX_GEV_DAYMET_VIC_BRIDGE <- NA_real_
df.Fail.NBI.Gage$T_MAX_LP3_DAYMET_VIC_BRIDGE <- NA_real_
df.Fail.NBI.Gage$T_MAX_P3_DAYMET_VIC_BRIDGE  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_GEV_DAYMET_VIC_BRIDGE"] <- 1/sapply(IDsToAnalyzeVIC, function(ID) pExcMaxBridge[[ID]]["GEV"]) # this was fixed
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_LP3_DAYMET_VIC_BRIDGE"] <- 1/sapply(IDsToAnalyzeVIC, function(ID) pExcMaxBridge[[ID]]["LP3"]) # this was fixed
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_P3_DAYMET_VIC_BRIDGE"]  <- 1/sapply(IDsToAnalyzeVIC, function(ID) pExcMaxBridge[[ID]]["P3"]) # this was fixed
df.Fail.NBI.Gage$T_MAX_PRE_FAIL_GEV_DAYMET_VIC_BRIDGE <- NA_real_
df.Fail.NBI.Gage$T_MAX_PRE_FAIL_LP3_DAYMET_VIC_BRIDGE <- NA_real_
df.Fail.NBI.Gage$T_MAX_PRE_FAIL_P3_DAYMET_VIC_BRIDGE  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_PRE_FAIL_GEV_DAYMET_VIC_BRIDGE"] <- 1/sapply(IDsToAnalyzeVIC, function(ID) pExcMaxPreFailBridge[[ID]]["GEV"]) # this was fixed
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_PRE_FAIL_LP3_DAYMET_VIC_BRIDGE"] <- 1/sapply(IDsToAnalyzeVIC, function(ID) pExcMaxPreFailBridge[[ID]]["LP3"])# this was fixed
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_PRE_FAIL_P3_DAYMET_VIC_BRIDGE"]  <- 1/sapply(IDsToAnalyzeVIC, function(ID) pExcMaxPreFailBridge[[ID]]["P3"])# this was fixed

View(df.Fail.NBI.Gage[rowsToAnalyzeVIC,c("ID","FAIL_CAUS","T_FAIL_D_HECP_USGS","T_FAIL_I_HECP_USGS","T_FAIL_GEV_DAYMET_VIC_BRIDGE","T_FAIL_LP3_DAYMET_VIC_BRIDGE","T_FAIL_P3_DAYMET_VIC_BRIDGE",
                                         "T_FAIL_GEV_PM2_DAYMET_VIC_BRIDGE","T_FAIL_LP3_PM2_DAYMET_VIC_BRIDGE","T_FAIL_P3_PM2_DAYMET_VIC_BRIDGE")])
View(df.Fail.NBI.Gage[rowsToAnalyzeVIC,c("ID","DATE_FAIL_EST_USGS","FAIL_CAUS","Q_FAIL_D_USGS","T_FAIL_D_HECP_USGS","Q_FAIL_I_USGS","T_FAIL_I_HECP_USGS","Q_FAIL_DVICB", "Q_FAILPM2_DVICB",
                                         "DATE_FAILPM2_DVICB","Q_FAILYRMAX_DVICB",
                                         "DATE_FAILYRMAX_DVICB", "T_FAIL_GEV_DAYMET_VIC_BRIDGE","T_FAIL_LP3_DAYMET_VIC_BRIDGE","T_FAIL_P3_DAYMET_VIC_BRIDGE","Q_MAX_D_USGS",
                                         "Q_MAX_DVICB","Q_MAXPREFAIL_D_USGS","Q_MAXPREFAIL_DVICB","DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM","DRAIN_AREA_DVICB_SQKM")])

# GAUGES (MLE)  --------
FailDateIndex         <- rep(NA_integer_,nIDsVIC)
FailYearIndex           <- list()
YrBltIndex            <- rep(NA_integer_,nIDsVIC)
QfailGage             <- rep(NA_real_,nIDsVIC)
names(QfailGage)      <- IDsToAnalyzeVIC
FailDateIndexGagePMn        <- rep(NA_integer_,nIDsVIC)
names(FailDateIndexGagePMn) <- IDsToAnalyzeVIC
QfailGagePMn          <- rep(NA_real_,nIDsVIC)
names(QfailGagePMn)   <- IDsToAnalyzeVIC
FailDateGagePMn       <- rep(NA_real_,nIDsVIC)
names(FailDateGagePMn)<- IDsToAnalyzeVIC
pExcGage              <- list()
pExcGagePMn           <- list()
MaxIndexGage          <- rep(NA_integer_,nIDsVIC)
names(MaxIndexGage)   <- IDsToAnalyzeVIC
QmaxGage              <- rep(NA_real_,nIDsVIC)
names(QmaxGage)       <- IDsToAnalyzeVIC
QmaxDateGage          <- rep(NA_real_,nIDsVIC)
names(QmaxDateGage)   <- IDsToAnalyzeVIC
MaxFailYearIndexGage         <- rep(NA_integer_,nIDsVIC)
names(MaxFailYearIndexGage ) <- IDsToAnalyzeVIC
QmaxFailYearGage             <- rep(NA_real_,nIDsVIC)
names(QmaxFailYearGage)      <- IDsToAnalyzeVIC
QmaxFailYearDateGage         <- rep(NA_real_,nIDsVIC)
names(QmaxFailYearDateGage)  <- IDsToAnalyzeVIC
MaxPreFailIndexGage          <- rep(NA_integer_,nIDsVIC)
names(MaxPreFailIndexGage)   <- IDsToAnalyzeVIC
QmaxPreFailGage              <- rep(NA_real_,nIDsVIC)
names(QmaxPreFailGage)       <- IDsToAnalyzeVIC
QmaxPreFailDateGage          <- rep(NA_real_,nIDsVIC)
names(QmaxPreFailDateGage)   <- IDsToAnalyzeVIC

# QFail from Daymet-VIC Gage site, distribution from Daymet-VIC Gage
for (i in 1:nIDsVIC){
  ID <- IDsToAnalyzeVIC[i]
  STAID <-STAIDtoAnalyzeVIC[i]
  FailDateIndex[ID]      <- which.min(abs( ls.GageRouted[[STAID]]$day$Date - as.Date(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"DATE_FAIL_EST_USGS"]) )) # checks out
  FailDatesIndexPMn      <- c(FailDateIndex[ID]-n,FailDateIndex[ID]+n)
  FailYearIndex[[ID]][1] <- which.min(abs( ls.GageRouted[[STAID]]$day$Date - 
                                             as.Date(paste(substr(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"DATE_FAIL_EST_USGS"],1,4),"01","01",sep="-")) )) # checks out
  FailYearIndex[[ID]][2] <- which.min(abs( ls.GageRouted[[STAID]]$day$Date - 
                                             as.Date(paste(substr(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"DATE_FAIL_EST_USGS"],1,4),"12","31",sep="-")) )) # checks out
  YrBltIndex[ID]         <- ifelse(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"YR_BLT_EST"] <= ls.GageRouted[[STAID]]$day$Date[1] |
                                     is.na(as.Date(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"YR_BLT_EST"],"1970-01-01")),
                                   1,
                                   which.min(abs(as.numeric(ls.GageRouted[[STAID]]$day$Date - as.Date(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"YR_BLT_EST"],"1970-01-01"))))
                                   )
  FailDatesIndexPMn      <- c(FailDateIndex[ID]-n,FailDateIndex[ID]+n)
  print(paste("For ID ",ID," Fail date est:",df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"DATE_FAIL_EST_USGS"],", From Daymet-VIC:",ls.GageRouted[[STAID]]$day$Date[FailDateIndex[ID]]))
  
  QfailGage[ID]  <- ls.GageRouted[[STAID]]$day$RUNOFF_CFS[FailDateIndex[ID]]
#   FGEV             <- F.GEV(QfailGage[ID],mlGagesAnnualMax[["GEV"]][[STAID]][1],mlGagesAnnualMax[["GEV"]][[STAID]][2],mlGagesAnnualMax[["GEV"]][[STAID]][3])
#   FLP3             <- F.gamma(log(QfailGage[ID]),mlGagesAnnualMax[["LP3"]][[STAID]][1],mlGagesAnnualMax[["LP3"]][[STAID]][2],mlGagesAnnualMax[["LP3"]][[STAID]][3])
#   FP3              <- F.gamma(QfailGage[ID],mlGagesAnnualMax[["P3"]][[STAID]][1],mlGagesAnnualMax[["P3"]][[STAID]][2],mlGagesAnnualMax[["P3"]][[STAID]][3])
#   pExcGage[[ID]] <- c(GEV = 1-FGEV, LP3 = 1-FLP3, P3 = 1-FP3)
#   
  FailDateIndexGagePMn[ID]   <- which.max(ls.GageRouted[[STAID]]$day$RUNOFF_CFS[FailDatesIndexPMn[1]:FailDatesIndexPMn[2]]) + FailDatesIndexPMn[1] - 1
  QfailGagePMn[ID]     <- ls.GageRouted[[STAID]]$day$RUNOFF_CFS[FailDateIndexGagePMn[ID]]
  FailDateGagePMn[ID]  <- ls.GageRouted[[STAID]]$day$Date[FailDateIndexGagePMn[ID]]
#   FGEV             <- F.GEV(QfailGagePMn[ID],mlGagesAnnualMax[["GEV"]][[STAID]][1],mlGagesAnnualMax[["GEV"]][[STAID]][2],mlGagesAnnualMax[["GEV"]][[STAID]][3])
#   FLP3             <- F.gamma(log(QfailGagePMn[ID]),mlGagesAnnualMax[["LP3"]][[STAID]][1],mlGagesAnnualMax[["LP3"]][[STAID]][2],mlGagesAnnualMax[["LP3"]][[STAID]][3])
#   FP3              <- F.gamma(QfailGagePMn[ID],mlGagesAnnualMax[["P3"]][[STAID]][1],mlGagesAnnualMax[["P3"]][[STAID]][2],mlGagesAnnualMax[["P3"]][[STAID]][3])
#   pExcGagePMn[[ID]] <- c(GEV = 1-FGEV, LP3 = 1-FLP3, P3 = 1-FP3)
  
  MaxIndexGage[ID] <- which.max(ls.GageRouted[[STAID]]$day$RUNOFF_CFS)
  QmaxGage[ID]     <- ls.GageRouted[[STAID]]$day$RUNOFF_CFS[MaxIndexGage[ID]]
  QmaxDateGage[ID] <- ls.GageRouted[[STAID]]$day$Date[MaxIndexGage[ID]]
  
  MaxFailYearIndexGage[ID] <- which.max(ls.GageRouted[[STAID]]$day$RUNOFF_CFS[c(FailYearIndex[[ID]][1]:FailYearIndex[[ID]][2])])
  QmaxFailYearGage[ID]     <- ls.GageRouted[[STAID]]$day$RUNOFF_CFS[MaxFailYearIndexGage[ID] + FailYearIndex[[ID]][1] - 1]
  QmaxFailYearDateGage[ID] <- ls.GageRouted[[STAID]]$day$Date[MaxFailYearIndexGage[ID] + FailYearIndex[[ID]][1] - 1]
  
  MaxPreFailIndexGage[ID]  <- which.max(ls.GageRouted[[STAID]]$day$RUNOFF_CFS[c(YrBltIndex[ID]:FailDateIndex[ID])])
  QmaxPreFailGage[ID]      <- ls.GageRouted[[STAID]]$day$RUNOFF_CFS[MaxPreFailIndexGage[ID] + YrBltIndex[ID] - 1]
  QmaxPreFailDateGage[ID]  <- ls.GageRouted[[STAID]]$day$Date[MaxPreFailIndexGage[ID] + YrBltIndex[ID] - 1]
}
df.Fail.NBI.Gage$Q_FAIL_DVICG <- NA_real_
df.Fail.NBI.Gage$EXCEED_GEV_DAYMET_VIC_GAGE <- NA_real_
df.Fail.NBI.Gage$EXCEED_LP3_DAYMET_VIC_GAGE <- NA_real_
df.Fail.NBI.Gage$EXCEED_P3_DAYMET_VIC_GAGE <- NA_real_
df.Fail.NBI.Gage$Q_MAX_DVICG <- NA_real_
df.Fail.NBI.Gage$Q_MAXPREFAIL_DVICG <- NA_real_
df.Fail.NBI.Gage$Q_FAILYRMAX_DVICG <- NA_real_
df.Fail.NBI.Gage$DATE_MAX_DVICG <- NA_real_
df.Fail.NBI.Gage$DATE_MAXPREFAIL_DVICG <- NA_real_
df.Fail.NBI.Gage$DATE_FAILYRMAX_DVICG <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q_FAIL_DVICG"]         <- QfailGage 
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"EXCEED_GEV_DAYMET_VIC_GAGE"]     <- sapply(IDsToAnalyzeVIC, function(ID) pExcGage[[ID]][paste("GEV",ID,sep=".")])
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"EXCEED_LP3_DAYMET_VIC_GAGE"]     <- sapply(IDsToAnalyzeVIC, function(ID) pExcGage[[ID]][paste("LP3",ID,sep=".")]) 
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"EXCEED_P3_DAYMET_VIC_GAGE"]      <- sapply(IDsToAnalyzeVIC, function(ID) pExcGage[[ID]][paste("P3",ID,sep=".")])
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q_MAX_DVICG"]          <- QmaxGage 
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q_MAXPREFAIL_DVICG"] <- QmaxPreFailGage
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q_FAILYRMAX_DVICG"]  <- QmaxFailYearGage
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DATE_MAX_DVICG"]          <- QmaxDateGage
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DATE_MAXPREFAIL_DVICG"] <- QmaxPreFailDateGage
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DATE_FAILYRMAX_DVICG"]  <- QmaxFailYearDateGage
df.Fail.NBI.Gage$T_FAIL_GEV_DAYMET_VIC_GAGE <- 1/df.Fail.NBI.Gage$EXCEED_GEV_DAYMET_VIC_GAGE
df.Fail.NBI.Gage$T_FAIL_LP3_DAYMET_VIC_GAGE <- 1/df.Fail.NBI.Gage$EXCEED_LP3_DAYMET_VIC_GAGE
df.Fail.NBI.Gage$T_FAIL_P3_DAYMET_VIC_GAGE  <- 1/df.Fail.NBI.Gage$EXCEED_P3_DAYMET_VIC_GAGE

df.Fail.NBI.Gage$Q_FAILPM2_DVICG <- NA_real_
df.Fail.NBI.Gage$DATE_FAILPM2_DVICG <- NA_real_
df.Fail.NBI.Gage$EXCEED_GEV_PM2_DAYMET_VIC_GAGE <- NA_real_
df.Fail.NBI.Gage$EXCEED_LP3_PM2_DAYMET_VIC_GAGE <- NA_real_
df.Fail.NBI.Gage$EXCEED_P3_PM2_DAYMET_VIC_GAGE <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q_FAILPM2_DVICG"]         <- QfailGagePMn
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DATE_FAILPM2_DVICG"]      <- FailDateGagePMn
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"EXCEED_GEV_PM2_DAYMET_VIC_GAGE"]     <- sapply(IDsToAnalyzeVIC, function(ID) pExcGagePMn[[ID]][paste("GEV",ID,sep=".")])
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"EXCEED_LP3_PM2_DAYMET_VIC_GAGE"]     <- sapply(IDsToAnalyzeVIC, function(ID) pExcGagePMn[[ID]][paste("LP3",ID,sep=".")])
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"EXCEED_P3_PM2_DAYMET_VIC_GAGE"]      <- sapply(IDsToAnalyzeVIC, function(ID) pExcGagePMn[[ID]][paste("P3",ID,sep=".")])
df.Fail.NBI.Gage$T_FAIL_GEV_PM2_DAYMET_VIC_GAGE <- 1/df.Fail.NBI.Gage$EXCEED_GEV_PM2_DAYMET_VIC_GAGE
df.Fail.NBI.Gage$T_FAIL_LP3_PM2_DAYMET_VIC_GAGE <- 1/df.Fail.NBI.Gage$EXCEED_LP3_PM2_DAYMET_VIC_GAGE
df.Fail.NBI.Gage$T_FAIL_P3_PM2_DAYMET_VIC_GAGE  <- 1/df.Fail.NBI.Gage$EXCEED_P3_PM2_DAYMET_VIC_GAGE

# Find return period of max events
pExcMaxGage              <- list()
pExcMaxPreFailGage       <- list()
for (i in 1:nIDsVIC){
  STAID                  <- STAIDtoAnalyzeVIC[i]
  ID <- IDsToAnalyzeVIC[i]
  row                 <- rowsToAnalyzeVIC[i]
  FGEV                <- F.GEV(df.Fail.NBI.Gage[row,"Q_MAXPREFAIL_DVICG"],mlGagesAnnualMax[["GEV"]][[STAID]][1],mlGagesAnnualMax[["GEV"]][[STAID]][2],mlGagesAnnualMax[["GEV"]][[STAID]][3])
  FLP3                <- F.gamma(log(df.Fail.NBI.Gage[row,"Q_MAXPREFAIL_DVICG"]),mlGagesAnnualMax[["LP3"]][[STAID]][1],mlGagesAnnualMax[["LP3"]][[STAID]][2],mlGagesAnnualMax[["LP3"]][[STAID]][3])
  FP3                 <- F.gamma(df.Fail.NBI.Gage[row,"Q_MAXPREFAIL_DVICG"],mlGagesAnnualMax[["P3"]][[STAID]][1],mlGagesAnnualMax[["P3"]][[STAID]][2],mlGagesAnnualMax[["P3"]][[STAID]][3])
  pExcMaxPreFailGage[[ID]] <- c(GEV = 1-FGEV, LP3 = 1-FLP3, P3 = 1-FP3)
  
  FGEV                       <- F.GEV(df.Fail.NBI.Gage[row,"Q_MAX_DVICG"],mlGagesAnnualMax[["GEV"]][[STAID]][1],mlGagesAnnualMax[["GEV"]][[STAID]][2],mlGagesAnnualMax[["GEV"]][[STAID]][3])
  FLP3                       <- F.gamma(log(df.Fail.NBI.Gage[row,"Q_MAX_DVICG"]),mlGagesAnnualMax[["LP3"]][[STAID]][1],mlGagesAnnualMax[["LP3"]][[STAID]][2],mlGagesAnnualMax[["LP3"]][[STAID]][3])
  FP3                        <- F.gamma(df.Fail.NBI.Gage[row,"Q_MAX_DVICG"],mlGagesAnnualMax[["P3"]][[STAID]][1],mlGagesAnnualMax[["P3"]][[STAID]][2],mlGagesAnnualMax[["P3"]][[STAID]][3])
  pExcMaxGage[[ID]] <- c(GEV = 1-FGEV, LP3 = 1-FLP3, P3 = 1-FP3)
}
df.Fail.NBI.Gage$T_MAX_GEV_DAYMET_VIC_GAGE <- NA_real_
df.Fail.NBI.Gage$T_MAX_LP3_DAYMET_VIC_GAGE <- NA_real_
df.Fail.NBI.Gage$T_MAX_P3_DAYMET_VIC_GAGE  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_GEV_DAYMET_VIC_GAGE"] <- 1/sapply(IDsToAnalyzeVIC, function(ID) pExcMaxGage[[ID]]["GEV"])
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_LP3_DAYMET_VIC_GAGE"] <- 1/sapply(IDsToAnalyzeVIC, function(ID) pExcMaxGage[[ID]]["LP3"]) 
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_P3_DAYMET_VIC_GAGE"]  <- 1/sapply(IDsToAnalyzeVIC, function(ID) pExcMaxGage[[ID]]["P3"])
df.Fail.NBI.Gage$T_MAX_PRE_FAIL_GEV_DAYMET_VIC_GAGE <- NA_real_
df.Fail.NBI.Gage$T_MAX_PRE_FAIL_LP3_DAYMET_VIC_GAGE <- NA_real_
df.Fail.NBI.Gage$T_MAX_PRE_FAIL_P3_DAYMET_VIC_GAGE  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_PRE_FAIL_GEV_DAYMET_VIC_GAGE"] <- 1/sapply(IDsToAnalyzeVIC, function(ID) pExcMaxPreFailGage[[ID]]["GEV"])
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_PRE_FAIL_LP3_DAYMET_VIC_GAGE"] <- 1/sapply(IDsToAnalyzeVIC, function(ID) pExcMaxPreFailGage[[ID]]["LP3"])
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_PRE_FAIL_P3_DAYMET_VIC_GAGE"]  <- 1/sapply(IDsToAnalyzeVIC, function(ID) pExcMaxPreFailGage[[ID]]["P3"])

savefile <- paste(gsub("-","",Sys.Date()),"df.Fail.NBI.Gage_With_T_Max_Daymet_VICgFixed.RData", sep="_")
save(df.Fail.NBI.Gage, rowsToView,IDsToView, file = file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

View(df.Fail.NBI.Gage[rowsToAnalyzeVIC,c("ID","FAIL_CAUS","T_FAIL_D_HECP_USGS","T_FAIL_I_HECP_USGS","T_FAIL_GEV_DAYMET_VIC_GAGE","T_FAIL_LP3_DAYMET_VIC_GAGE","T_FAIL_P3_DAYMET_VIC_GAGE",
                                         "T_FAIL_GEV_PM2_DAYMET_VIC_GAGE","T_FAIL_LP3_PM2_DAYMET_VIC_GAGE","T_FAIL_P3_PM2_DAYMET_VIC_GAGE")])
View(df.Fail.NBI.Gage[rowsToAnalyzeVIC,c("ID","STAID","FAIL_CAUS","Q_FAIL_D_USGS","T_FAIL_D_HECP_USGS","Q_FAIL_I_USGS","T_FAIL_I_HECP_USGS","Q_FAIL_DVICG","Q_FAILPM2_DVICG",
                                         "DATE_FAILPM2_DVICG","Q_FAILYRMAX_DVICG",
                                         "T_FAIL_GEV_DAYMET_VIC_GAGE","T_FAIL_LP3_DAYMET_VIC_GAGE","T_FAIL_P3_DAYMET_VIC_GAGE","Q_MAX_D_USGS","Q_MAX_DVICG",
                                         "Q_MAXPREFAIL_D_USGS","Q_MAXPREFAIL_DVICG","DRAIN_SQKM")])

# GAGE (HEC-SSP/BULLETIN 17B)----------
pExcGage        <- rep(NA_real_,nIDsVIC)
pExcGagePMn     <- rep(NA_real_,nIDsVIC)
pExcGagePreFail <- rep(NA_real_,nIDsVIC)
pExcGageMax     <- rep(NA_real_,nIDsVIC)
pExcGageMaxYr   <- rep(NA_real_,nIDsVIC)

Freq       <- sapply(HEC_PFA_VICG,"[[","FREQ", simplify = FALSE, USE.NAMES = TRUE) 
Freq       <- sapply(names(Freq), function(i) 0.01*Freq[[i]], simplify = FALSE, USE.NAMES = TRUE)
FlowAtFreq <- sapply(HEC_PFA_VICG,"[[","FLOW.Computed.Curve", simplify = FALSE, USE.NAMES = TRUE)
rm(HEC_PFA_VICG)

for (i in 1:nIDsVIC){
  STAID    <- df.Fail.NBI.Gage[rowsToAnalyzeVIC[i], "STAID"]
  log_flow <- log(FlowAtFreq[[STAID]])
  log_freq <- log(Freq[[STAID]])
  log_freq <- log_freq[!is.na(log_flow)]
  log_flow <- log_flow[!is.na(log_flow)]
  
  log_Val     <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_FAIL_DVICG"])
  pExcGage[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val        <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_FAILPM2_DVICG"])
  pExcGagePMn[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val        <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_MAX_DVICG"])
  pExcGageMax[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val            <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_MAXPREFAIL_DVICG"])
  pExcGagePreFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val            <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_FAILYRMAX_DVICG"])
  pExcGageMaxYr[i] <-exp(approxExtrap(log_flow,log_freq,log_Val)$y)
}


# Write exceedence and return period values to dataframe
df.Fail.NBI.Gage$T_FAIL_HEC_DVICG  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_HEC_DVICG"]  <- 1/pExcGage
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_FAIL_HEC_DVICG < 1 & !is.na(df.Fail.NBI.Gage$T_FAIL_HEC_DVICG),"T_FAIL_HEC_DVICG"] <- 1
df.Fail.NBI.Gage$T_FAILPM2_HEC_DVICG  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILPM2_HEC_DVICG"]  <- 1/pExcGagePMn
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_FAILPM2_HEC_DVICG < 1 & !is.na(df.Fail.NBI.Gage$T_FAILPM2_HEC_DVICG),"T_FAILPM2_HEC_DVICG"] <- 1
df.Fail.NBI.Gage$T_MAXPREFAIL_HEC_DVICG  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAXPREFAIL_HEC_DVICG"]  <- 1/pExcGagePreFail
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_MAXPREFAIL_HEC_DVICG < 1 & !is.na(df.Fail.NBI.Gage$T_MAXPREFAIL_HEC_DVICG),"T_MAXPREFAIL_HEC_DVICG"] <- 1
df.Fail.NBI.Gage$T_MAX_HEC_DVICG  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_HEC_DVICG"]  <- 1/pExcGageMax
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_MAX_HEC_DVICG < 1 & !is.na(df.Fail.NBI.Gage$T_MAX_HEC_DVICG),"T_MAX_HEC_DVICG"] <- 1
df.Fail.NBI.Gage$T_FAILYRMAX_HEC_DVICG  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILYRMAX_HEC_DVICG"]  <- 1/pExcGageMaxYr
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_FAILYRMAX_HEC_DVICG < 1 & !is.na(df.Fail.NBI.Gage$T_FAILYRMAX_HEC_DVICG),"T_FAILYRMAX_HEC_DVICG"] <- 1
df.Fail.NBI.Gage$T_MAXPREFAILPM2_HEC_DVICG <- df.Fail.NBI.Gage$T_MAX_PRE_FAIL_HEC_DAYMET_VIC_GAGE
rowsToFixPreFailPM2 <- which(df.Fail.NBI.Gage$T_MAXPREFAILPM2_HEC_DVICG < df.Fail.NBI.Gage$T_FAILPM2_HEC_DVICG & !is.na(df.Fail.NBI.Gage$T_MAXPREFAILPM2_HEC_DVICG))
df.Fail.NBI.Gage[rowsToFixPreFailPM2,"T_MAXPREFAILPM2_HEC_DVICG"] <- df.Fail.NBI.Gage[rowsToFixPreFailPM2,"T_FAILPM2_HEC_DVICG"]

rm(list=ls(pattern="pExc"))
rm(list=ls(pattern="log_"))
rm(list = ls(pattern ="Freq"))
rm(i,IDsToAnalyzeVIC,nIDsVIC,rowsToAnalyzeVIC,STAID,STAIDtoAnalyzeVIC,ID,ls.GageRouted)

savefile <- paste(gsub("-","",Sys.Date()),"df.Fail.NBI.Gage_VICG_T_HEC_Fixed.RData", sep="_")
save(df.Fail.NBI.Gage, file = file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

## BULLETIN 17B ANALYSIS USING PEAKFQ --------------------------------------------------------------
# Gage data
Freq       <- sapply(PFA_VICg,"[[","ANN_EXC_PROB", simplify = FALSE, USE.NAMES = TRUE) 
FlowAtFreq <- sapply(PFA_VICg,"[[","BULL17B_EST", simplify = FALSE, USE.NAMES = TRUE)

rm(PFA_VICg)

ExceedProbMax        <- rep(NA_real_,nIDsVIC)
ExceedProbMaxPreFail <- rep(NA_real_,nIDsVIC)
ExceedProbFail       <- rep(NA_real_,nIDsVIC)
ExceedProbFailPM2     <- rep(NA_real_,nIDsVIC)
ExceedProbFailYr     <- rep(NA_real_,nIDsVIC)
T500index  <- sapply(names(Freq), function(i) which(Freq[[i]]==0.002))
T100index  <- sapply(names(Freq), function(i) which(Freq[[i]]==0.01))
Q500       <- sapply(names(FlowAtFreq), function(i) FlowAtFreq[[i]][T500index[i]])
Q100       <- sapply(names(FlowAtFreq), function(i) FlowAtFreq[[i]][T100index[i]])

for (i in 1:nIDsVIC){
  STAID    <- STAIDtoAnalyzeVIC[i]
  log_flow <- log(FlowAtFreq[[STAID]])
  log_freq <- log(Freq[[STAID]])
  log_freq <- log_freq[!is.na(log_flow)]
  log_flow <- log_flow[!is.na(log_flow)]
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_FAIL_DVICG"])
  ExceedProbFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_FAILPM2_DVICG"])
  ExceedProbFailPM2[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_MAX_DVICG"])
  ExceedProbMax[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_MAXPREFAIL_DVICG"])
  ExceedProbMaxPreFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_FAILYRMAX_DVICG"])
  ExceedProbFailYr[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
}

# Write exceedence and return period values to dataframe
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_PKFQD_DVICG"]         <- 1/ExceedProbFail
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILPM2_PKFQD_DVICG"]     <- 1/ExceedProbFailPM2
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILYRMAX_PKFQD_DVICG"]  <- 1/ExceedProbFailYr
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_PKFQD_DVICG"]          <- 1/ExceedProbMax
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAXPREFAIL_PKFQD_DVICG"] <- 1/ExceedProbMaxPreFail
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q100_PKFQD_DVICG"]           <- Q100[STAIDtoAnalyzeVIC]
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q500_PKFQD_DVICG"]           <- Q500[STAIDtoAnalyzeVIC]

# Bridge data
Freq       <- sapply(PFA_VICb,"[[","ANN_EXC_PROB", simplify = FALSE, USE.NAMES = TRUE) 
FlowAtFreq <- sapply(PFA_VICb,"[[","BULL17B_EST", simplify = FALSE, USE.NAMES = TRUE)

rm(PFA_VICb)

ExceedProbMax        <- rep(NA_real_,nIDsVIC)
ExceedProbMaxPreFail <- rep(NA_real_,nIDsVIC)
ExceedProbFail       <- rep(NA_real_,nIDsVIC)
ExceedProbFailPM2     <- rep(NA_real_,nIDsVIC)
ExceedProbFailYr     <- rep(NA_real_,nIDsVIC)
T500index  <- sapply(names(Freq), function(i) which(Freq[[i]]==0.002))
T100index  <- sapply(names(Freq), function(i) which(Freq[[i]]==0.01))
Q500       <- sapply(names(FlowAtFreq), function(i) FlowAtFreq[[i]][T500index[i]])
Q100       <- sapply(names(FlowAtFreq), function(i) FlowAtFreq[[i]][T100index[i]])

for (i in 1:nIDsVIC){
  ID       <- IDsToAnalyzeVIC[i]
  if (ID=="1164") next()
  STAID    <- STAIDtoAnalyzeVIC[i]
  log_flow <- log(FlowAtFreq[[ID]])
  log_freq <- log(Freq[[ID]])
  log_freq <- log_freq[!is.na(log_flow)]
  log_flow <- log_flow[!is.na(log_flow)]
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_FAIL_DVICB"])
  ExceedProbFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_FAILPM2_DVICB"])
  ExceedProbFailPM2[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_MAX_DVICB"])
  ExceedProbMax[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_MAXPREFAIL_DVICB"])
  ExceedProbMaxPreFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_FAILYRMAX_DVICB"])
  ExceedProbFailYr[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
}

# Write exceedence and return period values to dataframe
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_PKFQD_DVICB"]         <- 1/ExceedProbFail
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILPM2_PKFQD_DVICB"]     <- 1/ExceedProbFailPM2
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILYRMAX_PKFQD_DVICB"]  <- 1/ExceedProbFailYr
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_PKFQD_DVICB"]          <- 1/ExceedProbMax
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAXPREFAIL_PKFQD_DVICB"] <- 1/ExceedProbMaxPreFail
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q100_PKFQD_DVICB"]           <- Q100[STAIDtoAnalyzeVIC]
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q500_PKFQD_DVICB"]           <- Q500[STAIDtoAnalyzeVIC]

savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage, rowsToAnalyzeVIC, IDsToView, file=file.path(dirs$dfActive,savefile))
rm(savefile)

### FAILURE RETURN PERIOD FROM PARTIAL DURATION DATA ############# -----------
load(file.path(dirs$DataDirAnalysis,"20151203_ecdfBrigesDailyData.RData"))
dates <- seq.Date(as.Date("1980-01-01"),as.Date("2013-12-31"),by = "day")
p.partial    <- list()
p.partialPM2 <- list()
p.partialMaxYr <- list()
for (ID in IDsToAnalyzeVIC){
  if (ID == "1164") {
    p.partial[[ID]] <- NA
    p.partialPM2[[ID]] <- NA
    p.partialMaxYr[[ID]] <- NA
    next
    }
  p.partial[[ID]] <- 1 - ecdfBridgeDaily[[ID]](df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAIL_DVICB"])
  p.partialPM2[[ID]] <- 1 - ecdfBridgeDaily[[ID]](df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAILPM2_DVICB"])
  p.partialMaxYr[[ID]] <- 1 - ecdfBridgeDaily[[ID]](df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAILYRMAX_DVICB"])
  if (p.partialPM2[[ID]]==0){
    p.partialPM2[[ID]] <- 1/length(dates)
  }
  if (p.partialMaxYr[[ID]]==0){
    p.partialMaxYr[[ID]] <- 1/length(dates)
  }
}

p.partial <- unlist(p.partial)
p.partialPM2 <- unlist(p.partialPM2)
p.partialMaxYr <- unlist(p.partialMaxYr)

df.Fail.NBI.Gage$T_FAIL_PARTDUR_DVICB <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_PARTDUR_DVICB"] <- 1/p.partial/365.25
df.Fail.NBI.Gage[rowsToAnalyzeVIC[df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_PARTDUR_DVICB"]<1 & !is.na(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_PARTDUR_DVICB"])],"T_FAIL_PARTDUR_DVICB"] <- 1
df.Fail.NBI.Gage$T_FAILPM2_PARTDUR_DVICB <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILPM2_PARTDUR_DVICB"] <- 1/p.partialPM2/365.25
df.Fail.NBI.Gage[rowsToAnalyzeVIC[df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILPM2_PARTDUR_DVICB"]<1 & !is.na(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILPM2_PARTDUR_DVICB"])],"T_FAILPM2_PARTDUR_DVICB"] <- 1
df.Fail.NBI.Gage$T_FAILYRMAX_PARTDUR_DVICB <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILYRMAX_PARTDUR_DVICB"] <- 1/p.partialMaxYr/365.25
df.Fail.NBI.Gage[rowsToAnalyzeVIC[df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILYRMAX_PARTDUR_DVICB"]<1 & !is.na(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILYRMAX_PARTDUR_DVICB"])],"T_FAILYRMAX_PARTDUR_DVICB"] <- 1
rm(ecdfBridgeDaily)

# GAGES
load(file.path(dirs$DataDirAnalysis,"20151203_ecdfGagesDailyData.RData"))
p.partial    <- list()
p.partialPM2 <- list()
p.partialMaxYr <- list()
for (i in rowsToAnalyzeVIC){
  ID                 <- as.character(df.Fail.NBI.Gage[i,"ID"])
  STAID              <- df.Fail.NBI.Gage[i,"STAID"]
  p.partial[[ID]]    <- 1 - ecdfGageDaily[[STAID]](df.Fail.NBI.Gage[i,"Q_FAIL_DVICG"])
  p.partialPM2[[ID]] <- 1 - ecdfGageDaily[[STAID]](df.Fail.NBI.Gage[i,"Q_FAILPM2_DVICG"])
  p.partialMaxYr[[ID]] <- 1 - ecdfGageDaily[[STAID]](df.Fail.NBI.Gage[i,"Q_FAILYRMAX_DVICG"])
  if (p.partialPM2[[ID]]==0){
    p.partialPM2[[ID]] <- 1/length(dates)
  }
  if (p.partialMaxYr[[ID]]==0){
    p.partialMaxYr[[ID]] <- 1/length(dates)
  }
}
p.partial <- unlist(p.partial)
p.partialPM2 <- unlist(p.partialPM2)
p.partialMaxYr <- unlist(p.partialMaxYr)

df.Fail.NBI.Gage$T_FAIL_PARTDUR_DVICG <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_PARTDUR_DVICG"] <- 1/p.partial/365.25
df.Fail.NBI.Gage[rowsToAnalyzeVIC[df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_PARTDUR_DVICG"]<1],"T_FAIL_PARTDUR_DVICG"] <- 1
df.Fail.NBI.Gage$T_FAILPM2_PARTDUR_DVICG <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILPM2_PARTDUR_DVICG"] <- 1/p.partialPM2 /365.25
df.Fail.NBI.Gage[rowsToAnalyzeVIC[df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILPM2_PARTDUR_DVICG"]<1],"T_FAILPM2_PARTDUR_DVICG"] <- 1
df.Fail.NBI.Gage$T_FAILYRMAX_PARTDUR_DVICG <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILYRMAX_PARTDUR_DVICG"] <- 1/p.partialMaxYr/365.25
df.Fail.NBI.Gage[rowsToAnalyzeVIC[df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILYRMAX_PARTDUR_DVICG"]<1] ,"T_FAILYRMAX_PARTDUR_DVICG"] <- 1
rm(ecdfGageDaily)

savefile <- paste(gsub("-","",Sys.Date()),"df.Fail.NBI.Gage_VICpartialsUpdated.RData", sep="_")
save(df.Fail.NBI.Gage, file = file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)



------------
  ## BULLETIN 17B ANALYSIS USING PEAKFQ for VIC RAPID reanalysis--------------------------------------------------------------
# Gage data
load(file.path(dirsGit$Data,"20180621_PeakFQ_PFA_VICRapidg_UserRegSkews.RData"))
load(file.path(dirsGit$Data,"20180620_GageRapidRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirsGit$Data,"20180620_ListReroutedFlows.RData"))
Dates <- as.Date(paste(ls.GageRapidRouted[[1]]$YEAR,ls.GageRapidRouted[[1]]$MONTH,ls.GageRapidRouted[[1]]$DAY,sep="-"))
for (i in names(ls.GageRapidRouted)){
  ls.GageRapidRouted[[i]]$Date <- Dates
}

savefile <- paste(gsub("-","",Sys.Date()),"VICgRapidRouted.RData", sep="_")
save(ls.GageRapidRouted, rowsToView, IDsToView, file=file.path(dirsGit$Data,savefile))
rm(savefile)

for (i in 1:nIDsVIC){
  ID <- IDsToAnalyzeVIC[i]
  STAID <-STAIDtoAnalyzeVIC[i]
  FailDateIndex[ID]      <- which.min(abs( ls.GageRapidRouted[[STAID]]$Date - as.Date(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"DATE_FAIL_EST_USGS"]) )) # checks out
  FailDatesIndexPMn      <- c(FailDateIndex[ID]-n,FailDateIndex[ID]+n)
  FailYearIndex[[ID]][1] <- which.min(abs( ls.GageRapidRouted[[STAID]]$Date - 
                                             as.Date(paste(substr(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"DATE_FAIL_EST_USGS"],1,4),"01","01",sep="-")) )) # checks out
  FailYearIndex[[ID]][2] <- which.min(abs( ls.GageRapidRouted[[STAID]]$Date - 
                                             as.Date(paste(substr(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"DATE_FAIL_EST_USGS"],1,4),"12","31",sep="-")) )) # checks out
  YrBltIndex[ID]         <- ifelse(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"YR_BLT_EST"] <= ls.GageRapidRouted[[STAID]]$Date[1] |
                                     is.na(as.Date(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"YR_BLT_EST"],"1970-01-01")),
                                   1,
                                   which.min(abs(as.numeric(ls.GageRapidRouted[[STAID]]$Date - as.Date(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"YR_BLT_EST"],"1970-01-01"))))
  )
  FailDatesIndexPMn      <- c(FailDateIndex[ID]-n,FailDateIndex[ID]+n)
  print(paste("For ID ",ID," Fail date est:",df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"DATE_FAIL_EST_USGS"],", From Daymet-VIC:",ls.GageRapidRouted[[STAID]]$Date[FailDateIndex[ID]]))
  
  QfailGage[ID]  <- ls.GageRapidRouted[[STAID]]$RUNOFF_CFS[FailDateIndex[ID]]
  
  FailDateIndexGagePMn[ID]   <- which.max(ls.GageRapidRouted[[STAID]]$RUNOFF_CFS[FailDatesIndexPMn[1]:FailDatesIndexPMn[2]]) + FailDatesIndexPMn[1] - 1
  QfailGagePMn[ID]     <- ls.GageRapidRouted[[STAID]]$RUNOFF_CFS[FailDateIndexGagePMn[ID]]
  FailDateGagePMn[ID]  <- ls.GageRapidRouted[[STAID]]$Date[FailDateIndexGagePMn[ID]]

  MaxIndexGage[ID] <- which.max(ls.GageRapidRouted[[STAID]]$RUNOFF_CFS)
  QmaxGage[ID]     <- ls.GageRapidRouted[[STAID]]$RUNOFF_CFS[MaxIndexGage[ID]]
  QmaxDateGage[ID] <- ls.GageRapidRouted[[STAID]]$Date[MaxIndexGage[ID]]
  
  MaxFailYearIndexGage[ID] <- which.max(ls.GageRapidRouted[[STAID]]$RUNOFF_CFS[c(FailYearIndex[[ID]][1]:FailYearIndex[[ID]][2])])
  QmaxFailYearGage[ID]     <- ls.GageRapidRouted[[STAID]]$RUNOFF_CFS[MaxFailYearIndexGage[ID] + FailYearIndex[[ID]][1] - 1]
  QmaxFailYearDateGage[ID] <- ls.GageRapidRouted[[STAID]]$Date[MaxFailYearIndexGage[ID] + FailYearIndex[[ID]][1] - 1]
  
  MaxPreFailIndexGage[ID]  <- which.max(ls.GageRapidRouted[[STAID]]$RUNOFF_CFS[c(YrBltIndex[ID]:FailDateIndex[ID])])
  QmaxPreFailGage[ID]      <- ls.GageRapidRouted[[STAID]]$RUNOFF_CFS[MaxPreFailIndexGage[ID] + YrBltIndex[ID] - 1]
  QmaxPreFailDateGage[ID]  <- ls.GageRapidRouted[[STAID]]$Date[MaxPreFailIndexGage[ID] + YrBltIndex[ID] - 1]
}
df.Fail.NBI.Gage$Q_FAIL_DVICG_RAPID <- NA_real_
df.Fail.NBI.Gage$Q_MAX_DVICG_RAPID <- NA_real_
df.Fail.NBI.Gage$Q_MAXPREFAIL_DVICG_RAPID <- NA_real_
df.Fail.NBI.Gage$Q_FAILYRMAX_DVICG_RAPID <- NA_real_
df.Fail.NBI.Gage$DATE_MAX_DVICG_RAPID <- NA_real_
df.Fail.NBI.Gage$DATE_MAXPREFAIL_DVICG_RAPID <- NA_real_
df.Fail.NBI.Gage$DATE_FAILYRMAX_DVICG_RAPID <- NA_real_
df.Fail.NBI.Gage$Q_FAILPM2_DVICG_RAPID <- NA_real_
df.Fail.NBI.Gage$DATE_FAILPM2_DVICG_RAPID <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q_FAIL_DVICG_RAPID"]         <- QfailGage 
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q_MAX_DVICG_RAPID"]          <- QmaxGage 
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q_MAXPREFAIL_DVICG_RAPID"] <- QmaxPreFailGage
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q_FAILYRMAX_DVICG_RAPID"]  <- QmaxFailYearGage
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DATE_MAX_DVICG_RAPID"]          <- QmaxDateGage
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DATE_MAXPREFAIL_DVICG_RAPID"] <- QmaxPreFailDateGage
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DATE_FAILYRMAX_DVICG_RAPID"]  <- QmaxFailYearDateGage
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q_FAILPM2_DVICG_RAPID"]         <- QfailGagePMn
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DATE_FAILPM2_DVICG_RAPID"]         <-FailDateGagePMn

Freq       <- sapply(PFA_VICRapidg,"[[","ANN_EXC_PROB", simplify = FALSE, USE.NAMES = TRUE) 
FlowAtFreq <- sapply(PFA_VICRapidg,"[[","BULL17B_EST", simplify = FALSE, USE.NAMES = TRUE)

rm(PFA_VICRapidg)

ExceedProbMax        <- rep(NA_real_,nIDsVIC)
ExceedProbMaxPreFail <- rep(NA_real_,nIDsVIC)
ExceedProbFail       <- rep(NA_real_,nIDsVIC)
ExceedProbFailPM2     <- rep(NA_real_,nIDsVIC)
ExceedProbFailYr     <- rep(NA_real_,nIDsVIC)
T500index  <- sapply(names(Freq), function(i) which(Freq[[i]]==0.002))
T100index  <- sapply(names(Freq), function(i) which(Freq[[i]]==0.01))
Q500       <- sapply(names(FlowAtFreq), function(i) FlowAtFreq[[i]][T500index[i]])
Q100       <- sapply(names(FlowAtFreq), function(i) FlowAtFreq[[i]][T100index[i]])

for (i in 1:nIDsVIC){
  STAID    <- STAIDtoAnalyzeVIC[i]
  log_flow <- log(FlowAtFreq[[STAID]])
  log_freq <- log(Freq[[STAID]])
  log_freq <- log_freq[!is.na(log_flow)]
  log_flow <- log_flow[!is.na(log_flow)]
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_FAIL_DVICG_RAPID"])
  ExceedProbFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_FAILPM2_DVICG_RAPID"])
  ExceedProbFailPM2[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_MAX_DVICG_RAPID"])
  ExceedProbMax[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_MAXPREFAIL_DVICG_RAPID"])
  ExceedProbMaxPreFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToAnalyzeVIC[i],"Q_FAILYRMAX_DVICG_RAPID"])
  ExceedProbFailYr[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
}

# Write exceedence and return period values to dataframe
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_PKFQD_DVICG_RAPID"]         <- 1/ExceedProbFail
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILPM2_PKFQD_DVICG_RAPID"]     <- 1/ExceedProbFailPM2
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILYRMAX_PKFQD_DVICG_RAPID"]  <- 1/ExceedProbFailYr
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_PKFQD_DVICG_RAPID"]          <- 1/ExceedProbMax
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAXPREFAIL_PKFQD_DVICG_RAPID"] <- 1/ExceedProbMaxPreFail
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q100_PKFQD_DVICG_RAPID"]           <- Q100[STAIDtoAnalyzeVIC]
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"Q500_PKFQD_DVICG_RAPID"]           <- Q500[STAIDtoAnalyzeVIC]

# (visual check)
Qs <- colnames(df.Fail.NBI.Gage)[grepl("Q_",colnames(df.Fail.NBI.Gage))]
Qs <- Qs[!grepl("NLDAS",Qs)]
Qs <- Qs[!grepl("VICB",Qs)]
View(df.Fail.NBI.Gage[rowsToAnalyzeVIC,Qs])
Ts <- colnames(df.Fail.NBI.Gage)[grepl("\\<T_",colnames(df.Fail.NBI.Gage))]
Ts <- Ts[!grepl("NLDAS",Ts)]
Ts <- Ts[!grepl("VICB",Ts)]
Ts <- Ts[!grepl("HEC",Ts)]
View(df.Fail.NBI.Gage[rowsToAnalyzeVIC,Ts])

  savefile <- paste(gsub("-","",Sys.Date()),"df.Fail.NBI.Gage_NEWVICG_OLDREGSKEWS.RData", sep="_")
  save(df.Fail.NBI.Gage, rowsToView, IDsToView, file=file.path(dirs$dfActive,savefile))
  rm(savefile)

#   savefile <- "df.Fail.NBI.Gage.Active.RData"
# save(df.Fail.NBI.Gage, rowsToView, IDsToView, file=file.path(dirs$dfActive,savefile))
# rm(savefile)

rm(list = ls())
