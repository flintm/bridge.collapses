# 2015-09-09 Estimate return period of failure event from MLE parameters of NLDAS distributions

require(stats)
require(nsRFA)
require(ggplot2)
require(Hmisc)

# Load and source ------------------
source(file.path(dirsGit$Scripts,"hydrologyDistrAssess.R"))

load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirs$DataDirAnalysis,"20151217_NLDASGageDailyMean.RData"))
load(file.path(dirs$DataDirAnalysis,"20151217_NLDASBridgeDailyMean.RData"))
load(file.path(dirs$DataDirFrequent,"20160107_PeakFQ_PFA_NLDASg_UserRegSkews.RData"))
load(file.path(dirs$DataDirFrequent,"20160107_PeakFQ_PFA_NLDASb_UserRegSkews.RData"))

# ESTIMATE RETURN PERIOD OF FAILURE FLOW -----------------------
rowsToAnalyzeNLDAS <- rowsToView[df.Fail.NBI.Gage[rowsToView,"YR_FAIL"] > as.Date("1979-01-01")]
IDsToAnalyzeNLDAS  <- as.character(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"ID"])
nNLDAS             <- length(rowsToAnalyzeNLDAS)
dates              <- seq.Date(as.Date("1979-01-01"),as.Date("2014-12-31"),by="1 day")
years              <- as.character(1979:2014)

# GAGES - BULLETIN 17B  --------
pExcGagePMn     <- rep(NA_real_,nNLDAS)
pExcGagePreFail <- rep(NA_real_,nNLDAS)
pExcGageMax     <- rep(NA_real_,nNLDAS)
pExcGageMaxYr   <- rep(NA_real_,nNLDAS)

Freq       <- sapply(PFA_NLDASg,"[[","ANN_EXC_PROB", simplify = FALSE, USE.NAMES = TRUE) 
FlowAtFreq <- sapply(PFA_NLDASg,"[[","BULL17B_EST", simplify = FALSE, USE.NAMES = TRUE)
rm(PFA_NLDASg)

DrainAreaG <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DRAIN_SQKM"]*1000^2 # in sq-m
kgCfs     <- 0.035315*(1/3600) 

for (i in 1:nNLDAS){
  STAID    <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[i], "STAID"]
  log_flow <- log(FlowAtFreq[[STAID]])
  log_freq <- log(Freq[[STAID]])
  log_freq <- log_freq[!is.na(log_flow)]
  log_flow <- log_flow[!is.na(log_flow)]
  
  log_Val        <- log(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[i],"Q_FAILPM2_NLDASG"]*DrainAreaG[i]*kgCfs)
  pExcGagePMn[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val        <- log(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[i],"Q_MAX_NLDASG"]*DrainAreaG[i]*kgCfs)
  pExcGageMax[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val            <- log(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[i],"Q_MAXPREFAIL_NLDASG"]*DrainAreaG[i]*kgCfs)
  pExcGagePreFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val            <- log(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[i],"Q_FAILYRMAX_NLDASG"]*DrainAreaG[i]*kgCfs)
  pExcGageMaxYr[i] <-exp(approxExtrap(log_flow,log_freq,log_Val)$y)
}


# Write exceedence and return period values to dataframe
df.Fail.NBI.Gage$T_FAILPM2_PKFQD_NLDASG  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_FAILPM2_PKFQD_NLDASG"]  <- 1/pExcGagePMn
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_FAILPM2_PKFQD_NLDASG < 1 & !is.na(df.Fail.NBI.Gage$T_FAILPM2_PKFQD_NLDASG),"T_FAILPM2_PKFQD_NLDASG"] <- 1
df.Fail.NBI.Gage$T_MAXPREFAIL_PKFQD_NLDASG  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_MAXPREFAIL_PKFQD_NLDASG"]  <- 1/pExcGagePreFail
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_MAXPREFAIL_PKFQD_NLDASG < 1 & !is.na(df.Fail.NBI.Gage$T_MAXPREFAIL_PKFQD_NLDASG),"T_MAXPREFAIL_PKFQD_NLDASG"] <- 1
df.Fail.NBI.Gage$T_MAX_PKFQD_NLDASG  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_MAX_PKFQD_NLDASG"]  <- 1/pExcGageMax
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_MAX_PKFQD_NLDASG < 1 & !is.na(df.Fail.NBI.Gage$T_MAX_PKFQD_NLDASG),"T_MAX_PKFQD_NLDASG"] <- 1
df.Fail.NBI.Gage$T_FAILYRMAX_PKFQD_NLDASG  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_FAILYRMAX_PKFQD_NLDASG"]  <- 1/pExcGageMaxYr
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_FAILYRMAX_PKFQD_NLDASG < 1 & !is.na(df.Fail.NBI.Gage$T_FAILYRMAX_PKFQD_NLDASG),"T_FAILYRMAX_PKFQD_NLDASG"] <- 1

# BRIDGES - BULLETIN 17B  --------
pExcBridgePMn     <- rep(NA_real_,nNLDAS)
pExcBridgePreFail <- rep(NA_real_,nNLDAS)
pExcBridgeMax     <- rep(NA_real_,nNLDAS)
pExcBridgeMaxYr   <- rep(NA_real_,nNLDAS)

Freq       <- sapply(PFA_NLDASb,"[[","ANN_EXC_PROB", simplify = FALSE, USE.NAMES = TRUE) 
FlowAtFreq <- sapply(PFA_NLDASb,"[[","BULL17B_EST", simplify = FALSE, USE.NAMES = TRUE)
rm(PFA_NLDASb)

DrainAreaB <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"]*1000^2 # in sq-m
kgCfs      <- 0.035315*(1/3600) 

for (i in 1:nNLDAS){
  ID       <- IDsToAnalyzeNLDAS[i]
  STAID    <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[i], "STAID"]
  log_flow <- log(FlowAtFreq[[ID]])
  log_freq <- log(Freq[[ID]])
  log_freq <- log_freq[!is.na(log_flow)]
  log_flow <- log_flow[!is.na(log_flow)]
  
  log_Val        <- log(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[i],"Q_FAILPM2_NLDASB"]*DrainAreaB[i]*kgCfs)
  pExcBridgePMn[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val        <- log(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[i],"Q_MAX_NLDASB"]*DrainAreaB[i]*kgCfs)
  pExcBridgeMax[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val            <- log(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[i],"Q_MAXPREFAIL_NLDASB"]*DrainAreaB[i]*kgCfs)
  pExcBridgePreFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val            <- log(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[i],"Q_FAILYRMAX_NLDASB"]*DrainAreaB[i]*kgCfs)
  pExcBridgeMaxYr[i] <-exp(approxExtrap(log_flow,log_freq,log_Val)$y)
}


# Write exceedence and return period values to dataframe
df.Fail.NBI.Gage$T_FAILPM2_PKFQD_NLDASB  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_FAILPM2_PKFQD_NLDASB"]  <- 1/pExcBridgePMn
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_FAILPM2_PKFQD_NLDASB < 1 & !is.na(df.Fail.NBI.Gage$T_FAILPM2_PKFQD_NLDASB),"T_FAILPM2_PKFQD_NLDASB"] <- 1
df.Fail.NBI.Gage$T_MAXPREFAIL_PKFQD_NLDASB  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_MAXPREFAIL_PKFQD_NLDASB"]  <- 1/pExcBridgePreFail
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_MAXPREFAIL_PKFQD_NLDASB < 1 & !is.na(df.Fail.NBI.Gage$T_MAXPREFAIL_PKFQD_NLDASB),"T_MAXPREFAIL_PKFQD_NLDASB"] <- 1
df.Fail.NBI.Gage$T_MAX_PKFQD_NLDASB  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_MAX_PKFQD_NLDASB"]  <- 1/pExcBridgeMax
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_MAX_PKFQD_NLDASB < 1 & !is.na(df.Fail.NBI.Gage$T_MAX_PKFQD_NLDASB),"T_MAX_PKFQD_NLDASB"] <- 1
df.Fail.NBI.Gage$T_FAILYRMAX_PKFQD_NLDASB  <- NA_real_
df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_FAILYRMAX_PKFQD_NLDASB"]  <- 1/pExcBridgeMaxYr
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_FAILYRMAX_PKFQD_NLDASB < 1 & !is.na(df.Fail.NBI.Gage$T_FAILYRMAX_PKFQD_NLDASB),"T_FAILYRMAX_PKFQD_NLDASB"] <- 1

savefile <- paste(gsub("-","",Sys.Date()),"df.Fail.NBI.Gage_WithPKFQv3.RData",sep="_")
save(df.Fail.NBI.Gage,rowsToView,IDsToView, file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage, rowsToView, IDsToView, file=file.path(dirsGit$Data,savefile))
rm(savefile)

### FAILURE RETURN PERIOD FROM PARTIAL DURATION DATA #############
load(file.path(dirs$DataDirAnalysis,"20151217_ecdfNLDASDailyData.RData"))

p.partialPM2 <- list()
p.partialMaxYr <- list()
for (ID in IDsToAnalyzeNLDAS){
  p.partialPM2[[ID]] <- 1 - ecdfNLDASb[[ID]](df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAILPM2_NLDASB"])
  p.partialMaxYr[[ID]] <- 1 - ecdfNLDASb[[ID]](df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAILYRMAX_NLDASB"])
  if (p.partialPM2[[ID]]==0){
    p.partialPM2[[ID]] <- 1/length(ls.NLDAS.Bridge[[1]]$Date)
  }
  if (p.partialMaxYr[[ID]]==0){
    p.partialMaxYr[[ID]] <- 1/length(ls.NLDAS.Bridge[[1]]$Date)
  }
}

df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_FAILPM2_PARTDUR_NLDASB"] <- 1/unlist(p.partialPM2)/365.25
df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_FAILPM2_PARTDUR_NLDASB"]<1],"T_FAILPM2_PARTDUR_NLDASB"] <- 1
df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_FAILYRMAX_PARTDUR_NLDASB"] <- 1/unlist(p.partialMaxYr)/365.25
df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_FAILYRMAX_PARTDUR_NLDASB"]<1],"T_FAILYRMAX_PARTDUR_NLDASB"] <- 1
rm(ecdfNLDASb)

# Gage
p.partialPM2 <- list()
p.partialMaxYr <- list()
for (i in 1:nNLDAS){
  ID    <- as.character(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[i],"ID"])
  STAID <- as.character(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[i],"STAID"])
  p.partialPM2[[ID]] <- 1 - ecdfNLDASg[[STAID]](df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAILPM2_NLDASG"])
  p.partialMaxYr[[ID]] <- 1 - ecdfNLDASg[[STAID]](df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAILYRMAX_NLDASG"])
  if (p.partialPM2[[ID]]==0){
    p.partialPM2[[ID]] <- 1/length(ls.NLDAS.Gage[[1]]$Date)
  }
  if (p.partialMaxYr[[ID]]==0){
    p.partialMaxYr[[ID]] <- 1/length(ls.NLDAS.Gage[[1]]$Date)
  }
}

df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_FAILPM2_PARTDUR_NLDASG"] <- 1/unlist(p.partialPM2)/365.25
df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_FAILPM2_PARTDUR_NLDASG"]<1],"T_FAILPM2_PARTDUR_NLDASG"] <- 1
df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_FAILYRMAX_PARTDUR_NLDASG"] <- 1/unlist(p.partialMaxYr)/365.25
df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"T_FAILYRMAX_PARTDUR_NLDASG"]<1],"T_FAILYRMAX_PARTDUR_NLDASG"] <- 1
rm(ecdfNLDASg)

savefile <- paste(gsub("-","",Sys.Date()),"df.Fail.NBI.Gage_T_PartialNLDAS.RData", sep="_")
save(df.Fail.NBI.Gage,rowsToView,IDsToView, file = file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage, rowsToView,IDsToView,file = file.path(dirsGit$Data,savefile))
rm(savefile)
