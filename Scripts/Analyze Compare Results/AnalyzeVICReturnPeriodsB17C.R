# 2015-08-17 Determine failure and max events, and estimate return period of failure and max events 
# from MLE parameters of Daymet-VIC distributions
# 215-10-07 from Bulletin 17B analysis using HEC-SSP

require(stats)
require(nsRFA)
require(ggplot2)
require(Hmisc)

# Load and source ------------------
source(file.path(dirsGit$ScriptsDir,"hydrologyDistrAssess.R"))

load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))

load(file.path(dirs$DataDirAnalysis,"20150909_Gage_Daymet_VIC_Annual_Max_P3_LP3_GEV_Param.RData"))
load(file.path(dirs$DataDirAnalysis,"20150813_Bridge_Daymet_VIC_Annual_Max_P3_LP3_GEV_Param.RData"))
load(file.path(dirs$DataDirFrequent,"20151203_GageRoutingVICDaymet.RData"))
load(file.path(dirs$DataDirFrequent,"20151203_BridgeRoutingVICDaymet.RData"))
load(file.path(dirs$DataDirAnalysis,"20151203_BridgeRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirs$DataDirAnalysis,"20151203_GageRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirs$DataDirFrequent,"20160107_HEC_PFA_VICG.RData"))
load(file.path(dirs$DataDirFrequent,"20160107_PeakFQ_PFA_VICg_UserRegSkews.RData"))
load(file.path(dirs$DataDirFrequent,"20160107_PeakFQ_PFA_VICb_UserRegSkews.RData"))



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

savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage, rowsToView, IDsToView, file=file.path(dirs$dfActive,savefile))
rm(savefile)

rm(list = ls())
