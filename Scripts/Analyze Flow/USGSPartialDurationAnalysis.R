# Partial Duration Analysis
# 2015-07-16 
# Madeleine Flint
# Uses ls.PartialDur and daily mean data on known/estimated failure date
# to estimate the % time the flow exceeded that of the failure date
library(Hmisc)

loadfile <- file.path(dirs$DataDirFrequent,"20151210_PartialDurationData.RData")
load(loadfile)
loadfile <- file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData")
load(loadfile)
rm(loadfile)

df.Fail.NBI.Gage$EXCEED_PARTIAL_USGS <- rep(NA_real_,nrow(df.Fail.NBI.Gage))
gageIDs     <- names(ls.PartialDur)
gage_sites  <- df.Fail.NBI.Gage[rowsToView,"STAID"]
PartialFlow <- lapply(ls.PartialDur,"[[",2)
PartialExc  <- unlist(ls.PartialDur[[1]][3])
PartialExc    <- 1 - PartialExc

ExceedDurFail <- numeric(length(rowsToView))
ExceedDurFailip <- numeric(length(rowsToView))
HasInstPeak <- !is.na(df.Fail.NBI.Gage[rowsToView,"Q_FAIL_IP_USGS"])
for (i in 1:length(rowsToView)){
  PartialExcSite <- c(1 - 1e-05, PartialExc[2:8],  1/PartialDurN[[gage_sites[i]]])
  log_flow <- log(PartialFlow[[gage_sites[i]]])
  if (any(-1*log_flow==Inf)) log_flow[-1*log_flow==Inf] <- NA_real_
  log_freq <- log(PartialExcSite)
  log_freq <- log_freq[!is.na(log_flow)]
  log_flow <- log_flow[!is.na(log_flow)]
  log_Fail <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_FAIL_D_USGS"])  
  ExceedDurFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Fail)$y)
  if (HasInstPeak[i]){
    log_Fail <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_FAIL_IP_USGS"])  
    ExceedDurFailip[i] <- exp(approxExtrap(log_flow,log_freq,log_Fail)$y)
  }
  else ExceedDurFailip[i] <- NA
}
df.Fail.NBI.Gage[rowsToView,"EXCEED_PARTIAL_USGS_INTERP"] <- ExceedDurFail
df.Fail.NBI.Gage$T_FAIL_D_PARTDUR_USGS <- 1/df.Fail.NBI.Gage$EXCEED_PARTIAL_USGS_INTERP/365.25
df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_PARTDUR_USGS"][!is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_PARTDUR_USGS"]) & df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_PARTDUR_USGS"] <1] <- 1
df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_PARTDUR_USGS"] <- 1/ExceedDurFailip/365.25
df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_PARTDUR_USGS"][!is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_PARTDUR_USGS"]) & df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_PARTDUR_USGS"] <1] <- 1

savefile <- file.path(dirs$DataDirAnalysis,
                      paste(gsub("-","",Sys.Date()),
                            "FailNBIGageDataFrameWithPartialDuration.RData",sep="_"))
save(df.Fail.NBI.Gage,file=savefile)
rm(savefile)
savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage, rowsToView,IDsToView,file = file.path(dirs$dfActive,savefile))
rm(savefile)

