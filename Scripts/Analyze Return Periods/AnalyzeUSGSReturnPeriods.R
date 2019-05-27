# 2015-04-09: Processing failure return periods for 8 bridges with gage < 0.15 but < 1km away
# 2015-04-13: Now using corrected data for daily mean streamflow
# 2015-05-12: for 25 selected "validation" bridges, using lists of data frames
# 2015-05-20: for 16 validations bridges with NLDAS data available, and with HEC results
# 2015-04-22: adding 9 additional HEC files
# 2015-05-29: adding analysis of instantaneous data for stations where available
# 2015-06-03: calculating return periods of max events
# 2015-07-29: added (1) -1 day for known failure date to determine max
#                   (2) count on number of days recorded in failure year
# 2015-09-04: looking at max pre-fail event using peaks data

load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirs$DataDirFrequent,"20150729_Discharge_39Gages.RData"))
load(file.path(dirs$DataDirFrequent,"20160107_HEC_PFA_41_Bridges.RData"))
load(file.path(dirs$DataDirFrequent,"20160107_HEC_PFA_AnMaxDayMean_36_Bridges.RData"))
load(file.path(dirs$DataDirFrequent,"20160107_PeakFQ_PFA_USGSd_UserRegSkews.RData"))
load(file.path(dirs$DataDirFrequent,"20160107_PeakFQ_PFA_USGSp_UserRegSkews.RData"))
require(Hmisc)
require(nsRFA)

# OBTAIN VECTORS OF FLOW AND DISTRIBUTION INFORMATION
FlowRecords     <- lapply(ls.Discharge.All,"[[","val") 
FlowDates       <- lapply(ls.Discharge.All,"[[","dates")
FlowRecordsInst <- lapply(ls.Discharge.Inst,"[[","val") 
FlowDatesInst   <- lapply(ls.Discharge.Inst,"[[","dates")
FlowPeaks       <- lapply(ls.Discharge.Peaks,"[[","peak_va") 
FlowDatesPeaks  <- lapply(ls.Discharge.Peaks,"[[","peak_dt")

Freq       <- sapply(HEC_PFA,"[[","FREQ", simplify = FALSE, USE.NAMES = TRUE) 
Freq       <- sapply(names(Freq), function(i) 0.01*Freq[[i]], simplify = FALSE, USE.NAMES = TRUE)
FlowAtFreq <- sapply(HEC_PFA,"[[","FLOW.Computed.Curve", simplify = FALSE, USE.NAMES = TRUE)

rm(ls.Discharge,ls.Discharge.All,ls.Discharge.Peaks, HEC_PFA)

# Use flag HAS_FAIL_DATE to determine whether or not a date is known, as that
# column incorporates my research (i.e., not just the NYSDOT info, which may not contain
# the failure date.) Which means that I will use FAIL_DATE_EST to index Fail Dates (but only
# for known fail dates from HAS_FAIL_DATE)
STAIDtoView             <- df.Fail.NBI.Gage[rowsToView,"STAID"]
nSites                <- length(STAIDtoView)
HasFailDate           <- df.Fail.NBI.Gage[rowsToView,"BOOL_HAS_FAIL_DATE"]
NoFailDate            <- !HasFailDate
HasFailIndex          <- c(1:length(rowsToView))[HasFailDate]
NoFailIndex           <- c(1:length(rowsToView))[NoFailDate]
HasYrBlt              <- !is.na(df.Fail.NBI.Gage[rowsToView,"YR_BLT_EST"])
NoYrBlt               <- !HasYrBlt
HasYrBltIndex         <- c(1:length(rowsToView))[HasYrBlt]
NoYrBltIndex          <- c(1:length(rowsToView))[NoYrBlt]

# Issue with one date in particular, where given fail date is in a different year than the given fail year - using the date as the true value
df.Fail.NBI.Gage$YR_FAIL_EST <- df.Fail.NBI.Gage$YR_FAIL
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID == 1424, "YR_FAIL_EST"] <- as.Date(paste(substr(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID == 1424, "DATE_FAIL_EST_USGS"],1,4), "01", "01",sep="-"))

# DAILY MEAN DATA -----------------------------------------
# Index failure dates, maxes
YrBltIndex                  <- rep(NA_integer_,nSites)
YrBltIndex[HasYrBlt]        <- unlist(sapply(HasYrBltIndex, function(i) which.min(abs(FlowDates[[STAIDtoView[i]]]-as.Date(df.Fail.NBI.Gage[rowsToView[i],"YR_BLT_EST"])))))
YrBltIndex[NoYrBlt]         <- 1
FailDateIndex               <- rep(NA_integer_, nSites)
FailDateIndex[HasFailIndex] <- unlist(sapply(HasFailIndex, function(i) which.min(abs(FlowDates[[STAIDtoView[i]]]-as.Date(df.Fail.NBI.Gage[rowsToView[i],"DATE_FAIL_EST_USGS"])))))
BeginFailYearIndex          <- sapply(1:nSites,  function(i) which.min(abs(FlowDates[[STAIDtoView[i]]]-df.Fail.NBI.Gage[rowsToView[i],"YR_FAIL_EST"])))
EndFailYearIndex            <- sapply(1:nSites,  function(i) which.min(abs(FlowDates[[STAIDtoView[i]]]-as.Date(paste(substr(df.Fail.NBI.Gage[rowsToView[i],"YR_FAIL_EST"],1,4),"12","31",sep="-")))))
Max_Q_Yr_Index              <- sapply(1:nSites,  function(i) which.max(FlowRecords[[STAIDtoView[i]]][BeginFailYearIndex[i]:EndFailYearIndex[i]]) +  BeginFailYearIndex[i]-1)
FailDateIndex[NoFailIndex]  <- Max_Q_Yr_Index[NoFailDate]
FailDateIndex               <- as.integer(FailDateIndex)
MaxIndex                    <- sapply(1:nSites,  function(i) which.max(FlowRecords[[STAIDtoView[i]]]))
Max_Q_Pre_Fail_Index        <- sapply(1:nSites,  function(i) which.max(FlowRecords[[STAIDtoView[i]]][1:FailDateIndex[i]]))

# Failure
FailDate                 <- sapply(1:nSites, function(i) FlowDates[[STAIDtoView[i]]][FailDateIndex[i]])
FailDate                 <- as.Date(as.numeric(FailDate),"1970-01-01",format="%Y-%m-%d")
dt_comment               <- character(nSites)
dt_comment[HasFailIndex] <- sapply(HasFailIndex, function(i) ifelse(FailDate[i]-as.Date(df.Fail.NBI.Gage[rowsToView[i],"DATE_FAIL_EST_USGS"])!=0,
                                                                    paste("Closest Q reading at",FailDate[i]-df.Fail.NBI.Gage[rowsToView[i],"DATE_FAIL"],"days from failure",sep=" "),
                                                                     "Flow on known fail date available."))
dt_comment[NoFailIndex]  <- "Fail date estimated from max flow in fail year"
Fail_Q               <- sapply(1:nSites, function(i) FlowRecords[[STAIDtoView[i]]][FailDateIndex[i]])

# Maxes
Max_Q_Yr_Date        <- sapply(1:nSites, function(i) FlowDates[[STAIDtoView[i]]][Max_Q_Yr_Index[i]])
Max_Q_Date           <- sapply(1:nSites, function(i) FlowDates[[STAIDtoView[i]]][MaxIndex[i]])
Max_Q_Pre_Fail_Date  <- sapply(1:nSites, function(i) FlowDates[[STAIDtoView[i]]][Max_Q_Pre_Fail_Index[i]])
Max_Q_Yr             <- sapply(1:nSites, function(i) FlowRecords[[STAIDtoView[i]]][Max_Q_Yr_Index[i]])
Max_Q                <- sapply(1:nSites, function(i) FlowRecords[[STAIDtoView[i]]][MaxIndex[i]])
Max_Q_Pre_Fail       <- sapply(1:nSites, function(i) FlowRecords[[STAIDtoView[i]]][Max_Q_Pre_Fail_Index[i]])

# Check for other dates of interest, analysis of results
Fail_Q_m1d           <- sapply(HasFailIndex, function(i) FlowRecords[[STAIDtoView[i]]][FailDateIndex[i]-1])
PrevDayLarger        <- HasFailIndex[Fail_Q_m1d > Fail_Q[HasFailIndex]]
  # IDs 103 and 1424 have a larger flow on the previous day
  # day do not have instantaneous flow data available, so no need to redo that analysis
Fail_Q_p1d           <- sapply(HasFailIndex, function(i) FlowRecords[[STAIDtoView[i]]][FailDateIndex[i]+1])
NextDayLarger        <- HasFailIndex[Fail_Q_p1d > Fail_Q[HasFailIndex]]
  #  IDs 1004, 1424 had a larger flow on the next day
MaxYrLarger          <- which(Max_Q_Yr_Index != FailDateIndex) 
  # IDs 103, 1004, 1424 all had larger flows at different points in the year
  # 3 days before (March 29) for ID 103, 72 days before for ID 1004 (Jan 19), and 182 days before (Jan 27) for 1424
  # So for 3 of 13 bridges with known fail dates, the largest flow in the year is not on the date of failure. 
MaxEqual             <- which(MaxIndex == FailDateIndex)
  # For 8 of the 13, the failure date was also the largest recorded flow

  # Use m1d values for the two sites
Fail_Q[PrevDayLarger]     <- Fail_Q_m1d[HasFailIndex %in% PrevDayLarger]
dt_comment[PrevDayLarger] <- "Day before fail date larger flow val used"

  # Times flow was exceeded
ExceedsFlowIndex     <- lapply(1:nSites, function(i) which(FlowRecords[[STAIDtoView[i]]][YrBltIndex[i]:FailDateIndex[i]] >= Fail_Q[i]))
ExceedCounts         <- sapply(1:nSites, function(i) ifelse(length(ExceedsFlowIndex[[i]])>1, 
                                                            sum(ExceedsFlowIndex[[i]][2:length(ExceedsFlowIndex[[i]])]-ExceedsFlowIndex[[i]][1:(length(ExceedsFlowIndex[[i]])-1)]>1),
                                                            1))

  # Number of days recorded during failure year
DaysRecorded <- sapply(rowsToView, function(i) sum(!is.na(FlowRecords[[df.Fail.NBI.Gage[i,"STAID"]]][format.Date(FlowDates[[df.Fail.NBI.Gage[i,"STAID"]]],"%YYYY")==format.Date(df.Fail.NBI.Gage[i,"YR_FAIL_EST"],"%YYYY")])))
names(DaysRecorded) <- df.Fail.NBI.Gage[rowsToView,"ID"]
  # IDs 1004 and 1390 have <365 days recorded
  # 1004, Hatchie River, none after April 4 (large event April 1, with known failure date, so OK)
  # 1390, Battle Run, no data starting July 11. Assumed failure date 6/27, major flash-flooding, so pretty confident got the biggest flow of the year. Put an asterisk on.

# Write values and dates to dataframe
df.Fail.NBI.Gage[rowsToView, "Q_FAIL_D_USGS"]                <- Fail_Q     # no changes to Date
df.Fail.NBI.Gage[rowsToView,"DATE_FAIL_EST_USGS"]          <- as.character(FailDate)
df.Fail.NBI.Gage[rowsToView,"COMMENT_Q_FAIL_USGS"]         <- dt_comment
df.Fail.NBI.Gage[rowsToView, "DATE_MAX_D_USGS"]            <- as.character(as.Date(Max_Q_Date, origin = "1970-01-01", format = "%Y-%m-%d")) # 8 changes, no changes to max val
df.Fail.NBI.Gage[rowsToView, "DATE_MAXPREFAIL_D_USGS"]   <- as.character(as.Date(Max_Q_Pre_Fail_Date, origin = "1970-01-01", format = "%Y-%m-%d")) # 4 changes
df.Fail.NBI.Gage[rowsToView, "Q_MAXPREFAIL_D_USGS"]        <- Max_Q_Pre_Fail # same 4 change
df.Fail.NBI.Gage$Q_FAILYRMAX_D_USGS                        <- NA_real_
df.Fail.NBI.Gage[rowsToView, "Q_FAILYRMAX_D_USGS"]         <- Max_Q_Yr
df.Fail.NBI.Gage$DATE_FAILYRMAX_D_USGS                   <- NA_real_
df.Fail.NBI.Gage[rowsToView, "DATE_FAILYRMAX_D_USGS"]    <- Max_Q_Yr_Date
df.Fail.NBI.Gage[rowsToView,"TIMES_Q_EXCEED"]         <- ExceedCounts # 7 change
df.Fail.NBI.Gage[rowsToView,"DAYS_RECORDED_YR_FAIL"]         <- DaysRecorded
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1004,"COMMENT_Q_FAIL_USGS"] <- paste(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1004,"COMMENT_Q_FAIL_USGS"],
                                                                      "<365days recorded - but avail on known fail date")
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1390,"COMMENT_Q_FAIL_USGS"] <- paste(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1390,"COMMENT_Q_FAIL_USGS"],
                                                                      "191 days recorded, no record 5 days after major flash flood")


# Obtain exceedence values and return periods from HEC
ExceedProbFail       <- rep(NA_real_,nSites)
ExceedProbMax        <- rep(NA_real_,nSites)
ExceedProbMaxPreFail <- rep(NA_real_,nSites)
T500index  <- sapply(names(Freq), function(i) which(Freq[[i]]==0.002))
T100index  <- sapply(names(Freq), function(i) which(Freq[[i]]==0.01))
Q500       <- sapply(names(FlowAtFreq), function(i) FlowAtFreq[[i]][T500index[i]])
Q100       <- sapply(names(FlowAtFreq), function(i) FlowAtFreq[[i]][T100index[i]])

for (i in 1:length(rowsToView)){
  STAID    <- STAIDtoView[i]
  print(STAID)
  log_flow <- log(FlowAtFreq[[STAID]][!is.na(FlowAtFreq[[STAID]])])
  log_freq <- log(Freq[[STAID]][!is.na(FlowAtFreq[[STAID]])])
  log_freq <- log_freq[!is.na(log_flow)]
  log_flow <- log_flow[!is.na(log_flow)]
  model.lm <- approxfun(log_flow,log_freq)
  
  # fail
  log_Val <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_FAIL_D_USGS"])  
  ExceedProbFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  # max
  log_Val <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_MAX_D_USGS"])
  ExceedProbMax[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  # max pre- fail
  log_Val <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_MAXPREFAIL_D_USGS"])
  ExceedProbMaxPreFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
}

# Write exceedence and return period values to dataframe
df.Fail.NBI.Gage[rowsToView,"EXCEED_HEC"]              <- ExceedProbFail
df.Fail.NBI.Gage$T_FAIL_D_HECP_USGS                            <- 1/df.Fail.NBI.Gage$EXCEED_HEC
# For better comparison to other data, disallow return periods less than 1 yea1/Er
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_FAIL_D_HECP_USGS < 1 & !is.na(df.Fail.NBI.Gage$T_FAIL_D_HECP_USGS),"T_FAIL_D_HECP_USGS"] <- 1
df.Fail.NBI.Gage[rowsToView,"EXCEED_HEC_MAX"]          <- ExceedProbMax
df.Fail.NBI.Gage$T_MAX_D_HECP_USGS                             <- 1/df.Fail.NBI.Gage$EXCEED_HEC_MAX
df.Fail.NBI.Gage[rowsToView,"EXCEED_HEC_MAX_PRE_FAIL"] <- ExceedProbMaxPreFail
df.Fail.NBI.Gage$T_MAXPREFAIL_D_HECP_USGS                    <- 1/df.Fail.NBI.Gage$EXCEED_HEC_MAX_PRE_FAIL

df.Fail.NBI.Gage$Q100_HECP_USGS                         <- NA_real_
df.Fail.NBI.Gage$Q500_HECP_USGS                         <- NA_real_
df.Fail.NBI.Gage[rowsToView,"Q100_HECP_USGS"]     <- Q100[STAIDtoView] # majority of these were already correct
df.Fail.NBI.Gage[rowsToView,"Q500_HECP_USGS"]     <- Q500[STAIDtoView]

# DIST DATA -------------------------------------------------
# MAXIMUM FROM DAILY MEAN 
MaxDailyMeans             <- lapply(ls.Discharge.Dist,"[[","max_va")
LifetimeMaxDailyMeanIndex <- sapply(1:nSites, function(i) which.max(MaxDailyMeans[[STAIDtoView[i]]]))
LifetimeMaxDailyMean      <- sapply(1:nSites, function(i) MaxDailyMeans[[STAIDtoView[i]]][LifetimeMaxDailyMeanIndex[i]])
MaxDailyMeansDate         <- sapply(1:nSites, function(i) paste(ls.Discharge.Dist[[STAIDtoView[i]]]$max_va_yr[LifetimeMaxDailyMeanIndex[i]],
                                                                ls.Discharge.Dist[[STAIDtoView[i]]]$month_nu[LifetimeMaxDailyMeanIndex[i]],
                                                                ls.Discharge.Dist[[STAIDtoView[i]]]$day_nu[LifetimeMaxDailyMeanIndex[i]],
                                                                sep="-"))
MaxDailyMeansDate         <- as.Date(MaxDailyMeansDate,"%Y-%m-%d")
rm(ls.Discharge.Dist)

# note that these values are exclusively smaller than what I had obtained
df.Fail.NBI.Gage$MAX_Q_DIST                    <- NA_real_
df.Fail.NBI.Gage[rowsToView,"MAX_Q_DIST"]      <- LifetimeMaxDailyMean
df.Fail.NBI.Gage$MAX_Q_DIST_DATE               <- ""
df.Fail.NBI.Gage[rowsToView,"MAX_Q_DIST_DATE"] <- as.character(MaxDailyMeansDate)

# INSTANTANEOUS DATA -----------------------------------------
# Index failure dates, maxes
HasInstData     <- paste(STAIDtoView,substr(df.Fail.NBI.Gage[rowsToView,"YR_FAIL_EST"],1,4),sep="_") %in% names(ls.Discharge.Inst)
FailMaxIndex    <- integer(nSites) # picks max from day or max from year
FailMaxDate     <- as.Date(rep(NA,nSites),format="%Y-%m-%d")
FailMaxQ        <- rep(NA_real_,nSites) 
FailIndices     <- list()

for (i in 1:nSites){
  print(i)
  if (HasInstData[i]){
    ii <- paste(STAIDtoView[i],substr(df.Fail.NBI.Gage[rowsToView[i],"YR_FAIL_EST"],1,4),sep="_")
    print(ii)
    FailIndices[[i]] <- which(FlowDatesInst[[ii]]==df.Fail.NBI.Gage[rowsToView[i],"DATE_FAIL_EST_USGS"])
    if (is.na(FailIndices[[i]][1]) | length(FailIndices[[i]])==0){
      print(paste("For site",STAIDtoView[i],"fail date not available in instantaneous data set"))
      FailIndices[[i]] <- which.min(abs(FlowDatesInst[[ii]]-as.Date(df.Fail.NBI.Gage[rowsToView[i],"DATE_FAIL_EST_USGS"])))
      temp             <- ifelse(abs(FlowDatesInst[[ii]][FailIndices[[i]]]-as.Date(df.Fail.NBI.Gage[rowsToView[i],"DATE_FAIL_EST_USGS"]))<=3,
                                 FailIndices[[i]],
                                 NA_integer_)
      if (is.na(temp[1])) FailIndices[[i]] <- NA_integer_
      else FailIndices[[i]] <- unlist(temp)
    }
    # compute max
    if (length(FailIndices[[i]])!=0 & !is.null(FailIndices[[i]]) & !is.na(FailIndices[[i]][1])){
      FailMaxIndex[i] <- which.max(FlowRecordsInst[[ii]][FailIndices[[i]]])
      FailMaxDate[i]  <- FlowDatesInst[[ii]][FailMaxIndex[i] + FailIndices[[i]][1]-1]
      FailMaxQ[i]     <- FlowRecordsInst[[ii]][FailMaxIndex[i] + FailIndices[[i]][1]-1]
    }
  }
  else FailIndices[[i]] <- NA_integer_
}
HasFailQ <- sapply(1:nSites, function(i) !is.na(FailIndices[[i]][1]))
FailMaxQ[!HasFailQ] <- NA

# Write values and dates to dataframe
df.Fail.NBI.Gage[rowsToView,"Q_FAIL_I_USGS"]         <- FailMaxQ
df.Fail.NBI.Gage[rowsToView,"DATE_FAIL_I_EST_USGS"]  <- FailMaxDate
# For ID 393, STAID 01643000, event appears to be short and data not available close enough to fail date - do not use (Monocacy)
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==393,"Q_FAIL_I_USGS"] <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==393,"T_FAIL_I_HECP_USGS"] <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==393,"T_FAIL_INST_LP3_USGS"] <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==393,"T_FAIL_INST_GEV_USGS"] <- NA_real_
# For ID 1482 STAID 01596500 Savage River, only day after event available and too low
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1424,"Q_FAIL_I_USGS"] <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1424,"T_FAIL_I_HECP_USGS"] <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1424,"T_FAIL_INST_LP3_USGS"] <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1424,"T_FAIL_INST_GEV_USGS"] <- NA_real_

# Obtain exceedence values and return periods from HEC
rowsWithInst   <- rowsToView[HasFailQ]
ExceedProbFail <- numeric(length(rowsWithInst))
for (i in 1:length(rowsWithInst)){
  STAID    <- df.Fail.NBI.Gage[rowsWithInst[i], "STAID"]
  log_flow <- log(FlowAtFreq[[STAID]])
  log_freq <- log(Freq[[STAID]])
  log_freq <- log_freq[!is.na(log_flow)]
  log_flow <- log_flow[!is.na(log_flow)]
  
  log_Val <- log(df.Fail.NBI.Gage[rowsWithInst[i],"Q_FAIL_I_USGS"])
  ExceedProbFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
}


# Write exceedence and return period values to dataframe
df.Fail.NBI.Gage$EXCEED_HEC_INST                  <- NA_real_
df.Fail.NBI.Gage[rowsWithInst,"EXCEED_HEC_INST"]  <- ExceedProbFail
df.Fail.NBI.Gage$T_FAIL_I_HECP_USGS                  <- 1/df.Fail.NBI.Gage$EXCEED_HEC_INST
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_FAIL_I_HECP_USGS < 1 & !is.na(df.Fail.NBI.Gage$T_FAIL_I_HECP_USGS),"T_FAIL_I_HECP_USGS"] <- 1

# PEAK DATA ------------------------------------------------
# Index failure dates, maxes
rowsWithPeakQ <- rowsToView[!is.na(df.Fail.NBI.Gage[rowsToView,"Q_FAIL_P_USGS"])]
FailQFromPeak <- sapply(NoFailIndex, function(i) max(FlowPeaks[[STAIDtoView[i]]][format.Date(FlowDatesPeaks[[STAIDtoView[i]]],"%Y")==format.Date(df.Fail.NBI.Gage[rowsToView[i],"YR_FAIL_EST"],"%Y")])) 
Max_Peak_Index <- sapply(1:nSites, function(i) which.max(FlowPeaks[[df.Fail.NBI.Gage[rowsToView[i],"STAID"]]]))
Max_Peak       <- sapply(1:nSites, function(i) FlowPeaks[[df.Fail.NBI.Gage[rowsToView[i],"STAID"]]][Max_Peak_Index[i]])
Max_Peak_Date  <- sapply(1:nSites, function(i) FlowDatesPeaks[[df.Fail.NBI.Gage[rowsToView[i],"STAID"]]][Max_Peak_Index[i]])
Max_Peak_Date  <- as.Date(Max_Peak_Date,"1970-01-01",format="%Y-%m-%d")
MaxQPreFailIndex     <- sapply(1:nSites, function(i) which.max(FlowPeaks[[STAIDtoView[i]]][FlowDatesPeaks[[STAIDtoView[i]]] <= df.Fail.NBI.Gage[rowsToView[i], "DATE_FAIL_EST_USGS"]]))
MaxQPreFail          <- sapply(1:nSites, function(i) FlowPeaks[[STAIDtoView[i]]][MaxQPreFailIndex[i]])
MaxQPreFailDate      <- sapply(1:nSites, function(i) FlowDatesPeaks[[STAIDtoView[i]]][MaxQPreFailIndex[i]])
MaxQPreFailDate      <- as.Date(MaxQPreFailDate, origin = "1970-01-01")

# Write values and dates to dataframe
df.Fail.NBI.Gage[rowsToView[NoFailIndex],"Q_FAIL_P_USGS"] <- FailQFromPeak

df.Fail.NBI.Gage[rowsToView,"Q_MAX_P_USGS"]      <- Max_Peak
df.Fail.NBI.Gage[rowsToView,"DATE_MAX_P_USGS"] <- Max_Peak_Date

df.Fail.NBI.Gage[rowsToView,"Q_MAXPREFAIL_P_USGS"]      <- MaxQPreFail
df.Fail.NBI.Gage[rowsToView,"DATE_MAXPREFAIL_P_USGS"] <- MaxQPreFailDate

# Obtain exceedence values and return periods from HEC
ExceedProbMax        <- rep(NA_real_,nSites)
ExceedProbMaxPreFail <- rep(NA_real_,nSites)
ExceedProbFail       <- rep(NA_real_,nSites)

for (i in 1:nSites){
  STAID    <- STAIDtoView[i]
  log_flow <- log(FlowAtFreq[[STAID]])
  log_freq <- log(Freq[[STAID]])
  log_freq <- log_freq[!is.na(log_flow)]
  log_flow <- log_flow[!is.na(log_flow)]
  
  if (rowsToView[i] %in% rowsWithPeakQ){
    log_Val  <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_FAIL_P_USGS"])
    ExceedProbFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  }
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_MAX_P_USGS"])
  ExceedProbMax[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_MAXPREFAIL_P_USGS"])
  ExceedProbMaxPreFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
}

# Write exceedence and return period values to dataframe
df.Fail.NBI.Gage$EXCEED_HEC_PEAK                         <- NA_real_
df.Fail.NBI.Gage[rowsToView,"EXCEED_HEC_PEAK"]           <- ExceedProbFail
df.Fail.NBI.Gage$T_FAIL_P_HECP_USGS                         <- 1/df.Fail.NBI.Gage$EXCEED_HEC_PEAK
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_FAIL_P_HECP_USGS < 1 & !is.na(df.Fail.NBI.Gage$T_FAIL_P_HECP_USGS),"T_FAIL_P_HECP_USGS"] <- 1
df.Fail.NBI.Gage$EXCEED_HEC_MAX_PEAK                     <- NA_real_
df.Fail.NBI.Gage[rowsToView,"EXCEED_HEC_MAX_PEAK"]       <- ExceedProbMax 
df.Fail.NBI.Gage$T_MAX_P_HECP_USGS                          <- 1/df.Fail.NBI.Gage$EXCEED_HEC_MAX_PEAK
df.Fail.NBI.Gage$T_MAXPREFAIL_P_HECP_USGS                 <- NA_real_
df.Fail.NBI.Gage[rowsToView, "T_MAXPREFAIL_P_HECP_USGS"]  <- 1/ExceedProbMaxPreFail

IDsToUpdateFailPeak <- c(3528,1442,1164,499,393) # all have same max peak date as failure date
rowsToUpdateFailPeak <- which(df.Fail.NBI.Gage$ID %in% IDsToUpdateFailPeak)
df.Fail.NBI.Gage[rowsToUpdateFailPeak,"T_FAIL_P_HECP_USGS"] <- sapply(rowsToUpdateFailPeak, function(i) df.Fail.NBI.Gage[i,"T_MAX_P_HECP_USGS"])
df.Fail.NBI.Gage[rowsToUpdateFailPeak,"Q_FAIL_P_USGS"]     <- sapply(rowsToUpdateFailPeak, function(i) df.Fail.NBI.Gage[i,"Q_MAX_P_USGS"])

# FIND T_FAIL_BEST_HEC (INSTANTANEOUS > PEAK > DAILY) ---------------
BestT <- character(length(rowsToView))
BestT[df.Fail.NBI.Gage[rowsToView,"BOOL_HAS_FAIL_DATE"]==TRUE & !is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_I_HECP_USGS"])]  <- "INST"
BestT[df.Fail.NBI.Gage[rowsToView,"BOOL_HAS_FAIL_DATE"]==TRUE & is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_I_HECP_USGS"])]   <- "DAILY"
BestT[df.Fail.NBI.Gage[rowsToView,"BOOL_HAS_FAIL_DATE"]==TRUE & !is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_P_HECP_USGS"]) & 
        as.Date(df.Fail.NBI.Gage[rowsToView,"DATE_MAX_P_USGS"],"1970-01-01")==df.Fail.NBI.Gage[rowsToView,"DATE_FAIL_EST_USGS"]] <- "PEAK"
BestT[df.Fail.NBI.Gage[rowsToView,"BOOL_HAS_FAIL_DATE"]==TRUE & !is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_P_HECP_USGS"])]  <- "PEAK"
BestT[df.Fail.NBI.Gage[rowsToView,"BOOL_HAS_FAIL_DATE"]==FALSE & !is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_P_HECP_USGS"])]  <- "PEAK"
BestT[df.Fail.NBI.Gage[rowsToView,"BOOL_HAS_FAIL_DATE"]==FALSE & is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_P_HECP_USGS"]) & !is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_I_HECP_USGS"])]  <- "INST"
BestT[df.Fail.NBI.Gage[rowsToView,"BOOL_HAS_FAIL_DATE"]==FALSE & is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_P_HECP_USGS"]) & is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_I_HECP_USGS"])]   <- "DAILY"
df.Fail.NBI.Gage$T_FAIL_BEST_HEC_SOURCE <- NA_character_
df.Fail.NBI.Gage[rowsToView,"T_FAIL_BEST_HEC_SOURCE"] <- BestT
df.Fail.NBI.Gage[rowsToView[df.Fail.NBI.Gage[rowsToView,"T_FAIL_BEST_HEC_SOURCE"]=="PEAK"],"Q_FAIL_IP_USGS"] <- df.Fail.NBI.Gage[rowsToView[df.Fail.NBI.Gage[rowsToView,"T_FAIL_BEST_HEC_SOURCE"]=="PEAK"],"Q_FAIL_P_USGS"]
df.Fail.NBI.Gage[rowsToView[df.Fail.NBI.Gage[rowsToView,"T_FAIL_BEST_HEC_SOURCE"]=="INST"],"Q_FAIL_IP_USGS"] <- df.Fail.NBI.Gage[rowsToView[df.Fail.NBI.Gage[rowsToView,"T_FAIL_BEST_HEC_SOURCE"]=="INST"],"Q_FAIL_I_USGS"]

df.Fail.NBI.Gage$T_FAIL_BEST_HEC <- NA_real_
df.Fail.NBI.Gage[rowsToView[df.Fail.NBI.Gage[rowsToView,"T_FAIL_BEST_HEC_SOURCE"]=="DAILY"],"T_FAIL_BEST_HEC"] <- df.Fail.NBI.Gage[rowsToView[df.Fail.NBI.Gage[rowsToView,"T_FAIL_BEST_HEC_SOURCE"]=="DAILY"],"T_FAIL_D_HECP_USGS"]
df.Fail.NBI.Gage[rowsToView[df.Fail.NBI.Gage[rowsToView,"T_FAIL_BEST_HEC_SOURCE"]=="INST"],"T_FAIL_BEST_HEC"]  <- df.Fail.NBI.Gage[rowsToView[df.Fail.NBI.Gage[rowsToView,"T_FAIL_BEST_HEC_SOURCE"]=="INST"],"T_FAIL_I_HECP_USGS"]
df.Fail.NBI.Gage[rowsToView[df.Fail.NBI.Gage[rowsToView,"T_FAIL_BEST_HEC_SOURCE"]=="PEAK"],"T_FAIL_BEST_HEC"]  <- df.Fail.NBI.Gage[rowsToView[df.Fail.NBI.Gage[rowsToView,"T_FAIL_BEST_HEC_SOURCE"]=="PEAK"],"T_FAIL_P_HECP_USGS"]

df.Fail.NBI.Gage[rowsToView[df.Fail.NBI.Gage[rowsToView,"T_FAIL_BEST_HEC_SOURCE"]=="PEAK"],"T_FAIL_IP_HECP_USGS"] <- df.Fail.NBI.Gage[rowsToView[df.Fail.NBI.Gage[rowsToView,"T_FAIL_BEST_HEC_SOURCE"]=="PEAK"],"T_FAIL_P_HECP_USGS"]
df.Fail.NBI.Gage[rowsToView[df.Fail.NBI.Gage[rowsToView,"T_FAIL_BEST_HEC_SOURCE"]=="INST"],"T_FAIL_IP_HECP_USGS"] <- df.Fail.NBI.Gage[rowsToView[df.Fail.NBI.Gage[rowsToView,"T_FAIL_BEST_HEC_SOURCE"]=="INST"],"T_FAIL_I_HECP_USGS"]

# SAVE DATAFRAME --------------------------------------
savefile <- paste(gsub("-","",Sys.Date()),"df.Fail.NBI.Gage_WithReCalcedHECdailyInstPeakBestValuesUsingLinInterp.RData",sep="_")
save(df.Fail.NBI.Gage, file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage, rowsToView, IDsToView, file=file.path(dirs$dfActive,savefile))
rm(savefile)

# HEC ANALYSIS USING DAILY MEAN DATA -------------------------------------------------------------------------------------
Freq       <- sapply(HEC_PFA_AnMaxDayMean,"[[","FREQ", simplify = FALSE, USE.NAMES = TRUE) 
Freq       <- sapply(names(Freq), function(i) 0.01*Freq[[i]], simplify = FALSE, USE.NAMES = TRUE)
FlowAtFreq <- sapply(HEC_PFA_AnMaxDayMean,"[[","FLOW.Computed.Curve", simplify = FALSE, USE.NAMES = TRUE)

rm(ls.Discharge.Peaks, ls.Discharge.Inst,ls.Discharge.Dist,HEC_PFA_AnMaxDayMean)

# Obtain exceedence values and return periods from HEC
ExceedProbMax        <- rep(NA_real_,nSites)
ExceedProbMaxPreFail <- rep(NA_real_,nSites)
ExceedProbFail       <- rep(NA_real_,nSites)
T500index  <- sapply(names(Freq), function(i) which(Freq[[i]]==0.002))
T100index  <- sapply(names(Freq), function(i) which(Freq[[i]]==0.01))
Q500       <- sapply(names(FlowAtFreq), function(i) FlowAtFreq[[i]][T500index[i]])
Q100       <- sapply(names(FlowAtFreq), function(i) FlowAtFreq[[i]][T100index[i]])

for (i in 1:nSites){
  STAID    <- STAIDtoView[i]
  log_flow <- log(FlowAtFreq[[STAID]])
  log_freq <- log(Freq[[STAID]])
  log_freq <- log_freq[!is.na(log_flow)]
  log_flow <- log_flow[!is.na(log_flow)]
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_FAIL_D_USGS"])
  ExceedProbFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_MAX_D_USGS"])
  ExceedProbMax[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_MAXPREFAIL_D_USGS"])
  ExceedProbMaxPreFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
}

# Write exceedence and return period values to dataframe
df.Fail.NBI.Gage$EXCEED_HEC_DAY                         <- NA_real_
df.Fail.NBI.Gage[rowsToView,"EXCEED_HEC_DAY"]           <- ExceedProbFail
df.Fail.NBI.Gage$T_FAIL_D_HECD_USGS                         <- 1/df.Fail.NBI.Gage$EXCEED_HEC_DAY
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_FAIL_D_HECD_USGS < 1 & !is.na(df.Fail.NBI.Gage$T_FAIL_D_HECD_USGS),"T_FAIL_D_HECD_USGS"] <- 1
df.Fail.NBI.Gage$EXCEED_HEC_MAX_DAY                     <- NA_real_
df.Fail.NBI.Gage[rowsToView,"EXCEED_HEC_MAX_DAY"]       <- ExceedProbMax 
df.Fail.NBI.Gage$T_MAX_D_HECD_USGS                          <- 1/df.Fail.NBI.Gage$EXCEED_HEC_MAX_DAY
df.Fail.NBI.Gage$T_MAXPREFAIL_D_HECD_USGS                 <- NA_real_
df.Fail.NBI.Gage[rowsToView, "T_MAXPREFAIL_D_HECD_USGS"]  <- 1/ExceedProbMaxPreFail
df.Fail.NBI.Gage[rowsToView,"Q100_D_HECD_USGS"]     <- Q100[STAIDtoView]
df.Fail.NBI.Gage[rowsToView,"Q500_D_HECD_USGS"]     <- Q500[STAIDtoView]

savefile <- paste(gsub("-","",Sys.Date()),"df.Fail.NBI.Gage_WithECmaxAnDayMeanFixed.RData",sep="_")
save(df.Fail.NBI.Gage, file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage, rowsToView, IDsToView, file=file.path(dirs$dfActive,savefile))
rm(savefile)

## BULLETIN 17B ANALYSIS USING PEAKFQ --------------------------------------------------------------
# Daily mean data
Freq       <- sapply(PFA_USGSd,"[[","ANN_EXC_PROB", simplify = FALSE, USE.NAMES = TRUE) 
FlowAtFreq <- sapply(PFA_USGSd,"[[","BULL17B_EST", simplify = FALSE, USE.NAMES = TRUE)

rm(ls.Discharge,ls.Discharge.All,ls.Discharge.Peaks, PFA_USGSd)

ExceedProbMax        <- rep(NA_real_,nSites)
ExceedProbMaxPreFail <- rep(NA_real_,nSites)
ExceedProbFail       <- rep(NA_real_,nSites)
T500index  <- sapply(names(Freq), function(i) which(Freq[[i]]==0.002))
T100index  <- sapply(names(Freq), function(i) which(Freq[[i]]==0.01))
Q500       <- sapply(names(FlowAtFreq), function(i) FlowAtFreq[[i]][T500index[i]])
Q100       <- sapply(names(FlowAtFreq), function(i) FlowAtFreq[[i]][T100index[i]])

for (i in 1:nSites){
  STAID    <- STAIDtoView[i]
  log_flow <- log(FlowAtFreq[[STAID]])
  log_freq <- log(Freq[[STAID]])
  log_freq <- log_freq[!is.na(log_flow)]
  log_flow <- log_flow[!is.na(log_flow)]
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_FAIL_D_USGS"])
  ExceedProbFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_MAX_D_USGS"])
  ExceedProbMax[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_MAXPREFAIL_D_USGS"])
  ExceedProbMaxPreFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
}

# Write exceedence and return period values to dataframe
df.Fail.NBI.Gage$EXCEED_PKFQ_DAY                         <- NA_real_
df.Fail.NBI.Gage[rowsToView,"EXCEED_PKFQ_DAY"]           <- ExceedProbFail
df.Fail.NBI.Gage$T_FAIL_D_PKFQD_USGS                         <- 1/df.Fail.NBI.Gage$EXCEED_PKFQ_DAY
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_FAIL_D_PKFQD_USGS < 1 & !is.na(df.Fail.NBI.Gage$T_FAIL_D_PKFQD_USGS),"T_FAIL_D_PKFQD_USGS"] <- 1
df.Fail.NBI.Gage$EXCEED_PKFQ_MAX_DAY                     <- NA_real_
df.Fail.NBI.Gage[rowsToView,"EXCEED_PKFQ_MAX_DAY"]       <- ExceedProbMax 
df.Fail.NBI.Gage$T_MAX_D_PKFQD_USGS                          <- 1/df.Fail.NBI.Gage$EXCEED_PKFQ_MAX_DAY
df.Fail.NBI.Gage$T_MAXPREFAIL_D_PKFQD_USGS                 <- NA_real_
df.Fail.NBI.Gage[rowsToView, "T_MAXPREFAIL_D_PKFQD_USGS"]  <- 1/ExceedProbMaxPreFail
df.Fail.NBI.Gage[rowsToView,"Q100_D_PKFQD_USGS"]     <- Q100[STAIDtoView]
df.Fail.NBI.Gage[rowsToView,"Q500_D_PKFQD_USGS"]     <- Q500[STAIDtoView]

savefile <- paste(gsub("-","",Sys.Date()),"df.Fail.NBI.Gage_WithPKFQmaxAnDayMean_WithUserRegSkews.RData",sep="_")
save(df.Fail.NBI.Gage, file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage, rowsToView, IDsToView, file=file.path(dirs$dfActive,savefile))
rm(savefile)

# Peak data
Freq       <- sapply(PFA_USGSp,"[[","ANN_EXC_PROB", simplify = FALSE, USE.NAMES = TRUE) 
FlowAtFreq <- sapply(PFA_USGSp,"[[","BULL17B_EST", simplify = FALSE, USE.NAMES = TRUE)

rm(ls.Discharge,ls.Discharge.All,ls.Discharge.Peaks, PFA_USGSp)

ExceedProbMax        <- rep(NA_real_,nSites)
ExceedProbMaxPreFail <- rep(NA_real_,nSites)
ExceedProbFail       <- rep(NA_real_,nSites)
T500index  <- sapply(names(Freq), function(i) ifelse(length(which(Freq[[i]]==0.002))>=1,which(Freq[[i]]==0.002),NA_integer_))
T100index  <- sapply(names(Freq), function(i) ifelse(length(which(Freq[[i]]==0.01))>=1,which(Freq[[i]]==0.01),NA_integer_))
Q500       <- sapply(names(FlowAtFreq), function(i) FlowAtFreq[[i]][T500index[i]])
Q100       <- sapply(names(FlowAtFreq), function(i) FlowAtFreq[[i]][T100index[i]])

for (i in 1:nSites){
  STAID    <- STAIDtoView[i]
  log_flow <- log(FlowAtFreq[[STAID]])
  log_freq <- log(Freq[[STAID]])
  log_freq <- log_freq[!is.na(log_flow)]
  log_flow <- log_flow[!is.na(log_flow)]
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_FAIL_D_USGS"])
  ExceedProbFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_MAX_D_USGS"])
  ExceedProbMax[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
  
  log_Val  <- log(df.Fail.NBI.Gage[rowsToView[i],"Q_MAXPREFAIL_D_USGS"])
  ExceedProbMaxPreFail[i] <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
}

# Write exceedence and return period values to dataframe
df.Fail.NBI.Gage$EXCEED_PKFQ_DAY                         <- NA_real_
df.Fail.NBI.Gage[rowsToView,"EXCEED_PKFQ_PEAK"]           <- ExceedProbFail
df.Fail.NBI.Gage$T_FAIL_P_PKFQP_USGS                         <- 1/df.Fail.NBI.Gage$EXCEED_PKFQ_PEAK
df.Fail.NBI.Gage[df.Fail.NBI.Gage$T_FAIL_P_PKFQP_USGS < 1 & !is.na(df.Fail.NBI.Gage$T_FAIL_P_PKFQP_USGS),"T_FAIL_P_PKFQP_USGS"] <- 1
df.Fail.NBI.Gage$EXCEED_PKFQ_MAX_PEAK                     <- NA_real_
df.Fail.NBI.Gage[rowsToView,"EXCEED_PKFQ_MAX_PEAK"]       <- ExceedProbMax 
df.Fail.NBI.Gage$T_MAX_P_PKFQP_USGS                          <- 1/df.Fail.NBI.Gage$EXCEED_PKFQ_MAX_PEAK
df.Fail.NBI.Gage$T_MAXPREFAIL_P_PKFQP_USGS                 <- NA_real_
df.Fail.NBI.Gage[rowsToView, "T_MAXPREFAIL_P_PKFQP_USGS"]  <- 1/ExceedProbMaxPreFail
df.Fail.NBI.Gage[rowsToView,"Q100_P_PKFQP_USGS"]     <- Q100[STAIDtoView]
df.Fail.NBI.Gage[rowsToView,"Q500_P_PKFQP_USGS"]     <- Q500[STAIDtoView]

savefile <- paste(gsub("-","",Sys.Date()),"df.Fail.NBI.Gage_WithPKFQpeaks_UserRegSkews.RData",sep="_")
save(df.Fail.NBI.Gage, file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage, rowsToView, IDsToView, file=file.path(dirs$dfActive,savefile))
rm(savefile)


