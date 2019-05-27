# Madeleine Flint 2015-11-30
# Uses waterData package and modifications to download USGS stream gauge data
# and find values related to failure, maximum, and maximum pre-failure
# Pulls 3 things: 
# 1. daily flow from start of bridge or gage to failure
# 2. instantaneous flow on day or max flow in year
# 3. daily distribution
# 4. peak flow
# 5. daily over whole life

library(waterData)

SitesChar <- # character vector of STAID to be analyzed

# 1 Record of Daily Discharge during period of coexistence of bridge and gage, 00060 ---------
BeginDate <- sapply(rowsAdditionalStudy, function(i) max(as.Date(as.numeric(df.Fail.NBI.Gage[i,"DATE_P_BEGIN_ALT_USGS"]),"1970-01-01"),df.Fail.NBI.Gage[i,"YR_BLT_EST"],na.rm=TRUE))
BeginDate <- as.Date(BeginDate,"%Y-%m-%d",origin="1970-01-01")
BeginDate <- sapply(BeginDate, function(y) substr(y,1,4))
BeginDate <- paste(BeginDate,"01","01",sep="-")
EndDate   <- sapply(rowsAdditionalStudy, function(i) min(as.Date(as.numeric(df.Fail.NBI.Gage[i,"DATE_P_END_ALT_USGS"]),origin="1970-01-01"),df.Fail.NBI.Gage[i,"YR_FAIL"],na.rm=TRUE))
EndDate   <- as.Date(as.numeric(EndDate),origin="1970-01-01")
EndDate   <- sapply(EndDate, function(y) substr(y,1,4))
EndDate   <- paste(EndDate,"12","31",sep="-")
ls.Discharge <- vector("list",length=length(SitesChar))
for (i in 1:length(SitesChar)){
  dat <- importDVs(SitesChar[i],code="00060",stat="00003",sdate=BeginDate[i],edate=EndDate[i])
  ls.Discharge[[i]]$staid <- dat$staid
  ls.Discharge[[i]]$val   <- dat$val
  ls.Discharge[[i]]$dates <- dat$dates
}
names(ls.Discharge) <- SitesChar
#     clean dataset
for (i in 1:length(SitesChar)){
  ls.Discharge[[i]] <- cleanUp(ls.Discharge[[i]],task="fix",replace=0.1)
}

savefile <- paste(gsub("-","",Sys.Date()),"DailyMeanFlow.RData",sep="_")
save(ls.Discharge, file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

# 2 Instantaneous flow on day of failure, 00061 is instantaneous discharge -----------------
HasFailDate <- !is.na(df.Fail.NBI.Gage[rowsAdditionalStudy,"DATE_FAIL"])
NoFailDate  <- !HasFailDate
FailDate    <- character(length(rowsAdditionalStudy))
FailDate[HasFailDate] <- df.Fail.NBI.Gage[rowsAdditionalStudy[HasFailDate],"DATE_FAIL"]
#      lookup max flow in year of failure if don't have failure date
NoFailIndex  <- c(1:length(rowsAdditionalStudy))[NoFailDate]
Dates <- lapply(ls.Discharge,"[[",3)
BeginFailYearIndex    <- sapply(NoFailIndex, function(i) which.min(abs(Dates[[SitesChar[i]]]-df.Fail.NBI.Gage[rowsAdditionalStudy[i],"YR_FAIL"])))
EndFailYearIndex      <- sapply(NoFailIndex, function(i) which.min(abs(Dates[[SitesChar[i]]]-as.Date(paste(substr(df.Fail.NBI.Gage[rowsAdditionalStudy[i],"YR_FAIL"],1,4),"12","31",sep="-")))))
FailDate[NoFailDate]  <- sapply(1:length(BeginFailYearIndex), function(i) ls.Discharge[[NoFailIndex[i]]][which.max(ls.Discharge[[NoFailIndex[i]]]$val[BeginFailYearIndex[i]:EndFailYearIndex[i]])+BeginFailYearIndex[i],"dates"])
FailDate <- as.Date(as.numeric(FailDate),"1970-01-01")

# http://ida.water.usgs.gov/ida to download pre-2007
# http://nwis.waterdata.usgs.gov/nwis/uv/?site_no=01031500
# says 00060 but gives instantaneous

# ---------------------WARNING---------------------
# The instantaneous data you have obtained from
# this automated U.S. Geological Survey database
# may or may not have been the basis for the published
# daily mean discharges for this station. Although
# automated filtering has been used to compare these
# data to the published daily mean values and to remove
# obviously bad data, there may still be significant
# error in individual values. Users are strongly
# encouraged to review all data carefully prior to use.
# These data are released on the condition that neither
# the USGS nor the United States Government may be held
# liable for any damages resulting from its use.
#
# This file consists of tab-separated columns of the
# following fields.
#
# column       column definition
# -----------  -----------------------------------------
# site_no      USGS site identification number
# date_time     date and time in format (YYYYMMDDhhmmss)
# tz_cd        time zone
# dd           internal USGS sensor designation (''data descriptor'')
# accuracy_cd  accuracy code
#                   0 - A daily mean discharge calculated from the instantaneous
#                       data on this day is 0.01 cubic feet per second
#                       or less and the published daily mean is zero.
#                   1 - A daily mean discharge calculated from the instantaneous
#                       data on this day matches the published daily mean
#                       within 1 percent.
#                   2 - A daily mean discharge calculated from the instantaneous
#                       data on this day matches the published daily mean
#                       from greater than 1 to 5 percent.
#                   3 - A daily mean discharge calculated from the instantaneous
#                       values on this day matches the published daily mean
#                       from greater than 5 to 10 percent.
#                   9 - The instantaneous value is considered correct by the
#                       collecting USGS Water Science Center. A published daily
#                       mean value does not exist and/or no comparison was made.
# value        discharge in cubic feet per second
# precision    digits of precision in the discharge
# remark       optional remark code
#                 Remark  Explanation
#                   <     Actual value is known to be less than reported value.
#                   >     Actual value is known to be greater than reported value.
#                   &     Value is affected by unspecified reasons.
#                   A     Value is affected by ice at the measurement site.
#                   B     Value is affected by backwater at the measurement site.
#                   e     Value has been estimated by USGS personnel.
#                   E     Value was computed from an estimated value.
#                   F     Value was modified due to automated filtering.
#                   K     Value is affected by instrument calibration drift.
#                   R     Rating is undefined for this value.
# ---------
IV_Dir          <- file.path(dir,"Data_original_files/StreamGages/Instantaneous/")
file_pattern    <- "[[:digit:]]{8}_[[:digit:]]{4}_[[:digit:]]{8}.rdb"
files           <- list.files(path=IV_Dir, pattern=file_pattern)

# Pre- and post-2007 data come in different formats
# Load pre-2007 files
Pre07files      <- files[as.integer(substr(files,10,13))<2007]
Pre07sites      <- substr(Pre07files,1,8)
colnames   <- c("site_no",  "date_time",  "tz_cd",	"dd",	"accuracy_cd",	"val",	"remark")
colClasses <- c("character", "character", "character", "integer","integer","numeric","character")
skip <- 67
ls.Discharge.Inst <- list() # need to do non-2011, and then do 2011 with different colClasses, colnames
for (i in 1:length(Pre07files)){
  ls.Discharge.Inst[[i]] <- read.table(file.path(dir,"Data_original_files/StreamGages/Instantaneous",Pre07files[i]),
                                       header=FALSE, skip=skip,
                                       col.names=colnames, colClasses=colClasses,
                                       fill=TRUE, stringsAsFactors=FALSE)
  ls.Discharge.Inst[[i]]$dates <- as.Date(substr(ls.Discharge.Inst[[i]]$date_time,1,8),format="%Y%m%d")
}

# Load post-2007 files
# Data provided for site 01349711
#    DD parameter   Description
#    02   00060     Discharge, cubic feet per second
#
# Data-value qualification codes included in this output: 
#     A  Approved for publication -- Processing and review completed. 
file_pattern    <- "[[:digit:]]{8}_[[:digit:]]{4}_20150604.txt"
Post07files     <- list.files(path=IV_Dir, pattern=file_pattern)
Post07sites     <- substr(Post07files,1,8)
colnames   <- c("agency_cd",  "site_no",	"date_time",	"tz_cd",	"val",	"remark")
colClasses <- c("character", "character", "character","character","numeric","character")
skip <- c(27,26)
for (i in 1:length(Post07files)){
  ls.Discharge.Inst[[i+length(Pre07files)]] <- read.table(file.path(dir,"Data_original_files/StreamGages/Instantaneous",Post07files[i]),
                                                          header=FALSE, skip=skip[i], sep = "\t",
                                                          col.names=colnames, colClasses=colClasses,
                                                          fill=TRUE, stringsAsFactors=FALSE)
  ls.Discharge.Inst[[i+length(Pre07files)]]$dates <- as.Date(substr(ls.Discharge.Inst[[i+length(Pre07files)]]$date_time,1,10),format="%Y-%m-%d")
}

names(ls.Discharge.Inst) <- c(substr(Pre07files,1,13), substr(Post07files,1,13))

# 3 Daily distribution -----------
source(file.path(dirs$UtilityScriptsDir,"importDSs.R"))
ls.Discharge.Dist <- list()
for (i in 1:length(rowsAdditionalStudy)){
  dat <- importDSs(SitesChar[i],code="00060")
  ls.Discharge.Dist[[i]] <- dat
}
names(ls.Discharge.Dist) <- SitesChar

savefile <- paste(gsub("-","",Sys.Date()),"DailyMeanFlowAndDist.RData",sep="_")
save(ls.Discharge,ls.Discharge.Dist, file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

# 4 Record of Daily Discharge during full period of gage, 00060 ---------
BeginDate <- "1900-01-01"
EndDate   <- Sys.Date()
ls.Discharge.All <- list()
for (i in 1:length(SitesChar)){
  dat <- importDVs(SitesChar[i],code="00060",stat="00003",sdate=BeginDate,edate=EndDate)
  ls.Discharge.All[[i]] <- dat
}
names(ls.Discharge.All) <- SitesChar

#     clean dataset
for (i in 1:length(SitesChar)){
  ls.Discharge.All[[i]] <- cleanUp(ls.Discharge.All[[i]],task="fix",replace=0.1)
}

savefile <- paste(gsub("-","",Sys.Date()),"DailyMeanFlowAllDates.RData",sep="_")
save(ls.Discharge.All, file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

# 5 peaks data - from a file downloaded from USGS which has the annual peaks
# record for every station of interest

#  agency_cd     Agency Code
#  site_no       USGS station number
#  peak_dt       Date of peak streamflow (format YYYY-MM-DD)
#  peak_tm       Time of peak streamflow (24 hour format, 00:00 - 23:59)
#  peak_va       Annual peak streamflow value in cfs
#  peak_cd       Peak Discharge-Qualification codes (see explanation below)
#  gage_ht       Gage height for the associated peak streamflow in feet
#  gage_ht_cd    Gage height qualification codes
#  year_last_pk  Peak streamflow reported is the highest since this year
#  ag_dt         Date of maximum gage-height for water year (if not concurrent with peak)
#  ag_tm         Time of maximum gage-height for water year (if not concurrent with peak
#  ag_gage_ht    maximum Gage height for water year in feet (if not concurrent with peak
#  ag_gage_ht_cd maximum Gage height code
# Peak Streamflow-Qualification Codes(peak_cd):
#   1 ... Discharge is a Maximum Daily Average
#   2 ... Discharge is an Estimate
#   3 ... Discharge affected by Dam Failure
#   4 ... Discharge less than indicated value,
#           which is Minimum Recordable Discharge at this site
#   5 ... Discharge affected to unknown degree by
#           Regulation or Diversion
#   6 ... Discharge affected by Regulation or Diversion
#   7 ... Discharge is an Historic Peak
#   8 ... Discharge actually greater than indicated value
#   9 ... Discharge due to Snowmelt, Hurricane,
#           Ice-Jam or Debris Dam breakup
#   A ... Year of occurrence is unknown or not exact
#   B ... Month or Day of occurrence is unknown or not exact
#   C ... All or part of the record affected by Urbanization,
#            Mining, Agricultural changes, Channelization, or other
#   D ... Base Discharge changed during this year
#   E ... Only Annual Maximum Peak available for this year
#
# Gage height qualification codes(gage_ht_cd,ag_gage_ht_cd):
#   1 ... Gage height affected by backwater
#   2 ... Gage height not the maximum for the year
#   3 ... Gage height at different site and(or) datum
#   4 ... Gage height below minimum recordable elevation
#   5 ... Gage height is an estimate
#   6 ... Gage datum changed during this year
colnames <- c("agency_cd",  "site_no",	"peak_dt",	"peak_tm",	"peak_va",	"peak_cd",
              "gage_ht",	"gage_ht_cd",	"year_last_pk",	"ag_dt",	"ag_tm",	"ag_gage_ht",	"ag_gage_ht_cd")
colClasses <- c("character","character","character",  "character","integer",  "character",
                "numeric","character",  "integer",    "character",   "character", "character", "character")
filename <- file.path(dirs$OrigDataDir,"01091500_peak")
skip <- 68
PeaksDat <- read.table(filename,header=FALSE, sep="\t", skip=skip,
                       col.names=colnames, colClasses=colClasses,
                       fill=TRUE, stringsAsFactors=FALSE)
PeaksDat$peak_dt <- as.Date(PeaksDat$peak_dt)
PeaksDat[PeaksDat$ag_dt=="","ag_dt"] <- NA
PeaksDat$ag_dt <- as.Date(PeaksDat$ag_dt)

# separate for each gage
ls.Discharge.Peaks        <- lapply(unique(SitesChar), function(i) PeaksDat[PeaksDat$site_no==i,])
names(ls.Discharge.Peaks) <- unique(SitesChar)
PeakCodes                 <- lapply(ls.Discharge.Peaks,"[[","peak_cd")
AnyDailyAverage           <- sapply(1:length(PeakCodes), function(i) any(grepl("1",PeakCodes[[i]])))
HasDailyAverage           <- which(AnyDailyAverage)

# SAVE --------
savefile <- paste(gsub("-","",Sys.Date()),"Discharge_Dist_Peaks_Instant.RData",sep="_")
save(ls.Discharge.All,ls.Discharge,ls.Discharge.Dist,ls.Discharge.Inst,ls.Discharge.Peaks, file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

# GET ANNUAL MAX DAILY MEANS FOR ALL SITES
ls.Discharge.Annual.Max <- list()
SitesChar <- names(ls.Discharge.All)
for (i in SitesChar){
  Dates <- ls.Discharge.All[[i]]$dates
  Flow  <- ls.Discharge.All[[i]]$val
  Years <- unique(format.Date(Dates,"%Y"))
  AnnualMaxes <- sapply(Years, function(y) max(Flow[format.Date(Dates,"%Y") == y]))
  ls.Discharge.Annual.Max[[i]]$val  <- AnnualMaxes
  ls.Discharge.Annual.Max[[i]]$year <- Years
}

# SAVE --------
savefile <- paste(gsub("-","",Sys.Date()),"Discharge_Dist_Peaks_Instant.RData",sep="_")
save(ls.Discharge.All,ls.Discharge,ls.Discharge.Dist,ls.Discharge.Inst,ls.Discharge.Peaks,ls.Discharge.Annual.Max file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
