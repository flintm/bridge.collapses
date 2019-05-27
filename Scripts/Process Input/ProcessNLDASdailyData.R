# 2015-05-21 takes NLDAS daily mean or cumulative .nc files and calculates return period of failure event
source(file.path(dirsGit$Scripts,"gcd_slc.R"))
source(file.path(dirsGit$Scripts,"deg2rad.R"))

# setup
library(ncdf4)
processType <- "mean"
# processType <- "cumulative"
NLDAS_Dir   <- file.path(dirs$OrigDataDir,"NLDAS_SSRUN_Daily")
     
# load NLDAS data for given processType
files           <- list.files(path=NLDAS_Dir, pattern=processType)
if (processType == "mean") yr_start <- 28
if (processType == "cumulative") yr_start <- 34

years           <- substr(files,yr_start,yr_start+3)
NLDAS_daily_all <- list()
for (i in 1:length(files)){
  NLDAS_daily <- nc_open(file.path(NLDAS_Dir,files[i]))
  varid       <- NLDAS_daily$var[[1]]$name
  varsize     <- NLDAS_daily$var[[1]]$varsize
  ndims       <- NLDAS_daily$var[[1]]$ndims
  nlat        <- NLDAS_daily$dim$lat$len
  nlon        <- NLDAS_daily$dim$lon$len
  ntime       <- NLDAS_daily$dim$time$len
  Atts        <- ncatt_get(NLDAS_daily,varid=varid)
  SSRUN_daily <- ncvar_get(NLDAS_daily,varid=varid)
  lat         <- ncvar_get(NLDAS_daily,varid="lat")
  lon         <- ncvar_get(NLDAS_daily,varid="lon")
  days        <- ncvar_get(NLDAS_daily,varid="time")
  days        <- as.Date(as.character(days),format="%Y%m%d")
  NLDAS_daily_all[[i]] <- list(days=days,lat=lat,lon=lon,SSRUN=SSRUN_daily,Atts=Atts,nlat=nlat,nlon=nlon,ntime=ntime)
  nc_close(NLDAS_daily)
}
names(NLDAS_daily_all) <- years
rm(NLDAS_daily)

if (processType=="mean")       savefile <- paste(gsub("-","",Sys.Date()),"NLDAS_Daily_Means",length(years),"years.RData",sep="_")
if (processType=="cumulative") savefile <- paste(gsub("-","",Sys.Date()),"NLDAS_Daily_Cumulative",length(years),"years.RData",sep="_")
save(NLDAS_daily_all, file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
     
# ----------------- GET FAILURE RUNOFF, OTHER IMPORTANT VALUES ---------------------------------
# pull locations of failed bridges (post-1979) and failure years, dates in order to perform analysis
# (for right now, make sure to check that have the data available)
# note that do not need to interpolate, need to find nearest neighbor grid cell because the grid point is
# actually the centroid of that cell-e.g., the runoff will contain the runoff value at the bridge
if (processType=="mean")       loadfile <- "20150604_NLDAS_Daily_Means_36_years.RData"
if (processType=="cumulative") loadfile <- paste(gsub("-","",Sys.Date()),"NLDAS_Daily_Cumulative",length(years),"years.RData",sep="_")
load(file.path(dirs$DataDirFrequent,loadfile))
     
rowsToAnalyzeNLDAS    <- rowsToView[format.Date(df.Fail.NBI.Gage[rowsToView,"YR_FAIL_EST"],"%Y") %in% years]
nSites                <- length(rowsToAnalyzeNLDAS)
HasFailDate           <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"BOOL_HAS_FAIL_DATE"]
NoFailDate            <- !HasFailDate
HasFailIndex          <- c(1:length(rowsToAnalyzeNLDAS))[HasFailDate]
NoFailIndex           <- c(1:length(rowsToAnalyzeNLDAS))[NoFailDate]
HasYrBlt              <- !is.na(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"YR_BLT_EST"])
NoYrBlt               <- !HasYrBlt
HasYrBltIndex         <- c(1:length(rowsToAnalyzeNLDAS))[HasYrBlt]
NoYrBltIndex          <- c(1:length(rowsToAnalyzeNLDAS))[NoYrBlt]
YrBltIndex            <- rep(NA_integer_,nSites)
n                     <- 2 # pm dates

LatsBridge    <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"LATDD"]
LonsBridge    <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"LONGDD"]
LatsGage      <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"LAT_GAGE"]
LonsGage      <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"LNG_GAGE"]
LatsAll       <- list(LatsBridge, LatsGage)
LonsAll       <- list(LonsBridge, LonsGage)
Dates         <- as.Date(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DATE_FAIL_EST_USGS"])
Years         <- format.Date(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DATE_FAIL_EST_USGS"],"%Y")

Lon_index           <- list(bridge = rep(NA_integer_,nSites), gage = rep(NA_integer_,nSites))
Lat_index           <- list(bridge = rep(NA_integer_,nSites), gage = rep(NA_integer_,nSites))
Lon_NLDAS           <- list(bridge = rep(NA,nSites), gage = rep(NA,nSites))
Lat_NLDAS           <- list(bridge = rep(NA,nSites), gage = rep(NA,nSites))
Day_index           <- rep(NA_integer_,nSites)
YrBltIndex          <- rep(NA_integer_,nSites)
BeginFailYearIndex  <- rep(NA_integer_,nSites)
EndFailYearIndex    <- rep(NA_integer_,nSites)
min_dist            <- list(bridge = rep(NA_integer_,nSites), gage = rep(NA_integer_,nSites))
FailRunoff          <- list(bridge = rep(NA_integer_,nSites), gage = rep(NA_integer_,nSites))
FailRunoffDate      <- list(bridge = rep(NA_integer_,nSites), gage = rep(NA_integer_,nSites))
MaxRunoffFailYr     <- list(bridge = rep(NA_integer_,nSites), gage = rep(NA_integer_,nSites))
MaxRunoffFailYrDate <- list(bridge = rep(NA_integer_,nSites), gage = rep(NA_integer_,nSites))
MaxRunoff           <- list(bridge = rep(NA_integer_,nSites), gage = rep(NA_integer_,nSites))
MaxRunoffDate       <- list(bridge = rep(NA_integer_,nSites), gage = rep(NA_integer_,nSites))
MaxRunPreFail       <- list(bridge = rep(NA_integer_,nSites), gage = rep(NA_integer_,nSites))
MaxRunPreFailDate   <- list(bridge = rep(NA_integer_,nSites), gage = rep(NA_integer_,nSites))

for (i in 1:length(rowsToAnalyzeNLDAS)){
  ii            <- Years[i]
  for (l in 1:2){
    Lats          <- LatsAll[[l]]
    Lons          <- LonsAll[[l]]
    Lon_index[[l]][i]  <- which(order(c(NLDAS_daily_all[[ii]]$lon,Lons[i]))==(NLDAS_daily_all[[ii]]$nlon+1))
    Lat_index[[l]][i]  <- which(order(c(NLDAS_daily_all[[ii]]$lat,Lats[i]))==(NLDAS_daily_all[[ii]]$nlat+1))
    lon_indices   <- c(Lon_index[[l]][i]-1,Lon_index[[l]][i]-1,Lon_index[[l]][i],Lon_index[[l]][i])
    lat_indices   <- c(Lat_index[[l]][i]-1,Lat_index[[l]][i],Lat_index[[l]][i]-1,Lat_index[[l]][i])
    d             <- numeric(4)
    for (j in 1:4){ # 4 points bounding bridge location
      d[j]        <- gcd_slc(deg2rad(NLDAS_daily_all[[ii]]$lon[lon_indices[j]]),
                             deg2rad(NLDAS_daily_all[[ii]]$lat[lat_indices[j]]),
                             deg2rad(Lons[i]),
                             deg2rad(Lats[i]))
    }
    nearest               <- which.min(d)
    Lon_index[[l]][i]     <- lon_indices[nearest]
    Lon_NLDAS[[l]][i]     <- NLDAS_daily_all[[ii]]$lon[Lon_index[[l]][i]]
    Lat_index[[l]][i]     <- lat_indices[nearest]
    Lat_NLDAS[[l]][i]     <- NLDAS_daily_all[[ii]]$lat[Lat_index[[l]][i]]
    min_dist[[l]][i]      <- d[nearest]
#     
#     RunoffAllTime    <- unlist(lapply(years, function(y) NLDAS_daily_all[[y]]$SSRUN[Lon_index[[l]][i],Lat_index[[l]][i],]))
#     RunoffDates      <- as.Date(unlist(lapply(years, function(y) NLDAS_daily_all[[y]]$days)),"1970-01-01")
#     
#     Day_index[i]  <- which(RunoffDates==Dates[i])
#     FailDateRange <- c((Day_index[i]-n):(Day_index[i]+n))
#     
#     YrBltIndex[i] <- ifelse(HasYrBlt[i],
#                             which.min(abs(as.Date(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[i],"YR_BLT_EST"],"1970-01-01") - RunoffDates)),
#                             1)
#     BeginFailYearIndex[i] <- which.min(abs(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[i],"YR_FAIL_EST"] - RunoffDates))
#     EndFailYearIndex[i]   <- which.min(abs(as.Date(paste(substr(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS[i],"YR_FAIL_EST"],1,4),"12","31",sep="-")) - RunoffDates))
#     
#     RunoffPreFail      <- RunoffAllTime[RunoffDates <= (Dates[i] + n) & RunoffDates >= YrBltIndex[i]]
#     RunoffPreFailDates <- RunoffDates[RunoffDates <= (Dates[i] + n) & RunoffDates >= YrBltIndex[i]]
#     
#     largest                <- which.max(RunoffAllTime[FailDateRange])
#     FailRunoff[[l]][i]     <- RunoffAllTime[FailDateRange][largest]
#     FailRunoffDate[[l]][i] <- RunoffDates[FailDateRange][largest]
#     
#     Index                 <- which.max(RunoffAllTime)
#     MaxRunoff[[l]][i]     <- RunoffAllTime[Index]
#     MaxRunoffDate[[l]][i] <- as.Date(RunoffDates[Index],"1970-01-01",format="%Y-%m-%d")
#     
#     Index                       <- which.max(RunoffAllTime[BeginFailYearIndex[i]:EndFailYearIndex[i]])
#     MaxRunoffFailYr[[l]][i]     <- RunoffAllTime[BeginFailYearIndex[i]:EndFailYearIndex[i]][Index]
#     MaxRunoffFailYrDate[[l]][i] <- as.Date(RunoffDates[BeginFailYearIndex[i]:EndFailYearIndex[i]][Index],"1970-01-01",format="%Y-%m-%d")
#     
#     Index                <- which.max(RunoffPreFail)
#     MaxRunPreFail[[l]][i]     <- RunoffPreFail[Index]
#     MaxRunPreFailDate[[l]][i] <- as.Date(RunoffPreFailDates[Index],"1970-01-01",format="%Y-%m-%d")
    
#     notNA                <- !is.na(RunoffAllTime[RunoffDates <= Dates[i] & RunoffDates >= YrBltIndex[i]])
#     NoTimesRunExc[i]     <- sum(RunoffAllTime[RunoffDates <= Dates[i] & RunoffDates >= YrBltIndex[i]][notNA] >= FailRunoff[[l]][i])
  }
}
     
# Store results ----------------
colBase <- c("Q_FAILPM2_NLDAS","DATE_FAILPM2_NLDAS","Q_MAX_NLDAS","DATE_MAX_NLDAS","Q_MAXPREFAIL_NLDAS",
             "DATE_MAXPREFAIL_NLDAS","Q_FAILYRMAX_NLDAS","DATE_FAILYRMAX_NLDAS")
colLocBase <- c("LONG_NLDAS","LAT_NLDAS")
if (processType=="mean"){
  for (l in 1:2){
    Base <- switch(l,
                   "1" = "B",
                   "2" = "G")
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[1],Base,sep="")]      <- FailRunoff[[l]]
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[2],Base,sep="")]      <- as.Date(FailRunoffDate[[l]],"1970-01-01",format="%Y-%m-%d")
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[3],Base,sep="")]      <- MaxRunoff[[l]]
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[4],Base,sep="")]      <- as.Date(MaxRunoffDate[[l]],"1970-01-01",format="%Y-%m-%d")
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[5],Base,sep="")]      <- MaxRunPreFail[[l]]
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[6],Base,sep="")]      <- as.Date(MaxRunPreFailDate[[l]],"1970-01-01",format="%Y-%m-%d")
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[7],Base,sep="")]      <- MaxRunoffFailYr[[l]]
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[8],Base,sep="")]      <- as.Date(MaxRunoffFailYrDate[[l]],"1970-01-01",format="%Y-%m-%d")
#     # df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[9],Base,sep="")]    <- NoTimesRunExc[[l]]
    
    df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colLocBase[1],Base,sep="")]   <- Lon_NLDAS[[l]]
    df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colLocBase[2],Base,sep="")]   <- Lat_NLDAS[[l]]
  }
}
# if (processType=="cumulative"){
#   for (l in 1:2){
#     Base <- switch(l,
#                    "1" = "CUMUL_BRIDGE",
#                    "2" = "CUMUL_GAGE")
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[1],Base,sep="_")]      <- FailRunoff[[l]]
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[2],Base,sep="_")]      <- FailRunoffDate[[l]]
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[3],Base,sep="_")]      <- MaxRunoff[[l]]
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[4],Base,sep="_")]      <- MaxRunoffDate[[l]]
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[5],Base,sep="_")]      <- MaxRunoffFailYr[[l]]
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[6],Base,sep="_")]      <- MaxRunoffFailYrDate[[l]]
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[7],Base,sep="_")]      <- MaxRunPreFail[[l]]
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[8],Base,sep="_")]      <- MaxRunPreFailDate[[l]]
#     df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,paste(colBase[9],Base,sep="_")]      <- NoTimesRunExc[[l]]
#   }
# }


# df.Fail.NBI.Gage$NLDAS_MAX_SAME_EVENT_BRIDGE  <- abs(as.Date(df.Fail.NBI.Gage$DATE_MAX_NLDASB,"1970-01-01") - as.Date(df.Fail.NBI.Gage$DATE_MAX_D_USGS)) <=3
# df.Fail.NBI.Gage$NLDAS_MAX_SAME_EVENT_GAGE    <- abs(as.Date(df.Fail.NBI.Gage$DATE_MAX_NLDASG,"1970-01-01") - as.Date(df.Fail.NBI.Gage$DATE_MAX_D_USGS)) <=3
# df.Fail.NBI.Gage$NLDAS_PRE_F_MAX_SAME_BRIDGE  <- abs(as.Date(df.Fail.NBI.Gage$DATE_MAXPREFAIL_NLDASB,"1970-01-01") - as.Date(df.Fail.NBI.Gage$DATE_MAXPREFAIL_D_USGS)) <=3
# df.Fail.NBI.Gage$NLDAS_PRE_F_MAX_SAME_GAGE    <- abs(as.Date(df.Fail.NBI.Gage$DATE_MAXPREFAIL_NLDASG,"1970-01-01") - as.Date(df.Fail.NBI.Gage$DATE_MAXPREFAIL_D_USGS)) <=3

savefile <- paste(gsub("-","",Sys.Date()),"withNLDASbridgeAndGageLocation.RData",sep="")
save(df.Fail.NBI.Gage, file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage, rowsToView, IDsToView, file=file.path(dirsGit$Data,savefile))
rm(savefile)
  
# SAVE FLOW VALUES AT SITES OF INTEREST ---------------------
STAIDtoAnalyzeNLDAS <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"STAID"]
IDsToAnalyzeNLDAS   <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"ID"]

years <- as.character(1979:2014)

ls.NLDAS.Bridge     <- list()
ls.NLDAS.Gage       <- list()
for (i in 1:length(rowsToAnalyzeNLDAS)){
  ID    <- as.character(IDsToAnalyzeNLDAS[i])
  STAID <- as.character(STAIDtoAnalyzeNLDAS[i])
  
  ls.NLDAS.Bridge[[ID]]$SSRUN  <- unlist(lapply(years, function(y) NLDAS_daily_all[[y]]$SSRUN[Lon_index[[1]][i],Lat_index[[1]][i],]))
  ls.NLDAS.Bridge[[ID]]$Date   <- as.Date(unlist(lapply(years, function(y) NLDAS_daily_all[[y]]$days)),"1970-01-01")
  
  ls.NLDAS.Gage[[STAID]]$SSRUN <- unlist(lapply(years, function(y) NLDAS_daily_all[[y]]$SSRUN[Lon_index[[2]][i],Lat_index[[2]][i],]))
  ls.NLDAS.Gage[[STAID]]$Date  <- as.Date(unlist(lapply(years, function(y) NLDAS_daily_all[[y]]$days)),"1970-01-01")
}

savefile <- paste(gsub("-","",Sys.Date()),"NLDASGageDailyMean.RData",sep="_")
save(ls.NLDAS.Gage,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

savefile <- paste(gsub("-","",Sys.Date()),"NLDASBridgeDailyMean.RData",sep="_")
save(ls.NLDAS.Bridge,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

ls.NLDAS.Bridge.AnnualMax <- lapply(names(ls.NLDAS.Bridge), 
                                    function(j) sapply(years, 
                                                       function(y) max(ls.NLDAS.Bridge[[j]]$SSRUN[format.Date(ls.NLDAS.Bridge[[j]]$Date,"%Y") == y], 
                                                                       na.rm=TRUE)
                                    )
)
names(ls.NLDAS.Bridge.AnnualMax) <- names(ls.NLDAS.Bridge)

savefile <- paste(gsub("-","",Sys.Date()),"NLDASBridgeAnnualMaxes.RData",sep="_")
save(ls.NLDAS.Bridge.AnnualMax,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

ls.NLDAS.Gage.AnnualMax <- lapply(names(ls.NLDAS.Gage), 
                                  function(j) sapply(years, 
                                                     function(y) max(ls.NLDAS.Gage[[j]]$SSRUN[format.Date(ls.NLDAS.Gage[[j]]$Date,"%Y") == y], 
                                                                     na.rm=TRUE)
                                  )
)
names(ls.NLDAS.Gage.AnnualMax) <- names(ls.NLDAS.Gage)

savefile <- paste(gsub("-","",Sys.Date()),"NLDASGageAnnualMaxes.RData",sep="_")
save(ls.NLDAS.Gage.AnnualMax,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
     
     
