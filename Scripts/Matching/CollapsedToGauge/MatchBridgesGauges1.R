# Script to compute distance from bridge to gauges in state, minimum distance, matched gauge
# Written by Madeleine Flint, 2015-02-17

State_Nos <-c(1:29)

HOME              <- "/home/flint" 
SCRATCH           <- "/scratch/users/flint"
DataDirectory     <- file.path(HOME,"matchbridgesgauges")
ScriptsDirectory  <- file.path(HOME,"matchbridgesgauges")
OutputDirectory   <- file.path(HOME,"matchbridgesgauges")

source(file.path(ScriptsDirectory,"deg2rad.R"))
source(file.path(ScriptsDirectory,"gcd_slc.R"))

# load(file.path(DataDirectory,"MissingStatesSTFIPS.RData"))
load(file.path(DataDirectory,"FailStatesWithMatchesSTFIPS.RData"))
MissingStatesSTFIPS <- FailStatesWithMatchesSTFIPS

load(file.path(DataDirectory,"nbiDataFrameWithStream.RData"))
nbiColsForMatch    <- c("STFIPS", "FIPS", "LATDD", "LONGDD", "ITEM6A","STREAM_TYPE")

load(file.path(DataDirectory,"gaugeDataFrame.RData"))
gaugeColsForMatch  <- c("STFIPS", "STANAME", "LAT_GAGE","LNG_GAGE")

for (State in State_Nos){
  nbiDataFrame          <- nbiDataFrame[nbiDataFrame$STFIPS==MissingStatesSTFIPS[State],nbiColsForMatch]
  nNBI                  <- nrow(nbiDataFrame)
  nbiDataFrame$LONGR    <- deg2rad(nbiDataFrame$LONGDD)
  nbiDataFrame$LATR     <- deg2rad(nbiDataFrame$LATDD)
  
  gaugeDataFrame        <- gaugeDataFrame[gaugeDataFrame$STFIPS==MissingStatesSTFIPS[State],gaugeColsForMatch]
  nGauge                <- nrow(gaugeDataFrame)
  gaugeDataFrame$LONGR  <- deg2rad(gaugeDataFrame$LNG_GAGE)
  gaugeDataFrame$LATR   <- deg2rad(gaugeDataFrame$LAT_GAGE)
  
  nbiBridgeGaugeDist    <- sapply(1:nGauge, function(i) sapply(1:nNBI, function(j) gcd_slc(nbiDataFrame[j,"LONGR"],
                                                                                        nbiDataFrame[j,"LATR"],
                                                                                        gaugeDataFrame[i,"LONGR"],
                                                                                        gaugeDataFrame[i,"LATR" ])))
  if (class(nbiBridgeGaugeDist)=="list") nbiBridgeGaugeDist <- as.data.frame(nbiBridgeGaugeDist)
  NearestGaugeIndex     <- sapply(1:nNBI,function(i) which.min(nbiBridgeGaugeDist[i,]))
  if (class(NearestGaugeIndex)=="list") NearestGaugeIndex <- unlist(NearestGaugeIndex)
  NearestGaugeDist      <- sapply(1:nNBI, function(i) nbiBridgeGaugeDist[i,NearestGaugeIndex[i]])
  save(NearestGaugeDist,NearestGaugeIndex,nbiBridgeGaugeDist,file=file.path(OutputDirectory,paste("nbiBridgeGauge_FailStateNo_",State,".RData",sep="")))
}