# Finding bridges close to Ruma's gages
load("~/bridge.collapses/Data/df.USgauges.RData")
STAID <- read.csv("/Volumes/GoogleDrive/My Drive/Advising/Fahmidah/ruma_gage_list.csv",colClasses = "character",
                  stringsAsFactors = FALSE)
STAID <- STAID$USGS.STATION.ID
STAID <- STAID[!duplicated(STAID)]
STAID_States <- as.character(unlist(sapply(STAID, function(s) df.USgauges[df.USgauges$STAID==s,"STATE"])))
STAID_States <- STAID_States[!duplicated(STAID_States)]
ls.Bridges <- list();
for( i in STAID_States){
  load(paste0("/Users/mflint/Documents/Research/Climate_Scour_Local/R_data/Analysis_Results_In_Progress/20151021_MatchingNBIbridgesToGages/20151021.nbiBridgesWithGage5km.",i,".RData"))
  STAID_i = BridgesWithGage5km$STAID
  if(any(STAID_i %in% STAID)){
    ls.Bridges[[i]] <- BridgesWithGage5km[BridgesWithGage5km$STAID %in% STAID,]
  }
}
# only returned 2 matches, will just need to recalculate nearest bridges
source(file.path("Scripts","helper",'gcd_slc.R'))
source(file.path("Scripts","helper",'deg2rad.R'))
load("/Users/mflint/bridge.collapses/Data/ls.Adj.States.RData")
StatesAdj <- unique(unlist(ls.Adj.States[STAID_States]))
STFIPS <- sapply(StatesAdj, function(s) df.States[df.States$STATE_CODE==s,"STFIPS_C"])
load("/Users/mflint/Documents/Research/Climate_Scour_Local/R_data/FrequentlyUsedArchiveData/NBI_bridges_over_water.RData")
nbiDataFrame <- df.USbridges.rivers[df.USbridges.rivers$STFIPS %in% STFIPS,c("STFIPS","LONGDD","LATDD")]
gageDataFrame <- df.USgauges[df.USgauges$STAID %in% STAID,]
R <- 6371 # Earth mean radius [km]
ls.Bridges20 <- list()
for (j in STFIPS){
  BridgesInState <- nbiDataFrame[nbiDataFrame$STFIPS==j,]
  BridgesInState$LATR <- deg2rad(BridgesInState$LATDD)
  BridgesInState$LONGR <- deg2rad(BridgesInState$LONGDD)
  gagesInAdjState <- gageDataFrame[gageDataFrame$STFIPS %in% ls.Adj.STFIPS[[j]],]
  rownames(gagesInAdjState) <- gagesInAdjState$STAID
  gagesInAdjState$LATR <- deg2rad(gagesInAdjState$LAT_GAGE)
  gagesInAdjState$LONGR <- deg2rad(gagesInAdjState$LNG_GAGE)
  lat1 = replicate(nrow(gagesInAdjState),BridgesInState$LATR)
  long1 = replicate(nrow(gagesInAdjState),BridgesInState$LONGR)
  lat2 = t(replicate(nrow(BridgesInState),gagesInAdjState$LATR))
  long2 = t(replicate(nrow(BridgesInState),gagesInAdjState$LONGR))
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  rownames(d) <- rownames(BridgesInState)
  colnames(d) <- rownames(gagesInAdjState)
  within20 <- d<20
  gagesWith20km   <- colnames(d)[apply(within20,MARGIN = 2, sum)>0]
  ls.Bridges20[[j]] <- lapply(gagesWith20km, function(g) rownames(d)[within20] # stopped here
  bridgesWithin20km <- rownames(BridgesInState)[which(MinDist<=20)]
  
  BridgesWithGage20km <- BridgesInState[Within20km,c("ITEM8","STFIPS","ITEM3","ITEM6A","LATDD","LONGDD")]
  BridgesWithGage20km$DIST_TO_GAGE <- MinDist[Within20km]
  cols <- c("STAID","STANAME","DRAIN_SQKM","LAT_GAGE","LNG_GAGE","STFIPS_GAGE","FIPS_GAGE","STREAM_NAME_GAGE","STREAM_TYPE_GAGE","STREAM_OTHER_GAGE","STREAM_TRIB_GAGE")
  colsG <- c("STAID","STANAME","DRAIN_SQKM","LAT_GAGE","LNG_GAGE","STFIPS","FIPS_GAGE","STREAM_NAME_GAGE","STREAM_TYPE_GAGE","STREAM_OTHER_GAGE","STREAM_TRIB_GAGE")
  BridgesWithGage20km[,cols] <- gagesInAdjState[BridgeGageWhichDistMin[Within20km],colsG]
  
  savefile <- paste(gsub("-","",Sys.Date()),"nbiBridgesWithGage20km",df.States[df.States$STFIPS==j,"STATE_CODE"],"RData",sep=".")
  save(BridgesWithGage20km, file=file.path(dirs$DataDirAnalysis,"20151021_MatchingNBIbridgesToGages",savefile))
  rm(savefile)
}
