# Finding bridges close to Ruma's gages
load("~/bridge.collapses/Data/df.USgauges.RData")
load("/Users/mflint/Documents/Research/Climate_Scour_Local/R_data/FrequentlyUsedArchiveData/NBI_bridges_over_water.RData")
load("/Users/mflint/bridge.collapses/Data/ls.Adj.States.RData")
load("~/bridge.collapses/Data/Input/Standard/df.States.RData")

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
# only returned 2 matches, will just need to recalculate nearest bridges -----

StatesAdj <- unique(unlist(ls.Adj.States[STAID_States]))
STFIPS <- sapply(StatesAdj, function(s) df.States[df.States$STATE_CODE==s,"STFIPS_C"])
nbiDataFrame <- df.USbridges.rivers[df.USbridges.rivers$STFIPS %in% STFIPS,c("STFIPS","LONGDD","LATDD")]
gageDataFrame <- df.USgauges[df.USgauges$STAID %in% STAID,]
rm(df.USbridges.rivers,df.USgauges)


ls.Bridges20 <- list()
ls.Bridges100 <- list()
for (j in STFIPS){
  BridgesInState <- nbiDataFrame[nbiDataFrame$STFIPS==j,]
  BridgesInState[,c("LATR","LONGR")] <- pi/180*BridgesInState[,c("LATDD","LONGDD")]
  gagesInAdjState <- gageDataFrame[gageDataFrame$STFIPS %in% ls.Adj.STFIPS[[j]],]
  rownames(gagesInAdjState) <- gagesInAdjState$STAID
  gagesInAdjState[,c("LATR","LONGR")] <- pi/180*gagesInAdjState[,c("LAT_GAGE","LNG_GAGE")]
  if(nrow(gagesInAdjState)>1 && nrow(BridgesInState) >1){
    lat1 = replicate(nrow(gagesInAdjState),BridgesInState$LATR)
    long1 = replicate(nrow(gagesInAdjState),BridgesInState$LONGR)
    lat2 = t(replicate(nrow(BridgesInState),gagesInAdjState$LATR))
    long2 = t(replicate(nrow(BridgesInState),gagesInAdjState$LONGR))
  }else{
    if(nrow(gagesInAdjState)==1){
      lat1 = BridgesInState$LATR
      long1 = BridgesInState$LONGR
      lat2 = replicate(nrow(BridgesInState),gagesInAdjState$LATR)
      long2 = replicate(nrow(BridgesInState),gagesInAdjState$LONGR)
    }else{ # no gages
      next
    }
  }
  
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * 6371 # Earth mean radius [km]
  d <- as.matrix(d)
  rownames(d) <- rownames(BridgesInState)
  colnames(d) <- rownames(gagesInAdjState)
  within20 <- d<20
  gagesWith20km   <- colnames(d)[apply(within20,MARGIN = 2, sum)>0]
  ls.Bridges20[[j]] <- lapply(gagesWith20km, function(g) d[,g][within20[,g]])
  names(ls.Bridges20[[j]]) <- gagesWith20km
  within100 <- d<100
  gagesWith100km   <- colnames(d)[apply(within100,MARGIN = 2, sum)>0]
  ls.Bridges100[[j]] <- lapply(gagesWith100km, function(g) d[,g][within100[,g]])
  names(ls.Bridges100[[j]]) <- gagesWith100km
}
ls.Bridges20 <- ls.Bridges20[sapply(names(ls.Bridges20), function(i) length(ls.Bridges20[[i]])>0)]
ls.Bridges100 <- ls.Bridges100[sapply(names(ls.Bridges100), function(i) length(ls.Bridges100[[i]])>0)]
# now try to compile
ls.gages20 <- list()
ls.gages100 <- list()
for (j in STAID){
  ls.gages20[[j]]  <- lapply(STFIPS, function(s) ls.Bridges20[[s]][j])
  bridgeRows <- lapply(names(ls.gages20[[j]]), function(i) names(ls.gages20[[j]][[i]]))
  temp <- data.frame
  ls.gages100[[j]] <- lapply(STFIPS, function(s) ls.Bridges100[[s]][j])
}
ls.gages20 <- ls.gages20[sapply(names(ls.gages20), function(i) !is.null(ls.gages20[[i]]))]