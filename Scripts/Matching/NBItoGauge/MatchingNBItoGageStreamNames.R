# Run matching on NBI bridges and nearest gauge to find stream match

library(parallel)
library(stringr) # make some comment

load(file.path(dirs$DataDirFrequent,"StateCountyCityData.RData"))
load(file.path(dirs$DataDirFrequent,"nbiDataFramePreProcessedAllFields.RData"))
nbiDataFrame <- nbiDataFrame[,c("ITEM8","STFIPS","ITEM3","ITEM6A","LATDD","LONGDD")]
load(file.path(dirs$DataDirFrequent,"20150423_gageDataFrameProcessedStreamName.RData"))

source(file.path(dirsGit$Scripts,'gcd_slc.R'))
source(file.path(dirsGit$Scripts,'deg2rad.R'))
source(file.path(dirsGit$Scripts,'MatchEntry.R'))
source(file.path(dirsGit$Scripts,'PerformMatch.R'))


df.Adj.States        <- read.table(file.path(dirs$OrigDataDir,"PoliticalBoundaries","AdjacentStatesList.txt"),skip=1,sep=c(",",";"),stringsAsFactors=FALSE)
ls.Adj.States        <- vector("list",50)
ls.Adj.States        <- lapply(1:length(ls.Adj.States), function(i) df.Adj.States[i,2])
ls.Adj.States        <- lapply(1:length(ls.Adj.States), function(i) unlist(strsplit(ls.Adj.States[[i]],";")))
names(ls.Adj.States) <- df.Adj.States[,1]
NoAdjStates          <- which(sapply(1:length(ls.Adj.States), function(i) length(ls.Adj.States[[i]])==0))
HasAdjStates         <- which(sapply(1:length(ls.Adj.States), function(i) length(ls.Adj.States[[i]])!=0))
ls.Adj.STFIPS        <- lapply(HasAdjStates, function(i) sapply(1:length(ls.Adj.States[[i]]), 
                                                                function(j) df.States[df.States$STATE_CODE==ls.Adj.States[[i]][j],"STFIPS"]))
names(ls.Adj.STFIPS) <- sapply(HasAdjStates, function(i) as.character(df.States[df.States$STATE_CODE==df.Adj.States[i,1],"STFIPS"]))
NoAdjStatesSTFIPS    <- sapply(NoAdjStates, function(i) as.character(df.States[df.States$STATE_CODE==df.Adj.States[i,1],"STFIPS"]))
ls.Adj.STFIPS[NoAdjStatesSTFIPS] <- lapply(NoAdjStates, function(i) integer(0))

# loop over data from all states and find bridges with close gauges
STFIPS <- df.States[!df.States$STATE_CODE %in% c("PR","AK","HI"),"STFIPS"]
for (j in STFIPS){
  BridgesInState <- nbiDataFrame[nbiDataFrame$STFIPS==j,]
  BridgesInState$LATR <- deg2rad(BridgesInState$LATDD)
  BridgesInState$LONGR <- deg2rad(BridgesInState$LONGDD)
  gagesInAdjState <- gageDataFrame[gageDataFrame$STFIPS %in% ls.Adj.STFIPS[[as.character(j)]],]
  gagesInAdjState$LATR <- deg2rad(gagesInAdjState$LAT_GAGE)
  gagesInAdjState$LONGR <- deg2rad(gagesInAdjState$LNG_GAGE)
  BridgeGageWhichDistMin <- sapply(1:nrow(BridgesInState), function(i) which.min(gcd_slc(rep(BridgesInState$LONGR[i],nrow(gagesInAdjState)),
                                                                                         rep(BridgesInState$LATR[i],nrow(gagesInAdjState)),
                                                                                              gagesInAdjState$LONGR,
                                                                                              gagesInAdjState$LATR))[1],
                                   simplify = TRUE)
  MinDist <- sapply(1:nrow(BridgesInState), function(i) gcd_slc(BridgesInState[i,"LONGR"],
                                                                BridgesInState[i,"LATR"],
                                                                gagesInAdjState[BridgeGageWhichDistMin[i],"LONGR"],
                                                                gagesInAdjState[BridgeGageWhichDistMin[i],"LATR"]))
  Within5km <- which(MinDist<=5)
  BridgesWithGage5km <- BridgesInState[Within5km,c("ITEM8","STFIPS","ITEM3","ITEM6A","LATDD","LONGDD")]
  BridgesWithGage5km$DIST_TO_GAGE <- MinDist[Within5km]
  cols <- c("STAID","STANAME","DRAIN_SQKM","LAT_GAGE","LNG_GAGE","STFIPS_GAGE","FIPS_GAGE","STREAM_NAME_GAGE","STREAM_TYPE_GAGE","STREAM_OTHER_GAGE","STREAM_TRIB_GAGE")
  colsG <- c("STAID","STANAME","DRAIN_SQKM","LAT_GAGE","LNG_GAGE","STFIPS","FIPS_GAGE","STREAM_NAME_GAGE","STREAM_TYPE_GAGE","STREAM_OTHER_GAGE","STREAM_TRIB_GAGE")
  BridgesWithGage5km[,cols] <- gagesInAdjState[BridgeGageWhichDistMin[Within5km],colsG]
  
  savefile <- paste(gsub("-","",Sys.Date()),"nbiBridgesWithGage5km",df.States[df.States$STFIPS==j,"STATE_CODE"],"RData",sep=".")
  save(BridgesWithGage5km, file=file.path(dirs$DataDirAnalysis,"20151021_MatchingNBIbridgesToGages",savefile))
  rm(savefile)
}

# now check stream name ---- SHASHANK
goodMatches <- list()
nBridgesGageWithin5 <- 0
for (j in STFIPS){
  loadfile <- paste(gsub("-","",Sys.Date()),"nbiBridgesWithGage5km",df.States[df.States$STFIPS==j,"STATE_CODE"],"RData",sep=".")
  load(file.path(dirs$DataDirAnalysis,"20151021_MatchingNBIbridgesToGages",loadfile))
  if (nrow(BridgesWithGage5km)!=0){
    nBridgesGageWithin5 <- nBridgesGageWithin5 + nrow(BridgesWithGage5km)
    Matched <- rep(FALSE,nrow(BridgesWithGage5km))
    for (i in 1:nrow(BridgesWithGage5km)){
      FailEntry <-  BridgesWithGage5km[i,c("STANAME","STREAM_NAME_GAGE","STREAM_TYPE_GAGE")]
      print("-----")
      print(BridgesWithGage5km[i,c("STANAME","STREAM_NAME_GAGE","DIST_TO_GAGE")])
      matchData <- BridgesWithGage5km[i,]
      print(matchData[,"ITEM6A"])
      PossibleMatchRows <- PerformMatch(FailEntry, matchData, "nbiGage", VERBOSE = TRUE)
      print(PossibleMatchRows)
      if (PossibleMatchRows >= 1){
        Matched[i] <- TRUE
      }
    }
    if(any(Matched)) goodMatches[[df.States[df.States$STFIPS==j,"STATE_CODE"]]] <- BridgesWithGage5km[Matched,]
  }
}
nbiBridgesWithMatchedGages5km <- do.call("rbind",goodMatches)
savefile <- paste(gsub("-","",Sys.Date()),"nbiBridgesWithMatchedStreamGage5km","RData",sep=".")
save(nbiBridgesWithMatchedGages5km, file=file.path(dirs$DataDirAnalysis,"20151021_MatchingNBIbridgesToGages",savefile))

savefile <- paste(gsub("-","",Sys.Date()),"nbiBridgesWithMatchedStreamGage5km","txt",sep=".")
write.table(nbiBridgesWithMatchedGages5km, file = file.path(dirs$DataDirAnalysis,"20151021_MatchingNBIbridgesToGages",savefile),
            sep = "\t")
rm(savefile)
