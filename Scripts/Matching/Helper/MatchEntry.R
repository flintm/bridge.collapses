# 2015-06-09 modified for possibility of dam matching
# 2015-04-20 modified from 2015-01-26 version to include possibility of gage matching
# relies on 2015-04-20 version of PerformMatch
MatchEntry <- function(FailEntryRow){
# for a given bridge in FailDataFrame, select possible matches in nbiDataFrame or gageDataFrame
# written such that works for either stream, road, route, or location match
# 2015-04-30 modified to check for streams based on correct type, then all types if no match
  
if (MatchType=="road"){
    if (FailDataFrame[FailEntryRow, "ROUTE_NO"]!=""){
      MatchType <- "route"
      FailEntry <- FailDataFrame[FailEntryRow, c("ID", "STFIPS", "FIPS", "FIPS_2", "FIPS_FROM_CITY", "FIPS_FROM_CITY_2", "FIPS_FROM_CITY_3", "LOCATION", "ROUTE_NO", "ROAD_TYPE", "ITEM5B")]
    }
    else{ # i.e., if it's a road and not a route
      FailEntry <- FailDataFrame[FailEntryRow, c("ID", "STFIPS", "FIPS", "FIPS_2", "FIPS_FROM_CITY", "FIPS_FROM_CITY_2", "FIPS_FROM_CITY_3", "LOCATION", "ROAD_NAME", "ROAD_TYPE")] 
    }
}
else {
  if (MatchType=="stream"){
    FailEntry <- FailDataFrame[FailEntryRow, c("ID", "STFIPS", "FIPS", "FIPS_2", "FIPS_FROM_CITY", "FIPS_FROM_CITY_2", "FIPS_FROM_CITY_3", "FEAT_UND", "STREAM_NAME", "STREAM_TYPE")]
  }
  else{
    if (MatchType=="gage" | MatchType=="dam"){
      FailEntry <- FailDataFrame[FailEntryRow, c("ID", "STFIPS", "FEAT_UND", "STREAM_NAME", "STREAM_TYPE")]
    }
    else{ 
      stop("MatchType undefined")
    }
  } 
}
if (VERBOSE==TRUE) print(paste("ID of FailEntry is: ",FailEntry$ID, ", and MatchType is: ",MatchType, sep = ""))

PossibleMatchRows = list()
PossibleMatchRows[[1]] = paste("ID",FailEntry$ID,sep="")

if (MatchType %in% c("road","route","stream")){
  MatchRowNames     <- rownames(nbiDataFrame)
  nbiBridgesInState <- MatchRowNames[nbiDataFrame$STFIPS == FailEntry$STFIPS]
  nbiDataState      <- nbiDataFrame[nbiBridgesInState,]
  
  CountyCols <- c("FIPS","FIPS_2", "FIPS_FROM_CITY", "FIPS_FROM_CITY_2", "FIPS_FROM_CITY_3")
  CountyCols <- CountyCols[FailEntry[,CountyCols]!="" & !is.na(FailEntry[,CountyCols])]
  
  # if have already conducted a match and only want to search in subset, load
  if (LoadFromFile & MatchType != "gage"){
    #print("  attempting to load from file")
    pattern   <- paste("ID",FailEntry$ID,"-r",sep="")
    matchFile <- list.files(path=OutputDirectory, pattern = pattern)[1]
    load(file.path(OutputDirectory,matchFile))
    SubsetPossibleMatches <- PossibleMatchRows[2:length(PossibleMatchRows)]
    SubsetPossibleMatchRows <- SubsetPossibleMatches[!grepl("-",SubsetPossibleMatches) & !grepl("[[:alpha:]]",SubsetPossibleMatches)]
    rm(PossibleMatchRows)
    if (length(SubsetPossibleMatchRows)>=1){ # if had a viable subset, use those rownames only  
      nbiBridgesInState <- nbiBridgesInState[nbiBridgesInState %in% SubsetPossibleMatchRows]
      # print("  successfully used subset")
    }
  }
  
  for (j in CountyCols){
    if (VERBOSE==TRUE) ifelse(MatchType=="route", print("checking in a county with correct route prefix"), print("  checking in a county"))
    PossibleMatchRows[[length(PossibleMatchRows)+1]] <- paste("ID", FailEntry$ID,"-FIPS",FailEntry[,j],sep="")
    nbiBridgesInCounty   <- nbiBridgesInState[nbiDataState$FIPS == FailEntry[,j]]
    # print(paste("# of bridges in county:", length(nbiBridgesInCounty),sep=" "))
    if (MatchType == "route"){ # only look at bridges with correct type of road 
      if (!is.na(FailEntry$ITEM5B)){
        nbiBridgesInCountyWithRte <- nbiBridgesInCounty[nbiDataState[nbiBridgesInCounty,"ITEM5B"] == FailEntry$ITEM5B]
        #  print(nbiBridgesInCountyWithRte)
        nbiDataState[nbiBridgesInCountyWithRte,"ITEM5D"] <- sapply(1:length(nbiBridgesInCountyWithRte), function(i) sub("^[0]+","",nbiDataState[nbiBridgesInCountyWithRte[i],"ITEM5D"]))
      }
      else nbiBridgesInCountyWithRte <- nbiBridgesInCounty
    }
    else nbiBridgesInCountyWithRte <- nbiBridgesInCounty
    if (length(nbiBridgesInCountyWithRte) > 0){
      PossibleMatches <- PerformMatch(FailEntry, nbiDataState[nbiBridgesInCountyWithRte,], MatchType)
    }
    else {
      if (VERBOSE==TRUE)     print("had a county with no route bridges")
      PossibleMatches <- ""
    }
    if (MatchType == "route" & PossibleMatches[1] < 0 ){ # if didn't find anything, check other types of road
      if (!is.na(FailEntry$ITEM5B)){
        nbiBridgesInCountyWithWrongRte <- nbiBridgesInCounty[nbiDataState[nbiBridgesInCounty,"ITEM5B"] != FailEntry$ITEM5B]
        nbiDataState[nbiBridgesInCountyWithWrongRte,"ITEM5D"] <- sapply(1:length(nbiBridgesInCountyWithWrongRte), function(i) sub("^[0]+","",nbiDataState[nbiBridgesInCountyWithWrongRte[i],"ITEM5D"]))
        PossibleMatches <- PerformMatch(FailEntry, nbiDataState[nbiBridgesInCountyWithWrongRte,], MatchType)
      }
    }
    PossibleMatchRows <- c(PossibleMatchRows, PossibleMatches)
  }
  # Find bridges where had county & stream/location data, but were unsuccessful in finding a match - county could be wrong, so check whole state     
  DataButNoMatch <- all(grepl("^-",PossibleMatchRows) | grepl("[[:alpha:]]", PossibleMatchRows))
  # If unknown county or has data but didn't match, consider all nbi bridges in state
  if (DataButNoMatch | length(CountyCols)==0) {
    if (VERBOSE==TRUE)print("  made it to state level")
    if (MatchType == "route" ){ # only look at bridges with correct type of road 
      if(!is.na(FailEntry$ITEM5B)){
        nbiBridgesInStateWithRte <- nbiBridgesInState[nbiDataState$ITEM5B == FailEntry$ITEM5B]
        nbiDataState[nbiBridgesInStateWithRte,"ITEM5D"] <- sapply(1:length(nbiBridgesInStateWithRte), function(i) sub("^[0]+","",nbiDataState[nbiBridgesInStateWithRte[i],"ITEM5D"]))
      }
      else nbiBridgesInStateWithRte <- nbiBridgesInState
    }
    else nbiBridgesInStateWithRte <- nbiBridgesInState
    PossibleMatchRows[[length(PossibleMatchRows)+1]] <- paste("ID", FailEntry$ID,"-STFIPS",FailEntry$STFIPS,sep="")
    if (length(nbiBridgesInStateWithRte) > 0) {
      PossibleMatches <- PerformMatch(FailEntry, nbiDataState[nbiBridgesInStateWithRte,], MatchType)
    }
    if (MatchType == "route" & PossibleMatches[1] < 0){ # if didn't find anything, check other types of road
      if (!is.na(FailEntry$ITEM5B)){
        nbiBridgesInStateWithWrongRte <- nbiBridgesInState[nbiDataState$ITEM5B != FailEntry$ITEM5B]
        nbiDataState[nbiBridgesInStateWithWrongRte,"ITEM5D"] <- sapply(1:length(nbiBridgesInStateWithWrongRte), function(i) sub("^[0]+","",nbiDataState[nbiBridgesInStateWithWrongRte[i],"ITEM5D"]))
        PossibleMatches <- PerformMatch(FailEntry, nbiDataState[nbiBridgesInStateWithWrongRte,], MatchType)
      }
    }
    PossibleMatchRows <- c(PossibleMatchRows, PossibleMatches)
  }
}
else { 
  if (MatchType=="gage"){
    MatchRowNames        <- rownames(gageDataFrame)
    gagesInAdjState      <- MatchRowNames[gageDataFrame$STFIPS %in% c(FailEntry$STFIPS,unlist(ls.Adj.STFIPS[as.character(FailEntry$STFIPS)]))]
    gageDataAdjState     <- gageDataFrame[gagesInAdjState,]
    gageDataAdjStateType <- gageDataAdjState[gageDataAdjState$STREAM_TYPE_GAGE==FailEntry$STREAM_TYPE,]
    PossibleMatches      <-  ifelse(nrow(gageDataAdjStateType)>=1,PerformMatch(FailEntry, gageDataAdjStateType, MatchType),-1)
    if (PossibleMatches[1] < 0){ # if didn't find anything, check other types of stream
      PossibleMatches    <- PerformMatch(FailEntry, gageDataAdjState, MatchType)
    }
    PossibleMatchRows    <- c(PossibleMatchRows, PossibleMatches)
  }
  else{ # must be dam
    MatchRowNames        <- rownames(damDataFrame)
    damsInAdjState       <- MatchRowNames[damDataFrame$STFIPS %in% c(FailEntry$STFIPS,unlist(ls.Adj.STFIPS[as.character(FailEntry$STFIPS)]))]
    damDataAdjState      <- damDataFrame[damsInAdjState,]
    damDataAdjStateType  <- damDataAdjState[damDataAdjState$STREAM_TYPE_GAGE==FailEntry$STREAM_TYPE,]
    PossibleMatches      <-  ifelse(nrow(damDataAdjStateType)>=1,PerformMatch(FailEntry, damDataAdjStateType, MatchType),-1)
    if (PossibleMatches[1] < 0){ # if didn't find anything, check other types of stream
      PossibleMatches    <- PerformMatch(FailEntry, damDataAdjState, MatchType)
    }
    if (PossibleMatches[1] < 0){ # if didn't find anything, check other stream data
      damsInAdjState    <- damsInAdjState[!is.na(damDataFrame[damsInAdjState,"STREAM_NAME_DAM_ALT"])]
      damDataAdjState   <- damDataFrame[damsInAdjState,]
      damDataAdjState$STREAM_NAME_DAM <- damDataAdjState$STREAM_NAME_DAM_ALT
      damDataAdjState$STREAM_TYPE_DAM <- damDataAdjState$STREAM_TYPE_DAM_ALT # easier to overwrite than change PerformMatch
      damDataAdjStateType <- damDataAdjState[damDataAdjState$STREAM_TYPE_GAGE==FailEntry$STREAM_TYPE,]
      PossibleMatches     <-  ifelse(nrow(damDataAdjStateType)>=1,PerformMatch(FailEntry, damDataAdjStateType, MatchType),-1)
      if (PossibleMatches[1] < 0){ # if didn't find anything, check other types of alt stream
        PossibleMatches    <- PerformMatch(FailEntry, damDataAdjState, MatchType)
      }
    }
    PossibleMatchRows    <- c(PossibleMatchRows, PossibleMatches)
  }
}

if (VERBOSE==TRUE)print("*****")
PossibleMatchRows <- unlist(PossibleMatchRows)
if (all(grepl("[[:alpha:]]",PossibleMatchRows) | grepl("^-",PossibleMatchRows)) & LoadFromFile == TRUE){
  # if had matches, but then the second match was unsuccessful, return the original matches
  if (length(SubsetPossibleMatchRows>=1)) PossibleMatchRows <- c(PossibleMatchRows[1], SubsetPossibleMatches)
}
save(PossibleMatchRows, file=paste(file.path(OutputDirectory,"ID"),FailEntry$ID,"-",MatchType,".RData",sep=""))
}
