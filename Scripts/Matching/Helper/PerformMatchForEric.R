# PerformMatch from 2015-01-21, where matched Failed bridge to NBI bridge
# modified on 2015-04-20 to generalize NBI data to any matching dataframe
# in order to support use for matching to bridge gage dataframe
# modified on 2015-04-23 to use updated gage data frame matching
PerformMatch <- function(FailEntry, matchData, MatchType, VERBOSE = FALSE, maxFuzzy = 10) {
MatchRowNames <- rownames(matchData)
PossibleMatchRows <- list()

# determine column names for type of data specified
if(MatchType == "stream") {
  FailMatchTypes <- c("FEAT_UND", "STREAM_NAME", "STREAM_TYPE")
  MatchToTypes <- "ITEM6A"
  Marker <- "d"
  if (VERBOSE==TRUE) print("matching for stream") 
}
else {
  if(MatchType == "road") {
    FailMatchTypes <- c("LOCATION", "ROAD_NAME", "ROAD_TYPE")
    MatchToTypes <- "ITEM7"
    Marker <- "b"
    if (VERBOSE==TRUE)  print("matching for road") 
  }
  else {
    if(MatchType == "route"){ # assume am given nbi data with correct route type, 
      # and that ITEM5D has already deleted leading 0's
      FailMatchTypes <- c("LOCATION", "ROUTE_NO", "ROAD_TYPE")
      MatchToTypes   <- "ITEM5D"
      Marker <- "c"
      if (VERBOSE==TRUE)  print("matching for rte")
    }
    else{
      if(MatchType == "gage"){ # matching to gageDataFrame
        FailMatchTypes <- c("FEAT_UND", "STREAM_NAME", "STREAM_TYPE")
        MatchToTypes   <- c("STREAM_NAME_GAGE", "STREAM_TYPE_GAGE")
        Marker <- "e"
        if (VERBOSE==TRUE)  print("matching for gage")
      }
      else {
        if(MatchType == "dam"){ # matching to damDataFrame
          FailMatchTypes <- c("FEAT_UND", "STREAM_NAME", "STREAM_TYPE")
          MatchToTypes   <- c("STREAM_NAME_DAM", "STREAM_TYPE_DAM")
          Marker <- "f"
          if (VERBOSE==TRUE) print("matching for dam")
        }
        else{
          if(MatchType == "nbiGage"){ # matching gage to NBI
            FailMatchTypes <- c("STANAME", "STREAM_NAME_GAGE", "STREAM_TYPE_GAGE")
            MatchToTypes   <- c("ITEM6A")
            Marker <- "g"
            if (VERBOSE==TRUE)  print("matching for NBI data to gage")
          }
          else{
              if(MatchType == "BIN"){ # matching BIN numbers
              FailMatchTypes <- c("BIN", "BIN_1", "maxBIN", "BIN_2", "BIN_3", "BIN_4")
              MatchToTypes <- c("ITEM8")
              Marker <- "a"
              if (VERBOSE==TRUE) print ("matching for BIN number")
          }
           else(stop("Error in matching -- must be stream, road, route, gage, BIN, or nbiGage"))
          }
        }
      }
    }
  }
}
# print(head(matchData[,MatchToTypes[1]]))
# if has stream data / location data
if (FailEntry[1,FailMatchTypes[1]]!="" & !is.na(FailEntry[1,FailMatchTypes[1]])){
    # attempt to match 1st type of processed data (STREAM_NAME or ROAD_NAME)
    if (FailEntry[1,FailMatchTypes[2]]!="" & !is.na(FailEntry[1,FailMatchTypes[2]])){
        if (VERBOSE==TRUE)print("    checking for NAME")
        if(MatchType != "gage") {print(paste("\\<",FailEntry[1,FailMatchTypes[2]],"\\>",sep=""))
            # print(matchData[,MatchToTypes[1]])
            if(MatchType == "BIN"){
                if(!is.na(FailEntry[1,FailMatchTypes[6]])){
                TestMatchBridges <- grepl(paste("\\<",FailEntry[1,FailMatchTypes[2]],"\\>",sep=""),matchData[,MatchToTypes[1]])&
                grepl(paste("\\<",FailEntry[1,FailMatchTypes[4]],"\\>",sep=""),matchData[,MatchToTypes[1]])&
                grepl(paste("\\<",FailEntry[1,FailMatchTypes[5]],"\\>",sep=""),matchData[,MatchToTypes[1]])&
                grepl(paste("\\<",FailEntry[1,FailMatchTypes[6]],"\\>",sep=""),matchData[,MatchToTypes[1]])
                }
                else{
                if(!is.na(FailEntry[1,FailMatchTypes[5]])){
                TestMatchBridges <- grepl(paste("\\<",FailEntry[1,FailMatchTypes[2]],"\\>",sep=""),matchData[,MatchToTypes[1]])&
                grepl(paste("\\<",FailEntry[1,FailMatchTypes[4]],"\\>",sep=""),matchData[,MatchToTypes[1]])&
                grepl(paste("\\<",FailEntry[1,FailMatchTypes[5]],"\\>",sep=""),matchData[,MatchToTypes[1]])
                }  
                else{
                if(!is.na(FailEntry[1,FailMatchTypes[4]])){
                TestMatchBridges <- grepl(paste("\\<",FailEntry[1,FailMatchTypes[2]],"\\>",sep=""),matchData[,MatchToTypes[1]])&
                grepl(paste("\\<",FailEntry[1,FailMatchTypes[4]],"\\>",sep=""),matchData[,MatchToTypes[1]])
                }  
                else{
                    TestMatchBridges<- grepl(paste("\\<",FailEntry[1,FailMatchTypes[2]],"\\>",sep=""),matchData[,MatchToTypes[1]])
                    }
                }
                }
                }
            else{
            TestMatchBridges <- grepl(paste("\\<",FailEntry[1,FailMatchTypes[2]],"\\>",sep=""),matchData[,MatchToTypes[1]])
            Marker <- paste("1",Marker,sep="")
            # print(any(TestMatchBridges))
            }
        }
        else  {TestMatchBridges <- FailEntry[1,FailMatchTypes[2]]==matchData[,MatchToTypes[1]]}
        if (!any(TestMatchBridges)){ # if first stream / location data did not yield match, try split string
            SplitName         <- unlist(strsplit(FailEntry[1,FailMatchTypes[2]], " "))
            NumberNameEntries <- length(SplitName)
            if (NumberNameEntries > 1){
                if (VERBOSE==TRUE) print("    checking for split name")
                TestMatchBridges <- data.frame(logical(length(MatchRowNames)))
                TestMatchBridges <- sapply(1:NumberNameEntries, function(l) grepl(paste("\\<",SplitName[l],"\\>",sep=""),matchData[,MatchToTypes[1]]))
                if (class(TestMatchBridges)=="list") {TestMatchBridges <- as.data.frame(TestMatchBridges)}
                TestMatchBridges <- ifelse(nrow(TestMatchBridges)>1 & 
                                               !is.na(nrow(TestMatchBridges)) & 
                                               !is.null(nrow(TestMatchBridges)),
                                           apply(TestMatchBridges, MARGIN = 1, any),
                                           ifelse(nrow(TestMatchBridges)==1,
                                                  any(TestMatchBridges),
                                                  FALSE))
            }
        }
        if (VERBOSE==TRUE) print("    checked for NAME")
    }
    else {TestMatchBridges <- FALSE}
    # if first stream / location data did not yield match, try second type of data, but only for road or stream match
    if (MatchType !="route" & MatchType !="gage" & MatchType !="dam" & !any(TestMatchBridges)){
        if (VERBOSE==TRUE)  print("    checking for TYPE")
        SplitType         <- unlist(strsplit(FailEntry[1,FailMatchTypes[3]], " "))
        NumberTypeEntries <- length(SplitType)
        if (NumberTypeEntries > 1){
            TestMatchBridges <- data.frame(logical(length(MatchRowNames)))
            TestMatchBridges <- sapply(1:NumberTypeEntries, function(l) grepl(paste("\\<",SplitType[1][l],"\\>",sep=""),matchData[,MatchToTypes[1]]))
            if (class(TestMatchBridges)=="list") {TestMatchBridges <- as.data.frame(TestMatchBridges)}
            TestMatchBridges <- ifelse(nrow(TestMatchBridges)>1 & 
                                           !is.na(nrow(TestMatchBridges)) & 
                                           !is.null(nrow(TestMatchBridges)),
                                       apply(TestMatchBridges, MARGIN = 1, any),
                                       ifelse(nrow(TestMatchBridges)==1,
                                              any(TestMatchBridges),
                                              FALSE))
            Marker <- paste("2",Marker,sep="")
        }
        if (VERBOSE==TRUE) print("    checked for TYPE")
    }
    # if matching by BIN, try third type of data if second type didn't yield a match
    if (MatchType == "BIN" & !any(TestMatchBridges)){
        print(FailEntry[FailMatchTypes[3]])
        if (VERBOSE==TRUE)  print("    Fuzzy Matching")
        if (FailEntry[FailMatchTypes[3]]!="" & !is.na(FailEntry[FailMatchTypes[3]])){
            BINDist <- stringdist(FailEntry[1,FailMatchTypes[3]],matchData[,MatchToTypes[1]])
            names(BINDist) <- MatchRowNames
            FuzzyBIN <- character()
            if (min(BINDist) < maxFuzzy){
              for (i in 0:min(max(c(BINDist,maxFuzzy)))){
                FuzzyBIN <- c(FuzzyBIN,names(which(BINDist==i)))
              }
              FuzzyBIN <- as.character(FuzzyBIN)
              TestMatchBridges <- MatchRowNames  %in% FuzzyBIN
            }
            else{TestMatchBridges <- rep(FALSE,length(matchRowNames))}
            
	          
            Marker <- paste("3",Marker,sep="")
        }
      
      if (FailEntry[FailMatchTypes[3]]!="" & !is.na(FailEntry[FailMatchTypes[3]])){
        BINDist <- stringdist(FailEntry[1,FailMatchTypes[3]],matchData[,MatchToTypes[1]])
        names(BINDist) <- MatchRowNames
        FuzzyBIN <- character()
        if (min(BINDist) < 10){
          for (i in 0:min(max(c(BINDist,10y)))){
            FuzzyBIN <- c(FuzzyBIN,names(which(BINDist==i)))
          }
          FuzzyBIN <- as.character(FuzzyBIN)
          TestMatchBridges <- MatchRowNames  %in% FuzzyBIN
        }
        else{TestMatchBridges <- rep(FALSE,length(matchRowNames))}
        Marker <- paste("3",Marker,sep="")
      }
    }
 
    # record matches
    if (any(TestMatchBridges)){
        MatchBridgesIndex <- which(TestMatchBridges)
        
        print(MatchBridgesIndex)
        PossibleMatchRows <- c()
        for (i in 1:MatchBridgesIndex){
            PossibleMatchRows[i] <- paste(Marker,MatchRowNames[MatchBridgesIndex[i]],sep="")
            }
        
        if (length(PossibleMatchRows) > 0.9*length(MatchRowNames) & length(PossibleMatchRows)!=1){
            PossibleMatchRows <- as.character(-1*length(MatchRowNames))
        }
    }
    else {PossibleMatchRows <- as.character(-1*length(MatchRowNames))}
}
    # if does not have stream / location data
    else {
        if (VERBOSE==TRUE) print("    had no useful data")
        PossibleMatchRows <- as.character(-1*length(MatchRowNames))
    }
    
    sort(PossibleMatchRows)
    
    return(PossibleMatchRows)}
