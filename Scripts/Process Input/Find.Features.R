Find.Features <- function(Data, 
                          DATA_SET,
                          Features, # Types of features to be extracted and their associated field (column) name (e.g., CITY = "LOCATION_COL_NAME")
                          VERBOSE = VERBOSE){
  source(file.path("Scripts","Process Input","Feature.Detect.R"))
  
  # # Determine which types of features are to be detected
  # DATA_TYPE   <- names(DATA_SETS)[sapply(DATA_SETS,"[[",1)==DATA_SET]
  Rows  <- rownames(Data)
  ls.patterns <- list(COUNTY = c(FIND = "WORD", STRICT = "COMPOUND"),
                      CITY =c(FIND = "WORD", STRICT ="COMPOUND"),
                      LOCATION = c("NEAR", "RELATIONAL","STREAM_NAME_LOC","STREAM_TYPE_LOC"),
                      ROAD = paste0("ROAD_",c("NAME","TYPE","DIRECTION","AUX")),
                      ROUTE = as.vector(sapply(1:2, function(i) paste0(paste0("ROUTE_",c("NAME","TYPE","DIRECTION","AUX")),i))),
                      STREAM = as.vector(sapply(1:2, function(i) paste0(paste0("STREAM_",c("NAME","TYPE","TRIB","FORK","AUX")),i))))
  ls.cols.out <- list(COUNTY = as.vector(sapply(1:2, function(i) paste0(c("COUNTY_NAME_","FIPS_"),i))),
                      CITY = as.vector(sapply(1:3, function(i) paste0(c("CITY_NAME_","FIPS_FROM_CITY_","ANSICODE_","GNIS_ID_"),i))),
                      LOCATION = c("NEAR", "RELATIONAL","STREAM_NAME_LOC","STREAM_TYPE_LOC"),
                      ROAD = paste0("ROAD_",c("NAME","TYPE","DIRECTION","AUX")),
                      ROUTE = as.vector(sapply(1:2, function(i) paste0(paste0("ROUTE_",c("NAME","TYPE","DIRECTION","AUX")),i))),
                      STREAM = as.vector(sapply(1:2, function(i) paste0(paste0("STREAM_",c("NAME","TYPE","TRIB","FORK","AUX")),i))))
  
  for(f in names(Features)){
    COL        <- Features[f]
    cols.out   <- ls.cols.out[[f]]
    names.out  <- unique(sub("_[[:digit:]]+","",cols.out))
    n.dup.cols <- max(as.numeric(gsub("[A-Z_]+","",ls.cols.out[[f]])),na.rm = T)
    Data[,cols.out] <- NA_character_
    
    # state-dependent checks for possible county and city names and special processing
    for(state in unique(Data$STFIPS)){
      rows <- Rows[Data$STFIPS==state & !is.na(Data[,COL])]
      if(f %in% c("COUNTY", "CITY")){
        if(VERBOSE) print(paste("Checking for",tolower(f),"in state:",df.States[state,"STATE_FULL"]))
        localities <- switch(f,
                             COUNTY = df.Counties[df.Counties$STFIPS_C == state, c("COUNTY_NAME", "FIPS_C")],
                             CITY = df.Citiesdf.Cities[df.Cities$STFIPS_C == state, c("CITY_NAME", "FIPS_C", "GNIS_ID", "ANSICODE")])
        colnames(localities)[grepl("NAME",colnames(localities))] <- "NAME"
        colnames(localities)[colnames(localities)=="FIPS_C"] <- "FIPS"
        localities$PATTERN <- localities$NAME
        
        # check for presence of locality name and record name and FIPS or other standard codes
        Data[rows,c(COL,cols.out)] <- Feature.Detect(Data[rows,], localities, ls.patterns[[f]]["FIND"], COL, cols.out , n.dup.cols = n.dup.cols, DELETE = FALSE)
        
      }
      # state-specific, not city or county
      if(state %in% names(ls.DOT.Keys)){
        if(f %in% names(ls.DOT.Keys[[state]])){
          pattern <- ls.DOT.Keys[[state]][f]["PATTERN"]
          match.keys <- grep(pattern,Data[rows,COL])
          for(i in match.keys){
            match.start <- regexpr(pattern,Data[rows[i],COL])
            match.last  <- match_start + attr(match_start,"match.length") - 1
            if(!is.na(ls.DOT.Keys[[state]][f]["MOVE"])) Data[rows[i],ls.DOT.Keys[[state]][f]["MOVE"]] <- ifelse(!is.na(Data[rows[i],ls.DOT.Keys[[state]][f]["MOVE"]]),
                                                                                                                paste(Data[rows[i],ls.DOT.Keys[[state]][f]["MOVE"]],
                                                                                                                      substr(Data[rows[i],COL],match.start,match.last)),
                                                                                                                substr(Data[rows[i],COL],match.start,match.last))
            if(!is.na(ls.DOT.Keys[[state]][f]["ADDTO"])) Data[rows[i],ls.DOT.Keys[[state]][f]["ADDTO"]] <- ifelse(!is.na(Data[rows[i],ls.DOT.Keys[[state]][f]["ADDTO"]]),
                                                                                                                  paste(Data[rows[i],ls.DOT.Keys[[state]][f]["ADDTO"]],
                                                                                                                        ls.DOT.Keys[[state]][f]["ADD"]),
                                                                                                                  ls.DOT.Keys[[state]][f]["ADD"])
                                                                                                                 
            if(!is.na(ls.DOT.Keys[[state]][f]["SUB"])) Data[rows[i],COL] <- sub(pattern,"",Data[rows[i],COL])
            
          } 
        }
      }
    }
    else{ # features not dependent on state
      for(ft in ls.Features[[f]]){
      ls.Keys <- get(paste0("ls.",ft,".Keys"))
    }
  }
  
  #  FailDataFrame[RowsWithCountyMatch[i],LocProcessColsOut]     <- GetAndRemoveLocationData(FailDataFrame[RowsWithCountyMatch[i], LocProcessColsIn], StateCounties[MatchedCountyIndex[[i]][k],"COUNTY_NAME"], LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, FALSE, TRUE)
  
}

for (j in FailStates){
  
  ls.RteKeysState <- ls.RteKeys
  ls.RteKeysState$stateName[1] <- tolower(df.States[df.States$STFIPS==j,"STATE_CODE"])
  ls.RteKeysState$stateName[2] <- tolower(df.States[df.States$STFIPS==j,"STATE_FULL"])
  single_abbrev <- substr(ls.RteKeysState$stateName[1],1,1)
  if (all(single_abbrev != c("i", "r", "p"))) ls.RteKeysState$stateName[3] <- single_abbrev
  ls.RteKeysState$stateName <- unlist(ls.RteKeysState$stateName)
  RteKeys <- unlist(ls.RteKeysState)
  RteKeyIndex <- sapply(RteKeys, function(s) grep(paste("\\<",s,"\\>",sep=""),ls.RteKeysState)[1])
  
  # SPECIAL PROCESSING FOR ARKANSAS AND IOWA, WHERE HAVE ##-##-## entries
  if (j == 5 | j == 19){
    # with km/mi posting (?)
    RowsWithDashedMatch <- rowsForState[grep("[[:alpha:]]{1,2}[[:space:]]?[[:digit:]]{1,4}-[[:graph:]]{1,}",FailDataFrame[rowsForState,"ROAD_NAME"])]
    unRowsWithMatchedDashedrows  <- rowsForState[!(rowsForState %in% RowsWithDashedMatch)]
    RowsWithDashedMatch2 <- unRowsWithMatchedDashedrows[grep("[[:alpha:]]?[[:digit:]]{1,4}",FailDataFrame[unRowsWithMatchedDashedrows,"ROAD_NAME"])]
    RowsWithDashedMatch  <- unique(c(RowsWithDashedMatch, RowsWithDashedMatch2))
    nRowsWithMatch       <- length(RowsWithDashedMatch)
    DashedSubstrings  <- lapply(RowsWithDashedMatch, function(i) unlist(strsplit(strsplit(FailDataFrame[i,"ROAD_NAME"],"-")[[1]],"[[:space:]]")))
    nDashedSubstrings <- sapply(1:nRowsWithMatch, function(i) length(DashedSubstrings[[i]]))
    route_index       <- sapply(1:nRowsWithMatch, function(i) min(grep("[[:digit:]]",DashedSubstrings[[i]])))
    nWithNumbers      <- sapply(1:nRowsWithMatch, function(i) max(grep("[[:digit:]]",DashedSubstrings[[i]])))
    HasMultipleNumberStrings <- (nWithNumbers - route_index) >= 1
    HasExtraStrings          <- nWithNumbers < nDashedSubstrings 
    HasPrefixStrings         <- route_index > 1
    FailDataFrame[RowsWithDashedMatch,"ROUTE_NO"]   <- sapply(1:nRowsWithMatch, function(i) gsub("[[:punct:]]","",DashedSubstrings[[i]][route_index[i]]))
    FailDataFrame[RowsWithDashedMatch,"RD_OTHER"]   <- sapply(1:nRowsWithMatch, function(i) ifelse(HasMultipleNumberStrings[i], gsub(",","",paste(DashedSubstrings[[i]][c((route_index[i]+1):nWithNumbers[i])],collapse="-")),""))
    FailDataFrame[RowsWithDashedMatch,"ROAD_NAME"]  <- sapply(1:nRowsWithMatch, function(i) ifelse(HasPrefixStrings[i], paste(DashedSubstrings[[i]][c(1:(route_index[i]-1))],collapse=" "),""))
    FailDataFrame[RowsWithDashedMatch,"ROAD_NAME"]  <- sapply(1:nRowsWithMatch, function(i) ifelse(HasExtraStrings[i], paste(FailDataFrame[RowsWithDashedMatch[i],"ROAD_NAME"],DashedSubstrings[[i]][c((nWithNumbers[i]+1):nDashedSubstrings[i])],collapse=" "),FailDataFrame[RowsWithDashedMatch[i],"ROAD_NAME"]))
    
    # now find road information
    HasDashedRoadTypeBool   <- sapply(paste("\\<",RteKeys,"\\>",sep=""), grepl, gsub("[[:punct:]]","",FailDataFrame[RowsWithDashedMatch,"ROAD_NAME"]))
    if (nRowsWithMatch == 1) HasDashedRoadTypeBool <- t(HasDashedRoadTypeBool) 
    RowsWithRoadTypeMatchIndex <- which(rowSums(HasDashedRoadTypeBool) != 0)
    RowsWithRoadTypeMatch   <- RowsWithDashedMatch[RowsWithRoadTypeMatchIndex]
    nRowsWithMatch <- length(RowsWithRoadTypeMatch)
    if (nRowsWithMatch >= 1){
      MatchedDashedRoadIndex  <- sapply(1:nRowsWithMatch, function(i) max(which(HasDashedRoadTypeBool[RowsWithRoadTypeMatchIndex[i],])))
      if (class(MatchedDashedRoadIndex) == "list") MatchedDashedRoadIndex <- unlist(MatchedDashedRoadIndex)
      FailDataFrame[RowsWithRoadTypeMatch,"RTE_PREFIX"]   <- sapply(1:length(RowsWithRoadTypeMatch), function(i) ls.RteKeysState[[RteKeyIndex[MatchedDashedRoadIndex[i]]]][1])
      FailDataFrame[RowsWithRoadTypeMatch,"ROAD_NAME"]   <- sapply(1:length(RowsWithRoadTypeMatch), function(i) gsub(RteKeys[MatchedDashedRoadIndex[i]],"",gsub("[[:punct:]]","",FailDataFrame[RowsWithRoadTypeMatch[i],"ROAD_NAME"])))
    }

    rowsForState <-  FailRowNames[FailDataFrame$STFIPS == j & !FailDataFrame$ROAD_NAME==""]
    nRowsState   <- length(rowsForState)
    if (nRowsState == 0) next
  }
  
    rowsForState <-  FailRowNames[FailDataFrame$STFIPS == j & !FailDataFrame$ROAD_NAME==""]
    nRowsState   <- length(rowsForState)
    if (nRowsState == 0) next
  }
  
  # FIND RELATIONAL STATEMENTS
  # statements with explicit distance, e.g., "3.5 miles from Stanford"
  MatchRegex <- "[[:digit:]]{1,3}[[:punct:]]?[[:digit:]]?[[:space:]]?m[i]{0,1}[l]{0,1}[e]{0,1}[s]{0,1}[[:punct:]]?[[:space:]]"
  RowsWithDistance <- rowsForState[grep(MatchRegex,FailDataFrame[rowsForState,"ROAD_NAME"])]
  for (i in RowsWithDistance){
    if (grepl("[[:digit:]]m",FailDataFrame[i,"ROAD_NAME"])){ # if no space between # and "mi", add
      make_space_first <- regexpr("[[:digit:]]m",FailDataFrame[i,"ROAD_NAME"])
      FailDataFrame[i,"ROAD_NAME"] <- paste(substr(FailDataFrame[i,"ROAD_NAME"],1,make_space_first),substr(FailDataFrame[i,"ROAD_NAME"],make_space_first+1,nchar(FailDataFrame[i,"ROAD_NAME"])))
    }
    match_start <- regexpr(MatchRegex,FailDataFrame[i,"ROAD_NAME"])
    match_last  <- match_start + attr(match_start, "match.length") - 1
    # find distance
    dist_start  <- regexpr("[[:digit:]]{1,3}[[:punct:]]{0,1}[[:digit:]]{0,1}",FailDataFrame[i,"ROAD_NAME"])
    dist_last  <- dist_start + attr(dist_start, "match.length") - 1
    FailDataFrame[i,"RD_RELATIONAL"] <- paste(substr(FailDataFrame[i,"ROAD_NAME"],dist_start,dist_last),"mi",sep=" ")
    FailDataFrame[i,"ROAD_NAME"] <- substr(FailDataFrame[i,"ROAD_NAME"],match_last+1,nchar(FailDataFrame[i,"ROAD_NAME"]))      
    # look for cardinal direction (north, nw, sw...)
    substrings <- unlist(strsplit(FailDataFrame[i,"ROAD_NAME"],"[[:space:]]"))
    directionalWord <- ls.CardinalKeys[[grep(paste("\\<",substrings[1],"\\>",sep=""), ls.CardinalKeys)]][1]
    FailDataFrame[i,"RD_RELATIONAL"] <- paste(FailDataFrame[i,"RD_RELATIONAL"],directionalWord,"of",sep=" ")      
    # look for place
    placeSubstring <- ifelse(grepl("\\<of\\>",substrings[2]), 3, 2)
    relationalPlace <- substrings[placeSubstring]
    if (placeSubstring ==2){
      FailDataFrame[i,"ROAD_NAME"] <- gsub("\\<of\\>","",FailDataFrame[i,"ROAD_NAME"])
    }
    PlaceIsCounty <- relationalPlace %in% StateCounties[,"COUNTY_NAME"]
    PlaceIsCity   <-  relationalPlace %in% StateCities[,"CITY_NAME"]
    if (PlaceIsCounty){
      MatchedCountyIndex <- grep(paste("\\<",relationalPlace,"\\>",sep=""),StateCounties[,"COUNTY_NAME"])
      FailDataFrame[i,LocProcessColsOut] <- GetAndRemoveLocationData(FailDataFrame[i, LocProcessColsIn], relationalPlace, LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, FALSE, TRUE)
      FailDataFrame[i,"COUNTY_NAME"] <- StateCounties[MatchedCountyIndex,"COUNTY_NAME"]
      FailDataFrame[i,"FIPS"] <- StateCounties[MatchedCountyIndex,"FIPS"]
      if (PlaceIsCity){
        MatchedCityIndex <- grep(paste("\\<",relationalPlace,"\\>",sep=""),StateCities[,"CITY_NAME"])
        FailDataFrame[i,"CITY_NAME"] <- StateCities[MatchedCityIndex,"CITY_NAME"]
        FailDataFrame[i,"FIPS_FROM_CITY"] <- StateCities[MatchedCityIndex,"FIPS"]
        FailDataFrame[i,"ANSICODE"] <- StateCities[MatchedCityIndex,"ANSICODE"]
        FailDataFrame[i,"GNIS_ID"] <- StateCities[MatchedCityIndex,"GNIS_ID"]
      }
    }
    else{
      if (PlaceIsCity){
        MatchedCityIndex <- grep(paste("\\<",relationalPlace,"\\>",sep=""),StateCities[,"CITY_NAME"])
        FailDataFrame[i,LocProcessColsOut] <- GetAndRemoveLocationData(FailDataFrame[i, LocProcessColsIn], relationalPlace, LocProcessColsOut,ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, TRUE, FALSE)
        FailDataFrame[i,"CITY_NAME"] <- StateCities[MatchedCityIndex,"CITY_NAME"]
        FailDataFrame[i,"FIPS_FROM_CITY"] <- StateCities[MatchedCityIndex,"FIPS"]
        FailDataFrame[i,"ANSICODE"] <- StateCities[MatchedCityIndex,"ANSICODE"]
        FailDataFrame[i,"GNIS_ID"] <- StateCities[MatchedCityIndex,"GNIS_ID"]
      }
      else{
        FailDataFrame[i,"ROAD_NEAR"] <- paste(FailDataFrame[i,"ROAD_NEAR"],relationalPlace,"-",sep=" ")
      }
    }
    FailDataFrame[i,"ROAD_NAME"] <- sub(MatchRegex,"",FailDataFrame[i,"ROAD_NAME"])
    FailDataFrame[i,"ROAD_NAME"] <- sub(paste("\\<",substrings[1],"\\>",sep=""),"",FailDataFrame[i,"ROAD_NAME"])
    FailDataFrame[i,"ROAD_NAME"] <- sub("\\<of\\>","",FailDataFrame[i,"ROAD_NAME"])
  }
  # find relational, no distance ("west of")
  HasRelationalBool <- sapply(RelationalKeys, grepl, gsub("[[:punct:]]","",FailDataFrame[rowsForState,"ROAD_NAME"]))
  if (nRowsState == 1) HasRelationalBool <- t(HasRelationalBool) 
  RowsWithRelationalMatchIndex <- which(rowSums(HasRelationalBool) != 0)
  RowsWithRelationalMatch   <- rowsForState[RowsWithRelationalMatchIndex]  
  nRowsWithMatch <- length(RowsWithRelationalMatch)
  if (nRowsWithMatch >= 1){ 
    MatchedRelationalIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasRelationalBool[RowsWithRelationalMatchIndex[i],])))
    if (class(MatchedRelationalIndex)=="list") MatchedRelationalIndex <- unlist(MatchedRelationalIndex)
    for (i in 1:nRowsWithMatch){
      match_start <- regexpr(paste("\\<",RelationalKeys[MatchedRelationalIndex[i]],"\\>",sep=""),gsub("[[:punct:]]","",FailDataFrame[RowsWithRelationalMatch[i],"ROAD_NAME"]))
      match_last  <- match_start + attr(match_start,"match.length") - 1
      FailDataFrame[RowsWithRelationalMatch[i],"RD_RELATIONAL"] <- substr(gsub("[[:punct:]]","",FailDataFrame[RowsWithRelationalMatch[i],"ROAD_NAME"]),match_start,match_last)
      FailDataFrame[RowsWithRelationalMatch[i],"ROAD_NAME"] <- gsub(paste("\\<",RelationalKeys[MatchedRelationalIndex[i]],"\\>",sep=""),"",gsub("[[:punct:]]","",FailDataFrame[RowsWithRelationalMatch[i],"ROAD_NAME"]))
    }
  }
  
  # FIND BRIDGE NO (I.E., ANY NUMBERS IN PARENTHESES OR WITH "NO" BUT NOT "ROUTE NO" OR SIMILAR)
  # Assumed bridge numbers in parentheticals
  MatchRegex <- "\\([[:digit:]]{0,5}[[:punct:]]?[[:alpha:]]{0,2}[[:punct:]]?[[:digit:]]{0,5}\\>\\)"
  RowsWithParentheticalMatchIndex <- grep(MatchRegex, FailDataFrame[rowsForState,"ROAD_NAME"])
  RowsWithParentheticalMatch <- rowsForState[RowsWithParentheticalMatchIndex]
  nRowsWithMatch <- length(RowsWithParentheticalMatch)
  if (nRowsWithMatch >= 1){ 
    # print(RowsWithParentheticalMatch)
    for (i in 1:nRowsWithMatch){
      match_start <- regexpr(MatchRegex,FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"])
      match_last  <- match_start + attr(match_start,"match.length") - 1
      FailDataFrame[RowsWithParentheticalMatch[i],"BRIDGE_NAME_NO"] <- substr(FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"],match_start+1,match_last-1)
      FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"] <- gsub(MatchRegex,"",FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"])
    }
  }
  # ones that say "bridge no" or similar
  HasBridgeNoBool <- sapply(paste("\\<",BridgeNoKeys,"\\>[[:space:]]{0,2}[[:digit:]]{1,5}",sep=""), grepl, gsub("[[:punct:]]","",FailDataFrame[rowsForState,"ROAD_NAME"]))
  if (nRowsState == 1) HasBridgeNoBool <- t(HasBridgeNoBool) 
  RowsWithBridgeNoMatchIndex <- which(rowSums(HasBridgeNoBool) != 0)
  RowsWithBridgeNoMatch   <- rowsForState[RowsWithBridgeNoMatchIndex]
  nRowsWithMatch <- length(RowsWithBridgeNoMatch)
  if (nRowsWithMatch >= 1){ 
    MatchedBridgeNoIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasBridgeNoBool[RowsWithBridgeNoMatchIndex[i],])))
    if (class(MatchedBridgeNoIndex)=="list") MatchedBridgeNoIndex <- unlist(MatchedBridgeNoIndex)
    for (i in 1:nRowsWithMatch){
      match_start <- regexpr(paste("\\<",BridgeNoKeys[MatchedBridgeNoIndex[i]],"\\>[[:space:]]{0,2}[[:digit:]]{1,5}",sep=""),gsub("[[:punct:]]","",FailDataFrame[RowsWithBridgeNoMatch[i],"ROAD_NAME"]))
      match_last  <- match_start + attr(match_start,"match.length") - 1
      br_start   <- regexpr("[[:digit:]]{1,4}",FailDataFrame[RowsWithBridgeNoMatch[i],"ROAD_NAME"])
      FailDataFrame[RowsWithBridgeNoMatch[i],"BRIDGE_NAME_NO"] <- substr(gsub("[[:punct:]]","",FailDataFrame[RowsWithBridgeNoMatch[i],"ROAD_NAME"]),br_start,match_last)
      FailDataFrame[RowsWithBridgeNoMatch[i],"ROAD_NAME"] <- gsub(paste("\\<",BridgeNoKeys[MatchedBridgeNoIndex[i]],"\\>[[:space:]]{0,2}[[:digit:]]{1,5}",sep=""),"",gsub("[[:punct:]]","",FailDataFrame[RowsWithBridgeNoMatch[i],"ROAD_NAME"]))
    }
  }

  # ones that say ___ ___ bridge
  HasBridgeBool <- sapply(paste("\\<",ls.RoadKeys$bridge,"\\>",sep=""), grepl, gsub("[[:space:]]+|[[:space:]]"," ",gsub("[[:punct:]]","",FailDataFrame[rowsForState,"ROAD_NAME"])))
  if (nRowsState == 1) HasBridgeBool <- t(HasBridgeBool) 
  RowsWithBridgeMatchIndex <- which(rowSums(HasBridgeBool) != 0)
  RowsWithBridgeMatch   <- rowsForState[RowsWithBridgeMatchIndex]
  nRowsWithMatch <- length(RowsWithBridgeMatch)
  if (nRowsWithMatch >= 1){ 
    MatchedBridgeIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasBridgeBool[RowsWithBridgeMatchIndex[i],])))
    if (class(MatchedBridgeIndex)=="list") MatchedBridgeIndex <- unlist(MatchedBridgeIndex)
    for (i in 1:nRowsWithMatch){
      temp_entry  <- str_trim(gsub("[[:punct:]]","",FailDataFrame[RowsWithBridgeMatch[i],"ROAD_NAME"]))
      match_start <- regexpr(paste("\\<",ls.RoadKeys$bridge[MatchedBridgeIndex[i]],"\\>",sep=""),temp_entry)
      match_last  <- match_start + attr(match_start,"match.length") - 1
      FailDataFrame[RowsWithBridgeMatch[i],"BRIDGE_NAME_NO"] <- substr(temp_entry,1,match_start-1)
      HasStreamBool <- sapply(paste("\\<",StreamKeys,"\\>",sep=""), grepl, substr(temp_entry,1,match_start-1))
      HasRoadBool   <- sapply(paste("\\<",RoadKeys,"\\>",sep=""), grepl, substr(temp_entry,match_last+1,match_last+5))
      if (any(HasStreamBool)){
        FailDataFrame[RowsWithBridgeMatch[i],"ROAD_NAME"]      <- paste(substr(temp_entry,1,match_start-1),
                                                                        substr(temp_entry,match_last+1,nchar(temp_entry)), sep = ",")
      }
      else{
        if (any(HasRoadBool)){
          # do nothing
        }
        else{
          FailDataFrame[RowsWithBridgeMatch[i],"ROAD_NAME"]      <- substr(temp_entry,match_last+1,nchar(temp_entry))
        }
      }
    }
  }
  
  # GET ROUTE NUMBER
  MatchRegex <- "\\<[[:alpha:]]{0,6}[[:punct:]]?[[:space:]]?[[:alpha:]]{1,9}[[:punct:]]?[[:space:]]?[[:alpha:]]{0,2}[[:space:]]?[[:digit:]]{1,4}[[:space:]]?[[:alpha:]]?\\>"
  RowsWithRteNo   <- rowsForState[grep(MatchRegex,FailDataFrame[rowsForState,"ROAD_NAME"])]
  RteStrings      <- list()
  RemainStrings   <- list()
  for (i in RowsWithRteNo){
    if (j==72){ # have different encodings for Puerto Rico, throws off regexpr
      match_start <- regexpr(MatchRegex,FailDataFrame[i,"ROAD_NAME"],useBytes=TRUE)
      rte_start   <- regexpr("[[:digit:]]{1,4}",FailDataFrame[i,"ROAD_NAME"],useBytes=TRUE)
    }
    else {
      match_start <- regexpr(MatchRegex,FailDataFrame[i,"ROAD_NAME"])
      rte_start   <- regexpr("[[:digit:]]{1,4}",FailDataFrame[i,"ROAD_NAME"])
    }
    match_last  <- match_start + attr(match_start,"match.length") - 1    
    FailDataFrame[i,"ROUTE_NO"] <- substr(FailDataFrame[i,"ROAD_NAME"],rte_start,match_last)
    RteStrings[[i]] <- gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",substr(FailDataFrame[i,"ROAD_NAME"],match_start,rte_start-1)),perl=TRUE)
    RemainStrings[[i]] <- substr(FailDataFrame[i,"ROAD_NAME"],match_last+1,nchar(FailDataFrame[i,"ROAD_NAME"]))
  }
  
  if (length(RowsWithRteNo)>=1){
    FailDataFrame[RowsWithRteNo,"ROAD_NAME"]   <- sapply(RowsWithRteNo, function(i) RemainStrings[[i]])
    FailDataFrame[RowsWithRteNo,"RTE_PREFIX"]  <- sapply(RowsWithRteNo, function(i) RteStrings[[i]])
    # first find cardinals
    HasCardinalBool <- sapply(paste("\\<",CardinalKeys,"\\>",sep=""),grepl,FailDataFrame[RowsWithRteNo,"RTE_PREFIX"])
    if (length(RowsWithRteNo)==1) HasCardinalBool <- t(HasCardinalBool)
    RowsWithCardinalIndex <- which(rowSums(HasCardinalBool) != 0)
    RowsWithCardinal <- RowsWithRteNo[RowsWithCardinalIndex]
    nRowsWithMatch <- length(RowsWithCardinal)
    if (nRowsWithMatch >= 1){
      MatchedDirectionIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasCardinalBool[RowsWithCardinalIndex[i],])))     
      FailDataFrame[RowsWithCardinal,"RTE_DIRECTION"] <- sapply(1:nRowsWithMatch, function(i) ls.CardinalKeys[[CardinalKeyIndex[MatchedDirectionIndex[i]]]][1])
      FailDataFrame[RowsWithCardinal,"RTE_PREFIX"]     <- sapply(1:nRowsWithMatch, function(i) gsub(paste("\\<",CardinalKeys[MatchedDirectionIndex[i]],"\\>",sep=""),"", FailDataFrame[RowsWithCardinal[i],"RTE_PREFIX"]))
    }
    # then find road prefix types
    HasRteTypeBool <- sapply(paste("\\<",RteKeys,"\\>",sep=""), grepl, FailDataFrame[RowsWithRteNo,"RTE_PREFIX"])
    if (length(RowsWithRteNo)==1) HasRteTypeBool <- t(HasRteTypeBool) 
    RowsWithRteTypeIndex   <- which(rowSums(HasRteTypeBool) != 0)
    RowsWithRteTypeMatch   <- RowsWithRteNo[RowsWithRteTypeIndex]
    nRowsWithMatch         <- length(RowsWithRteTypeMatch)
    if (nRowsWithMatch >= 1){
      MatchedRteTypeIndex  <- sapply(1:length(RowsWithRteTypeMatch), function(i) max(which(HasRteTypeBool[RowsWithRteTypeIndex[i],])))     
      FailDataFrame[RowsWithRteTypeMatch,"RTE_PREFIX"]   <- sapply(1:length(RowsWithRteTypeMatch), function(i) ls.RteKeysState[[RteKeyIndex[MatchedRteTypeIndex[i]]]][1])
    }  
  }
  # check for secondary route numbers (i.e., if mentions two roads)
  HasRteTypeBool <- sapply(paste("\\<",RteKeys,"[[:punct:]]?[[:space:]]?[[:digit:]]{1,4}\\>",sep=""), grepl, FailDataFrame[rowsForState,"ROAD_NAME"])
  if (length(rowsForState)==1) HasRteTypeBool <- t(HasRteTypeBool) 
  RowsWithRteTypeIndex   <- which(rowSums(HasRteTypeBool) != 0)
  RowsWithRteTypeMatch   <- rowsForState[RowsWithRteTypeIndex]
  nRowsWithMatch         <- length(RowsWithRteTypeMatch)
  if (nRowsWithMatch >= 1){
    for (i in RowsWithRteTypeMatch){
      rte_start   <- regexpr("[[:digit:]]{1,4}",FailDataFrame[i,"ROAD_NAME"])
      rte_last    <- rte_start + attr(rte_start, "match.length") - 1
      FailDataFrame[i,"RTE_SECOND"]  <- substr(FailDataFrame[i,"ROAD_NAME"],rte_start,rte_last)
    }
    FailDataFrame[RowsWithRteTypeMatch,"ROAD_NAME"] <- sapply(1:nRowsWithMatch, function(i) gsub(FailDataFrame[RowsWithRteTypeMatch[i],"RTE_SECOND"],"",FailDataFrame[RowsWithRteTypeMatch[i],"ROAD_NAME"]))
    MatchedRteTypeIndex  <- sapply(1:length(RowsWithRteTypeMatch), function(i) max(which(HasRteTypeBool[RowsWithRteTypeIndex[i],])))
    if (class(MatchedRteTypeIndex) == "list") MatchedRteTypeIndex <- unlist(MatchedRteTypeIndex)
    FailDataFrame[RowsWithRteTypeMatch,"RTE_SECOND"]   <- sapply(1:nRowsWithMatch, function(i) paste(ls.RteKeysState[[RteKeyIndex[MatchedRteTypeIndex[i]]]][1],FailDataFrame[RowsWithRteTypeMatch[i],"RTE_SECOND"],sep=" "))
    FailDataFrame[RowsWithRteTypeMatch,"ROAD_NAME"] <- sapply(1:nRowsWithMatch, function(i) gsub(paste("\\<",RteKeys[MatchedRteTypeIndex[i]],"\\>",sep=""),"",FailDataFrame[RowsWithRteTypeMatch[i],"ROAD_NAME"]))
  }  
  # Any remaining with a number should be the route number, even if they don't have a route type
  RowsWithNo <- rowsForState[grep("[[:digit:]]{1,4}\\>",FailDataFrame[rowsForState,"ROAD_NAME"])]
  for (i in RowsWithNo){
    rte_start   <- regexpr("[[:digit:]]{1,4}",FailDataFrame[i,"ROAD_NAME"])
    rte_last    <- rte_start + attr(rte_start, "match.length") - 1
    if (FailDataFrame[i,"ROUTE_NO"]==""){
      FailDataFrame[i,"ROUTE_NO"]  <- substr(FailDataFrame[i,"ROAD_NAME"],rte_start,rte_last)
      FailDataFrame[i,"ROAD_NAME"] <- gsub(FailDataFrame[i,"ROUTE_NO"],"",FailDataFrame[i,"ROAD_NAME"])
    }
    else {
      FailDataFrame[i,"RTE_SECOND"]  <- substr(FailDataFrame[i,"ROAD_NAME"],rte_start,rte_last)
      FailDataFrame[i,"ROAD_NAME"] <- gsub(FailDataFrame[i,"RTE_SECOND"],"",FailDataFrame[i,"ROAD_NAME"])
    }
  }
  # find alphabet route numbers (i.e., supplemental or secondary routes in Missouri)
  HasRteTypeBool <- sapply(paste("\\<",RteKeys,"\\>[[:punct:]]?[[:space:]]?\\<[[:alpha:]]{1,2}\\>",sep=""), grepl, FailDataFrame[rowsForState,"ROAD_NAME"])
  if (length(rowsForState)==1) HasRteTypeBool <- t(HasRteTypeBool) 
  RowsWithRteTypeIndex   <- which(rowSums(HasRteTypeBool) != 0)
  RowsWithRteTypeMatch   <- rowsForState[RowsWithRteTypeIndex]
  nRowsWithMatch         <- length(RowsWithRteTypeMatch)
  if (nRowsWithMatch >= 1){
    MatchedRteTypeIndex  <- sapply(1:length(RowsWithRteTypeMatch), function(i) max(which(HasRteTypeBool[RowsWithRteTypeIndex[i],])))
    if (class(MatchedRteTypeIndex) == "list") MatchedRteTypeIndex <- unlist(MatchedRteTypeIndex)
    for (i in 1:nRowsWithMatch){
      RteType <- RteKeys[MatchedRteTypeIndex[i]]
      rte_start   <- regexpr(paste("\\<",RteType,"\\>[[:punct:]]?[[:space:]]?\\<[[:alpha:]]{1,2}\\>",sep=""),FailDataFrame[RowsWithRteTypeMatch[i],"ROAD_NAME"])
      rte_last    <- rte_start + attr(rte_start, "match.length") - 1
      rte_temp    <- substr(FailDataFrame[RowsWithRteTypeMatch[i],"ROAD_NAME"], rte_start, rte_last)
      FailDataFrame[RowsWithRteTypeMatch[i],"ROUTE_NO"]    <- gsub("[[:space:]]","",gsub("[[:punct:]]","",substr(rte_temp,nchar(RteType)+1,nchar(rte_temp))))
      FailDataFrame[RowsWithRteTypeMatch[i], "RTE_PREFIX"] <- ls.RteKeysState[[RteKeyIndex[MatchedRteTypeIndex[i]]]][1]
      FailDataFrame[RowsWithRteTypeMatch[i], "ROAD_NAME"]  <- paste(substr(FailDataFrame[RowsWithRteTypeMatch[i], "ROAD_NAME"],1,rte_start-1),substr(FailDataFrame[RowsWithRteTypeMatch[i], "ROAD_NAME"],rte_last+1,nchar(FailDataFrame[RowsWithRteTypeMatch[i], "ROAD_NAME"])), sep=", ")
    }
  }  
  
  # ROUTE DIRECTION - GRAB FROM ROUTE NO, THEN FROM OTHER PLACES 
  HasDirectionBool <- sapply(paste("[[:digit:]]",CardinalKeys,sep=""), grepl, FailDataFrame[rowsForState,"ROUTE_NO"])
  if (length(rowsForState)==1) HasDirectionBool <- t(as.vector(unlist(HasDirectionBool))) 
  RowsWithDirectionRowsWithMatchIndex <- which(rowSums(HasDirectionBool) != 0)
  RowsWithDirectionRowsWithMatch <- rowsForState[RowsWithDirectionRowsWithMatchIndex]
  nRowsWithMatch <- length(RowsWithDirectionRowsWithMatch)
  if (length(RowsWithDirectionRowsWithMatch)>=1){
    MatchedDirectionIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasDirectionBool[RowsWithDirectionRowsWithMatchIndex[i],])))     
    FailDataFrame[RowsWithDirectionRowsWithMatch,"RTE_DIRECTION"]   <- sapply(1:nRowsWithMatch, function(i) ls.CardinalKeys[[CardinalKeyIndex[MatchedDirectionIndex[i]]]][1])
    FailDataFrame[RowsWithDirectionRowsWithMatch,"ROUTE_NO"]   <- sapply(1:nRowsWithMatch, function(i) gsub(paste(CardinalKeys[MatchedDirectionIndex[i]],"\\>",sep=""),"", FailDataFrame[RowsWithDirectionRowsWithMatch[i],"ROUTE_NO"]))
  }
  # check for nb/wb/eb/sb in ROAD_NAME
  HasDirectionBool <- sapply(paste("\\<",CardBoundKeys,"\\>",sep=""), grepl, FailDataFrame[rowsForState,"ROAD_NAME"])
  if (length(rowsForState)==1) HasDirectionBool <- t(as.vector(unlist(HasDirectionBool))) 
  RowsWithDirectionRowsWithMatchIndex <- which(rowSums(HasDirectionBool) != 0)
  RowsWithDirectionRowsWithMatch <- rowsForState[RowsWithDirectionRowsWithMatchIndex]
  nRowsWithMatch <- length(RowsWithDirectionRowsWithMatch)
  if (length(RowsWithDirectionRowsWithMatch)>=1){
    MatchedDirectionIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasDirectionBool[RowsWithDirectionRowsWithMatchIndex[i],])))     
    FailDataFrame[RowsWithDirectionRowsWithMatch,"RTE_DIRECTION"]   <- sapply(1:nRowsWithMatch, function(i) ls.CardinalKeys[[CardBoundKeyIndex[MatchedDirectionIndex[i]]]][1])
    FailDataFrame[RowsWithDirectionRowsWithMatch,"ROAD_NAME"]   <- sapply(1:nRowsWithMatch, function(i) gsub(paste("\\<",CardBoundKeys[MatchedDirectionIndex[i]],"\\>",sep=""),"", FailDataFrame[RowsWithDirectionRowsWithMatch[i],"ROAD_NAME"]))
  }
  
  # PERFORM COUNTY MATCHING - STRICT (i.e., only deletes if has name followed by "county" or "co")
  if (nrow(StateCounties)==0) {
    warning(paste("No counties in state #",as.character(j),"-",as.character(df.States[df.States==j,"STATE_CODE"]),sep = " "))
    return(FailDataFrame)
  }
  HasCountyStringBool <- sapply(paste("\\<",StateCounties$COUNTY_NAME,"\\>",sep=""),grepl,FailDataFrame[rowsForState,"ROAD_NAME"])
  if (nRowsState==1) HasCountyStringBool <- t(HasCountyStringBool)
  RowsWithCountyIndex <- which(rowSums(HasCountyStringBool) != 0)
  RowsWithCountyMatch <- rowsForState[RowsWithCountyIndex]
  nRowsWithMatch <- length(RowsWithCountyMatch)
  if (nRowsWithMatch > 0){
    MatchedCountyIndex  <- lapply(1:nRowsWithMatch, function(i) which(HasCountyStringBool[RowsWithCountyIndex[i],]))
    nRowsWithMatchesRow <- sapply(1:nRowsWithMatch, function(i) length(MatchedCountyIndex[[i]]))
    for (i in 1:nRowsWithMatch){
      for (k in 1:nRowsWithMatchesRow[i]){
        FailDataFrame[RowsWithCountyMatch[i],LocProcessColsOut]     <- GetAndRemoveLocationData(FailDataFrame[RowsWithCountyMatch[i], LocProcessColsIn], StateCounties[MatchedCountyIndex[[i]][k],"COUNTY_NAME"], LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, FALSE, TRUE)
        FailDataFrame[RowsWithCountyMatch[i],CountyOutCols[[1]][k]] <- StateCounties[MatchedCountyIndex[[i]][k],"COUNTY_NAME"]
        FailDataFrame[RowsWithCountyMatch[i],CountyOutCols[[2]][k]] <- StateCounties[MatchedCountyIndex[[i]][k],"FIPS"]
        #print(paste("Matched county", StateCounties[MatchedCountyIndex[[i]][k],"COUNTY_NAME"],"to entry", FailDataFrame[rowsForState[i],"LOCATION"],sep=" "))
      }
    }
  }
  print("    Finished county identification")
  
  # PERFORM CITY MATCHING - NOT STRICT (still checks to see if part of road or stream name)
  # named cities, townships, villages (e.g., "City of Baltimore" or "Luellen Township)
  HasCityTypeBool <- sapply(paste("\\<",CityKeys,"\\>",sep=""), grepl, FailDataFrame[rowsForState,"ROAD_NAME"])
  if (nRowsState==1) HasCityTypeBool <- t(HasCityTypeBool)
  RowsWithCityMatchIndex <- which(rowSums(HasCityTypeBool) != 0)
  RowsWithCityMatch      <- rowsForState[RowsWithCityMatchIndex]
  nRowsWithMatch <- length(RowsWithCityMatch)
  if (nRowsWithMatch > 0){
    MatchedCityIndex  <- sapply(1:nRowsWithMatch, function(i) max(which(HasCityTypeBool[RowsWithCityMatchIndex[i],])))
    for (i in 1:nRowsWithMatch){
      MatchRegex <- paste("\\<",CityKeys[MatchedCityIndex[i]],"\\>",sep="")
      MatchRegexOf <- paste(MatchRegex,"[[:space:]]{1,2}\\<of\\>",sep="")
      HasOfBool <- grepl(MatchRegexOf,FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"])
      if (HasOfBool){
        match_start <- regexpr(MatchRegexOf, FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"])
        temp_entry <- substr(FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"],match_start,nchar(FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"]))
        temp_entry <- unlist(strsplit(unlist(strsplit(temp_entry,"[[:punct:]]")), "[[:digit:]]"))[1] 
        entry <- str_trim(gsub(MatchRegexOf,"",temp_entry))
        to_delete <- MatchRegexOf
      }
      else{
        match_start <- regexpr(MatchRegex, FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"])
        match_last  <- match_start + attr(match_start,"match.length") - 1
        temp_entry <- substr(FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"],1,match_last)
        substrings <- unlist(strsplit(temp_entry,"[[:punct:]]"))
        temp_entry <- substrings[length(substrings)]
        entry <- str_trim(gsub(MatchRegex,"",temp_entry))
        to_delete <- MatchRegex
      }
      HasKnownCityBool <- unlist(sapply(StateCities$CITY_NAME, grepl, temp_entry))
      if (all(!HasKnownCityBool)){ # means an unknown township, so record now (otherwise will catch later)
        FailDataFrame[RowsWithCityMatch[i],"ROAD_NEAR"] <- paste(FailDataFrame[RowsWithCityMatch[i],"ROAD_NEAR"], entry, ls.JurisdictionKeys[[CityKeyIndex[MatchedCityIndex[i]]]][1], "-", sep=" ")
        FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"] <- gsub(temp_entry,"",FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"])
      }
      else{
        FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"] <- gsub(to_delete,"",FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"])
      }
    }  
  }
  # not-explicitly-named cities
  if (nrow(StateCities)==0) {
    warning(paste("No cities in state #",as.character(j),"-",as.character(df.States[df.States==j,"STATE_CODE"]),sep = " "))
    return(FailDataFrame)
  }
  HasCityStringBool <- sapply(StateCities$CITY_NAME,grepl,FailDataFrame[rowsForState,"ROAD_NAME"]) 
  if (nRowsState==1) HasCityStringBool <- t(HasCityStringBool)
  RowsWithCityMatch <- which(rowSums(HasCityStringBool) != 0)
  nRowsWithMatch <- length(RowsWithCityMatch)
  if (nRowsWithMatch > 0){
    MatchedCityIndex            <- lapply(RowsWithCityMatch, function(i) which(HasCityStringBool[i,]))
    nRowsWithMatchesRow                    <- integer(nRowsState)
    nRowsWithMatchesRow[RowsWithCityMatch] <- sapply(1:nRowsWithMatch, function(i) length(MatchedCityIndex[[i]]))
    for (i in 1:nRowsWithMatch){
      for (k in 1:nRowsWithMatchesRow[RowsWithCityMatch[i]]){
        FailDataFrame[rowsForState[RowsWithCityMatch[i]],LocProcessColsOut] <- GetAndRemoveLocationData(FailDataFrame[rowsForState[RowsWithCityMatch[i]], LocProcessColsIn], StateCities[MatchedCityIndex[[i]][k],"CITY_NAME"], LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, TRUE, FALSE)
        FailDataFrame[rowsForState[RowsWithCityMatch[i]],CityOutCols[[1]][k]] <- StateCities[MatchedCityIndex[[i]][k],"CITY_NAME"]
        FailDataFrame[rowsForState[RowsWithCityMatch[i]],CityOutCols[[2]][k]] <- StateCities[MatchedCityIndex[[i]][k],"FIPS"]
        FailDataFrame[rowsForState[RowsWithCityMatch[i]],CityOutCols[[3]][k]] <- StateCities[MatchedCityIndex[[i]][k],"ANSICODE"]
        FailDataFrame[rowsForState[RowsWithCityMatch[i]],CityOutCols[[4]][k]] <- StateCities[MatchedCityIndex[[i]][k],"GNIS_ID"] 
        #print(paste("Matched city", StateCities[MatchedCityIndex[[i]][k],"CITY_NAME"],"to entry", FailDataFrame[rowsForState[i],"LOCATION"],sep=" "))
      } 
    }  
  }
  print("    Finished city identification")
  
  # PROCESS STATE NAMES
  State <- tolower(df.States[df.States$STFIPS==j,"STATE_FULL"])
  RowsWithState <- rowsForState[grep(State, FailDataFrame[rowsForState,"ROAD_NAME"])]
  nRowsWithMatch <- length(RowsWithState)
  if (nRowsWithMatch > 0){
    for (i in RowsWithState){
      FailDataFrame[i,LocProcessColsOut] <- GetAndRemoveLocationData(FailDataFrame[i, LocProcessColsIn], State, LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, TRUE, FALSE)
    }
  }
  # abbreviated version
  State <- tolower(df.States[df.States$STFIPS==j,"STATE_CODE"])
  RowsWithState <- rowsForState[grep(paste("\\<",State,"\\>",sep=""), FailDataFrame[rowsForState,"ROAD_NAME"])]
  nRowsWithMatch <- length(RowsWithState)
  if (nRowsWithMatch > 0){
    for (i in RowsWithState){
      FailDataFrame[i,LocProcessColsOut] <- GetAndRemoveLocationData(FailDataFrame[i, LocProcessColsIn], State, LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, TRUE, FALSE)
    }
  }
  # assume that other mentioned states are "near" statements
  HasStateBool      <- sapply(tolower(df.States$STATE_FULL), grepl, FailDataFrame[rowsForState,"ROAD_NAME"])
  if (nRowsState==1) HasStateBool <- t(HasStateBool)
  RowsWithStateIndex  <- which(rowSums(HasStateBool) != 0)
  RowsWithStateMatch <- rowsForState[RowsWithStateIndex]
  nRowsWithMatch <- length(RowsWithStateMatch)
  if (nRowsWithMatch > 0){
    MatchedStateIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasStateBool[RowsWithStateIndex[i],])))
    if (class(MatchedStateIndex)=="list") MatchedStateIndex <- unlist(MatchedStateIndex)
    StateMatch <- sapply(1:nRowsWithMatch, function(i) tolower(df.States$STATE_FULL[MatchedStateIndex[i]]))
    for (i in 1:nRowsWithMatch){
      FailDataFrame[RowsWithStateMatch[i],LocProcessColsOut] <- GetAndRemoveLocationData(FailDataFrame[RowsWithStateMatch[i], LocProcessColsIn], StateMatch[i], LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, TRUE, FALSE)
    }
  }
  
  # RE-PROCESS COUNTIES, NON-STRICT
  HasCountyStringBool <- sapply(StateCounties$COUNTY_NAME,grepl,FailDataFrame[rowsForState,"ROAD_NAME"])
  if (nRowsState==1) HasCountyStringBool <- t(HasCountyStringBool)
  RowsWithCountyIndex <- which(rowSums(HasCountyStringBool) != 0)
  RowsWithCountyMatch <- rowsForState[RowsWithCountyIndex]
  nRowsWithMatch <- length(RowsWithCountyMatch)
  if (nRowsWithMatch > 0){
    # assume only one county name remaining after city matching
    MatchedCountyIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasCountyStringBool[RowsWithCountyIndex[i],])))
    for (i in 1:nRowsWithMatch){
      FailDataFrame[RowsWithCountyMatch[i],LocProcessColsOut]     <- GetAndRemoveLocationData(FailDataFrame[RowsWithCountyMatch[i], LocProcessColsIn], StateCounties[MatchedCountyIndex[[i]][k],"COUNTY_NAME"], LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, FALSE, FALSE)
      #print(paste("Matched county", StateCounties[MatchedCountyIndex[[i]][k],"COUNTY_NAME"],"to entry", FailDataFrame[rowsForState[i],"LOCATION"],sep=" "))
    }
  }
  
  # RIVER DATA - MIDDLE FORK, ETC
  HasTribStringBool <- sapply(paste("\\<",TribRelatKeys,"\\>",sep=""), grepl, gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",FailDataFrame[rowsForState, "ROAD_NAME"]), perl=TRUE))
  if (nRowsState==1) HasTribStringBool <- t(HasTribStringBool)
  RowsWithTribIndex <- which(rowSums(HasTribStringBool) != 0)
  RowsWithTribMatch <- rowsForState[RowsWithTribIndex]
  nRowsWithMatch <- length(RowsWithTribMatch)
  if (nRowsWithMatch > 0){
    MatchedTribIndex    <- sapply(1:nRowsWithMatch, function(i) min(which(HasTribStringBool[RowsWithTribIndex[i],])))
    # nRowsWithMatchesRow <- sapply(1:nRowsWithMatch, function(i) length(MatchedTribIndex[[i]]))
    for (i in 1:nRowsWithMatch){
      temp_entry  <- gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]","",FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"]), perl=TRUE)
      match_start <- regexpr(paste("\\<",TribRelatKeys[MatchedTribIndex[i]],"\\>",sep=""),temp_entry)
      match_last  <- match_start + attr(match_start,"match.length") - 1
      HasRoadBool   <- sapply(paste("\\<",RoadKeys,"\\>",sep=""), grepl, substr(temp_entry,match_last+1,match_last+5))
      if (all(!HasRoadBool)){
        FailDataFrame[RowsWithTribMatch[i],"STREAM_TRIB"] <- paste(FailDataFrame[RowsWithTribMatch[i],"STREAM_TRIB"], TribRelatKeys[MatchedTribIndex[i]], "-", sep = " ")
        FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"] <- gsub(paste("\\<",TribRelatKeys[MatchedTribIndex[i]],"\\>",sep=""),"",gsub("^ *|(?<= ) | *$", "",gsub("[[:punct:]]"," ",FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"]),perl=TRUE))
      }
    }
  }
  
  # STREAM NAMES AS CAN BE INFERRED, REMOVE IF NO ROAD WORD
  HasStreamStringBool <- sapply(paste("[[:print:]]{3,}\\<",StreamKeys,"\\>",sep=""), grepl, FailDataFrame[rowsForState,"LOCATION"])  
  if (nRowsState==1) HasStreamStringBool <- t(HasStreamStringBool)
  RowsWithStreamMatch <- which(rowSums(HasStreamStringBool) != 0)
  nRowsWithMatch <- length(RowsWithStreamMatch)
  if (nRowsWithMatch > 0){
    MatchedStreamIndex             <- lapply(RowsWithStreamMatch, function(i) which(HasStreamStringBool[i,]))
    nRowsWithMatchesRow                    <- integer(nRowsState)
    nRowsWithMatchesRow[RowsWithStreamMatch] <- sapply(1:nRowsWithMatch, function(i) length(MatchedStreamIndex[[i]]))
    for (i in 1:nRowsWithMatch){
      for (k in 1:nRowsWithMatchesRow[RowsWithStreamMatch[i]]){
        FailDataFrame[rowsForState[RowsWithStreamMatch[i]],LocProcessColsOut] <- GetAndRemoveLocationForStream(FailDataFrame[rowsForState[RowsWithStreamMatch[i]],LocProcessColsIn], StreamKeys[MatchedStreamIndex[[i]][k]], ls.StreamKeys[[StreamKeyIndex[MatchedStreamIndex[[i]][k]]]][1], LocProcessColsOut,ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys)
      }
    }
  }
  print("    Finished stream (in location field) identification")
  
  #  REST OF PARENTHETICALS
  MatchRegex <- "\\([[:print:]]{1,15}\\)"
  RowsWithParentheticalMatchIndex <- grep(MatchRegex, FailDataFrame[rowsForState,"ROAD_NAME"])
  RowsWithParentheticalMatch <- rowsForState[RowsWithParentheticalMatchIndex]
  nRowsWithMatch <- length(RowsWithParentheticalMatch)
  if (nRowsWithMatch >= 1){ 
    for (i in 1:nRowsWithMatch){
      match_start <- regexpr(MatchRegex,FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"])
      match_last  <- match_start + attr(match_start,"match.length") - 1
      FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NEAR"] <- paste(FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NEAR"],substr(FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"],match_start+1,match_last-1),"-",sep=" ")
      FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"] <- gsub(MatchRegex,"",FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"])
    }
  }
  
  # FINAL TRIBS CLEANUP
  HasTribStringBool <- sapply(paste("\\<",unlist(ls.TribsKeys),"\\>",sep=""), grepl, gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",FailDataFrame[rowsForState, "ROAD_NAME"]), perl=TRUE))
  if (nRowsState==1) HasTribStringBool <- t(HasTribStringBool)
  RowsWithTribIndex <- which(rowSums(HasTribStringBool) != 0)
  RowsWithTribMatch <- rowsForState[RowsWithTribIndex]
  nRowsWithMatch <- length(RowsWithTribMatch)
  if (nRowsWithMatch > 0){
    MatchedTribIndex    <- sapply(1:nRowsWithMatch, function(i) min(which(HasTribStringBool[RowsWithTribIndex[i],])))
    for (i in 1:nRowsWithMatch){
      temp_entry  <- gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]","",FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"]), perl=TRUE)
      match_start <- regexpr(paste("\\<",unlist(ls.TribsKeys)[MatchedTribIndex[i]],"\\>",sep=""),temp_entry)
      match_last  <- match_start + attr(match_start,"match.length") - 1
      HasRoadBool   <- sapply(paste("\\<",RoadKeys,"\\>",sep=""), grepl, substr(temp_entry,match_last+1,match_last+5))
      if (all(!HasRoadBool)){
        FailDataFrame[RowsWithTribMatch[i],"STREAM_TRIB"] <- paste(FailDataFrame[RowsWithTribMatch[i],"STREAM_TRIB"], unlist(ls.TribsKeys)[MatchedTribIndex[i]], "-", sep = " ")
        FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"] <- gsub(paste("\\<",unlist(ls.TribsKeys)[MatchedTribIndex[i]],"\\>",sep=""),"",gsub("^ *|(?<= ) | *$", "",gsub("[[:punct:]]"," ",FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"]),perl=TRUE))
      }
    }
  }
  
  # RAILWAYS
  HasRailStringBool <- sapply(paste("\\<",RailKeys,"\\>",sep=""), grepl, gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",FailDataFrame[rowsForState, "ROAD_NAME"]), perl=TRUE))
  if (nRowsState==1) HasRailStringBool <- t(HasRailStringBool)
  RowsWithRailIndex <- which(rowSums(HasRailStringBool) != 0)
  RowsWithRailMatch <- rowsForState[RowsWithRailIndex]
  nRowsWithMatch <- length(RowsWithRailMatch)
  if (nRowsWithMatch > 0){
    MatchedRailIndex    <- sapply(1:nRowsWithMatch, function(i) min(which(HasRailStringBool[RowsWithRailIndex[i],])))
    for (i in 1:nRowsWithMatch){
      temp_entry  <- gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]","",FailDataFrame[RowsWithRailMatch[i],"LOCATION"]), perl=TRUE)
      match_start <- regexpr(paste("\\<",RailKeys[MatchedRailIndex[i]],"\\>",sep=""),temp_entry)
      match_last  <- match_start + attr(match_start,"match.length") - 1
      HasRoadBool   <- sapply(paste("\\<",RoadKeys,"\\>",sep=""), grepl, substr(temp_entry,match_last+1,match_last+5))
      if (all(!HasRoadBool)){
        FailDataFrame[RowsWithRailMatch[i],"ROAD_NAME"] <- gsub(paste("\\<",RailKeys[MatchedRailIndex[i]],"\\>",sep=""),"",gsub("^ *|(?<= ) | *$", "",gsub("[[:punct:]]"," ",FailDataFrame[RowsWithRailMatch[i],"ROAD_NAME"]),perl=TRUE))
        FailDataFrame[RowsWithRailMatch[i],"RAIL_NAME"] <- substr(temp_entry, 1, match_start - 1)
        if (nchar(FailDataFrame[RowsWithRailMatch[i],"RAIL_NAME"]) > 1){
          RailName <- unlist(strsplit(FailDataFrame[RowsWithRailMatch[i],"RAIL_NAME"]," ")) 
          for (j in 1:length(RailName)){
            FailDataFrame[RowsWithRailMatch[i],"ROAD_NAME"] <- sub(paste("\\<",RailName[j],"\\>", sep=""),"", FailDataFrame[RowsWithRailMatch[i],"ROAD_NAME"])
          }
        }
        else{
          FailDataFrame[RowsWithRailMatch[i],"RAIL_NAME"] <- "railway"
        }
      }
    }
  }
  
  # PROCESS ROAD NAMES
  FailDataFrame[rowsForState, "ROAD_NAME"] <- str_trim(gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",FailDataFrame[rowsForState, "ROAD_NAME"]), perl = TRUE))
  NonEmptyRows <- rowsForState[FailDataFrame[rowsForState, "ROAD_NAME"]!=""]
  if (length(NonEmptyRows) >= 1){
    HasRoadTypeBool <- sapply(paste("\\<",RoadKeys,"\\>",sep=""), grepl, FailDataFrame[NonEmptyRows,"ROAD_NAME"])
    if (length(NonEmptyRows)==1) HasRoadTypeBool <- t(HasRoadTypeBool) 
    RowsWithRoadTypeIndex   <- which(rowSums(HasRoadTypeBool) != 0)
    RowsWithRoadTypeMatch   <- NonEmptyRows[RowsWithRoadTypeIndex]
    nRowsWithMatch         <- length(RowsWithRoadTypeMatch)
    RowsWithNoRoadTypeIndex   <- which(rowSums(HasRoadTypeBool) != 1)
    RowsWithNoRoadTypeMatch   <- NonEmptyRows[RowsWithNoRoadTypeIndex]
    nRowsWithNoMatch         <- length(RowsWithNoRoadTypeMatch)
    if (nRowsWithMatch >= 1){
      MatchedRoadTypeIndex  <- sapply(1:length(RowsWithRoadTypeMatch), function(i) max(which(HasRoadTypeBool[RowsWithRoadTypeIndex[i],])))     
      FailDataFrame[RowsWithRoadTypeMatch,"RTE_PREFIX"]   <- sapply(1:length(RowsWithRoadTypeMatch), function(i) ls.RoadKeys[[RoadKeyIndex[MatchedRoadTypeIndex[i]]]][1])
      for (i in 1:nRowsWithMatch){
        temp_entry  <- FailDataFrame[RowsWithRoadTypeMatch[i],"ROAD_NAME"]
        match_start <- regexpr(paste("\\<",RoadKeys[MatchedRoadTypeIndex[i]],"\\>", sep = ""), temp_entry)
        match_last  <- match_start + attr(match_start,"match.length") - 1
        temp_length <- nchar(temp_entry)
        if ((match_start-2) > 1) FailDataFrame[RowsWithRoadTypeMatch[i],"ROAD_NAME"]   <- substr(temp_entry, 1, match_start - 2)
        if ((match_last+2) < temp_length) FailDataFrame[RowsWithRoadTypeMatch[i],"ROAD_NEAR"]   <- paste(FailDataFrame[RowsWithRoadTypeMatch[i],"ROAD_NEAR"] , substr(temp_entry, match_last+2, temp_length), sep=" ")
      }
    } 
    if (nRowsWithNoMatch >= 1){     
      FailDataFrame[RowsWithNoRoadTypeMatch,"ROAD_NEAR"]   <- sapply(RowsWithNoRoadTypeMatch, function(i) paste(FailDataFrame[i,"ROAD_NEAR"], FailDataFrame[i,"ROAD_NAME"],sep=" "))
      FailDataFrame[RowsWithNoRoadTypeMatch, "ROAD_NAME"] <- ""
    }
  }
  FailDataFrame[rowsForState,"ROAD_NEAR"] <- str_trim(FailDataFrame[rowsForState,"ROAD_NEAR"])
}
  