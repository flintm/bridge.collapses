Find.Features <- function(Data,
                          Features, # Types of features to be extracted and their associated field (column) name (e.g., CITY = "LOCATION_COL_NAME")
                          VERBOSE = VERBOSE){
  source(file.path("Scripts","Process Input","Feature.Detect.R"))

  ## Determine which types of features are to be detected
  Rows  <- rownames(Data)
  ls.cols.out <- list(COUNTY = as.vector(sapply(1:2, 
                                                function(i) 
                                                  paste0(c("COUNTY_NAME_","FIPS_"),i))),
                      CITY = as.vector(sapply(1:2, 
                                              function(i) 
                                                paste0(c("CITY_NAME_","FIPS_FROM_CITY_","ANSICODE_","GNIS_ID_"),i))),
                      LOCATION = c("LOC_AUX_1","LOC_AUX_2","BRIDGE_NAME"),
                      ROAD = paste0("ROAD_",c("NAME","TYPE","DIRECTION","AUX")),
                      ROUTE = as.vector(sapply(1:2, 
                                               function(i) paste0(paste0("ROUTE_",c("NAME_","TYPE_","DIRECTION_","AUX_")),i))),
                      STREAM = as.vector(sapply(1:2, 
                                                function(i) paste0(paste0("STREAM_",c("NAME_","TYPE_","TRIB_","AUX_")),i))))
  
  ## Loop over features
  for(f in names(Features)){
    if(VERBOSE) print(f)
    COL        <- Features[f]
    cols.out   <- ls.cols.out[[f]]
    names.out  <- unique(sub("_[[:digit:]]+","",cols.out))
    n.dup.cols <- ifelse(any(grepl("[[:digit:]]",ls.cols.out[[f]])),
                         max(as.numeric(gsub("[A-Z_]+","",ls.cols.out[[f]])),na.rm = T),
                         1)
    Data[,cols.out] <- NA_character_

    # state-dependent checks for possible county and city names and special processing -------
    for(state in unique(Data$STFIPS)){
      rows <- Rows[Data$STFIPS==state & !is.na(Data[,COL])]
      if(f %in% c("COUNTY", "CITY")){
        if(VERBOSE) print(paste("Checking for",tolower(f),"in state:",df.States[state,"STATE_FULL"]))
        localities <- switch(f,
                             COUNTY = df.Counties[df.Counties$STFIPS_C == state, c("COUNTY_NAME", "FIPS_C")],
                             CITY   = df.Cities[df.Cities$STFIPS_C == state, c("CITY_NAME", "FIPS_C", "GNIS_ID", "ANSICODE")])
        colnames(localities)[grepl("NAME",colnames(localities))] <- "NAME"
        colnames(localities)[colnames(localities)=="FIPS_C"] <- "FIPS"
        localities$PATTERN1 <- localities$NAME
        localities$PATTERN2 <- ""
        localities$REGEX    <- FALSE
        
        # check for presence of locality name and record name and FIPS or other standard codes
        Data[rows,c(COL,cols.out)] <- Feature.Detect(Data[rows,], 
                                                     localities, 
                                                     "NONE", 
                                                     COL, 
                                                     cols.out, 
                                                     n.dup.cols = n.dup.cols, 
                                                     DELETE = FALSE)
      }
      # state-specific, not city or county --------
      if(state %in% names(ls.DOT.Keys)){
        if(f %in% names(ls.DOT.Keys[[state]])){
          pattern <- ls.DOT.Keys[[state]][[f]]["PATTERN"]
          if(VERBOSE) print(paste("Checking for state-specific pattern",pattern,"in state:",df.States[state,"STATE_FULL"]))
          match.keys <- grep(pattern,Data[rows,COL])
          for(i in match.keys){
            str <- regmatches(Data[rows[i],COL],regexpr(pattern,Data[rows[i],COL]))
            if(!is.na(ls.DOT.Keys[[state]][[f]]["MOVE"])){ 
              Data[rows[i],ls.DOT.Keys[[state]][[f]]["MOVE"]] <- ifelse(!is.na(Data[rows[i],ls.DOT.Keys[[state]][[f]]["MOVE"]]),
                                                                        paste(Data[rows[i],ls.DOT.Keys[[state]][[f]]["MOVE"]],
                                                                              str),
                                                                        str)
              if(VERBOSE) print(paste('moved to col',ls.DOT.Keys[[state]][[f]]["MOVE"]))
              }
            if(!is.na(ls.DOT.Keys[[state]][[f]]["ADDTO"])){ 
              Data[rows[i],ls.DOT.Keys[[state]][[f]]["ADDTO"]] <- ifelse(!is.na(Data[rows[i],ls.DOT.Keys[[state]][[f]]["ADDTO"]]),
                                                                         paste(Data[rows[i],ls.DOT.Keys[[state]][[f]]["ADDTO"]],
                                                                               ls.DOT.Keys[[state]][[f]]["ADD"]),
                                                                         ls.DOT.Keys[[state]][[f]]["ADD"])
              if(VERBOSE) print(paste('added to col',ls.DOT.Keys[[state]][[f]]["ADDTO"]))
              }
                                                                                                                 
            if(!is.na(ls.DOT.Keys[[state]][[f]]["SUB"])){ 
              if(VERBOSE) print(paste('Deleted in col',COL))
              Data[rows[i],COL] <- sub(ls.DOT.Keys[[state]][[f]][["SUBPTRN"]],ls.DOT.Keys[[state]][[f]][["SUB"]],Data[rows[i],COL])
            }
          }
        } 
      }
      
      # state-specific route naming -------
      if(f=="ROUTE"){
        ls.RteKeysState <- ls.RteKeys
        ls.RteKeysState$state <- tolower(df.States[state,"STATE_CODE"])
        ls.RteKeysState$statefull <- tolower(df.States[state,"STATE_FULL"])
        
        if(VERBOSE) print(paste("Checking for",tolower(f),"in state:",df.States[state,"STATE_FULL"]))
        routes     <- data.frame(PATTERN1 = unlist(ls.RteKeysState), PATTERN2 = "", REGEX = TRUE)
        routes$PATTERN1 <- paste0(routes$PATTERN1,"[[:space:]]?[[:punct:]]?[[:digit:]]+")
        routes$ROUTE_NAME <- "[[:digit:]]+"
        routes$ROUTE_TYPE <- unlist(ls.RteKeysState)
        Data[rows,c(COL,cols.out)] <- Feature.Detect(Data[rows,], 
                                                     routes, 
                                                     "NONE", 
                                                     COL, 
                                                     cols.out, 
                                                     n.dup.cols = n.dup.cols, 
                                                     DELETE = TRUE)
      }
    }
    
    # non state-specific checks ---------
    if(VERBOSE) print("Moving to non-state-specific checks")
    rows        <- Rows[!is.na(Data[,COL]) & Data[,COL]!=""]
    if(f =="LOCATION"){
      cols.out <- cols.out[grepl("AUX",cols.out)]
      print(cols.out)
      # aux phrases: -------
      if(length(rows)>0){
        # parentheticals ------
        rows    <- rows[grepl("(",Data[rows,COL], fixed = TRUE)]
        if(length(rows)>0){
          parenth <- data.frame(PATTERN1 = "\\(*\\)", PATTERN2 = "",
                                REGEX = TRUE, stringsAsFactors = FALSE)
          parenth[,"LOC_AUX"] <- "\\([a-z 0-9]+\\)"
          if(VERBOSE) print("Checking parenthetical auxilliary phrases")
          Data[rows,c(COL,cols.out)] <- Feature.Detect(Data[rows,],
                                                                 parenth,
                                                                 "NONE",
                                                                 COL,
                                                                 cols.out,
                                                                 n.dup.cols = 2,
                                                                 DELETE = TRUE)
        }

        # statements with explicit distance, e.g., "3.5 miles from Stanford"---------
        # TODO: fix such that pulls out the location that it's related to
        rows <- Rows[!is.na(Data[,COL]) & grepl("[[:digit:]]",Data[,COL])]
        if(length(rows)>0){
          relationals <- sapply(unlist(ls.RelationalKeys[c("miles","kilometers")]),
                                function(dist)
                                  sapply(c("",unlist(ls.CardKeys[1:8])),
                                         function(card)
                                           sapply(c("",unlist(ls.RelationalKeys[c("off","from","by","of")])),
                                                  function(rel)
                                                    paste0("[[:digit:]]+[.]?[[:digit:]]?[[:space:]]?",
                                                           dist,
                                                           "[[:space:]]?",
                                                           card,
                                                           "[[:space:]]?",
                                                           rel,
                                                           "[[:space:]]?[[:alpha:]]+")
                                           )))
          relationals <- data.frame(PATTERN1 = as.vector(relationals), PATTERN2 = "",
                                    REGEX = TRUE, stringsAsFactors = FALSE)
          relationals$PATTERN1 <- gsub("[[:space:]]?[[:space:]]?","[[:space:]]?",relationals$PATTERN1,fixed = TRUE)
          relationals$PATTERN1 <- sub("[[:space:]]?[[:alpha:]]+","\\> [[:alpha:]]+",relationals$PATTERN1,fixed = TRUE)
          relationals <- relationals[!duplicated(relationals$PATTERN1),]
          relationals[,"LOC_AUX"] <- relationals$PATTERN1
          relationals <- relationals[order(nchar(relationals$PATTERN1),decreasing = TRUE),]
          if(VERBOSE) print("Checking distance-relational auxilliary phrases")
          Data[rows,c(COL,cols.out)] <- Feature.Detect(Data[rows,],
                                                       relationals,
                                                       "NONE",
                                                       COL,
                                                       cols.out,
                                                       n.dup.cols = 2,
                                                       DELETE = TRUE)
        }
        
        
        # non-explicit distance relationals
        # rows        <- Rows[!is.na(Data[,COL]) & 
        #                       apply(
        #                       sapply(unlist(ls.RelationalKeys[c("off","from",
        #                                                         "by","of","near","at",
        #                                                         "in_loc")]),
        #                              function(s) grepl(s,Data[,COL])),
        #                       MARGIN = 1, any)]
        # if(length(rows)>0){
        #   print(rows)
        #   relationals <- sapply(c("",unlist(ls.CardKeys[1:8])),
        #                         function(card) 
        #                           sapply(unlist(ls.RelationalKeys[c("off","from",
        #                                                             "by","of","near","at",
        #                                                             "in_loc")]),
        #                                  function(rel)
        #                                    paste0(card,
        #                                           "[[:space:]]?",
        #                                           rel,
        #                                           "[[:space:]][[:alpha:]]+")
        #                           ))
        #   relationals <- data.frame(PATTERN1 = as.vector(relationals), PATTERN2 = "", REGEX = TRUE, stringsAsFactors = FALSE)
        #   relationals[,paste0(COL,"_AUX")] <- relationals$PATTERN1
        #   if(VERBOSE) print("Checking relational auxilliary phrases")
        #   Data[rows,c(COL,cols.out[grepl("AUX",cols.out)])] <- Feature.Detect(Data[rows,], 
        #                                                                       relationals, 
        #                                                                       "NONE", 
        #                                                                       COL, 
        #                                                                       cols.out[grepl("AUX",cols.out)], 
        #                                                                       n.dup.cols = n.dup.cols, 
        #                                                                       DELETE = TRUE)
        # }

        rows  <- Rows[!is.na(Data[,COL]) & Data[,COL]!=""] #------
        if(length(rows)>0){
          localities <- data.frame(PATTERN1 = df.Counties[df.Counties$STFIPS_C == state, "COUNTY_NAME"],
                                   PATTERN2 = "",
                                   REGEX   = TRUE, stringsAsFactors = FALSE)
          localities$PATTERN1 <- paste0(tolower(localities$PATTERN1),"[[:space:]]co[unty.]?[[:space:]]?[^r(oad)+]")
          localities$LOC      <- localities$PATTERN1
          if(VERBOSE) print("removing county names (strict)")
          Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                           localities, 
                                           "NONE", 
                                           COL, 
                                           COL, 
                                           n.dup.cols = 1, 
                                           DELETE = TRUE)
          localities$PATTERN1 <- paste0("co[unty.]? of ",tolower(df.Counties[df.Counties$STFIPS_C == state, "COUNTY_NAME"]),"\\>")
          localities$LOC      <- localities$PATTERN1
          if(VERBOSE) print("removing county names (strict)")
          Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                           localities, 
                                           "NONE", 
                                           COL, 
                                           COL, 
                                           n.dup.cols = 1, 
                                           DELETE = TRUE)
        }
      }
    }
    if(f=="STREAM"){
      # Tributary checks
      Data[rows,COL] <- gsub("[[:punct:]]","",Data[rows,COL])
      rows <- rows[apply(sapply(unlist(ls.TribKeys),
                                function(s) grepl(s,Data[rows,COL])),
                         MARGIN = 1, any)]
      if(length(rows)>0){
        if(VERBOSE) print("Checking for tributaries, forks, branches")
        tribs <- data.frame(PATTERN1 = unlist(ls.TribKeys), PATTERN2 = "", REGEX = TRUE, stringsAsFactors = FALSE)
        tribs$STREAM_TRIB <- tribs$PATTERN1
        tribs <- tribs[order(nchar(tribs$PATTERN1),decreasing = TRUE),]
        Data[rows,c(COL,"STREAM_TRIB_1","STREAM_TRIB_2")] <- Feature.Detect(Data[rows,], 
                                         tribs, 
                                         "NONE", 
                                         COL, 
                                         c("STREAM_TRIB_1","STREAM_TRIB_2"), 
                                         n.dup.cols = 2, 
                                         DELETE = TRUE)
      }
    }
  }
  # Data[,c("STREAM_UNDER","STREAM_NAME_1","STREAM_TYPE_1","STREAM_TRIB_1")] <- data.frame(c("",""),
                                                                                         # c("forked deer", "sandy"),
                                                                                         # c("river","creek"),
                                                                                         # c("south fork","left fork"),
                                                                                         # stringsAsFactors = FALSE)
  rm(Feature.Detect, envir = .GlobalEnv)
  return(Data) #---------
}

#   # FIND BRIDGE NO (I.E., ANY NUMBERS IN PARENTHESES OR WITH "NO" BUT NOT "ROUTE NO" OR SIMILAR)
#   # Assumed bridge numbers in parentheticals
#   MatchRegex <- "\\([[:digit:]]{0,5}[[:punct:]]?[[:alpha:]]{0,2}[[:punct:]]?[[:digit:]]{0,5}\\>\\)"
#   RowsWithParentheticalMatchIndex <- grep(MatchRegex, FailDataFrame[rowsForState,"ROAD_NAME"])
#   RowsWithParentheticalMatch <- rowsForState[RowsWithParentheticalMatchIndex]
#   nRowsWithMatch <- length(RowsWithParentheticalMatch)
#   if (nRowsWithMatch >= 1){ 
#     # print(RowsWithParentheticalMatch)
#     for (i in 1:nRowsWithMatch){
#       match_start <- regexpr(MatchRegex,FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"])
#       match_last  <- match_start + attr(match_start,"match.length") - 1
#       FailDataFrame[RowsWithParentheticalMatch[i],"BRIDGE_NAME_NO"] <- substr(FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"],match_start+1,match_last-1)
#       FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"] <- gsub(MatchRegex,"",FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"])
#     }
#   }
#   # ones that say "bridge no" or similar
#   HasBridgeNoBool <- sapply(paste("\\<",BridgeNoKeys,"\\>[[:space:]]{0,2}[[:digit:]]{1,5}",sep=""), grepl, gsub("[[:punct:]]","",FailDataFrame[rowsForState,"ROAD_NAME"]))
#   if (nRowsState == 1) HasBridgeNoBool <- t(HasBridgeNoBool) 
#   RowsWithBridgeNoMatchIndex <- which(rowSums(HasBridgeNoBool) != 0)
#   RowsWithBridgeNoMatch   <- rowsForState[RowsWithBridgeNoMatchIndex]
#   nRowsWithMatch <- length(RowsWithBridgeNoMatch)
#   if (nRowsWithMatch >= 1){ 
#     MatchedBridgeNoIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasBridgeNoBool[RowsWithBridgeNoMatchIndex[i],])))
#     if (class(MatchedBridgeNoIndex)=="list") MatchedBridgeNoIndex <- unlist(MatchedBridgeNoIndex)
#     for (i in 1:nRowsWithMatch){
#       match_start <- regexpr(paste("\\<",BridgeNoKeys[MatchedBridgeNoIndex[i]],"\\>[[:space:]]{0,2}[[:digit:]]{1,5}",sep=""),gsub("[[:punct:]]","",FailDataFrame[RowsWithBridgeNoMatch[i],"ROAD_NAME"]))
#       match_last  <- match_start + attr(match_start,"match.length") - 1
#       br_start   <- regexpr("[[:digit:]]{1,4}",FailDataFrame[RowsWithBridgeNoMatch[i],"ROAD_NAME"])
#       FailDataFrame[RowsWithBridgeNoMatch[i],"BRIDGE_NAME_NO"] <- substr(gsub("[[:punct:]]","",FailDataFrame[RowsWithBridgeNoMatch[i],"ROAD_NAME"]),br_start,match_last)
#       FailDataFrame[RowsWithBridgeNoMatch[i],"ROAD_NAME"] <- gsub(paste("\\<",BridgeNoKeys[MatchedBridgeNoIndex[i]],"\\>[[:space:]]{0,2}[[:digit:]]{1,5}",sep=""),"",gsub("[[:punct:]]","",FailDataFrame[RowsWithBridgeNoMatch[i],"ROAD_NAME"]))
#     }
#   }
# 
#   # ones that say ___ ___ bridge
#   HasBridgeBool <- sapply(paste("\\<",ls.RoadKeys$bridge,"\\>",sep=""), grepl, gsub("[[:space:]]+|[[:space:]]"," ",gsub("[[:punct:]]","",FailDataFrame[rowsForState,"ROAD_NAME"])))
#   if (nRowsState == 1) HasBridgeBool <- t(HasBridgeBool) 
#   RowsWithBridgeMatchIndex <- which(rowSums(HasBridgeBool) != 0)
#   RowsWithBridgeMatch   <- rowsForState[RowsWithBridgeMatchIndex]
#   nRowsWithMatch <- length(RowsWithBridgeMatch)
#   if (nRowsWithMatch >= 1){ 
#     MatchedBridgeIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasBridgeBool[RowsWithBridgeMatchIndex[i],])))
#     if (class(MatchedBridgeIndex)=="list") MatchedBridgeIndex <- unlist(MatchedBridgeIndex)
#     for (i in 1:nRowsWithMatch){
#       temp_entry  <- str_trim(gsub("[[:punct:]]","",FailDataFrame[RowsWithBridgeMatch[i],"ROAD_NAME"]))
#       match_start <- regexpr(paste("\\<",ls.RoadKeys$bridge[MatchedBridgeIndex[i]],"\\>",sep=""),temp_entry)
#       match_last  <- match_start + attr(match_start,"match.length") - 1
#       FailDataFrame[RowsWithBridgeMatch[i],"BRIDGE_NAME_NO"] <- substr(temp_entry,1,match_start-1)
#       HasStreamBool <- sapply(paste("\\<",StreamKeys,"\\>",sep=""), grepl, substr(temp_entry,1,match_start-1))
#       HasRoadBool   <- sapply(paste("\\<",RoadKeys,"\\>",sep=""), grepl, substr(temp_entry,match_last+1,match_last+5))
#       if (any(HasStreamBool)){
#         FailDataFrame[RowsWithBridgeMatch[i],"ROAD_NAME"]      <- paste(substr(temp_entry,1,match_start-1),
#                                                                         substr(temp_entry,match_last+1,nchar(temp_entry)), sep = ",")
#       }
#       else{
#         if (any(HasRoadBool)){
#           # do nothing
#         }
#         else{
#           FailDataFrame[RowsWithBridgeMatch[i],"ROAD_NAME"]      <- substr(temp_entry,match_last+1,nchar(temp_entry))
#         }
#       }
#     }
#   }
#   

#   # find alphabet route numbers (i.e., supplemental or secondary routes in Missouri)
#   HasRteTypeBool <- sapply(paste("\\<",RteKeys,"\\>[[:punct:]]?[[:space:]]?\\<[[:alpha:]]{1,2}\\>",sep=""), grepl, FailDataFrame[rowsForState,"ROAD_NAME"])
#   if (length(rowsForState)==1) HasRteTypeBool <- t(HasRteTypeBool) 
#   RowsWithRteTypeIndex   <- which(rowSums(HasRteTypeBool) != 0)
#   RowsWithRteTypeMatch   <- rowsForState[RowsWithRteTypeIndex]
#   nRowsWithMatch         <- length(RowsWithRteTypeMatch)
#   if (nRowsWithMatch >= 1){
#     MatchedRteTypeIndex  <- sapply(1:length(RowsWithRteTypeMatch), function(i) max(which(HasRteTypeBool[RowsWithRteTypeIndex[i],])))
#     if (class(MatchedRteTypeIndex) == "list") MatchedRteTypeIndex <- unlist(MatchedRteTypeIndex)
#     for (i in 1:nRowsWithMatch){
#       RteType <- RteKeys[MatchedRteTypeIndex[i]]
#       rte_start   <- regexpr(paste("\\<",RteType,"\\>[[:punct:]]?[[:space:]]?\\<[[:alpha:]]{1,2}\\>",sep=""),FailDataFrame[RowsWithRteTypeMatch[i],"ROAD_NAME"])
#       rte_last    <- rte_start + attr(rte_start, "match.length") - 1
#       rte_temp    <- substr(FailDataFrame[RowsWithRteTypeMatch[i],"ROAD_NAME"], rte_start, rte_last)
#       FailDataFrame[RowsWithRteTypeMatch[i],"ROUTE_NO"]    <- gsub("[[:space:]]","",gsub("[[:punct:]]","",substr(rte_temp,nchar(RteType)+1,nchar(rte_temp))))
#       FailDataFrame[RowsWithRteTypeMatch[i], "RTE_PREFIX"] <- ls.RteKeysState[[RteKeyIndex[MatchedRteTypeIndex[i]]]][1]
#       FailDataFrame[RowsWithRteTypeMatch[i], "ROAD_NAME"]  <- paste(substr(FailDataFrame[RowsWithRteTypeMatch[i], "ROAD_NAME"],1,rte_start-1),substr(FailDataFrame[RowsWithRteTypeMatch[i], "ROAD_NAME"],rte_last+1,nchar(FailDataFrame[RowsWithRteTypeMatch[i], "ROAD_NAME"])), sep=", ")
#     }
#   }  
#   
#   # ROUTE DIRECTION - GRAB FROM ROUTE NO, THEN FROM OTHER PLACES 
#   HasDirectionBool <- sapply(paste("[[:digit:]]",CardinalKeys,sep=""), grepl, FailDataFrame[rowsForState,"ROUTE_NO"])
#   if (length(rowsForState)==1) HasDirectionBool <- t(as.vector(unlist(HasDirectionBool))) 
#   RowsWithDirectionRowsWithMatchIndex <- which(rowSums(HasDirectionBool) != 0)
#   RowsWithDirectionRowsWithMatch <- rowsForState[RowsWithDirectionRowsWithMatchIndex]
#   nRowsWithMatch <- length(RowsWithDirectionRowsWithMatch)
#   if (length(RowsWithDirectionRowsWithMatch)>=1){
#     MatchedDirectionIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasDirectionBool[RowsWithDirectionRowsWithMatchIndex[i],])))     
#     FailDataFrame[RowsWithDirectionRowsWithMatch,"RTE_DIRECTION"]   <- sapply(1:nRowsWithMatch, function(i) ls.CardinalKeys[[CardinalKeyIndex[MatchedDirectionIndex[i]]]][1])
#     FailDataFrame[RowsWithDirectionRowsWithMatch,"ROUTE_NO"]   <- sapply(1:nRowsWithMatch, function(i) gsub(paste(CardinalKeys[MatchedDirectionIndex[i]],"\\>",sep=""),"", FailDataFrame[RowsWithDirectionRowsWithMatch[i],"ROUTE_NO"]))
#   }
#   # check for nb/wb/eb/sb in ROAD_NAME
#   HasDirectionBool <- sapply(paste("\\<",CardBoundKeys,"\\>",sep=""), grepl, FailDataFrame[rowsForState,"ROAD_NAME"])
#   if (length(rowsForState)==1) HasDirectionBool <- t(as.vector(unlist(HasDirectionBool))) 
#   RowsWithDirectionRowsWithMatchIndex <- which(rowSums(HasDirectionBool) != 0)
#   RowsWithDirectionRowsWithMatch <- rowsForState[RowsWithDirectionRowsWithMatchIndex]
#   nRowsWithMatch <- length(RowsWithDirectionRowsWithMatch)
#   if (length(RowsWithDirectionRowsWithMatch)>=1){
#     MatchedDirectionIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasDirectionBool[RowsWithDirectionRowsWithMatchIndex[i],])))     
#     FailDataFrame[RowsWithDirectionRowsWithMatch,"RTE_DIRECTION"]   <- sapply(1:nRowsWithMatch, function(i) ls.CardinalKeys[[CardBoundKeyIndex[MatchedDirectionIndex[i]]]][1])
#     FailDataFrame[RowsWithDirectionRowsWithMatch,"ROAD_NAME"]   <- sapply(1:nRowsWithMatch, function(i) gsub(paste("\\<",CardBoundKeys[MatchedDirectionIndex[i]],"\\>",sep=""),"", FailDataFrame[RowsWithDirectionRowsWithMatch[i],"ROAD_NAME"]))
#   }
#   

#   # RIVER DATA - MIDDLE FORK, ETC
#   HasTribStringBool <- sapply(paste("\\<",TribRelatKeys,"\\>",sep=""), grepl, gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",FailDataFrame[rowsForState, "ROAD_NAME"]), perl=TRUE))
#   if (nRowsState==1) HasTribStringBool <- t(HasTribStringBool)
#   RowsWithTribIndex <- which(rowSums(HasTribStringBool) != 0)
#   RowsWithTribMatch <- rowsForState[RowsWithTribIndex]
#   nRowsWithMatch <- length(RowsWithTribMatch)
#   if (nRowsWithMatch > 0){
#     MatchedTribIndex    <- sapply(1:nRowsWithMatch, function(i) min(which(HasTribStringBool[RowsWithTribIndex[i],])))
#     # nRowsWithMatchesRow <- sapply(1:nRowsWithMatch, function(i) length(MatchedTribIndex[[i]]))
#     for (i in 1:nRowsWithMatch){
#       temp_entry  <- gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]","",FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"]), perl=TRUE)
#       match_start <- regexpr(paste("\\<",TribRelatKeys[MatchedTribIndex[i]],"\\>",sep=""),temp_entry)
#       match_last  <- match_start + attr(match_start,"match.length") - 1
#       HasRoadBool   <- sapply(paste("\\<",RoadKeys,"\\>",sep=""), grepl, substr(temp_entry,match_last+1,match_last+5))
#       if (all(!HasRoadBool)){
#         FailDataFrame[RowsWithTribMatch[i],"STREAM_TRIB"] <- paste(FailDataFrame[RowsWithTribMatch[i],"STREAM_TRIB"], TribRelatKeys[MatchedTribIndex[i]], "-", sep = " ")
#         FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"] <- gsub(paste("\\<",TribRelatKeys[MatchedTribIndex[i]],"\\>",sep=""),"",gsub("^ *|(?<= ) | *$", "",gsub("[[:punct:]]"," ",FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"]),perl=TRUE))
#       }
#     }
#   }
#   
#   # STREAM NAMES AS CAN BE INFERRED, REMOVE IF NO ROAD WORD
#   HasStreamStringBool <- sapply(paste("[[:print:]]{3,}\\<",StreamKeys,"\\>",sep=""), grepl, FailDataFrame[rowsForState,"LOCATION"])  
#   if (nRowsState==1) HasStreamStringBool <- t(HasStreamStringBool)
#   RowsWithStreamMatch <- which(rowSums(HasStreamStringBool) != 0)
#   nRowsWithMatch <- length(RowsWithStreamMatch)
#   if (nRowsWithMatch > 0){
#     MatchedStreamIndex             <- lapply(RowsWithStreamMatch, function(i) which(HasStreamStringBool[i,]))
#     nRowsWithMatchesRow                    <- integer(nRowsState)
#     nRowsWithMatchesRow[RowsWithStreamMatch] <- sapply(1:nRowsWithMatch, function(i) length(MatchedStreamIndex[[i]]))
#     for (i in 1:nRowsWithMatch){
#       for (k in 1:nRowsWithMatchesRow[RowsWithStreamMatch[i]]){
#         FailDataFrame[rowsForState[RowsWithStreamMatch[i]],LocProcessColsOut] <- GetAndRemoveLocationForStream(FailDataFrame[rowsForState[RowsWithStreamMatch[i]],LocProcessColsIn], StreamKeys[MatchedStreamIndex[[i]][k]], ls.StreamKeys[[StreamKeyIndex[MatchedStreamIndex[[i]][k]]]][1], LocProcessColsOut,ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys)
#       }
#     }
#   }
#   print("    Finished stream (in location field) identification")
#   
#   
#   # FINAL TRIBS CLEANUP
#   HasTribStringBool <- sapply(paste("\\<",unlist(ls.TribsKeys),"\\>",sep=""), grepl, gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",FailDataFrame[rowsForState, "ROAD_NAME"]), perl=TRUE))
#   if (nRowsState==1) HasTribStringBool <- t(HasTribStringBool)
#   RowsWithTribIndex <- which(rowSums(HasTribStringBool) != 0)
#   RowsWithTribMatch <- rowsForState[RowsWithTribIndex]
#   nRowsWithMatch <- length(RowsWithTribMatch)
#   if (nRowsWithMatch > 0){
#     MatchedTribIndex    <- sapply(1:nRowsWithMatch, function(i) min(which(HasTribStringBool[RowsWithTribIndex[i],])))
#     for (i in 1:nRowsWithMatch){
#       temp_entry  <- gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]","",FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"]), perl=TRUE)
#       match_start <- regexpr(paste("\\<",unlist(ls.TribsKeys)[MatchedTribIndex[i]],"\\>",sep=""),temp_entry)
#       match_last  <- match_start + attr(match_start,"match.length") - 1
#       HasRoadBool   <- sapply(paste("\\<",RoadKeys,"\\>",sep=""), grepl, substr(temp_entry,match_last+1,match_last+5))
#       if (all(!HasRoadBool)){
#         FailDataFrame[RowsWithTribMatch[i],"STREAM_TRIB"] <- paste(FailDataFrame[RowsWithTribMatch[i],"STREAM_TRIB"], unlist(ls.TribsKeys)[MatchedTribIndex[i]], "-", sep = " ")
#         FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"] <- gsub(paste("\\<",unlist(ls.TribsKeys)[MatchedTribIndex[i]],"\\>",sep=""),"",gsub("^ *|(?<= ) | *$", "",gsub("[[:punct:]]"," ",FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"]),perl=TRUE))
#       }
#     }
#   }
#   
#   # RAILWAYS
#   HasRailStringBool <- sapply(paste("\\<",RailKeys,"\\>",sep=""), grepl, gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",FailDataFrame[rowsForState, "ROAD_NAME"]), perl=TRUE))
#   if (nRowsState==1) HasRailStringBool <- t(HasRailStringBool)
#   RowsWithRailIndex <- which(rowSums(HasRailStringBool) != 0)
#   RowsWithRailMatch <- rowsForState[RowsWithRailIndex]
#   nRowsWithMatch <- length(RowsWithRailMatch)
#   if (nRowsWithMatch > 0){
#     MatchedRailIndex    <- sapply(1:nRowsWithMatch, function(i) min(which(HasRailStringBool[RowsWithRailIndex[i],])))
#     for (i in 1:nRowsWithMatch){
#       temp_entry  <- gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]","",FailDataFrame[RowsWithRailMatch[i],"LOCATION"]), perl=TRUE)
#       match_start <- regexpr(paste("\\<",RailKeys[MatchedRailIndex[i]],"\\>",sep=""),temp_entry)
#       match_last  <- match_start + attr(match_start,"match.length") - 1
#       HasRoadBool   <- sapply(paste("\\<",RoadKeys,"\\>",sep=""), grepl, substr(temp_entry,match_last+1,match_last+5))
#       if (all(!HasRoadBool)){
#         FailDataFrame[RowsWithRailMatch[i],"ROAD_NAME"] <- gsub(paste("\\<",RailKeys[MatchedRailIndex[i]],"\\>",sep=""),"",gsub("^ *|(?<= ) | *$", "",gsub("[[:punct:]]"," ",FailDataFrame[RowsWithRailMatch[i],"ROAD_NAME"]),perl=TRUE))
#         FailDataFrame[RowsWithRailMatch[i],"RAIL_NAME"] <- substr(temp_entry, 1, match_start - 1)
#         if (nchar(FailDataFrame[RowsWithRailMatch[i],"RAIL_NAME"]) > 1){
#           RailName <- unlist(strsplit(FailDataFrame[RowsWithRailMatch[i],"RAIL_NAME"]," ")) 
#           for (j in 1:length(RailName)){
#             FailDataFrame[RowsWithRailMatch[i],"ROAD_NAME"] <- sub(paste("\\<",RailName[j],"\\>", sep=""),"", FailDataFrame[RowsWithRailMatch[i],"ROAD_NAME"])
#           }
#         }
#         else{
#           FailDataFrame[RowsWithRailMatch[i],"RAIL_NAME"] <- "railway"
#         }
#       }
#     }
#   }
#   
#   # PROCESS ROAD NAMES
#   FailDataFrame[rowsForState, "ROAD_NAME"] <- str_trim(gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",FailDataFrame[rowsForState, "ROAD_NAME"]), perl = TRUE))
#   NonEmptyRows <- rowsForState[FailDataFrame[rowsForState, "ROAD_NAME"]!=""]
#   if (length(NonEmptyRows) >= 1){
#     HasRoadTypeBool <- sapply(paste("\\<",RoadKeys,"\\>",sep=""), grepl, FailDataFrame[NonEmptyRows,"ROAD_NAME"])
#     if (length(NonEmptyRows)==1) HasRoadTypeBool <- t(HasRoadTypeBool) 
#     RowsWithRoadTypeIndex   <- which(rowSums(HasRoadTypeBool) != 0)
#     RowsWithRoadTypeMatch   <- NonEmptyRows[RowsWithRoadTypeIndex]
#     nRowsWithMatch         <- length(RowsWithRoadTypeMatch)
#     RowsWithNoRoadTypeIndex   <- which(rowSums(HasRoadTypeBool) != 1)
#     RowsWithNoRoadTypeMatch   <- NonEmptyRows[RowsWithNoRoadTypeIndex]
#     nRowsWithNoMatch         <- length(RowsWithNoRoadTypeMatch)
#     if (nRowsWithMatch >= 1){
#       MatchedRoadTypeIndex  <- sapply(1:length(RowsWithRoadTypeMatch), function(i) max(which(HasRoadTypeBool[RowsWithRoadTypeIndex[i],])))     
#       FailDataFrame[RowsWithRoadTypeMatch,"RTE_PREFIX"]   <- sapply(1:length(RowsWithRoadTypeMatch), function(i) ls.RoadKeys[[RoadKeyIndex[MatchedRoadTypeIndex[i]]]][1])
#       for (i in 1:nRowsWithMatch){
#         temp_entry  <- FailDataFrame[RowsWithRoadTypeMatch[i],"ROAD_NAME"]
#         match_start <- regexpr(paste("\\<",RoadKeys[MatchedRoadTypeIndex[i]],"\\>", sep = ""), temp_entry)
#         match_last  <- match_start + attr(match_start,"match.length") - 1
#         temp_length <- nchar(temp_entry)
#         if ((match_start-2) > 1) FailDataFrame[RowsWithRoadTypeMatch[i],"ROAD_NAME"]   <- substr(temp_entry, 1, match_start - 2)
#         if ((match_last+2) < temp_length) FailDataFrame[RowsWithRoadTypeMatch[i],"ROAD_NEAR"]   <- paste(FailDataFrame[RowsWithRoadTypeMatch[i],"ROAD_NEAR"] , substr(temp_entry, match_last+2, temp_length), sep=" ")
#       }
#     } 
#     if (nRowsWithNoMatch >= 1){     
#       FailDataFrame[RowsWithNoRoadTypeMatch,"ROAD_NEAR"]   <- sapply(RowsWithNoRoadTypeMatch, function(i) paste(FailDataFrame[i,"ROAD_NEAR"], FailDataFrame[i,"ROAD_NAME"],sep=" "))
#       FailDataFrame[RowsWithNoRoadTypeMatch, "ROAD_NAME"] <- ""
#     }
#   }
#   FailDataFrame[rowsForState,"ROAD_NEAR"] <- str_trim(FailDataFrame[rowsForState,"ROAD_NEAR"])
# }
  