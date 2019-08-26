Find.Features <- function(Data,
                          Features, # Types of features to be extracted and their associated field (column) name (e.g., CITY = "LOCATION_COL_NAME")
                          VERBOSE = VERBOSE){
  source(file.path("Scripts","Process Input","Feature.Detect.R"))

  ## Determine which types of features are to be detected
  Rows  <- rownames(Data)
  ls.cols.out <- list(COUNTY = as.vector(sapply(1:3, 
                                                function(i) 
                                                  paste0(c("COUNTY_NAME_","FIPS_"),i))),
                      CITY   = as.vector(sapply(1:3, 
                                              function(i) 
                                                paste0(c("CITY_NAME_","FIPS_FROM_CITY_","ANSICODE_","GNIS_ID_"),i))),
                      LOCATION = c("LOC_AUX_1","LOC_AUX_2","BRIDGE_NAME"),
                      ROAD     = paste0("ROAD_",c("NAME","TYPE","DIRECTION","AUX")),
                      ROUTE    = as.vector(sapply(1:2, 
                                               function(i) paste0(paste0("ROUTE_",c("NAME_","TYPE_","DIRECTION_","AUX_")),i))),
                      STREAM   = as.vector(sapply(1:2, 
                                                function(i) paste0(paste0("STREAM_",c("NAME_","TYPE_","TRIB_","AUX_")),i))))
  
  ## Loop over features
  for(f in names(Features)){
    if(VERBOSE) print(paste("CHECKING FEATURE:",f,"--------------"))
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
            # str <- regmatches(Data[rows[i],COL],regexpr(pattern,Data[rows[i],COL]))
            if(!is.na(ls.DOT.Keys[[state]][[f]]["MOVE"])){ 
              str <- regmatches(Data[rows[i],COL],regexpr(ls.DOT.Keys[[state]][[f]]["MVPTRN"],Data[rows[i],COL]))
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
              Data[rows[i],COL] <- sub(ls.DOT.Keys[[state]][[f]]["SUBPTRN"],ls.DOT.Keys[[state]][[f]]["SUB"],Data[rows[i],COL])
            }
          }
        } 
      }
      # state-specific and general route naming: strict -------
      if(f=="ROUTE"){
        ls.RteKeysState <- ls.RteKeys
        ls.RteKeysState$state <- tolower(df.States[state,"STATE_CODE"])
        ls.RteKeysState$statefull <- tolower(df.States[state,"STATE_FULL"])
        rows <- Rows[Data$STFIPS==state & !is.na(Data[,COL]) & Data[,COL]!="" &
                       (grepl("[[:digit:]]",Data[,COL]) | any(sapply(unlist(ls.RteKeysState),
                                                                     function(r) grepl(r, Data[,COL]))))]
        if(length(rows) > 0){
          if(VERBOSE) print(paste("Checking for",tolower(f),"in state:",df.States[state,"STATE_FULL"]))
          # full: route type (prefix), digit, and direction
          rows   <- rows[grepl("[[:digit:]]+[[:space:]]?[nsew]{1}",Data[rows,COL])]
          if(length(rows) > 0){
            if(VERBOSE) print("Looking for route number with direction (nb/sb/eb/wb)")
            routes <- as.vector(sapply(unlist(ls.CardKeys[grepl("b",names(ls.CardKeys))]),
                                       function(card)
                                         paste0(unlist(ls.RteKeysState),
                                                "[[:space:]]?[[:punct:]]?[[:space:]]?[[:digit:]]+[[:space:]]?",
                                                card,"\\>")))
            routes     <- data.frame(PATTERN1 = routes, PATTERN2 = "", REGEX = TRUE, stringsAsFactors = FALSE)
            routes$ROUTE_NAME <- "[[:digit:]]+"
            routes$ROUTE_TYPE <- unlist(ls.RteKeysState)
            routes$ROUTE_DIRECTION <- sapply(1:nrow(routes), function(i) 
              gsub("[[:punct:]]","",regmatches(routes[i,"PATTERN1"],
                                               regexpr("([snew]{1}[ouraseth]{0,4}[b]{1}[ound]{0,4})|([snew]{1}[\\\\>]{2})",
                                                       routes[i,"PATTERN1"],
                                                       perl = TRUE, useBytes = TRUE))))
            routes     <- routes[order(nchar(routes$PATTERN1)),]
            Data[rows,c(COL,cols.out)] <- Feature.Detect(Data[rows,], 
                                                         routes, 
                                                         "NONE", 
                                                         COL, 
                                                         cols.out, 
                                                         n.dup.cols = n.dup.cols, 
                                                         DELETE = TRUE)
          }
          
          # route type and digit only
          rows <- Rows[Data$STFIPS==state & !is.na(Data[,COL]) & Data[,COL]!="" &
                         (grepl("[[:digit:]]",Data[,COL]) & 
                            apply(sapply(unlist(ls.RteKeysState),
                                         function(r) grepl(r, Data[,COL])),
                                  MARGIN = 1, any))]
          if(length(rows) > 0){
            if(VERBOSE) print("Looking for route number without direction")
            routes     <- data.frame(PATTERN1 = unlist(ls.RteKeysState), PATTERN2 = "", REGEX = TRUE)
            routes$PATTERN1 <- paste0(routes$PATTERN1,"[[:space:]]?[[:punct:]]?[[:space:]]?[[:digit:]]+\\>")
            routes$ROUTE_NAME <- "[[:digit:]]+"
            routes$ROUTE_TYPE <- unlist(ls.RteKeysState)
            routes     <- routes[order(nchar(routes$PATTERN1)),]
            Data[rows,c(COL,cols.out)] <- Feature.Detect(Data[rows,], 
                                                         routes, 
                                                         "NONE", 
                                                         COL, 
                                                         cols.out, 
                                                         n.dup.cols = n.dup.cols, 
                                                         DELETE = TRUE)
          }
        }
      }
    }
    
    # non state-specific checks ---------
    rows        <- Rows[!is.na(Data[,COL]) & Data[,COL]!=""]
    if(f =="LOCATION"){
      if(VERBOSE) print("Moving to non-state-specific location checks")
      cols.out <- cols.out[grepl("AUX",cols.out)]
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
        
        # non-explicit distance relationals -------
        rows        <- Rows[!is.na(Data[,COL]) &
                              apply(
                              sapply(unlist(ls.RelationalKeys[c("off","from",
                                                                "by","of","near","at",
                                                                "in_loc")]),
                                     function(s) grepl(s,Data[,COL])),
                              MARGIN = 1, any)]
        if(length(rows)>0){
          locales     <- unlist(ls.JurisKeys[c("county")])
          relationals <- as.vector(sapply(c("", locales),
                                  function(loc)
                                    sapply(c("",unlist(ls.CardKeys[1:8])),
                                           function(card)
                                             sapply(unlist(ls.RelationalKeys[c("off","from",
                                                                               "by","of","near","at",
                                                                               "in_loc")]),
                                                    function(rel)
                                                      paste0(card,
                                                             "[[:space:]]?",
                                                             rel,
                                                             "[[:space:]][[:alpha:]]+",
                                                             paste0("( ",loc,")?"))
                                  ))))
          relationals <- sub("^(\\[\\[\\:space\\:\\]\\]\\?)","\\\\<",relationals,perl = TRUE)
          relationals <- sub("( )?","", relationals, fixed = TRUE)
          relationals <- relationals[!duplicated(relationals)]
          relationals <- relationals[order(nchar(relationals), decreasing = TRUE)]
          
          relationals <- data.frame(PATTERN1 = relationals, PATTERN2 = "", REGEX = TRUE, stringsAsFactors = FALSE)
          relationals[,paste0(COL,"_AUX")] <- relationals$PATTERN1
          if(VERBOSE) print("Checking relational auxilliary phrases")
          Data[rows,c(COL,cols.out[grepl("AUX",cols.out)])] <- Feature.Detect(Data[rows,],
                                                                              relationals,
                                                                              "NONE",
                                                                              COL,
                                                                              cols.out[grepl("AUX",cols.out)],
                                                                              n.dup.cols = n.dup.cols,
                                                                              DELETE = TRUE)
        }
        
      }
    }
    if(f=="STREAM"){
      if(VERBOSE) print("Moving to non-state-specific stream checks")
      # Tributary checks -------
      Data[rows,COL] <- gsub("[[:punct:]]","",Data[rows,COL])
      rows <- rows[apply(sapply(unlist(ls.TribKeys),
                                function(s) grepl(s,Data[rows,COL])),
                         MARGIN = 1, any)]
      if(length(rows)>0){
        if(VERBOSE) print("Checking for tributaries, forks, branches")
        tribs <- data.frame(PATTERN1 = ls.TribKeys, PATTERN2 = "", REGEX = TRUE, stringsAsFactors = FALSE)
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
    
    # road name, type, direction, aux  ------
    if(f=="ROAD"){
      rows <- Rows[!is.na(Data[,COL]) & Data[,COL]!=""]
      if(length(rows)>0){
        roads <- unlist(ls.RoadKeys)
      }
    }
    
    # return to state-specific for strict cleanup of city and county -------------
    if(f %in% c("LOCATION", "STREAM", "BRIDGE")){
      for(state in unique(Data$STFIPS)){
      # clean up explicitly named counties or cities
      rows  <- Rows[!is.na(Data[,COL]) & Data[,COL]!="" & Data$STFIPS==state & 
                      (!is.na(Data[,"FIPS_1"]) | !is.na(Data[,"ANSICODE_1"]))] 
      if(length(rows)>0){
        # end with county, or county and not county road/route ------
        rows  <- Rows[!is.na(Data[,COL]) & Data[,COL]!="" & Data$STFIPS==state & 
                        !is.na(Data[,"FIPS_1"]) & sapply(Rows, function(i) grepl(Data[i,"COUNTY_NAME_1"],Data[i,COL]))]
        if(length(rows)>0){
        localities <- data.frame(PATTERN1 = tolower(df.Counties[df.Counties$STFIPS_C == 
                                                                  state, "COUNTY_NAME"]),
                                 PATTERN2 = "",
                                 REGEX   = TRUE, stringsAsFactors = FALSE)
        notMatch   <- switch(f,
                             "LOCATION" = " (?!((road)|(rd[.]?)|(ro[.]?$)|(route)|(rt[e.]?))))",
                             "STREAM"   = " (?!((stream)|(str[.]?)|(f[or]?k[.]?$)|(route)|(creek))))",
                             "BRIDGE"   = "  (?!((bridge)|(br[.]?))))")
        localities$PATTERN1 <- paste0("(",
                                      localities$PATTERN1,
                                      "\\Z)|(",
                                      localities$PATTERN1,
                                      " co([unty.]{0,4})\\Z)|(",
                                      localities$PATTERN1,
                                      " co([unty.]{0,4},))|(",
                                      localities$PATTERN1,
                                      " co([unty.]{0,4})",
                                      notMatch)
        
        localities$LOC      <- paste0(tolower(df.Counties[df.Counties$STFIPS_C == 
                                                            state, "COUNTY_NAME"]),
                                      " co([unty.]{0,4})")
        if(VERBOSE) print("removing '___ county' names (strict)")
        Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                         localities, 
                                         "NONE",
                                         COL, 
                                         COL, 
                                         perl = TRUE,
                                         useBytes = TRUE,
                                         n.dup.cols = 1, 
                                         DELETE = TRUE)
        }
      # county of ----
      rows  <- rows[grepl("\\<of\\>",Data[rows,COL])] 
      if(length(rows)>0){
        localities$PATTERN1 <- paste0("co[unty.]? of ",
                                      tolower(df.Counties[df.Counties$STFIPS_C 
                                                          == state, "COUNTY_NAME"]),"\\>")
        localities$LOC      <- localities$PATTERN1
        if(VERBOSE) print("removing 'county of ____' names (strict)")
        Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                         localities, 
                                         "NONE", 
                                         COL, 
                                         COL, 
                                         n.dup.cols = 1, 
                                         DELETE = TRUE)
      }
      # city of ----
      rows  <- Rows[!is.na(Data[,COL]) & Data[,COL]!="" & Data$STFIPS==state & 
                      !is.na(Data[,"ANSICODE_1"]) &
                      grepl("\\<of\\>",Data[,COL]) & sapply(Rows, function(i) grepl(Data[i,"CITY_NAME_1"],Data[i,COL]))] 
      if(length(rows)>0){
        localities <- data.frame(PATTERN1 = as.vector(sapply(
          unlist(ls.JurisKeys[c("city", "town", 
                                "township", "village")]),
          function(l) paste0(l,"[.]? of ",
                             tolower(df.Cities[df.Cities$STFIPS_C==state,"CITY_NAME"])))),
          PATTERN2 = "", REGEX = TRUE)
        localities$LOC      <- localities$PATTERN1
        if(VERBOSE) print("removing 'city of ___' names (strict)")
        Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                         localities, 
                                         "NONE", 
                                         COL, 
                                         COL, 
                                         n.dup.cols = 1, 
                                         DELETE = TRUE)
      }
      # __ city or __ at end of line -----
      rows  <- Rows[!is.na(Data[,COL]) & Data[,COL]!="" & Data$STFIPS==state & 
                      !is.na(Data[,"ANSICODE_1"]) &
                      sapply(Rows, function(i) grepl(Data[i,"CITY_NAME_1"],Data[i,COL]))]
      if(length(rows)>0){
        localities <- data.frame(PATTERN1 = as.vector(sapply(
          unlist(ls.JurisKeys[c("city", "town", 
                                "township", "village")]),
          function(l) paste0("",
                             tolower(df.Cities[df.Cities$STFIPS_C==state,"CITY_NAME"]),
                             " ",l,"[.]?$"))),
          PATTERN2 = "", REGEX = TRUE)
        localities <- rbind(localities,
                            data.frame(PATTERN1 = 
                                         paste0(tolower(df.Cities[df.Cities$STFIPS_C 
                                                                  ==state,"CITY_NAME"]),"\\Z"),
                                       PATTERN2 = "", REGEX = TRUE))
        localities$LOC <- localities$PATTERN1
        if(VERBOSE) print("removing '___ city' names (strict)")
        Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                         localities, 
                                         "NONE", 
                                         COL, 
                                         COL, 
                                         perl = TRUE,
                                         useBytes = TRUE,
                                         n.dup.cols = 1, 
                                         DELETE = TRUE)
      }
      }
      
      # may be excess punctuation and spacing, clean up
      Data[,COL] <- str_squish(gsub("[[:punct:]]"," ",Data[,COL]))
      }
    }
    
    # now detecting feature name and type
  }
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
  