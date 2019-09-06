Find.Features <- function(Data,
                          Features, # Types of features to be extracted and their associated field (column) name (e.g., CITY = "LOCATION_COL_NAME")
                          VERBOSE = VERBOSE){
  source(file.path("Scripts","Process Input","Feature.Detect.R"))

  ## Determine which types of features are to be detected
  Rows  <- rownames(Data)
  FLAG  <- rep(FALSE, length(unique(Features)))
  names(FLAG) <- unique(Features)
  ls.cols.out <- list(COUNTY = as.vector(sapply(1:3, 
                                                function(i) 
                                                  paste0(c("COUNTY_NAME_","FIPS_"),i))),
                      CITY   = as.vector(sapply(1:7, 
                                              function(i) 
                                                paste0(c("CITY_NAME_","FIPS_FROM_CITY_","ANSICODE_","GNIS_ID_"),i))),
                      LOCATION = c("LOC_AUX_1","LOC_AUX_2","BRIDGE_NAME"),
                      ROAD     = paste0("ROAD_",c("NAME","TYPE","DIRECTION","AUX")),
                      ROUTE    = as.vector(sapply(1:2, 
                                               function(i) paste0(paste0("ROUTE_",c("NAME_","TYPE_","DIR_","AUX_")),i))),
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
                             COUNTY = df.Counties[df.Counties$STFIPS_C == state, c("county", "FIPS_C")],
                             CITY   = df.Cities[df.Cities$STFIPS_C == state, c("city", "FIPS_C", "GNIS_ID", "ANSICODE")])
        colnames(localities)[1] <- "NAME"
        colnames(localities)[colnames(localities)=="FIPS_C"] <- "FIPS"
        localities$PATTERN <- localities$NAME
        localities$REGEX    <- FALSE
        localities2 <- localities[grepl("(\\<north\\>)|(\\<south\\>)|(\\<east\\>)|(\\<west\\>)|(\\<new\\>)|(\\<old\\>)",
                                        localities$PATTERN),]
        localities2$PATTERN <- str_squish(sub("(\\<north\\>)|(\\<south\\>)|(\\<east\\>)|(\\<west\\>)|(\\<new\\>)|(\\<old\\>)","",
                                    localities2$PATTERN))
        localities <- rbind(localities, localities2)
        
        # look for other populated places
        if(f=="CITY"){ 
          localities[localities$ANSICODE=="-999","ANSICODE"] <- NA_character_
          localities[localities$GNIS_ID=="-999", "GNIS_ID"] <- NA_character_
          localities3 <- df.GNIS[df.GNIS$STFIPS==state & df.GNIS$FEATURE_CLASS %in% c("Civil"), c("name","FIPS","GNIS_ID")]
          colnames(localities3)[1] <- "NAME"
          localities3$ANSICODE <- NA_character_
          localities3$PATTERN  <- localities3$NAME
          localities3$REGEX    <- FALSE
          
          # delete those already present by GNIS
          localities3 <- localities3[!(localities3$GNIS_ID %in% localities$GNIS_ID) & !grepl("county",localities3$name),]
    
          localities <- rbind(localities, localities3)
          rm(localities2, localities3)
          localities <- localities[order(nchar(localities$NAME), decreasing = TRUE),]
        }

        # check for presence of locality name and record name and FIPS or other standard codes
        Data[rows,c(COL,cols.out)] <- Feature.Detect(Data[rows,], 
                                                     localities, 
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
              str <- regmatches(Data[rows[i],COL],
                                regexpr(ls.DOT.Keys[[state]][[f]]["MVPTRN"],
                                        Data[rows[i],COL]))
              Data[rows[i],ls.DOT.Keys[[state]][[f]]["MOVE"]] <- ifelse(!is.na(Data[rows[i],ls.DOT.Keys[[state]][[f]]["MOVE"]]),
                                                                        paste(Data[rows[i],ls.DOT.Keys[[state]][[f]]["MOVE"]],
                                                                              str),
                                                                        str)
              # if(VERBOSE) print(paste('moved to col',ls.DOT.Keys[[state]][[f]]["MOVE"]))
              }
            if(!is.na(ls.DOT.Keys[[state]][[f]]["ADDTO"])){ 
              Data[rows[i],ls.DOT.Keys[[state]][[f]]["ADDTO"]] <- ifelse(!is.na(Data[rows[i],ls.DOT.Keys[[state]][[f]]["ADDTO"]]),
                                                                         paste(Data[rows[i],ls.DOT.Keys[[state]][[f]]["ADDTO"]],
                                                                               ls.DOT.Keys[[state]][[f]]["ADD"]),
                                                                         ls.DOT.Keys[[state]][[f]]["ADD"])
              # if(VERBOSE) print(paste('added to col',ls.DOT.Keys[[state]][[f]]["ADDTO"]))
              }
            if(!is.na(ls.DOT.Keys[[state]][[f]]["SUB"])){ 
              # if(VERBOSE) print(paste('Deleted in col',COL))
              Data[rows[i],COL] <- sub(ls.DOT.Keys[[state]][[f]]["SUBPTRN"],
                                       ls.DOT.Keys[[state]][[f]]["SUB"],
                                       Data[rows[i],COL])
            }
          }
        } 
      }
      
      # state-specific and general route naming: strict -------
      if(f=="ROUTE"){
        ls.RteKeysState <- ls.RteKeys[names(ls.RteKeys)!="statefull"]
        ls.RteKeysState$state <- c(tolower(df.States[state,"STATE_CODE"]), tolower(df.States[state,"STATE_FULL"]))
        if(substr(ls.RteKeysState$state[1],1,1) %in% c("a", "c", "d", "g", "k", "l", "m", "n", "o", "t", "u", "w") &
           !grepl("(north)|(south)|(west)", ls.RteKeysState[2])) ls.RteKeysState$state[3] <- substr(ls.RteKeysState$state[1],1,1)
        rows <- Rows[Data$STFIPS==state & !is.na(Data[,COL]) & Data[,COL]!="" &
                       (grepl("[[:digit:]]",Data[,COL]) | apply(sapply(unlist(ls.RteKeysState),
                                                                     function(r) grepl(r, Data[,COL])),
                                                                MARGIN = 1, any))]
        if(length(rows) > 0){
          if(VERBOSE) print(paste("Checking for",tolower(f),"in state:",df.States[state,"STATE_FULL"]))
          
          # full: route type (prefix), digit, and direction
          rows   <- rows[grepl("[[:digit:]][ .,-]?[nsew]{1}",Data[rows,COL])]
          if(length(rows) > 0){
            if(VERBOSE) print("Looking for route number with direction (nb/sb/eb/wb)")
            routes <- as.vector(sapply(unlist(ls.CardKeys[grepl("b",names(ls.CardKeys))]),
                                       function(card)
                                         paste0(unlist(ls.RteKeysState),
                                                "[ .,-]{0,2}[[:digit:]]+[ ]?",
                                                card,"\\>")))
            routes     <- data.frame(PATTERN = routes,  REGEX = TRUE, stringsAsFactors = FALSE)
            routes$ROUTE_NAME <- "[[:digit:]]+"
            routes$ROUTE_TYPE <- sub("([\\[])(.+)","",routes$PATTERN)
            routes$ROUTE_DIRECTION <- sapply(1:nrow(routes), function(i) 
              gsub("[[:punct:]]","",regmatches(routes[i,"PATTERN"],
                                               regexpr("([snew]{1}[ouraseth]{0,4}[b]{1}[ound]{0,4})|([snew]{1}[\\\\>]{2})",
                                                       routes[i,"PATTERN"],
                                                       perl = TRUE, useBytes = TRUE))))
            routes     <- routes[order(nchar(routes$PATTERN)),]
            Data[rows,c(COL,cols.out)] <- Feature.Detect(Data[rows,], 
                                                         routes, 
                                                          
                                                         COL, 
                                                         cols.out, 
                                                         n.dup.cols = n.dup.cols, 
                                                         DELETE = TRUE)
          }
          
          # with modified/aux: route type (prefix), digit, and business/bypass/etc. suffix
          # Eg US Route 1 Alternate, US Route 1A, US Route 1A Business
          # AT = Alternate Truck, TB = Truck Business, Business, Spur, Connector, Scenic
          # Inner/Outler Loop, Bypass. Alt., Bus. Truck, Byp, Temp., Conn., Spur,  Old
          pattern <- unlist(ls.SpecKeys)
          pattern <- pattern[nchar(pattern)>1]
          pattern <- paste0("([[:digit:]]+ \\<",pattern)
          pattern[1:(length(pattern)-1)] <- paste0(pattern[1:(length(pattern)-1)],"\\>)|")
          pattern <- c("([[:digit:]]+[abtcsio]{1}\\>)|",pattern,"\\>)")
          pattern <- paste0(pattern,collapse = "")
          
          rows   <- Rows[Data$STFIPS==state & !is.na(Data[,COL]) & Data[,COL]!="" &
                        grepl(pattern,Data[,COL])]
          if(length(rows) > 0){
            if(VERBOSE) print("Looking for special route number (bypass, business,...)")
            routes <- as.vector(sapply(unlist(ls.SpecKeys),
                                       function(alt)
                                         paste0(unlist(ls.RteKeysState),
                                                "[[:space:]]?[[:punct:]]?[[:space:]]?[[:digit:]]+[[:space:]]?",
                                                alt,"\\>")))
            routes     <- data.frame(PATTERN = routes,  REGEX = TRUE, stringsAsFactors = FALSE)
            routes$ROUTE_NAME <- "[[:digit:]]+"
            routes$ROUTE_TYPE <- sub("([\\[])(.+)","",routes$PATTERN)
            routes$ROUTE_AUX  <- paste0("(\\<",paste(unlist(ls.SpecKeys),collapse = "\\>)|(\\<"),"\\>)")
            routes     <- routes[order(nchar(routes$PATTERN)),]
            Data[rows,c(COL,cols.out)] <- Feature.Detect(Data[rows,], 
                                                         routes, 
                                                          
                                                         COL, 
                                                         cols.out, 
                                                         n.dup.cols = n.dup.cols, 
                                                         DELETE = TRUE)
          }
          # Modified/aux preceding route prefix and number
          rows <- Rows[Data$STFIPS==state & !is.na(Data[,COL]) & Data[,COL]!="" &
                         (grepl("[[:digit:]]",Data[,COL]) | apply(sapply(unlist(ls.RteKeysState),
                                                                       function(r) grepl(r, Data[,COL])),
                                                                  MARGIN = 1, any)) &
                       grepl("[abtcs]{1}[[:print:]]{1,12}[[:digit:]]+[[:alpha:]]?\\>",
                             Data[,COL])]
          
          # route type and digit only
          rows <- Rows[Data$STFIPS==state & !is.na(Data[,COL]) & Data[,COL]!="" &
                         (grepl("[[:digit:]]",Data[,COL]) & 
                            apply(sapply(unlist(ls.RteKeysState),
                                         function(r) grepl(r, Data[,COL])),
                                  MARGIN = 1, any))]
          if(length(rows) > 0){
            if(VERBOSE) print("Looking for route number without direction")
            routes     <- data.frame(PATTERN = unlist(ls.RteKeysState),  
                                     REGEX = TRUE, stringsAsFactors = FALSE)
            routes$PATTERN <- paste0(routes$PATTERN,"[ .-]{0,2}[[:digit:]]+\\>")
            routes$ROUTE_NAME <- "[[:digit:]]+"
            routes$ROUTE_TYPE <- unlist(ls.RteKeysState)
            routes     <- routes[order(nchar(routes$PATTERN)),]
            Data[rows,c(COL,cols.out)] <- Feature.Detect(Data[rows,], 
                                                         routes, 
                                                          
                                                         COL, 
                                                         cols.out, 
                                                         n.dup.cols = n.dup.cols, 
                                                         DELETE = TRUE)
          }
        }
      }
    }
    # Look for bridge aux here
    
    # non state-specific auxilliary checks ---------
    rows        <- Rows[!is.na(Data[,COL]) & Data[,COL]!=""]
    if(f =="LOCATION"){
      if(VERBOSE) print("Moving to non-state-specific location checks")
      cols.out <- cols.out[grepl("AUX",cols.out)]
      n.dup.cols <- length(cols.out)
      # aux phrases: -------
      if(length(rows)>0){
        # statements with explicit distance, e.g., "3.5 miles from Stanford"---------
        # TODO: fix such that pulls out the location that it's related to
        rows <- Rows[!is.na(Data[,COL]) & 
                       grepl("[[:digit:]][[:space:]]?(m|k)",Data[,COL])]
        if(length(rows)>0){
          if(VERBOSE) print("Checking distance-relational auxilliary phrases")
          # note: assumes that the relational destination ("stanford") is 1 or 2 words in length
          # followed by end-of-line, comma, period, or dash.
          relationals <- sapply(unlist(ls.RelationalKeys[c("miles","kilometers")]),
                                function(dist)
                                  sapply(c("",unlist(ls.CardKeys[1:8])),
                                         function(card)
                                           sapply(c("",unlist(ls.RelationalKeys[c("off","from","by","of")])),
                                                  function(rel)
                                                    paste0("([[:digit:]]+[.]?[[:digit:]]?[[:space:]]?)(",
                                                           dist,
                                                           " ",
                                                           card,
                                                           " ",
                                                           rel,
                                                           ")( [[:alpha:]]+[[:space:]]?[[:alpha:]]{0,}[[:space:]]?[[:alpha:]]{0,})([,.-]|$)")
                                           )))
          relationals <- data.frame(PATTERN = as.vector(relationals), 
                                    REGEX = TRUE, stringsAsFactors = FALSE)
          relationals$PATTERN <- gsub("  "," ",relationals$PATTERN,fixed = TRUE)
          relationals$PATTERN <- gsub(" )",")",relationals$PATTERN,fixed = TRUE)
          # relationals$PATTERN <- sub("[[:space:]]?[[:alpha:]]+","\\> [[:alpha:]]+",relationals$PATTERN,fixed = TRUE)
          # relationals <- relationals[!duplicated(relationals$PATTERN),]
          relationals$AUX <- relationals$PATTERN
          relationals <- relationals[order(nchar(relationals$PATTERN),decreasing = TRUE),]
          
          Data[rows,c(COL,cols.out)] <- Feature.Detect(Data[rows,],
                                                       relationals,
                                                       
                                                       COL,
                                                       cols.out,
                                                       n.dup.cols = n.dup.cols,
                                                       DELETE = TRUE)
        }
        
        # non-explicit distance relationals -------
        rows        <- Rows[!is.na(Data[,COL]) &
                              (apply(
                              sapply(paste0("\\<",unlist(ls.RelationalKeys[c("off","from",
                                                                "by","of","near","at",
                                                                "in_loc")]),"\\>"),
                                     function(s) grepl(s,Data[,COL])),
                              MARGIN = 1, any) |
                                apply(
                                  sapply(paste0("\\<",unlist(ls.CardKeys[!grepl("b",names(ls.CardKeys))]),"\\>"),
                                         function(s) grepl(s,Data[,COL])),
                                  MARGIN = 1, any))]
        if(length(rows)>0){
          if(VERBOSE) print("Checking relational auxilliary phrases")
          relationals <- as.vector(sapply(c("",unlist(ls.CardKeys[1:8])),
                                           function(card)
                                             sapply(c(unlist(ls.RelationalKeys[c("off","from",
                                                                               "by","of","near","at",
                                                                               "in_loc")]),""),
                                                    function(rel)
                                                      paste0("([\\(]?)(\\<",card,"\\>",
                                                             "[. ]{0,2}",
                                                             rel,
                                                             ")( [[:alpha:]]+[[:space:]]?[[:alpha:]]{0,}[[:space:]]?[[:alpha:]]{0,})([,.-\\)]|$)")
                                  )))
          relationals <- sub("(\\<\\>[. ]{0,2}", "(\\<",relationals, fixed = TRUE)
          # relationals <- sub("[. ]{0,2} ","[.]? ", relationals, fixed = TRUE)
          relationals <- relationals[order(nchar(relationals), decreasing = TRUE)]
          relationals <- relationals[relationals!="([\\(]?)(\\<)( [[:alpha:]]+[[:space:]]?[[:alpha:]]{0,}[[:space:]]?[[:alpha:]]{0,})([,.-\\)]|$)"]
          relationals <- relationals[relationals!="([\\(]?)(\\<of)( [[:alpha:]]+[[:space:]]?[[:alpha:]]{0,}[[:space:]]?[[:alpha:]]{0,})([,.-\\)]|$)"]
          relationals <- data.frame(PATTERN = relationals,  REGEX = TRUE, stringsAsFactors = FALSE)
          relationals$AUX <-relationals$PATTERN # sub(" [[:alpha:]]+","",relationals$PATTERN, fixed = TRUE)
          # relationals[!grepl("(\\?)$",relationals$AUX),
                      # "AUX"] <- paste0(relationals[!grepl("(\\?)$",
                      #                                                  relationals$AUX),"AUX"],
                      #                                        "\\>")
          # print(n.dup.cols)
          # print(colnames(relationals))
          # print(c(COL,cols.out))
          Data[rows,c(COL,cols.out)] <- Feature.Detect(Data[rows,],
                                                       relationals,
                                                       
                                                       COL,
                                                       cols.out,
                                                       n.dup.cols = n.dup.cols,
                                                       DELETE = TRUE)
          Data[rows,COL] <- str_squish(gsub("(","",gsub(")","",Data[rows,COL], fixed = T), fixed = TRUE))
        }
        # return(Data)
        # parentheticals ------
        rows    <- Rows[!is.na(Data[,COL]) & grepl("(",Data[,COL], fixed = TRUE)]
        if(length(rows)>0){
          if(VERBOSE) print("Checking parenthetical auxilliary phrases")
          parenth <- data.frame(PATTERN = "[\\(][.,a-z0-9-]+[\\)]", 
                                REGEX = TRUE, stringsAsFactors = FALSE)
          parenth$AUX <- parenth$PATTERN
          Data[rows,c(COL,cols.out)] <- Feature.Detect(Data[rows,],
                                                       parenth,
                                                       
                                                       COL,
                                                       cols.out,
                                                       n.dup.cols = n.dup.cols,
                                                       DELETE = TRUE)
          Data[,COL] <- str_squish(gsub("(","",gsub(")","",Data[,COL], fixed = T), fixed = TRUE))
          
          
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
        tribs <- data.frame(PATTERN = ls.TribKeys,  REGEX = TRUE, stringsAsFactors = FALSE)
        tribs$STREAM_TRIB <- tribs$PATTERN
        tribs <- tribs[order(nchar(tribs$PATTERN),decreasing = TRUE),]
        Data[rows,c(COL,"STREAM_TRIB_1","STREAM_TRIB_2")] <- Feature.Detect(Data[rows,], 
                                         tribs, 
                                          
                                         COL, 
                                         c("STREAM_TRIB_1","STREAM_TRIB_2"), 
                                         n.dup.cols = 2, 
                                         DELETE = TRUE)
      }
    }
    
    # road name, type, direction, aux !! Add business/bypass/...!! ------
    if(f=="ROAD"){
      rows <- Rows[!is.na(Data[,COL]) & Data[,COL]!=""]
      if(length(rows)>0){
        roads <- unlist(ls.RoadKeys)
      }
    }
    
    # return to state-specific for strict cleanup of city and county -------------
    if(f %in% c("LOCATION", "STREAM", "BRIDGE")){
      if(!FLAG[COL]){
        FLAG[COL] <- TRUE # only need to do this once per each original field
        for(state in unique(Data$STFIPS)){
          # clean up explicitly named counties or cities
          rows  <- Rows[!is.na(Data[,COL]) & Data[,COL]!="" & Data$STFIPS==state & 
                          (!is.na(Data$FIPS_1) | !is.na(Data$FIPS_FROM_CITY_1))] 
          # print(Data[rows,COL])
          if(length(rows)>0){
            # end with county, or county and not county road/route ------
            rows  <- Rows[!is.na(Data[,COL]) & Data[,COL]!="" & Data$STFIPS==state & 
                            !is.na(Data[,"FIPS_1"])]
            if(length(rows)>0){
              localities <- data.frame(PATTERN = df.Counties[df.Counties$STFIPS_C == 
                                                                state, "county"],
                                       
                                       REGEX   = TRUE, stringsAsFactors = FALSE)
              notMatch   <- switch(f,
                                   "LOCATION" = " (?!((road)|(rd[.]?)|(ro[.]?$)|(route)|(rt[e.]?))))",
                                   "STREAM"   = " (?!((stream)|(str[.]?)|(f[or]?k[.]?$)|(route)|(creek))))",
                                   "BRIDGE"   = "  (?!((bridge)|(br[.]?))))")
              localities$PATTERN <- paste0("(",
                                            localities$PATTERN,
                                            "\\Z)|(",
                                            localities$PATTERN,
                                            " co([unty.]{0,4})\\Z)|(",
                                            localities$PATTERN,
                                            " co([unty.]{0,4},))|(",
                                            localities$PATTERN,
                                            " co([unty.]{0,4})",
                                            notMatch)
              
              localities$LOC      <- paste0("(",df.Counties[df.Counties$STFIPS_C == 
                                                              state, "county"],
                                            " co([unty.]{0,4}))|(",
                                            df.Counties[df.Counties$STFIPS_C == 
                                                          state, "county"],
                                            "\\Z)")
              if(VERBOSE) print("removing '___ county' names (strict)")
              Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                               localities, 
                                               COL, 
                                               COL, 
                                               perl = TRUE,
                                               useBytes = TRUE,
                                               n.dup.cols = 1, 
                                               DELETE = TRUE)
            }
            # print(Data[rows,COL])
            # county of ----
            rows  <- rows[grepl("\\<of\\>",Data[rows,COL])] 
            if(length(rows)>0){
              localities$PATTERN <- paste0("co[unty.]? of ",
                                            df.Counties[df.Counties$STFIPS_C 
                                                        == state, "county"],"\\>")
              localities$LOC      <- localities$PATTERN
              if(VERBOSE) print("removing 'county of ____' names (strict)")
              Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                               localities, 
                                               COL, 
                                               COL, 
                                               n.dup.cols = 1, 
                                               DELETE = TRUE)
            }
            Data[,COL] <- sub("([,][ ]?)$","",Data[,COL])
            print(Data[rows,COL])
            # city of ----
            rows  <- Rows[!is.na(Data[,COL]) & Data[,COL]!="" & Data$STFIPS==state & 
                            !is.na(Data$FIPS_FROM_CITY_1) &
                            grepl("\\<of\\>",Data[,COL])] 
            if(length(rows)>0){
              cities <- unique(c(df.Cities[df.Cities$STFIPS_C==state,"city"],
                                     df.GNIS[df.GNIS$STFIPS==state & df.GNIS$FEATURE_CLASS =="Civil","name"]))
              localities <- data.frame(PATTERN = as.vector(sapply(
                unlist(ls.JurisKeys[c("city", "town", 
                                      "township", "village")]),
                function(l) paste0(l,"[.]? of ",
                                   cities))),
                 REGEX = TRUE, stringsAsFactors = FALSE)
              
              localities$LOC      <- localities$PATTERN
              if(VERBOSE) print("removing 'city of ___' names (strict)")
              Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                               localities, 
                                               COL, 
                                               COL, 
                                               n.dup.cols = 1, 
                                               DELETE = TRUE)
            }
            print(Data[rows,COL])
            # city __ OR __ city OR __ at end of line/end of clause -----
            rows  <- Rows[!is.na(Data[,COL]) & Data[,COL]!="" & Data$STFIPS==state & 
                            !is.na(Data$FIPS_FROM_CITY_1)]
            if(length(rows)>0){
              localities <- data.frame(PATTERN = as.vector(sapply(
                unlist(ls.JurisKeys[c("city", "town", 
                                      "township", "village")]),
                function(l) paste0("\\b",
                                   cities,
                                   "\\b ",l,"[.]?\\b"))),
                 REGEX = TRUE, stringsAsFactors = FALSE)
              localities <- rbind(localities,
                                  data.frame(PATTERN = as.vector(sapply(
                unlist(ls.JurisKeys[c("city", "town", 
                                      "township", "village")]),
                function(l) paste0("\\b",l,"[.]?\\b \\b",
                                   cities,
                                   "\\b"))),
                REGEX = TRUE, stringsAsFactors = FALSE))
              localities <- rbind(localities,
                                  data.frame(PATTERN = 
                                               paste0("(",cities,"\\Z)|(",
                                                      cities,",)"),
                                              REGEX = TRUE, stringsAsFactors = FALSE))
              localities$LOC <- localities$PATTERN
              if(VERBOSE) print("removing '___ city' names (strict)")
              Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                               localities, 
                                               COL, 
                                               COL, 
                                               perl = TRUE,
                                               useBytes = TRUE,
                                               n.dup.cols = 1, 
                                               DELETE = TRUE)
            }
            print(Data[rows,COL])
            # Those without a full match but explicit name, e.g. "liberty twp", which doesn't
            # match the state-specific options of "north liberty", "new liberty", etc.
            rows <- Rows[!is.na(Data[,COL]) & Data[,COL]!="" & Data$STFIPS==state & 
                           !is.na(Data$FIPS_FROM_CITY_1) & apply(sapply(
                             unlist(ls.JurisKeys[c("city", "town", 
                                                   "township", "village")]),
                             function(l) grepl(l, Data[,COL])), MARGIN = 1, any)]
            if(length(rows)>0){
              if(VERBOSE) print("Cleaning up named cities that are not a full state match")
              localities <- data.frame(PATTERN = as.vector(sapply(
                unlist(ls.JurisKeys[c("city", "town", 
                                      "township", "village")]),
                function(l) paste0("(\\b[[:alpha:]]+\\b)(( \\b[[:alpha:]]+\\b)?)( \\b",l,"[.]?\\b)"))),
                 REGEX = TRUE, stringsAsFactors = FALSE)
              localities$LOC <- localities$PATTERN
              Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                               localities,
                                               COL, 
                                               COL, 
                                               perl = TRUE,
                                               useBytes = TRUE,
                                               n.dup.cols = 1, 
                                               DELETE = TRUE)
            }
            
            # there will still be some left behind because they are at the end of line
            # but don't say "twp" or the like. Delete those as well.
            # Data[,COL] <- gsub("(\\A[[:punct:]]+)|([[:punct:]]+\\Z)","",str_squish(gsub("(\\A[[:punct:]]+)|([[:punct:]]+\\Z)","",
            #                                                                             str_squish(Data[,COL]), perl = T)))
            # 
            # rows <- Rows[!is.na(Data[,COL]) & Data[,COL]!="" & Data$STFIPS==state & 
            #                !is.na(Data[,"ANSICODE_1"])]
            # Data[rows,COL] <- gsub("(\\A[[:punct:]]+)|([[:punct:]]+\\Z)","",str_squish(gsub("(\\A[[:punct:]]+)|([[:punct:]]+\\Z)","",
            #                                                                                 str_squish(Data[rows,COL]), perl = T)))
            # 
            # if(length(rows)>0){
            #   print("trying to delete not full matches to city")
            #   print(Data[rows,COL])
            #   for(i in rows){
            #     print(regmatches(Data[i,COL],regexpr("\\w+\\Z",Data[i,COL],
            #                                          perl = TRUE)))
            #   }
            #   Data[rows,COL] <- sapply(rows, function(i)
            #     ifelse(any(grepl(regmatches(Data[i,COL],regexpr("\\w+\\Z",Data[i,COL],
            #                                                     perl = TRUE)),
            #                      Data[i,grepl("CITY_NAME",colnames(Data))]),na.rm = T),
            #            sub("\\w+\\Z","",Data[i,COL], perl = TRUE),
            #            Data[i,COL]))
            #   Data[rows,COL] <- gsub("(\\A[[:punct:]]+)|([[:punct:]]+\\Z)","",str_squish(gsub("(\\A[[:punct:]]+)|([[:punct:]]+\\Z)","",
            #                                                                                   str_squish(Data[rows,COL]), perl = T)))
            #   
            # }
            # 
            # rows <- Rows[!is.na(Data[,COL]) & Data[,COL]!="" & Data$STFIPS==state & 
            #                !is.na(Data[,"ANSICODE_1"])]
            # if(length(rows)>0){
            #   print("going for not full matches again")
            #   print(Data[rows,COL])
            #   Data[rows,COL] <- sapply(rows, function(i)
            #     ifelse(any(grepl(regmatches(Data[i,COL],regexpr("\\w+\\Z",Data[i,COL],
            #                                                     perl = TRUE)),
            #                      Data[i,grepl("CITY_NAME",colnames(Data))]),na.rm = T),
            #            sub("\\w+\\Z","",Data[i,COL], perl = TRUE),
            #            Data[i,COL]))
            # }
          }
          
          # may be excess punctuation and spacing, clean up
          Data[,COL] <- gsub("(\\A[[:punct:]]+)|([[:punct:]]+\\Z)","",str_squish(Data[,COL]), perl = T)
        }
      }
    }
      
    # now detecting feature name and type -----
  }
  # rm(Feature.Detect, envir = .GlobalEnv)
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
  