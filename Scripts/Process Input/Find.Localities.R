# Code chunk (not a true subfunction) segmenting city/county detection-related 
# Feature Detection
# Assumes access to Data, df.Counties, df.Cities, df.GNIS
Find.Localities <- function(Data,Feature,COL,cols.out,n.dup.cols, DELETION = FALSE, VERBOSE = FALSE){
  Rows <- colnames(Data)
  print(c(COL,cols.out))
  jurisNames <- switch(Feature,
                       "COUNTY" = c("county", "parish"),
                       "CITY"   = c("city", "town", 
                                      "township", "village"),
                       "LOCATION" = c("city","town","township","village","county", "parish"),
                       "BRIDGE"   = c("city","town","township","village","county", "parish"),
                       "STREAM"   = c("city","town","township","village","county", "parish"))
  str_clout <- function(str){gsub("(\\A[[:punct:]]+)(\\A[[:space:]]+)|([[:punct:]]+\\Z)([[:space:]]+\\Z)","",str_squish(str), perl = T)}
  print(head(Data[,c(COL,cols.out)]))
  # Loop over states to set and detect state-specific localities
  for(state in unique(Data$STFIPS)){
    rowsSt <- Rows[Data$STFIPS==state & !is.na(Data[,COL]) & grepl("[[:alpha:]]", Data[,COL])]
    if(VERBOSE) print(paste("Checking for",tolower(Feature),"in state:",
                            df.States[state,"STATE_FULL"]))
    localities <- switch(Feature,
                         COUNTY = df.Counties[df.Counties$STFIPS_C == state, 
                                              c("county", "FIPS_C")],
                         CITY   = df.Cities[df.Cities$STFIPS_C == state, 
                                            c("city", "FIPS_C", "GNIS_ID", "ANSICODE")])
    colnames(localities)[1] <- "NAME"
    colnames(localities)[colnames(localities)=="FIPS_C"] <- "FIPS"
    localities$PATTERN <- localities$NAME
    
    localities2 <- localities[grepl(
      "(\\<north\\>)|(\\<south\\>)|(\\<east\\>)|(\\<west\\>)|(\\<new\\>)|(\\<old\\>)",
                                    localities$NAME),]
    localities2$PATTERN <- str_squish(sub(
      "(\\<north\\>)|(\\<south\\>)|(\\<east\\>)|(\\<west\\>)|(\\<new\\>)|(\\<old\\>)","",
                                          localities2$PATTERN))
    localities <- rbind(localities, localities2)
    
    # look for other populated places
    if(Feature=="CITY"){ 
      localities[localities$ANSICODE=="-999","ANSICODE"] <- NA_character_
      localities[localities$GNIS_ID=="-999", "GNIS_ID"]  <- NA_character_
      localities3 <- df.GNIS[df.GNIS$STFIPS==state & 
                               df.GNIS$FEATURE_CLASS =="Civil", 
                             c("name","FIPS","GNIS_ID")]
      colnames(localities3)[1] <- "NAME"
      localities3$ANSICODE <- NA_character_
      localities3$PATTERN  <- localities3$NAME
      
      # delete those already present by GNIS
      localities3 <- localities3[!(localities3$GNIS_ID %in% 
                                     c(localities$GNIS_ID,localities$ANSICODE)) & 
                                   !grepl("county",localities3$NAME),]
      
      localities <- rbind(localities, localities3)
      rm(localities2, localities3)
    }
    
    localities <- localities[order(nchar(localities$NAME), decreasing = TRUE),]
    print(head(localities))
    # If in detection mode only, check for presence of locality name and record name 
    # and FIPS or other standard codes -------
    if(!DELETION){
      rows <- rowsSt
      localities$PATTERN <- paste0("\\<",localities$NAME,"\\>")
      localities$REGEX    <- FALSE

      Data[rows,c(COL,cols.out)] <- Feature.Detect(Data[rows,], 
                                                   localities, 
                                                   COL, 
                                                   cols.out, 
                                                   n.dup.cols = n.dup.cols, 
                                                   DELETE = FALSE)
    }
    
    # If in deletion mode, delete entire name (i.e., "Mercer County", not just "Mercer")-----
    if(DELETION){
      rowsSt <- rowsSt[!is.na(Data[rowsSt,cols.out[grepl("NAME",cols.out)][1]])]
      # (1) city/county/parrish of _______ ----
      rows <- rowsSt[grepl("\\<of\\>",Data[rowsSt,COL])]

      if(length(rows)>0){
        patterns <- sapply(unlist(ls.JurisKeys[jurisNames]), function(l)
          paste0("(\\b",l,"[.]? of ",localities$NAME,"\\b)"))
        localities$PATTERN <- apply(patterns[1:5,], MARGIN = 1, paste0,collapse = "|")
        localities$REGEX   <- TRUE
        localities$LOC     <- localities$PATTERN
        
        if(VERBOSE) print("removing 'county/city of ___' names (strict)")
        Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                         localities, 
                                         COL, 
                                         COL, 
                                         n.dup.cols = 1, 
                                         perl = TRUE,
                                         useBytes = TRUE,
                                         DELETE = TRUE)
        Data[rows,COL] <- str_clout(Data[rows,COL])
      }

      # (2) county/city __ -----
      rows  <- rowsSt[!is.na(Data[rowsSt,cols.out[grepl("NAME",cols.out)][1]])]
      if(length(rows)>0){
        patterns <- sapply(unlist(ls.JurisKeys[jurisNames]), function(l)
          paste0("(\\b",l,"[.]? ",localities$NAME,"\\b)"))
        localities$PATTERN <- apply(patterns, MARGIN = 1, paste0,collapse = "|")
        localities$LOC     <- localities$PATTERN
        
        Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                         localities, 
                                         COL, 
                                         COL, 
                                         perl = TRUE,
                                         useBytes = TRUE,
                                         n.dup.cols = 1, 
                                         DELETE = TRUE)
        Data[rows,COL] <- str_clout(Data[rows,COL])
      }
        
      # (3) ___ county/city: be careful of "___ county road"-----
      rows <- rows[grepl("[[:alpha:]]",Data[rows,COL])]
      if(length(rows) > 0){
        notMatch   <- switch(Feature,
                             "COUNTY" = " (?!((road)|(rd[.]?)|(ro[.]?$)|(route)|(rt[e.]?)|(highway))))",
                             "CITY" = " (?!((road)|(rd[.]?)|(ro[.]?$)|(route)|(rt[e.]?)|(highway))))",
                             "STREAM"   = " (?!((stream)|(str[.]?)|(f[or]?k[.]?$)|(route)|(creek))))",
                             "BRIDGE"   = "  (?!((bridge)|(br[.]?))))")
        
        patterns <- sapply(unlist(ls.JurisKeys[jurisNames]), function(l)
          paste0("(\\b",localities$NAME,"\\b", l,"[.]?\\b)"))
        localities$PATTERN <- apply(patterns, MARGIN = 1, paste0,collapse = "|")
        localities$PATTERN <-  paste0("(", localities$PATTERN, notMatch)
        localities$LOC     <- localities$PATTERN
        
        if(VERBOSE) print("removing '___ county/city' names (strict)")
        Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                         localities, 
                                         COL, 
                                         COL, 
                                         perl = TRUE,
                                         useBytes = TRUE,
                                         n.dup.cols = 1, 
                                         DELETE = TRUE)
        Data[rows,COL] <- str_clout(Data[rows,COL])
      }
      
      # (4) ___, or ___[EOL] -----
      rows <- rows[grepl("[[:alpha:]]",Data[rows,COL])]
      if(length(rows) > 0){
        patterns <- paste0("(\\b",localities$NAME,"\\b")
        localities$PATTERN <- apply(patterns, MARGIN = 1, paste0,collapse = "|")
        localities$PATTERN <-  paste0("(", localities$PATTERN, ",)|(",localities$PATTERN,"\\Z)")
        localities$LOC     <- localities$PATTERN
        
        if(VERBOSE) print("removing end of clause/line names (strict)")
        Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                         localities, 
                                         COL, 
                                         COL, 
                                         perl = TRUE,
                                         useBytes = TRUE,
                                         n.dup.cols = 1, 
                                         DELETE = TRUE)
        Data[rows,COL] <- str_clout(Data[rows,COL])
      }
      
      # (5) Explicitly named as county/city, but doesn't match fully -----
      # Those without a full match but explicit name, e.g. "liberty twp", which doesn't
      # match the state-specific options of "north liberty", "new liberty", etc.
      rows <- rowsSt[grepl(paste0(c("(",unlist(ls.JurisKeys[jurisNames]),")"), collapse = ")|("),
                           Data[rowsSt,COL])]
      if(length(rows) > 0){
        localities <- data.frame(PATTERN = as.vector(sapply(
          unlist(ls.JurisKeys[jurisNames]),
          function(l) paste0("(\\b[[:alpha:]]+\\b)(( \\b[[:alpha:]]+\\b)?)( \\b",l,"[.]?\\b)"))),
          REGEX = TRUE, stringsAsFactors = FALSE)
        localities$LOC <- localities$PATTERN
        
        if(VERBOSE) print("removing non-full matches with named locality")
        Data[rows,COL] <- Feature.Detect(Data[rows,], 
                                         localities, 
                                         COL, 
                                         COL, 
                                         perl = TRUE,
                                         useBytes = TRUE,
                                         n.dup.cols = 1, 
                                         DELETE = TRUE)
        Data[rows,COL] <- str_clout(Data[rows,COL])
      }
    }
  }
  return(Data)
}