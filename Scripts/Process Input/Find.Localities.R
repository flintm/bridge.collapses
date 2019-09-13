# Code chunk (not a true subfunction) segmenting city/county detection-related 
# Feature Detection
# Assumes access to Data, df.Counties, df.Cities, df.GNIS
Find.Localities <- function(Data,Feature,col.in,cols.out,n.dup.cols, DELETION = FALSE, VERBOSE = FALSE){
  Rows <- rownames(Data)

  jurisNames <- switch(Feature,
                       "COUNTY" = c("county", "parish"),
                       "CITY"   = c("city", "town", 
                                      "township", "village"))
  
  # Loop over states to set and detect state-specific localities
  for(state in unique(Data$STFIPS)){
    rowsSt <- Rows[Data$STFIPS==state & !is.na(Data[,col.in]) & grepl("[[:alpha:]]", Data[,col.in])]
    if(VERBOSE){ 
      out <- ifelse(DELETION, 
                    paste("Deleting",tolower(Feature),"in state:",
                          df.States[state,"STATE_FULL"]),
                    paste("Checking for",tolower(Feature),"in state:",
                          df.States[state,"STATE_FULL"]))
      print(out)
      }
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
      
      # delete those already present by GNIS
      localities3 <- localities3[!(localities3$GNIS_ID %in% 
                                     c(localities$GNIS_ID,localities$ANSICODE)),]
      
      localities3$ANSICODE <- NA_character_
      localities3$PATTERN  <- localities3$NAME

      localities <- rbind(localities, localities3)
      rm(localities2, localities3)
    }
    
    localities <- localities[order(nchar(localities$NAME), decreasing = TRUE),]
    
    # If in detection mode only, check for presence of locality name and record name 
    # and FIPS or other standard codes -------
    if(!DELETION){
      rows <- rowsSt
      localities$PATTERN <- paste0("\\<",localities$NAME,"\\>")
      localities$REGEX    <- FALSE
      print("DETECTION RUN:")
      Data[rows,c(col.in,cols.out)] <- Feature.Detect(Data[rows,], 
                                                   localities, 
                                                   col.in, 
                                                   cols.out, 
                                                   n.dup.cols = n.dup.cols, 
                                                   DELETE = FALSE)
    }
    
    # If in deletion mode, delete entire name (i.e., "Mercer County", not just "Mercer")-----
    if(DELETION){
      print("DELETION RUN:")
      Data[,col.in] <- str_clout(Data[,col.in])
      rowsSt <- rowsSt[!is.na(Data[rowsSt,colnames(Data)[grepl(paste(Feature,"NAME",sep="_"),colnames(Data), fixed = T)][1]])]

      localities <- localities[!duplicated(localities$NAME),]
      localities$REGEX   <- TRUE
      jurisKeys <- unlist(ls.JurisKeys[jurisNames])
      
      # (1) city/county/parrish of _______ ----
      rows <- rowsSt[grepl("\\<of\\>",Data[rowsSt,col.in])]
      if(length(rows)>0){
        if(VERBOSE) print(paste0("(1) Deleting '",tolower(Feature)," of ___' names (strict)"))
        patterns <- sapply(jurisKeys, function(l)
          paste0("(\\b",l,"[.]? of ",localities$NAME,"\\b)"))
        localities$PATTERN <- apply(patterns, MARGIN = 1, paste0,collapse = "|")
        localities[,col.in]     <- localities$PATTERN

        Data[rows,col.in] <- Feature.Detect(Data[rows,], 
                                         localities, 
                                         col.in, 
                                         col.in, 
                                         n.dup.cols = 1, 
                                         perl = TRUE,
                                         useBytes = TRUE,
                                         DELETE = TRUE)
        Data[rows,col.in] <- str_clout(Data[rows,col.in])
      }

      # (2) ___ county/city: be careful of "___ county road"-----
      rows <-  rowsSt[grepl(paste0("(\\b",paste0(jurisKeys, collapse = "\\b)|(\\b"),"\\b)"),
                            Data[rowsSt,col.in],perl = TRUE)]
      if(length(rows) > 0){
        if(VERBOSE) print(paste0("(2) Deleting '_____ ",tolower(Feature),"' names (strict)"))
        notMatch   <- switch(Feature,
                             "COUNTY" = "(?!(( road)|( rd[.]?)|( ro[.]?$)|( route)|( rt[e.]?)|( highway)))",
                             "CITY" = "(?!(( road)|( rd[.]?)|( ro[.]?$)|( route)|( rt[e.]?)|( highway)))",
                             "STREAM"   = "(?!(( stream)|( str[.]?)|( f[or]?k[.]?$)|( route)|( creek)))",
                             "BRIDGE"   = "(?!(( bridge)|( br[.]?)))")
        
        patterns <- sapply(jurisKeys, function(l)
          paste0("(\\b",localities$NAME," ", l,"[.]?\\b)"))
        localities$PATTERN  <- apply(patterns, MARGIN = 1, paste0,collapse = "|")
        localities$PATTERN  <-  paste0("(", localities$PATTERN,")", notMatch)
        localities[,col.in] <- localities$PATTERN

        Data[rows,col.in] <- Feature.Detect(Data[rows,], 
                                            localities, 
                                            col.in, 
                                            col.in, 
                                            perl = TRUE,
                                            useBytes = TRUE,
                                            n.dup.cols = 1, 
                                            DELETE = TRUE)
        Data[rows,col.in] <- str_clout(Data[rows,col.in])
      }
      
      # (3) county/city ___ -----
      rows  <- rowsSt[grepl(paste0("(\\b",paste0(jurisKeys, collapse = "\\b)|(\\b"),"\\b)"),
                            Data[rowsSt,col.in],perl = TRUE)]
      
      if(length(rows)>0){
        if(VERBOSE) print(paste0("(3) Deleting '",tolower(Feature)," ___' names (strict)"))
        patterns <- sapply(jurisKeys, function(l)
          paste0("(\\b",l,"[.]? ",localities$NAME,"\\b)"))
        localities$PATTERN <- apply(patterns, MARGIN = 1, paste0,collapse = "|")
        localities[,col.in]     <- localities$PATTERN
        
        Data[rows,col.in] <- Feature.Detect(Data[rows,], 
                                         localities, 
                                         col.in, 
                                         col.in, 
                                         perl = TRUE,
                                         useBytes = TRUE,
                                         n.dup.cols = 1, 
                                         DELETE = TRUE)
        Data[rows,col.in] <- str_clout(Data[rows,col.in])
      }
      
      # (4) ___, or ___[EOL] -----
      rows <- rowsSt[grepl(paste0("(\\b",paste0(localities$NAME, 
                                                collapse = "\\b)|(\\b"),"\\b)"),Data[rowsSt,col.in])]
      
      if(length(rows) > 0){
        if(VERBOSE) print(paste0("(4) Deleting end of clause/line ",tolower(Feature)," names (strict)"))
        patterns <- paste0("\\b",localities$NAME,"\\b")
        localities$PATTERN <-  paste0("(", patterns, ",)|(",patterns,"\\Z)")
        localities[,col.in]     <- localities$PATTERN

        Data[rows,col.in] <- Feature.Detect(Data[rows,], 
                                         localities, 
                                         col.in, 
                                         col.in, 
                                         perl = TRUE,
                                         useBytes = TRUE,
                                         n.dup.cols = 1, 
                                         DELETE = TRUE)
        Data[rows,col.in] <- str_clout(Data[rows,col.in])
      }
      
      # (5) Explicitly named as county/city, but doesn't match fully -----
      # Those without a full match but explicit name, e.g. "liberty twp", which doesn't
      # match the state-specific options of "north liberty", "new liberty", etc.
      rows  <- rowsSt[grepl(paste0("(\\b",paste0(jurisKeys, collapse = "\\b)|(\\b"),"\\b)"),
                            Data[rowsSt,col.in],perl = TRUE)]
      if(length(rows) > 0){
        if(VERBOSE) print(paste0("(5) Deleting explicitly named ",tolower(Feature)," (non-strict)"))
        notMatch   <- switch(Feature,
                             "COUNTY" = "(?!(( road)|( rd[.]?)|( ro[.]?$)|( route)|( rt[e.]?)|( highway)))",
                             "CITY" = "(?!(( road)|( rd[.]?)|( ro[.]?$)|( route)|( rt[e.]?)|( highway)))",
                             "STREAM"   = "(?!(( stream)|( str[.]?)|( f[or]?k[.]?$)|( route)|( creek)))",
                             "BRIDGE"   = "(?!(( bridge)|( br[.]?)))")
        
        localities <- data.frame(PATTERN = as.vector(sapply(
          jurisKeys,
          function(l) paste0("(\\b[[:alpha:]]+\\b)(( \\b[[:alpha:]]+\\b)?)( \\b",l,"[.]?\\b)",notMatch))),
          REGEX = TRUE, stringsAsFactors = FALSE)
        localities[,col.in] <- localities$PATTERN

        Data[rows,col.in] <- Feature.Detect(Data[rows,], 
                                         localities, 
                                         col.in, 
                                         col.in, 
                                         perl = TRUE,
                                         useBytes = TRUE,
                                         n.dup.cols = 1, 
                                         DELETE = TRUE)
        Data[rows,col.in] <- str_clout(Data[rows,col.in])
      }
      Data[!grepl("[[:alnum:]]",Data[,col.in]),col.in] <- NA_character_
    }
  }
  
  return(Data[,unique(c(col.in,cols.out))])
}