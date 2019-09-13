# Code chunk (not a true subfunction) segmenting route detection
Find.Routes <- function(Data,Feature,col.in,cols.out,n.dup.cols, DELETION = TRUE, VERBOSE = FALSE){
  Rows <- rownames(Data)
  
  for(state in unique(Data$STFIPS)){
    rowsSt <- Rows[Data$STFIPS==state & !is.na(Data[,col.in]) & grepl("[[:alnum:]]", Data[,col.in])]
    
    # set up route names and aux information -------
    ls.RteKeys$state <- c(tolower(df.States[state,"STATE_FULL"]), tolower(df.States[state,"STATE_CODE"]))
    ls.RteKeys$state <- c(paste(ls.RteKeys$state,"highway"), ls.RteKeys$state)
    if(substr(ls.RteKeys$state[1],1,1) %in% c("a", "c", "d", "g", "k", "l", "m", "n", "o", "t", "u", "w") &
       !grepl("(north)|(south)|(west)", ls.RteKeys[2])) ls.RteKeys$state[3] <- substr(ls.RteKeys$state[1],1,1)
    rteKeys  <- unlist(ls.RteKeys)
    # print(rteKeys)
    cardKeys <- unlist(ls.CardKeys[grepl("b",names(ls.CardKeys))])
    specKeys <- unlist(ls.SpecKeys)
    
    # Begin checks if rows are present
    rtePat   <- paste0("(\\b",rteKeys, "\\b)", collapse = "|")
    rows <- rowsSt[grepl(rtePat, Data[rowsSt,col.in], perl = TRUE)]
    if(length(rows) > 0){
      if(VERBOSE) print(paste("Checking for",tolower(Feature),"in state:",df.States[state,"STATE_FULL"]))
      
      # full: route type (prefix), digit, and direction
      rows   <- rows[grepl("[[:digit:]][ .,-]?[nsew]{1}",Data[rows,col.in])]
      if(length(rows) > 0){
        if(VERBOSE) print("Looking for route number with direction (nb/sb/eb/wb)")
        routes <- as.vector(sapply(cardKeys,
                                   function(card)
                                     paste0(rteKeys,
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
        Data[rows,c(col.in,cols.out)] <- Feature.Detect(Data[rows,], 
                                                     routes,
                                                     col.in, 
                                                     cols.out, 
                                                     n.dup.cols = n.dup.cols, 
                                                     DELETE = TRUE)
      }
      
      # with modified/aux: route type (prefix), digit, and business/bypass/etc. suffix
      # Eg US Route 1 Alternate, US Route 1A, US Route 1A Business
      # AT = Alternate Truck, TB = Truck Business, Business, Spur, Connector, Scenic
      # Inner/Outler Loop, Bypass. Alt., Bus. Truck, Byp, Temp., Conn., Spur,  Old
      rtePat <- paste0("([[:digit:]]+ \\b",
                       specKeys[nchar(specKeys)>1],"\\b)",collapse="|")
      rtePat <- paste0("([[:digit:]]+[abtcsio]{1}\\b)|",rtePat,collapse = "")
      rows   <- rowsSt[grepl(rtePat,Data[rowsSt,col.in], perl = TRUE)]
      if(length(rows) > 0){
        if(VERBOSE) print("Looking for special route number (bypass, business,...)")
        routes <- as.vector(sapply(specKeys,
                                   function(alt)
                                     paste0(rteKeys,
                                            "[[:space:]]?[[:punct:]]?[[:space:]]?[[:digit:]]+[[:space:]]?",
                                            alt,"\\b")))
        routes     <- data.frame(PATTERN = routes,  REGEX = TRUE, stringsAsFactors = FALSE)
        routes$ROUTE_NAME <- "[[:digit:]]+"
        routes$ROUTE_TYPE <- sub("([\\[])(.+)","",routes$PATTERN)
        routes$ROUTE_AUX  <- paste0("(\\b",paste(specKeys,collapse = "\\b)|(\\b"),"\\b)")
        routes     <- routes[order(nchar(routes$PATTERN)),]
        Data[rows,c(col.in,cols.out)] <- Feature.Detect(Data[rows,], 
                                                     routes, 
                                                     col.in, 
                                                     cols.out, 
                                                     n.dup.cols = n.dup.cols,
                                                     perl = TRUE,
                                                     DELETE = TRUE)
      }
      
      # Modified/aux preceding route prefix and number
      rtePat   <- paste0("(\\b",rteKeys, "\\b)", collapse = "|")
      
      rows <- rowsSt[(grepl("[[:digit:]]",Data[rowsSt,col.in]) | grepl(rtePat,Data[rowsSt,col.in],perl = TRUE)) &
                     grepl("[abtcs]{1}[[:print:]]{1,12}[[:digit:]]+[[:alpha:]]?\\>",
                           Data[rowsSt,col.in])]
      if(length(rows)>0){
        if(VERBOSE) print("Looking for modified/auxilliary route number without direction!!!!!!!!")
      }
      
      # route type and digit only
      rows <- rowsSt[(grepl("[[:digit:]]",Data[rowsSt,col.in]) | grepl(rtePat,Data[rowsSt,col.in],perl = TRUE))]
      if(length(rows) > 0){
        
        if(VERBOSE) print("Looking for route number without direction")
        print(Data[rows,col.in])
        routes     <- data.frame(PATTERN = rteKeys,  
                                 REGEX = TRUE, stringsAsFactors = FALSE)
        routes$PATTERN <- paste0(routes$PATTERN,"[ .-]{0,2}[[:digit:]]+\\>")
        routes$ROUTE_NAME <- "[[:digit:]]+"
        routes$ROUTE_TYPE <- rteKeys
        routes     <- routes[order(nchar(routes$PATTERN)),]
        Data[rows,c(col.in,cols.out)] <- Feature.Detect(Data[rows,], 
                                                     routes, 
                                                     col.in, 
                                                     cols.out, 
                                                     n.dup.cols = n.dup.cols, 
                                                     DELETE = TRUE)
      }
      
      # non-digit route numbers
    }
  }
  return(Data[,c(col.in,cols.out)])
}