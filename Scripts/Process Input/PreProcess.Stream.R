# Sub-function to process NYSDOT Bridge Failure Database FEAT_UND and NBI ITEM6A fields
# Particularly looks to expand branch and tributary abbreviations to aid feature identification

PreProcess.Stream <- function(Data,             
                              DATA_SET,
                              FieldNames,         # FieldNames is a character of form c(STREAM = "STREAM_COL_NAME")
                              VERBOSE = FALSE){ #
  
  if(!("STREAM") %in% names(FieldNames)) stop("Stream column name not properly specified.")
  STREAM  <- FieldNames["STREAM"]

  Rows <- switch(DATA_SET,
                 "Fail" = row.names(Data[(grepl("hyd",Data$FAIL_CAUS,ignore.case = TRUE) | 
                                            apply(sapply(paste0("\\<",unlist(ls.StreamKeys),"\\>"), grepl, Data[,STREAM], ignore.case = TRUE),
                                                  MARGIN = 1, any)) & 
                                           !is.na(Data[,STREAM]),]),
                 "NBI"  = row.names(Data[!is.na(Data[,grepl("38",FieldNames(Data))]),]))
  
  punct <- c("'","&","*")
  Data$STREAM_UNDER    <- str_squish(gsub("[\\&\\*]+"," ", gsub("'","",Data[,STREAM])))
  Data$STREAM_NO       <- NA_character_
  Data$ROUTE_UNDER     <- NA_character_

  # Dataset-specific corrections
  DATA_TYPE <- names(DATA_SETS)[sapply(DATA_SETS,"[[",1)==DATA_SET]
  ls.Keys   <- get(paste0("ls.",sub("Data","",DATA_TYPE),"Keys"))
  
  ls.Stream <- ls.Keys[sapply(1:length(ls.Keys), function(i) "STREAM" %in% ls.Keys[[i]])]
  if(length(ls.Stream)>0){
    rowsID <- Rows[sapply(names(ls.Stream), function(i) which(Data[Rows,FieldNames["ID"]]==i))]
    Data[rowsID,"STREAM_UNDER"] <- sapply(1:length(ls.Stream), 
                                          function(i) sub(ls.Stream[[i]][1],ls.Stream[[i]][2],Data[rowsID[i],"STREAM_UNDER"]))
  }
  
  # Check for numbers in string to see if related to a stream
  rows       <- Rows[grepl("[[:digit:]]",Data[Rows,"STREAM_UNDER"])]
  match.keys <- Rows[grepl("\\<[[:digit:]]{1,3}[ \\-]?mi[\\.]?[l]?[e]?\\>[[:space:]]{1}", Data[Rows,"STREAM_UNDER"])]
  # digits are part of a stream name, e.g., "16 mi. ck"
  for(i in match.keys){
    phrase <- sub("\\<mi[\\.]?\\>.*?$","mile",Data[i,"STREAM_UNDER"])
    phrase <- sub("mile.*?$","",phrase)
    if(grepl("[\\.]",phrase)){ # not a stream name
      match.keys <- match.keys[match.keys!=i]
      next
    }
    digits <- as.numeric(sub(".*?([0-9]+).*?$", "\\1",phrase))
    word   <- as.english(digits)
    Data[i,"STREAM_UNDER"] <- sub("[\\-]?mi[\\.]?[l]?[e]?\\>"," mile",Data[i,"STREAM_UNDER"])
    Data[i,"STREAM_UNDER"] <- str_squish(sub(as.character(digits),word,Data[i,"STREAM_UNDER"]))
  }
  if(VERBOSE) print("Finished checking for numbered stream names.")
  # digits are not part of a stream name: if we're using a smaller database we can check for other info
  if(DATA_SET=="Fail"){
    # : check for stream number
    rows       <- rows[!(rows %in% match.keys)]
    key.index  <- sapply(paste0("\\<[nloumberck \\.\\#\\(]{0,7}[[:digit:]]{1,}"),grepl,Data[rows,"STREAM_UNDER"],ignore.case = TRUE)
    match.keys <- which(apply(key.index, MARGIN = 1, any))
    for(i in match.keys){ # route under
      digits <- gsub("[[:alpha:]]?[[:punct:]]?[[:space:]]?", "",Data[rows[i],"STREAM_UNDER"],ignore.case = TRUE)
      Data[rows[i],"STREAM_NO"]    <- digits
      Data[rows[i],"STREAM_UNDER"] <- sub(paste0("[[:space:]]{1}[no\\. ]{0,3}[number ]{0,6}[\\#]?[\\(]?",digits,"[\\)]?"),"",Data[rows[i],"STREAM_UNDER"],ignore.case = TRUE)
    }
    
    # digits are not part of a stream name: check for route under
    rows       <- rows[-match.keys]
    key.index  <- sapply(paste0("\\<",unique(unlist(ls.RteKeys)),"[ \\.\\-]?[[:digit:]]{1,}"),grepl,Data[rows,"STREAM_UNDER"],ignore.case = TRUE)
    match.keys <- which(apply(key.index, MARGIN = 1, any))
    for(i in match.keys){ # route under
      rte    <- unlist(ls.RteKeys)[which(key.index[i,])[1]]
      phrase <- sub(paste0(".*?",rte),rte,Data[rows[i],"STREAM_UNDER"])
      digits <- sub(".*?([0-9]+).*?$", "\\1",phrase)
      Data[rows[i],"ROUTE_UNDER"]  <- paste(rte,digits)
      Data[rows[i],"STREAM_UNDER"] <- sub(paste0("\\<",rte,"[ \\.\\-]?",digits),"",Data[rows[i],"STREAM_UNDER"],ignore.case = TRUE)
    }
  }
  return(Data)
}
