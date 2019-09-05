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
                                            apply(sapply(paste0("\\<",unlist(ls.StreamKeys),"\\>"), 
                                                                       grepl, Data[,STREAM], ignore.case = TRUE),
                                                  MARGIN = 1, any)) & 
                                           !is.na(Data[,STREAM]),]),
                 "NBI"  = row.names(Data[!is.na(Data[,grepl("38",FieldNames)]) | !is.na(Data[,grepl("FEAT",FieldNames)]),]))
  
  Data$STREAM_UNDER    <- str_squish(gsub("[\\&\\*]+"," ", gsub("'","",Data[,STREAM])))
  Data$STREAM_NO       <- NA_character_
  Data$ROUTE_UNDER     <- NA_character_

  # Dataset-specific corrections
  ls.Keys   <- get(paste0("ls.",sub("Data","",DATA_SET),".Keys"))
  if(length(ls.Keys) > 0){
    if(VERBOSE) print("Implementing dataset-specific corrections for streams.")
    ls.Stream <- ls.Keys[sapply(1:length(ls.Keys), function(i) "STREAM" %in% ls.Keys[[i]])]
    if(length(ls.Stream)>0){
      if(any(names(ls.Stream) %in% Rows)){
        rowsID <- Rows[sapply(names(ls.Stream), function(i) which(Data[Rows,FieldNames["ID"]]==i))]
        Data[rowsID,"STREAM_UNDER"] <- sapply(1:length(ls.Stream), 
                                              function(i) sub(ls.Stream[[i]][1],
                                                              ls.Stream[[i]][2],
                                                              Data[rowsID[i],"STREAM_UNDER"],
                                                              fixed = TRUE))
      }
    }
  }
  
  # for places
  if(VERBOSE) print("Checking for place mispellings and abbreviations from dictionary.")
  for (j in c(1:length(ls.PlaceKeys))){
    key.index <- sapply(paste0("\\<",
                               ls.PlaceKeys[[j]][c(2:length(ls.PlaceKeys[[j]]))],
                               "\\>[[:punct:]]?"),
                        grepl,
                        Data[Rows,"STREAM_UNDER"])
    if(sum(key.index)==0){ 
      key.index <- sapply(ls.PlaceKeys[[j]][c(2:length(ls.PlaceKeys[[j]]))],
                          function(cn) grepl(cn, Data[Rows,"STREAM_UNDER"], fixed = TRUE) & 
                            !grepl(ls.PlaceKeys[[j]][1], Data[Rows,"STREAM_UNDER"], fixed = TRUE))
      useFix <- TRUE
    }
    else useFix <- FALSE
    dim(key.index) <- c(length(Rows), length(ls.PlaceKeys[[j]])-1)
    match.keys <- switch(as.character(length(dim(key.index))),
                         "2" = which(apply(key.index, MARGIN = 1, any)),
                         "1" = which(key.index))
    for (i in match.keys){
      pattern <- ifelse(useFix,
                        ls.PlaceKeys[[j]][which(key.index[i,])[1]+1],
                        paste0("\\<",
                               ls.PlaceKeys[[j]][which(key.index[i,])[1]+1],
                               "\\>"))
      str     <- regmatches(Data[Rows[i],"STREAM_UNDER"], 
                            regexpr(pattern, 
                                    Data[Rows[i],"STREAM_UNDER"],fixed = useFix))
      if(length(str)==0) next # no match
      str.out <- ls.PlaceKeys[[j]][1]
      Data[Rows[i],"STREAM_UNDER"] <- ifelse(useFix,
                               sub(str,
                                   str.out,
                                   Data[Rows[i],"STREAM_UNDER"],
                                   fixed = TRUE),
                               sub(paste0("(",str,"[[:punct:]]?)"),
                                   str.out,
                                   Data[Rows[i],"STREAM_UNDER"]))
    }
  }
  
  # Check for numbers in string to see if related to a stream
  rows       <- Rows[grepl("[[:digit:]]",Data[Rows,"STREAM_UNDER"])]
  match.keys <- Rows[grepl("\\<[[:digit:]]{1,3}[ \\-]?mi[\\.]?[l]?[e]?\\>[[:space:]]{1}", 
                           Data[Rows,"STREAM_UNDER"])]
  
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
    rows  <- rows[!(rows %in% match.keys)]
    if(length(rows)>0){
      key.index  <- sapply(paste0("\\<[nloumberck \\.\\#\\(]{0,7}[[:digit:]]{1,}"),grepl,Data[rows,"STREAM_UNDER"],ignore.case = TRUE)
      match.keys <- switch(as.character(length(dim(key.index))),
                           "2" = which(apply(key.index, MARGIN = 1, any)),
                           "1" = which(key.index))
      for(i in match.keys){ # route under
        digits <- gsub("[[:alpha:]]?[[:punct:]]?[[:space:]]?", "",Data[rows[i],"STREAM_UNDER"],ignore.case = TRUE)
        Data[rows[i],"STREAM_NO"]    <- digits
        Data[rows[i],"STREAM_UNDER"] <- sub(paste0("[[:space:]]{1}[no\\. ]{0,3}[number ]{0,6}[\\#]?[\\(]?",digits,"[\\)]?"),"",Data[rows[i],"STREAM_UNDER"],ignore.case = TRUE)
      }
    }
    
    # digits are not part of a stream name: check for route under
    rows  <- rows[!(rows %in% match.keys)]
    if(length(rows)>0){
      key.index  <- sapply(paste0("\\<",unique(unlist(ls.RteKeys)),"[ \\.\\-]?[[:digit:]]{1,}"),grepl,Data[rows,"STREAM_UNDER"],ignore.case = TRUE)
      match.keys <- switch(as.character(length(dim(key.index))),
                           "2" = which(apply(key.index, MARGIN = 1, any)),
                           "1" = which(key.index))
      for(i in match.keys){ # route under
        rte    <- unlist(ls.RteKeys)[which(key.index[i,])[1]]
        phrase <- sub(paste0(".*?",rte),rte,Data[rows[i],"STREAM_UNDER"])
        digits <- sub(".*?([0-9]+).*?$", "\\1",phrase)
        Data[rows[i],"ROUTE_UNDER"]  <- paste(rte,digits)
        Data[rows[i],"STREAM_UNDER"] <- sub(paste0("\\<",rte,"[ \\.\\-]?",digits),"",Data[rows[i],"STREAM_UNDER"],ignore.case = TRUE)
      }
    }
  }
  
  # cleanup
  Data$STREAM_UNDER <- gsub("(\\A[.,-]+)|([.,-]+\\Z)","",Data$STREAM_UNDER, perl = TRUE)
  Data$STREAM_UNDER <- str_squish(gsub("[[:blank:]]{1}[\\,]",",",Data$STREAM_UNDER))
  return(Data)
}
