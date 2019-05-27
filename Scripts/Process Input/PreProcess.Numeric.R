PreProcess.Numeric <- function(Data,             
                               FieldNames,         # FieldNames is a character of form c(BIN = "BIN_COL_NAME", ROUTE = "ROUTE_COL_NAME")
                               VERBOSE = FALSE){ #
  
  Rows <- row.names(Data)
  cols <- c("BIN","ROUTE")[c("BIN","ROUTE") %in% names(FieldNames)]
  split.pattern <- c(BIN = "([a-zA-Z\\-\\.])+", ROUTE = "([a-zA-Z])+")
  for(col in cols){
    COL <- FieldNames[col]
    Data$COL_NUM <- str_squish(sub("^[0]{0,16}$|^ $",NA_character_,Data[,COL]))
    rows <- Rows[!is.na(Data$COL_NUM)]
    if(VERBOSE) print(paste("Finished classifying informationless entries as NA for",col))

    # leading, internal padded, and trailing zeros
    Data[rows,"COL_NUM"] <- gsub("^[0]+","",Data[rows,"COL_NUM"])
    if(VERBOSE) print(paste("Finished removing leading 0s for",col))
    Data[rows,"COL_NUM"] <- gsub("[a-zA-Z\\.\\-]+[0]+$","",Data[rows,"COL_NUM"])
    if(VERBOSE) print(paste("Finished removing trailing 0s for",col))
    Data[rows,"COL_NUM"] <- gsub("[a-zA-Z\\.\\-]+[0]+"," ",Data[rows,"COL_NUM"])
    if(VERBOSE) print(paste("Finished removing internal padded 0s for",col))
    
    # split strings and remove non-numeric characters
    splits <- rows[grepl(paste0("^[[:digit:]]+",split.pattern[col],"[[:digit:]]+$"), Data[rows,"COL_NUM"])]
    # Data[splits,"COL_NUM"] <- sapply(splits, function(i) str_squish(paste(unlist(str_split(Data[i,"COL_NUM"],split.pattern[col])),collapse=" ")))
    Data[splits,"COL_NUM"] <- str_squish(gsub(split.pattern[col]," ",Data[splits,"COL_NUM"]))
    if(VERBOSE) print(paste("Finished split numeric strings for",col))
    
    # remove leading or trailing letters from BIN
    if(col == "BIN"){
      Data[rows,"COL_NUM"] <- gsub("^[[:alpha:]]+|[[:alpha:]]$","",Data[rows,"COL_NUM"])
      if(VERBOSE) print(paste("Finished removing leading and trailing letters for",col))
    }
    
    FieldNames(Data)[FieldNames(Data)=="COL_NUM"] <- paste0(col,"_NUM")
    if(VERBOSE) print(paste("Finished making numeric version of field",col))
  }
  return(Data)
}