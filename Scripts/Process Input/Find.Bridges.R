# Code chunk (not a true subfunction) segmenting bridge name detection
Find.Bridges <- function(Data,Feature,col.in,cols.out,n.dup.cols, DELETION = FALSE, VERBOSE = FALSE){
  Rows <- rownames(Data)[!is.na(Data[,col.in]) & grepl("[[:alnum:]]",Data[col.in])]
  
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
  return(Data[,c(col.in,cols.out)])
}