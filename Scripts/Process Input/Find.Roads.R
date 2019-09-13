# Code chunk (not a true subfunction) segmenting road name/type detection
Find.Roads <- function(Data,Feature,col.in,cols.out,n.dup.cols, DELETION = TRUE, VERBOSE = FALSE){
  Rows <- rownames(Data)[!is.na(Data[,col.in]) & Data[,col.in]!=""]
  
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
  
  return(Data[,c(col.in,cols.out)])
}