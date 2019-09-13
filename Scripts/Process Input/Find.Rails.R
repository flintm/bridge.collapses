# Code chunk (not a true subfunction) segmenting railway name detection
Find.Rails <- function(Data,Feature,col.in,cols.out,n.dup.cols, DELETION = FALSE, VERBOSE = FALSE){
  Rows <- rownames(Data)[!is.na(Data[,col.in]) & grepl("[[:alpha:]]",Data[col.in])]
  
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
  return(Data[,c(col.in,cols.out)])
}