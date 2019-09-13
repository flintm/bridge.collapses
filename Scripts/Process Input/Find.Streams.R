# Code chunk (not a true subfunction) segmenting stream trib/name/type detection
Find.Streams <- function(Data,Feature,col.in,cols.out,n.dup.cols, DELETION = TRUE, VERBOSE = FALSE){
  Rows <- rownames(Data)[!is.na(Data[,col.in]) & Data[,col.in]!=""]
  # Tributary checks -------
  Data[Rows,col.in] <- gsub("[[:punct:]]","",Data[Rows,col.in])
  rows <- rows[apply(sapply(unlist(ls.TribKeys),
                            function(s) grepl(s,Data[rows,col.in])),
                     MARGIN = 1, any)]
  if(length(rows)>0){
    if(VERBOSE) print("Checking for tributaries, forks, branches")
    tribs <- data.frame(PATTERN = ls.TribKeys,  REGEX = TRUE, stringsAsFactors = FALSE)
    tribs$STREAM_TRIB <- tribs$PATTERN
    tribs <- tribs[order(nchar(tribs$PATTERN),decreasing = TRUE),]
    Data[rows,c(col.in,"STREAM_TRIB_1","STREAM_TRIB_2")] <- Feature.Detect(Data[rows,], 
                                                                        tribs, 
                                                                        
                                                                        col.in, 
                                                                        c("STREAM_TRIB_1","STREAM_TRIB_2"), 
                                                                        n.dup.cols = 2, 
                                                                        DELETE = TRUE)
  }
  
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
  
  return(Data[,c(col.in,cols.out)])
}