# Code chunk (not a true subfunction) segmenting state-specific
# feature detection, mostly for bridge numbering or other GIS-type data
Find.State.Specific <- function(Data,Feature,col.in, VERBOSE = FALSE){
  Rows <- rownames(Data)
  for(state in unique(Data$STFIPS)){
    rows <- Rows[Data$STFIPS==state]
    
    if(state %in% names(ls.DOT.Keys)){
      if(Feature %in% names(ls.DOT.Keys[[state]])){
        pattern <- ls.DOT.Keys[[state]][[Feature]]["PATTERN"]
        if(VERBOSE) print(paste0("Checking for state-specific pattern '",pattern,"' in state: ",df.States[state,"STATE_FULL"]))
        match.keys <- grep(pattern,Data[rows,col.in])
        for(i in match.keys){
          if(!is.na(ls.DOT.Keys[[state]][[Feature]]["MOVE"])){ 
            str <- regmatches(Data[rows[i],col.in],
                              regexpr(ls.DOT.Keys[[state]][[Feature]]["MVPTRN"],
                                      Data[rows[i],col.in]))
            Data[rows[i],ls.DOT.Keys[[state]][[Feature]]["MOVE"]] <- ifelse(!is.na(Data[rows[i],ls.DOT.Keys[[state]][[Feature]]["MOVE"]]),
                                                                      paste(Data[rows[i],ls.DOT.Keys[[state]][[Feature]]["MOVE"]],
                                                                            str),
                                                                      str)
            if(VERBOSE) print(paste('moved to col',ls.DOT.Keys[[state]][[Feature]]["MOVE"]))
          }
          if(!is.na(ls.DOT.Keys[[state]][[Feature]]["ADDTO"])){ 
            Data[rows[i],ls.DOT.Keys[[state]][[Feature]]["ADDTO"]] <- ifelse(!is.na(Data[rows[i],ls.DOT.Keys[[state]][[Feature]]["ADDTO"]]),
                                                                       paste(Data[rows[i],ls.DOT.Keys[[state]][[Feature]]["ADDTO"]],
                                                                             ls.DOT.Keys[[state]][[Feature]]["ADD"]),
                                                                       ls.DOT.Keys[[state]][[Feature]]["ADD"])
            if(VERBOSE) print(paste('added to col',ls.DOT.Keys[[state]][[Feature]]["ADDTO"]))
          }
          if(!is.na(ls.DOT.Keys[[state]][[Feature]]["SUB"])){ 
            if(VERBOSE) print(paste('Deleted in col',col.in))
            Data[rows[i],col.in] <- sub(ls.DOT.Keys[[state]][[Feature]]["SUBPTRN"],
                                     ls.DOT.Keys[[state]][[Feature]]["SUB"],
                                     Data[rows[i],col.in])
          }
        }
      } 
    }
  }
  return(Data)
}