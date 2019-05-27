# Combine and Sort across match types

# load all matched files and combine
MatchTypes <- c("Stream","Road","Route","BIN")
for (MatchType in MatchTypes){ 
  loadPath <- file.path(dirs$DataDirAnalysis,paste0(gsub("-","",Sys.Date()),".PossibleMatchRowsAll", MatchType,".RData"))
  load(loadPath)
  
  assign("PossibleMatchRowsAll",get(paste0("PossibleMatchRowsAll",MatchType)))
  
  if(MatchType==MatchTypes[1]){
    PossibleMatchRowsAllAll <- PossibleMatchRowsAll
  }
  else{
    PossibleMatchRowsAllAll <- mapply(c, PossibleMatchRowsAllAll, PossibleMatchRowsAll)
  }
}
names(PossibleMatchRowsAllAll) <- names(PossibleMatchRowsAll)
savePath <- file.path(dirs$DataDirAnalysis,paste0(gsub("-","",Sys.Date()),".PossibleMatchRowsAllAll.RData"))
save(PossibleMatchRowsAllAll,file=savePath)

# find duplicates
PossibleMatchRowsSorted <- list()
for (n in names(PossibleMatchRowsAllAll)){
  PossibleMatches <- PossibleMatchRowsAllAll[[n]]
  MatchRows    <-  PossibleMatches[!grepl("ID",PossibleMatches) &  !grepl("-",PossibleMatches)]
  if(length(MatchRows)==0){
    PossibleMatchRowsSorted[[n]] <- NA_character_
    next
  }
  MatchRowOnly <- sub("[[:alnum:]]+.","",MatchRows)
  MatchQualOnly <-  sub(".[[:digit:]]+\\>","",MatchRows)
  MatchQualOnly <- sapply(MatchQualOnly, function(q) paste0(unlist(strsplit(q,""))[c(2,3,1)],collapse=""))
  UniqueRows <- unique(MatchRowOnly)
  MatchQualComb <- character(length(UniqueRows))
  for (u in 1:length(UniqueRows)){
    uEntries <- which(MatchRowOnly==UniqueRows[u])
    # if(sum(uEntries)>1) print(paste("multiple matches for ID",n,", row", UniqueRows[u]))
    MatchQuals <- MatchQualOnly[uEntries]
    MatchQuals <- sort(MatchQuals)
    MatchQualComb[u] <- paste0(MatchQuals,collapse = "")
  }
  df.MatchComb <- data.frame(n = nchar(MatchQualComb),Qual = MatchQualComb, Row = UniqueRows)
  df.MatchComb$L <- 1/3*(12-df.MatchComb$n)
  df.MatchComb <- df.MatchComb[with(df.MatchComb, order(L, Qual, Row)),]
  PossibleMatchRowsSorted[[n]] <- paste0(df.MatchComb$Qual,".",df.MatchComb$Row)
}

savePath <- file.path(dirs$DataDirAnalysis,paste0(gsub("-","",Sys.Date()),".PossibleMatchRowsAllSorted.RData"))
save(PossibleMatchRowsSorted,file=savePath)
