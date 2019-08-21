Rank.Matched.Bridges <- function(ls.Matches){
  m.types <- names(ls.Matches)
  IDs     <- names(ls.Matches[[1]])
  ls.Match.Comb <- list()
  for(ID in IDs){
    entry  <- ls.Matches[[m.types[1]]][[ID]][3:length(ls.Matches[[m.types[1]]][[ID]])]
    entry <- entry[!grepl("-",entry)]
    rows  <- sub("[[:alnum:]]+.","",entry)
    qual  <- sub("[.][0-9a-z.]+\\>","",entry)
    ls.Match.Comb[[ID]] <- list(MatchID = rows, Qual = qual)
    for(m in m.types[2:length(m.types)]){
      entry  <- ls.Matches[[m]][[ID]][3:length(ls.Matches[[m]][[ID]])]
      entry <- entry[!grepl("-",entry)]
      rows  <- sub("[[:alnum:]]+.","",entry)
      qual  <- sub(".[[:digit:]]+\\>","",entry)
      ls.Match.Comb[[ID]]$MatchID <- c(ls.Match.Comb[[ID]]$MatchID, rows)
      ls.Match.Comb[[ID]]$Qual <- c(ls.Match.Comb[[ID]]$Qual, qual)
      if(anyDuplicated(ls.Match.Comb[[ID]]$MatchID)>0){
        ls.Match.Comb[[ID]]$Qual[duplicated(ls.Match.Comb[[ID]]$MatchID,fromLast = TRUE)] <- paste0(ls.Match.Comb[[ID]]$Qual[duplicated(ls.Match.Comb[[ID]]$MatchID)],
                                                                                                    ls.Match.Comb[[ID]]$Qual[duplicated(ls.Match.Comb[[ID]]$MatchID,fromLast = TRUE)],
                                                                                                    collapse = "")
        ls.Match.Comb[[ID]]$Qual    <- ls.Match.Comb[[ID]]$Qual[!duplicated(ls.Match.Comb[[ID]]$MatchID)]
        ls.Match.Comb[[ID]]$MatchID <- ls.Match.Comb[[ID]]$MatchID[!duplicated(ls.Match.Comb[[ID]]$MatchID)]
        ls.Match.Comb[[ID]]$MatchID <- ls.Match.Comb[[ID]]$MatchID[order(ls.Match.Comb[[ID]]$Qual, decreasing = TRUE)]
        ls.Match.Comb[[ID]]$Qual    <- ls.Match.Comb[[ID]]$Qual[order(ls.Match.Comb[[ID]]$Qual, decreasing = TRUE)]
      }
    }
  }
  return(ls.Match.Comb)
}