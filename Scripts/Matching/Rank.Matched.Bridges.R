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
      ls.Match.Comb[[ID]]$Qual    <- c(ls.Match.Comb[[ID]]$Qual, qual)
      if(anyDuplicated(ls.Match.Comb[[ID]]$MatchID)>0){
        for(i in 1:sum(duplicated(ls.Match.Comb[[ID]]$MatchID))){
          r     <- ls.Match.Comb[[ID]]$MatchID[duplicated(ls.Match.Comb[[ID]]$MatchID)][1]
          qOrig <- ls.Match.Comb[[ID]]$Qual[ls.Match.Comb[[ID]]$MatchID==r][1]
          qNew  <- ls.Match.Comb[[ID]]$Qual[ls.Match.Comb[[ID]]$MatchID==r][2]
          df <- data.frame(qs = c(qOrig, qNew), stringsAsFactors = FALSE)
          df$n  <- -1*nchar(df$qs)
          q     <- df$qs[order(df$n, df$qs)]
          q     <- paste0(q,collapse="")
          ls.Match.Comb[[ID]]$Qual[ls.Match.Comb[[ID]]$MatchID==r] <- q
          ls.Match.Comb[[ID]]$Qual    <- ls.Match.Comb[[ID]]$Qual[-which(ls.Match.Comb[[ID]]$MatchID==r)[2]]
          ls.Match.Comb[[ID]]$MatchID <- ls.Match.Comb[[ID]]$MatchID[-which(ls.Match.Comb[[ID]]$MatchID==r)[2]]
        }
      }
      orderL <- data.frame(n=-1*nchar(ls.Match.Comb[[ID]]$Qual), a = ls.Match.Comb[[ID]]$Qual, stringsAsFactors = F)
      ls.Match.Comb[[ID]]$Qual    <- ls.Match.Comb[[ID]]$Qual[order(orderL$n,orderL$a)]
      ls.Match.Comb[[ID]]$MatchID <- ls.Match.Comb[[ID]]$MatchID[order(orderL$n,orderL$a)]
    }
  }
  return(ls.Match.Comb)
}
