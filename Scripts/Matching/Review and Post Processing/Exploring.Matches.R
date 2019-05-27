# loop over Match Type, assume PossibleMatchRowsAll__ has already been run
source('~/hydraulic.failures/Scripts/Matching/Helper/GenerateMatches.R')
source('~/hydraulic.failures/Scripts/Matching/Helper/PerformMatch.R')
load("~/hydraulic.failures/FailandNBIforMatching.RData")
load("~/hydraulic.failures/Data/df.FAIL.NBI.Gage.Active.RData")
load("~/hydraulic.failures/Data/ls.Adj.States.RData")

nMatchEntries <- nrow(df.Fail)
nMatchConf    <- nrow(df.Fail.NBI.Gage)

MatchTypes <- c("Stream","Road","Route","BIN") # 
maxStringDist <- c(Stream = 2, Road = 2, Route = 1, BIN = 0)

for (MatchType in MatchTypes){ 
  PossibleMatchRowsAll <- lapply(1:nMatchEntries, 
                                 function(i) GenerateMatches(df.Fail[i,],tolower(MatchType),"fail","nbi",maxStringDist = maxStringDist[MatchType],VERBOSE = TRUE))

  # assign("PossibleMatchRowsAll",get(paste0("PossibleMatchRowsAll",MatchType)))
  names(PossibleMatchRowsAll) <- df.Fail$ID
  # nMatchAll <- sapply(1:nMatchEntries, function(i) sum(!grepl("ID",PossibleMatchRowsAll[[i]]) &
  #                                                     !grepl("-",PossibleMatchRowsAll[[i]])))
  # names(nMatchAll) <- names(PossibleMatchRowsAll)
  # 
  # hist(nMatchAll[nMatchAll < 100]) # 991 less than 100
  # hist(nMatchAll[nMatchAll < 10])  # 563 less than 10, 335 with 1 only
  # 
  # PossibleMatchRowsConf <- PossibleMatchRowsAll[names(PossibleMatchRowsAll) %in% df.Fail.NBI.Gage$ID]
  # nMatchConf <- sapply(names(PossibleMatchRowsConf), function(i) sum(!grepl("ID",PossibleMatchRowsConf[[i]]) &
  #                                                     !grepl("-",PossibleMatchRowsConf[[i]])))
  # names(nMatchConf) <- names(PossibleMatchRowsConf)
  # 
  # hist(nMatchConf[nMatchConf < 100]) 
  # print(paste("Number of",MatchType,"matches with n < 100:",sum(nMatchConf < 100)))
  # hist(nMatchConf[nMatchConf < 10])  
  # print(paste("Number of",MatchType,"matches with n < 10:",sum(nMatchConf < 10)))
  # print(paste("Number of",MatchType,"matches with n = 1:",sum(nMatchConf == 1)))
  # 
  # # matches evaluation
  # MatchesConf <- lapply(names(PossibleMatchRowsConf), function(i) PossibleMatchRowsConf[[i]][!grepl("ID",PossibleMatchRowsConf[[i]]) &
  #                                                                                                                !grepl("-",PossibleMatchRowsConf[[i]])])
  # names(MatchesConf) <- names(PossibleMatchRowsConf)
  # MatchesConf <- lapply(names(MatchesConf), function(i) sub("[[:alnum:]]+.","",MatchesConf[[i]]))
  # names(MatchesConf) <- names(PossibleMatchRowsConf)
  # BoolCorrectMatch <-sapply(names(MatchesConf), function(ID) df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"NBI_ROW"] %in% MatchesConf[[ID]])
  # nCorrectMatch <- sum(BoolCorrectMatch) # 326 / nMatchConf
  # print(paste("Number of",MatchType,"matches with correct match:",nCorrectMatch))
  # 
  # # see if single-matches are correct
  # SingleMatchConf <- unlist(MatchesConf[nMatchConf==1])
  # BoolCorrectSingleMatchConf <- sapply(names(SingleMatchConf), function(ID) SingleMatchConf[ID] == df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"NBI_ROW"])
  # print(paste("Number of",MatchType,"single matches with correct match:",
  #             sum(BoolCorrectSingleMatchConf, na.rm = T),
  #             "of", sum(nMatchConf == 1)))
  
  #save results
  assign(paste0("PossibleMatchRowsAll",MatchType),PossibleMatchRowsAll)
  # assign(paste0("PossibleMatchRowsConf",MatchType),PossibleMatchRowsConf)
  # assign(paste0("nMatchAll",MatchType),nMatchAll)
  # assign(paste0("nMatchConf",MatchType),nMatchConf)
  # assign(paste0("MatchesConf",MatchType),MatchesConf)
  # assign(paste0("SingleMatchConf",MatchType),SingleMatchConf)
  savePath <- file.path(dirs$DataDirAnalysis,paste0(gsub("-","",Sys.Date()),".PossibleMatchRowsAll", MatchType,".RData"))
  # save(list = c(paste0("PossibleMatchRowsAll",MatchType),
  #      paste0("PossibleMatchRowsConf",MatchType),
  #      paste0("nMatchAll",MatchType),
  #      paste0("nMatchConf",MatchType),
  #      paste0("MatchesConf",MatchType),
  #      paste0("SingleMatchConf",MatchType)), 
  #      file = savePath)
  save(list = c(paste0("PossibleMatchRowsAll",MatchType)#,
                # paste0("PossibleMatchRowsConf",MatchType),
                # paste0("nMatchAll",MatchType),
                # paste0("nMatchConf",MatchType),
                # paste0("MatchesConf",MatchType),
                # paste0("SingleMatchConf",MatchType)
                ), 
       file = savePath)
  rm(list=ls(pattern = "Match[CRASB]"))
  rm(list=ls(pattern = "tMatch\\>"))
  rm(list=ls(pattern = "Matches[[:alpha:]]+"))
  rm(savePath)
}
