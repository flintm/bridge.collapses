# full stream set ------
PossibleMatchRowsAllStream <- lapply(1:1127, function(i) GenerateMatches(df.Fail[i,],"stream","fail","nbi",maxStringDist = 2,VERBOSE = TRUE))

names(PossibleMatchRowsAllStream) <- df.Fail$ID
nMatchAllStream <- sapply(1:1127, function(i) sum(!grepl("ID",PossibleMatchRowsAllStream[[i]]) &
                                                    !grepl("-",PossibleMatchRowsAllStream[[i]])))
names(nMatchAllStream) <- names(PossibleMatchRowsAllStream)

hist(nMatchAllStream[nMatchAllStream < 100]) # 991 less than 100
hist(nMatchAllStream[nMatchAllStream < 10])  # 563 less than 10, 335 with 1 only

PossibleMatchRowsConfStream <- PossibleMatchRowsAllStream[names(PossibleMatchRowsAllStream) %in% df.Fail.NBI.Gage$ID]
nMatchConfStream <- sapply(1:387, function(i) sum(!grepl("ID",PossibleMatchRowsConfStream[[i]]) &
                                                    !grepl("-",PossibleMatchRowsConfStream[[i]])))
names(nMatchConfStream) <- names(PossibleMatchRowsConfStream)

hist(nMatchConfStream[nMatchConfStream < 100]) 
sum(nMatchConfStream < 100) # 348
hist(nMatchConfStream[nMatchConfStream < 10])  
sum(nMatchConfStream < 10) # 158
sum(nMatchConfStream == 1) # 52

# matches evaluation
MatchesConfStream <- lapply(names(PossibleMatchRowsConfStream), function(i) PossibleMatchRowsConfStream[[i]][!grepl("ID",PossibleMatchRowsConfStream[[i]]) &
                                                                                                        !grepl("-",PossibleMatchRowsConfStream[[i]])])
names(MatchesConfStream) <- names(PossibleMatchRowsConfStream)
MatchesConfStream <- lapply(names(MatchesConfStream), function(i) sub("[[:alnum:]]+.","",MatchesConfStream[[i]]))
names(MatchesConfStream) <- names(PossibleMatchRowsConfStream)
BoolCorrectMatchStream <-sapply(names(MatchesConfStream), function(ID) df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"NBI_ROW"] %in% MatchesConfStream[[ID]])
nCorrectMatchStream <- sum(BoolCorrectMatchStream) # 326 / 387

# see if single-matches are correct
SingleMatchConfStream <- unlist(MatchesConfStream[nMatchConfStream==1])
BoolCorrectSingleMatchConfStream <- sapply(names(SingleMatchNBIRowStream), function(ID) SingleMatchNBIRowStream[ID] == df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"NBI_ROW"])
nCorrectSingleMatchConfStream <- sum(BoolCorrectSingleMatchConfStream) # 37 / 52

#save results
savePath <- file.path(dirs$DataDirAnalysis,paste(gsub("-","",Sys.Date()),"PossibleMatchRowsAllStream.RData",sep="."))
save(PossibleMatchRowsAllStream,PossibleMatchRowsConfStream,
     nMatchAllStream,nMatchConfStream,MatchesConfStream,SingleMatchConfStream, file = savePath)


# full road set ------
PossibleMatchRowsAllRoad <- lapply(1:1127, function(i) GenerateMatches(df.Fail[i,],"road","fail","nbi",maxStringDist = 2,VERBOSE = TRUE))

names(PossibleMatchRowsAllRoad) <- df.Fail$ID
nMatchAllRoad <- sapply(1:1127, function(i) sum(!grepl("ID",PossibleMatchRowsAllRoad[[i]]) &
                                                    !grepl("-",PossibleMatchRowsAllRoad[[i]])))
names(nMatchAllRoad) <- names(PossibleMatchRowsAllRoad)

names(nMatchAllRoad) <- names(PossibleMatchRowsAllRoad)

hist(nMatchAllRoad[nMatchAllRoad < 100]) # 991 less than 100
hist(nMatchAllRoad[nMatchAllRoad < 10])  # 563 less than 10, 335 with 1 only

PossibleMatchRowsConfRoad <- PossibleMatchRowsAllRoad[names(PossibleMatchRowsAllRoad) %in% df.Fail.NBI.Gage$ID]
nMatchConfRoad <- sapply(1:387, function(i) sum(!grepl("ID",PossibleMatchRowsConfRoad[[i]]) &
                                                    !grepl("-",PossibleMatchRowsConfRoad[[i]])))
names(nMatchConfRoad) <- names(PossibleMatchRowsConfRoad)

hist(nMatchConfRoad[nMatchConfRoad < 100]) 
sum(nMatchConfRoad < 100) # 348
hist(nMatchConfRoad[nMatchConfRoad < 10])  
sum(nMatchConfRoad < 10) # 158
sum(nMatchConfRoad == 1) # 60

MatchesConfRoad <- lapply(names(PossibleMatchRowsConfRoad), function(i) PossibleMatchRowsConfRoad[[i]][!grepl("ID",PossibleMatchRowsConfRoad[[i]]) &
                                                                                                        !grepl("-",PossibleMatchRowsConfRoad[[i]])])
names(MatchesConfRoad) <- names(PossibleMatchRowsConfRoad)
MatchesConfRoad <- lapply(names(MatchesConfRoad), function(i) sub("[[:alnum:]]+.","",MatchesConfRoad[[i]]))
names(MatchesConfRoad) <- names(PossibleMatchRowsConfRoad)
BoolCorrectMatchRoad <-sapply(names(MatchesConfRoad), function(ID) df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"NBI_ROW"] %in% MatchesConfRoad[[ID]])
nCorrectMatchRoad <- sum(BoolCorrectMatchRoad) # 189 / 387

# see if single-matches are correct
SingleMatchConfRoad <- unlist(MatchesConfRoad[nMatchConfRoad==1])
BoolCorrectSingleMatchConfRoad <- sapply(names(SingleMatchNBIRowRoad), function(ID) SingleMatchNBIRowRoad[ID] == df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"NBI_ROW"])
nCorrectSingleMatchConfRoad <- sum(BoolCorrectSingleMatchConfRoad, na.rm = T) # 36 / 60

#save results
savePath <- file.path(dirs$DataDirAnalysis,paste(gsub("-","",Sys.Date()),"PossibleMatchRowsAllRoad.RData",sep="."))
save(PossibleMatchRowsAllRoad,PossibleMatchRowsConfRoad,
     nMatchAllRoad,nMatchConfRoad,MatchesConfRoad,SingleMatchConfRoad, file = savePath)

# full route set ------
PossibleMatchRowsAllRoute <- lapply(1:1127, function(i) GenerateMatches(df.Fail[i,],"route","fail","nbi",maxStringDist = 1,VERBOSE = TRUE))

names(PossibleMatchRowsAllRoute) <- df.Fail$ID
nMatchAllRoute <- sapply(1:1127, function(i) sum(!grepl("ID",PossibleMatchRowsAllRoute[[i]]) &
                                                  !grepl("-",PossibleMatchRowsAllRoute[[i]])))
names(nMatchAllRoute) <- names(PossibleMatchRowsAllRoute)

hist(nMatchAllRoute[nMatchAllRoute < 100]) # 1057 less than 100
sum(nMatchAllRoute < 100) # 1057 less than 100
hist(nMatchAllRoute[nMatchAllRoute < 10])  
sum(nMatchAllRoute < 10) # 772 less than 100
sum(nMatchAllRoute == 1) # 21 with one. NOTE: no different than maxString = 2, so best must have been 1 or lower in all cases

PossibleMatchRowsConfRoute <- PossibleMatchRowsAllRoute[names(PossibleMatchRowsAllRoute) %in% df.Fail.NBI.Gage$ID]
nMatchConfRoute <- sapply(1:387, function(i) sum(!grepl("ID",PossibleMatchRowsConfRoute[[i]]) &
                                                  !grepl("-",PossibleMatchRowsConfRoute[[i]])))
names(nMatchConfRoute) <- names(PossibleMatchRowsConfRoute)

hist(nMatchConfRoute[nMatchConfRoute < 100]) 
sum(nMatchConfRoute < 100) # 353
hist(nMatchConfRoute[nMatchConfRoute < 10])  
sum(nMatchConfRoute < 10) # 216
sum(nMatchConfRoute == 1) # 7

# see if all matches are correct
MatchesConfRoute <- lapply(names(PossibleMatchRowsConfRoute), function(i) PossibleMatchRowsConfRoute[[i]][!grepl("ID",PossibleMatchRowsConfRoute[[i]]) &
                                                                                               !grepl("-",PossibleMatchRowsConfRoute[[i]])])
names(MatchesConfRoute) <- names(PossibleMatchRowsConfRoute)
MatchesConfRoute <- lapply(names(MatchesConfRoute), function(i) sub("[[:alnum:]]+.","",MatchesConfRoute[[i]]))
names(MatchesConfRoute) <- names(PossibleMatchRowsConfRoute)
BoolCorrectMatchRoute <-sapply(names(MatchesConfRoute), function(ID) df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"NBI_ROW"] %in% MatchesConfRoute[[ID]])
nCorrectMatchRoute <- sum(BoolCorrectMatchRoute) # 188 / 387

# see if single-matches are correct
SingleMatchConfRoute <- unlist(MatchesConfRoute[nMatchConfRoute==1])
BoolCorrectSingleMatchConfRoute <- sapply(names(SingleMatchNBIRowRoute), function(ID) SingleMatchNBIRowRoute[ID] == df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"NBI_ROW"])
nCorrectSingleMatchConfRoute <- sum(BoolCorrectSingleMatchConfRoute, na.rm = T) # 5 / 7

#save results
savePath <- file.path(dirs$DataDirAnalysis,paste(gsub("-","",Sys.Date()),"PossibleMatchRowsAllRoute.RData",sep="."))
save(PossibleMatchRowsAllRoute,PossibleMatchRowsConfRoute,
     nMatchAllRoute,nMatchConfRoute,MatchesConfRoute,SingleMatchConfRoute, file = savePath)