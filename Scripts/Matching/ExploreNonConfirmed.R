# Explore rows with potential matches but no confirmed match
load("~/hydraulic.failures/Data/dirs.RData")
load("~/hydraulic.failures/Data/FailandNBIforMatching.RData")
load("~/hydraulic.failures/Data/df.FAIL.NBI.Gage.Active.RData")

MatchTypes <- c("Stream","Road","Route","BIN")
# df.HasMatch <- data.frame("ID" = df.Fail$ID[!(df.Fail$ID %in% df.Fail.NBI.Gage$ID)])
# 
# for (MatchType in MatchTypes){ 
#   loadPath <- file.path(dirs$DataDirAnalysis,paste0(gsub("-","",Sys.Date()),".PossibleMatchRowsAllwExtras", MatchType,".RData"))
#   load(loadPath)
#   
#   assign("PossibleMatchRowsAll",get(paste0("PossibleMatchRowsAll",MatchType)))
#   assign("nMatchAll",get(paste0("nMatchAll",MatchType)))
#   PossibleMatchRowsNonConf <- PossibleMatchRowsAll[!(names(PossibleMatchRowsAll) %in% df.Fail.NBI.Gage$ID)]
#   MatchesNonConf <- lapply(names(PossibleMatchRowsNonConf), function(i) PossibleMatchRowsNonConf[[i]][!grepl("ID",PossibleMatchRowsNonConf[[i]]) &
#                                                                                                !grepl("-",PossibleMatchRowsNonConf[[i]])])
#   names(MatchesNonConf) <- names(PossibleMatchRowsNonConf)
#   MatchesNonConf <- lapply(names(MatchesNonConf), function(i) sub("[[:alnum:]]+.","",MatchesNonConf[[i]]))
#   names(MatchesNonConf) <- names(PossibleMatchRowsNonConf)
#   nMatchesNonConf <- unlist(lapply(MatchesNonConf,length))
#   if(MatchType==MatchTypes[1]){
#     MatchesNonConfAll <- MatchesNonConf
#   }
#   else{
#     MatchesNonConfAll <- mapply(c,MatchesNonConfAll,MatchesNonConf)
#   }
#   
#   BoolHasPossMatch    <- nMatchesNonConf > 0
#   df.HasMatch[,MatchType] <- BoolHasPossMatch
# }
# MatchesNonConfAllUnique <- lapply(MatchesNonConfAll,unique)
# nMatchesNonConfAll <- unlist(lapply(MatchesNonConfAllUnique,length))
# sum(nMatchesNonConfAll<3) # 339 with 2 or fewer, then 126 when capped returns
# sum(nMatchesNonConfAll==1) # 50 with 1, 69 with capped
# 
# MatchesNonConfAllDup <- lapply(1:length(MatchesNonConfAll),function(i) MatchesNonConfAll[[i]][any(duplicated(MatchesNonConfAll[[i]]))])
# names(MatchesNonConfAllDup) <- names(MatchesNonConfAll)
# MatchesNonConfDup    <- MatchesNonConfAllDup[sapply(names(MatchesNonConfAllDup), function(i) length(MatchesNonConfAllDup[[i]])!=0)] # 54
# nDupMatchesNonConf   <-  lapply(names(MatchesNonConfDup),function(i) sapply(1:length(MatchesNonConfDup[[i]]), function(j) sum(MatchesNonConfAll[[i]]==MatchesNonConfDup[[i]][j])) )
# # with cap there's more variation and these might be able to be used for sorting
# 
# # unique IDs
# uIDs <- names(MatchesNonConfAllUnique[nMatchesNonConfAll==1]) # these are no mostly ND ones with no stream
# uRows <- unlist(MatchesNonConfAllUnique[uIDs])
# df.Fail.PossNBI <- data.frame(ID=uIDs,NBI_ROW = uRows)
# df.Fail.PossNBI <- merge(df.Fail.PossNBI,df.Fail,by="ID")
# df.PossNBI <- df.NBI[uRows,]
# df.PossNBI$NBI_ROW <- row.names(df.PossNBI)
# df.Fail.PossNBI <- merge(df.Fail.PossNBI,df.PossNBI,by="NBI_ROW")
# View(df.Fail.PossNBI[,c(5,2,6,7,8,46,169,55:57,14,170)])

# 2018-12-17 matches now sorting, pick ones with fewer options, better best options
PossibleMatchRowsNonConf <- PossibleMatchRowsSorted[!(names(PossibleMatchRowsSorted)%in% df.Fail.NBI.Gage$ID)]
nMatchNC <- unlist(lapply(names(PossibleMatchRowsNonConf),
                          function(n) ifelse(!is.na(PossibleMatchRowsNonConf[[n]][1]),
                                             length(PossibleMatchRowsNonConf[[n]][1]),
                                             NA_integer_)))
PossibleMatchesNC <- PossibleMatchRowsNonConf[!is.na(nMatchNC)]
nMatchNC <- unlist(lapply(PossibleMatchesNC,length))
rm(PossibleMatchRowsNonConf,PossibleMatchRowsSorted)

BoolMatchFew <- nMatchNC > 4 #nMatchNC > 4 & nMatchNC <= 7
qualFirstNC <- unlist(lapply(names(PossibleMatchesNC),function(n) sub(".[[:digit:]]+\\>","",PossibleMatchesNC[[n]][1])))
BoolQualMatch <- nchar(qualFirstNC) > 3 & substr(qualFirstNC,1,1)=="1"
sum(BoolMatchFew) # 17, seems reasonable, 27 more when up to 7 potential
BoolExactFirst <- grepl("1f[[:alnum:]]{5,}", qualFirstNC) # no exact, 358 full, 187 multiple, 48 3 match
BoolBIN <- grepl("fB.",qualFirstNC) # &  #277, 172 if full,33 if no others

IDs <- names(PossibleMatchesNC[BoolBIN]) #  & BoolQualMatch BoolMatchFew
ls.Rows <- lapply(IDs, function(n) sub("[[:alnum:]]+.","",PossibleMatchesNC[[n]]))
names(ls.Rows) <- IDs
ls.df.Fail.Poss.NBI <- list()
for (n in IDs){
  nRow <- length(ls.Rows[[n]])
  df.Fail.Poss.NBI <- data.frame(ID = rep(n, nRow),
                                         NBI_ROW = ls.Rows[[n]])
  df.Fail.Poss.NBI <- merge(df.Fail.Poss.NBI,df.Fail,by="ID")
  df.PossNBI <- df.NBI[ls.Rows[[n]],]
  df.PossNBI$NBI_ROW <- row.names(df.PossNBI)
  df.Fail.Poss.NBI <- merge(df.Fail.Poss.NBI,df.PossNBI,by="NBI_ROW")
  ls.df.Fail.Poss.NBI[[n]] <- df.Fail.Poss.NBI
  
}
i<-0

i <- i +1
View(ls.df.Fail.Poss.NBI[[i]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl(ls.df.Fail.Poss.NBI[[i]]$BIN[1],ls.df.Fail.Poss.NBI[[i]]$ITEM8),])


View(ls.df.Fail.Poss.NBI[["1927"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][ls.df.Fail.Poss.NBI[["1927"]]$ITEM6A=="mill creek" & grepl("094",ls.df.Fail.Poss.NBI[["1927"]]$ROUTE_NO.y),])
View(ls.df.Fail.Poss.NBI[["1942"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][ls.df.Fail.Poss.NBI[["1942"]]$ITEM6A=="jordan creek" & grepl("101",ls.df.Fail.Poss.NBI[["1942"]]$ROUTE_NO.y),])
View(ls.df.Fail.Poss.NBI[["1345"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][ grepl("05",ls.df.Fail.Poss.NBI[["1345"]]$ROUTE_NO.y),])
View(ls.df.Fail.Poss.NBI[["294"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("big lost river",ls.df.Fail.Poss.NBI[["294"]]$ITEM6A) & grepl("93",ls.df.Fail.Poss.NBI[["294"]]$ROUTE_NO.y),])
View(ls.df.Fail.Poss.NBI[["1486"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("windsor",ls.df.Fail.Poss.NBI[["1486"]]$ITEM7) ,])
View(ls.df.Fail.Poss.NBI[["377"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("woodfield school",ls.df.Fail.Poss.NBI[["377"]]$ITEM7) ,])
View(ls.df.Fail.Poss.NBI[["386"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("stablers church",ls.df.Fail.Poss.NBI[["386"]]$ITEM7) ,])
View(ls.df.Fail.Poss.NBI[["404"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("cool spring",ls.df.Fail.Poss.NBI[["404"]]$ITEM7) ,])
View(ls.df.Fail.Poss.NBI[["114"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("peckman",ls.df.Fail.Poss.NBI[["114"]]$ITEM6A) ,])
View(ls.df.Fail.Poss.NBI[["254"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("esopus",ls.df.Fail.Poss.NBI[["254"]]$ITEM6A) & grepl("28",ls.df.Fail.Poss.NBI[["254"]]$ROUTE_NO.y),])
View(ls.df.Fail.Poss.NBI[["813"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("west canada",ls.df.Fail.Poss.NBI[["813"]]$ITEM6A) & grepl("8",ls.df.Fail.Poss.NBI[["813"]]$ROUTE_NO.y),])
View(ls.df.Fail.Poss.NBI[["1053"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("goose",ls.df.Fail.Poss.NBI[["1053"]]$ITEM6A) & grepl("732",ls.df.Fail.Poss.NBI[["1053"]]$ROUTE_NO.y),])
View(ls.df.Fail.Poss.NBI[["1247"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("goose",ls.df.Fail.Poss.NBI[["1247"]]$ITEM6A) & grepl("680",ls.df.Fail.Poss.NBI[["1247"]]$ROUTE_NO.y),])
View(ls.df.Fail.Poss.NBI[["1071"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("methow",ls.df.Fail.Poss.NBI[["1071"]]$ITEM6A) & grepl("20",ls.df.Fail.Poss.NBI[["1071"]]$ROUTE_NO.y),])
View(ls.df.Fail.Poss.NBI[["1072"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("methow",ls.df.Fail.Poss.NBI[["1072"]]$ITEM6A) & grepl("153",ls.df.Fail.Poss.NBI[["1072"]]$ROUTE_NO.y),])
View(ls.df.Fail.Poss.NBI[["1087"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("satus",ls.df.Fail.Poss.NBI[["1087"]]$ITEM6A) & grepl("97",ls.df.Fail.Poss.NBI[["1087"]]$ROUTE_NO.y),])
