# Script to calculate metrics of matches (recall, precision, F1)
load("~/hydraulic.failures/Data/dirs.RData")
load("~/hydraulic.failures/FailandNBIforMatching.RData")
load("~/hydraulic.failures/Data/df.FAIL.NBI.Gage.Active.RData")

# library(RCurl)
MatchTypes <- c("Stream","Road","Route","BIN")
p <- sum(!is.na(df.Fail.NBI.Gage$NBI_ROW))
df.MatchMetrics <- data.frame("recall"=numeric(),"precision"=numeric(),"f1"=numeric(),"f2"=numeric())
df.WhichMatch <- data.frame("ID" = df.Fail.NBI.Gage$ID[!is.na(df.Fail.NBI.Gage$NBI_ROW)])

for (MatchType in MatchTypes){ 
  loadPath <- file.path(dirs$DataDirAnalysis,paste0(gsub("-","",Sys.Date()),".PossibleMatchRowsAllwExtras", MatchType,".RData"))
  load(loadPath)
  
  assign("PossibleMatchRowsAll",get(paste0("PossibleMatchRowsAll",MatchType)))
  assign("PossibleMatchRowsConf",get(paste0("PossibleMatchRowsConf",MatchType)))
  assign("nMatchAll",get(paste0("nMatchAll",MatchType)))
  assign("nMatchConf",get(paste0("nMatchConf",MatchType)))
  assign("MatchesConf",get(paste0("MatchesConf",MatchType)))
  assign("SingleMatchConf",get(paste0("SingleMatchConf",MatchType)))
  MatchesConfNBI <- MatchesConf[names(MatchesConf) %in% df.Fail.NBI.Gage$ID[!is.na(df.Fail.NBI.Gage$NBI_ROW)]]
  nMatchConfNBI       <- nMatchConf[names(nMatchConf) %in% df.Fail.NBI.Gage$ID[!is.na(df.Fail.NBI.Gage$NBI_ROW)]]
  
  if(MatchType==MatchTypes[1]){
    PossibleMatchRowsAllAll <- PossibleMatchRowsAll
    MatchesConfNBIAll <- MatchesConfNBI
  }
  else{
    PossibleMatchRowsAllAll <- mapply(c, PossibleMatchRowsAllAll, PossibleMatchRowsAll)
    MatchesConfNBIAll <- mapply(c, MatchesConfNBIAll, MatchesConfNBI)
  }
  
  print(paste("For",MatchType))
  # range(nMatchAll)
  # range(nMatchConf)
  # range(nMatchConfNBI)
  
  BoolCorrectMatch    <- sapply(names(MatchesConf), function(ID) df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"NBI_ROW"] %in% MatchesConf[[ID]])
  BoolCorrectMatchNBI <- BoolCorrectMatch[names(BoolCorrectMatch) %in% df.Fail.NBI.Gage$ID[!is.na(df.Fail.NBI.Gage$NBI_ROW)]]
  df.WhichMatch[,MatchType] <- BoolCorrectMatchNBI
  
  # DistToMatchNBI      <- sapply()
  
  
  tp <- sum(BoolCorrectMatchNBI)
  fn <- p - tp
  fp <- sum(nMatchConfNBI) - tp
  recall    = tp/(tp+fn)
  precision = tp/(tp+fp)
  f1 <- 2*recall*precision/(recall+precision)
  f2 <- 5*recall*precision/(recall+4*precision)
  MatchMetrics <- c(recall = recall, precision = precision, f1 = f1, f2 = f2)
  df.MatchMetrics[MatchType,] <- MatchMetrics
  assign(paste0("MatchMetrics",MatchType),MatchMetrics)
}


View(df.WhichMatch)
sum(apply(df.WhichMatch[,MatchTypes],MARGIN = 1,sum)==1) # 47 identified through only 1 type
sum(apply(df.WhichMatch[,MatchTypes],MARGIN = 1,sum)==2) # 145 identified through 2 types
sum(apply(df.WhichMatch[,MatchTypes],MARGIN = 1,sum)==3) # 147 identified through 3 types
sum(apply(df.WhichMatch[,MatchTypes],MARGIN = 1,sum)==4) # 25 identified through 4 types

# look at OR
# PossibleMatchRowsAllAll <- list(PossibleMatchRowsAllStream, PossibleMatchRowsAllBIN,PossibleMatchRowsAllRoad,PossibleMatchRowsAllRoute)
# keys <- unique(unlist(lapply(PossibleMatchRowsAllAll, names)))
# setNames(do.call(mapply, c(FUN=c, lapply(PossibleMatchRowsAllAll, `[`, keys))), keys)

totalMatches <- sum(apply(df.WhichMatch[,MatchTypes],MARGIN = 1,any)) # 364, now 358 with cutoff for nMatch
MatchesConfNBIUnique <- lapply(MatchesConfNBIAll,unique)
nMatchesConfNBIUnique <- sapply(MatchesConfNBIUnique,length)

df.MatchMetrics["any","recall"] <- totalMatches/p # 96%!
df.MatchMetrics["any","precision"] <- p/(sum(nMatchesConfNBIUnique))
df.MatchMetrics["any","f1"] <- 2*df.MatchMetrics["any","precision"]*df.MatchMetrics["any","recall"]/(df.MatchMetrics["any","recall"]+df.MatchMetrics["any","precision"])
df.MatchMetrics["any","f2"] <- 5*df.MatchMetrics["any","precision"]*df.MatchMetrics["any","recall"]/(df.MatchMetrics["any","recall"]+4*df.MatchMetrics["any","precision"])
View(df.MatchMetrics)


onlyOneMatch <- which(apply(df.WhichMatch[,MatchTypes],MARGIN = 1,sum)==1)
whichMatched <- sapply(df.WhichMatch[onlyOneMatch,MatchTypes],which)
nOnlyMatches <- sapply(names(whichMatched), function(i) length(whichMatched[[i]])) # stream > route ~= road > BIN

savePath <- file.path(dirs$DataDirAnalysis,paste0(gsub("-","",Sys.Date()),".MatchMetrics.RData"))
save(MatchesConfNBIUnique,df.MatchMetrics,df.WhichMatch,nOnlyMatches,totalMatches,file=savePath)

PossibleMatchRowsAllAllSorted <- lapply(names(PossibleMatchRowsAllAll), function(i) PossibleMatchRowsAllAll[[i]][!grepl("ID",PossibleMatchRowsAllAll[[i]]) &
                                                                                     !grepl("-",PossibleMatchRowsAllAll[[i]])])
PossDup <- lapply(names(PossibleMatchRowsAllAllSorted), function(i) duplicated(PossibleMatchRowsAllAll[[i]])
PossibleMatchRowsAllAllSorted <- lapply(names(PossibleMatchRowsAllAllSorted), function(i) sort(PossibleMatchRowsAllAllSorted[[i]]))
