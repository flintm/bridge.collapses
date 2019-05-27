load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirsGit$Data,"df.USbridges.water.RData"))
nBridges <- nrow(df.USbridges.water)
pF175 <- pnorm(-1.75)
pF3   <- pnorm(-3)

p50 <- 0.02

n50yrInGivenYr <- round(p50*nBridges)

pAnyFail175 <- 1 - dbinom(0, n50yrInGivenYr, pF175)
pAnyFail3   <- 1 - dbinom(0, n50yrInGivenYr, pF3)

n50yrIn19years <- n50yrInGivenYr*19 # 19 unique failure years
p35Fail19yr175 <- dbinom(35, n50yrIn19years,pF175)
p35Fail19yr3 <- dbinom(35, n50yrIn19years,pF3)

mu19yrs175 <- n50yrIn19years*pF175
mu19yrs3   <- n50yrIn19years*pF3

# 71 unique fail years in FailDataFrame with 1127
n50yrIn71years <- 71*n50yrInGivenYr
dbinom(1127,n50yrIn71years,pF3)

# compare beta assumptions versus my results--mean, median, and then lognormal dist
t.analysis <- 100
TfMeanD <- mean(df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_HECD_USGS"])
TfMeanIP <- mean(df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"],na.rm=TRUE)
TfMedianD <- median(df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_HECD_USGS"])
TfMedianIP <- median(df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"],na.rm=TRUE)
# probability of failure in 100-year period assuminging pF is PMF with all mass at mean val
# pF_100 years = pF_Mean event happens in 100 years
pMeanD   <- 1/TfMeanD
pMeanIP  <- 1/TfMeanIP
lambdaD  <- pMeanD*100
lambdaIP <- pmeanIP*100
pF100meanD   <- 1 - ppois(0,lambdaD)
pF100meanIP  <- 1 - ppois(0,lambdaIP)

# PMF with all mass at median
pMedianD   <- 1/TfMedianD
pMedianIP  <- 1/TfMedianIP
lambdaD    <- pMedianD*100
lambdaIP   <- pMedianIP*100
pF100medD  <- 1 - ppois(0,lambdaD)
pF100medIP <- 1 - ppois(0,lambdaIP)

# lognormal distribution for return period of failure event
logTfD  <- log(df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_HECD_USGS"])
logTfIP <- df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"]
logTfIP <- logTfIP[!is.na(logTfIP)]
meanlogTfD <- mean(logTfD)
sdlogTfD   <- sd(logTfD)

meanlogTfIP <- mean(logTfIP)
sdlogTfIP   <- sd(logTfIP)

# to get probability of failure 
# convolution integral of probability that it fails given the return period is "t"
# times the probability of seeing an event with probability "t"
dt <- 1
t <- seq(dt,12000,by=dt)
# load(file.path(dirs$DataDirFrequent,"20160107_PeakFQ_PFA_USGSd_UserRegSkews.RData"))
# load(file.path(dirs$DataDirFrequent,"20160107_PeakFQ_PFA_USGSp_UserRegSkews.RData"))
# Q.full <- exp(seq(log(10),log(1000000), length.out = 10000))
# lambda_Q <- list()
# library(sfsmisc)
require(Hmisc)
# Freq       <- sapply(PFA_USGSd,"[[","ANN_EXC_PROB", simplify = FALSE, USE.NAMES = TRUE) 
# FlowAtFreq <- sapply(PFA_USGSd,"[[","BULL17B_EST", simplify = FALSE, USE.NAMES = TRUE)

# for (i in 1:length(rowsToView)){
#   STAID <- df.Fail.NBI.Gage[rowsToView[i],"STAID"]
#   ID    <- df.Fail.NBI.Gage[rowsToView[i],"ID"]
#   #   Q     <- PFA_USGSd[[STAID]]$BULL17B_EST
#   #   F_Q   <- 1 - PFA_USGSd[[STAID]]$ANN_EXC_PROB
#   #   FQ.full <- approxExtrap
#   #   fQD   <- D1ss(Q, F_Q, xout = Q)
#   #   fQD[fQD < 0] <- 0
#   #   F_Q   <- 
#   #   lambda_Q <- fQD/(1-F_Q)
#   log_flow <- log(FlowAtFreq[[STAID]][!is.na(FlowAtFreq[[STAID]])])
#   log_freq <- log(Freq[[STAID]][!is.na(FlowAtFreq[[STAID]])])
#   log_freq <- log_freq[!is.na(log_flow)]
#   log_flow <- log_flow[!is.na(log_flow)]
#   Q <- seq(min(1,min(FlowAtFreq[[STAID]])), max(FlowAtFreq[[STAID]]), length.out = 1000)
#   G.Q      <- exp(approxExtrap(log_flow,log_freq,log(Q))$y)
#   G.Q[G.Q > 1] <- 1
#   G.Q[G.Q < 0] <- 0
#   f.Q <- D1ss(Q,1-G.Q)
#   lambda.Q <- f.Q/G.Q
#   P.F.Q <- 1-pnorm(log(Q), meanlogTfD, sdlogTfD)
# }


pFlogD <- 0
pFlogIP <- 0
for (i in 1:length(t)){
  dP <- dnorm(log(t[i]),meanlogTfD,sdlogTfD)*1/t[i]^2
  pFlogD <- pFlogD + dP
  dP <- dnorm(log(t[i]),meanlogTfIP,sdlogTfIP)*1/t[i]^2
  pFlogIP <- pFlogIP + dP
}

# using kernel density for T
pTkernelD  <- density(df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_HECD_USGS"], bw = "nrd", adjust = 0.5, from=0, to=2000)
pTkernelIP <- density(df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"][!is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"])], bw = "nrd",adjust = 0.5, from=0, to=11000)
pTkernelDinterp <- approxExtrap(pTkernelD$x, pTkernelD$y, t)
pTkernelIPinterp <- approxExtrap(pTkernelIP$x, pTkernelIP$y, t)
pFkernelD  <- 0
pFkernelIP <- 0
for (i in 1:length(t)){
  dP <- pTkernelDinterp$y[i]*1/t[i]^2
  pFkernelD <- pFkernelD + dP
  dP <- pTkernelIPinterp$y[i]*1/t[i]^2
  pFkernelIP <- pFkernelIP + dP
}

pF35 <- pnorm(-3.5)
pF50y175 <- 0.02*pnorm(-1.75)
pF100y175 <- 0.01*pnorm(-1.75)
pFannualEstimates <- list(pFD  = c(mean = pMeanD, median = pMedianD, logn = pFlogD, kernel = pFkernelD),
                          pFIP = c(mean = pMeanIP, median = pMedianIP, logn = pFlogIP, kernel = pFkernelIP),
                          pFrel = c("3pt5" = pF35, "50y1pt75" = pF50y175, "100yr1pt75" = pF100y175))
save(pFannualEstimates, file = file.path(dirsGit$Data, "20160419_FailureReliabilityComparison.RData"))
# get results of linear trends analysis - peakLinearFits of only significant MK stations ----------
load(file.path(dirs$DataDirAnalysis,"20150729_TrendAnalysisResults.RData"))
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))


for(i in 1:length(rowsToView)){
  if(df.Fail.NBI.Gage[rowsToView[i],"STAID"] %in% stationsSignifTrends){
    T.Fail <- df.Fail.NBI.Gage[rowsToView[i],"T_FAIL_D_HECD_USGS"] 
    Q.Fail <- df.Fail.NBI.Gage[rowsToView[i],"Q_FAIL_D_USGS"]
    # Q.Fail.25 <- 
  }
}

# look for nearby bridges - within 10km of the 34 gauges
load(file.path(dirs$DataDirFrequent,"StateCountyCityData.RData"))
load(file.path(dirsGit$Data,"df.USgauges.RData"))
df.Adj.States        <- read.table(file.path(dirs$OrigDataDir,"PoliticalBoundaries","AdjacentStatesList.txt"),skip=1,sep=c(",",";"),stringsAsFactors=FALSE)
ls.Adj.States        <- vector("list",50)
ls.Adj.States        <- lapply(1:length(ls.Adj.States), function(i) df.Adj.States[i,2])
ls.Adj.States        <- lapply(1:length(ls.Adj.States), function(i) unlist(strsplit(ls.Adj.States[[i]],";")))
names(ls.Adj.States) <- df.Adj.States[,1]
NoAdjStates          <- which(sapply(1:length(ls.Adj.States), function(i) length(ls.Adj.States[[i]])==0))
HasAdjStates         <- which(sapply(1:length(ls.Adj.States), function(i) length(ls.Adj.States[[i]])!=0))
ls.Adj.STFIPS        <- lapply(HasAdjStates, function(i) sapply(1:length(ls.Adj.States[[i]]), 
                                                                function(j) df.States[df.States$STATE_CODE==ls.Adj.States[[i]][j],"STFIPS"]))
names(ls.Adj.STFIPS) <- sapply(HasAdjStates, function(i) as.character(df.States[df.States$STATE_CODE==df.Adj.States[i,1],"STFIPS"]))
NoAdjStatesSTFIPS    <- sapply(NoAdjStates, function(i) as.character(df.States[df.States$STATE_CODE==df.Adj.States[i,1],"STFIPS"]))
ls.Adj.STFIPS[NoAdjStatesSTFIPS] <- lapply(NoAdjStates, function(i) integer(0))
source(file.path(dirsGit$Scripts,'gcd_slc.R'))
source(file.path(dirsGit$Scripts,'deg2rad.R'))
load(file.path(dirsGit$Data,"df.USbridges.water.RData"))
STAIDtoView <- unique(df.Fail.NBI.Gage[rowsToView,"STAID"])
possibleMatches <- list()
for (i in rowsToView){
  STAID   <- df.Fail.NBI.Gage[i,"STAID"]
  ID    <- as.character(df.Fail.NBI.Gage[i,"ID"])
  maxDate <- as.Date(df.Fail.NBI.Gage[i,"YR_FAIL_EST"]+364,"1970-01-01")
  
  STFIPS <- c(df.USgauges[df.USgauges$STAID == STAID,"STFIPS"], ls.Adj.STFIPS[[as.character(df.USgauges[df.USgauges$STAID == STAID,"STFIPS"])]])
  bridgesInState <- row.names(df.USbridges.water[df.USbridges.water$STFIPS %in% STFIPS & (!is.na(df.USbridges.water$ITEM27) & df.USbridges.water$ITEM27 < maxDate),])
  dist <- gcd_slc(deg2rad(df.USbridges.water[bridgesInState,"LONGDD"]),
                  deg2rad(df.USbridges.water[bridgesInState,"LATDD"]),
                  rep(deg2rad(df.USgauges[df.USgauges$STAID == STAID,"LNG_GAGE"]),length(bridgesInState)),
                  rep(deg2rad(df.USgauges[df.USgauges$STAID == STAID,"LAT_GAGE"]),length(bridgesInState)))
  bridgesNearGage <- bridgesInState[dist <= 10]
  possibleMatches[[ID]] <- bridgesNearGage
}


# find bridges within 10km on the right stream
library(stringr)
load(file.path(dirs$DataDirFrequent,"20150423_gageDataFrameProcessedStreamName.RData"))
source(file.path(dirsGit$Scripts,'MatchEntry.R'))
source(file.path(dirsGit$Scripts,'PerformMatch.R'))
cols <- c("STAID","STANAME","DRAIN_SQKM","LAT_GAGE","LNG_GAGE","STFIPS_GAGE","FIPS_GAGE","STREAM_NAME_GAGE","STREAM_TYPE_GAGE","STREAM_OTHER_GAGE","STREAM_TRIB_GAGE")
colsG <- c("STAID","STANAME","DRAIN_SQKM","LAT_GAGE","LNG_GAGE","STFIPS","FIPS_GAGE","STREAM_NAME_GAGE","STREAM_TYPE_GAGE","STREAM_OTHER_GAGE","STREAM_TRIB_GAGE")
goodMatches <- list()
for (i in rowsToView){
  STAID   <- df.Fail.NBI.Gage[i,"STAID"]
  ID    <- as.character(df.Fail.NBI.Gage[i,"ID"])
  BridgesWithin10km <- df.USbridges.water[possibleMatches[[ID]],c("ITEM8","STFIPS","ITEM3","ITEM6A","LATDD","LONGDD")]
  BridgesWithin10km[,cols] <- gageDataFrame[gageDataFrame$STAID == STAID,colsG]
  Matched <- rep(FALSE,nrow(BridgesWithin10km))
  for (j in 1:nrow(BridgesWithin10km)){
    FailEntry <-  BridgesWithin10km[j,c("STANAME","STREAM_NAME_GAGE","STREAM_TYPE_GAGE")]
    print("-----")
    print(BridgesWithin10km[j,c("STANAME","STREAM_NAME_GAGE")])
    matchData <- BridgesWithin10km[j,]
    print(matchData[,"ITEM6A"])
    PossibleMatchRows <- PerformMatch(FailEntry, matchData, "nbiGage", VERBOSE = TRUE)
    print(PossibleMatchRows)
    if (PossibleMatchRows >= 1){
      Matched[j] <- TRUE
    }
  }
  if(any(Matched)) goodMatches[[ID]] <- row.names(BridgesWithin10km[Matched,])
}

# find bridges within 10km on the right stream that are not the failed bridges
goodMatchesNotFailed <- list()
for (i in rowsToView){
  ID    <- as.character(df.Fail.NBI.Gage[i,"ID"])
  if(!is.na(df.Fail.NBI.Gage[i,"NBI_ROW"])) goodMatchesNotFailed[[ID]] <- goodMatches[[ID]][goodMatches[[ID]]!=df.Fail.NBI.Gage[i,"NBI_ROW"]]
  else goodMatchesNotFailed[[ID]] <- goodMatches[[ID]]
}

# now check against other bridges in failure database to make sure we're not being unfair
# first the other ones I've located
for (i in rowsToView){
  ID    <- as.character(df.Fail.NBI.Gage[i,"ID"])
  hasFail <- sapply(goodMatchesNotFailed[[ID]], function(s) any(!is.na(df.Fail.NBI.Gage$NBI_ROW) & df.Fail.NBI.Gage$NBI_ROW==s))
  if(any(hasFail)) goodMatchesNotFailed[[ID]] <- goodMatchesNotFailed[[ID]][!hasFail]
}
# now check all of the other failed bridges
load(file.path(dirs$DataDirFrequent,"FailDataFrame-Processed-RoadCountyStream.RData"))
PossFailMatchesStream <- list()
PossFailMatchesRoad   <- list()
NotMachedFail <- FailDataFrame[!(FailDataFrame$ID %in% df.Fail.NBI.Gage$ID),]
matchData <- df.USbridges.water[unlist(goodMatchesNotFailed),c("ITEM8","STFIPS","ITEM3","ITEM5D","ITEM7","ITEM6A","LATDD","LONGDD")]
Matched <- list()
for (j in 1:nrow(NotMachedFail)){
  FailEntry <-  NotMachedFail[j,]
  print("-----")
  print(FailEntry[j,c("LOCATION","FEAT_UND")])
  PossibleMatchRowsStream <- PerformMatch(FailEntry, matchData, "stream", VERBOSE = FALSE)
  PossibleMatchRowsRoad   <- PerformMatch(FailEntry, matchData, "road", VERBOSE = FALSE)
  PossibleMatchRowsRte    <- PerformMatch(FailEntry, matchData, "route", VERBOSE = FALSE)
  print(PossibleMatchRows)
  if (any(as.numeric(c(PossibleMatchRowsStream, PossibleMatchRowsRoad, PossibleMatchRowsRte)) > 0)){
    Matched[[as.character(FailEntry$ID)]] <- c(PossibleMatchRowsStream, PossibleMatchRowsRoad, PossibleMatchRowsRte)[as.numeric(c(PossibleMatchRowsStream, PossibleMatchRowsRoad, PossibleMatchRowsRte)) > 0 ])
  }
}
# then will figure out the largest return-period event the bridge has survived
# then need to look into survival analysis with censoring: the failed bridges give us a max failure return period, the non-failed bridges
# give us a minimum failure return period

