# Analysis of Daymet-VIC and NLDAS timeseries against USGS and internally
# Written by Madeleine Flint on 2015-08-25

require(stats)
require(nsRFA)
require(ggplot2)
require(plyr)
require(hydroGOF)

source(file.path(dirsGit$Scripts,"ks_test_approx_MF.R"))

load(file.path(dirsGit$Data, "df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirs$DataDirFrequent,"20150729_Discharge_39Gages.RData"))
rm(ls.Discharge.Inst,ls.Discharge.Dist)
load(file.path(dirs$DataDirAnalysis,"20151203_GageRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirs$DataDirAnalysis,"20151203_GageRoutingVICDaymet.RData"))
load(file.path(dirs$DataDirAnalysis,"20151203_BridgeRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirs$DataDirFrequent,"20151203_BridgeRoutingVICDaymet.RData"))

IDsToAnalyzeVIC    <- as.character(IDsToView[df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"DATE_FAIL_EST_USGS"] >= as.Date("1980-01-01")])
gaugesToAnalyzeVIC <- unique(names(ls.Discharge.All)[names(ls.Discharge.All) %in% df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"STAID"]])
alphas = c(0.01, 0.05, 0.1)
nIDs <- length(IDsToAnalyzeVIC)
######### (C1) How well-correlated/sufficient are Daymet-VIC gage and USGS? (individual) -------------------------------------
######### (C3) Are the gauge distributions produced by Daymet-VIC consistent with the USGS distribution (empirical)? (individual) -------
DaymetVICperformance     <- list()
DaymetVICanPkPerformance <- list()
for(i in gaugesToAnalyzeVIC){
  # daily data
  tempDaymetVIC <- ls.GageRouted[[i]]$day[c("Date","RUNOFF_CFS")]
  tempDaymetVIC$type <- "DaymetVIC"
  tempUSGS      <- ls.Discharge.All[[i]][,c("dates","val")]
  tempUSGS$type <- "USGS"
  colnames(tempUSGS)[1:2] <- c("Date","RUNOFF_CFS")
  df <- join(tempDaymetVIC,tempUSGS,type="full")
  df <- df[order(as.numeric(df$Date)),]
  
  
  DaymetVICperformance[[i]]["PearsonRho"] <- list(cor.test(df[duplicated(df$Date,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"],
                                                           df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"],
                                                           method = "pearson"))
  DaymetVICperformance[[i]]["KendallTau"] <- list(cor.test(df[duplicated(df$Date,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"],
                                                      df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"],
                                                      method = "kendall"))
  DaymetVICperformance[[i]]["Nash"]       <- NSE(df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"], 
                                                 df[duplicated(df$Date,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"])
  DaymetVICperformance[[i]]["RMSE"]       <-  RMSE(df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"], 
                                                   df[duplicated(df$Date,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"], na.rm = TRUE)
  DaymetVICperformance[[i]]["MAE"]        <- MAE(df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"],
                                                 df[duplicated(df$Date,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"], na.rm = TRUE)
  DaymetVICperformance[[i]]["R2"]         <- R2(df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"], 
                                                df[duplicated(df$Date,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"], na.rm = TRUE)
  DaymetVICperformance[[i]]["KSall"]       <- list(ks.test(tempDaymetVIC$RUNOFF_CFS,tempUSGS$RUNOFF_CFS))
  DaymetVICperformance[[i]]["KSsameDates"] <- list(ks.test(df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"], 
                                                           df[duplicated(df$Date,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"]))
  
   # ggplot(df) + stat_ecdf(aes(x=RUNOFF_CFS,color=type)) + scale_x_log10()
   
   # annual peaks (from daily) data
   tempDaymetVIC <- data.frame(RUNOFF_CFS = ls.GageRoutedAnnualMax[[i]])
   tempDaymetVIC$YEAR = c(1980:2013)
   tempDaymetVIC$type <- "DaymetVIC"
   # need to get peaks from daily data, not actual peaks file
   years <- unique(format.Date(tempUSGS$Date,"%Y"))
   tempUSGSanDayPk               <- data.frame(YEAR = as.integer(years))
   tempUSGSanDayPk$RUNOFF_CFS    <- sapply(years, function(y) ifelse(sum(is.na(tempUSGS[format.Date(tempUSGS$Date,"%Y")==y,"RUNOFF_CFS"]))<20,
                                                                     max(tempUSGS[format.Date(tempUSGS$Date,"%Y")==y,"RUNOFF_CFS"],na.rm=TRUE),
                                                                     NA))
   tempUSGSanDayPk <- tempUSGSanDayPk[!is.na(tempUSGSanDayPk$RUNOFF_CFS),]
   if(any(is.infinite(tempUSGSanDayPk$RUNOFF_CFS))){
     warning(paste("Infinite max runoff value for i =",i))
     break
   }
   tempUSGSanDayPk$type          <- "USGS"
   df <- join(tempDaymetVIC,tempUSGSanDayPk,type="full")
   df <- df[order(as.numeric(df$YEAR)),]
   
   # ggplot(data=df) + geom_line(aes(x=YEAR,y=RUNOFF_CFS,color=type))
   
   DaymetVICanPkPerformance[[i]]["KSall"]       <- list(ks.test(tempDaymetVIC$RUNOFF_CFS,tempUSGSanDayPk$RUNOFF_CFS))
   DaymetVICanPkPerformance[[i]]["KSsameDates"] <- list(ks.test(df[duplicated(df$YEAR) & df$type=="USGS","RUNOFF_CFS"], 
                                                            df[duplicated(df$YEAR,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"]))
   
   # ggplot(df) + stat_ecdf(aes(x=RUNOFF_CFS,color=type)) + scale_x_log10()
   nDup <- nrow(df[duplicated(df$YEAR,fromLast = TRUE),])
   anPusgs <- sort(df[duplicated(df$YEAR) & df$type=="USGS","RUNOFF_CFS"])
   anPvic  <- df[duplicated(df$YEAR,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"]
   
   ecdfUSGS <- ecdf(anPusgs)
   ecdfVIC  <- ecdf(anPvic)
   x  <- seq(0,max(max(anPvic),max(anPusgs)),by=0.1)
   Dn <-  max(abs(ecdfUSGS(x)-ecdfVIC(x)))
   
   DaymetVICanPkPerformance[[i]][["KSpassAlphasSameYears"]]   <- sapply(alphas, 
                                                             function(alpha) ks_test_approx_MF(Dn,nDup,alpha,TRUE,nDup,FALSE),
                                                             simplify = FALSE,
                                                             USE.NAMES = TRUE
   )
   
   ecdfUSGS <- ecdf(tempUSGSanDayPk$RUNOFF_CFS)
   ecdfVIC  <- ecdf(tempDaymetVIC$RUNOFF_CFS)
   x  <- seq(0,max(max(tempUSGSanDayPk$RUNOFF_CFS,na.rm=TRUE),max(tempDaymetVIC$RUNOFF_CFS,na.rm=TRUE)),by=0.1)
   Dn <-  max(abs(ecdfUSGS(x)-ecdfVIC(x)))
   DaymetVICanPkPerformance[[i]][["KSpassAlphasAllData"]]   <- sapply(alphas, 
                                                                        function(alpha) ks_test_approx_MF(Dn,length(years),alpha,TRUE,nrow(tempDaymetVIC),FALSE),
                                                                        simplify = FALSE,
                                                                        USE.NAMES = TRUE
   )
}

# Moet advises to ignore locations with NSE below 0 (presumably for monthly)
DaymetVICanPkPerformance <- DaymetVICanPkPerformance[names(DaymetVICanPkPerformance) %in% gaugesToAnalyzeVIC]
DaymetVICperformance <- DaymetVICperformance[names(DaymetVICperformance) %in% gaugesToAnalyzeVIC]

NSE <- unlist(lapply(DaymetVICperformance,"[[","Nash"))
sum(NSE < -1, na.rm=TRUE) 
# 19 out of 25 have NSE <=0 at the daily scale. 6 between -1 and 0; 5 > 0
hist(NSE[NSE>-1])

RMSE <- unlist(lapply(DaymetVICperformance,"[[","RMSE"))
hist(RMSE)
which(RMSE>500)

MAE <- unlist(lapply(DaymetVICperformance,"[[","MAE"))
hist(MAE)
which(MAE>300)
# 10312000 01031500 02482550 
# 2       14       20

# 14 have K-S below 0.2 - p
KSsameDates <- lapply(DaymetVICperformance,"[[","KSsameDates")
KSsameDates <- unlist(lapply(KSsameDates,"[[","statistic"))
hist(KSsameDates)
KSpsameDates <- lapply(DaymetVICperformance,"[[","KSsameDates")
KSpsameDates <- sapply(names(KSpsameDates), function(i) KSpsameDates[[i]]$p.value)
hist(KSpsameDates)

# for the daily-derived annual peaks data, have 4 around or below 0.2, many higher (although appears to happen early on and might just be due to
# not having much overlapping data)
KSsameDates <- lapply(DaymetVICanPkPerformance,"[[","KSsameDates")
KSsameDates <- unlist(lapply(KSsameDates,"[[","statistic"))
hist(KSsameDates)

# looking at my version of K-S with duplicate data
KSsameDates <- lapply(DaymetVICanPkPerformance,"[[","KSpassAlphasSameYears")
KSsameDates <- unlist(lapply(KSsameDates,"[[",2))
sum(KSsameDates,na.rm=TRUE)
# 6 pass the two-sample K-S test with alpha = 0.01, 5 with alpha = 0.05

# looking at my version of K-S using all available annual peak data
KS <- lapply(DaymetVICanPkPerformance,"[[","KSpassAlphasAllData")
KS <- unlist(lapply(KS,"[[",1))
sum(KS,na.rm=TRUE)
KS & # only 6 pass the two-sample K-S test using all available data from USGS and VIC, 3 at 0.05

savefile <- paste(gsub("-","",Sys.Date()),"USGS_DaymetVIC_CorrelationAndTestStatistics.RData",sep="_")
save(DaymetVICperformance,DaymetVICanPkPerformance,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

which(KS==TRUE & NSE !=1 & NSE > 0)
# 10312000 01049500 07030050 02482550 04273800 
# 2       15       18       20       23 

which(KS==FALSE & NSE < 0)
# 01624800 04216500 02021500 01365000 02487500 07277700 06927000 07069500 01387450 01349810 01387400 01031500 01090800 04234000 01648000 06891500 01596500 07075000 01349711 
# 1        3        4        5        6        7        8        9       10       11       13       14       16       17       19       21       22       24       25 

which(KS==FALSE & NSE < -1)
# 01624800 04216500 02021500 02487500 07277700 06927000 01387450 01349810 01387400 01090800 04234000 06891500 01596500 07075000 01349711 
# 1        3        4        6        7        8       10       11       13       16       17       21       22       24       25 

######### (C2) How well-correlated/sufficient are Daymet-VIC bridge and USGS? (individual) -------------------------------------
######### (C4) Are the bridge distributions produced by Daymet-VIC consistent with the USGS distribution (empirical)? (individual) ------
DaymetVICbridgePerformance     <- list()
DaymetVICbridgeAnPkPerformance <- list()
for(i in 1:nIDs){
  ID    <- IDsToAnalyzeVIC[i]
  STAID <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"STAID"]
  name  <- paste(STAID,ID,sep="-")
  tempDaymetVIC <- ls.BridgeRouted[[ID]]$day[c("Date","RUNOFF_CFS")]
  tempDaymetVIC$type <- "DaymetVIC"
  tempUSGS      <- ls.Discharge.All[[STAID]][,c("dates","val")]
  tempUSGS$type <- "USGS"
  colnames(tempUSGS)[1:2] <- c("Date","RUNOFF_CFS")
  df <- join(tempDaymetVIC,tempUSGS,type="full")
  df <- df[order(as.numeric(df$Date)),]
  
  # ggplot(data=df) + geom_line(aes(x=Date,y=RUNOFF_CFS,color=type))
  
  DaymetVICbridgePerformance[[name]]["PearsonRho"] <- list(cor.test(df[duplicated(df$Date,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"],
                                                           df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"],
                                                           method = "pearson"))
  DaymetVICbridgePerformance[[name]]["KendallTau"] <- list(cor.test(df[duplicated(df$Date,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"],
                                                           df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"],
                                                           method = "kendall"))
  DaymetVICbridgePerformance[[name]]["Nash"]       <- NSE(df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"], 
                                                 df[duplicated(df$Date,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"])
  DaymetVICbridgePerformance[[name]]["RMSE"]       <-  RMSE(df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"], 
                                                   df[duplicated(df$Date,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"])
  DaymetVICbridgePerformance[[name]]["MAE"]        <- MAE(df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"],
                                                 df[duplicated(df$Date,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"])
  DaymetVICbridgePerformance[[name]]["R2"]         <- R2(df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"], 
                                                df[duplicated(df$Date,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"])
  DaymetVICbridgePerformance[[name]]["KSall"]       <- list(ks.test(tempDaymetVIC$RUNOFF_CFS,tempUSGS$RUNOFF_CFS))
  DaymetVICbridgePerformance[[name]]["KSsameDates"] <- list(ks.test(df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"], 
                                                           df[duplicated(df$Date,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"]))
  
  # ggplot(df) + stat_ecdf(aes(x=RUNOFF_CFS,color=type)) + scale_x_log10()
  
  # annual peaks (from daily) data
  tempDaymetVIC <- data.frame(RUNOFF_CFS = ls.BridgeRoutedAnnualMax[[ID]])
  tempDaymetVIC$YEAR = c(1980:2013)
  tempDaymetVIC$type <- "DaymetVIC"
  # need to get peaks from daily data, not actual peaks file
  years <- unique(format.Date(tempUSGS$Date,"%Y"))
  tempUSGSanDayPk               <- data.frame(YEAR = as.integer(years))
  tempUSGSanDayPk$RUNOFF_CFS    <- sapply(years, function(y) ifelse(sum(is.na(tempUSGS[format.Date(tempUSGS$Date,"%Y")==y,"RUNOFF_CFS"]))<20,
                                                                    max(tempUSGS[format.Date(tempUSGS$Date,"%Y")==y,"RUNOFF_CFS"],na.rm=TRUE),
                                                                    NA))
  tempUSGSanDayPk <- tempUSGSanDayPk[!is.na(tempUSGSanDayPk$RUNOFF_CFS),]
  if(any(is.infinite(tempUSGSanDayPk$RUNOFF_CFS))){
    warning(paste("Infinite max runoff value for i =",i))
    break
  }
  tempUSGSanDayPk$type          <- "USGS"
  df <- join(tempDaymetVIC,tempUSGSanDayPk,type="full")
  df <- df[order(as.numeric(df$YEAR)),]
  
  # ggplot(data=df) + geom_line(aes(x=YEAR,y=RUNOFF_CFS,color=type))
  
  DaymetVICbridgeAnPkPerformance[[name]]["KSall"]       <- list(ks.test(tempDaymetVIC$RUNOFF_CFS,tempUSGSanDayPk$RUNOFF_CFS))
  DaymetVICbridgeAnPkPerformance[[name]]["KSsameDates"] <- list(ks.test(df[duplicated(df$YEAR) & df$type=="USGS","RUNOFF_CFS"], 
                                                               df[duplicated(df$YEAR,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"]))
  
  # ggplot(df) + stat_ecdf(aes(x=RUNOFF_CFS,color=type)) + scale_x_log10()
  nDup <- nrow(df[duplicated(df$YEAR,fromLast = TRUE),])
  anPusgs <- sort(df[duplicated(df$YEAR) & df$type=="USGS","RUNOFF_CFS"])
  anPvic  <- df[duplicated(df$YEAR,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"]
  
  ecdfUSGS <- ecdf(anPusgs)
  ecdfVIC  <- ecdf(anPvic)
  x  <- seq(0,max(max(anPvic),max(anPusgs)),by=0.1)
  Dn <-  max(abs(ecdfUSGS(x)-ecdfVIC(x)))
  
  DaymetVICbridgeAnPkPerformance[[name]][["KSpassAlphasSameYears"]]   <- sapply(alphas, 
                                                                       function(alpha) ks_test_approx_MF(Dn,nDup,alpha,TRUE,nDup,FALSE),
                                                                       simplify = FALSE,
                                                                       USE.NAMES = TRUE
  )
  
  ecdfUSGS <- ecdf(tempUSGSanDayPk$RUNOFF_CFS)
  ecdfVIC  <- ecdf(tempDaymetVIC$RUNOFF_CFS)
  x  <- seq(0,max(max(tempUSGSanDayPk$RUNOFF_CFS,na.rm=TRUE),max(tempDaymetVIC$RUNOFF_CFS,na.rm=TRUE)),by=0.1)
  Dn <-  max(abs(ecdfUSGS(x)-ecdfVIC(x)))
  DaymetVICbridgeAnPkPerformance[[name]][["KSpassAlphasAllData"]]   <- sapply(alphas, 
                                                                     function(alpha) ks_test_approx_MF(Dn,length(years),alpha,TRUE,nrow(tempDaymetVIC),FALSE),
                                                                     simplify = FALSE,
                                                                     USE.NAMES = TRUE
  )
}

# Moet advises to ignore locations with NSE below 0 (presumably for monthly)
NSE <- unlist(lapply(DaymetVICbridgePerformance,"[[","Nash"))
sum(NSE<=0, na.rm=TRUE) 
# 21 out of 26 have NSE <=0 at the daily scale. 5 between -1 and 0; 5 > 0
hist(NSE[NSE>-1])
# 12 have K-S below 0.2 - p values all negative?
KSsameDates <- lapply(DaymetVICbridgePerformance,"[[","KSsameDates")
KSsameDates <- unlist(lapply(KSsameDates,"[[","statistic"))
hist(KSsameDates)
KSpsameDates <- lapply(DaymetVICbridgePerformance,"[[","KSsameDates")
KSpsameDates <- sapply(names(KSpsameDates), function(i) KSpsameDates[[i]]$p.value)
hist(KSpsameDates)

# for the daily-derived annual peaks data, have 3 around or below 0.2, many higher (although appears to happen early on and might just be due to
# not having much overlapping data)
KSsameDates <- lapply(DaymetVICbridgeAnPkPerformance,"[[","KSsameDates")
KSsameDates <- unlist(lapply(KSsameDates,"[[","statistic"))
hist(KSsameDates)

# looking at my version of K-S with duplicate data
KSsameDates <- lapply(DaymetVICbridgeAnPkPerformance,"[[","KSpassAlphasSameYears")
KSsameDates <- unlist(lapply(KSsameDates,"[[",1))
sum(KSsameDates,na.rm=TRUE)
# 9 pass the two-sample K-S test with alpha = 0.01, 7 with alpha = 0.05 - THIS IS BETTER THAN THE GAUGES

# looking at my version of K-S using all available annual peak data
KS <- lapply(DaymetVICbridgeAnPkPerformance,"[[","KSpassAlphasAllData")
KS <- unlist(lapply(KS,"[[",1))
sum(KS,na.rm=TRUE)
# 6 pass the two-sample K-S test using all available data from USGS and VIC, 4 at 0.05

savefile <- paste(gsub("-","",Sys.Date()),"USGS_DaymetVICbridge_CorrelationAndTestStatistics.RData",sep="_")
save(DaymetVICbridgePerformance ,DaymetVICbridgeAnPkPerformance,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

which(KS==TRUE & NSE !=1 & NSE > 0)
# 10312000-809 07030050-1004 04273800-1550 - THESE ARE NOT THE SAME ONES THAT FIT WELL WITH THE GAGE!
# 7            10            17 

which(KS==FALSE & NSE < 0)
# 01662800-136  01624800-195  01387400-203  01031500-476  01090800-799  04234000-864  04216500-901 02021500-1044 01648000-1272 01662800-1390 06891500-1442 01596500-1482 01365000-1631 02487500-1673 
# 1             2             3             4             6             8             9            11            12            14            15            16            18            19 
# 07277700-1675 06927000-1740 07069500-3528 07075000-3535 01387450-3610 01349810-3629 
# 20            21            22            23            24            26 

which(KS==FALSE & NSE < -1)
# 01387400-203  01090800-799  04234000-864  04216500-901 02021500-1044 01648000-1272 01662800-1390 06891500-1442 01596500-1482 01365000-1631 02487500-1673 07277700-1675 06927000-1740 07069500-3528 07075000-3535 01349810-3629 
# 3             6             8             9            11            12            14            15            16            18            19            20            21            22            23            26

########  (D1) How well-correlated are Daymet-VIC bridge/gage? (individual) ####### -------------------------
########  (D2) Internal to Daymet-VIC, how well do runoff distributions at the gauge (empirical and fitted) ------
#              represent distributions at the bridge? (individual)
DaymetVICbridgeGagePerformance      <- list()
DaymetVICbridgeGageAnPkPerformance  <- list()
for(i in 1:nIDs){
  ID    <- IDsToAnalyzeVIC[i]
  STAID <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"STAID"]
  name  <- paste(STAID,ID,sep="-")
  # daily data
  tempDaymetVICg <- ls.GageRouted[[STAID]]$day[c("Date","RUNOFF_CFS")]
  tempDaymetVICg$type <- "gauge"
  tempDaymetVICb <- ls.BridgeRouted[[ID]]$day[c("Date","RUNOFF_CFS")]
  tempDaymetVICb$type <- "bridge"
  df <- join(tempDaymetVICg,tempDaymetVICb,type="full")
  df <- df[order(as.numeric(df$Date)),]
  
  # ggplot(data=df) + geom_line(aes(x=Date,y=RUNOFF_CFS,color=type))
  
  DaymetVICbridgeGagePerformance [[name]]["PearsonRho"] <- list(cor.test(df[df$type=="gauge","RUNOFF_CFS"],
                                                           df[df$type=="bridge","RUNOFF_CFS"],
                                                           method = "pearson"))
  DaymetVICbridgeGagePerformance [[name]]["KendallTau"] <- list(cor.test(df[df$type=="gauge","RUNOFF_CFS"],
                                                                         df[df$type=="bridge","RUNOFF_CFS"],
                                                           method = "kendall"))
  DaymetVICbridgeGagePerformance [[name]]["Nash"]       <- NSE(df[df$type=="gauge","RUNOFF_CFS"],
                                                               df[df$type=="bridge","RUNOFF_CFS"])
  DaymetVICbridgeGagePerformance [[name]]["RMSE"]       <-  RMSE(df[df$type=="gauge","RUNOFF_CFS"],
                                                                 df[df$type=="bridge","RUNOFF_CFS"])
  DaymetVICbridgeGagePerformance [[name]]["MAE"]        <- MAE(df[df$type=="gauge","RUNOFF_CFS"],
                                                               df[df$type=="bridge","RUNOFF_CFS"])
  DaymetVICbridgeGagePerformance [[name]]["R2"]         <- R2(df[df$type=="gauge","RUNOFF_CFS"],
                                                              df[df$type=="bridge","RUNOFF_CFS"])
  DaymetVICbridgeGagePerformance [[name]]["KSall"]       <- list(ks.test(tempDaymetVICg$RUNOFF_CFS,tempDaymetVICb$RUNOFF_CFS))
  
  # ggplot(df) + stat_ecdf(aes(x=RUNOFF_CFS,color=type)) + scale_x_log10()
  
  # annual peaks (from daily) data
  tempDaymetVICg <- data.frame(RUNOFF_CFS = ls.GageRoutedAnnualMax[[STAID]])
  tempDaymetVICg$YEAR = c(1980:2013)
  tempDaymetVICg$type <- "gauge"
  tempDaymetVICb <- data.frame(RUNOFF_CFS = ls.BridgeRoutedAnnualMax[[ID]])
  tempDaymetVICb$YEAR = c(1980:2013)
  tempDaymetVICb$type <- "bridge"
 
  df <- join(tempDaymetVICg,tempDaymetVICb,type="full")
  df <- df[order(as.numeric(df$YEAR)),]
  
  # ggplot(data=df) + geom_line(aes(x=YEAR,y=RUNOFF_CFS,color=type))
  
  DaymetVICbridgeGageAnPkPerformance [[name]]["KSall"]       <- list(ks.test(tempDaymetVICg$RUNOFF_CFS,tempDaymetVICb$RUNOFF_CFS))
  
  # ggplot(df) + stat_ecdf(aes(x=RUNOFF_CFS,color=type)) + scale_x_log10()
  nDup <- nrow(df[duplicated(df$YEAR,fromLast = TRUE),])
  anPusgs <- sort(df[duplicated(df$YEAR) & df$type=="USGS","RUNOFF_CFS"])
  anPvic  <- df[duplicated(df$YEAR,fromLast = TRUE) & df$type=="DaymetVIC","RUNOFF_CFS"]
  
  ecdfVICg <- ecdf(tempDaymetVICg$RUNOFF_CFS)
  ecdfVICb <- ecdf(tempDaymetVICb$RUNOFF_CFS)
  x  <- seq(0,max(max(tempDaymetVICg$RUNOFF_CFS,na.rm=TRUE),max(tempDaymetVICb$RUNOFF_CFS,na.rm=TRUE)),by=0.1)
  Dn <-  max(abs(ecdfVICg(x)-ecdfVICb(x)))
  DaymetVICbridgeGageAnPkPerformance [[name]][["KSpassAlphasAllData"]]   <- sapply(alphas, 
                                                                     function(alpha) ks_test_approx_MF(Dn,nrow(tempDaymetVICg),alpha,TRUE,nrow(tempDaymetVICb),FALSE),
                                                                     simplify = FALSE,
                                                                     USE.NAMES = TRUE
  )
}

# Moet advises to ignore locations with NSE below 0 (presumably for monthly)
NSE <- unlist(lapply(DaymetVICbridgeGagePerformance,"[[","Nash"))
sum(NSE<=0, na.rm=TRUE) 
# 5 have NSE <=0 at the daily scale. 5 between -1 and 0; 5 > 0
hist(NSE[NSE>0]) # 14 are equal to 1 (same grid cell)
# 
KSsameDates <- lapply(DaymetVICbridgeGagePerformance,"[[","KSall") # all and same dates are equivalent here
KSsameDates <- unlist(lapply(KSsameDates,"[[","statistic"))
hist(KSsameDates) # 14 with zero difference (same grid cell), 5 < 0.2
KSpsameDates <- lapply(DaymetVICbridgeGagePerformance,"[[","KSall")
KSpsameDates <- sapply(names(KSpsameDates), function(i) KSpsameDates[[i]]$p.value)
hist(KSpsameDates) # 17 below 0.2, 0 above 0.8, none in-between

# 14 0, then exponential-looking distribution
KSsameDates <- lapply(DaymetVICanPkPerformance,"[[","KSall")
KSsameDates <- unlist(lapply(KSsameDates,"[[","statistic"))
hist(KSsameDates)

# looking at my version of K-S using all available annual peak data
KS <- lapply(DaymetVICbridgeGageAnPkPerformance,"[[","KSpassAlphasAllData")
KS <- unlist(lapply(KS,"[[",1))
sum(KS,na.rm=TRUE)
# 17 pass the two-sample K-S test with alpha=0.01, 16 at 0.05

which(KS==TRUE & NSE !=1 & NSE > 0)
# these are the sites where they're in different grid cells but are still getting good correlation
#  01624800-195  01049500-479  10312000-809 01648000-1272 04273800-1550 02487500-1673 07069500-3528 07075000-3535 
#             2             5             7            12            17            19            22            23

# Presumably these are where the gauge and bridge are in the same grid cell
which(KS==TRUE & NSE == 1)
# 01662800-136  01031500-476  01090800-799  04234000-864  04216500-901 07030050-1004 02482550-1289 06891500-1442 06927000-1740 
# 1             4             6             8             9            10            13            15            21 

which(KS==FALSE & NSE < 0)
# 01387400-203 01662800-1390 01596500-1482 01365000-1631 07277700-1675 
# 3           14            16            18            20 

which(KS==FALSE & NSE < -1)
# 01387400-203 01662800-1390 01596500-1482 07277700-1675 
# 3            14            16            20 

savefile <- paste(gsub("-","",Sys.Date()),"USGS_DaymetVICbridgeGage_CorrelationAndTestStatistics.RData",sep="_")
save(DaymetVICbridgeGagePerformance,DaymetVICbridgeGageAnPkPerformance,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)


## ANALYSIS FOR PLOTTING AND DETERMINATION OF "GOOD" LOCATION CHARACTERISITICS ####### ----------------

savefile <- paste("20150826","USGS_DaymetVIC_CorrelationAndTestStatistics.RData",sep="_")
load(file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
savefile <- paste("20150827","USGS_DaymetVICbridge_CorrelationAndTestStatistics.RData",sep="_")
load(file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
savefile <- paste("20150827","USGS_DaymetVICbridgeGage_CorrelationAndTestStatistics.RData",sep="_")
load(file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

# VIG-G USGS, KS(alpha=0.05)=TRUE & NSE >0 & NSE != 1
# 10312000 01049500 07030050 02482550 04273800 
# 2       15       18       20       23 

# VIB-B USGS, KS(alpha=0.05)=TRUE & NSE >0 & NSE != 1
# 10312000-809 07030050-1004 04273800-1550

# VIC-G VIC-B, KS(alpha=0.05)=TRUE & NSE!=1 & NSE > 0
# these are the sites where they're in different grid cells but are still getting good correlation
#  01624800-195  01049500-479  10312000-809 01648000-1272 04273800-1550 02487500-1673 07069500-3528 07075000-3535 
#             2             5             7            12            17            19            22            23

# VIC-G VIC-B, KS(alpha=0.05)=TRUE & NSE==1 
# Presumably these are where the gauge and bridge are in the same grid cell
# 01662800-136  01031500-476  01090800-799  04234000-864  04216500-901 07030050-1004 02482550-1289 06891500-1442 06927000-1740 
# 1             4             6             8             9            10            13            15            21 


# 10312000-809 appears in all 3 sets
# 01049500-479  NSE gage/bridge is 0.983, so pretty much the same - and gage does a good job for USGS
# 07030050-1004 have the same distribution for the VICs, so both agree with USGS
# 02482550-1289 same as previous
# 04273800-1550 NSE gage/bridge is 0.995, so pretty much the same - and gage does a good job for USGS

IDsWithGoodAgreement <- c("479","809","1004","1289","1550")
View(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,c("ID","STATE_CODE","DRAIN_SQKM")])
# in different states: "ME" "NV" "TN" "MS" "NY"

# ANALYZE THOSE THAT AGREE -------------
# Drainage area
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DRAIN_SQKM"]
#  561.3147 3800.6730 5967.8740 3478.0600  176.9175
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DRAIN_SQKM"])
# 2796.968
sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DRAIN_SQKM"])
# 2418.097
skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DRAIN_SQKM"])
# 0.05728185
hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DRAIN_SQKM"])
# 2 below 2000, 2 between 2000 and 4000, one between 4000 and 6000

# Distance to gauge
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DIST_TO_GAGE"])
# 1.237549
sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DIST_TO_GAGE"])
# 2.177208
skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DIST_TO_GAGE"])
# 1.047744
hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DIST_TO_GAGE"])
# 4 less than 1, 1 between 5 and 6 (ID 809)

# Ratio of VIC bridge Drainage / USGS drainage
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"AREA_RATIO_VIC"]
# 0.8493909 0.9092193 0.9969585 0.9639457 1.3831599
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"AREA_RATIO_VIC"]) 
# 1.020535
sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"AREA_RATIO_VIC"])
# 0.2103191
skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"AREA_RATIO_VIC"])
# 0.8835069
hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"AREA_RATIO_VIC"])
# 4 less than 1, one between 1.3 and 1.4 (ID 1550)

# Bridge NHD / USGS drainage
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"AREA_RATIO_NHD"]) 
# 0.9916281
sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"AREA_RATIO_NHD"])
# 0.07665306
skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"AREA_RATIO_NHD"])
# -0.07737589
hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"AREA_RATIO_NHD"])
# between 0.85 and 1.1, 3/5 less than 1

# Ratio of VIC-estimated bridge drainage area to actual area from NHD+
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DRAIN_AREA_DVICB_SQKM"]/df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])
# 1.024581
sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DRAIN_AREA_DVICB_SQKM"]/df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])
# 0.1495259
skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DRAIN_AREA_DVICB_SQKM"]/df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])
# 0.4524551
hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DRAIN_AREA_DVICB_SQKM"]/df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])
# pretty tight, range from 0.8 to 1.3

# Failure flow
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"Q_FAIL_D_USGS"])
# 9062
sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"Q_FAIL_D_USGS"])
# 4810
skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"Q_FAIL_D_USGS"])
# 0.0597564
hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"Q_FAIL_D_USGS"]) 
# range 2000 to 16000

# Return period of failure flow from HEC
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"T_FAIL_D_HECP_USGS"])
# 1092
sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"T_FAIL_D_HECP_USGS"]) 
# 1991, one above 4000
skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"T_FAIL_D_HECP_USGS"]) 
# 1.027714
hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"T_FAIL_D_HECP_USGS"])
# 0.6, 179, 4620, 660, 16 
# although there's only one really frequent event, whereas the majority of the other set are very frequent events

# Failure cause
table(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"FAIL_CAUS"])
# hydraulic flood  hydraulic scour 
#               1                4    

# link to hurricane
table(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement,"COMMENT_LINKED_HURRICANE"])
# NONE WERE LINKED TO HURRICANES - odds of getting a hurricane is 8/26, 31%
# random odds of no hurricane picked (binomial) is 0.159 - so within reason that it's random, but still notable

# ANALYSE THOSE THAT DON'T AGREE ---------------
# Drainage area
df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_SQKM"]
#66.6531  187.3944  231.6789  769.0482  163.1421  320.6385   62.5806  851.6961  136.8279   66.6531 1103.7210  124.6505   99.5688 1099.7300  315.3789  667.0980 3007.2100  781.4988   32.1759   12.6972  73.6560
mean(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_SQKM"])
# 484.4618
sd(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_SQKM"])
# 681.7627
skew(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_SQKM"])
# 2.391218
hist(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_SQKM"])
# one above 3000, but rest below 1500, 14 below 500

# Distance to gauge
mean(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DIST_TO_GAGE"])
# 4.010694
sd(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DIST_TO_GAGE"])
# 2.566247
skew(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DIST_TO_GAGE"])
# 0.3248395
hist(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DIST_TO_GAGE"])
# 3 below 2, 9 between 2 and 4, 2 between 4 and6, 5 between 6 and 8, 1 between 8 and 10 - MUCH LONGER THAN THOSE THAT AGREE

# VIC bridge Drainage / USGS drainage
mean(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_VIC"]) 
#  1.1625
sd(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_VIC"])
# 1.249754
skew(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_VIC"])
# 3.138912
hist(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_VIC"])
# One above 6, rest below 3 11 below 1, 8 between 1 and 2

# Bridge NHD / USGS drainage
mean(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_NHD"]) 
#  1.197701
sd(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_NHD"])
# 1.241275
skew(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_NHD"])
# 3.378514
hist(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_NHD"])
# again, one above 6, equal numbers between 0 and 1 and 1 and 2

# Ratio of VIC bridge drainage area and actual bridge drainage area from NHD+
mean(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_DVICB_SQKM"]/
       df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])
# 0.9428201
sd(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_DVICB_SQKM"]/
       df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])
#0.2073456
skew(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_DVICB_SQKM"]/
                 df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])
#  -0.6085388
hist(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_DVICB_SQKM"]/
       df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])
# looks normal-ish, range 0.2 to 1.6, bulk between 0.6 and 1.2

# Failure flow
mean(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"Q_FAIL_D_USGS"])
#  16689.1
sd(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"Q_FAIL_D_USGS"])
# 28289.99
skew(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"Q_FAIL_D_USGS"])
# 2.200596
hist(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"Q_FAIL_D_USGS"]) 
# 15 of 21 less than 9062, mean of good matches, 10 < 5000

# Return period of failure flow from HEC
mean(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"T_FAIL_D_HECP_USGS"])
#  19889
sd(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"T_FAIL_D_HECP_USGS"])
# 90029.7
skew(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"T_FAIL_D_HECP_USGS"])
# 3.948241
hist(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"T_FAIL_D_HECP_USGS"]) 
# one reall huge value is skewing results - 18 have a return period less than 250, 16 of those are less than 50, 11 of those less than 5

# Failure cause
table(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"FAIL_CAUS"])
# hydraulic  hydraulic agnes hydraulic debris  hydraulic flood  hydraulic scour 
#         2                1                1                9                8 

# Link to hurricane
table(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithGoodAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"COMMENT_LINKED_HURRICANE"])
#  None Hurricane Agnes   Hurricane Fran  Hurricane Irene Hurricane Jeanne   Hurricane Juan 
#   13                1                1                4                1                1 

# ####### ANALYSIS OF "BAD" PERFORMANCE OF VIC --------

# VIC-G USGS, KS==FALSE & NSE < 0 (19 of 25 fall into this category)
# 01624800 04216500 02021500 01365000 02487500 07277700 06927000 07069500 01387450 01349810 01387400 01031500 01090800 04234000 01648000 06891500 01596500 07075000 01349711 
# 1        3        4        5        6        7        8        9       10       11       13       14       16       17       19       21       22       24       25 
# KS==FALSE & NSE < -1
# 01624800 04216500 02021500 02487500 07277700 06927000 01387450 01349810 01387400 01090800 04234000 06891500 01596500 07075000 01349711 
# 1        3        4        6        7        8       10       11       13       16       17       21       22       24       25
# NSE<= -5 & KS==FALSE
# 06927000 01349810 07075000 
# 8       11       24
# RMSE>500 - for the last one, KS is true and NS is 0.5
# 02021500 01031500 02482550 
# 4       14       20
# MAE>300 - 
# 10312000 01031500 02482550 
# 2       14       20


# VIC-B USGS, KS==FALSE & NSE < 0 (20 of 26 of these)
# 01662800-136  01624800-195  01387400-203  01031500-476  01090800-799  04234000-864  04216500-901 02021500-1044 01648000-1272 01662800-1390 06891500-1442 01596500-1482 01365000-1631 02487500-1673 
# 1             2             3             4             6             8             9            11            12            14            15            16            18            19 
# 07277700-1675 06927000-1740 07069500-3528 07075000-3535 01387450-3610 01349810-3629 
# 20            21            22            23            24            26 
# KS==FALSE & NSE < -1)
# 01387400-203  01090800-799  04234000-864  04216500-901 02021500-1044 01648000-1272 01662800-1390 06891500-1442 01596500-1482 01365000-1631 02487500-1673 07277700-1675 06927000-1740 07069500-3528 07075000-3535 01349810-3629 
# 3             6             8             9            11            12            14            15            16            18            19            20            21            22            23            26

# VIC-G VIC-B, KS==FALSE & NSE < 0 (5 of these)
# 01387400-203 01662800-1390 01596500-1482 01365000-1631 07277700-1675 
# 3           14            16            18            20 
# KS==FALSE & NSE < -1)
# 01387400-203 01662800-1390 01596500-1482 07277700-1675 
# 3            14            16            20 


# maybe 01662800-1390 (USGS-gage NSE is -0.35), 01387400-203 (USGS-gage NSE is -1.218), definitely 07075000-3535, 01349810-3629, 02021500-1044 (high MSE and NSE = -2.8), 01031500-476 (high MAE, fails KS, NSE=-0.52)
IDsWithBadAgreement <- c("203","476","1044","3535","3629")
IDsWithNeitherGoodNorBadAgreement <- IDsToAnalyzeVIC[!(IDsToAnalyzeVIC %in% IDsWithBadAgreement) & !(IDsToAnalyzeVIC %in% IDsWithGoodAgreement)]

# ANALYZE THOSE THAT HAVE BAD AGREEMENT -------
# Drainage area
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DRAIN_SQKM"]
# 231.6789 769.0482 851.6961 781.4988  73.6560
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DRAIN_SQKM"])
# 541.5156
sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DRAIN_SQKM"])
# 360.7168
skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DRAIN_SQKM"])
# -0.326881
hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DRAIN_SQKM"])
# 3 above 600, 2 below 400

# Distance to gauge
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DIST_TO_GAGE"]
# 2.9067065 0.0286261 6.3332319 2.6637936 2.1088951
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DIST_TO_GAGE"])
# 2.808251
sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DIST_TO_GAGE"])
# 2.273294
skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DIST_TO_GAGE"])
# 0.3741764
hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DIST_TO_GAGE"])
# 1 below 2, 3 between 2 and 4, one above 6 

# Ratio of VIC bridge Drainage / USGS drainage
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"AREA_RATIO_VIC"]
# 0.1393653 1.0023231 1.2499798 0.9134892 1.0777656
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"AREA_RATIO_VIC"]) 
# 0.8765846 
sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"AREA_RATIO_VIC"])
# 0.4302796
skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"AREA_RATIO_VIC"])
# -0.8496677
hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"AREA_RATIO_VIC"])
# One below 0.2 (really bad), the rest fairly reasonable

# Bridge NHD / USGS drainage
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"AREA_RATIO_NHD"]
# 0.1412774 0.9967373 1.3412625 0.8754497 1.0989329
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"AREA_RATIO_NHD"]) 
# 0.8907319
sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"AREA_RATIO_NHD"])

skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"AREA_RATIO_NHD"])

hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"AREA_RATIO_NHD"])
# again, 1 really bad one, most are OK

# Ratio of VIC-estimated bridge drainage area to actual area from NHD+
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DRAIN_AREA_DVICB_SQKM"]/df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"]
# 0.9864654 1.0056042 0.9319427 1.0434513 0.9807383
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DRAIN_AREA_DVICB_SQKM"]/df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])

sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DRAIN_AREA_DVICB_SQKM"]/df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])

skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DRAIN_AREA_DVICB_SQKM"]/df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])

hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DRAIN_AREA_DVICB_SQKM"]/df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])
# These are all very close to 1

# Failure flow
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"Q_FAIL_D_USGS"]
# 8200 31700 41500 75000  6630
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"Q_FAIL_D_USGS"])

sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"Q_FAIL_D_USGS"])

skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"Q_FAIL_D_USGS"])

hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"Q_FAIL_D_USGS"]) 
# pretty evenly distributed across magnitudes

# Return period of failure flow from HEC
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"T_FAIL_D_HECP_USGS"]
# 0.486206   85.782329 3393.000454 1075.262855    5.420085
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"T_FAIL_D_HECP_USGS"])

sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"T_FAIL_D_HECP_USGS"]) 

skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"T_FAIL_D_HECP_USGS"]) 

hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"T_FAIL_D_HECP_USGS"])
# 3 less than 100, 2 less than 10, 1 less than 1 - variable

# Failure cause
table(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"FAIL_CAUS"])
# hydraulic flood hydraulic scour 
# 3               2 
# Slightly more flood than scour?

# link to hurricane
table(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement,"COMMENT_LINKED_HURRICANE"])
# None  Hurricane Irene  Hurricane Juan 
#    2               2               1 
# THERE'S ONLY A 14% CHANCE OF PICKING 3 HURRICANES

# ANALYZE THOSE THAT AREN'T BAD ---------
# Drainage area
df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_SQKM"]
# 66.6531  187.3944  561.3147  163.1421 3800.6730  320.6385   62.5806 5967.8740  136.8279 3478.0600   66.6531 1103.7210  124.6505  176.9175   99.5688 1099.7300  315.3789  667.0980
# 3007.2100   32.1759   12.697
mean(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_SQKM"])
# 1021.474
sd(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_SQKM"])
# 1625.276
skew(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_SQKM"])
# 1.726734
hist(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_SQKM"])
# 15 below 1000, a few very large

# Distance to gauge
mean(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DIST_TO_GAGE"])
# 3.636718
sd(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DIST_TO_GAGE"])
# 2.813248
skew(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DIST_TO_GAGE"])
# 0.3099999
hist(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DIST_TO_GAGE"])
# 6 below 2, 6 between 2 and 4, 4 between 4 and 6 and 6 and 8, 1 above 8 - DOESN'T SEEM AS CRITICAL BETWEEN BAD AND NOT-BAD AS GOOD AND NOT-GOOD

# VIC bridge Drainage / USGS drainage
mean(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_VIC"]) 
# 1.196774
sd(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_VIC"])
# 1.231695
skew(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_VIC"])
# 3.226043
hist(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_VIC"])
# 13 below 1, 6 between 1 and 2, 1 between 2 and 3, one between 6 and 7 - DOESN'T SEEM TO BE CRITICAL

# Bridge NHD / USGS drainage
mean(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_NHD"]) 
# 1.196774
sd(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_NHD"])
# 1..219598
skew(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_NHD"])
# 3.536055
hist(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"AREA_RATIO_NHD"])
# 10 < 1, 10 between 1 and 2, 1 between 6 and 7 - ALSO DOESN'T SEEM TO BE CRITICAL

# Ratio of VIC bridge drainage area and actual bridge drainage area from NHD+
mean(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_DVICB_SQKM"]/
       df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])

sd(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_DVICB_SQKM"]/
     df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])

skew(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_DVICB_SQKM"]/
       df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])

hist(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_DVICB_SQKM"]/
       df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])
# THESE HAVE A MUCH LARGER SPREAD THAN THE BAD ONES - COUNTERINTUITIVE BECAUSE IT WOULD SUGGEST THAT THE BAD ONES SHOULD PERFORM BETTER

# Failure flow
mean(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"Q_FAIL_D_USGS"])

sd(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"Q_FAIL_D_USGS"])

skew(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"Q_FAIL_D_USGS"])

hist(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"Q_FAIL_D_USGS"]) 
# one very huge one, most below 20,000 - NO DISCERNIBLE TREND

# Return period of failure flow from HEC
mean(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"T_FAIL_D_HECP_USGS"])

sd(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"T_FAIL_D_HECP_USGS"])

skew(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"T_FAIL_D_HECP_USGS"])

hist(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"T_FAIL_D_HECP_USGS"]) 
# 16 less than 100, 12 less than 10... So more frequent? NO STRONG TREND

# Failure cause
table(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"FAIL_CAUS"])
# hydraulic  hydraulic agnes hydraulic debris  hydraulic flood  hydraulic scour
# 2                1                1                7                10       
# NO TREND

# Link to hurricane
table(df.Fail.NBI.Gage[!(df.Fail.NBI.Gage$ID %in% IDsWithBadAgreement) & df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"COMMENT_LINKED_HURRICANE"])
# NONE  Hurricane Agnes   Hurricane Fran  Hurricane Irene Hurricane Jeanne 
# 16                 1                1                2                1 
# 15.6% CHANCE OF PICKING THAT FEW

# ANALYZE THOSE THAT HAVE NEITHER GOOD NOT BAD AGREEMENT -------
# Drainage area
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DRAIN_SQKM"]
# 66.6531  187.3944  163.1421  320.6385   62.5806  136.8279   66.6531 1103.7210  124.6505   99.5688 1099.7300  315.3789  667.0980 3007.2100   32.1759   12.6972
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DRAIN_SQKM"])
# 466.6325
sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DRAIN_SQKM"])
# 
skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DRAIN_SQKM"])
# 
hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DRAIN_SQKM"])
# 12 below 500 - NOTHING APPARENT

# Distance to gauge
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DIST_TO_GAGE"]
# 0.75103082 6.68431087 6.62830759 2.83673225 5.22212533 2.96813531 2.60589314 0.07654717 2.49197136 3.94698011 5.21459879 9.65963115 6.90712566 7.40633501 2.54142666 4.24217786
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DIST_TO_GAGE"])
# 4.386458
sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DIST_TO_GAGE"])
# 
skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DIST_TO_GAGE"])
# 
hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DIST_TO_GAGE"])
# Fairly evenly distributed - NOTHING APPARENT

# Ratio of VIC bridge Drainage / USGS drainage
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"AREA_RATIO_VIC"]
# 1.50771682 1.26305322 0.96098984 1.08529575 1.00582609 1.21733214 0.50247475 0.95371774 0.26536115 0.64168997 0.67748120 0.05600882 0.75805069 0.87233436 2.01016289 6.25207920
mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"AREA_RATIO_VIC"]) 
#  1.251848
sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"AREA_RATIO_VIC"])
# 1.413287
skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"AREA_RATIO_VIC"])
# 2.694883
hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"AREA_RATIO_VIC"])
# NOTHING APPARENT

# Bridge NHD / USGS drainage
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"AREA_RATIO_NHD"]

mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"AREA_RATIO_NHD"]) 
# 
sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"AREA_RATIO_NHD"])

skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"AREA_RATIO_NHD"])

hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"AREA_RATIO_NHD"])


# Ratio of VIC-estimated bridge drainage area to actual area from NHD+
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DRAIN_AREA_DVICB_SQKM"]/df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"]

mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DRAIN_AREA_DVICB_SQKM"]/df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])

sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DRAIN_AREA_DVICB_SQKM"]/df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])

skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DRAIN_AREA_DVICB_SQKM"]/df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])

hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DRAIN_AREA_DVICB_SQKM"]/df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"])


# Failure flow
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"Q_FAIL_D_USGS"]

mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"Q_FAIL_D_USGS"])

sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"Q_FAIL_D_USGS"])

skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"Q_FAIL_D_USGS"])

hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"Q_FAIL_D_USGS"]) 


# Return period of failure flow from HEC
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"T_FAIL_D_HECP_USGS"]

mean(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"T_FAIL_D_HECP_USGS"])

sd(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"T_FAIL_D_HECP_USGS"]) 

skew(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"T_FAIL_D_HECP_USGS"]) 

hist(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"T_FAIL_D_HECP_USGS"])
# 14 less than 100, 11 less than 10, 2 less than 1 - THIS DOES NOT SEEM TO BE IMPORTANT

# Failure cause
table(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"FAIL_CAUS"])
# hydraulic  hydraulic agnes hydraulic debris  hydraulic flood  hydraulic scour 
#         2             1                1                6                6 

# link to hurricane
table(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsWithNeitherGoodNorBadAgreement,"COMMENT_LINKED_HURRICANE"])
# none         Hurricane Agnes   Hurricane Fran  Hurricane Irene   Hurricane Jeanne 
# 11                        1                1                2                  1
# 21% chance of picking that many hurricanes

savefile <- paste(gsub("-","",Sys.Date()),"GoodBadNeitherIDsForTimeSeriesCorrelationPlotting.RData",sep="_")
save(IDsWithGoodAgreement,IDsWithBadAgreement,IDsWithNeitherGoodNorBadAgreement,DaymetVICanPkPerformance,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

##    NLDAS ---------------
load(file.path(dirs$DataDirAnalysis,"20151217_NLDASGageAnnualMaxes.RData"))
load(file.path(dirs$DataDirAnalysis,"20151217_NLDASGageDailyMean.RData"))

rowsToAnalyzeNLDAS  <- rownames(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView & df.Fail.NBI.Gage$YR_FAIL_EST >= "1979-01-01",])
NLDASperformance     <- list()
NLDASanPkPerformance <- list()
gaugesToAnalyzeNLDAS <- unique(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"STAID"])
kgCfs          <- 0.035315*(1/3600)
alphas <- c(0.01, 0.05, 0.1)
for(i in gaugesToAnalyzeNLDAS){
  # daily data
  tempNLDAS <- as.data.frame(ls.NLDAS.Gage[[i]][c("Date","SSRUN")])
  tempNLDAS <- tempNLDAS[!duplicated(tempNLDAS$Date),]
  tempNLDAS$RUNOFF_CFS <- kgCfs*df.Fail.NBI.Gage[df.Fail.NBI.Gage$STAID==i,"DRAIN_SQKM"][1]*1000^2*tempNLDAS$SSRUN
  tempNLDAS$type <- "NLDAS"
  tempUSGS      <- ls.Discharge.All[[i]][,c("dates","val")]
  tempUSGS$type <- "USGS"
  colnames(tempUSGS)[1:2] <- c("Date","RUNOFF_CFS")
  df <- join(tempNLDAS[,c("Date","RUNOFF_CFS","type")],tempUSGS,type="full")
  df <- df[order(as.numeric(df$Date)),]
  
  
  NLDASperformance[[i]]["PearsonRho"] <- list(cor.test(df[duplicated(df$Date, fromLast = TRUE) & df$type=="NLDAS","RUNOFF_CFS"],
                                                           df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"],
                                                           method = "pearson"))
  NLDASperformance[[i]]["KendallTau"] <- list(cor.test(df[duplicated(df$Date,fromLast = TRUE) & df$type=="NLDAS","RUNOFF_CFS"],
                                                           df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"],
                                                           method = "kendall"))
  NLDASperformance[[i]]["Nash"]       <- NSE(df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"], 
                                                 df[duplicated(df$Date,fromLast = TRUE) & df$type=="NLDAS","RUNOFF_CFS"])
  NLDASperformance[[i]]["RMSE"]       <-  RMSE(df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"], 
                                                   df[duplicated(df$Date,fromLast = TRUE) & df$type=="NLDAS","RUNOFF_CFS"],
                                               na.rm = TRUE)
  NLDASperformance[[i]]["MAE"]        <- MAE(df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"],
                                                 df[duplicated(df$Date,fromLast = TRUE) & df$type=="NLDAS","RUNOFF_CFS"],
                                             na.rm = TRUE)
  NLDASperformance[[i]]["R2"]         <- R2(df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"], 
                                                df[duplicated(df$Date,fromLast = TRUE) & df$type=="NLDAS","RUNOFF_CFS"],
                                            na.rm = TRUE)
  NLDASperformance[[i]]["KSall"]       <- list(ks.test(tempNLDAS$RUNOFF_CFS,tempUSGS$RUNOFF_CFS))
  NLDASperformance[[i]]["KSsameDates"] <- list(ks.test(df[duplicated(df$Date) & df$type=="USGS","RUNOFF_CFS"], 
                                                           df[duplicated(df$Date,fromLast = TRUE) & df$type=="NLDAS","RUNOFF_CFS"]))
  
  # annual peaks (from daily) data
  tempNLDAS      <- data.frame(SSRUN = ls.NLDAS.Gage.AnnualMax[[i]])
  tempNLDAS$RUNOFF_CFS <- kgCfs*df.Fail.NBI.Gage[df.Fail.NBI.Gage$STAID==i,"DRAIN_SQKM"][1]*1000^2*tempNLDAS$SSRUN
  tempNLDAS$type <- "NLDAS"
  tempNLDAS$YEAR <- names(ls.NLDAS.Gage.AnnualMax[[i]])
  tempNLDAS$type <- "NLDAS"
  # need to get peaks from daily data, not actual peaks file
  years <- unique(format.Date(tempUSGS$Date,"%Y"))
  tempUSGSanDayPk               <- data.frame(YEAR = as.integer(years))
  tempUSGSanDayPk$RUNOFF_CFS    <- sapply(years, function(y) ifelse(sum(is.na(tempUSGS[format.Date(tempUSGS$Date,"%Y")==y,"RUNOFF_CFS"]))<20,
                                                                    max(tempUSGS[format.Date(tempUSGS$Date,"%Y")==y,"RUNOFF_CFS"],na.rm=TRUE),
                                                                    NA))
  tempUSGSanDayPk <- tempUSGSanDayPk[!is.na(tempUSGSanDayPk$RUNOFF_CFS),]
  if(any(is.infinite(tempUSGSanDayPk$RUNOFF_CFS))){
    warning(paste("Infinite max runoff value for i =",i))
    break
  }
  tempUSGSanDayPk$type          <- "USGS"
  df <- join(tempNLDAS,tempUSGSanDayPk,type="full")
  df <- df[order(as.numeric(df$YEAR)),]
  
  NLDASanPkPerformance[[i]]["KSall"]       <- list(ks.test(tempNLDAS$RUNOFF_CFS,tempUSGSanDayPk$RUNOFF_CFS))
  NLDASanPkPerformance[[i]]["KSsameDates"] <- list(ks.test(df[duplicated(df$YEAR) & df$type=="USGS","RUNOFF_CFS"], 
                                                               df[duplicated(df$YEAR,fromLast = TRUE) & df$type=="NLDAS","RUNOFF_CFS"]))
  
  nDup <- nrow(df[duplicated(df$YEAR,fromLast = TRUE),])
  anPusgs <- sort(df[duplicated(df$YEAR) & df$type=="USGS","RUNOFF_CFS"])
  anPNLDAS  <- df[duplicated(df$YEAR,fromLast = TRUE) & df$type=="NLDAS","RUNOFF_CFS"]
  
  ecdfUSGS <- ecdf(anPusgs)
  ecdfNLDAS  <- ecdf(anPNLDAS)
  x  <- seq(0,max(max(anPNLDAS),max(anPusgs)),by=0.1)
  Dn <-  max(abs(ecdfUSGS(x)-ecdfNLDAS(x)))
  
  NLDASanPkPerformance[[i]][["KSpassAlphasSameYears"]]   <- sapply(alphas, 
                                                                       function(alpha) ks_test_approx_MF(Dn,nDup,alpha,TRUE,nDup,FALSE),
                                                                       simplify = FALSE,
                                                                       USE.NAMES = TRUE
  )
  
  ecdfUSGS <- ecdf(tempUSGSanDayPk$RUNOFF_CFS)
  ecdfNLDAS  <- ecdf(tempNLDAS$RUNOFF_CFS)
  x  <- seq(0,max(max(tempUSGSanDayPk$RUNOFF_CFS,na.rm=TRUE),max(tempNLDAS$RUNOFF_CFS,na.rm=TRUE)),by=0.1)
  Dn <-  max(abs(ecdfUSGS(x)-ecdfNLDAS(x)))
  NLDASanPkPerformance[[i]][["KSpassAlphasAllData"]]   <- sapply(alphas, 
                                                                     function(alpha) ks_test_approx_MF(Dn,length(years),alpha,TRUE,nrow(tempNLDAS),FALSE),
                                                                     simplify = FALSE,
                                                                     USE.NAMES = TRUE
  )
}

savefile <- paste(gsub("-","",Sys.Date()),"USGS_NLDASgage_CorrelationAndTestStatistics.RData",sep="_")
save(NLDASperformance ,NLDASanPkPerformance,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

# look at results
NSE  <- unlist(lapply(NLDASperformance,"[[","Nash"))
Rho  <- sapply(names(NLDASperformance), function(i) NLDASperformance[[i]][["PearsonRho"]]$estimate)
Tau  <- sapply(names(NLDASperformance), function(i) NLDASperformance[[i]][["KendallTau"]]$estimate)
MAE  <- unlist(lapply(NLDASperformance,"[[","MAE"))
RMSE <- unlist(lapply(NLDASperformance,"[[","RMSE"))
R2   <- unlist(lapply(NLDASperformance,"[[","R2"))
KSdailyAll       <- sapply(names(NLDASperformance), function(i) NLDASperformance[[i]][["KSall"]]$statistic)
KSdailySameDates <- sapply(names(NLDASperformance), function(i) NLDASperformance[[i]][["KSsameDates"]]$statistic)  
KSanMaxAll       <- sapply(names(NLDASanPkPerformance), function(i) NLDASanPkPerformance[[i]][["KSall"]]$statistic)
KSanMaxSameDates <- sapply(names(NLDASanPkPerformance), function(i) NLDASanPkPerformance[[i]][["KSsameDates"]]$statistic)  
