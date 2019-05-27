# Data exploration
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
source(file.path(dirsGit$ScriptsPlotDir,"SetupEncoding.R"))

BridgesDataFrame <- df.Fail.NBI.Gage[rowsToView,]
rm(df.Fail.NBI.Gage)

BridgesDataFrame$FAIL_CAUS_CODE  <- as.character(BridgesDataFrame$FAIL_CAUS_CODE)
BridgesDataFrame$FAIL_CAUS_CODE  <- factor(BridgesDataFrame$FAIL_CAUS_CODE, levels=c("flood","scour","hurricane","other hydraulic"), labels = labelsP$Fail)

BridgesDataFrame$BOOL_KNOWN_FAIL_DATE <- factor(BridgesDataFrame$BOOL_HAS_FAIL_DATE, levels = c(TRUE, FALSE), labels = labelsP$Date)

BridgesDataFrame$BOOL_WAS_HURRICANE                                 <- nchar(BridgesDataFrame$COMMENT_LINKED_HURRICANE)!=0
BridgesDataFrame$HURRICANE                                     <- labelsP$Hurr[2]
# --------------------------------
Checks <- c("two.sided","less","greater")
# general
length(unique(BridgesDataFrame$STAID)) # 34
sum(BridgesDataFrame$BOOL_HAS_FAIL_DATE) # 13
sum(BridgesDataFrame$BOOL_WAS_HURRICANE) # 14
sum(BridgesDataFrame$FAIL_CAUS_CODE=="OTHER") # 5
sum(BridgesDataFrame$FAIL_CAUS_CODE=="SCOUR") # 16
sum(BridgesDataFrame$FAIL_CAUS_CODE=="FLOOD") # 13
table(BridgesDataFrame$STATE)
sum(abs(BridgesDataFrame$AREA_RATIO_NHD-1)>0.5) # 4
mean(BridgesDataFrame$AREA_RATIO_NHD[abs(BridgesDataFrame$AREA_RATIO_NHD-1)<0.5]) # 0.98
sd(BridgesDataFrame$AREA_RATIO_NHD[abs(BridgesDataFrame$AREA_RATIO_NHD-1)<0.5]) # 0.19
sum(BridgesDataFrame$DIST_TO_GAGE<5) # 25
range(BridgesDataFrame$DRAIN_SQKM) # 13 - 8140
as.Date(range(BridgesDataFrame$YR_BLT_EST,na.rm=TRUE),"1970-01-01")
BridgesDataFrame$FAIL_Q_COMMENT
sum(BridgesDataFrame$DRAIN_SQKM < 100) # 8
sum(BridgesDataFrame$DRAIN_SQKM > 1000)

# Daily mean-derived HEC (of 35) ---------------------
sum(round(BridgesDataFrame$T_FAIL_D_HECD_USGS)>=100) # 8
sum(round(BridgesDataFrame$T_FAIL_D_HECD_USGS)>=50) # 15
sum(round(BridgesDataFrame$T_FAIL_D_HECD_USGS)>=500) # 0
8/35 # 22% 100-year fail

sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM<100,"T_FAIL_D_HECD_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM>=100,"T_FAIL_D_HECD_USGS"],
                                   alternative = check),
       USE.NAMES = TRUE,
       simplify = TRUE)
sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM>1000,"T_FAIL_D_HECD_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM<=1000,"T_FAIL_D_HECD_USGS"],
                                   alternative = check),
       USE.NAMES = TRUE,
       simplify = TRUE)
sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$BOOL_REGULATION == TRUE,"T_FAIL_D_HECD_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$BOOL_REGULATION == FALSE,"T_FAIL_D_HECD_USGS"],
                                   alternative = check, eexact = TRUE),
       USE.NAMES = TRUE,
       simplify = TRUE)
sum(round(BridgesDataFrame$T_MAX_D_HECD_USGS)>=100) # 15
sum(round(BridgesDataFrame$T_MAX_D_HECD_USGS)>=50) # 29
sum(round(BridgesDataFrame$T_MAX_D_HECD_USGS)>=500) # 0 
max(BridgesDataFrame$T_MAX_D_HECD_USGS,na.rm = TRUE) # 162
15/35 # 43% 100-year experienced

sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM<100,"T_MAX_D_HECD_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM>=100,"T_MAX_D_HECD_USGS"],
                                   alternative = check),
       USE.NAMES = TRUE,
       simplify = TRUE)
sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM>1000,"T_MAX_D_HECD_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM<=1000,"T_MAX_D_HECD_USGS"],
                                   alternative = check),
       USE.NAMES = TRUE,
       simplify = TRUE)
sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$BOOL_REGULATION == TRUE,"T_MAX_D_HECD_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$BOOL_REGULATION == FALSE,"T_MAX_D_HECD_USGS"],
                                   alternative = check, exact = TRUE),
       USE.NAMES = TRUE,
       simplify = TRUE)

sum(BridgesDataFrame$Q_FAIL_D_USGS == BridgesDataFrame$Q_MAX_D_USGS, na.rm = TRUE) # 17
sum(BridgesDataFrame$Q_FAIL_D_USGS == BridgesDataFrame$Q_MAXPREFAIL_D_USGS, na.rm = TRUE) # 20
20/35 # 57% up and to time of failure is max
sum(BridgesDataFrame$Q_FAIL_D_USGS == BridgesDataFrame$Q_MAXPREFAIL_D_USGS & BridgesDataFrame$BOOL_HAS_FAIL_DATE == TRUE, na.rm = TRUE) # 8
sum(BridgesDataFrame$Q_FAIL_D_USGS == BridgesDataFrame$Q_MAX_D_USGS & BridgesDataFrame$BOOL_HAS_FAIL_DATE == TRUE, na.rm = TRUE) # 8
sum(BridgesDataFrame$Q_FAIL_D_USGS == BridgesDataFrame$Q_MAX_D_USGS & BridgesDataFrame$BOOL_WAS_HURRICANE == TRUE, na.rm = TRUE) # 10
sum(BridgesDataFrame$Q_FAIL_D_USGS == BridgesDataFrame$Q_MAXPREFAIL_D_USGS & BridgesDataFrame$BOOL_WAS_HURRICANE == TRUE, na.rm = TRUE) # 12

# Inst/peak-derived HEC - of  -----------------
sum(!is.na(BridgesDataFrame$FAIL_Q_INST)) #
sum(!is.na(BridgesDataFrame$Q_FAIL_P_USGS)) # 23
sum(!is.na(BridgesDataFrame$Q_FAIL_IP_USGS)) # 31

sum(round(BridgesDataFrame$T_FAIL_IP_HECP_USGS)>=100, na.rm = TRUE) # 16
sum(round(BridgesDataFrame$T_FAIL_IP_HECP_USGS)>=50, na.rm = TRUE) # 22
sum(round(BridgesDataFrame$T_FAIL_IP_HECP_USGS)>=500, na.rm = TRUE) # 4
16/31 # 52%

sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM<100,"T_FAIL_IP_HECP_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM>=100,"T_FAIL_IP_HECP_USGS"],
                                   alternative = check),
       USE.NAMES = TRUE,
       simplify = TRUE)
sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM>1000,"T_FAIL_IP_HECP_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM<=1000,"T_FAIL_IP_HECP_USGS"],
                                   alternative = check),
       USE.NAMES = TRUE,
       simplify = TRUE)
sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$BOOL_REGULATION == TRUE,"T_FAIL_IP_HECP_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$BOOL_REGULATION == FALSE,"T_FAIL_IP_HECP_USGS"],
                                   alternative = check, eexact = TRUE),
       USE.NAMES = TRUE,
       simplify = TRUE)

sum(round(BridgesDataFrame$T_MAX_P_HECP_USGS)>=100, na.rm = TRUE) # 19
sum(round(BridgesDataFrame$T_MAX_P_HECP_USGS)>=50) # 28
sum(round(BridgesDataFrame$T_MAX_P_HECP_USGS)>=500) # 7
19/31 # 61%

sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM<100,"T_MAX_P_HECP_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM>=100,"T_MAX_P_HECP_USGS"],
                                   alternative = check),
       USE.NAMES = TRUE,
       simplify = TRUE)
sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM>1000,"T_MAX_P_HECP_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM<=1000,"T_MAX_P_HECP_USGS"],
                                   alternative = check),
       USE.NAMES = TRUE,
       simplify = TRUE)
sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$BOOL_REGULATION == TRUE,"T_MAX_P_HECP_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$BOOL_REGULATION == FALSE,"T_MAX_P_HECP_USGS"],
                                   alternative = check, exact = TRUE),
       USE.NAMES = TRUE,
       simplify = TRUE)

sum(BridgesDataFrame$Q_FAIL_IP_USGS == BridgesDataFrame$Q_MAX_P_USGS, na.rm = TRUE) # 21
sum(BridgesDataFrame$Q_FAIL_IP_USGS == BridgesDataFrame$Q_MAXPREFAIL_P_USGS, na.rm = TRUE) # 22
22/31 # 71%

sum(BridgesDataFrame$Q_FAIL_IP_USGS == BridgesDataFrame$Q_MAXPREFAIL_P_USGS & BridgesDataFrame$BOOL_HAS_FAIL_DATE == TRUE, na.rm = TRUE) # 8 of 10
sum(BridgesDataFrame$Q_FAIL_IP_USGS == BridgesDataFrame$Q_MAX_P_USGS & BridgesDataFrame$BOOL_HAS_FAIL_DATE == TRUE, na.rm = TRUE) # 8 of 10

View(BridgesDataFrame[BridgesDataFrame$BOOL_HAS_FAIL_DATE==TRUE,c("Q_FAIL_D_USGS","Q_FAIL_IP_USGS","Q_MAX_D_USGS","Q_MAX_P_USGS")])

# peaks trend
sum(BridgesDataFrame$BOOL_MK_SIGNIF==TRUE) # 10 when not consiring duplicate

# partial duration ----------
sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM<100,"T_FAIL_D_PARTDUR_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM>=100,"T_FAIL_D_PARTDUR_USGS"],
                                   alternative = check),
       USE.NAMES = TRUE,
       simplify = TRUE)
sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM>1000,"T_FAIL_D_PARTDUR_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM<=1000,"T_FAIL_D_PARTDUR_USGS"],
                                   alternative = check),
       USE.NAMES = TRUE,
       simplify = TRUE)
sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$BOOL_REGULATION == TRUE,"T_FAIL_D_PARTDUR_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$BOOL_REGULATION == FALSE,"T_FAIL_D_PARTDUR_USGS"],
                                   alternative = check, exact = TRUE),
       USE.NAMES = TRUE,
       simplify = TRUE)

sapply(Checks, 
       function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$BOOL_REGULATION == TRUE,"T_FAIL_IP_PARTDUR_USGS"],
                                   BridgesDataFrame[BridgesDataFrame$BOOL_REGULATION == FALSE,"T_FAIL_IP_PARTDUR_USGS"],
                                   alternative = check, exact = TRUE),
       USE.NAMES = TRUE,
       simplify = TRUE)


# probability of failure
beta <- 3
pf <- pnorm(-beta)
pf_all <- pnorm(-beta)^35
lambda <- pf*35
pf_one <- 1 - ppois(0, lambda)
beta <- 1.75
pf <- pnorm(-beta)
pf_all <- pnorm(-beta)^35
lambda <- pf*35
pf_one <- 1 - ppois(0, lambda)



