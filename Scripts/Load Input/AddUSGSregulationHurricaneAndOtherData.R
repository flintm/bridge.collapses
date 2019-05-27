library(plyr)
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))

# 2015-07-29 Cleanup of dataframe after investigated all locations
df.Fail.NBI.Gage$LATR <- deg2rad(df.Fail.NBI.Gage$LATDD)
df.Fail.NBI.Gage$LONGR <- deg2rad(df.Fail.NBI.Gage$LONGDD)
df.Fail.NBI.Gage$LONGR_GAGE <- deg2rad(df.Fail.NBI.Gage$LNG_GAGE)
df.Fail.NBI.Gage$LONGR_GAGE_ALT <- deg2rad(df.Fail.NBI.Gage$LNG_GAGE_ALT)
df.Fail.NBI.Gage$LATR_GAGE_ALT <- deg2rad(df.Fail.NBI.Gage$LAT_GAGE_ALT)
df.Fail.NBI.Gage$LATR_GAGE <- deg2rad(df.Fail.NBI.Gage$LAT_GAGE)
df.Fail.NBI.Gage$DIST_TO_GAGE <- gcd_slc(df.Fail.NBI.Gage[,"LONGR"],df.Fail.NBI.Gage[,"LATR"],df.Fail.NBI.Gage[,"LONGR_GAGE"],df.Fail.NBI.Gage[,"LATR_GAGE"])
df.Fail.NBI.Gage$DIST_TO_GAGE_ALT <- gcd_slc(df.Fail.NBI.Gage[,"LONGR"],df.Fail.NBI.Gage[,"LATR"],df.Fail.NBI.Gage[,"LONGR_GAGE_ALT"],df.Fail.NBI.Gage[,"LATR_GAGE_ALT"])

# add info on dams, link to hurricanes, and confirm failure date as known where appropriate
df.Fail.NBI.Gage$COMMENT_LINKED_HURRICANE <- character(nrow(df.Fail.NBI.Gage))
df.Fail.NBI.Gage$COMMENT_LOCAL_DAM        <- character(nrow(df.Fail.NBI.Gage))
ID <- 136
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LINKED_HURRICANE"] <- "Hurricane Jeanne"
ID<-203
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LINKED_HURRICANE"] <- "Hurricane Irene"
ID<-376
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LINKED_HURRICANE"] <- "Tropical Storm Heidi"
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LOCAL_DAM"] <- "reservoir"
ID<-393
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LINKED_HURRICANE"] <- "Hurricane Agnes"
ID<-398
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LINKED_HURRICANE"] <- "Hurricane Agnes"
ID<-476
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"BOOL_HAS_FAIL_DATE"] <- TRUE
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_Q_FAIL_USGS"] <- paste(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_Q_FAIL_USGS"],
                                                                    "- Flood report lists failure date")
ID<-479
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LOCAL_DAM"] <- "Grand_ID 2111,2116,hydro,bridge 0.5mi downstream"
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"BOOL_HAS_FAIL_DATE"] <- TRUE
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_Q_FAIL_USGS"] <- paste(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_Q_FAIL_USGS"],
                                                                    "- Flood report lists failure date")
ID<-499
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"BOOL_HAS_FAIL_DATE"] <- TRUE
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_Q_FAIL_USGS"] <- paste(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_Q_FAIL_USGS"],
                                                                    "- Flood report lists failure date")
ID<-799
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LOCAL_DAM"] <- "Everett Dam, upstream"
ID<-1166
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LOCAL_DAM"] <- "dam, downstream"
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LINKED_HURRICANE"] <- "Hurricane Juan"
ID<-1272
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LINKED_HURRICANE"] <- "Hurricane Agnes"
ID<-1289
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LOCAL_DAM"] <- "two reservoirs, downstream"
ID<-3535
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LOCAL_DAM"] <- "Grand ID 1067 downstream"
ID<-3626
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LINKED_HURRICANE"] <- "Hurricane Irene"
ID<-195
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LINKED_HURRICANE"] <- "Hurricane Fran"
ID<-809
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LOCAL_DAM"] <- "Grand ID 514, Lahontan Dam, down, 535 Bodie Dam on E. Fork"
ID<-1044
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LOCAL_DAM"] <- "several small dam ruins"
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LINKED_HURRICANE"] <- "Hurricane Juan"
ID<-1160
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LINKED_HURRICANE"] <- "Hurricane Juan"
ID<-1164
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LINKED_HURRICANE"] <- "Hurricane Juan"
ID<-1631
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LOCAL_DAM"] <- "reservoir upstream"
ID<-1675
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LOCAL_DAM"] <- "Arkabutla lake downstream but backs up creek when flood control used"
ID<-1740
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LOCAL_DAM"] <- "Mariosa lock and dam on Osage River downstream"
ID<-3528
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LOCAL_DAM"] <- "dam above Hardy 20km upstream"
ID<-3610
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LINKED_HURRICANE"] <- "Hurricane Irene"
ID<-3629
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LINKED_HURRICANE"] <- "Hurricane Irene"
ID<-391
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_LINKED_HURRICANE"] <- "Hurricane Agnes"

savefile <- paste(gsub("-","",Sys.Date()),"df.Fail.NBI.Gage_WithHurricaneAndDamComments.RData",sep="_")
save(df.Fail.NBI.Gage, file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
savefile <- "df.Fail.NBI.Gage.active.RData"
save(df.Fail.NBI.Gage,file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

# 2015-08-05 adding drainage area calcs from ORNL - THINK THIS MAY BE CORRUPTED- LOAD DIRECTLY FROM DATA
library(plyr)
BridgeGageList <- read.table(file.path(dirs$OrigDataDir,"ORNL_Area_Calcs","20150610_SitesForRouting.txt"),
                             header = TRUE,
                             colClasses = c("character","numeric","numeric","factor","numeric"))
BridgeList <- BridgeGageList[BridgeGageList$TYPE=="bridge",]
BridgeList[,"Bridge.Id"] <- sapply(1:nrow(BridgeList), function(i) paste("B", sprintf("%04d",i),sep=""))
GageList <- BridgeGageList[BridgeGageList$TYPE=="gauge",]
GageList[,"Gauge.ID"] <- sapply(1:nrow(GageList), function(i) paste("G", sprintf("%04d",i),sep=""))

AreaData <- read.csv(file = file.path(dirs$OrigDataDir,"ORNL_Area_Calcs","Routing_allsites_area_ORNL.csv"))
AreaDataGage   <- AreaData[!is.na(AreaData[,"X.2"]),c("Gauge.ID","VIC_area","gauge_aera")]
AreaDataBridge <- AreaData[!is.na(AreaData[,"X.3"]),c("Bridge.Id", "VIC_area.1",  "Bridge.Area..NHDflow..")]
LocDataGage    <- AreaData[!is.na(AreaData[,"X.2"]),c("Gauge.ID","lat","long")]
LocDataBridge  <- AreaData[!is.na(AreaData[,"X.3"]),c("Bridge.Id", "lat.1",  "long.1")]

AreaDataBridge <- join(BridgeList,AreaDataBridge,by="Bridge.Id")
colnames(AreaDataBridge)[7:8] <- c("DRAIN_AREA_DVICB_SQKM", "DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM")
AreaDataGage   <- join(GageList, AreaDataGage, by = "Gauge.ID")
colnames(AreaDataGage)[c(1,7)] <- c("STAID","DRAIN_AREA_DVICG_SQKM")
LocDataGage    <- join(GageList, LocDataGage, by = "Gauge.ID")
colnames(LocDataGage)[c(1,7:8)] <- c("STAID","LAT_DVICG","LONG_DVICG")
LocDataBridge  <- join(BridgeList,LocDataBridge,by="Bridge.Id")
colnames(LocDataBridge)[7:8] <- c("LAT_DVICB","LONG_DVICB")

df.Fail.NBI.Gage <- df.Fail.NBI.Gage[,!(colnames(df.Fail.NBI.Gage) %in% c("ORNL_LAT","ORNL_LONG","DRAIN_AREA_DVICG_SQKM","DRAIN_AREA_DVICB_SQKM","DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM") )]
df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage, AreaDataGage[,c("STAID","DRAIN_AREA_DVICG_SQKM")])
df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage, AreaDataBridge[,c("ID","DRAIN_AREA_DVICB_SQKM","DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM")])
df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage, LocDataGage[,c("STAID","LAT_DVICG","LONG_DVICG")])
df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage, LocDataBridge[,c("ID","LAT_DVICB","LONG_DVICB")])

# colNames   <- c("ID","USGSSTAID","ORNL ID","USGSLAT","USGSLONG","ORNL_LAT","ORNL_LONG","DIST_TO_GAGE_KM",
#                 "USGS_DRAINAGE_SQKM","DRAIN_AREA_DVICG_SQKM","GAUGE_AREA_SQKM","DRAIN_AREA_DVICB_SQKM",
#                 "DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM","AREA_ASHLEY_SQKM","AREA_EMMA_SQKM","NOTES_A_E")
# colClasses <- c("integer", "character", "integer", rep("numeric",12),"character")
# drainageDataORNL <- read.csv(file.path(dirs$OrigDataDir,"ORNL_Area_Calcs","WatershedCalculations.csv"),
#                              header=FALSE,
#                              skip = 2,
#                              col.names = colNames,
#                              stringsAsFactors = FALSE,
#                              colClasses = colClasses)
# rm(colNames,colClasses)
# df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage,drainageDataORNL[,c("ID","ORNL_LAT","ORNL_LONG","DRAIN_AREA_DVICG_SQKM","DRAIN_AREA_DVICB_SQKM","DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM")],by="ID",type="left",match="first")
# # df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage,drainageDataORNL[,c("ID","DRAIN_AREA_DVICG_SQKM")],by="ID",type="left",match="first")
# rm(drainageDataORNL)
# from Stanford Calcs (Ashley)
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1164,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"] <- 2208
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==203,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"] <- 201
  
# 2015-11-18 updated NHD and VIC-bridge from ORNL
ID <- 1166
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID == ID,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"] <- 1756
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID == ID,"DRAIN_AREA_DVICB_SQKM"] <- 1599


df.Fail.NBI.Gage$AREA_RATIO_VIC <- df.Fail.NBI.Gage$VIC_AREA_BRIDGE_SQKM/df.Fail.NBI.Gage$DRAIN_SQKM
df.Fail.NBI.Gage$AREA_RATIO_DVICG <- df.Fail.NBI.Gage$VIC_AREA_GAGE_SQKM/df.Fail.NBI.Gage$DRAIN_SQKM
df.Fail.NBI.Gage$AREA_RATIO_NHD <- df.Fail.NBI.Gage$BRIDGE_NHDFLOWPLUS_SQKM/df.Fail.NBI.Gage$DRAIN_SQKM
df.Fail.NBI.Gage$AREA_VIC_AREA_NHD_BRIDGE_RATIO <- df.Fail.NBI.Gage$VIC_AREA_BRIDGE_SQKM/df.Fail.NBI.Gage$BRIDGE_NHDFLOWPLUS_SQKM

# something really weird seems to be going on at 1160/1164 - drainage area is off by a factor of 8 between VIC and USGS

df.Fail.NBI.Gage$BOOL_ONE_AREA_WIN_10PCT <- abs(df.Fail.NBI.Gage$AREA_RATIO_VIC - 1) <= 0.1 | abs(df.Fail.NBI.Gage$AREA_RATIO_NHD - 1) <= 0.1
df.Fail.NBI.Gage$BOOL_ONE_AREA_WIN_20PCT <- abs(df.Fail.NBI.Gage$AREA_RATIO_VIC - 1) <= 0.2 | abs(df.Fail.NBI.Gage$AREA_RATIO_NHD - 1) <= 0.2

savefile <- paste(gsub("-","",Sys.Date()),"df.Fail.NBI.Gage_WithDrainageInfoAndVIClocations.RData",sep="_")
save(df.Fail.NBI.Gage, file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage,file=file.path(dirsGit$Data,savefile))
rm(savefile)

# Load assessment of routed monthly performance (from plots sent by Moet)
df.VICperformance <- read.table(file = file.path(dirs$OrigDataDir,"ORNL_Area_Calcs","20151211_ORNL_Gage_Data.tab"), 
                                header = TRUE, stringsAsFactors = FALSE, 
                                colClasses = c("integer", "character", "character", rep("numeric",4)))
df.VICperformance <- df.VICperformance[!duplicated(df.VICperformance$STAID),c(2,4:7)]
colnames(df.VICperformance)[2:5] <- paste(colnames(df.VICperformance)[2:5],"VIC","GAGE",sep="_")
df.Fail.NBI.Gage  <- join(df.Fail.NBI.Gage[,!(colnames(df.Fail.NBI.Gage) %in% colnames(df.VICperformance)[2:5])], df.VICperformance, by = "STAID")


# 2015-09-22 Degree of regulation from USGS
regulationData <- read.table(file.path(dirs$OrigDataDir,"20150922_DegreeOfRegulation.txt"),
                             skip = 1,
                             col.names = c("ID","BOOL_REGULATION","COMMENT_REGULATION"),
                             sep = "\t",
                             stringsAsFactors = FALSE,
                             colClasses = c("integer", "logical", "character"))
df.Fail.NBI.Gage.temp <- join(df.Fail.NBI.Gage[,c("ID","STAID")],regulationData,by="ID")
df.Fail.NBI.Gage.temp <- df.Fail.NBI.Gage.temp[order(df.Fail.NBI.Gage.temp$ID),]
rowsToView            <- rownames(df.Fail.NBI.Gage.temp[df.Fail.NBI.Gage.temp$ID %in% IDsToView,])
colnames(df.Fail.NBI.Gage.temp)[colnames(df.Fail.NBI.Gage.temp)=="T_PARTIAL"] <- "T_PARTIAL_USGS"
colnames(df.Fail.NBI.Gage.temp)[colnames(df.Fail.NBI.Gage.temp)=="EXCEED_PARTIAL"] <- "EXCEED_PARTIAL_USGS"
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1164,"BOOL_REGULATION"] <- df.Fail.NBI.Gage.temp[df.Fail.NBI.Gage.temp$ID==1164,"BOOL_REGULATION"]
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1164,"COMMENT_REGULATION"] <- df.Fail.NBI.Gage.temp[df.Fail.NBI.Gage.temp$ID==1164,"COMMENT_REGULATION"]
df.Fail.NBI.Gage <- df.Fail.NBI.Gage.temp
rm(df.Fail.NBI.Gage.temp,regulationData)
regulationData <- read.table(file.path(dirs$OrigDataDir,"20151111_DegreeOfRegulationShort.txt"),
                             skip = 1,
                             col.names = c("ID","BOOL_REGULATION","COMMENT_REGULATION_SHORT"),
                             sep = "\t",
                             stringsAsFactors = FALSE,
                             colClasses = c("integer", "logical", "character"))
df.Fail.NBI.Gage.temp <- join(df.Fail.NBI.Gage[,c("ID","STAID")],regulationData[,c("ID","COMMENT_REGULATION_SHORT")],by="ID")
df.Fail.NBI.Gage$COMMENT_REGULATION_SHORT <- df.Fail.NBI.Gage.temp$REGULATION_SHORT

# clean up T entries
T_Fail_cols <- colnames(df.Fail.NBI.Gage)[grepl("T_FAIL",colnames(df.Fail.NBI.Gage))]
for (j in T_Fail_cols){
  df.Fail.NBI.Gage[is.na(df.Fail.NBI.Gage[,j]) | df.Fail.NBI.Gage[,j] == 0 | is.infinite(df.Fail.NBI.Gage[,j]), j] <- NA_real_
}

df.Fail.NBI.Gage$T_FAIL_IP_HECP_USGS <- df.Fail.NBI.Gage$T_FAIL_HEC_INST
df.Fail.NBI.Gage[is.na(df.Fail.NBI.Gage$T_FAIL_IP_HECP_USGS),"T_FAIL_IP_HECP_USGS"] <- df.Fail.NBI.Gage$T_FAIL_HEC_PEAK[is.na(df.Fail.NBI.Gage$T_FAIL_IP_HECP_USGS)]
df.Fail.NBI.Gage$Q_FAIL_IP_USGS <- df.Fail.NBI.Gage$FAIL_INST_Q
df.Fail.NBI.Gage[is.na(df.Fail.NBI.Gage$Q_FAIL_IP_USGS),"Q_FAIL_IP_USGS"] <- df.Fail.NBI.Gage$FAIL_Q_PEAK[is.na(df.Fail.NBI.Gage$Q_FAIL_IP_USGS)]

# regional skews
skews1 <- read.table(file.path(dirs$OrigDataDir,"HEC","20150605_16Sites_Skew.txt"),
header = TRUE,
sep = "\t",
colClasses = c("integer", "character","character", "numeric", "numeric", "numeric"))
skews2 <- read.table(file.path(dirs$OrigDataDir,"HEC","20150519_Skews.txt"),
header = TRUE,
sep = "\t",
colClasses = c("integer", "numeric", "numeric", "numeric","character"))
skews <- join(skews1,skews2, by="ID", type = "full")
skews$REGIONAL_SKEW <- skews$SKEW
df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage,skews[,c("ID","REGIONAL_SKEW")],by="ID",type="left")
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% c(1160, 1164),"REGIONAL SKEW"] <- 0.4

# NSE values for VIC
load(file.path(dirs$DataDirAnalysis,"20151205_USGS_DaymetVIC_CorrelationAndTestStatistics.RData"))
NSE <- unlist(sapply(names(DaymetVICperformance), function(i) DaymetVICperformance[[i]]["Nash"], USE.NAMES = TRUE))
df <- data.frame(STAID = substr(names(NSE),1,8), NSE = NSE)
df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage,df,by="STAID")
df.Fail.NBI.Gage$BOOL_NSE_D_DVICG_POS <- df.Fail.NBI.Gage$NSE_D_DVICG > 0

# KS values for VIC
KS <- unlist(sapply(names(DaymetVICanPkPerformance), function(i) DaymetVICanPkPerformance[[i]][["KSall"]]$p.value, USE.NAMES = TRUE))
KSpass <- sapply(names(DaymetVICanPkPerformance), function(i) unlist(DaymetVICanPkPerformance[[i]]["KSpassAlphasAllData"]), USE.NAMES = TRUE, simplify = FALSE)
alphas <- as.character(c(0.01, 0.05, 0.1))
BestAlphaPassed <- sapply(1:length(KSpass), function(i) ifelse(any(KSpass[[i]]),alphas[sum(KSpass[[i]])],"FAIL"), USE.NAMES = TRUE)
names(BestAlphaPassed) <- names(KSpass)
df.Fail.NBI.Gage[rowsToView,"BOOL_KS_ALL_DATA_DVICG_PASS"] <- sapply(rowsToView, function(i) BestAlphaPassed[df.Fail.NBI.Gage[i,"STAID"]])

# REPORT CITATION INFORMATION
df.Fail.NBI.Gage$COMMENT_T_REPORT_CITE <- ""
df.Fail.NBI.Gage[df.Fail.NBI.Gage$COMMENT_T_REPORT_NAME=="USGS WSP-1820","COMMENT_T_REPORT_CITE"] <- "\\cite{Rostvedt1968}"
df.Fail.NBI.Gage[df.Fail.NBI.Gage$COMMENT_T_REPORT_NAME=="USGS PP 0924","COMMENT_T_REPORT_CITE"] <- "\\cite{Bailey1975}"
df.Fail.NBI.Gage[df.Fail.NBI.Gage$COMMENT_T_REPORT_NAME=="USGS WSP-2424","COMMENT_T_REPORT_CITE"] <- "\\cite{Fontaine1994}"
df.Fail.NBI.Gage[df.Fail.NBI.Gage$COMMENT_T_REPORT_NAME=="USGS WSP 2375","COMMENT_T_REPORT_CITE"] <- "\\cite{Paulson1988}"
df.Fail.NBI.Gage[df.Fail.NBI.Gage$COMMENT_T_REPORT_NAME=="USGS OFR 86-486","COMMENT_T_REPORT_CITE"] <- "\\cite{Lescinsky1986}"
df.Fail.NBI.Gage[df.Fail.NBI.Gage$COMMENT_T_REPORT_NAME=="USGS WRI 97-4277","COMMENT_T_REPORT_CITE"] <- "\\cite{Sumioka1998}"
df.Fail.NBI.Gage[df.Fail.NBI.Gage$COMMENT_T_REPORT_NAME=="Pontrelli 1999","COMMENT_T_REPORT_CITE"] <- "\\cite{Pontrelli1999}"
df.Fail.NBI.Gage[df.Fail.NBI.Gage$COMMENT_T_REPORT_NAME=="USGS OFR 2007-1036 *DISCHARGE DOESN'T MATCH","COMMENT_T_REPORT_CITE"] <- "\\cite{Suro2007}"

df.Fail.NBI.Gage[df.Fail.NBI.Gage$COMMENT_T_REPORT_NAME=="USGS PP 0924","COMMENT_T_REPORT_NAME"] <- "USGS PP 924"

savefile <- paste(gsub("-","",Sys.Date()),"df.Fail.NBI.Gage_WithNSEandKS.RData", sep="_")
save(df.Fail.NBI.Gage, rowsToView,IDsToView,file = file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

# ROAD TYPE (E.G., INTERSTATE) FROM NBI ITEM5D
load(file.path(dirs$DataDirFrequent,"NBI_bridges_over_water.RData"))
ITEM5B <- df.USbridges.rivers[df.Fail.NBI.Gage$NBI_ROW,"ITEM5B"]
df.Fail.NBI.Gage$ITEM5B <- ITEM5B

savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage, rowsToView,IDsToView,file = file.path(dirs$dfActive,savefile))
rm(savefile)

# COMMENTS - CLEANED UP
df.Fail.NBI.Gage$COMMMENTS_SHORT <- ""
ID<-136
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_SHORT"] <- "Pier scour during flood"
ID <- 203
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_SHORT"] <-"Approach wash out/scour"
ID <- 1004
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_SHORT"] <-"Channel migration/scour"
ID<-3610
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"COMMENT_SHORT"] <-"Abutment scour"

savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage, rowsToView,IDsToView,file = file.path(dirs$dfActive,savefile))
rm(savefile)

# SKEWS
skews1 <- read.table(file.path(dirs$OrigDataDir,"HEC","20150605_16Sites_Skew.txt"),
                     header = TRUE,
                     sep = "\t", 
                     colClasses = c("integer", "character","character", "numeric", "numeric", "numeric"))

skews2 <- read.table(file.path(dirs$OrigDataDir,"HEC","20150519_Skews.txt"),
                     header = TRUE,
                     sep = "\t", 
                     colClasses = c("integer", "numeric", "numeric", "numeric","character"))

skews <- join(skews1,skews2, by="ID", type = "full")
skews$REGIONAL_SKEW <- skews$SKEW
df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage,skews[,c("ID","REGIONAL_SKEW")],by="ID",type="left")
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1164,"REGIONAL_SKEW"] <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1160,"REGIONAL_SKEW"]

savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage, rowsToView,IDsToView,file = file.path(dirs$dfActive,savefile))
rm(savefile)
