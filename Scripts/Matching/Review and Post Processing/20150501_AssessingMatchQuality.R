# 2015-05-01 Additional processing of df.Fail.NBI.Gage and selection of "good" vs. "bad" data
load(file.path(DataDir,"Analysis_Results_In_Progress","20150429_MatchesToNBIandToGagesAllDataPostSelectionProcessed.RData"))
library(reshape2)
library(ggmap)
library(plyr)
# processing ----------
FailCauses <- c("flood","scour","hurricane","other hydraulic")
df.Fail.NBI.Gage[grepl("scour",df.Fail.NBI.Gage$FAIL_CAUS),"FAIL_CAUS_CODE"] <- FailCauses[2]
df.Fail.NBI.Gage[grepl("hurricane",df.Fail.NBI.Gage$FAIL_CAUS) | grepl("agnes",df.Fail.NBI.Gage$FAIL_CAUS) | grepl("camille",df.Fail.NBI.Gage$FAIL_CAUS),"FAIL_CAUS_CODE"] <- FailCauses[3]
df.Fail.NBI.Gage[grepl("flood",df.Fail.NBI.Gage$FAIL_CAUS),"FAIL_CAUS_CODE"] <- FailCauses[1]
df.Fail.NBI.Gage[is.na(df.Fail.NBI.Gage$FAIL_CAUS_CODE),"FAIL_CAUS_CODE"] <- FailCauses[4]
df.Fail.NBI.Gage$FAIL_CAUS_CODE <- factor(x=df.Fail.NBI.Gage$FAIL_CAUS_CODE, c("flood","scour","hurricane","other hydraulic"))


df.Fail.NBI.Gage$FAIL_Q_AVAIL_EITHER <- df.Fail.NBI.Gage$FAIL_Q_AVAIL
df.Fail.NBI.Gage[!df.Fail.NBI.Gage$FAIL_Q_AVAIL & !is.na(df.Fail.NBI.Gage$FAIL_Q_AVAIL_ALT), "FAIL_Q_AVAIL_EITHER"] <- df.Fail.NBI.Gage[!df.Fail.NBI.Gage$FAIL_Q_AVAIL & !is.na(df.Fail.NBI.Gage$FAIL_Q_AVAIL_ALT), "FAIL_Q_AVAIL_ALT"]

df.Fail.NBI.Gage$GAGE_ON_BRIDGE_EITHER <- df.Fail.NBI.Gage$GAGE_ON_BRIDGE
df.Fail.NBI.Gage[!df.Fail.NBI.Gage$GAGE_ON_BRIDGE & !is.na(df.Fail.NBI.Gage$GAGE_ON_BRIDGE_ALT), "GAGE_ON_BRIDGE_EITHER"] <- df.Fail.NBI.Gage[!df.Fail.NBI.Gage$GAGE_ON_BRIDGE & !is.na(df.Fail.NBI.Gage$GAGE_ON_BRIDGE_ALT), "GAGE_ON_BRIDGE_ALT"]

df.Fail.NBI.Gage[,c("LAT_GAGE_ALT","LNG_GAGE_ALT","LATR_GAGE_ALT","LAT_GAGE","LNG_GAGE")] <- sapply(c("LAT_GAGE_ALT","LNG_GAGE_ALT","LATR_GAGE_ALT","LAT_GAGE","LNG_GAGE"), function(j) as.numeric(df.Fail.NBI.Gage[,j]))
df.Fail.NBI.Gage[,c("PEAK_COUNT","PEAK_COUNT_ALT","FIPS_GAGE","FIPS_GAGE_ALT")] <- sapply(c("PEAK_COUNT","PEAK_COUNT_ALT","FIPS_GAGE","FIPS_GAGE_ALT"), function(j) as.integer(df.Fail.NBI.Gage[,j]))

savefile  <- paste(gsub("-","",Sys.Date()),"MatchesToNBIandToGagesPostSelectionProcessed.RData",sep="_")
save(df.Fail.NBI.Gage, file=file.path(DataDir,"FrequentlyUsedArchiveData",savefile))

df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage,FailDataFrame[,c("ID","STREAM_NAME","STREAM_TYPE","STREAM_TRIB")],by="ID")
df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage,gageDataFrame[,c("STAID","STREAM_NAME_GAGE","STREAM_TYPE_GAGE","STREAM_TRIB_GAGE")],by="STAID")
gageDataFrame[,c("STAID_ALT","STREAM_NAME_GAGE_ALT","STREAM_TYPE_GAGE_ALT","STREAM_TRIB_GAGE_ALT")] <- gageDataFrame[,c("STAID","STREAM_NAME_GAGE","STREAM_TYPE_GAGE","STREAM_TRIB_GAGE")]
df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage,gageDataFrame[,c("STAID_ALT","STREAM_NAME_GAGE_ALT","STREAM_TYPE_GAGE_ALT","STREAM_TRIB_GAGE_ALT")],by="STAID_ALT")

savefile  <- paste(gsub("-","",Sys.Date()),"MatchesToNBIandToGagesWithStreamCols.RData",sep="_")
save(df.Fail.NBI.Gage, file=file.path(DataDir,"Analysis_Results_In_Progress",savefile))

# data quality assessment ----------
# looking for sites with:
#    (a) bridge on gage at time of failure
#    (b) bridge on gage, not at time of failure, with correct-stream local gage at time of failure
#    (c) bridge on gage, not at time of failure, with nearby, not-correct-fork local gage at failure
#    (d) bridge on gage, not at time of failure, with nearby, not-correct-stream local gage at failure
#    (e) bridge on nearby, correct stream at time of failure
#    (f) bridge on nearby, incorrect fork of stream at time of failure
#    (g) bridge on distant, correct stream at time of failure
#    (h)... will look at these categories only if necessary

# VARIABLES:
#  STREAM NAME MATCH (and ALT and EITHER)
#  HAS FORK MATCH (and ALT and EITHER)
#  UPSTREAM (and ALT and EITHER)
#  STREAM_GAGE_QUALITY_CONFIRMED (and ALT and EITHER)
df.Fail.NBI.Gage$STREAM_NAME_MATCH <- sapply(1:nrow(df.Fail.NBI.Gage), function(i) grepl(df.Fail.NBI.Gage[i,"STREAM_NAME"],df.Fail.NBI.Gage[i,"STREAM_NAME_GAGE"]))
df.Fail.NBI.Gage$STREAM_NAME_MATCH_ALT <- sapply(1:nrow(df.Fail.NBI.Gage), function(i) grepl(df.Fail.NBI.Gage[i,"STREAM_NAME"],df.Fail.NBI.Gage[i,"STREAM_NAME_GAGE_ALT"]))
df.Fail.NBI.Gage$STREAM_NAME_MATCH_EITHER <- df.Fail.NBI.Gage$STREAM_NAME_MATCH
df.Fail.NBI.Gage[!df.Fail.NBI.Gage$STREAM_NAME_MATCH & !is.na(df.Fail.NBI.Gage$STREAM_NAME_MATCH_ALT), "STREAM_NAME_MATCH_EITHER"] <- df.Fail.NBI.Gage[!df.Fail.NBI.Gage$STREAM_NAME_MATCH & !is.na(df.Fail.NBI.Gage$STREAM_NAME_MATCH_ALT), "STREAM_NAME_MATCH_ALT"]

df.Fail.NBI.Gage$FORK_MATCH     <- (df.Fail.NBI.Gage$STREAM_TRIB=="" & df.Fail.NBI.Gage$STREAM_TRIB_GAGE=="") | (df.Fail.NBI.Gage$STREAM_TRIB!="" & df.Fail.NBI.Gage$STREAM_TRIB_GAGE!="")
df.Fail.NBI.Gage[!is.na(df.Fail.NBI.Gage$STREAM_TRIB_GAGE_ALT),"FORK_MATCH_ALT"] <- (df.Fail.NBI.Gage[!is.na(df.Fail.NBI.Gage$STREAM_TRIB_GAGE_ALT),"STREAM_TRIB"]=="" & df.Fail.NBI.Gage[!is.na(df.Fail.NBI.Gage$STREAM_TRIB_GAGE_ALT),"STREAM_TRIB_GAGE_ALT"]=="") | 
                                          (df.Fail.NBI.Gage[!is.na(df.Fail.NBI.Gage$STREAM_TRIB_GAGE_ALT),"STREAM_TRIB"]!="" & df.Fail.NBI.Gage[!is.na(df.Fail.NBI.Gage$STREAM_TRIB_GAGE_ALT),"STREAM_TRIB_GAGE_ALT"]!="")

df.Fail.NBI.Gage$FORK_MATCH_EITHER <- df.Fail.NBI.Gage$FORK_MATCH
df.Fail.NBI.Gage[!df.Fail.NBI.Gage$FORK_MATCH & !is.na(df.Fail.NBI.Gage$FORK_MATCH_ALT), "FORK_MATCH_EITHER"] <- df.Fail.NBI.Gage[!df.Fail.NBI.Gage$FORK_MATCH & !is.na(df.Fail.NBI.Gage$FORK_MATCH_ALT), "FORK_MATCH_ALT"]

View(df.Fail.NBI.Gage[df.Fail.NBI.Gage$STREAM_NAME_MATCH_EITHER & df.Fail.NBI.Gage$FORK_MATCH_EITHER,c("ID","FEAT_UND","ITEM6A","STANAME","DIST_TO_GAGE","STANAME_ALT","DIST_TO_GAGE_ALT")])


# WHEN NEED TO VERIFY CORRECT STREAM VISUALLY
df.Subset.Melt <- melt(df.Fail.NBI.Gage[,c("ID","LATDD","LONGDD","LAT_GAGE","LNG_GAGE","LAT_GAGE_ALT","LNG_GAGE_ALT")],id.vars="ID")
df.Subset.Melt <- df.Subset.Melt[order(df.Subset.Melt$ID),]
df.Subset.Melt[df.Subset.Melt$variable=="LATDD" | df.Subset.Melt$variable=="LONGDD","TYPE"] <- "bridge"
df.Subset.Melt[is.na(df.Subset.Melt$TYPE),"TYPE"] <- "gage"
df.Subset.Melt$LAT  <- NA
df.Subset.Melt$LONG <- NA
df.Subset.Melt[df.Subset.Melt$variable=="LATDD","LAT"] <- df.Subset.Melt[df.Subset.Melt$variable=="LATDD","value"]
df.Subset.Melt[df.Subset.Melt$variable=="LATDD","LONG"] <- df.Subset.Melt[df.Subset.Melt$variable=="LONGDD","value"]
df.Subset.Melt[df.Subset.Melt$variable=="LAT_GAGE","LAT"] <- df.Subset.Melt[df.Subset.Melt$variable=="LAT_GAGE","value"]
df.Subset.Melt[df.Subset.Melt$variable=="LAT_GAGE","LONG"] <- df.Subset.Melt[df.Subset.Melt$variable=="LNG_GAGE","value"]
df.Subset.Melt[df.Subset.Melt$variable=="LAT_GAGE_ALT","LAT"] <- df.Subset.Melt[df.Subset.Melt$variable=="LAT_GAGE_ALT","value"]
df.Subset.Melt[df.Subset.Melt$variable=="LAT_GAGE_ALT","LONG"] <- df.Subset.Melt[df.Subset.Melt$variable=="LNG_GAGE","value"]
df.Subset.Melt <- df.Subset.Melt[!is.na(df.Subset.Melt$LAT),]

df.Subset.Melt[df.Subset.Melt$variable=="LAT_GAGE","FAIL_Q_AVAIL"] <- df.Fail.NBI.Gage$FAIL_Q_AVAIL
df.Subset.Melt[df.Subset.Melt$variable=="LAT_GAGE_ALT","FAIL_Q_AVAIL"] <- df.Fail.NBI.Gage[!is.na(df.Fail.NBI.Gage$STAID_ALT),"FAIL_Q_AVAIL_ALT"]
df.Subset.Melt[df.Subset.Melt$variable=="LATDD","HAS_FAIL_DATE"] <- df.Fail.NBI.Gage$HAS_FAIL_DATE
df.Subset.Melt[df.Subset.Melt$variable=="LATDD","FAIL_CAUS_CODE"] <- as.character(df.Fail.NBI.Gage$FAIL_CAUS_CODE)
df.Subset.Melt$TYPE <- factor(df.Subset.Melt$TYPE)
df.Subset.Melt <- df.Subset.Melt[,c(1,4:9)]
df.Subset.Melt$LAT <- as.numeric(df.Subset.Melt$LAT)
df.Subset.Melt$LONG <- as.numeric(df.Subset.Melt$LONG)
df.Subset.Melt[is.na(df.Subset.Melt$FAIL_CAUS_CODE),"FAIL_CAUS_CODE"] <- "gage"
df.Subset.Melt$FAIL_CAUS_CODE <- factor(x=df.Subset.Melt$FAIL_CAUS_CODE, c("flood","scour","hurricane","other hydraulic","gage"),labels=c("bridge-flood","bridge-scour","bridge-hurricane","bridge-hydraulic","gage"))
df.Subset.Melt[is.na(df.Subset.Melt$FAIL_Q_AVAIL),"FAIL_Q_AVAIL"] <- TRUE

row <- "52"
basemap<- get_map(location=c(lon=df.Fail.NBI.Gage[row,"LONGDD"],lat=df.Fail.NBI.Gage[row,"LATDD"]),zoom=10,maptype="road",source="google",color="bw")
map1 <- ggmap(basemap, extent="normal",darken=c(0.4,"black"),legend="bottom",base_layer = ggplot(df.Subset.Melt, aes(x=LONG,y=LAT)),maprange=TRUE)+ylim(41.4,43.6)+xlim(-76.6,-73.4)
map2 <- map1 + ggtitle(plotTitle) + xlab("") + ylab("") 
map4 <- map2 + geom_point(aes(shape=FAIL_CAUS_CODE,color=FAIL_Q_AVAIL,fill=HAS_FAIL_DATE,size=4)) + scale_shape_manual(values=c(21,22,23,25,4))  +  
  scale_color_manual(values=c("red","blue")) +  scale_fill_manual(values=c("white","black")) 
print(map4)

# load my assessment columns
df.Assessment <- read.table("/Users/MM/Documents/Research/Climate_Scour_Local/Results_Synthesized/20150504_StreamMatchQualityAssessment.txt",header=TRUE,colClasses=c("integer",rep("logical",4)))
df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage,df.Assessment,by="ID")

savefile  <- paste(gsub("-","",Sys.Date()),"MatchesToNBIandToGagesWithQualityAssessmentAndSomeFixedLocs.RData",sep="_")
save(df.Fail.NBI.Gage, file=file.path(DataDir,"FrequentlyUsedArchiveData",savefile))

ConfirmedMatches <- (df.Fail.NBI.Gage$CONFIRM_STREAM & df.Fail.NBI.Gage$CONFIRM_FORK & !is.na(df.Fail.NBI.Gage$CONFIRM_STREAM)) | 
                      (df.Fail.NBI.Gage$CONFIRM_STREAM_ALT & df.Fail.NBI.Gage$CONFIRM_FORK_ALT & !is.na(df.Fail.NBI.Gage$CONFIRM_STREAM_ALT))
ConfirmedMatches <- ConfirmedMatches & ((df.Fail.NBI.Gage$DIST_TO_GAGE<5 & !is.na(df.Fail.NBI.Gage$CONFIRM_STREAM)) |
                                           (df.Fail.NBI.Gage$DIST_TO_GAGE_ALT<5 & !is.na(df.Fail.NBI.Gage$CONFIRM_STREAM_ALT)))
rows <- rownames(df.Fail.NBI.Gage)
ConfirmedMatches <- rows[ConfirmedMatches & !(df.Fail.NBI.Gage$ID %in% c(3,103,136,476,1068,1289,1442,1099,1471,1925))]
View(df.Fail.NBI.Gage[ConfirmedMatches,c("ID","FEAT_UND","ITEM6A","STANAME","DIST_TO_GAGE","STANAME_ALT","DIST_TO_GAGE_ALT","FAIL_Q_AVAIL","FAIL_Q_AVAIL_ALT","STAID","STAID_ALT")])

IDsForValidation  <- c(3,103,136,476,1068,1289,1442)
IDsForCorrelation <- c(1099,1471,1925)
IDsWithin5km <- c(50,203,376,391,393, 398, 405,421,479,499,799,864,923, # 799 is a non-necessary correlation entry (good chance to verify)
                  1004,1166,1272,1390,1482,1548,1550,3535,3626) #1004 also non-necessary
IDsForCorrelation <- c(IDsForCorrelation, 394,799,829,1004,1047,1630,3551,3618)
# 7 entries on bridge with failure, 23 within 5km with failure data
# 10 IDs for correlation study
rowsToView <- rownames(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% c(IDsForValidation,IDsWithin5km),])
View(df.Fail.NBI.Gage[rowsToView,c("ID","STATE_CODE","TYPE","MAT","YR_BLT", "YR_FAIL", "DATE_FAIL", "FAIL_TYPE", "FAIL_CAUS", "COMMENTS",
                                                         "DRAIN_SQKM","HUC02", "PEAK_BEGIN_DATE", "PEAK_END_DATE", "PEAK_COUNT", "DIST_TO_GAGE",
                                                         paste(c("DRAIN_SQKM", "HUC02","PEAK_BEGIN_DATE", "PEAK_END_DATE", "PEAK_COUNT", "DIST_TO_GAGE"),"_ALT",sep=""))])

savefile  <- paste(gsub("-","",Sys.Date()),"IDsForFurtherStudy.RData",sep="_")
save(IDsForCorrelation,IDsForValidation,IDsWithin5km, file=file.path(DataDir,"Analysis_Results_In_Progress",savefile))

