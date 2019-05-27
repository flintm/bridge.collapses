# 2015-04-30 Selecting gage and alternate gages for matched bridges
# using df.Fail.NBI.Gage.All
load(file.path(DataDir,"Analysis_Results_In_Progress","20150429_MatchesToNBIandToGagesAllDataPreSelection.RData"))
# or load("/Users/MM/Documents/Research/Climate_Scour_Local/R_data/Analysis_Results_In_Progress/20150429_ReadyToSelectBestGageFromAllMatchesWorkspace.RData")
df.Fail.NBI.Gage.All <- df.Fail.NBI.Gage.All[order(df.Fail.NBI.Gage.All$ID),]
df.Fail.NBI.Gage.All[,c("STAID_ALT","STATE_ALT","STANAME_ALT","DRAIN_SQKM_ALT","HUC02_ALT","FIPS_GAGE_ALT","peak_begin_date_alt",
                        "peak_end_date_alt","LAT_GAGE_ALT","LNG_GAGE_ALT","LATR_GAGE_ALT","LONGR_GAGE_ALT","DIST_TO_GAGE_ALT",
                        "FAIL_Q_AVAIL_ALT","GAGE_ON_BRIDGE_ALT","FAIL_Q_ALT","TIME_Q_EXCEED_ALT","FAIL_Q_COMMENT_ALT",
                        "MAX_Q_ALT","MAX_Q_DATE_ALT")] <- NA
View(df.Fail.NBI.Gage.All[,c("ID","STATE_CODE","LOCATION","FEAT_UND","STANAME","DIST_TO_GAGE","FAIL_Q_AVAIL","STANAME_ALT","DIST_TO_GAGE_ALT","FAIL_Q_AVAIL_ALT")])
# compile alt data for IDs with multiple gage options

df.Fail.NBI.Gage.All <- df.Fail.NBI.Gage.All[!(rownames(df.Fail.NBI.Gage.All) %in% rowsToRemoveAll),]
df.Fail.NBI.Gage <- df.Fail.NBI.Gage.All
savefile  <- paste(gsub("-","",Sys.Date()),"MatchesToNBIandToGagesAllDataPostSelection.RData",sep="_")
save(df.Fail.NBI.Gage, file=file.path(DataDir,"Analysis_Results_In_Progress",savefile))

# now make sure all rows have all desired data
library(plyr)
df.Fail.NBI.Gage[,c("BIN","TYPE","MAT","YR_BLT","YR_FAIL","DATE_FAIL","FAIL_TYPE","FAIL_CAUS","COMMENTS")] <- FailDataFrame[FailDataFrame$ID %in% df.Fail.NBI.Gage$ID,c("BIN","TYPE","MAT","YR_BLT","YR_FAIL","DATE_FAIL","FAIL_TYPE","FAIL_CAUS","COMMENTS")]

colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage)%in% c("ITEM42A","ITEM42B")] <- c("ITEM43A","ITEM43B")
df.Fail.NBI.Gage[,c("ITEM3","ITEM27","ITEM43A","ITEM43B")] <- nbiDataFrame[df.Fail.NBI.Gage$NBI_ROW,c("ITEM3","ITEM27","ITEM43A","ITEM43B")]

df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage[,colnames(df.Fail.NBI.Gage)[!(colnames(df.Fail.NBI.Gage) %in% c("STATE","DRAIN_SQKM", "HUC02","FIPS_GAGE"))]],gageDataFrame[c("STAID","STATE","DRAIN_SQKM", "HUC02","FIPS_GAGE")],by="STAID")
gageDataFrame$STAID_ALT <- gageDataFrame$STAID
gageDataFrame$STATE_ALT <- gageDataFrame$STATE
gageDataFrame$DRAIN_SQKM_ALT <- gageDataFrame$DRAIN_SQKM
gageDataFrame$HUC02_ALT <- gageDataFrame$HUC02
gageDataFrame$FIPS_GAGE_ALT <- gageDataFrame$FIPS_GAGE
df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage[,colnames(df.Fail.NBI.Gage)[!(colnames(df.Fail.NBI.Gage) %in% c("STATE_ALT","DRAIN_SQKM_ALT", "HUC02_ALT","FIPS_GAGE_ALT"))]],gageDataFrame[,c("STAID_ALT","STATE_ALT","DRAIN_SQKM_ALT", "HUC02_ALT","FIPS_GAGE_ALT")],by="STAID_ALT")

df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage[,colnames(df.Fail.NBI.Gage)[!(colnames(df.Fail.NBI.Gage) %in% c("peak_begin_date","peak_end_date"))]],gageDataFullInventory[,c("STAID","peak_begin_date","peak_end_date","peak_count_nu")],by="STAID")
gageDataFullInventory$peak_begin_date_alt <- gageDataFullInventory$peak_begin_date
gageDataFullInventory$peak_end_date_alt <- gageDataFullInventory$peak_end_date
gageDataFullInventory$peak_count_nu_alt <- gageDataFullInventory$peak_count_nu
gageDataFullInventory$STAID_ALT <- gageDataFullInventory$STAID
df.Fail.NBI.Gage <- join(df.Fail.NBI.Gage[,colnames(df.Fail.NBI.Gage)[!(colnames(df.Fail.NBI.Gage) %in% c("peak_begin_date_alt","peak_end_date_alt"))]],gageDataFullInventory[,c("STAID_ALT","peak_begin_date_alt","peak_end_date_alt","peak_count_nu_alt")],by="STAID_ALT")

# now must process dates again
df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_begin_date)==7,"peak_begin_date"] <- paste(df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_begin_date)==7,"peak_begin_date"],"01",sep="-")
df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_begin_date)==4,"peak_begin_date"] <- paste(df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_begin_date)==4,"peak_begin_date"],"01","01",sep="-")
df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_end_date)==7,"peak_end_date"] <- paste(df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_end_date)==7,"peak_end_date"],"28",sep="-")
df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_end_date)==4,"peak_end_date"] <- paste(df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_end_date)==4,"peak_end_date"],"12","31",sep="-")
df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_begin_date_alt)==7,"peak_begin_date_alt"] <- paste(df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_begin_date_alt)==7,"peak_begin_date_alt"],"01",sep="-")
df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_begin_date_alt)==4,"peak_begin_date_alt"] <- paste(df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_begin_date_alt)==4,"peak_begin_date_alt"],"01","01",sep="-")
df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_end_date_alt)==7,"peak_end_date_alt"] <- paste(df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_end_date_alt)==7,"peak_end_date_alt"],"28",sep="-")
df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_end_date_alt)==4,"peak_end_date_alt"] <- paste(df.Fail.NBI.Gage[nchar(df.Fail.NBI.Gage$peak_end_date_alt)==4,"peak_end_date_alt"],"12","31",sep="-")

# now put columns into correct order
ColsToKeep <- c("ID","BIN","STFIPS","STATE_CODE" ,"LOCATION" , "FEAT_UND","TYPE","MAT","YR_BLT", "YR_FAIL",  "DATE_FAIL", "FAIL_TYPE","FAIL_CAUS","COMMENTS",
                "NBI_ROW","ITEM3", "ITEM5D","ITEM6A","ITEM7","ITEM8", "ITEM9", "ITEM27","ITEM43A","ITEM43B","LATDD","LONGDD", "LATR","LONGR",             
                "STAID", "STATE","STANAME", "DRAIN_SQKM" ,"HUC02","FIPS_GAGE", "peak_begin_date", "peak_end_date","peak_count_nu","LAT_GAGE", "LNG_GAGE","LONGR_GAGE", "LATR_GAGE",     
                "HAS_FAIL_DATE", "DATE_FAIL_EST", "DIST_TO_GAGE","FAIL_Q_AVAIL", "GAGE_ON_BRIDGE",
                "FAIL_Q","TIMES_Q_EXCEED","FAIL_Q_COMMENT", "MAX_Q","MAX_Q_DATE",
                "STAID_ALT","STATE_ALT","STANAME_ALT","DRAIN_SQKM_ALT", "HUC02_ALT","FIPS_GAGE_ALT","peak_begin_date_alt","peak_end_date_alt","peak_count_nu_alt",
                "LAT_GAGE_ALT","LNG_GAGE_ALT","LATR_GAGE_ALT","LONGR_GAGE_ALT","DIST_TO_GAGE_ALT","FAIL_Q_AVAIL_ALT", "GAGE_ON_BRIDGE_ALT",
                "FAIL_Q_ALT","TIME_Q_EXCEED_ALT","FAIL_Q_COMMENT_ALT", "MAX_Q_ALT","MAX_Q_DATE_ALT")
df.Fail.NBI.Gage <- df.Fail.NBI.Gage[,ColsToKeep]
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% c("peak_begin_date", "peak_end_date","peak_count_nu","peak_begin_date_alt","peak_end_date_alt","peak_count_nu_alt","TIME_Q_EXCEED_ALT")] <- c("PEAK_BEGIN_DATE","PEAK_END_DATE","PEAK_COUNT",
                                                                                                                                                                                 "PEAK_BEGIN_DATE_ALT","PEAK_END_DATE_ALT","PEAK_COUNT_ALT",
                                                                                                                                                                                 "TIMES_Q_EXCEED_ALT")
#apparently missing a few FAIL_Q_AVAILs
rows <- which(df.Fail.NBI.Gage$DATE_FAIL_EST=="" & is.na(df.Fail.NBI.Gage$FAIL_Q_AVAIL))
df.Fail.NBI.Gage[rows,"DATE_FAIL_EST"] <- df.Fail.NBI.Gage[rows,"DATE_FAIL"]
df.Fail.NBI.Gage[is.na(df.Fail.NBI.Gage[rows,"DATE_FAIL_EST"]),"DATE_FAIL_EST"] <- df.Fail.NBI.Gage[is.na(df.Fail.NBI.Gage[rows,"DATE_FAIL_EST"]),"YR_FAIL"]


GageDuringFailure <- sapply(rows, function(i) ifelse(!is.na(df.Fail.NBI.Gage[i,"PEAK_BEGIN_DATE"]),
                                                     df.Fail.NBI.Gage[i,"DATE_FAIL_EST"] >= df.Fail.NBI.Gage[i,"PEAK_BEGIN_DATE"],
                                                     TRUE)) &
  sapply(rows, function(i) ifelse(!is.na(df.Fail.NBI.Gage[i,"PEAK_END_DATE"]),
                                  df.Fail.NBI.Gage[i,"DATE_FAIL_EST"] <= df.Fail.NBI.Gage[i,"PEAK_END_DATE"],
                                  TRUE))

df.Fail.NBI.Gage[rows,"FAIL_Q_AVAIL"] <- GageDuringFailure
df.Fail.NBI.Gage$FAIL_Q_AVAIL <- as.logical(df.Fail.NBI.Gage$FAIL_Q_AVAIL)
df.Fail.NBI.Gage$FAIL_Q_AVAIL_ALT <- as.logical(df.Fail.NBI.Gage$FAIL_Q_AVAIL_ALT)
df.Fail.NBI.Gage$GAGE_ON_BRIDGE <- as.logical(df.Fail.NBI.Gage$GAGE_ON_BRIDGE)
df.Fail.NBI.Gage$GAGE_ON_BRIDGE_ALT <- as.logical(df.Fail.NBI.Gage$GAGE_ON_BRIDGE_ALT)

df.Fail.NBI.Gage[is.na(df.Fail.NBI.Gage$FAIL_Q_AVAIL),"FAIL_Q_AVAIL"] <- FALSE
df.Fail.NBI.Gage[is.na(df.Fail.NBI.Gage$HAS_FAIL_DATE),"HAS_FAIL_DATE"] <- FALSE
df.Fail.NBI.Gage[is.na(df.Fail.NBI.Gage$GAGE_ON_BRIDGE),"GAGE_ON_BRIDGE"] <- FALSE

# 20150504 found a bad match on potomac, so replacing
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1168,c("STAID_ALT","STATE_ALT","STANAME_ALT", "DRAIN_SQKM_ALT" ,"HUC02_ALT","LAT_GAGE_ALT", "LNG_GAGE_ALT")] <- gageDataFrame["1024",c("STAID","STATE","STANAME", "DRAIN_SQKM" ,"HUC02","LAT_GAGE", "LNG_GAGE")]
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1168,c("PEAK_BEGIN_DATE_ALT","PEAK_END_DATE_ALT","PEAK_COUNT_ALT")] <- gageDataFullInventory["1024",c("peak_begin_date","peak_end_date","peak_count_nu")]
i <- "197"
GageDuringFailure <-  ifelse(!is.na(df.Fail.NBI.Gage[i,"PEAK_BEGIN_DATE_ALT"]),
                                                     df.Fail.NBI.Gage[i,"DATE_FAIL_EST"] >= df.Fail.NBI.Gage[i,"PEAK_BEGIN_DATE_ALT"],
                                                     TRUE) &
                       ifelse(!is.na(df.Fail.NBI.Gage[i,"PEAK_END_DATE_ALT"]),
                                  df.Fail.NBI.Gage[i,"DATE_FAIL_EST"] <= df.Fail.NBI.Gage[i,"PEAK_END_DATE_ALT"],
                                  TRUE)
df.Fail.NBI.Gage[i,"FAIL_Q_AVAIL_ALT"] <- GageDuringFailure
df.Fail.NBI.Gage[i,"LATR_GAGE_ALT"] <- deg2rad(df.Fail.NBI.Gage[i,"LAT_GAGE_ALT"])
df.Fail.NBI.Gage[i,"LONGR_GAGE_ALT"] <- deg2rad(df.Fail.NBI.Gage[i,"LNG_GAGE_ALT"])
df.Fail.NBI.Gage[i,"DIST_TO_GAGE_ALT"] <- gcd_slc(df.Fail.NBI.Gage[i,"LONGR"],df.Fail.NBI.Gage[i,"LATR"],df.Fail.NBI.Gage[i,"LONGR_GAGE_ALT"],df.Fail.NBI.Gage[i,"LATR_GAGE_ALT"])

#1023 1024 are gage rows for s fork s branch

savefile  <- paste(gsub("-","",Sys.Date()),"MatchesToNBIandToGagesAllDataPostSelectionProcessed.RData",sep="_")
save(df.Fail.NBI.Gage, file=file.path(DataDir,"FrequentlyUsedArchiveData",savefile))

