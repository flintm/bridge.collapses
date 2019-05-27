# 2015-04-24 Setup for re-considering matches on bridges where there is a good match for a nearby gage

load("~/Documents/Research/Climate_Scour_Local/R_data/FrequentlyUsedArchiveData/FailDataFrameHandCorrectedStreams.RData")
load("~/Documents/Research/Climate_Scour_Local/R_data/FrequentlyUsedArchiveData/20150423_gageDataFrameProcessedStreamName.RData")
load(file.path(DataDir,"Analysis_Results_In_Progress","20150424_FailDataPossibleGages.RData")
load("~/Documents/Research/Climate_Scour_Local/Archived_Analysis_Runs/matchbridges_stream_20150130/PossibleMatchesAllAfter-stream.RData")
load("~/Documents/Research/Climate_Scour_Local/R_data/FrequentlyUsedArchiveData/nbiDataFrameWithStream.RData")

IDGoodBridgeGageMatch <- c(1380,1982,1315,1321,1322,210,212,215,216,1453,1457,354,355,377,379,380,382,386,390,392,395,396,405,420,421,456,1262,1263,
                           1269,1270,1482,475,1548,167,187,502,506,509,1495,1496,1498,1499,1501,1502,783,796,798,203,250,254,811,826,836,888,3615,
                           3637,948,995,999,2742,74,75,1252,1388,1246,1419,1071,1072,1073,1074,1607,1649,1660,1662,47,50,57,1160,1165,1166,1168,1176,1177)
IDGoodBridgeGageMatch <- IDGoodBridgeGageMatch[order(IDGoodBridgeGageMatch)]
FailDataFrameSubset   <- FailDataFrame[FailDataFrame$ID %in% IDGoodBridgeGageMatch,] #83 rows
FailDataFrameSubset   <- FailDataFrameSubset[order(FailDataFrameSubset$ID),]

MatchIDs <- unlist(lapply(PossibleMatchRowsAll,"[",1)) #1127
PossibleMatchRowsBridgeGageMatch <- list()
for (i in 1:length(IDGoodBridgeGageMatch)){
  PossibleMatchRowsBridgeGageMatch[[i]] <- PossibleMatchRowsAll[[which(MatchIDs==paste("ID",IDGoodBridgeGageMatch[i],sep=""))]]
}
# 83 entries in PossibleMatchRowsBridgeGageMatch
rm(PossibleMatchRowsAll)

names(PossibleMatchRowsBridgeGageMatch) <- as.character(IDGoodBridgeGageMatch)
savefile  <- paste(gsub("-","",Sys.Date()),"PossibleMatchRowsForFailDataPossibleGages.RData",sep="_")
save(PossibleMatchRowsBridgeGageMatch, file=file.path(DataDir,"Analysis_Results_In_Progress",savefile))

NumberMatches<- integer(length(IDGoodBridgeGageMatch))
for (i in 1:length(IDGoodBridgeGageMatch)){
  NumberMatches[i] <- length(PossibleMatchRowsBridgeGageMatch[[i]][!grepl("[[:alpha:]]",PossibleMatchRowsBridgeGageMatch[[i]]) & !grepl("-", PossibleMatchRowsBridgeGageMatch[[i]])])
}
names(NumberMatches) <- IDGoodBridgeGageMatch
cutoff <- 10
hist(NumberMatches[NumberMatches<=cutoff & NumberMatches> 0])
View(FailDataFrameSubset[NumberMatches<=cutoff,]) # 17 rows

# merge appropriate NBI and Fail rows
CheckIndex             <- which(NumberMatches<=cutoff& NumberMatches> 0)   # 17 rows, 18 for road match
nbiBridgesToCheck      <- unlist(lapply(CheckIndex, function(i) PossibleMatchRowsBridgeGageMatch[[i]]
                                        [!grepl("[[:alpha:]]",PossibleMatchRowsBridgeGageMatch[[i]])&!grepl("-",PossibleMatchRowsBridgeGageMatch[[i]])])) # 107 NBI rownames
FailIDsRep             <- unlist(sapply(CheckIndex, function(i) rep(IDGoodBridgeGageMatch[i],NumberMatches[i]))) # 107 IDs
nbiMatchDataFrame      <- nbiDataFrame[nbiBridgesToCheck,] # 107 rows
nbiMatchDataFrame$ID   <- FailIDsRep
colnames(nbiMatchDataFrame)[c(1,4,16,17)] <- c("STFIPS_NBI","ITEM5B_NBI","STREAM_TYPE_NBI","FIPS_NBI")

FailDataPossibleNBI <- join(FailDataFrameSubset[CheckIndex,],nbiMatchDataFrame,by="ID",match="all")
View(FailDataPossibleNBI[,c("ID","LOCATION","FEAT_UND","FIPS","FIPS_FROM_CITY","FIPS_NBI","ITEM5D","ITEM6A","ITEM7","ITEM9")])

# only one decent match of 17 -  try with road only match.
load("~/Documents/Research/Climate_Scour_Local/Archived_Analysis_Runs/2015-01-26-matchbridges-road/PossibleMatchesAllAfter-road-1.RData")

# had better yield, so am going to record the matches
MatchDF <- data.frame(ID = c(379,392,456,783,1262,1270),
                      row = c(2,   1,  1,  1,   7,   1))
MatchIndex <- which(FailDataFrameSubset$ID %in% MatchDF$ID)
nbiBridgesToCheck      <- lapply(MatchIndex, function(i) PossibleMatchRowsBridgeGageMatch[[i]]
                                        [!grepl("[[:alpha:]]",PossibleMatchRowsBridgeGageMatch[[i]])&!grepl("-",PossibleMatchRowsBridgeGageMatch[[i]])])
nbiMatches <- sapply(1:length(nbiBridgesToCheck), function(i) nbiBridgesToCheck[[i]][MatchDF[i,"row"]])
nbiMatchDataFrame      <- nbiDataFrame[nbiMatches,]
nbiMatchDataFrame$ID   <- MatchDF$ID
colnames(nbiMatchDataFrame)[c(1,4,16,17)] <- c("STFIPS_NBI","ITEM5B_NBI","STREAM_TYPE_NBI","FIPS_NBI")
FailDataFrameMatched <- join(FailDataFrameSubset[MatchIndex,],nbiMatchDataFrame,by="ID",match="all")
FailDataMatchedPossGages <- FailDataPossibleGages[FailDataPossibleGages$ID %in% FailDataFrameMatched$ID,]
FailDataMatchedPossGages <- FailDataMatchedPossGages[order(FailDataMatchedPossGages$ID),]
FailDataFrameMatched[,colnames(FailDataMatchedPossGages)[42:66]] <- FailDataMatchedPossGages[,c(42:66)]
FailDataFrameMatched$NBI_ROW <- nbiMatches

# supplement with additional data from FailDataFrame and NBI
load("~/Documents/Research/Climate_Scour_Local/R_data/FrequentlyUsedArchiveData/FailDataFrameProcessed.RData")
FailDataFrameMatched <- join(FailDataFrameMatched,FailDataFrame[,c("ID","BIN","TYPE","MAT","YR_BLT","YR_FAIL","DATE_FAIL","FAIL_TYPE","FAIL_CAUS","FATAL","INJURY",
                                                   "SOURCE","COMMENTS")],by="ID")
load("~/Documents/Research/Climate_Scour_Local/R_data/FrequentlyUsedArchiveData/nbiDataFramePreProcessedAllFields.RData")
FailDataFrameMatched[,c("ITEM27","ITEM42A","ITEM42B")] <- nbiDataFrame[FailDataFrameMatched$NBI_ROW,c("ITEM27","ITEM42A","ITEM42B")]
savefile  <- paste(gsub("-","",Sys.Date()),"MatchesConfirmedPossibleGages.RData",sep="_")
save(FailDataFrameMatched, file=file.path(DataDir,"Analysis_Results_In_Progress",savefile))

# now process, see if good matches or not
nMatch <- nrow(FailDataFrameMatched)
FailDataFrameMatched$LONGR <- deg2rad(FailDataFrameMatched$LONGDD)
FailDataFrameMatched$LATR  <- deg2rad(FailDataFrameMatched$LATDD)
FailDataFrameMatched$LONGR_GAGE  <- deg2rad(FailDataFrameMatched$LNG_GAGE)
FailDataFrameMatched$LATR_GAGE   <- deg2rad(FailDataFrameMatched$LAT_GAGE)
FailDataFrameMatched$DIST_TO_GAGE <-  sapply(1:nMatch, function(j) gcd_slc(FailDataFrameMatched[j,"LONGR"],
                                                                           FailDataFrameMatched[j,"LATR"],
                                                                           FailDataFrameMatched[j,"LONGR_GAGE"],
                                                                           FailDataFrameMatched[j,"LATR_GAGE" ]))
# was able to find all bridges using Google Earth
FailDataFrameMatched[1,c("LATDD","LONGDD")] <- c(39.201368, -77.203959)
FailDataFrameMatched[2,c("LATDD","LONGDD")] <- c(39.445738, -77.238757)
FailDataFrameMatched[3,c("LATDD","LONGDD")] <- c(39.318109, -77.326772)
FailDataFrameMatched[5,c("LATDD","LONGDD")] <- c(39.381825, -76.500878)
FailDataFrameMatched[6,c("LATDD","LONGDD")] <- c(39.239438,-76.847171)

load("~/Documents/Research/Climate_Scour_Local/R_data/FrequentlyUsedArchiveData/20150420_All_Avail_UGSS_Gage_Data.RData")
View(gageDataFullInventory[gageDataFullInventory$site_no==FailDataFrameMatched[2,"STAID"],])
# One in 2km, looks like no major tribs, but peaks from 2008-2010 only (fail in 1972)
View(gageDataFullInventory[gageDataFullInventory$site_no==FailDataFrameMatched[5,"STAID"],])
# other in 0.5km, no major tribs, fail in 1971, recorded 1995-2014

# OK. So clearly need to take a step back, look at which gage matches also have coverage when needed

# Not-contraindicated matches
IDokBridgeGageMatch <- c(3552,3585,215,296,175,1007,1019,1021,1023,1046,1052,1053,1085,1088,1609)
IDokBridgeGageMatch <- IDokBridgeGageMatch[order(IDokBridgeGageMatch)]
FailDataFrameSubset <- FailDataFrame[FailDataFrame$ID %in% IDokBridgeGageMatch,] #15 rows
FailDataFrameSubset <- FailDataFrameSubset[order(FailDataFrameSubset$ID),]

# Matches with wrong county or state
IDwrongLocationMatch <- c(1870,1927,3504,214,1983,3597,87,91,1304,1307,1308,1311,1313,1314,1323,1330,1332,1333,
                          1334,1347,1348,1351,1352,1367,1368,1369,297,1446,1451,1452,1462,1465,361,418,458,459,
                          1264,7,12,150,166,485,1707,1738,501,1497,1233,534,823,831,840,874,877,902,1589,3617,
                          3622,947,3599,1491,1634,1011,1013,1015,1247,1256,1391,1393,1403,1404,1727,1560,1101,
                          1652,1657,1117,1124,1127,1128,1140,1159,1165,1172,1188,1193,1194) #86
IDwrongLocationMatch <- IDwrongLocationMatch[order(IDwrongLocationMatch)]

IDwrongStreamMatch <- c(206,209,231,228,290,1312,294,315,1438,1445,1463,1466,399,400,401,1263,1271,1277,1483,
                        1484,477,1734,1740,1504,1232,807,808,868,1552,176,1241,2738,135,187,1016,1020,1040,
                        1405,1075,1617,30,31,54,1164,1178,1187,1189,1191)
IDwrongStreamMatch <- IDwrongStreamMatch[order(IDwrongStreamMatch)]

IDmatchStatus <- data.frame(ID=ID <- c(IDGoodBridgeGageMatch,IDokBridgeGageMatch,IDwrongLocationMatch,IDwrongStreamMatch))
IDmatchStatus$STATUS <- c(rep("GOOD",length(IDGoodBridgeGageMatch)),rep("OK",length(IDokBridgeGageMatch)),
                          rep("BAD_LOCATION",length(IDwrongLocationMatch)),rep("BAD_STREAM",length(IDwrongStreamMatch)))
IDmatchStatus <- IDmatchStatus[order(IDmatchStatus$ID),]
rownames(IDmatchStatus) <- as.character(1:nrow(IDmatchStatus))

# 2015-04-28 process located bridges - have used "000000" where have known lat/long but not known NBI row - have 49 obs.
ID_NBIrows <- read.table("/Users/MM/Documents/Research/Climate_Scour_Local/Data_original_files/20150428_LocatedBridgeRows.txt",
                         header=TRUE,)
ID_NBIrows <- ID_NBIrows[order(ID_NBIrows$ID),]
KnownLoc   <- ID_NBIrows[ID_NBIrows$NBI_ROW==0,"ID"] # 6
ID_NBIrows <- ID_NBIrows[!(ID_NBIrows$ID %in% KnownLoc),] # 43
ID_NBIrows$NBI_ROW <- as.character(ID_NBIrows$NBI_ROW)
FailDataMatched2 <- FailDataFrame[FailDataFrame$ID %in% ID_NBIrows$ID | FailDataFrame$ID %in% KnownLoc,] # 49 - good
FailDataMatched2 <- FailDataMatched2[order(FailDataMatched2$ID),]

FailDataMatched2[FailDataMatched2$ID %in% ID_NBIrows$ID,"NBI_ROW"] <- ID_NBIrows$NBI_ROW
FailDataMatched2[,c("ITEM8","ITEM5D","ITEM7","ITEM9","ITEM6A","ITEM27","ITEM43A","ITEM43B","ITEM27","ITEM42A","ITEM42B","LATDD","LONGDD")] <- nbiDataFrame[FailDataMatched2$NBI_ROW,c("ITEM8","ITEM5D","ITEM7","ITEM9","ITEM6A","ITEM27","ITEM43A","ITEM43B","ITEM27","ITEM42A","ITEM42B","LATDD","LONGDD")]
FailDataMatched2$NBI_FIPS <- nbiDataFrame[FailDataMatched2$NBI_ROW,"STFIPS"]*1000 + nbiDataFrame[FailDataMatched2$NBI_ROW,"ITEM3"]
FailDataMatched2[FailDataMatched2$ID==KnownLoc[1],c("LATDD","LONGDD")] <- c(41.16167, -74.18861)   #ID 203
FailDataMatched2[FailDataMatched2$ID==KnownLoc[2],c("LATDD","LONGDD")] <- c(42.53639, -75.53611)   #ID 250
FailDataMatched2[FailDataMatched2$ID==KnownLoc[3],c("LATDD","LONGDD")] <- c(39.449821, -76.786796) #ID 382
FailDataMatched2[FailDataMatched2$ID==KnownLoc[4],c("LATDD","LONGDD")] <- c(39.722164, -78.086966) #ID 1484
FailDataMatched2[FailDataMatched2$ID==KnownLoc[5],c("LATDD","LONGDD")] <- c(40.00333, -92.71333)   #ID 1707
FailDataMatched2[FailDataMatched2$ID==KnownLoc[6],c("LATDD","LONGDD")] <- c(38.369722, -91.986017) #ID 1740
FailDataMatched2[FailDataMatched2$ID==380,c("LATDD","LONGDD")]         <- c(39.269258, -76.631074)
FailDataMatched2[FailDataMatched2$ID==400,c("LATDD","LONGDD")]         <- c(39.355531, -76.573076)
FailDataMatched2[FailDataMatched2$ID==458,c("LATDD","LONGDD")]         <- c(39.519500, -77.450990) # correct for NBI, although ambiguous for fail (nearby bridge)
FailDataMatched2[FailDataMatched2$ID==1269,c("LATDD","LONGDD")]        <- c(39.382723, -76.758021)
FailDataMatched2[FailDataMatched2$ID==1482,c("LATDD","LONGDD")]        <- c(39.588056, -79.084622)
# Now pull gages
FailDataMatched2 <- FailDataMatched2[order(FailDataMatched2$ID),]
PossibleMatchRowsMatched2 <- PossibleMatchRowsAll[names(PossibleMatchRowsAll) %in% as.character(FailDataMatched2$ID)]
PossibleMatchRowsMatched2 <- PossibleMatchRowsMatched2[order(as.integer(names(PossibleMatchRowsMatched2)))]
# only one entry with 2 possible matches - will look at later if necessary
GageRows <- unlist(lapply(PossibleMatchRowsMatched2,"[[",2))
FailDataMatched2[,colnames(gageDataFrame)[c(1:2,5:9,25)]] <- gageDataFrame[GageRows,c(1:2,5:9,25)]

load("~/Documents/Research/Climate_Scour_Local/R_data/FrequentlyUsedArchiveData/20150420_All_Avail_UGSS_Gage_Data.RData")

nMatch <- nrow(FailDataMatched2)
FailDataMatched2$LONGR <- deg2rad(FailDataMatched2$LONGDD)
FailDataMatched2$LATR  <- deg2rad(FailDataMatched2$LATDD)
FailDataMatched2$LONGR_GAGE  <- deg2rad(FailDataMatched2$LNG_GAGE)
FailDataMatched2$LATR_GAGE   <- deg2rad(FailDataMatched2$LAT_GAGE)
FailDataMatched2$DIST_TO_GAGE <-  sapply(1:nMatch, function(j) gcd_slc(FailDataMatched2[j,"LONGR"],
                                                                           FailDataMatched2[j,"LATR"],
                                                                           FailDataMatched2[j,"LONGR_GAGE"],
                                                                           FailDataMatched2[j,"LATR_GAGE" ]))
# bridge with 2 matches (Cayuga inlet) is 182km from first gage, 5km from second, so use second
FailDataMatched2[FailDataMatched2$ID==826,colnames(gageDataFrame)[c(1:2,5:9,25)]] <- gageDataFrame["3703",c(1:2,5:9,25)]

FailDataMatched2 <- join(FailDataMatched2,gageDataFullInventory[,c("STAID","peak_begin_date","peak_end_date")])

savefile  <- paste(gsub("-","",Sys.Date()),"MatchesToNBIandToGagesRound2.RData",sep="_")
save(FailDataMatched2, file=file.path(DataDir,"Analysis_Results_In_Progress",savefile))

## 2015-04-29 Fixing peak_begin and peak_end to see if gage during failure 
FailDataMatched2$DATE_FAIL_EST <- FailDataMatched2$DATE_FAIL
FailDataMatched2[is.na(FailDataMatched2$DATE_FAIL_EST),"DATE_FAIL_EST"] <- FailDataMatched2[is.na(FailDataMatched2$DATE_FAIL_EST),"YR_FAIL"]

FailDataMatched2[nchar(FailDataMatched2$peak_begin_date)==7,"peak_begin_date"] <- paste(FailDataMatched2[nchar(FailDataMatched2$peak_begin_date)==7,"peak_begin_date"],"01",sep="-")
FailDataMatched2[nchar(FailDataMatched2$peak_begin_date)==4,"peak_begin_date"] <- paste(FailDataMatched2[nchar(FailDataMatched2$peak_begin_date)==4,"peak_begin_date"],"01","01",sep="-")
FailDataMatched2[nchar(FailDataMatched2$peak_end_date)==7,"peak_end_date"] <- paste(FailDataMatched2[nchar(FailDataMatched2$peak_end_date)==7,"peak_end_date"],"28",sep="-")
FailDataMatched2[nchar(FailDataMatched2$peak_end_date)==4,"peak_end_date"] <- paste(FailDataMatched2[nchar(FailDataMatched2$peak_end_date)==4,"peak_end_date"],"12","31",sep="-")

rows <- which(!is.na(FailDataMatched2$DATE_FAIL_EST))
GageDuringFailure <- sapply(rows, function(i) ifelse(!is.na(FailDataMatched2[i,"peak_begin_date"]),
                                                     FailDataMatched2[i,"DATE_FAIL_EST"] >= FailDataMatched2[i,"peak_begin_date"],
                                                     TRUE)) &
                                            sapply(rows, function(i) ifelse(!is.na(FailDataMatched2[i,"peak_end_date"]),
                                                  FailDataMatched2[i,"DATE_FAIL_EST"] <= FailDataMatched2[i,"peak_end_date"],
                                                  TRUE))

FailDataMatched2$FAIL_Q_AVAIL <- "NA"
FailDataMatched2[rows,"FAIL_Q_AVAIL"] <- GageDuringFailure
FailDataMatched2$GAGE_ON_BRIDGE <- FailDataMatched2$DIST_TO_GAGE < 0.3
FailDataMatched2$HAS_FAIL_DATE <- !is.na(FailDataMatched2$DATE_FAIL)
savefile  <- paste(gsub("-","",Sys.Date()),"MatchesToNBIandToGagesRound2.RData",sep="_")
save(FailDataMatched2, file=file.path(DataDir,"Analysis_Results_In_Progress",savefile))

View(FailDataMatched2[FailDataMatched2$DIST_TO_GAGE<15,c("ID","FEAT_UND","STANAME","DIST_TO_GAGE","FAIL_Q_AVAIL")])

# -----------------
# looks like have 8-9 good options depending on how far away is OK. that's pretty good. Going to go back through previous 330 matches to see if there are
# any with a gage that is perhaps farther, but on the right stream

IDsToReCheck <- names(PossibleMatchRowsAll)[which(!grepl("-",unlist(lapply(PossibleMatchRowsAll,"[[",2))))]
IDsToReCheck <- as.integer(IDsToReCheck)
IDsToReCheck <- IDsToReCheck[IDsToReCheck %in% df.Gages.Bridges$ID] # 144 of these
FailDataMatched3 <- df.Gages.Bridges[df.Gages.Bridges$ID %in% IDsToReCheck,c(1:27)]
IDsToReCheck <- IDsToReCheck[order(IDsToReCheck)]
FailDataMatched3$ID <- as.integer(FailDataMatched3$ID)
FailDataMatched3 <- FailDataMatched3[order(FailDataMatched3$ID),]
PossibleMatchRowsMatched3 <- PossibleMatchRowsAll[names(PossibleMatchRowsAll) %in% as.character(IDsToReCheck)]
PossibleMatchRowsMatched3 <- PossibleMatchRowsMatched3[order(as.integer(names(PossibleMatchRowsMatched3)))]
# only one entry with 2 possible matches - will look at later if necessary
OneMatch       <- names(PossibleMatchRowsMatched3)[unlist(lapply(PossibleMatchRowsMatched3,length))==2] #129 single matches
GageRowsSingle <- unlist(lapply(PossibleMatchRowsMatched3,"[[",2))
FailDataMatched3[,colnames(gageDataFrame)[c(1:2,5:9,25)]] <- gageDataFrame[GageRowsSingle,c(1:2,5:9,25)]

nMatch <- nrow(FailDataMatched3)
FailDataMatched3$LONGR_GAGE  <- deg2rad(FailDataMatched3$LNG_GAGE)
FailDataMatched3$LATR_GAGE   <- deg2rad(FailDataMatched3$LAT_GAGE)
FailDataMatched3$DIST_TO_GAGE <-  sapply(1:nMatch, function(j) gcd_slc(FailDataMatched3[j,"LONGR"],
                                                                       FailDataMatched3[j,"LATR"],
                                                                       FailDataMatched3[j,"LONGR_GAGE"],
                                                                       FailDataMatched3[j,"LATR_GAGE" ]))

# continue processing - may want to switch gages depending on distance OR availability of data, so need to compute
FailDataMatched3$DATE_FAIL_EST <- FailDataMatched3$DATE_FAIL
FailDataMatched3[is.na(FailDataMatched3$DATE_FAIL_EST),"DATE_FAIL_EST"] <- FailDataMatched3[is.na(FailDataMatched3$DATE_FAIL_EST),"YR_FAIL"]

FailDataMatched3 <- join(FailDataMatched3,gageDataFullInventory[,c("STAID","peak_begin_date","peak_end_date")])

FailDataMatched3[nchar(FailDataMatched3$peak_begin_date)==7,"peak_begin_date"] <- paste(FailDataMatched3[nchar(FailDataMatched3$peak_begin_date)==7,"peak_begin_date"],"01",sep="-")
FailDataMatched3[nchar(FailDataMatched3$peak_begin_date)==4,"peak_begin_date"] <- paste(FailDataMatched3[nchar(FailDataMatched3$peak_begin_date)==4,"peak_begin_date"],"01","01",sep="-")
FailDataMatched3[nchar(FailDataMatched3$peak_end_date)==7,"peak_end_date"] <- paste(FailDataMatched3[nchar(FailDataMatched3$peak_end_date)==7,"peak_end_date"],"28",sep="-")
FailDataMatched3[nchar(FailDataMatched3$peak_end_date)==4,"peak_end_date"] <- paste(FailDataMatched3[nchar(FailDataMatched3$peak_end_date)==4,"peak_end_date"],"12","31",sep="-")

rows <- which(!is.na(FailDataMatched3$DATE_FAIL_EST))
GageDuringFailure <- sapply(rows, function(i) ifelse(!is.na(FailDataMatched3[i,"peak_begin_date"]),
                                                     FailDataMatched3[i,"DATE_FAIL_EST"] >= FailDataMatched3[i,"peak_begin_date"],
                                                     TRUE)) &
  sapply(rows, function(i) ifelse(!is.na(FailDataMatched3[i,"peak_end_date"]),
                                  FailDataMatched3[i,"DATE_FAIL_EST"] <= FailDataMatched3[i,"peak_end_date"],
                                  TRUE))

FailDataMatched3$FAIL_Q_AVAIL <- "NA"
FailDataMatched3[rows,"FAIL_Q_AVAIL"] <- GageDuringFailure
FailDataMatched3$GAGE_ON_BRIDGE <- FailDataMatched3$DIST_TO_GAGE < 0.3
FailDataMatched3$HAS_FAIL_DATE <- !is.na(FailDataMatched3$DATE_FAIL)

savefile  <- paste(gsub("-","",Sys.Date()),"MatchesToNBIandToGagesRound3.RData",sep="_")
save(FailDataMatched3, file=file.path(DataDir,"Analysis_Results_In_Progress",savefile))
View(FailDataMatched3[FailDataMatched3$DIST_TO_GAGE<15 & FailDataMatched3$FAIL_Q_AVAIL=="TRUE",c("ID","FEAT_UND","STANAME","DIST_TO_GAGE","FAIL_Q_AVAIL")])
# 28 in there, although may overlap with previous "nearest" approach
MultipleMatches       <- names(PossibleMatchRowsMatched3)[unlist(lapply(PossibleMatchRowsMatched3,length))>2] #15 multiple matches
NumberMatchesMultiple <- integer(length(MultipleMatches))
for (i in 1:length(MultipleMatches)){
  NumberMatchesMultiple[i] <- length(PossibleMatchRowsMatched3[[MultipleMatches[i]]][!grepl("[[:alpha:]]",PossibleMatchRowsMatched3[[MultipleMatches[i]]]) & !grepl("-", PossibleMatchRowsMatched3[[MultipleMatches[i]]])])
}
View(FailDataMatched3[FailDataMatched3$ID %in% as.integer(MultipleMatches),c("ID","STATE_CODE","ITEM7","ITEM6A","STANAME","STATE","DIST_TO_GAGE","FAIL_Q_AVAIL")])
# df.Gages.Bridges <- df.Gages.Bridges[order(as.integer(df.Gages.Bridges$ID)),]
View(df.Gages.Bridges[df.Gages.Bridges$ID %in% as.integer(MultipleMatches),c("ID","STATE_CODE","ITEM7","ITEM6A","station_nm","DIST_TO_GAGE","FAIL_Q_AVAIL")])
for (mm in 7:max(NumberMatchesMultiple)){
  HasMultiple <- NumberMatchesMultiple >= mm
  print(paste(sum(HasMultiple),"have multiple of at least",mm))
  GageRowsMultiple <- unlist(lapply(PossibleMatchRowsMatched3[MultipleMatches[HasMultiple]],"[[",mm+1))
  
  FailDataTemp <- FailDataMatched3[FailDataMatched3$ID %in% as.integer(MultipleMatches[HasMultiple]),c(1:27)]
  FailDataTemp[,colnames(gageDataFrame)[c(1:2,5:9,25)]] <- gageDataFrame[GageRowsMultiple,c(1:2,5:9,25)]
  
  FailDataTemp$LONGR_GAGE  <- deg2rad(FailDataTemp[FailDataTemp$ID %in% as.integer(MultipleMatches[HasMultiple]),"LNG_GAGE"])
  FailDataTemp$LATR_GAGE   <- deg2rad(FailDataTemp[FailDataTemp$ID %in% as.integer(MultipleMatches[HasMultiple]),"LAT_GAGE"])
  FailDataTemp$DIST_TO_GAGE <-  sapply(1:nrow(FailDataTemp), function(j) gcd_slc(FailDataTemp[j,"LONGR"],
                                                                         FailDataTemp[j,"LATR"],
                                                                         FailDataTemp[j,"LONGR_GAGE"],
                                                                         FailDataTemp[j,"LATR_GAGE"]))
  
  FailDataTemp <- join(FailDataTemp,gageDataFullInventory[,c("STAID","peak_begin_date","peak_end_date")])
  
  FailDataTemp$DATE_FAIL_EST <- FailDataTemp$DATE_FAIL
  FailDataTemp[is.na(FailDataTemp$DATE_FAIL_EST),"DATE_FAIL_EST"] <- FailDataTemp[is.na(FailDataTemp$DATE_FAIL_EST),"YR_FAIL"]
  
  FailDataTemp[nchar(FailDataTemp$peak_begin_date)==7,"peak_begin_date"] <- paste(FailDataTemp[nchar(FailDataTemp$peak_begin_date)==7,"peak_begin_date"],"01",sep="-")
  FailDataTemp[nchar(FailDataTemp$peak_begin_date)==4,"peak_begin_date"] <- paste(FailDataTemp[nchar(FailDataTemp$peak_begin_date)==4,"peak_begin_date"],"01","01",sep="-")
  FailDataTemp[nchar(FailDataTemp$peak_end_date)==7,"peak_end_date"] <- paste(FailDataTemp[nchar(FailDataTemp$peak_end_date)==7,"peak_end_date"],"28",sep="-")
  FailDataTemp[nchar(FailDataTemp$peak_end_date)==4,"peak_end_date"] <- paste(FailDataTemp[nchar(FailDataTemp$peak_end_date)==4,"peak_end_date"],"12","31",sep="-")
  
  rows <- which(!is.na(FailDataTemp$DATE_FAIL_EST))
  GageDuringFailure <- sapply(rows, function(i) ifelse(!is.na(FailDataTemp[i,"peak_begin_date"]),
                                                       FailDataTemp[i,"DATE_FAIL_EST"] >= FailDataTemp[i,"peak_begin_date"],
                                                       TRUE)) &
    sapply(rows, function(i) ifelse(!is.na(FailDataTemp[i,"peak_end_date"]),
                                    FailDataTemp[i,"DATE_FAIL_EST"] <= FailDataTemp[i,"peak_end_date"],
                                    TRUE))
  
  FailDataTemp$FAIL_Q_AVAIL <- "NA"
  FailDataTemp[rows,"FAIL_Q_AVAIL"] <- GageDuringFailure
  FailDataTemp$GAGE_ON_BRIDGE <- FailDataTemp$DIST_TO_GAGE < 0.3
  FailDataTemp$HAS_FAIL_DATE <- !is.na(FailDataTemp$DATE_FAIL)
  View(FailDataTemp[,c("ID","FEAT_UND","STANAME","DIST_TO_GAGE","FAIL_Q_AVAIL")])
  cat ("Press [enter] to continue")
  line <- readline()
}
# results of looking at multiple gages:
# 77 should take the 3rd match - X
# 1160 should take 7th match, but 10 on fork, 11 on same branch, {12,13,14,15,16,17} on main, so presumably upstream
# 1293 is good at #1, but 2 is close, and 3,4,6,8 all on same stream
# 3520 should be #2 - X
# 3535 #1 is good, but #4 is 80km upstream
# 3542 should be #2, #4 is upstream 58km - X
mm <- 2 # 3520, 3542
fixIDs <- c(3520, 3542) #us temp data frame above
FailDataMatched3[FailDataMatched3$ID %in% fixIDs,colnames(FailDataTemp)[c(28:44)]] <- FailDataTemp[FailDataTemp$ID %in% fixIDs,colnames(FailDataTemp)[c(28:44)]]
mm <- 3
fixIDs <- 77
FailDataMatched3[FailDataMatched3$ID %in% fixIDs,colnames(FailDataTemp)[c(28:44)]] <- FailDataTemp[FailDataTemp$ID %in% fixIDs,colnames(FailDataTemp)[c(28:44)]]
mm <- 7
fixIDs <- 1160
FailDataMatched3[FailDataMatched3$ID %in% fixIDs,colnames(FailDataTemp)[c(28:44)]] <- FailDataTemp[FailDataTemp$ID %in% fixIDs,colnames(FailDataTemp)[c(28:44)]]
savefile  <- paste(gsub("-","",Sys.Date()),"MatchesToNBIandToGagesRound3.RData",sep="_")
save(FailDataMatched3, file=file.path(DataDir,"Analysis_Results_In_Progress",savefile))

# load("~/Documents/Research/Climate_Scour_Local/R_data/FrequentlyUsedArchiveData/FailDataFrameProcessed.RData")
# FailDataFrame <- FailDataFrame[order(FailDataFrame$ID),]
# FailDataMatched3[,c("YR_BLT","YR_FAIL","DATE_FAIL")] <- FailDataFrame[FailDataFrame$ID %in% FailDataMatched3$ID,c("YR_BLT","YR_FAIL","DATE_FAIL")]

# ----------
# now, quickly go back to check from original matching (which yielded 330 with data) if there were any NBI matches that were missing lat/long that I
# could pull (and that haven't already)
IDmatchedOrigNoLoc <- c(1262,1264,1265,1267,1271,1484,379,392,397,456,459)
IDmatchedOrigNoLoc <- IDmatchedOrigNoLoc[order(IDmatchedOrigNoLoc)]
# which(IDmatchedOrigNoLoc %in% as.integer(df.Gages.Bridges$ID)) # none
# IDmatchedOrigNoLoc[IDmatchedOrigNoLoc %in% FailDataFrameMatched$ID] #4
# IDmatchedOrigNoLoc[IDmatchedOrigNoLoc %in% FailDataMatched2$ID] #1
# IDmatchedOrigNoLoc[IDmatchedOrigNoLoc %in% FailDataMatched3$ID]# none
IDmatchedOrigNoLoc <- IDmatchedOrigNoLoc[!(IDmatchedOrigNoLoc %in% FailDataFrameMatched$ID) & !(IDmatchedOrigNoLoc %in% FailDataMatched2$ID)]
IDmatchedOrigNoLoc[IDmatchedOrigNoLoc %in% names(NumberMatches)[NumberMatches>=1]]

MatchedNUnlocated <- MatchedNUnlocated[order(as.integer(MatchedNUnlocated$ID)),]
FailDataMatched4  <- MatchedNUnlocated[MatchedNUnlocated$ID %in% as.character(IDmatchedOrigNoLoc[IDmatchedOrigNoLoc %in% names(NumberMatches)[NumberMatches>=1]]),]
FailDataMatched4[1,c("LATDD","LONGDD")] <- c(39.490885, -77.427676) # from google maps
FailDataMatched4[2,c("LATDD","LONGDD")] <- c(39.537767, -76.647774) # from google maps
FailDataMatched4[3,c("LATDD","LONGDD")] <- c(39.331766, -76.870107) # from google maps
FailDataMatched4[4,c("LATDD","LONGDD")] <- c(39.422104, -76.710282) # google maps calls it north branch of jones falls, consistent with NBI but not with failure
FailDataMatched4[5,c("LATDD","LONGDD")] <- c(39.363774, -76.969082) # from google maps - note that actually on the s branch, not main
# ID 1267 is actually at 39.425004, -76.688618
FailDataMatched4[4,c(2:7)] <- NA
FailDataMatched4[4,c("LATDD","LONGDD")] <-c(39.425004, -76.688618)
FailDataMatched4$ID <- as.integer(FailDataMatched4$ID)
FailDataMatched4 <- FailDataMatched4[order(FailDataMatched4$ID),]
# temp <- join(FailDataMatched4,FailDataFrame[,c("YR_BLT","YR_FAIL","DATE_FAIL","TYPE","MAT","FAIL_TYPE","FAIL_CAUS","FATAL","COMMENTS")],by="ID")
FailDataMatched4[,c("YR_BLT","YR_FAIL","DATE_FAIL","TYPE","MAT","FAIL_TYPE","FAIL_CAUS","FATAL","COMMENTS")] <- FailDataFrame[FailDataFrame$ID %in% FailDataMatched4$ID,c("YR_BLT","YR_FAIL","DATE_FAIL","TYPE","MAT","FAIL_TYPE","FAIL_CAUS","FATAL","COMMENTS")]
FailDataMatched4<- join(FailDataFrameSubset[CheckIndex,],nbiMatchDataFrame,by="ID",match="all")
FailDataMatched4[,c("ITEM27","ITEM42A","ITEM42B")] <- nbiDataFrame[FailDataMatched4$NBI_ROW,c("ITEM27","ITEM42A","ITEM42B")]

# now to the gages
IDsToReCheck <- names(PossibleMatchRowsAll)[which(!grepl("-",unlist(lapply(PossibleMatchRowsAll,"[[",2))))]
IDsToReCheck <- as.integer(IDsToReCheck)
IDsToReCheck <- IDsToReCheck[IDsToReCheck %in% FailDataMatched4$ID] # 144 of these
IDsToReCheck <- IDsToReCheck[order(IDsToReCheck)]
PossibleMatchRowsMatched4 <- PossibleMatchRowsAll[names(PossibleMatchRowsAll) %in% as.character(IDsToReCheck)]
PossibleMatchRowsMatched4 <- PossibleMatchRowsMatched4[order(as.integer(names(PossibleMatchRowsMatched4)))] 
# there are 4, conveniently all with NBI, and also conveniently they all have 1 match
FailDataMatched4 <- FailDataMatched4[FailDataMatched4$ID %in% as.integer(names(PossibleMatchRowsMatched4)),]
GageRows <- unlist(lapply(PossibleMatchRowsMatched4,"[[",2))
GageRows["1271"] <- "974" # is matching to patapsco, not s. branch as should because FailDF had the wrong stream name
FailDataMatched4[,colnames(gageDataFrame)[c(1:2,5:9,25)]] <- gageDataFrame[GageRows,c(1:2,5:9,25)]
FailDataMatched4 <- join(FailDataMatched4,gageDataFullInventory[,c("STAID","peak_begin_date","peak_end_date")]) # all are formatted
nMatch <- nrow(FailDataMatched4)
FailDataMatched4$LONGR <- deg2rad(FailDataMatched4$LONGDD)
FailDataMatched4$LATR  <- deg2rad(FailDataMatched4$LATDD)
FailDataMatched4$LONGR_GAGE  <- deg2rad(FailDataMatched4$LNG_GAGE)
FailDataMatched4$LATR_GAGE   <- deg2rad(FailDataMatched4$LAT_GAGE)
FailDataMatched4$DIST_TO_GAGE <-  sapply(1:nMatch, function(j) gcd_slc(FailDataMatched4[j,"LONGR"],
                                                                       FailDataMatched4[j,"LATR"],
                                                                       FailDataMatched4[j,"LONGR_GAGE"],
                                                                       FailDataMatched4[j,"LATR_GAGE" ]))
FailDataMatched4$DATE_FAIL_EST <- FailDataMatched4$DATE_FAIL
FailDataMatched4[is.na(FailDataMatched4$DATE_FAIL_EST),"DATE_FAIL_EST"] <- FailDataMatched4[is.na(FailDataMatched4$DATE_FAIL_EST),"YR_FAIL"]

rows <- which(!is.na(FailDataMatched4$DATE_FAIL_EST))
GageDuringFailure <- sapply(rows, function(i) ifelse(!is.na(FailDataMatched4[i,"peak_begin_date"]),
                                                     FailDataMatched4[i,"DATE_FAIL_EST"] >= FailDataMatched4[i,"peak_begin_date"],
                                                     TRUE)) &
                                              sapply(rows, function(i) ifelse(!is.na(FailDataMatched4[i,"peak_end_date"]),
                                                     FailDataMatched4[i,"DATE_FAIL_EST"] <= FailDataMatched4[i,"peak_end_date"],
                                                     TRUE))

FailDataMatched4$FAIL_Q_AVAIL <- "NA"
FailDataMatched4[rows,"FAIL_Q_AVAIL"] <- GageDuringFailure
FailDataMatched4$GAGE_ON_BRIDGE <- FailDataMatched4$DIST_TO_GAGE < 0.3
FailDataMatched4$HAS_FAIL_DATE <- !is.na(FailDataMatched4$DATE_FAIL)

savefile  <- paste(gsub("-","",Sys.Date()),"MatchesToNBIandToGagesRound4.RData",sep="_")
save(FailDataMatched4, file=file.path(DataDir,"Analysis_Results_In_Progress",savefile))

View(FailDataMatched4[FailDataMatched4$DIST_TO_GAGE<15,c("ID","FEAT_UND","STANAME","DIST_TO_GAGE","FAIL_Q_AVAIL")])

# now clean all of this up so that have a decent data frame
df.Gages.Bridges$ID <- as.integer(df.Gages.Bridges$ID)
FailDataMatched <- df.Gages.Bridges[,c(1:28,30,32,33,38,43,49,50,51:64)]
FailDataMatched[,c("STAID",   "STANAME",   "LAT_GAGE",  "LNG_GAGE",   "HUC02",  "DRAIN_SQKM",    "peak_begin_date",    "peak_end_date")] <- FailDataMatched[,c("site_no", "station_nm","dec_lat_va","dec_long_va","huc_cd", "drain_area_va", "BeginDateFormatted", "EndDateFormatted")] 
FailDataMatched <- FailDataMatched[,c(1:27,36:57)]
colnames(FailDataMatched)[colnames(FailDataMatched)%in% c("LATR_G","LONGR_G")] <- c("LATR_GAGE","LONGR_GAGE")
ColsToKeep <- c("ID","STFIPS","STATE_CODE" ,"LOCATION" , "FEAT_UND","TYPE","MAT","YR_BLT", "YR_FAIL",  "DATE_FAIL", "FAIL_TYPE","FAIL_CAUS","COMMENTS",
                "NBI_ROW","ITEM3", "ITEM5D","ITEM6A","ITEM7","ITEM8", "ITEM9", "ITEM27","ITEM42A","ITEM42B","LATDD","LONGDD", "LATR","LONGR",             
                "STAID", "STATE","STANAME", "DRAIN_SQKM" ,"HUC02","FIPS_GAGE", "peak_begin_date", "peak_end_date","LAT_GAGE", "LNG_GAGE","LONGR_GAGE", "LATR_GAGE",     
                "HAS_FAIL_DATE", "DATE_FAIL_EST", "DIST_TO_GAGE","FAIL_Q_AVAIL", "GAGE_ON_BRIDGE",
                "FAIL_Q","TIMES_Q_EXCEED","FAIL_Q_COMMENT", "MAX_Q","MAX_Q_DATE")
FailDataMatchedTemp  <- FailDataMatched[,colnames(FailDataMatched) %in% ColsToKeep]
FactorCol <- colnames(FailDataMatchedTemp)[which(sapply(1:ncol(FailDataMatchedTemp),function(j) class(FailDataMatchedTemp[,j]))=="factor")]
FailDataMatchedTemp[,FactorCol] <- sapply(FactorCol, function(j) as.character(FailDataMatchedTemp[,j]))
FailDataMatched1Temp <- FailDataMatched1[,colnames(FailDataMatched1) %in% ColsToKeep]
FailDataMatched1Temp$HUC02 <- as.character(FailDataMatched1Temp$HUC02)
FailDataMatched1Temp$STATE <- as.character(FailDataMatched1Temp$STATE)
FailDataMatched2Temp <- FailDataMatched2[,colnames(FailDataMatched2) %in% ColsToKeep]
FailDataMatched3Temp <- FailDataMatched3[,colnames(FailDataMatched3) %in% ColsToKeep]
FailDataMatched4Temp <- FailDataMatched4[,colnames(FailDataMatched4) %in% ColsToKeep]

df.Fail.NBI.Gage.All           <- FailDataMatchedTemp
df.Fail.NBI.Gage.All$ITEM3     <- integer(nrow(df.Fail.NBI.Gage.All))
df.Fail.NBI.Gage.All$STATE     <- character(nrow(df.Fail.NBI.Gage.All))
df.Fail.NBI.Gage.All$FIPS_GAGE <- integer(nrow(df.Fail.NBI.Gage.All))
df.Fail.NBI.Gage.All$ITEM27    <- character(nrow(df.Fail.NBI.Gage.All))
df.Fail.NBI.Gage.All$ITEM42A   <- integer(nrow(df.Fail.NBI.Gage.All))
df.Fail.NBI.Gage.All$ITEM42B   <- integer(nrow(df.Fail.NBI.Gage.All))
df.Fail.NBI.Gage.All[(nrow(df.Fail.NBI.Gage.All)+1):(nrow(df.Fail.NBI.Gage.All)+nrow(FailDataMatched1Temp)),colnames(FailDataMatched1Temp)] <- FailDataMatched1Temp
df.Fail.NBI.Gage.All$DATE_FAIL_EST<- character(nrow(df.Fail.NBI.Gage.All))
df.Fail.NBI.Gage.All[(nrow(df.Fail.NBI.Gage.All)+1):(nrow(df.Fail.NBI.Gage.All)+nrow(FailDataMatched2Temp)),colnames(FailDataMatched2Temp)] <- FailDataMatched2Temp
df.Fail.NBI.Gage.All[(nrow(df.Fail.NBI.Gage.All)+1):(nrow(df.Fail.NBI.Gage.All)+nrow(FailDataMatched3Temp)),colnames(FailDataMatched3Temp)] <- FailDataMatched3Temp
df.Fail.NBI.Gage.All[(nrow(df.Fail.NBI.Gage.All)+1):(nrow(df.Fail.NBI.Gage.All)+nrow(FailDataMatched4Temp)),colnames(FailDataMatched4Temp)] <- FailDataMatched4Temp
df.Fail.NBI.Gage.All <- df.Fail.NBI.Gage.All[,ColsToKeep]
savefile  <- paste(gsub("-","",Sys.Date()),"MatchesToNBIandToGagesAllDataPreSelection.RData",sep="_")
save(df.Fail.NBI.Gage.All, file=file.path(DataDir,"Analysis_Results_In_Progress",savefile))


reMatchedIDs <- unlist(c(df.Gages.Bridges$ID,FailDataFrameMatched$ID, FailDataMatched2$ID, FailDataMatched3$ID, FailDataMatched4$ID)) # 145 duplicates
reMatchedIDsUnique <- unique(reMatchedIDs)
whichDuplicate <- sapply(reMatchedIDsUnique, function(ID) sum(reMatchedIDs==ID)>1)
reMatchedIDsUnique[whichDuplicate]

