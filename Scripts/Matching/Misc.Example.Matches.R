# finding good examples for pre-processing and matching
View(df.Fail.NBI.Gage[,c("STATE_CODE","ID","NBI_ROW","BIN","ITEM8","ITEM3","LOCATION","ITEM7","ITEM5B","ITEM5D","FEAT_UND","ITEM6A")])
load("~/hydraulic.failures/Data/FailDataFrame-Processed-RoadCountyStream.RData")
View(FailDataFrame[FailDataFrame$ID %in% df.Fail.NBI.Gage$ID,]) 
unique(df.USbridges.water[df.USbridges.water$STFIPS=="54","ITEM3"])

# 1st example, route, 1 match
View(FailDataFrame[FailDataFrame$ID == "132",]) 
sum(df.USbridges.water$STFIPS=="51")
sum(df.USbridges.water$STFIPS=="51" & df.USbridges.water$ITEM3==127,na.rm = T)
sum(df.USbridges.water$STFIPS=="51" & df.USbridges.water$ITEM3==127 & grepl("607",df.USbridges.water$ITEM5D),na.rm = T)
sum(df.USbridges.water$STFIPS=="51" & df.USbridges.water$ITEM3==127 & grepl("dickerson",df.USbridges.water$ITEM6A),na.rm = T)
View(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID=="132",c(1:21, 41:51)])


# 2nd, county only from road
View(FailDataFrame[FailDataFrame$ID == "504",])
sum(df.USbridges.water$STFIPS=="30")
sum(df.USbridges.water$STFIPS=="30" & df.USbridges.water$ITEM3==35,na.rm = T)
which(df.USbridges.water$STFIPS=="30" & df.USbridges.water$ITEM3==35)
#2nd, road to stream
sum(df.USbridges.water$STFIPS=="30" & df.USbridges.water$ITEM3==35 & grepl("milk",df.USbridges.water$ITEM6A),na.rm = T)
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="30" & df.USbridges.water$ITEM3==35 & grepl("milk",df.USbridges.water$ITEM6A),])
which(row.names(df.USbridges.water[df.USbridges.water$STFIPS=="30" & df.USbridges.water$ITEM3==35 ,])=="314640")
View(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID=="504",c(1:21, 41:51)])
# non-matches
View(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID=="504",c(1:21)])
View(df.USbridges.water[df.USbridges.water$STFIPS=="30" & df.USbridges.water$ITEM3==35 & grepl("milk",df.USbridges.water$ITEM6A),c(1:46)][c(1:3,5),])

View(FailDataFrame[FailDataFrame$ID == "1168",]) 

# 3rd stream
View(FailDataFrame[FailDataFrame$ID == "1164",])
sum(df.USbridges.water$STFIPS=="54")
sum(df.USbridges.water$STFIPS=="54" & df.USbridges.water$ITEM3==31,na.rm = T)
sum(df.USbridges.water$STFIPS=="54" & df.USbridges.water$ITEM3==31 & grepl("potomac",df.USbridges.water$ITEM6A),na.rm = T)
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="54" & df.USbridges.water$ITEM3==31 & grepl("potomac",df.USbridges.water$ITEM6A),])
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="54" & df.USbridges.water$ITEM3==31,])
which(row.names(df.USbridges.water[df.USbridges.water$STFIPS=="54" & df.USbridges.water$ITEM3==31,])=="582905")
View(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID=="1164",c(1:21, 41:51)])
View(df.USbridges.water[df.USbridges.water$STFIPS=="54" & df.USbridges.water$ITEM3==31 & grepl("potomac",df.USbridges.water$ITEM6A),c(1:46)][c(2:5),])

# 4th, BIN helps, 1675
View(FailDataFrame[FailDataFrame$ID == "1673",])
sum(df.USbridges.water$STFIPS=="28")
sum(df.USbridges.water$STFIPS=="28" & df.USbridges.water$ITEM3==127,na.rm = T)
sum(df.USbridges.water$STFIPS=="28" & df.USbridges.water$ITEM3==127 & grepl("strong",df.USbridges.water$ITEM6A),na.rm = T)
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="28" & df.USbridges.water$ITEM3==127 & grepl("strong",df.USbridges.water$ITEM6A),])
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="28" & df.USbridges.water$ITEM3==127 & grepl("strong river",df.USbridges.water$ITEM7),])
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="28" & df.USbridges.water$ITEM3==127 & grepl("strong river",df.USbridges.water$ITEM7) & grepl("strong",df.USbridges.water$ITEM6A),])
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="28" & grepl("64",df.USbridges.water$ITEM8) & grepl("144",df.USbridges.water$ITEM8),])
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="28" & grepl("SA64-144",df.USbridges.water$ITEM8),])
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="28" & grepl("64",df.USbridges.water$ITEM8)& grepl("144",df.USbridges.water$ITEM8)& grepl("strong",df.USbridges.water$ITEM6A),])
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="28" & df.USbridges.water$ITEM3==127 & grepl("64",df.USbridges.water$ITEM8)& grepl("144",df.USbridges.water$ITEM8),])
View(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID=="1673",c(1:21, 41:51)])
View(df.USbridges.water[df.USbridges.water$STFIPS=="28" & df.USbridges.water$ITEM3==127 & grepl("64",df.USbridges.water$ITEM8)& grepl("144",df.USbridges.water$ITEM8),c(1:46)])

# 5th, not able to confirm match, ID 28
View(FailDataFrame[FailDataFrame$ID == "28",])
sum(df.USbridges.water$STFIPS=="54")
sum(df.USbridges.water$STFIPS=="54" & df.USbridges.water$ITEM3==35,na.rm = T)
sum(df.USbridges.water$STFIPS=="54" & df.USbridges.water$ITEM3==35 & grepl("little mill",df.USbridges.water$ITEM6A),na.rm = T)
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="54" & df.USbridges.water$ITEM3==35 & grepl("little mill",df.USbridges.water$ITEM6A),])
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="54" & df.USbridges.water$ITEM3==35 & grepl("alto",df.USbridges.water$ITEM7),])
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="54" & grepl("18",df.USbridges.water$ITEM8) & grepl("87",df.USbridges.water$ITEM8),])
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="54" & grepl("18-87",df.USbridges.water$ITEM8),])
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="54" & grepl("18",df.USbridges.water$ITEM8)& grepl("87",df.USbridges.water$ITEM8)& 
                               grepl("little",df.USbridges.water$ITEM6A) & grepl("mill",df.USbridges.water$ITEM6A),])
row.names(df.USbridges.water[df.USbridges.water$STFIPS=="54" & df.USbridges.water$ITEM3==35 & grepl("18",df.USbridges.water$ITEM8)& grepl("87",df.USbridges.water$ITEM8),])
# View(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID=="28",c(1:21, 41:51)])
View(df.USbridges.water[df.USbridges.water$STFIPS=="54" & df.USbridges.water$ITEM3==35 & grepl("little mill",df.USbridges.water$ITEM6A),c(1:46)])
View(df.USbridges.water[df.USbridges.water$STFIPS=="54" & grepl("18",df.USbridges.water$ITEM8)& grepl("87",df.USbridges.water$ITEM8),])
