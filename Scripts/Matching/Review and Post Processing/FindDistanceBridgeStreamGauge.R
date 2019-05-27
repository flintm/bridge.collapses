
# resolution not good enough, trying with terrain file
library(ggplot2)
library(ggmap)
library(maps)
library(sp)
library(parallel)

# FIND GEODESIC DISTANCE TO STREAM GAUGE
StateData     <- data.frame(STATE_CODE="VA", STFIPS=51, STATE_FIPS="51",stringsAsFactors = FALSE)
MatchesSubset <- MatchesFullData[MatchesFullData$STATE_CODE == StateData$STATE_CODE,]
GaugesSubset  <- df.ds.gauges.cont[df.ds.gauges.cont$STATE==StateData$STATE_CODE,]
nMatch        <- nrow(MatchesSubset)
nGauge        <- nrow(GaugesSubset)

# Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)
MatchesSubset$LONGR <- deg2rad(MatchesSubset$LONGDD)
MatchesSubset$LATR  <- deg2rad(MatchesSubset$LATDD)
GaugesSubset$LONGR  <- deg2rad(GaugesSubset$LNG_GAGE)
GaugesSubset$LATR   <- deg2rad(GaugesSubset$LAT_GAGE)
MatchesSubset$NO    <- c(1:nMatch)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}

BridgeGaugeDist <- sapply(1:nGauge, function(i) sapply(1:nMatch, function(j) gcd.slc(MatchesSubset[j,"LONGR"],
                                                                                   MatchesSubset[j,"LATR"],
                                                                                   GaugesSubset[i,"LONGR"],
                                                                                   GaugesSubset[i,"LATR" ])))
BridgeGaugeDistNear <- sapply(1:nMatch,function(i) min(BridgeGaugeDist[i,]))

hist(BridgeGaugeDistNear[IndexCloserXkm],breaks=20, freq=TRUE,main="Distance to Nearest Gauge (VA Bridges)",xlab="Distance, d (km)",
     ylab="Number of Failed Bridges with Gauge at Distance d (of 38)")#,xlim=c(0,40),ylim=c(0,15))
sum(BridgeGaugeDistNear<=15) # 46 with distance < 10k
sum(BridgeGaugeDistNear<=5) # 28 with distance <= 5k <- start here

NearestGaugeLoc <- GaugesSubset[sapply(1:nMatch, function(i) which(BridgeGaugeDist[i,]==BridgeGaugeDistNear[i])),]

Xkm <- 1
IndexCloserXkm <- which(BridgeGaugeDistNear<=Xkm)
nXkm <- sum(BridgeGaugeDistNear<=Xkm)
# IndexCloserXkm <- which(BridgeGaugeDistNear<=Xkm & BridgeGaugeDistNear > 5)
# nXkm <- sum(BridgeGaugeDistNear<=Xkm & BridgeGaugeDistNear > 5)
BridgeGaugeDF <- MatchesSubset[IndexCloserXkm,c("LATDD","LONGDD")]
BridgeGaugeDF$TYPE <- "Bridge"
BridgeGaugeDF[c((nXkm+1):(2*nXkm)),c("LATDD","LONGDD")] <- NearestGaugeLoc[IndexCloserXkm,c("LAT_GAGE","LNG_GAGE")]
BridgeGaugeDF[c((nXkm+1):(2*nXkm)),"TYPE"] <- "Gauge"
              
# add hydro lines to plot  - subset from full dataset
# NYriversSpatial <- ds.UShydro[ds.UShydro$STFIPS==StateData$STATE_FIPS,]
# ARriversSpatial <- ds.UShydro[ds.UShydro$STFIPS==StateData$STATE_FIPS,]
# # plot
# plot(ds.states[ds.states$STATE_FIPS == "5",])#StateData$STATE_FIPS,])#,xlim=c(-77,-74),ylim=c(42.5,44))
# # plot(NearestGaugeLoc[IndexCloserXkm,"LNG_GAGE"],NearestGaugeLoc[IndexCloserXkm,"LAT_GAGE"],col="blue",pch=4,cex=0.7)
# plot(NearestGaugeLoc[,"LNG_GAGE"],NearestGaugeLoc[,"LAT_GAGE"],col="blue",pch=4,cex=0.7)
# points(MatchesSubset[IndexCloserXkm,"LONGDD"],MatchesSubset[IndexCloserXkm,"LATDD"],col="red",pch=17,cex=1)
# title("Failed Bridges in New York with Gauge within 5km")
#  lines(ARriversSpatial)


BridgeID <- 1
GaugeID <- BridgeID + nXkm
MapLon <- 0.5*(BridgeGaugeDF[BridgeID,"LONGDD"]+BridgeGaugeDF[GaugeID,"LONGDD"])
MapLat <- 0.5*(BridgeGaugeDF[BridgeID,"LATDD"]+BridgeGaugeDF[GaugeID,"LATDD"])
basemap <- get_map(location=c(lon=MapLon,lat=MapLat),zoom=12,maptype="terrain",source="google")
#   basemap <- get_map(location="Mississippi State",zoom=3,maptype="terrain",source="google")
map1 <- ggmap(basemap, extent="panel",base_layer = ggplot(BridgeGaugeDF, aes(x=BridgeGaugeDF$LONGDD,y=BridgeGaugeDF$LATDD))) 
map1 <- map1 + geom_point(aes(shape=TYPE), size=6)
print(map1)

BridgeGaugeDF[c(BridgeID,GaugeID),"GAUGE_USE"] <- FALSE
BridgeGaugeDF[c(BridgeID,GaugeID),"GAUGE_USE"] <- TRUE

#################
# find all bridges with gauge on bridge, then analyze data - 2015-02-11
nMatches <- nrow(MatchesFullData)
nGauges <- nrow(df.ds.gauges.cont)
deg2rad <- function(deg) return(deg*pi/180)
MatchesFullData$LONGR    <- deg2rad(MatchesFullData$LONGDD)
MatchesFullData$LATR     <- deg2rad(MatchesFullData$LATDD)
df.ds.gauges.cont$LONGR  <- deg2rad(df.ds.gauges.cont$LNG_GAGE)
df.ds.gauges.cont$LATR   <- deg2rad(df.ds.gauges.cont$LAT_GAGE)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- R * (2 * asin(min(1,sqrt(a))))
  return(c) # Distance in km
}
library(parallel)
GaugeBridgeDistances <- mclapply(1:nMatches, function(i) sapply(1:nGauges, function(j) gcd.hf(MatchesFullData[i,"LONGR"],
                                                                                         MatchesFullData[i,"LATR"],
                                                                                         df.ds.gauges.cont[j,"LONGR"],
                                                                                         df.ds.gauges.cont[j,"LATR" ])),mc.cores=3)
GaugeBridgeDistanceDF <- as.data.frame(GaugeBridgeDistances)
HowManyNAs <- sapply(1:ncol(GaugeBridgeDistanceDF),function(j) sum(is.na(GaugeBridgeDistanceDF[,j])))
HasGaugeOnBridge <- sapply(1:ncol(GaugeBridgeDistanceDF), function(j) any(GaugeBridgeDistanceDF[!is.na(GaugeBridgeDistanceDF[,j]),j]<0.5))
MinGaugeDist <- sapply(1:ncol(GaugeBridgeDistanceDF), function(j) min(GaugeBridgeDistanceDF[!is.na(GaugeBridgeDistanceDF[,j]),j]))
WhichGaugeNeareast <- sapply(1:ncol(GaugeBridgeDistanceDF), function(j) which(GaugeBridgeDistanceDF[,j]==MinGaugeDist[j]))


# plot for all states
plot(ds.states[!(ds.states$STATE_FIPS %in% NonContString),])
points(df.ds.gauges.cont[WhichGaugeNeareast[HasGaugeOnBridge],"LNG_GAGE"],df.ds.gauges.cont[WhichGaugeNeareast[HasGaugeOnBridge],"LAT_GAGE"],col="blue",pch=4,cex=0.7)
points(MatchesFullData[HasGaugeOnBridge,"LONGDD"],MatchesFullData[HasGaugeOnBridge,"LATDD"],col="red",pch=17,cex=1)

# plot for one state
plot(ds.states[ds.states$STATE_FIPS == "51",])#StateData$STATE_FIPS,])#,xlim=c(-77,-74),ylim=c(42.5,44))
points(df.ds.gauges.cont[df.ds.gauges.cont$STATE=="VA","LNG_GAGE"],df.ds.gauges.cont[df.ds.gauges.cont$STATE=="VA","LAT_GAGE"],col="blue",pch=4,cex=0.7)
points(MatchesFullData[28,"LONGDD"],MatchesFullData[28,"LATDD"],col="red",pch=17,cex=1)

# plot for one bridge with terrain map
nGaugeOnBridge <- sum(HasGaugeOnBridge)
BridgeLoc <- MatchesFullData[HasGaugeOnBridge,]
GaugeLoc <- df.ds.gauges.cont[WhichGaugeNeareast[HasGaugeOnBridge],]
BridgeGaugeDF <- BridgeLoc
BridgeGaugeDF$TYPE <- "Bridge"
BridgeGaugeDF[c((nGaugeOnBridge+1):(2*nGaugeOnBridge)),c("LATDD","LONGDD")] <- GaugeLoc[,c("LAT_GAGE","LNG_GAGE")]
BridgeGaugeDF[c((nGaugeOnBridge+1):(2*nGaugeOnBridge)),"TYPE"] <- "Gauge"

BridgeID <- 13
GaugeID <- BridgeID + nGaugeOnBridge
MapLon <- 0.5*(BridgeGaugeDF[BridgeID,"LONGDD"]+BridgeGaugeDF[GaugeID,"LONGDD"])
MapLat <- 0.5*(BridgeGaugeDF[BridgeID,"LATDD"]+BridgeGaugeDF[GaugeID,"LATDD"])
basemap <- get_map(location=c(lon=MapLon,lat=MapLat),zoom=16,maptype="terrain",source="google")
map1 <- ggmap(basemap, extent="panel",base_layer = ggplot(BridgeGaugeDF, aes(x=BridgeGaugeDF$LONGDD,y=BridgeGaugeDF$LATDD))) 
plotTitle <- paste(BridgeLoc[BridgeID,"STATE_CODE"],BridgeLoc[BridgeID,"LOCATION"],BridgeLoc[BridgeID, "FEAT_UND"],"Fail Year",BridgeLoc[BridgeID,"YR_FAIL"],sep=" - ")
map1 <- map1 + geom_point(aes(shape=TYPE, color=TYPE), size=6) + xlab("Longitude") + ylab("Latitude")
map1 <- map1 + ggtitle(plotTitle)
print(map1)



## ANALYSIS OF GAGES
GageData <- read.csv(file="/Users/MM/Documents/Research/Climate_Scour_Local/GagesOnBridgesData.csv",header=TRUE)
df.Gages.Bridges <- BridgeLoc
df.Gages.Bridges[,c("STAID","STANAME","DRAIN_SQKM","HUC02","LAT_GAGE","LNG_GAGE","STATE",
"YR_DIS_BEG","YR_DIS_END","YR_HT_BEG","YR_HT_END")] <- GageData[,c("STAID","STANAME","DRAIN_SQKM","HUC02","LAT_GAGE","LNG_GAGE","STATE",
"YR_DIS_BEG","YR_DIS_END","YR_HT_BEG","YR_HT_END")]
GageDuringFailure <- df.Gages.Bridges$YR_FAIL > df.Gages.Bridges$YR_DIS_BEG & sapply(1:nrow(df.Gages.Bridges), 
                                                                                     function(i) ifelse(!is.na(df.Gages.Bridges[i,"YR_DIS_END"]),
                                                                                                        df.Gages.Bridges[i,"YR_FAIL"] < df.Gages.Bridges[i,"YR_DIS_END"],
                                                                                                        TRUE))
df.Gages.Bridges$FAIL_DIS <- "0"
df.Gages.Bridges[GageDuringFailure,"FAIL_DIS"] <- "1"
df.Gages.Bridges$DIST_TO_GAGE <- MinGaugeDist
df.Gages.Bridges$DIST_TO_GAGE <- sapply(1:nrow(df.Gages.Bridges,function(i) gcd.slc(df.Gages.Bridges[i,""])


## ANALYSIS OF BRIDGES
hist(df.Gages.Bridges$YR_FAIL,breaks=10, freq=TRUE,main="Year of Failure for Bridges with Gages",xlab="Failure Year, F_Y",
     ylab="Number of Bridges with Failure Year = F_Y ( out of 13)")
hist(MatchesFullData$YR_FAIL,breaks=10, freq=TRUE,main="Year of Failure for All Located Bridges",xlab="Failure Year, F_Y",
     ylab="Number of Bridges with Failure Year = F_Y ( out of 330)")

############
# for NY only
basemap1 <- get_map(location="New York State",zoom=7,maptype="road",source="google",color="bw")
map2 <- ggmap(basemap1,maprange=TRUE,darken=c(0.4,"black"),legend="bottom",base_layer = ggplot(df.Gages.Bridges, aes(x=LONGDD,y=LATDD)))
map2 <- map2 + ggtitle(plotTitle) + xlab("") + ylab("")
map3 <- map2 + geom_point(aes(color=HAS_FAIL_DATE,shape=GAUGE_ON_BRIDGE,fill=FAIL_Q_AVAIL), size=4,data=df.Gages.Bridges,alpha=0.5) 
map4 <- map3 + scale_shape_manual(values=c(21,23)) + scale_fill_manual(values=c("red","blue"),guide=guide_legend(override.aes=aes(shape=22)))+scale_color_manual(values=c("white","black"),guide=guide_legend(override.aes=aes(shape=22)))
map5 <- map4 + geom_point(aes(color=HAS_FAIL_DATE),size=6, data=subset(df.Gages.Bridges,as.logical(FAIL_Q_AVAIL)==TRUE & DIST_TO_GAUGE < 3)) + scale_color_manual(values=c("white","black"))
print(map5)
# Plot all bridges on map - emphasis on whether there is a gauge at the right time

basemap2 <- get_map(location="United States",zoom=3,maptype="road",source="google",color="bw")
plotTitle <- "Status of Nearest Gauge to Failed Bridge"
fortifies_states <- fortify(ds.states)
map2 <- ggmap(basemap2, extent="normal",darken=c(0.4,"black"),legend="bottom",base_layer = ggplot(df.Gages.Bridges, aes(x=LONGDD,y=LATDD)))+ylim(25,52)+xlim(-125,-60) 
map2 <- map2 + ggtitle(plotTitle) + xlab("") + ylab("")
map3 <- map2 + geom_point(aes(color=HAS_FAIL_DATE,shape=GAGE_ON_BRIDGE,fill=FAIL_Q_AVAIL), size=4,data=df.Gages.Bridges,alpha=0.5) 
map4 <- map3 + scale_shape_manual(values=c(21,23)) + scale_fill_manual(values=c("red","blue"),guide=guide_legend(override.aes=aes(shape=22)))+scale_color_manual(values=c("white","black"),guide=guide_legend(override.aes=aes(shape=22)))
print(map4)
# map5 <- map4 + geom_text(aes(label=YR_FAIL),hjust=0, vjust=0,color="red",size=6,angle=30)
# map3 <- map2 + geom_polygon(aes(x = long, y = lat, group = group), data = fortifies_states, color = "white", fill = "black", alpha = .2, size = .3)
#map5 <- map4 + geom_point(aes(color=FAIL_Q_AVAIL,shape=GAUGE_ON_BRIDGE,fill=HAS_FAIL_DATE),size=6,data=subset(df.Gages.Bridges,GAUGE_ON_BRIDGE==TRUE & FAIL_Q_AVAIL==TRUE))
# map6 <- map5 + scale_colour_discrete(guide=guide_legend(override.aes=aes(shape=21)))
rm(basemap2,map2,map3)

# emphasis on distance to nearest gauge
plotTitle2 <- "Distance from Nearest Gage to Failed Bridge"
map21 <- ggmap(basemap2, extent="normal",darken=c(0.4,"black"),legend="bottom",base_layer = ggplot(df.Gages.Bridges, aes(x=LONGDD,y=LATDD)))+ylim(25,52)+xlim(-125,-60) 
map22 <- map21 + ggtitle(plotTitle2) + xlab("") + ylab("") 
map24 <- map22 + geom_point(aes(color=FAIL_Q_AVAIL,size=DIST_TO_GAGE), data=df.Gages.Bridges,alpha=0.6) + scale_color_manual(values=c("red","blue"))#+ scale_color_hue(l=45)+ scale_shape_manual(values=c(4,16)) 
print(map24)

plotTitle3 <- "Potentially Useful Sites"
map31 <- ggmap(basemap2, extent="normal",darken=c(0.4,"black"),legend="bottom",base_layer = ggplot(df.Gages.Bridges, aes(x=LONGDD,y=LATDD)))+ylim(25,52)+xlim(-125,-60) 
map32 <- map31 + ggtitle(plotTitle3) + xlab("") + ylab("") 
map34 <- map32 + geom_point(aes(color=HAS_FAIL_DATE),size=3, data=subset(df.Gages.Bridges,as.logical(FAIL_Q_AVAIL)==TRUE & DIST_TO_GAGE < 0.3)) + scale_color_manual(values=c("purple","green")) 
print(map34)

multiplot(map4,map24,map34)
##############
# ANALYSIS OF BRIDGES
WithGages <- data.frame(YR_FAIL=df.Gages.Bridges$YR_FAIL)
AllBridges <- data.frame(YR_FAIL=MatchesFullData[!HasGaugeOnBridge,"YR_FAIL"])
WithGages$GAUGE <- "On bridge"
AllBridges$GAUGE <- "Not on bridge"
FailYears <- rbind(WithGages,AllBridges)
FailYears <- FailYears[!is.na(FailYears$YR_FAIL),] # had some NAs
# hist(df.Gages.Bridges$YR_FAIL,breaks=10, freq=TRUE,main="Year of Failure for Bridges with Gages",xlab="Failure Year, F_Y",
#      ylab="Number of Bridges with Failure Year = F_Y ( out of 13)")
# hist(MatchesFullData$YR_FAIL,breaks=10, freq=TRUE,main="Year of Failure for All Located Bridges",xlab="Failure Year, F_Y",
#      ylab="Number of Bridges with Failure Year = F_Y ( out of 330)")

hist1 <- ggplot(FailYears, aes(YR_FAIL, fill = GAUGE)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
hist1 <- hist1 + xlab("Failure Year") + ggtitle("Comparison of Bridges with Gage on Bridge (N=13) and Bridges with Nearest Gage Not on Bridge (N=317)")
hist1 <- hist1 + xlim(1840,2020) + ylim(0, 0.06)
print(hist1)

NearestGageIDs <- as.character(df.ds.gauges.cont[WhichGaugeNeareast,"STAID"])

coln <- c("agency_cd ",          "site_no",	    "station_nm",	         "site_tp_cd",	   "dec_lat_va",	    "dec_long_va",	"coord_acy_cd",	
          "dec_coord_datum_cd",	"state_cd",	    "county_cd",	         "huc_cd",	       "basin_cd",	      "topo_cd",	     "construction_dt",	
          "inventory_dt",	     "drain_area_va",	"contrib_drain_area_va","reliability_cd",	"peak_begin_date", "peak_end_date","peak_count_nu", "NA")
colc <- c("character",          "character",    "character",            "character",      "numeric",        "numeric",       "character",
          "character",          "integer",       "integer",             "character",      "character",      "character",     "character",
          "character",           "numeric",       "numeric",             "character",     "character",     "character",      "character", "character")
GageDataAll <- read.csv(file="/Users/MM/Documents/Research/Climate_Scour_Local/GageData330NearestBridgesWithDate.csv",header=FALSE,col.names=coln,
                        colClasses = colc)
GageDataAll <- GageDataAll[,c(1:21)]
GageIDs <- data.frame(site_no=NearestGageIDs,stringsAsFactors=FALSE)
GageData <- merge(GageIDs,GageDataAll,by="site_no",all.x=TRUE)
GageData$YR_DIS_END <- as.integer(substr(GageData$peak_end_date,1,4))
library(stringr)
for (i in 1:nrow(GageData)){
  if (grepl("/",GageData[i,"peak_begin_date"])){
    GageData[i,"YR_DIS_BEG"] <- as.integer(str_extract(GageData[i,"peak_begin_date"],"[[:digit:]]{4,4}"))
  }
  else{
    GageData[i,"YR_DIS_BEG"] <- as.integer(substr(GageData[i,"peak_begin_date"],1,4))
  }
}

df.Gages.Bridges <- MatchesFullData
df.Gages.Bridges[,colnames(GageData)] <- GageData
GageDuringFailure <- df.Gages.Bridges$YR_FAIL > df.Gages.Bridges$YR_DIS_BEG & sapply(1:nrow(df.Gages.Bridges), 
                                                                                     function(i) ifelse(!is.na(df.Gages.Bridges[i,"YR_DIS_END"]),
                                                                                                        df.Gages.Bridges[i,"YR_FAIL"] < df.Gages.Bridges[i,"YR_DIS_END"],
                                                                                                        TRUE))
df.Gages.Bridges$FAIL_Q_AVAIL <- "NA"
df.Gages.Bridges[GageDuringFailure[!is.na(GageDuringFailure)],"FAIL_Q_AVAIL"] <- "TRUE"
df.Gages.Bridges[!GageDuringFailure[!is.na(GageDuringFailure)],"FAIL_Q_AVAIL"] <- "FALSE"
df.Gages.Bridges$GAUGE_ON_BRIDGE <- HasGaugeOnBridge
df.Gages.Bridges$HAS_FAIL_DATE <- df.Gages.Bridges$DATE_FAIL!=""



# histogram of gage begin years
GagesOnBridges    <- data.frame(YR_DIS_BEG=GageData[HasGaugeOnBridge,"YR_DIS_BEG"])
GagesNotOnBridges <- data.frame(YR_DIS_BEG=GageData[!HasGaugeOnBridge,"YR_DIS_BEG"])
GagesOnBridges$GAUGE <- "On bridge"
GagesNotOnBridges$GAUGE <- "Not on bridge"
Peak_Data_Years <- rbind(GagesOnBridges,GagesNotOnBridges)
Peak_Data_Years <- Peak_Data_Years[!is.na(Peak_Data_Years$YR_DIS_BEG),]
hist2 <- ggplot(Peak_Data_Years, aes(YR_DIS_BEG, fill = GAUGE)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
hist2 <- hist2 + xlab("Year Peak Flow Data Available")# + ggtitle("Comparison of Year of Flow Data Availability for Gages on Bridges (N=13), Gages not on Bridges (N=317)")
hist2 <- hist2 + xlim(1840,2020) + ylim(0, 0.06)
print(hist2)
multiplot(hist1,hist2)

df.Gages.Bridges.All <- MatchesFullData
df.Gages.Bridges.All[,c("SITE_NO","SITE_NAME","LAT_GAGE","LNG_GAGE","YR")]

#####################
# FINDING AREAS TO TARGET FOR FINDING NEW BRIDGES (BASED ON GAGE DENSITY)
All_States <- levels(ds.states$STATE_ABBR)
MissingStates <- All_States[!(All_States %in% unique(MatchesLocatedActiveCont$STATE_CODE))]
FailStates <- unique(FailDataFrame$STATE_CODE)
MissingStates <- MissingStates[MissingStates %in% FailStates]

State <- 1
State_Name <- df.States[df.States$STATE_CODE==MissingStates[State],"STATE_FULL"]
State_FIPS <- df.States[df.States$STATE_CODE==MissingStates[State],"STFIPS"]
basemap1 <- get_map(location=paste(State_Name,"State",sep=" "),zoom=8,maptype="road",source="google",color="bw")
map2 <- ggmap(basemap1,maprange=TRUE,darken=c(0.4,"black"),legend="bottom",base_layer = ggplot(df.ds.gauges.cont, aes(x=LNG_GAGE,y=LAT_GAGE)))
map2 <- map2 + ggtitle("Gauges In State") + xlab("") + ylab("")
map3 <- map2 + geom_point(color="blue",data=subset(df.ds.gauges.cont,STFIPS==State_FIPS))
map4 <- map3 + geom_point(color="red",aes(x=LONGDD, y=LATDD), data=subset(nbiDataFrame,STFIPS==State_FIPS))
print(map3)
print(map4)

nbiDataFrame$LONGR <- deg2rad(nbiDataFrame$LONGDD)
nbiDataFrame$LATR  <- deg2rad(nbiDataFrame$LATDD)
df.ds.gauges.cont$LONGR  <- deg2rad(df.ds.gauges.cont$LNG_GAGE)
df.ds.gauges.cont$LATR   <- deg2rad(df.ds.gauges.cont$LAT_GAGE)
nbiBridgesInState <- which(nbiDataFrame$STFIPS==State_FIPS)
gaugesInState <- which(df.ds.gauges.cont$STFIPS==State_FIPS)

# cl <- makeCluster(2)
nbiBridgeGaugeDist_state <- mclapply(gaugesInState, function(i) sapply(nbiBridgesInState, function(j) gcd.slc(nbiDataFrame[j,"LONGR"],
                                                                                                            nbiDataFrame[j,"LATR"],
                                                                                                            df.ds.gauges.cont[i,"LONGR"],
                                                                                                            df.ds.gauges.cont[i,"LATR" ])),mc.cores=3)
nbiBridgeGaugeDist_state <- as.data.frame(nbiBridgeGaugeDist_state)
BridgeGaugeDistNearState <- mclapply(1:length(nbiBridgesInState),function(i) min(nbiBridgeGaugeDist_state[i,]),mc.cores=3)

hist(BridgeGaugeDistNear[IndexCloserXkm],breaks=20, freq=TRUE,main="Distance to Nearest Gauge (VA Bridges)",xlab="Distance, d (km)",
     ylab="Number of Failed Bridges with Gauge at Distance d (of 38)")#,xlim=c(0,40),ylim=c(0,15))
sum(BridgeGaugeDistNear<=15) # 46 with distance < 10k
sum(BridgeGaugeDistNear<=5) # 28 with distance <= 5k <- start here

NearestGaugeLoc <- GaugesSubset[sapply(1:nMatch, function(i) which(BridgeGaugeDist[i,]==BridgeGaugeDistNear[i])),]
# map4 <- map3 + scale_shape_manual(values=c(21,23)) + scale_fill_manual(values=c("red","blue"),guide=guide_legend(override.aes=aes(shape=22)))+scale_color_manual(values=c("white","black"),guide=guide_legend(override.aes=aes(shape=22)))
# map5 <- map4 + geom_point(aes(color=HAS_FAIL_DATE),size=6, data=subset(df.Gages.Bridges,as.logical(FAIL_Q_AVAIL)==TRUE & DIST_TO_GAUGE < 3)) + scale_color_manual(values=c("white","black"))
