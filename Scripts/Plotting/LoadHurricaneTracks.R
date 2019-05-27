# Incorporate hurricane data automatically
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
source(file.path(dirsGit$Scripts,"Helper","gcd_slc.R"))
source(file.path(dirsGit$Scripts,"Helper","deg2rad.R"))

cols <- c("DATE","UTC","IDENTIFIER","TYPE","LAT","LONG","WINDSPEED_MAX_SUSTAINED_KNOTS",
          "PRESSURE_MIN_MILLIBAR",
          paste("RADIUS_MAX_",c("NE","SE","SW","NW"),"_34_KNOT_NAUTMILES",sep=""),
          paste("RADIUS_MAX_",c("NE","SE","SW","NW"),"_50_KNOT_NAUTMILES",sep=""),
          paste("RADIUS_MAX_",c("NE","SE","SW","NW"),"_64_KNOT_NAUTMILES",sep=""),
          "NA")
colClasses <- c("character","character","factor","factor","character","character",rep("integer",15))
df.hurricanes <- read.csv(file.path(dirs$OrigDataDir,"hurdat2-1851-2014-060415.txt"), 
                          header = FALSE, colClasses = colClasses,col.names = cols)
df.hurricanes <- df.hurricanes[,1:20]
df.hurricanes$LATDD <- as.numeric(sub("[[:alpha:]]","",df.hurricanes$LAT))
df.hurricanes$LONGDD <- -1*as.numeric(sub("[[:alpha:]]","",df.hurricanes$LONG))
df.hurricanes.temp <- df.hurricanes
nEntries <- sum(df.hurricanes$TYPE=="")-1
ls.hurricanes <- list()
for (i in 1:nEntries){
  name <- df.hurricanes.temp[1,"DATE"]
  ls.hurricanes[[name]] <- list(NAME           = gsub("[[:space:]]","",df.hurricanes.temp[1,"UTC"]),
                                BASIN          = substr(name,1,2),
                                CYCLONE_NUMBER = as.integer(substr(name,3,4)),
                                YEAR           = as.Date(paste(substr(name,5,8),"-01-01",sep="")))
  temp <- df.hurricanes.temp[2:(1+as.integer(as.character(df.hurricanes.temp[1,"IDENTIFIER"]))),c(1:4,21:22,7:20)]
  temp$DATE <- as.Date(temp$DATE, format = "%Y%m%d")
  ls.hurricanes[[name]]$TRACK <- temp
  df.hurricanes.temp <- df.hurricanes.temp[(2+as.integer(as.character(df.hurricanes.temp[1,"IDENTIFIER"]))):nrow(df.hurricanes.temp),]
}
rm(df.hurricanes.temp)

savefile <- paste(gsub("-","",Sys.Date()),"HURDAT2_Hurricane_Tracks.RData",sep="_")
save(ls.hurricanes,file = file.path(dirsGit$Data,savefile))
rm(savefile)
# RECORD IDENTIFIERS
# L (Space 17) – Record identifier (see notes below)
# C – Closest approach to a coast, not followed by a landfall G – Genesis
# I – An intensity peak in terms of both pressure and wind L – Landfall (center of system crossing a coastline)
# P – Minimum in central pressure
# R – Provides additional detail on the intensity of the cyclone when rapid changes are underway S – Change of status of the system
# T – Provides additional detail on the track (position) of the cyclone
# W – Maximum sustained wind speed

# RECORD TYPE - TS (Spaces 20-21, before 3rd comma) – Status of system. Options are:
# TD – Tropical cyclone of tropical depression intensity (< 34 knots)
# TS – Tropical cyclone of tropical storm intensity (34-63 knots)
# HU – Tropical cyclone of hurricane intensity (> 64 knots)
# EX – Extratropical cyclone (of any intensity)
# SD – Subtropical cyclone of subtropical depression intensity (< 34 knots)
# SS – Subtropical cyclone of subtropical storm intensity (> 34 knots)
# LO – A low that is neither a tropical cyclone, a subtropical cyclone, nor an extratropical cyclone (of any intensity) 
# WV – Tropical Wave (of any intensity)
# DB – Disturbance (of any intensity)

# States where hurricanes need to be considered:
hurrStates <- c("TX","LA","MS","AL","FL","GA","SC","NC","VA","MD","DE","NJ","PA","NY","CT","RI","MA","NH","ME","DC","WV")

# Max radius of 34 knots was 640 nautical miles or 1185.3 km, 34 knots is 39mi/hr
# mean is 115 naut miles for all, 80 for tropical storms, 123 for hurr
# vast majority less than 300 (560km) for hurricanes, 200 (370km) for tropical storms

maxDist <- c(HU=560, TS = 230, EX = 930, DB = 300, LO = 560, SD = 560, TD = 230, SS = 560, WV = 230)*3
Years <- as.Date(sapply(1:length(ls.hurricanes), function(i) ls.hurricanes[[i]]$YEAR,
                simplify = TRUE,
                USE.NAMES = TRUE), "1970-01-01")
names(Years) <- names(ls.hurricanes)
df.Fail.NBI.Gage$BOOL_POSSIBLE_HURRICANE    <- FALSE
df.Fail.NBI.Gage$COMMENT_POSSIBLE_HURRICANE <- ""

for (i in 1:nrow(df.Fail.NBI.Gage)){
  if (!(df.Fail.NBI.Gage[i,"STATE"] %in% hurrStates) | is.na(df.Fail.NBI.Gage[i,"YR_FAIL_EST"])) next()
  hurrsYear <- names(Years)[Years == df.Fail.NBI.Gage[i,"YR_FAIL_EST"]]
  # print(hurrsYear)
  if (df.Fail.NBI.Gage[i,"BOOL_HAS_FAIL_DATE"]){
    hurrs <- hurrsYear[sapply(hurrsYear, function(h) any(ls.hurricanes[[h]]$TRACK$DATE == df.Fail.NBI.Gage[i,"DATE_FAIL_EST_USGS"]))]
  } else hurrs <- hurrsYear
 
#   print(df.Fail.NBI.Gage[i,"BOOL_HAS_FAIL_DATE"])
#   print(hurrs)
  # if (length(hurrs)==1 & is.na(hurrs)) hurrs <- character(0)
  
  for (h in hurrs[!is.na(hurrs)]){
    df.track <- as.data.frame(ls.hurricanes[[h]][["TRACK"]])
    df.track$TYPE <- factor(substr(as.character(df.track$TYPE),2,3))
    dist <- sapply(1:nrow(df.track), 
                   function(k) gcd_slc(deg2rad(df.Fail.NBI.Gage[i,"LONGDD"]),
                                       deg2rad(df.Fail.NBI.Gage[i,"LATDD"]),
                                       deg2rad(df.track[k,"LONGDD"]),
                                       deg2rad(df.track[k,"LATDD"])))
    checkDist <- sapply(1:nrow(df.track),
                        function(k) ifelse(df.track[k,"RADIUS_MAX_NE_34_KNOT_NAUTMILES"]!=-999 & df.track[k,"RADIUS_MAX_NE_34_KNOT_NAUTMILES"]!=0,
                                           df.track[k,"RADIUS_MAX_NE_34_KNOT_NAUTMILES"],
                                           maxDist[df.track[k,"TYPE"]]))
    
    minDist <- min(dist - checkDist)
    if (minDist < 0){
      print(paste(IDsToView[i],"affected by hurricane:",h))
      df.Fail.NBI.Gage[i,"BOOL_POSSIBLE_HURRICANE"]       <- TRUE
      df.Fail.NBI.Gage[i,"COMMENT_POSSIBLE_HURRICANE"] <- paste(df.Fail.NBI.Gage[i,"COMMENT_POSSIBLE_HURRICANE"],
                                                                ls.hurricanes[[h]]$NAME,
                                                                round(min(dist)),"km -")
    }
  }
}
View(df.Fail.NBI.Gage[df.Fail.NBI.Gage$BOOL_WAS_HURRICANE | df.Fail.NBI.Gage$BOOL_POSSIBLE_HURRICANE,c("ID","STATE","COMMENTS","FAIL_CAUS","COMMENT_LINKED_HURRICANE","COMMENT_POSSIBLE_HURRICANE")])




