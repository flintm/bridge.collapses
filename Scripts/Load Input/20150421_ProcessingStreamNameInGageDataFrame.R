# 2015-04-20 Realized that current version of gageDataFrame does not include FIPS (county_code)
load("~/Documents/Research/Climate_Scour_Local/R_data/FrequentlyUsedArchiveData/20150420_GageDataFrame_For_Matching.RData")
inventoryFile         <- "/Users/MM/Documents/Research/Climate_Scour_Local/Data_original_files/StreamGages/20150420_USGS_GageList_All_Data_Inventory_ToLoad.txt"
gageDataFullInventory <- read.table(inventoryFile, header=TRUE, sep="\t",stringsAsFactors = FALSE, colClasses = "character", fill=TRUE,quote="\"")
DuplicateGages        <- which(sapply(1:nrow(gageDataFrame), function(i) sum(gageDataFullInventory$site_no==gageDataFrame[i,"STAID"])>1))
View(gageDataFrame[DuplicateGages,])
View(gageDataFullInventory[gageDataFullInventory$site_no %in% gageDataFrame[DuplicateGages,"STAID"],])
gageDataFullInventory <- gageDataFullInventory[!(rownames(gageDataFullInventory) %in% c("1238","5841","6104","6108","6185","7976")),]
gageDataFullInventory$STAID <- gageDataFullInventory$site_no
savefile  <- paste(gsub("-","",Sys.Date()),"All_Avail_UGSS_Gage_Data.RData",sep="_")
save(gageDataFullInventory,NumberMatches, file=file.path(DataDir,"FrequentlyUsedArchiveData",savefile))
library(plyr)
gageDataFrame <- join(gageDataFrame,gageDataFullInventory[,c("STAID","state_cd","county_cd","country_cd")],by="STAID",match="first")
savefile  <- paste(gsub("-","",Sys.Date()),"GageDataFrame_with_FIPS.RData",sep="_")
save(gageDataFrame,NumberMatches, file=file.path(DataDir,"FrequentlyUsedArchiveData",savefile))

# Now pulling code from processing Stream Name (originally written for Fail and NBI DFs)
library(stringr)
library(stringdist)
OrigDataDir <- "/Users/MM/Documents/Research/Climate_Scour_Local/Data_original_files"
source("/Users/MM/Documents/Research/Climate_Scour_Local/R_scripts/Utility_Scripts/convert.df.classes.R")
source('/Users/MM/Documents/Research/Climate_Scour_Local/R_scripts/Utility_Scripts/clean.df.strings.R')
# source('/Users/MM/Documents/Research/Climate_Scour_Local/R_scripts/Analysis_Scripts/MatchingFailToNBI/20150126_MatchEntry.R')
# source('/Users/MM/Documents/Research/Climate_Scour_Local/R_scripts/Analysis_Scripts/MatchingFailToNBI/20150121_PerformMatch.R') 
# source('/Users/MM/Documents/Research/Climate_Scour_Local/R_scripts/Analysis_Scripts/ProcessingFailAndNBIdata/LookingForLocation/20141205_GetAndRemoveLocationData_CityCounty.R')
# source('/Users/MM/Documents/Research/Climate_Scour_Local/R_scripts/Analysis_Scripts/ProcessingFailAndNBIdata/LookingForStream/20141208-GetAndRemoveLocationForStream.R')

# STATE NAMES, STFIPS CODES, AND NBI CODES
df.States <- read.csv(file.path(OrigDataDir,"PoliticalBoundaries","StateData.csv"), header = TRUE,
                      sep = ",", quote = "\"", dec = ".", 
                      colClasses = c("character", "character", "integer", "integer"),
                      col.names = c("STATE_CODE", "STATE_FULL", "STFIPS", "NBI_STATE")) 

# COUNTY NAMES, FIPS CODES, AND STATES
df.Counties <- read.csv(file.path(OrigDataDir,"PoliticalBoundaries","CountyData.csv"), header = TRUE,
                        sep  = ",", quote = "\"", dec = ".",
                        col.names = c("COUNTY_FULL", "COUNTY_NAME", "COUNTY_SUFFIX", "FIPS", "POP", "STFIPS"),
                        na.strings = "NA" )

CountiesTypes <- c("character", "character", "character", "integer","numeric", "integer")
CountiesTypes <- t(CountiesTypes)
colnames(CountiesTypes) <- colnames(df.Counties)
df.Counties <- convert.df.classes(df.Counties,CountiesTypes,FALSE,1)
df.Counties <- subset(df.Counties,df.Counties$POP!=-999) #-999 is code for regions in the Great Lakes

# CITY NAMES, GNIS AND ANSI CODES, LAT/LONG DATA
df.Cities <- read.csv(file.path(OrigDataDir,"PoliticalBoundaries","CityData.csv"), header = TRUE, 
                      sep = ",", quote = "\"", dec = ".", 
                      colClasses = c("character", "character", "character", "character", "character", "numeric", "numeric"),
                      col.names = c("GNIS_ID", "ANSICODE", "CITY_NAME", "FIPS", "STFIPS","LATDD","LONGDD"))
df.Cities$FIPS <- as.integer(paste(df.Cities$STFIPS,df.Cities$FIPS,sep=""))
df.Cities$STFIPS <- as.integer(df.Cities$STFIPS)
# STEP 0D: CITY, CARDINAL, RELATIONAL AND LANDSCAPE KEWYORDS, AND ABBREVIATIONS ----------------------------

# CARDINAL DIRECTIONS AND RELATIONAL DIRECTIONS KEYWORDS  -----
ls.CardinalKeys <- list(south = c("south", "southern", "so", "s"), 
                        north  = c("north", "northern", "no","n"), 
                        east   = c("east", "eastern", "e"), 
                        west   = c("west", "western", "w"),
                        nw     = c("northwest", "nw"), 
                        ne     = c("northeast", "ne"), 
                        sw     = c("southwest", "sw"), 
                        se     = c("southeast", "se"), 
                        sb     = c("southbound", "sb"), 
                        nb     = c("northbound", "nb"), 
                        eb     = c("eastbound", "eb"), 
                        wb     = c("westbound", "wb")
)
CardinalKeys <- unlist(ls.CardinalKeys)
CardinalKeyIndex <- sapply(CardinalKeys, function(s) grep(paste("\\<",s,"\\>",sep=""),ls.CardinalKeys)[1])
CardBoundKeys <- unlist(ls.CardinalKeys[c("sb", "nb", "eb", "wb")])
CardBoundKeyIndex <- c(1, 1, 2, 2, 3, 3, 4, 4)

ls.RelationalKeys <- list(main   = c("main", "m"),
                          middle = c("middle", "mid", "m"),
                          left   = c("left", "lt", "l"),
                          right  = c("right", "rt", "r"),
                          below  = c("below", "blw","bl","bel"),
                          above  = c("above","abv","ab"),
                          upstream = c("upstream"),
                          downstream = c("downstream", "ds"),
                          near   = c("near", "nr"),
                          by     = c("by"),
                          of     = c("of"),
                          on     = c("on"),
                          over   = c("over"),
                          intersection = c("intersection", "int"),
                          junction = c("junction", "jct"),
                          in_loc   = c("in"),
                          off      = c("off"),
                          at       = c("at")               
)

RelationalKeys <- paste("\\<",unlist(ls.CardinalKeys[c("south","north","east","west","nw","ne","se","sw")]),"\\>",sep="")
RelationalKeys <- c(paste(RelationalKeys,"[[:space:]]{1,3}\\<",ls.RelationalKeys$of,"\\>",sep=""), paste("\\<",unlist(ls.RelationalKeys[c("near","on","over","in_loc","at","above","below","upstream","downstream")]),"\\>",sep=""))

ls.DistanceKeys <- list(miles      = c("miles", "mile", "mi", "m"),
                        kilometers = c("kilometers", "kilometer", "km", "k"),
                        feet       = c("feet", "ft")
                        )
DistanceKeys    <- paste("\\<",unlist(ls.DistanceKeys),"\\>",sep="")

# Landscape keywords
ls.LandscapeKeys <- list(
  little   = c("little", "litttle", "ltle"),
  mountain = c("mountain", "mtn"),
  railroad = c("railway", "railroad", "rr"),
  school   = c("school", "sch"),
  trail    = c("trail", "traill"),
  hill     = c("hill", "hil"),
  valley   = c("valley", "vly"),
  south_fork = c("south fork", "sf"),
  north_fork = c("north fork", "nf"),
  platte   = c("platte", "plat"),
  plantation = c("plantation", "plt")
)
# STEP 0B: STREAM KEYWORDS ---------------------------------------------------------------------------------
ls.StreamKeys <- list(arroyo    = c("arroyo", "ary"),
                      bayou     = c("bayou", "byou", "byo","bay"),
                      brook     = c("brook", "brk", "bk", "bro", "b"),
                      creek     = c("creek", "crk", "ck", "cr", "c"),
                      canal     = c("canal", "cnl", "can", "cn"),
                      canyon    = c("canyon", "cnyn"),
                      channel   = c("channel", "chnl", "chn", "cha", "ch"),
                      cove      = c("cove"),
                      dam       = c("dam"),
                      delta     = c("delta"),
                      ditch     = c("ditch", "dtch", "dt", "dit", "d"),
                      eddy      = c("eddy", "edd"),
                      falls     = c("falls", "fall", "fll", "fal", "f"),
                      floodway  = c("floodway", "fld"),
                      forks     = c("forks", "frks"),
                      gorge     = c("gorge"),
                      gully     = c("gully"),
                      hollow    = c("hollow", "hllw", "hlw", "hol", "h"),
                      inlet     = c("inlet"),
                      kill      = c("kill", "kll", "kl", "kil", "k"),
                      lakes     = c("lakes", "lake", "lks", "lk", "lak", "l"),
                      lock      = c("lock"),
                      ponds     = c("ponds", "pond", "pd"),
                      river     = c("river", "rive", "rvr", "riv", "rv", "r"),
                      reservoir = c("reservoir","res"),
                      rio       = c("rio"),
                      run       = c("run", "rn"),
                      outlet    = c("outlet","out"),
                      overflow  = c("overflow","overflo", "overf"),
                      slough    = c("slough", "slgh", "slo", "sl"),
                      spillway  = c("spillway", "spw"),
                      stream    = c("stream", "stre", "str"),
                      swamp     = c("swamp", "swmp", "swa", "sw"),
                      wash      = c("wash", "wsh", "w")
)

ls.TribsKeys <- list( branch    = c("branch", "brnch", "brn", "bra", "br", "b"),
                      fork      = c("fork", "frk", "fk", "f"),
                      main.fork = c("main fork", "mf"),
                      east.fork = c("east fork", "ef"),
                      south.fork = c("south fork", "sf"),
                      north.fork = c("north fork", "nf"),
                      west.fork = c("west fork", "wf"),
                      tributary = c("tributary", "trb", "trib", "tr", "t", "to")
)
TribKeys   <- paste("\\<",
                    c(unlist(ls.CardinalKeys[c("south","north","east","west","nw","ne","se","sw")]),
                      unlist(ls.RelationalKeys[c("main", "middle","left","right","upstream","downstream")]),
                    "first"),
                    "\\>",sep="")
TribKeysOnly <- unlist(sapply(1:length(ls.TribsKeys), function(i) sapply(1:length(TribKeys),
                                                          function(j) paste(TribKeys[j],"[[:space:]]{1,3}\\<",ls.TribsKeys[[i]],"\\>",sep=""))))                 
TribKeys     <- c(paste(TribKeysOnly,"[[:space:]]{1,3}\\<to\\>",sep=""),
                paste(TribKeysOnly,"[[:space:]]{1,3}\\<of\\>",sep=""),
                paste("\\<",unlist(ls.TribsKeys),"\\>[[:space:]]{1,3}\\<to\\>",sep=""),
                paste("\\<",unlist(ls.TribsKeys),"\\>[[:space:]]{1,3}\\<of\\>",sep=""))

StreamKeys <- c(unlist(ls.StreamKeys), unlist(ls.TribsKeys))



# SETUP --------
GageRowNames <- rownames(gageDataFrame)
nGage        <- nrow(gageDataFrame)

# DEAL WITH LANDSCAPE KEYS ----------------------
for (j in c(1:length(ls.LandscapeKeys))){
  key_index_loc <- sapply(paste("\\<",ls.LandscapeKeys[[j]][c(2:length(ls.LandscapeKeys[[j]]))],"\\>", sep = ""),grepl,gageDataFrame$STANAME)
  match_keys_loc <- which(apply(key_index_loc, MARGIN = 1, any))
  key_index_stream <- sapply(paste("\\<",ls.LandscapeKeys[[j]][c(2:length(ls.LandscapeKeys[[j]]))],"\\>", sep = ""),grepl,gageDataFrame$STANAME)
  match_keys_stream <- which(apply(key_index_stream, MARGIN = 1, any))
  for (i in match_keys_loc){
    gageDataFrame[GageRowNames[i],"STANAME"] <- gsub(paste("\\<",ls.LandscapeKeys[[j]][which(key_index_loc[i,])[1]+1],"\\>", sep = ""),ls.LandscapeKeys[[j]][1],gageDataFrame[GageRowNames[i],"STANAME"])
  }
  for (i in match_keys_stream){
    gageDataFrame[GageRowNames[i],"STANAME"] <- gsub(paste("\\<",ls.LandscapeKeys[[j]][which(key_index_stream[i,])[1]+1],"\\>", sep = ""),ls.LandscapeKeys[[j]][1],gageDataFrame[GageRowNames[i],"STANAME"])
  }
}

# # STEP 4: PROCESS STREAM NAME ----------------------------------------------------------------
NoStreamDataIndex  <- which(gageDataFrame$STANAME == "")
HasStreamDataIndex <- subset(GageRowNames, !(GageRowNames %in% NoStreamDataIndex) )

# for entries with a waterway, separate all non-name data (e.g., river, creek, left fork, ...)
gageDataFrame$STREAM_NAME_GAGE  <- gageDataFrame$STANAME
gageDataFrame$STREAM_TYPE_GAGE  <- character(nGage)
gageDataFrame$STREAM_OTHER_GAGE <- character(nGage)
gageDataFrame$STREAM_TRIB_GAGE  <- character(nGage)

# 2015-04-21 removing ##.## <measurement unit> <of/from/upstream>
# FIND RELATIONAL STATEMENTS
# statements with explicit distance, e.g., "3.5 miles from Stanford" - units as miles, km, feet
MatchRegex <- "[[:digit:]]{1,3}[[:punct:]]?[[:digit:]]?[[:space:]]?[mkf][[:alpha:]]{0,4}[[:punct:]]?[[:space:]]"
RowsWithDistance <- GageRowNames[grep(MatchRegex,gageDataFrame$STREAM_NAME_GAGE)]
for (i in RowsWithDistance){
  OrigString  <- gageDataFrame[i,"STREAM_NAME_GAGE"]
  if (grepl("[[:digit:]][mkf]",gageDataFrame[i,"STREAM_NAME_GAGE"])){ # if no space between # and "mi", add
    make_space_first <- regexpr("[[:digit:]]m",gageDataFrame[i,"STREAM_NAME_GAGE"])
    gageDataFrame[i,"STREAM_NAME_GAGE"] <- paste(substr(gageDataFrame[i,"STREAM_NAME_GAGE"],1,make_space_first),substr(gageDataFrame[i,"STREAM_NAME_GAGE"],make_space_first+1,nchar(gageDataFrame[i,"STREAM_NAME_GAGE"])))
  }
  match_start <- regexpr(MatchRegex,gageDataFrame[i,"STREAM_NAME_GAGE"])
  match_last  <- match_start + attr(match_start, "match.length") - 1
  # find distance and distance unit
  # distance
  dist_start  <- regexpr("[[:digit:]]{1,3}[[:punct:]]{0,1}[[:digit:]]{0,1}",gageDataFrame[i,"STREAM_NAME_GAGE"])
  dist_last   <- dist_start + attr(dist_start, "match.length") - 1
  gageDataFrame[i,"STREAM_OTHER_GAGE"] <- substr(gageDataFrame[i,"STREAM_NAME_GAGE"],dist_start,dist_last)
  # distance unit
  for (j in 1:length(ls.DistanceKeys)){
    key_index  <- sapply(ls.DistanceKeys[[j]],grepl,substr(OrigString,dist_last+2,match_last))
    if (any(key_index)){
      match_key      <- which(key_index)[1]
      unit_start    <- regexpr(ls.DistanceKeys[[j]][match_key],substr(OrigString,dist_last+2,match_last))
      unit_last     <- match_start + attr(match_start, "match.length") - 1
      gageDataFrame[i,"STREAM_OTHER_GAGE"] <- paste(gageDataFrame[i,"STREAM_OTHER_GAGE"],ls.DistanceKeys[[j]][1])
    }
  }
  gageDataFrame[i,"STREAM_NAME_GAGE"]  <- paste(substr(OrigString,1,match_start-1),substr(OrigString,match_last+1,nchar(OrigString)))
}
print("Completed check for distance")
# ----- if want to look for city ----------
#   # look for cardinal direction (north, nw, sw...)
#   substrings <- unlist(strsplit(gageDataFrame[i,"STREAM_NAME_GAGE"],"[[:space:]]"))
#   directionalWord <- ls.CardinalKeys[[grep(paste("\\<",substrings[1],"\\>",sep=""), ls.CardinalKeys)]][1]
#   gageDataFrame[i,"STREAM_OTHER_GAGE"] <- paste(gageDataFrame[i,"STREAM_OTHER_GAGE"],directionalWord,"of",sep=" ")      
#   # look for place
#   placeSubstring <- ifelse(grepl("\\<of\\>",substrings[2]), 3, 2)
#   relationalPlace <- substrings[placeSubstring]
#   if (placeSubstring ==2){
#     gageDataFrame[i,"STREAM_NAME_GAGE"] <- gsub("\\<of\\>","",gageDataFrame[i,"STREAM_NAME_GAGE"])
#   }
#   PlaceIsCounty <- relationalPlace %in% StateCounties[,"COUNTY_NAME"]
#   PlaceIsCity   <-  relationalPlace %in% StateCities[,"CITY_NAME"]
#   if (PlaceIsCounty){
#     MatchedCountyIndex <- grep(paste("\\<",relationalPlace,"\\>",sep=""),StateCounties[,"COUNTY_NAME"])
#     gageDataFrame[i,LocProcessColsOut] <- GetAndRemoveLocationData(gageDataFrame[i, LocProcessColsIn], relationalPlace, LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, FALSE, TRUE)
#     gageDataFrame[i,"COUNTY_NAME"] <- StateCounties[MatchedCountyIndex,"COUNTY_NAME"]
#     gageDataFrame[i,"FIPS"] <- StateCounties[MatchedCountyIndex,"FIPS"]
#     if (PlaceIsCity){
#       MatchedCityIndex <- grep(paste("\\<",relationalPlace,"\\>",sep=""),StateCities[,"CITY_NAME"])
#       gageDataFrame[i,"CITY_NAME"] <- StateCities[MatchedCityIndex,"CITY_NAME"]
#       gageDataFrame[i,"FIPS_FROM_CITY"] <- StateCities[MatchedCityIndex,"FIPS"]
#       gageDataFrame[i,"ANSICODE"] <- StateCities[MatchedCityIndex,"ANSICODE"]
#       gageDataFrame[i,"GNIS_ID"] <- StateCities[MatchedCityIndex,"GNIS_ID"]
#     }
#   }
#   else{
#     if (PlaceIsCity){
#       MatchedCityIndex <- grep(paste("\\<",relationalPlace,"\\>",sep=""),StateCities[,"CITY_NAME"])
#       gageDataFrame[i,LocProcessColsOut] <- GetAndRemoveLocationData(gageDataFrame[i, LocProcessColsIn], relationalPlace, LocProcessColsOut,ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, TRUE, FALSE)
#       gageDataFrame[i,"CITY_NAME"] <- StateCities[MatchedCityIndex,"CITY_NAME"]
#       gageDataFrame[i,"FIPS_FROM_CITY"] <- StateCities[MatchedCityIndex,"FIPS"]
#       gageDataFrame[i,"ANSICODE"] <- StateCities[MatchedCityIndex,"ANSICODE"]
#       gageDataFrame[i,"GNIS_ID"] <- StateCities[MatchedCityIndex,"GNIS_ID"]
#     }
#     else{
#       gageDataFrame[i,"ROAD_NEAR"] <- paste(gageDataFrame[i,"ROAD_NEAR"],relationalPlace,sep=" ")
#     }
#   }
#   gageDataFrame[i,"STREAM_NAME_GAGE"] <- sub(MatchRegex,"",gageDataFrame[i,"STREAM_NAME_GAGE"])
#   gageDataFrame[i,"STREAM_NAME_GAGE"] <- sub(paste("\\<",substrings[1],"\\>",sep=""),"",gageDataFrame[i,"STREAM_NAME_GAGE"])
#   gageDataFrame[i,"STREAM_NAME_GAGE"] <- sub("\\<of\\>","",gageDataFrame[i,"STREAM_NAME_GAGE"])
# }

# 2015-04-21 removing relational information - basically adding processing for LOCATION to this processing (__ River near __, State)
# only looking for "west of" so will not accidentally remove "west" from "west kill"
# ---------

# 2015-04-21 remove anything after punctuation
key_index <- sapply("[[:punct:]]",grepl,gageDataFrame$STREAM_NAME_GAGE)
# GageRowNames <- GageRowNames[c(117,498,941)]
# key_index <- sapply("[[:punct:]]",grepl,gageDataFrame[c("117","498","941"),"STREAM_NAME_GAGE"])
match_keys <- which(apply(key_index, MARGIN = 1, any))
for (i in match_keys){
  match_start    <- regexpr("[[:punct:]]",gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"])
  match_last     <- match_start + attr(match_start, "match.length") - 1
  gageDataFrame[GageRowNames[i],"STREAM_OTHER_GAGE"] <- paste(gageDataFrame[GageRowNames[i],"STREAM_OTHER_GAGE"],substr(gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"],match_start,nchar(gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"])))
  gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"]  <- substr(gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"],1,match_start-1)
}
print("Completed additional (punctuated) info analysis for gageDataFrame")

# relational
# key_index  <- sapply(RelationalKeys,grepl,gageDataFrame[c("117","498","941"),"STREAM_NAME_GAGE"])
key_index  <- sapply(RelationalKeys,grepl,gageDataFrame$STREAM_NAME_GAGE)
match_keys <- which(apply(key_index, MARGIN = 1, any))
for (i in match_keys){
  matches  <- which(key_index[i,])
  for (j in matches){
    match_start    <- regexpr(RelationalKeys[j],gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"])
    match_last     <- match_start + attr(match_start, "match.length") - 1
    if (!is.na(match_start) & match_start > 0){
      gageDataFrame[GageRowNames[i],"STREAM_OTHER_GAGE"] <- paste(gageDataFrame[GageRowNames[i],"STREAM_OTHER_GAGE"],substr(gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"],match_start,nchar(gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"])))
      gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"]  <- substr(gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"],1,match_start-1)
    }
  }  
}
print("Completed relational analysis for gageDataFrame")



# tributaries - but only if not followed by an additional river word, i.e., not "branch river"
key_index  <- sapply(TribKeys,grepl,gageDataFrame$STREAM_NAME_GAGE)
match_keys <- which(apply(key_index, MARGIN = 1, any))
for (i in match_keys){
  OrigString     <- gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"]
  match_start    <- regexpr(paste("\\<",TribKeys[which(key_index[i,])[1]],"\\>[[:space:]]{1,3}",sep=""),OrigString)
  match_last     <- match_start + attr(match_start, "match.length") - 1
  gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"] <- gsub(TribKeys[which(key_index[i,])[1]],"",gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"])
  gageDataFrame[GageRowNames[i],"STREAM_TRIB_GAGE"] <- paste(gageDataFrame[GageRowNames[i],"STREAM_TRIB_GAGE"],substr(OrigString,match_start,match_last), sep = " ")
}

# tributaries (mostly forks) if fork/branch/trib is NOT the last word in the string
key_index  <- sapply(paste("\\<",unlist(ls.TribsKeys),"\\>[[:space:]]{1,3}[[:alpha:]]",sep=""),grepl,gageDataFrame$STREAM_NAME_GAGE)
match_keys <- which(apply(key_index, MARGIN = 1, any))
for (i in match_keys){
  OrigString     <- gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"]
  match_start    <- regexpr(paste("\\<",unlist(ls.TribsKeys)[which(key_index[i,])[1]],"\\>[[:space:]]{1,3}",sep=""),OrigString)
  match_last     <- match_start + attr(match_start, "match.length") - 1
  RemainSplitStrings   <- unlist(strsplit(substr(OrigString,match_last+1,nchar(OrigString)),"[[:space:]]"))
  if (!any(sapply(paste("\\<",StreamKeys,"\\>",sep=""),grepl,RemainSplitStrings[1]))){
    gageDataFrame[GageRowNames[i],"STREAM_TRIB_GAGE"] <- paste(gageDataFrame[GageRowNames[i],"STREAM_TRIB_GAGE"],substr(OrigString,1,match_last))
    gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"] <- substr(OrigString,match_last+1,nchar(OrigString))
  } 
}

gageDataFrame[,c("STREAM_NAME_GAGE","STREAM_OTHER_GAGE","STREAM_TYPE_GAGE", "STREAM_TRIB_GAGE")] <- sapply(c("STREAM_NAME_GAGE","STREAM_OTHER_GAGE","STREAM_TYPE_GAGE", "STREAM_TRIB_GAGE"), function(j) str_trim(gageDataFrame[,j]))

# where tributary is the last word
key_index  <- sapply(paste("\\<",ls.TribsKeys$tributary,"$",sep=""),grepl,gageDataFrame$STREAM_NAME_GAGE)
match_keys <- which(apply(key_index, MARGIN = 1, any))
for (i in match_keys){
  OrigString     <- gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"]
  match_start    <- regexpr(paste("\\<",ls.TribsKeys$tributary[which(key_index[i,])[1]],"$",sep=""),OrigString)
  match_last     <- match_start + attr(match_start, "match.length") - 1
  gageDataFrame[GageRowNames[i],"STREAM_TRIB_GAGE"] <- paste(gageDataFrame[GageRowNames[i],"STREAM_TRIB_GAGE"],"tributary")
  gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"] <- substr(OrigString,1,match_start-1)
}
print("Completed tributary analysis for gageDataFrame")

# stream types after tributary processing
print("Beginning processing of STREAM TYPE data")
for (j in c(1:length(ls.StreamKeys))){
  print(paste("Looking for matches to STREAM TYPE", ls.StreamKeys[[j]][1],sep=" "))
  key_index <- sapply(paste("\\<",ls.StreamKeys[[j]],"\\>", sep = ""),grepl,gageDataFrame$STREAM_NAME_GAGE)
  match_keys <- which(apply(key_index, MARGIN = 1, any))
  for (i in match_keys){
    gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"] <- gsub(paste("\\<",ls.StreamKeys[[j]][which(key_index[i,])[1]],"\\>", sep = ""),"",gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"])
    gageDataFrame[GageRowNames[i],"STREAM_TYPE_GAGE"] <- paste(gageDataFrame[GageRowNames[i],"STREAM_TYPE_GAGE"], ls.StreamKeys[[j]][1], sep = " ")
  }
  print("     Completed for gageDataFrame")
}

gageDataFrame[,c("STREAM_NAME_GAGE","STREAM_OTHER_GAGE","STREAM_TYPE_GAGE", "STREAM_TRIB_GAGE")] <- sapply(c("STREAM_NAME_GAGE","STREAM_OTHER_GAGE","STREAM_TYPE_GAGE", "STREAM_TRIB_GAGE"), function(j) str_trim(gageDataFrame[,j]))

# if no stream type given, and branch word given, move to stream type
for (j in c(1:length(ls.TribsKeys))){
  print(paste("Looking for matches to STREAM TYPE", ls.TribsKeys[[j]][1],sep=" "))
  key_index <- sapply(paste("\\<",ls.TribsKeys[[j]],"$", sep = ""),grepl,gageDataFrame$STREAM_NAME_GAGE)
  match_keys <- which(apply(key_index, MARGIN = 1, any) & gageDataFrame$STREAM_TYPE_GAGE=="")
  for (i in match_keys){
    gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"] <- gsub(paste("\\<",ls.TribsKeys[[j]][which(key_index[i,])[1]],"\\>", sep = ""),"",gageDataFrame[GageRowNames[i],"STREAM_NAME_GAGE"])
    gageDataFrame[GageRowNames[i],"STREAM_TYPE_GAGE"] <- paste(gageDataFrame[GageRowNames[i],"STREAM_TYPE_GAGE"], ls.TribsKeys[[j]][1], sep = " ")
  }
  print("     Completed for gageDataFrame")
}

gageDataFrame[,"STREAM_NAME_GAGE"] <- sapply("STREAM_NAME_GAGE", function(j)  gsub("[[:punct:]]"," ",gageDataFrame[,j]))
gageDataFrame[,c("STREAM_NAME_GAGE","STREAM_OTHER_GAGE","STREAM_TYPE_GAGE", "STREAM_TRIB_GAGE")] <- sapply(c("STREAM_NAME_GAGE","STREAM_OTHER_GAGE","STREAM_TYPE_GAGE", "STREAM_TRIB_GAGE"), function(j) str_trim(gageDataFrame[,j]))

print("Finished identifying stream names")

# one-offs
gageDataFrame["8048",c("STREAM_NAME_GAGE","STREAM_TYPE_GAGE","STREAM_OTHER_GAGE","STREAM_TRIB_GAGE")] <- c("american","river","above north fork dam ca","north fork")
gageDataFrame["8049",c("STREAM_NAME_GAGE","STREAM_TYPE_GAGE","STREAM_OTHER_GAGE","STREAM_TRIB_GAGE")] <- c("american","river","above french meadows ca","middle fork")
gageDataFrame["8081",c("STREAM_NAME_GAGE","STREAM_TYPE_GAGE","STREAM_OTHER_GAGE","STREAM_TRIB_GAGE")] <- c("american","river","above fair oaks ca","")
# still not perfect because pulling "lake river" into TYPE
savefile  <- paste(gsub("-","",Sys.Date()),"gageDataFrameProcessedStreamName.RData",sep="_")
save(gageDataFrame,file=file.path(DataDir,"FrequentlyUsedArchiveData",savefile))
