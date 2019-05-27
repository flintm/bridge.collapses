Match_Failed_Bridges <- function(FailDataSeqBy,nbiDataSeqBy){
 # Match failed bridges to locations of bridges in NBI database
 # Location needed to analyze climate data, and later to look at impacts (replacement value, traffic disruption)
 # Updated to load state and county data from CSV files rather than hard-codes
 # Updated on 2014-11-17 to load pre-processed data
 # Updated on 2014-11-21 to compute matching in parallel (over states)
 # Updated on 2014-12-04 with new method for matching cities and counties
 
 # Method
 #   General : will need to do some sort of partial string matching - need to convert case, need to remove
 #   any extraneous information or characters in the route and stream information.
 #   if NBI record has STATUS = 'ST' -> flag location data as questionable
 #   if NBI record has STATUS = 'XX' -> flag location data as unavailable
 #
 #   Steps:
 #     (0) prelim: define internal data frames to store different formats of same info (state as XX, as NBI ###, as STFIPS ##)
 #     (1) convert and clean failure and NBI data frames
 #     (2) prepare data frames for matching (reduce set of NBI, fix cross-listings, fix misspellings, etc.)
 #     (3) process failure LOCATION field 
 #     (4) process failure FEAT_UND field and NBI ITEM6A to pull out stream name, type
 #     (5) match full-data failed bridges (have county, stream, and road name)
 #     (6) attempt to match partial-data failed bridges (county and road or stream) 
 #     (7) attempt to match scarce-data failed bridges (only county or road or stream)
 #     (8) for unRowsWithMatched in steps (6)-(9), use alternate approach (recursive)
 #     (9) return merged dataframe and report on un-matched and un-located failed bridges
 #
 # To-do list:  
  #     (2) write the code for step (5) above
  #     (3) write code for step (6), add to step (7) - county & road
  #     (4) add code that looks for bad data (streams in location, bridge data in stream, etc.) - step 1B?
  #     (5) write the code for step (8) above
  #     (6) write the code for step (9) above
  #     (7) see how many matches were made to figure out how should proceed
# Non-critical to-do list:
  #     (0)  add support for matching NBI bridge data (type, material)
  #     (0a) map from failure data and NBI data (would need older NBI records)
  #     (0b) find types that can use to infer material if missing (e.g., "i beam" -> steel ?)
  #     (1) Have it auto-create new CITY or COUNTY column as max number of potential matches grows
  #     (2) Have it sort the bridges after the merge with ITEM5B according to BIN
# Cleanup list
  #     (1) consistent notation
  #     (2) move all repeated processing steps and put to subfunction
  #
# EXTERNAL FUNCTION CALLS, LIBRARIES, AND DATA--------------------------------------------------------------------------- 
  # function clean.df.strings converts columns in object to lowercase, removes punctuation and extra spaces
  # function convert.df.classes converts object to provided types (e.g., "character")
  
  library(stringr)
  library(stringdist)
  library(parallel)
  source(file.path(dirs$ScriptsDirUtility,'convert.df.classes.R'))
  source(file.path(dirs$ScriptsDirUtility,'clean.df.strings.R'))
  source(file.path(dirsGit$ScriptsDir,"PerformMatch.R"))
  source(file.path(dirsGit$ScriptsDir,"MatchEntry.R"))
  load(file.path(dirs$DataDirFrequent,"StateCountyCityData.RData"))
  # source('~/Documents/Research/Climate_Scour_Local/R_data/20141205_GetAndRemoveLocationData_CityCounty.R')
  # source('~/Documents/Research/Climate_Scour_Local/R_data/20141208-GetAndRemoveLocationForStream.R')

 

# STEP 0: DEFINITIONS OF KEYWORD INDICES TO SUPPORT MATCHING  -----------------------------------------------------------------------------------------------------------


# STEP 0B: STREAM KEYWORDS ---------------------------------------------------------------------------------
ls.StreamKeys <- list(arroyo    = c("arroyo", "ary"),
                      bayou     = c("bayou", "byou", "byo","bay"),
                      brook     = c("brook", "brk", "bk", "bro", "b"),
                      creek     = c("creek", "crk", "ck", "cr"),
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
                      forks      = c("forks", "frks"),
                      gorge     = c("gorge"),
                      gully     = c("gully"),
                      hollow    = c("hollow", "hllw", "hlw", "hol", "h"),
                      inlet     = c("inlet"),
                      kill      = c("kill", "kll", "kl", "kil", "k"),
                      lakes     = c("lakes", "lake", "lks", "lk", "lak", "la", "l"),
                      lock      = c("lock"),
                      ponds     = c("ponds", "pond"),
                      river     = c("river", "rive", "rvr", "riv", "rv", "r"),
                      rio       = c("rio"),
                      run       = c("run", "rn"),
                      overflow  = c("overflow","overflo", "overf"),
                      slough    = c("slough", "slgh", "slo", "sl"),
                      spillway  = c("spillway", "spw"),
                      stream    = c("stream", "stre", "str"),
                      swamp     = c("swamp", "swmp", "swa", "sw"),
                      wash      = c("wash", "wsh", "w")
                      )

ls.TribsKeys <- list( branch    = c("branch", "brnch", "brn", "bra", "br"),
                      fork      = c("fork", "frk", "fk", "f"),
                      tributary = c("tributary", "trb", "trib", "tr", "t"),
                      sf        = c("southern fork", "sf"),
                      nf        = c("northern fork", "nf")
                      )
                      
#StreamKeys <- c(unlist(ls.StreamKeys), unlist(ls.TribsKeys))
StreamKeys     <- unlist(ls.StreamKeys)
StreamKeyIndex <- sapply(1:length(StreamKeys), function(i) grep(paste("\\<",StreamKeys[i],"\\>",sep=""),ls.StreamKeys))

# STEP 0C: ROAD KEYWORDS ------------------------------------------------------------------------------------
ls.RoadKeys <- list(avenue     = c("avenue", "ave"),
                    bridge     = c("bridge", "brdge", "brdg", "br"),                    
                    court      = c("court", "crt", "ct"),
                    crossing   = c("crossing", "crss", "crs", "cross", "xng"),
                    drive      = c("drive", "drv", "dr", "d"),
                    freeway    = c("freeway", "frwy", "fwy", "free", "f"),
                    highway    = c("highway", "hwy", "hw", "high","h"),
                    interstate = c("interstate", "int", "i"),
                    lane       = c("lane", "ln"),
                    pass       = c("pass", "pss", "pa", "p"),
                    road       = c("road", "rd"),
                    route      = c("route", "rte", "rt", "r"),
                    street     = c("street", "strt", "st"),
                    countyRoad = c("county road", "co road", "co rd", "county rd", "c r", "cr"),
                    countyHighway = c("county highway", "co highway", "co hwy"),
                    privateRoad=c("private road", "priv road", "prvt road", "priv rd", "priv road", "pr"),
                    stateRoute = c("state route", "state rte", "st rte", "st rt", "s r", "sr"),
                    stateHighway=c("state highway", "st hwy", "sh", "psh"), #psh = "primary state highway" in washington
                    stateSecondary=c("state secondary", "st secondary"),
                    thruway    = c("thruway","thwy","th"),
                    usHighway  = c("us highway", "us route", "us rte", "us")
                    )
#  ITEM5B ROUTE SIGNING PREFIX 
#    1 INTERSTATE HIGHWAY
#    2 US NUMBERED HIGHWAY
#    3 STATE HIGHWAY
#    4 COUNTY HIGHWAY
#    5 CITY STREET
#    6 FEDERAL LANDS ROAD
#    7 STATE LANDS ROAD
#    8 OTHER
df.RoadType <- data.frame(
  ROAD_TYPE = c("interstate",    "us highway",     "state highway",     "state route",   "state secondary",    "route",     "freeway",    "highway",
                 "thruway",       "county road",    "county highway",    "avenue",        "crossing",           "court",     "drive",      "lane",
                 "road",           "street",        "pass",              "private road"),
  ITEM5B    =  c(1,               2,                3,                    3,               3,                    3,            3,            3,
                 3,               4,                4,                    5,               5,                    5,            5,            5,
                 5,               5,                7,                    8),
    stringsAsFactors = FALSE)
nRoadTypes <- nrow(df.RoadType)
df.RoadType[c((nRoadTypes+1):(nRoadTypes + nrow(df.States))),"ROAD_TYPE"] <- tolower(df.States$STATE_CODE)
df.RoadType[c((nRoadTypes+1):(nRoadTypes + nrow(df.States))),"ITEM5B"]     <- 3


RoadKeys <- unlist(ls.RoadKeys)
RoadKeyIndex <- sapply(RoadKeys, function(s) grep(paste("\\<",s,"\\>",sep=""),ls.RoadKeys)[1])
ls.RteKeys <- as.list(ls.RoadKeys[c("route","freeway","highway","interstate","countyRoad","stateRoute","stateHighway","usHighway","countyHighway", "stateSecondary", "thruway")])
BridgeNoKeys <- c(paste(ls.RoadKeys$bridge,"no",sep=" "), ls.RoadKeys$bridge, "no")
StreamRoadKeys <- c(StreamKeys, RoadKeys)

RailKeys <- c("railway", "railroad", "rlwy", "rail")

# STEP 0D: CITY, CARDINAL, RELATIONAL AND LANDSCAPE KEWYORDS, AND ABBREVIATIONS ----------------------------
# CITY AND COUNTY KEYWORDS -----
ls.JurisdictionKeys <- list(city     = c("city", "cty"),
                            county   = c("county", "co", "cty"),
                            state    = c("state"),
                            town     = c("town", "twn"),
                            township = c("township", "twsp", "twp"),
                            village  = c("village")
                            )
CityKeys <- unlist(ls.JurisdictionKeys[c("city", "town", "township", "village")])
CityKeyIndex <- sapply(CityKeys, function(s) grep(paste("\\<",s,"\\>",sep=""),ls.JurisdictionKeys)[1])

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
                          near   = c("near", "nr"),
                          by     = c("by"),
                          of     = c("of"),
                          on     = c("on"),
                          over   = c("over"),
                          intersection = c("intersection", "int"),
                          junction = c("junction", "jct"),
                          in_loc   = c("in"),
                          off      = c("off"),
                          at       = c("at"),
                          miles    = c("miles", "mile", "mi", "m"),
                          kilometers = c("kilometers", "kilometer", "km", "k") 
                          )

RelationalKeys <- paste("\\<",unlist(ls.CardinalKeys[c("south","north","east","west","nw","ne","se","sw")]),"\\>",sep="")
RelationalKeys <- c(paste(RelationalKeys,"[[:space:]]{1,3}\\<",ls.RelationalKeys$of,"\\>",sep=""), paste("\\<",unlist(ls.RelationalKeys[c("near","on","over","in_loc","at")]),"\\>",sep=""))
TribRelatKeys  <- sapply(unlist(ls.RelationalKeys[c("middle", "main","left", "right")]), function(s) paste(s, unlist(ls.TribsKeys[c("branch", "fork")]), sep = " "))
TribRelatKeys <- c(TribRelatKeys, sapply(unlist(ls.CardinalKeys[c("south", "north","east", "west")]), function(s) paste(s, unlist(ls.TribsKeys[c("branch", "fork")]), sep = " ")))
#TribRelatKeys  <- c(TribRelatKeys, unlist(ls.TribsKeys))

# Landscape keywords
ls.LandscapeKeys <- list(
                        little   = c("little", "litttle", "ltle"),
                        mountain = c("mountain", "mtn"),
                        railroad = c("railway", "railroad", "rr"),
                        school   = c("school", "sch"),
                        hill     = c("hill", "hil"),
                        valley   = c("valley", "vly"),
                        south_fork = c("south fork", "sf"),
                        north_fork = c("north fork", "nf"),
                        platte   = c("platte", "plat"),
                        plantation = c("plantation", "plt"),
                        cumberland = c("cumberland","cumberlan"),
                        savannah   = c("savannah", "sav")
                        )

# COUNTY AND STATE ABBREVIATION KEYWORDS ------
ls.CountyAbbrevKeys <- list(
                      prince      = c("prince", "prin"),
                      montgomery  = c("montgomery", "montgomry", "montgom", "mont. co", "mont co"),
                      baltimore   = c("baltimore", "balt"),
                      le_sueur    = c("le sueur", "lesueur"),
                      lewis       = c("lewis", "lewisco"),
                      hamp        = c("hampshire", "hamp"),
                      doddridge   = c("doddridge", "dodridge", "dodd"),
                      pendleton   = c("pendleton", "pend"),
                      pocahontas  = c("pocahontas", "poca"),
                      rand        = c("randolph", "rand", "ran"),
                      preston     = c("preston", "pres"),
                      monongalia  = c("monongalia", "monogalia"),
                      humboldt    = c("humboldt", "humbolt"),
                      audubon     = c("audubon", "audabon"),
                      atchison    = c("atchison", "atchinson"),
                      buena_vista = c("buena vista", "buean vista"),
                      buch        = c("buchanan", "buch"),
                      geary       = c("geary" , "greary"),
                      pottawatomie   = c("pottawatomie", "pottawotamie"),
                      prince_georges = c("prince george", "prince georges", "p geor", "p geo", "prince george", "prince georg", "p. geor"),
                      saint_clair    = c("saint clair", "st. clair", "st clair"),
                      saint_louis    = c("saint louis", "st louis", "st. louis"),
                      saint_charles  = c("saint charles", "st. charles", "st charles"),
                      burleigh       = c("burleigh", "bruliegh", "bruleigh"),
                      northumberland = c("northumberland", "no umberland"),
                      anasco         = c("añasco", "anasco"),
                      prince_william = c("prince william", "pr. william", "pr william"),
                      rappahannock   = c("rappahannock", "rappahannaok"),
                      albemarle      = c("albemarle", "abermarle", "abemarle"),
                      valencia       = c("valencia", "velencia"),
                      saint_george   = c("saint george", "st. george", "st george"),
                      denison        = c("denison", "denisen"),
                      arrington      = c("arrington", "arrinton"),
                      bedford        = c("bedford", "beford"),
                      louisa         = c("louisa", "lousia"),
                      nicolaus       = c("nicolaus", "nicholaus"),
                      saint_helen    = c("saint helen", "st. helen", "st helen"),
                      saint_john     = c("saint john", "st. john", "st john"),
                      saint_ann      = c("saint ann", "st. ann", "st ann"),
                      mercer         = c("mercer", "mercert"),
                      bismarck       = c("bismarck", "bismark"),
                      ill            = c("illinois", "ill"),
                      traill         = c("traill county", "trail county")
                      )

ls.AbbrevGreen <- list(
                        AL = c( 1, "greene"),
                        AZ = c( 4, "greenlee"),
                        AR = c( 5, "greene"),
                        GA = c( 13, "greene"),
                        IL = c( 17, "greene"),
                        IN = c( 18, "greene"),
                        IA = c( 19, "greene"),
                        KS = c( 20, "greenwood"),
                       # KY = c(21, "green", "greenup"),
                        MS = c( 28, "greene"),
                        MO = c( 29, "greene"),
                        NY = c( 36, "greene"),
                        NC = c( 37, "greene"),
                        OH = c( 39, "greene"),
                        PA = c( 42, "greene"),
                       # SC = c(45, "greenville", "greenwood"),
                        TN = c( 47, "greene"),
                        TX = c( 48, "tom green"),
                      #  VA = c(51, "greene", "greensville"),
                        WV = c( 54, "greenbrier")
                       # WI = (55, "green", "green lake")
                       )
StatesWithGreenCounty <- as.integer(unlist(lapply(ls.AbbrevGreen, function(l) l[[1]][1] )))

ls.AbbrevWeb   <- list(
                        GA = c( 13, "webster"),
                        IA = c( 19, "webster"),
                        KY = c( 21, "webster"),
                        LA = c( 22, "webster"),
                        MS = c( 28, "webster"),
                        MO = c( 29, "webster"),
                        NE = c( 31, "webster"),
                        TX = c( 48, "webb"),
                        UT = c( 49, "weber"),
                        WV = c( 54, "webster")
                      )
StatesWithWebCounty <- as.integer(unlist(lapply(ls.AbbrevWeb, function(l) l[[1]][1] )))

StatesToCheckCountyNames <- unique(c(StatesWithGreenCounty, StatesWithWebCounty))

# STEP 1A: PRE-PROCESSING OF DATAFRAMES -----------------------------------------------------------------------------------------------------------
# LOAD PRE-PROCESSED BRIDGE DATA
 load(file.path(dirs$DataDirFrequent,"nbiDataFramePreProcessedLocationFields.RData"))
 nbiDataFrame <- nbiDataFrame[seq(1,nrow(nbiDataFrame),by=nbiDataSeqBy),]
 nbiRowNames <- rownames(nbiDataFrame)
 
# LOAD PRE-PROCESSED FAILURE DATA
# load("~/Documents/Research/Climate_Scour_Local/R_data/FailDataFramePreProcessedLocationFields.RData")
# FailColNames <- colnames(FailDataFrame)


# FailDataFrame <- FailDataFrame[seq(1,nrow(FailDataFrame),by=FailDataSeqBy),]
# FailDataFrame.Char <- c("LOCATION", "FEAT_UND")
# FailRowNames <- rownames(FailDataFrame)
# nFail <- nrow(FailDataFrame)
nNBI <- nrow(nbiDataFrame)
print("Processed NBI and Failure dataframes loaded")

# STEP 2: PRELIMINARIES FOR MATCHING - REDUCE SIZE,  CORRECT MIS-SPELLINGS -------------------------------------------
# reduce size of data set (delete rows in NBI dataframe from states where don't have any failure data)
# FailDataFrame <- merge(FailDataFrame,df.States[,c("STATE_CODE","STFIPS")],by="STATE_CODE")
# FailStates    <- unique(FailDataFrame$STFIPS)

# nbiDataFrame <- subset(nbiDataFrame, nbiDataFrame$STFIPS %in% FailStates)
nbiRowNames <- rownames(nbiDataFrame)
# FailRowNames  <- rownames(FailDataFrame)

# FailDataFrame$FLAG_LOCATION <- integer(nFail)
# FailDataFrame$FLAG_STREAMS  <- integer(nFail)

# # substitute all abbreviations for counties, other critical features
# for (j in c(1:length(ls.CountyAbbrevKeys))){
#   key_index <- sapply(paste("\\<",ls.CountyAbbrevKeys[[j]][c(2:length(ls.CountyAbbrevKeys[[j]]))],"\\>[\\.]?", sep = ""),grepl,FailDataFrame$LOCATION)
#   match_keys <- which(apply(key_index, MARGIN = 1, any))
#   for (i in match_keys){  
#     FailDataFrame[FailRowNames[i],"LOCATION"] <- gsub(paste("\\<",ls.CountyAbbrevKeys[[j]][which(key_index[i,])[1]+1],"\\>[\\.]?", sep = ""),ls.CountyAbbrevKeys[[j]][1],FailDataFrame[FailRowNames[i],"LOCATION"])
#     FailDataFrame[FailRowNames[i], "FLAG_LOCATION"] <- FailDataFrame[FailRowNames[i], "FLAG_LOCATION"] + 1
#     
#   }
# }
# 
# for (j in c(1:length(ls.LandscapeKeys))){
#   key_index_loc <- sapply(paste("\\<",ls.LandscapeKeys[[j]][c(2:length(ls.LandscapeKeys[[j]]))],"\\>", sep = ""),grepl,FailDataFrame$LOCATION)
#   match_keys_loc <- which(apply(key_index_loc, MARGIN = 1, any))
#   key_index_stream <- sapply(paste("\\<",ls.LandscapeKeys[[j]][c(2:length(ls.LandscapeKeys[[j]]))],"\\>", sep = ""),grepl,FailDataFrame$FEAT_UND)
#   match_keys_stream <- which(apply(key_index_stream, MARGIN = 1, any))
#   for (i in match_keys_loc){
#     FailDataFrame[FailRowNames[i],"LOCATION"] <- gsub(paste("\\<",ls.LandscapeKeys[[j]][which(key_index_loc[i,])[1]+1],"\\>", sep = ""),ls.LandscapeKeys[[j]][1],FailDataFrame[FailRowNames[i],"LOCATION"])
#     FailDataFrame[FailRowNames[i], "FLAG_LOCATION"] <- FailDataFrame[FailRowNames[i], "FLAG_LOCATION"] + 1
#   }
#   for (i in match_keys_stream){
#     FailDataFrame[FailRowNames[i],"FEAT_UND"] <- gsub(paste("\\<",ls.LandscapeKeys[[j]][which(key_index_stream[i,])[1]+1],"\\>", sep = ""),ls.LandscapeKeys[[j]][1],FailDataFrame[FailRowNames[i],"FEAT_UND"])
#     FailDataFrame[FailRowNames[i], "FLAG_STREAMS"] <- FailDataFrame[FailRowNames[i], "FLAG_STREAMS"] + 1
#   }
# }
# 
# # get rid of all possessives ("stabler's road")
# FailDataFrame$LOCATION <- gsub("\\'s\\>","s", FailDataFrame$LOCATION)
# FailDataFrame$FEAT_UND <- gsub("\\'s\\>","s", FailDataFrame$FEAT_UND)
nbiDataFrame$ITEM7 <- gsub("\\'s\\>","s", nbiDataFrame$ITEM7)
nbiDataFrame$ITEM6A <- gsub("\\'s\\>","s", nbiDataFrame$ITEM6A)
nbiDataFrame$ITEM9 <- gsub("\\'s\\>","s", nbiDataFrame$ITEM9)

# substitute special abbreviations in county names (Green/Greene/etc., Web/Webster/Webb/etc.)
# FailStatesToCheck <- StatesToCheckCountyNames[StatesToCheckCountyNames %in% FailStates]
# 
# for (j in FailStatesToCheck){
#   rowsForState <-  FailRowNames[FailDataFrame$STFIPS == j]
#   if (j %in% StatesWithGreenCounty){ 
#     RowsWithGreenCounty <- which(grepl("\\<green\\>", FailDataFrame[rowsForState,"LOCATION"]) & grepl("co", FailDataFrame[rowsForState,"LOCATION"]))
#     for (i in RowsWithGreenCounty) {
#       FailDataFrame[rowsForState[i],"LOCATION"] <- gsub("\\<green\\>",ls.AbbrevGreen[[which(unlist(lapply(ls.AbbrevGreen, function(l) l[[1]][1]==j)))]][2],FailDataFrame[rowsForState[i],"LOCATION"])
#       FailDataFrame[rowsForState[i],"FLAG_LOCATION"] <- FailDataFrame[rowsForState[i],"FLAG_LOCATION"] + 1
#       }
#     }
#   if (j %in% StatesWithWebCounty){ 
#     RowsWithWebCounty <- which(grepl("\\<web\\>", FailDataFrame[rowsForState,"LOCATION"]) & grepl("co", FailDataFrame[rowsForState,"LOCATION"]))
#     for (i in RowsWithWebCounty) {
#       FailDataFrame[rowsForState[i],"LOCATION"] <- gsub("\\<web\\>",ls.AbbrevWeb[[which(unlist(lapply(ls.AbbrevWeb, function(l) l[[1]][1]==j)))]][2],FailDataFrame[rowsForState[i],"LOCATION"])
#       FailDataFrame[rowsForState[i],"FLAG_LOCATION"] <- FailDataFrame[rowsForState[i],"FLAG_LOCATION"] + 1
#     }
#   }
# }
# # one-off corrections
# if (FailDataSeqBy==1){
#   FailDataFrame[583,"LOCATION"] <- gsub("richmond", "richland", FailDataFrame[583,"LOCATION"])
#   FailDataFrame[170,"LOCATION"] <- gsub("wash.", "washington", FailDataFrame[170,"LOCATION"])
#   FailDataFrame[108,"LOCATION"] <- gsub("amer.", "american", FailDataFrame[108,"LOCATION"])
#   FailDataFrame[145,"LOCATION"] <- gsub("s4&5","s4-5", FailDataFrame[145,"LOCATION"])
#   FailDataFrame[1021,"LOCATION"] <- paste(FailDataFrame[1021,"LOCATION"],")",sep="")
#   FailDataFrame[FailDataFrame$ID=="70","FEAT_UND"] <- sub("claf","calf",FailDataFrame[FailDataFrame$ID=="70","FEAT_UND"])
# }
# print("Finished preliminaries for matching")
# save(FailDataFrame, file="FailDataFrameProcessedAbbreviations.RData")


# STEP 3: PROCESS "LOCATION" FIELD - COUNTY, CITY, ROAD/ROUTE  ----------------------------------------------------------
nbiDataFrame$FIPS      <- nbiDataFrame$STFIPS*1000+nbiDataFrame$ITEM3
# NoLocationDataIndex    <- rownames(which(FailDataFrame$LOCATION==""))
# HasLocationDataIndex   <- subset(FailRowNames, !(FailRowNames %in% NoLocationDataIndex) )

# county
# CountyOutCols <- list(c("COUNTY_NAME", "COUNTY_NAME_2"),c("FIPS","FIPS_2"))
# FailDataFrame[,unlist(CountyOutCols)] <- character(nFail)
# # city
# CityOutCols   <- list(c("CITY_NAME", "CITY_NAME_2", "CITY_NAME_3"),c("FIPS_FROM_CITY","FIPS_FROM_CITY_2","FIPS_FROM_CITY_3"),
#                       c("ANSICODE", "ANSICODE_2", "ANSICODE_3"), c("GNIS_ID", "GNIS_ID_2", "GNIS_ID_3"))
# FailDataFrame[,unlist(CityOutCols)] <- character(nFail)
# # road
# LocProcessColsIn  <- c("ID",         "ROAD_NAME",  "FLAG_LOCATION",     "ROAD_NEAR",   "ROAD_TYPE",  "ROUTE_NO",  "RD_RELATIONAL", "RTE_DIRECTION",  "RD_OTHER", "RTE_SECOND",  "BRIDGE_NAME_NO", "STREAM_NAME_FROM_LOC", "STREAM_TYPE_FROM_LOC")
# #                   e.g., from FailDF, "mchenry"    if bad match     "school"          ITEM5B           ITEM5D          "15 miles"         ITEM5E    unclassified   second entry  "San Mateo" "Lobitos"           "Creek"               
# LocProcessColsOut <- LocProcessColsIn
# FailDataFrame$ROAD_NAME     <- FailDataFrame$LOCATION
# FailDataFrame$FLAG_LOCATION      <- integer(nFail)
# FailDataFrame[, LocProcessColsIn[3:length(LocProcessColsIn)]] <- character(nFail)
# FailDataFrame$STREAM_TRIB <- character(nFail)
# FailDataFrame$RAIL_NAME <- character(nFail)
# 
# for (j in FailStates){
#   rowsForState <-  FailRowNames[FailDataFrame$STFIPS == j & !FailDataFrame$LOCATION==""]
#   nRowsState   <- length(rowsForState)
#   print(paste("Processing LOCATION field for bridges in state",df.States[df.States$STFIPS==j, "STATE_CODE"],sep=" "))
#   
#   StateCounties <- df.Counties[df.Counties$STFIPS == j, c("COUNTY_NAME", "FIPS")] 
#   StateCounties$COUNTY_NAME <- tolower(StateCounties$COUNTY_NAME)  
#   
#   StateCities <- df.Cities[df.Cities$STFIPS == j, c("CITY_NAME", "FIPS", "GNIS_ID", "ANSICODE")] 
#   StateCities$CITY_NAME <- tolower(StateCities$CITY_NAME)
#   
#   ls.RteKeysState <- ls.RteKeys
#   ls.RteKeysState$stateName[1] <- tolower(df.States[df.States$STFIPS==j,"STATE_CODE"])
#   ls.RteKeysState$stateName[2] <- tolower(df.States[df.States$STFIPS==j,"STATE_FULL"])
#   single_abbrev <- substr(ls.RteKeysState$stateName[1],1,1)
#   if (all(single_abbrev != c("i", "r", "p"))) ls.RteKeysState$stateName[3] <- single_abbrev
#   ls.RteKeysState$stateName <- unlist(ls.RteKeysState$stateName)
#   RteKeys <- unlist(ls.RteKeysState)
#   RteKeyIndex <- sapply(RteKeys, function(s) grep(paste("\\<",s,"\\>",sep=""),ls.RteKeysState)[1])
#   
#   # SPECIAL PROCESSING FOR ARKANSAS AND IOWA, WHERE HAVE ##-##-## entries
#   if (j == 5 | j == 19){
#     # with km/mi posting (?)
#     RowsWithDashedMatch <- rowsForState[grep("[[:alpha:]]{1,2}[[:space:]]?[[:digit:]]{1,4}-[[:graph:]]{1,}",FailDataFrame[rowsForState,"ROAD_NAME"])]
#     unRowsWithMatchedDashedrows  <- rowsForState[!(rowsForState %in% RowsWithDashedMatch)]
#     RowsWithDashedMatch2 <- unRowsWithMatchedDashedrows[grep("[[:alpha:]]?[[:digit:]]{1,4}",FailDataFrame[unRowsWithMatchedDashedrows,"ROAD_NAME"])]
#     RowsWithDashedMatch  <- unique(c(RowsWithDashedMatch, RowsWithDashedMatch2))
#     nRowsWithMatch       <- length(RowsWithDashedMatch)
#     DashedSubstrings  <- lapply(RowsWithDashedMatch, function(i) unlist(strsplit(strsplit(FailDataFrame[i,"ROAD_NAME"],"-")[[1]],"[[:space:]]")))
#     nDashedSubstrings <- sapply(1:nRowsWithMatch, function(i) length(DashedSubstrings[[i]]))
#     route_index       <- sapply(1:nRowsWithMatch, function(i) min(grep("[[:digit:]]",DashedSubstrings[[i]])))
#     nWithNumbers      <- sapply(1:nRowsWithMatch, function(i) max(grep("[[:digit:]]",DashedSubstrings[[i]])))
#     HasMultipleNumberStrings <- (nWithNumbers - route_index) >= 1
#     HasExtraStrings          <- nWithNumbers < nDashedSubstrings 
#     HasPrefixStrings         <- route_index > 1
#     FailDataFrame[RowsWithDashedMatch,"ROUTE_NO"]   <- sapply(1:nRowsWithMatch, function(i) gsub("[[:punct:]]","",DashedSubstrings[[i]][route_index[i]]))
#     FailDataFrame[RowsWithDashedMatch,"RD_OTHER"]   <- sapply(1:nRowsWithMatch, function(i) ifelse(HasMultipleNumberStrings[i], gsub(",","",paste(DashedSubstrings[[i]][c((route_index[i]+1):nWithNumbers[i])],collapse="-")),""))
#     FailDataFrame[RowsWithDashedMatch,"ROAD_NAME"]  <- sapply(1:nRowsWithMatch, function(i) ifelse(HasPrefixStrings[i], paste(DashedSubstrings[[i]][c(1:(route_index[i]-1))],collapse=" "),""))
#     FailDataFrame[RowsWithDashedMatch,"ROAD_NAME"]  <- sapply(1:nRowsWithMatch, function(i) ifelse(HasExtraStrings[i], paste(FailDataFrame[RowsWithDashedMatch[i],"ROAD_NAME"],DashedSubstrings[[i]][c((nWithNumbers[i]+1):nDashedSubstrings[i])],collapse=" "),FailDataFrame[RowsWithDashedMatch[i],"ROAD_NAME"]))
# 
#    # now find road information
#     HasDashedRoadTypeBool   <- sapply(paste("\\<",RteKeys,"\\>",sep=""), grepl, gsub("[[:punct:]]","",FailDataFrame[RowsWithDashedMatch,"ROAD_NAME"]))
#     if (nRowsWithMatch == 1) HasDashedRoadTypeBool <- t(HasDashedRoadTypeBool) 
#     RowsWithRoadTypeMatchIndex <- which(rowSums(HasDashedRoadTypeBool) != 0)
#     RowsWithRoadTypeMatch   <- RowsWithDashedMatch[RowsWithRoadTypeMatchIndex]
#     nRowsWithMatch <- length(RowsWithRoadTypeMatch)
#     if (nRowsWithMatch >= 1){
#       MatchedDashedRoadIndex  <- sapply(1:nRowsWithMatch, function(i) max(which(HasDashedRoadTypeBool[RowsWithRoadTypeMatchIndex[i],])))
#       if (class(MatchedDashedRoadIndex) == "list") MatchedDashedRoadIndex <- unlist(MatchedDashedRoadIndex)
#       FailDataFrame[RowsWithRoadTypeMatch,"ROAD_TYPE"]   <- sapply(1:length(RowsWithRoadTypeMatch), function(i) ls.RteKeysState[[RteKeyIndex[MatchedDashedRoadIndex[i]]]][1])
#       FailDataFrame[RowsWithRoadTypeMatch,"ROAD_NAME"]   <- sapply(1:length(RowsWithRoadTypeMatch), function(i) gsub(RteKeys[MatchedDashedRoadIndex[i]],"",gsub("[[:punct:]]","",FailDataFrame[RowsWithRoadTypeMatch[i],"ROAD_NAME"])))
#     }
#   if (j==19){
#     IowaCountyHighways <- RowsWithDashedMatch[grepl("[[:alpha:]][[:digit:]]{1,4}", FailDataFrame[RowsWithDashedMatch,"ROUTE_NO"])&FailDataFrame[RowsWithDashedMatch,"ROAD_TYPE"]==""]
#     FailDataFrame[IowaCountyHighways,"ROAD_TYPE"]   <- "county highway"
#   }  
#   rowsForState <-  FailRowNames[FailDataFrame$STFIPS == j & !FailDataFrame$ROAD_NAME==""]
#   nRowsState   <- length(rowsForState)
#   if (nRowsState == 0) next
#   }
#   # special processing for Ohio and Tenessee, probably bridge numbers
#   if (j==39 | j==47){
#     MatchRegx           <- "\\<[[:alnum:]]{1,3}-[[:alnum:]]{1,5}[[:punct:]]?[[:alnum:]]{0,3}[[:punct:]]?[[:alnum:]]{0,3}\\>"
#     RowsWithDashedMatch <- rowsForState[grep(MatchRegx,FailDataFrame[rowsForState,"ROAD_NAME"])]
#     nRowsWithMatch <- length(RowsWithDashedMatch)
#     for (i in RowsWithDashedMatch){
#       match_start <- regexpr(MatchRegx,FailDataFrame[i,"ROAD_NAME"])
#       match_last  <- match_start + attr(match_start,"match.length") - 1
#       FailDataFrame[i,"BRIDGE_NAME_NO"] <- substr(FailDataFrame[i,"ROAD_NAME"],match_start,match_last)
#       FailDataFrame[i,"ROAD_NAME"] <- gsub(MatchRegx,"",FailDataFrame[i,"ROAD_NAME"])
#     } 
#     rowsForState <-  FailRowNames[FailDataFrame$STFIPS == j & !FailDataFrame$ROAD_NAME==""]
#     nRowsState   <- length(rowsForState)
#     if (nRowsState == 0) next
#   }
  
#   # FIND RELATIONAL STATEMENTS
#   # statements with explicit distance, e.g., "3.5 miles from Stanford"
#   MatchRegex <- "[[:digit:]]{1,3}[[:punct:]]?[[:digit:]]?[[:space:]]?m[i]{0,1}[l]{0,1}[e]{0,1}[s]{0,1}[[:punct:]]?[[:space:]]"
#   RowsWithDistance <- rowsForState[grep(MatchRegex,FailDataFrame[rowsForState,"ROAD_NAME"])]
#   for (i in RowsWithDistance){
#     if (grepl("[[:digit:]]m",FailDataFrame[i,"ROAD_NAME"])){ # if no space between # and "mi", add
#       make_space_first <- regexpr("[[:digit:]]m",FailDataFrame[i,"ROAD_NAME"])
#       FailDataFrame[i,"ROAD_NAME"] <- paste(substr(FailDataFrame[i,"ROAD_NAME"],1,make_space_first),substr(FailDataFrame[i,"ROAD_NAME"],make_space_first+1,nchar(FailDataFrame[i,"ROAD_NAME"])))
#     }
#     match_start <- regexpr(MatchRegex,FailDataFrame[i,"ROAD_NAME"])
#     match_last  <- match_start + attr(match_start, "match.length") - 1
#     # find distance
#     dist_start  <- regexpr("[[:digit:]]{1,3}[[:punct:]]{0,1}[[:digit:]]{0,1}",FailDataFrame[i,"ROAD_NAME"])
#     dist_last  <- dist_start + attr(dist_start, "match.length") - 1
#     FailDataFrame[i,"RD_RELATIONAL"] <- paste(substr(FailDataFrame[i,"ROAD_NAME"],dist_start,dist_last),"mi",sep=" ")
#     FailDataFrame[i,"ROAD_NAME"] <- substr(FailDataFrame[i,"ROAD_NAME"],match_last+1,nchar(FailDataFrame[i,"ROAD_NAME"]))      
#     # look for cardinal direction (north, nw, sw...)
#     substrings <- unlist(strsplit(FailDataFrame[i,"ROAD_NAME"],"[[:space:]]"))
#     directionalWord <- ls.CardinalKeys[[grep(paste("\\<",substrings[1],"\\>",sep=""), ls.CardinalKeys)]][1]
#     FailDataFrame[i,"RD_RELATIONAL"] <- paste(FailDataFrame[i,"RD_RELATIONAL"],directionalWord,"of",sep=" ")      
#     # look for place
#     placeSubstring <- ifelse(grepl("\\<of\\>",substrings[2]), 3, 2)
#     relationalPlace <- substrings[placeSubstring]
#     if (placeSubstring ==2){
#       FailDataFrame[i,"ROAD_NAME"] <- gsub("\\<of\\>","",FailDataFrame[i,"ROAD_NAME"])
#     }
#     PlaceIsCounty <- relationalPlace %in% StateCounties[,"COUNTY_NAME"]
#     PlaceIsCity   <-  relationalPlace %in% StateCities[,"CITY_NAME"]
#     if (PlaceIsCounty){
#       MatchedCountyIndex <- grep(paste("\\<",relationalPlace,"\\>",sep=""),StateCounties[,"COUNTY_NAME"])
#       FailDataFrame[i,LocProcessColsOut] <- GetAndRemoveLocationData(FailDataFrame[i, LocProcessColsIn], relationalPlace, LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, FALSE, TRUE)
#       FailDataFrame[i,"COUNTY_NAME"] <- StateCounties[MatchedCountyIndex,"COUNTY_NAME"]
#       FailDataFrame[i,"FIPS"] <- StateCounties[MatchedCountyIndex,"FIPS"]
#       if (PlaceIsCity){
#         MatchedCityIndex <- grep(paste("\\<",relationalPlace,"\\>",sep=""),StateCities[,"CITY_NAME"])
#         FailDataFrame[i,"CITY_NAME"] <- StateCities[MatchedCityIndex,"CITY_NAME"]
#         FailDataFrame[i,"FIPS_FROM_CITY"] <- StateCities[MatchedCityIndex,"FIPS"]
#         FailDataFrame[i,"ANSICODE"] <- StateCities[MatchedCityIndex,"ANSICODE"]
#         FailDataFrame[i,"GNIS_ID"] <- StateCities[MatchedCityIndex,"GNIS_ID"]
#       }
#     }
#     else{
#       if (PlaceIsCity){
#         MatchedCityIndex <- grep(paste("\\<",relationalPlace,"\\>",sep=""),StateCities[,"CITY_NAME"])
#         FailDataFrame[i,LocProcessColsOut] <- GetAndRemoveLocationData(FailDataFrame[i, LocProcessColsIn], relationalPlace, LocProcessColsOut,ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, TRUE, FALSE)
#         FailDataFrame[i,"CITY_NAME"] <- StateCities[MatchedCityIndex,"CITY_NAME"]
#         FailDataFrame[i,"FIPS_FROM_CITY"] <- StateCities[MatchedCityIndex,"FIPS"]
#         FailDataFrame[i,"ANSICODE"] <- StateCities[MatchedCityIndex,"ANSICODE"]
#         FailDataFrame[i,"GNIS_ID"] <- StateCities[MatchedCityIndex,"GNIS_ID"]
#       }
#       else{
#         FailDataFrame[i,"ROAD_NEAR"] <- paste(FailDataFrame[i,"ROAD_NEAR"],relationalPlace,"-",sep=" ")
#       }
#     }
#     FailDataFrame[i,"ROAD_NAME"] <- sub(MatchRegex,"",FailDataFrame[i,"ROAD_NAME"])
#     FailDataFrame[i,"ROAD_NAME"] <- sub(paste("\\<",substrings[1],"\\>",sep=""),"",FailDataFrame[i,"ROAD_NAME"])
#     FailDataFrame[i,"ROAD_NAME"] <- sub("\\<of\\>","",FailDataFrame[i,"ROAD_NAME"])
#   }
#   # find relational, no distance ("west of")
#   HasRelationalBool <- sapply(RelationalKeys, grepl, gsub("[[:punct:]]","",FailDataFrame[rowsForState,"ROAD_NAME"]))
#   if (nRowsState == 1) HasRelationalBool <- t(HasRelationalBool) 
#   RowsWithRelationalMatchIndex <- which(rowSums(HasRelationalBool) != 0)
#   RowsWithRelationalMatch   <- rowsForState[RowsWithRelationalMatchIndex]  
#   nRowsWithMatch <- length(RowsWithRelationalMatch)
#   if (nRowsWithMatch >= 1){ 
#     MatchedRelationalIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasRelationalBool[RowsWithRelationalMatchIndex[i],])))
#     if (class(MatchedRelationalIndex)=="list") MatchedRelationalIndex <- unlist(MatchedRelationalIndex)
#     for (i in 1:nRowsWithMatch){
#        match_start <- regexpr(paste("\\<",RelationalKeys[MatchedRelationalIndex[i]],"\\>",sep=""),gsub("[[:punct:]]","",FailDataFrame[RowsWithRelationalMatch[i],"ROAD_NAME"]))
#        match_last  <- match_start + attr(match_start,"match.length") - 1
#       FailDataFrame[RowsWithRelationalMatch[i],"RD_RELATIONAL"] <- substr(gsub("[[:punct:]]","",FailDataFrame[RowsWithRelationalMatch[i],"ROAD_NAME"]),match_start,match_last)
#       FailDataFrame[RowsWithRelationalMatch[i],"ROAD_NAME"] <- gsub(paste("\\<",RelationalKeys[MatchedRelationalIndex[i]],"\\>",sep=""),"",gsub("[[:punct:]]","",FailDataFrame[RowsWithRelationalMatch[i],"ROAD_NAME"]))
#     }
#   }
#   
#   # FIND BRIDGE NO (I.E., ANY NUMBERS IN PARENTHESES OR WITH "NO" BUT NOT "ROUTE NO" OR SIMILAR)
#   # Assumed bridge numbers in parentheticals
#   MatchRegex <- "\\([[:digit:]]{0,5}[[:punct:]]?[[:alpha:]]{0,2}[[:punct:]]?[[:digit:]]{0,5}\\>\\)"
#   RowsWithParentheticalMatchIndex <- grep(MatchRegex, FailDataFrame[rowsForState,"ROAD_NAME"])
#   RowsWithParentheticalMatch <- rowsForState[RowsWithParentheticalMatchIndex]
#   nRowsWithMatch <- length(RowsWithParentheticalMatch)
#   if (nRowsWithMatch >= 1){ 
#    # print(RowsWithParentheticalMatch)
#     for (i in 1:nRowsWithMatch){
#       match_start <- regexpr(MatchRegex,FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"])
#       match_last  <- match_start + attr(match_start,"match.length") - 1
#       FailDataFrame[RowsWithParentheticalMatch[i],"BRIDGE_NAME_NO"] <- substr(FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"],match_start+1,match_last-1)
#       FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"] <- gsub(MatchRegex,"",FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"])
#     }
#   }
#   # ones that say "bridge no" or similar
#   HasBridgeNoBool <- sapply(paste("\\<",BridgeNoKeys,"\\>[[:space:]]{0,2}[[:digit:]]{1,5}",sep=""), grepl, gsub("[[:punct:]]","",FailDataFrame[rowsForState,"ROAD_NAME"]))
#   if (nRowsState == 1) HasBridgeNoBool <- t(HasBridgeNoBool) 
#   RowsWithBridgeNoMatchIndex <- which(rowSums(HasBridgeNoBool) != 0)
#   RowsWithBridgeNoMatch   <- rowsForState[RowsWithBridgeNoMatchIndex]
#   nRowsWithMatch <- length(RowsWithBridgeNoMatch)
#   if (nRowsWithMatch >= 1){ 
#     MatchedBridgeNoIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasBridgeNoBool[RowsWithBridgeNoMatchIndex[i],])))
#     if (class(MatchedBridgeNoIndex)=="list") MatchedBridgeNoIndex <- unlist(MatchedBridgeNoIndex)
#     for (i in 1:nRowsWithMatch){
#       match_start <- regexpr(paste("\\<",BridgeNoKeys[MatchedBridgeNoIndex[i]],"\\>[[:space:]]{0,2}[[:digit:]]{1,5}",sep=""),gsub("[[:punct:]]","",FailDataFrame[RowsWithBridgeNoMatch[i],"ROAD_NAME"]))
#       match_last  <- match_start + attr(match_start,"match.length") - 1
#       br_start   <- regexpr("[[:digit:]]{1,4}",FailDataFrame[RowsWithBridgeNoMatch[i],"ROAD_NAME"])
#       FailDataFrame[RowsWithBridgeNoMatch[i],"BRIDGE_NAME_NO"] <- substr(gsub("[[:punct:]]","",FailDataFrame[RowsWithBridgeNoMatch[i],"ROAD_NAME"]),br_start,match_last)
#       FailDataFrame[RowsWithBridgeNoMatch[i],"ROAD_NAME"] <- gsub(paste("\\<",BridgeNoKeys[MatchedBridgeNoIndex[i]],"\\>[[:space:]]{0,2}[[:digit:]]{1,5}",sep=""),"",gsub("[[:punct:]]","",FailDataFrame[RowsWithBridgeNoMatch[i],"ROAD_NAME"]))
#     }
#   }
#   if (j==47){ # TN bridges sometimes begin with FAU
#     MatchRegex <- "\\<fau[[:space:]]?[[:alpha:]]{0,3}[[:space:]]?[[:digit:]]{1,5}\\>"
#     RowsWithFauMatch <- rowsForState[grep(MatchRegex,FailDataFrame[rowsForState,"ROAD_NAME"])]
#     nRowsWithMatch <- length(RowsWithFauMatch)
#     if (nRowsWithMatch >= 1){
#       for (i in RowsWithFauMatch){
#         match_start <- regexpr(MatchRegex,FailDataFrame[i,"ROAD_NAME"])
#         match_last  <- match_start + attr(match_start,"match.length") - 1
#         FailDataFrame[i,"BRIDGE_NAME_NO"] <- paste(FailDataFrame[i,"BRIDGE_NAME_NO"],substr(FailDataFrame[i,"ROAD_NAME"],match_start,match_last), sep=" ")
#         FailDataFrame[i,"ROAD_NAME"]      <- gsub(MatchRegex,"",FailDataFrame[i,"ROAD_NAME"])
#       }
#     }
#   }
#   # ones that say ___ ___ bridge
#   HasBridgeBool <- sapply(paste("\\<",ls.RoadKeys$bridge,"\\>",sep=""), grepl, gsub("[[:space:]]+|[[:space:]]"," ",gsub("[[:punct:]]","",FailDataFrame[rowsForState,"ROAD_NAME"])))
#   if (nRowsState == 1) HasBridgeBool <- t(HasBridgeBool) 
#   RowsWithBridgeMatchIndex <- which(rowSums(HasBridgeBool) != 0)
#   RowsWithBridgeMatch   <- rowsForState[RowsWithBridgeMatchIndex]
#   nRowsWithMatch <- length(RowsWithBridgeMatch)
#   if (nRowsWithMatch >= 1){ 
#     MatchedBridgeIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasBridgeBool[RowsWithBridgeMatchIndex[i],])))
#     if (class(MatchedBridgeIndex)=="list") MatchedBridgeIndex <- unlist(MatchedBridgeIndex)
#     for (i in 1:nRowsWithMatch){
#       temp_entry  <- str_trim(gsub("[[:punct:]]","",FailDataFrame[RowsWithBridgeMatch[i],"ROAD_NAME"]))
#       match_start <- regexpr(paste("\\<",ls.RoadKeys$bridge[MatchedBridgeIndex[i]],"\\>",sep=""),temp_entry)
#       match_last  <- match_start + attr(match_start,"match.length") - 1
#       FailDataFrame[RowsWithBridgeMatch[i],"BRIDGE_NAME_NO"] <- substr(temp_entry,1,match_start-1)
#       HasStreamBool <- sapply(paste("\\<",StreamKeys,"\\>",sep=""), grepl, substr(temp_entry,1,match_start-1))
#       HasRoadBool   <- sapply(paste("\\<",RoadKeys,"\\>",sep=""), grepl, substr(temp_entry,match_last+1,match_last+5))
#       if (any(HasStreamBool)){
#         FailDataFrame[RowsWithBridgeMatch[i],"ROAD_NAME"]      <- paste(substr(temp_entry,1,match_start-1),
#                                                                         substr(temp_entry,match_last+1,nchar(temp_entry)), sep = ",")
#       }
#       else{
#         if (any(HasRoadBool)){
#           # do nothing
#         }
#         else{
#           FailDataFrame[RowsWithBridgeMatch[i],"ROAD_NAME"]      <- substr(temp_entry,match_last+1,nchar(temp_entry))
#         }
#       }
#     }
#   }
#   
#   # GET ROUTE NUMBER
#   MatchRegex <- "\\<[[:alpha:]]{0,6}[[:punct:]]?[[:space:]]?[[:alpha:]]{1,9}[[:punct:]]?[[:space:]]?[[:alpha:]]{0,2}[[:space:]]?[[:digit:]]{1,4}[[:space:]]?[[:alpha:]]?\\>"
#   RowsWithRteNo   <- rowsForState[grep(MatchRegex,FailDataFrame[rowsForState,"ROAD_NAME"])]
#   RteStrings      <- list()
#   RemainStrings   <- list()
#   for (i in RowsWithRteNo){
#     if (j==72){ # have different encodings for Puerto Rico, throws off regexpr
#       match_start <- regexpr(MatchRegex,FailDataFrame[i,"ROAD_NAME"],useBytes=TRUE)
#       rte_start   <- regexpr("[[:digit:]]{1,4}",FailDataFrame[i,"ROAD_NAME"],useBytes=TRUE)
#     }
#     else {
#       match_start <- regexpr(MatchRegex,FailDataFrame[i,"ROAD_NAME"])
#       rte_start   <- regexpr("[[:digit:]]{1,4}",FailDataFrame[i,"ROAD_NAME"])
#     }
#     match_last  <- match_start + attr(match_start,"match.length") - 1    
#     FailDataFrame[i,"ROUTE_NO"] <- substr(FailDataFrame[i,"ROAD_NAME"],rte_start,match_last)
#     RteStrings[[i]] <- gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",substr(FailDataFrame[i,"ROAD_NAME"],match_start,rte_start-1)),perl=TRUE)
#     RemainStrings[[i]] <- substr(FailDataFrame[i,"ROAD_NAME"],match_last+1,nchar(FailDataFrame[i,"ROAD_NAME"]))
#   }
#   
#   if (length(RowsWithRteNo)>=1){
#     FailDataFrame[RowsWithRteNo,"ROAD_NAME"]   <- sapply(RowsWithRteNo, function(i) RemainStrings[[i]])
#     FailDataFrame[RowsWithRteNo,"ROAD_TYPE"]  <- sapply(RowsWithRteNo, function(i) RteStrings[[i]])
#     # first find cardinals
#     HasCardinalBool <- sapply(paste("\\<",CardinalKeys,"\\>",sep=""),grepl,FailDataFrame[RowsWithRteNo,"ROAD_TYPE"])
#     if (length(RowsWithRteNo)==1) HasCardinalBool <- t(HasCardinalBool)
#     RowsWithCardinalIndex <- which(rowSums(HasCardinalBool) != 0)
#     RowsWithCardinal <- RowsWithRteNo[RowsWithCardinalIndex]
#     nRowsWithMatch <- length(RowsWithCardinal)
#     if (nRowsWithMatch >= 1){
#       MatchedDirectionIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasCardinalBool[RowsWithCardinalIndex[i],])))     
#       FailDataFrame[RowsWithCardinal,"RTE_DIRECTION"] <- sapply(1:nRowsWithMatch, function(i) ls.CardinalKeys[[CardinalKeyIndex[MatchedDirectionIndex[i]]]][1])
#       FailDataFrame[RowsWithCardinal,"ROAD_TYPE"]     <- sapply(1:nRowsWithMatch, function(i) gsub(paste("\\<",CardinalKeys[MatchedDirectionIndex[i]],"\\>",sep=""),"", FailDataFrame[RowsWithCardinal[i],"ROAD_TYPE"]))
#     }
#     # then find road prefix types
#     HasRteTypeBool <- sapply(paste("\\<",RteKeys,"\\>",sep=""), grepl, FailDataFrame[RowsWithRteNo,"ROAD_TYPE"])
#     if (length(RowsWithRteNo)==1) HasRteTypeBool <- t(HasRteTypeBool) 
#     RowsWithRteTypeIndex   <- which(rowSums(HasRteTypeBool) != 0)
#     RowsWithRteTypeMatch   <- RowsWithRteNo[RowsWithRteTypeIndex]
#     nRowsWithMatch         <- length(RowsWithRteTypeMatch)
#     if (nRowsWithMatch >= 1){
#       MatchedRteTypeIndex  <- sapply(1:length(RowsWithRteTypeMatch), function(i) max(which(HasRteTypeBool[RowsWithRteTypeIndex[i],])))     
#       FailDataFrame[RowsWithRteTypeMatch,"ROAD_TYPE"]   <- sapply(1:length(RowsWithRteTypeMatch), function(i) ls.RteKeysState[[RteKeyIndex[MatchedRteTypeIndex[i]]]][1])
#     }  
#   }
#   # check for secondary route numbers (i.e., if mentions two roads)
#   HasRteTypeBool <- sapply(paste("\\<",RteKeys,"[[:punct:]]?[[:space:]]?[[:digit:]]{1,4}\\>",sep=""), grepl, FailDataFrame[rowsForState,"ROAD_NAME"])
#   if (length(rowsForState)==1) HasRteTypeBool <- t(HasRteTypeBool) 
#   RowsWithRteTypeIndex   <- which(rowSums(HasRteTypeBool) != 0)
#   RowsWithRteTypeMatch   <- rowsForState[RowsWithRteTypeIndex]
#   nRowsWithMatch         <- length(RowsWithRteTypeMatch)
#   if (nRowsWithMatch >= 1){
#     for (i in RowsWithRteTypeMatch){
#       rte_start   <- regexpr("[[:digit:]]{1,4}",FailDataFrame[i,"ROAD_NAME"])
#       rte_last    <- rte_start + attr(rte_start, "match.length") - 1
#       FailDataFrame[i,"RTE_SECOND"]  <- substr(FailDataFrame[i,"ROAD_NAME"],rte_start,rte_last)
#     }
#     FailDataFrame[RowsWithRteTypeMatch,"ROAD_NAME"] <- sapply(1:nRowsWithMatch, function(i) gsub(FailDataFrame[RowsWithRteTypeMatch[i],"RTE_SECOND"],"",FailDataFrame[RowsWithRteTypeMatch[i],"ROAD_NAME"]))
#     MatchedRteTypeIndex  <- sapply(1:length(RowsWithRteTypeMatch), function(i) max(which(HasRteTypeBool[RowsWithRteTypeIndex[i],])))
#     if (class(MatchedRteTypeIndex) == "list") MatchedRteTypeIndex <- unlist(MatchedRteTypeIndex)
#     FailDataFrame[RowsWithRteTypeMatch,"RTE_SECOND"]   <- sapply(1:nRowsWithMatch, function(i) paste(ls.RteKeysState[[RteKeyIndex[MatchedRteTypeIndex[i]]]][1],FailDataFrame[RowsWithRteTypeMatch[i],"RTE_SECOND"],sep=" "))
#     FailDataFrame[RowsWithRteTypeMatch,"ROAD_NAME"] <- sapply(1:nRowsWithMatch, function(i) gsub(paste("\\<",RteKeys[MatchedRteTypeIndex[i]],"\\>",sep=""),"",FailDataFrame[RowsWithRteTypeMatch[i],"ROAD_NAME"]))
#   }  
#   # Any remaining with a number should be the route number, even if they don't have a route type
#   RowsWithNo <- rowsForState[grep("[[:digit:]]{1,4}\\>",FailDataFrame[rowsForState,"ROAD_NAME"])]
#   for (i in RowsWithNo){
#     rte_start   <- regexpr("[[:digit:]]{1,4}",FailDataFrame[i,"ROAD_NAME"])
#     rte_last    <- rte_start + attr(rte_start, "match.length") - 1
#     if (FailDataFrame[i,"ROUTE_NO"]==""){
#       FailDataFrame[i,"ROUTE_NO"]  <- substr(FailDataFrame[i,"ROAD_NAME"],rte_start,rte_last)
#       FailDataFrame[i,"ROAD_NAME"] <- gsub(FailDataFrame[i,"ROUTE_NO"],"",FailDataFrame[i,"ROAD_NAME"])
#     }
#     else {
#       FailDataFrame[i,"RTE_SECOND"]  <- substr(FailDataFrame[i,"ROAD_NAME"],rte_start,rte_last)
#       FailDataFrame[i,"ROAD_NAME"] <- gsub(FailDataFrame[i,"RTE_SECOND"],"",FailDataFrame[i,"ROAD_NAME"])
#     }
#   }
#   # find alphabet route numbers (i.e., supplemental or secondary routes in Missouri)
#   HasRteTypeBool <- sapply(paste("\\<",RteKeys,"\\>[[:punct:]]?[[:space:]]?\\<[[:alpha:]]{1,2}\\>",sep=""), grepl, FailDataFrame[rowsForState,"ROAD_NAME"])
#   if (length(rowsForState)==1) HasRteTypeBool <- t(HasRteTypeBool) 
#   RowsWithRteTypeIndex   <- which(rowSums(HasRteTypeBool) != 0)
#   RowsWithRteTypeMatch   <- rowsForState[RowsWithRteTypeIndex]
#   nRowsWithMatch         <- length(RowsWithRteTypeMatch)
#   if (nRowsWithMatch >= 1){
#     MatchedRteTypeIndex  <- sapply(1:length(RowsWithRteTypeMatch), function(i) max(which(HasRteTypeBool[RowsWithRteTypeIndex[i],])))
#     if (class(MatchedRteTypeIndex) == "list") MatchedRteTypeIndex <- unlist(MatchedRteTypeIndex)
#     for (i in 1:nRowsWithMatch){
#       RteType <- RteKeys[MatchedRteTypeIndex[i]]
#       rte_start   <- regexpr(paste("\\<",RteType,"\\>[[:punct:]]?[[:space:]]?\\<[[:alpha:]]{1,2}\\>",sep=""),FailDataFrame[RowsWithRteTypeMatch[i],"ROAD_NAME"])
#       rte_last    <- rte_start + attr(rte_start, "match.length") - 1
#       rte_temp    <- substr(FailDataFrame[RowsWithRteTypeMatch[i],"ROAD_NAME"], rte_start, rte_last)
#       FailDataFrame[RowsWithRteTypeMatch[i],"ROUTE_NO"]    <- gsub("[[:space:]]","",gsub("[[:punct:]]","",substr(rte_temp,nchar(RteType)+1,nchar(rte_temp))))
#       FailDataFrame[RowsWithRteTypeMatch[i], "ROAD_TYPE"] <- ls.RteKeysState[[RteKeyIndex[MatchedRteTypeIndex[i]]]][1]
#       FailDataFrame[RowsWithRteTypeMatch[i], "ROAD_NAME"]  <- paste(substr(FailDataFrame[RowsWithRteTypeMatch[i], "ROAD_NAME"],1,rte_start-1),substr(FailDataFrame[RowsWithRteTypeMatch[i], "ROAD_NAME"],rte_last+1,nchar(FailDataFrame[RowsWithRteTypeMatch[i], "ROAD_NAME"])), sep=", ")
#     }
#   }  
#  
#   # ROUTE DIRECTION - GRAB FROM ROUTE NO, THEN FROM OTHER PLACES 
#   HasDirectionBool <- sapply(paste("[[:digit:]]",CardinalKeys,sep=""), grepl, FailDataFrame[rowsForState,"ROUTE_NO"])
#   if (length(rowsForState)==1) HasDirectionBool <- t(as.vector(unlist(HasDirectionBool))) 
#   RowsWithDirectionRowsWithMatchIndex <- which(rowSums(HasDirectionBool) != 0)
#   RowsWithDirectionRowsWithMatch <- rowsForState[RowsWithDirectionRowsWithMatchIndex]
#   nRowsWithMatch <- length(RowsWithDirectionRowsWithMatch)
#   if (length(RowsWithDirectionRowsWithMatch)>=1){
#     MatchedDirectionIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasDirectionBool[RowsWithDirectionRowsWithMatchIndex[i],])))     
#     FailDataFrame[RowsWithDirectionRowsWithMatch,"RTE_DIRECTION"]   <- sapply(1:nRowsWithMatch, function(i) ls.CardinalKeys[[CardinalKeyIndex[MatchedDirectionIndex[i]]]][1])
#     FailDataFrame[RowsWithDirectionRowsWithMatch,"ROUTE_NO"]   <- sapply(1:nRowsWithMatch, function(i) gsub(paste(CardinalKeys[MatchedDirectionIndex[i]],"\\>",sep=""),"", FailDataFrame[RowsWithDirectionRowsWithMatch[i],"ROUTE_NO"]))
#   }
#   # check for nb/wb/eb/sb in ROAD_NAME
#   HasDirectionBool <- sapply(paste("\\<",CardBoundKeys,"\\>",sep=""), grepl, FailDataFrame[rowsForState,"ROAD_NAME"])
#   if (length(rowsForState)==1) HasDirectionBool <- t(as.vector(unlist(HasDirectionBool))) 
#   RowsWithDirectionRowsWithMatchIndex <- which(rowSums(HasDirectionBool) != 0)
#   RowsWithDirectionRowsWithMatch <- rowsForState[RowsWithDirectionRowsWithMatchIndex]
#   nRowsWithMatch <- length(RowsWithDirectionRowsWithMatch)
#   if (length(RowsWithDirectionRowsWithMatch)>=1){
#     MatchedDirectionIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasDirectionBool[RowsWithDirectionRowsWithMatchIndex[i],])))     
#     FailDataFrame[RowsWithDirectionRowsWithMatch,"RTE_DIRECTION"]   <- sapply(1:nRowsWithMatch, function(i) ls.CardinalKeys[[CardBoundKeyIndex[MatchedDirectionIndex[i]]]][1])
#     FailDataFrame[RowsWithDirectionRowsWithMatch,"ROAD_NAME"]   <- sapply(1:nRowsWithMatch, function(i) gsub(paste("\\<",CardBoundKeys[MatchedDirectionIndex[i]],"\\>",sep=""),"", FailDataFrame[RowsWithDirectionRowsWithMatch[i],"ROAD_NAME"]))
#   }
#   
#   # PERFORM COUNTY MATCHING - STRICT (i.e., only deletes if has name followed by "county" or "co")
#   if (nrow(StateCounties)==0) {
#     warning(paste("No counties in state #",as.character(j),"-",as.character(df.States[df.States==j,"STATE_CODE"]),sep = " "))
#     return(FailDataFrame)
#   }
#   HasCountyStringBool <- sapply(paste("\\<",StateCounties$COUNTY_NAME,"\\>",sep=""),grepl,FailDataFrame[rowsForState,"ROAD_NAME"])
#   if (nRowsState==1) HasCountyStringBool <- t(HasCountyStringBool)
#   RowsWithCountyIndex <- which(rowSums(HasCountyStringBool) != 0)
#   RowsWithCountyMatch <- rowsForState[RowsWithCountyIndex]
#   nRowsWithMatch <- length(RowsWithCountyMatch)
#   if (nRowsWithMatch > 0){
#     MatchedCountyIndex  <- lapply(1:nRowsWithMatch, function(i) which(HasCountyStringBool[RowsWithCountyIndex[i],]))
#     nRowsWithMatchesRow <- sapply(1:nRowsWithMatch, function(i) length(MatchedCountyIndex[[i]]))
#     for (i in 1:nRowsWithMatch){
#       for (k in 1:nRowsWithMatchesRow[i]){
#         FailDataFrame[RowsWithCountyMatch[i],LocProcessColsOut]     <- GetAndRemoveLocationData(FailDataFrame[RowsWithCountyMatch[i], LocProcessColsIn], StateCounties[MatchedCountyIndex[[i]][k],"COUNTY_NAME"], LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, FALSE, TRUE)
#         FailDataFrame[RowsWithCountyMatch[i],CountyOutCols[[1]][k]] <- StateCounties[MatchedCountyIndex[[i]][k],"COUNTY_NAME"]
#         FailDataFrame[RowsWithCountyMatch[i],CountyOutCols[[2]][k]] <- StateCounties[MatchedCountyIndex[[i]][k],"FIPS"]
#         #print(paste("Matched county", StateCounties[MatchedCountyIndex[[i]][k],"COUNTY_NAME"],"to entry", FailDataFrame[rowsForState[i],"LOCATION"],sep=" "))
#       }
#     }
#   }
#   print("    Finished county identification")
#   
#   # PERFORM CITY MATCHING - NOT STRICT (still checks to see if part of road or stream name)
#   # named cities, townships, villages (e.g., "City of Baltimore" or "Luellen Township)
#   HasCityTypeBool <- sapply(paste("\\<",CityKeys,"\\>",sep=""), grepl, FailDataFrame[rowsForState,"ROAD_NAME"])
#   if (nRowsState==1) HasCityTypeBool <- t(HasCityTypeBool)
#   RowsWithCityMatchIndex <- which(rowSums(HasCityTypeBool) != 0)
#   RowsWithCityMatch      <- rowsForState[RowsWithCityMatchIndex]
#   nRowsWithMatch <- length(RowsWithCityMatch)
#   if (nRowsWithMatch > 0){
#     MatchedCityIndex  <- sapply(1:nRowsWithMatch, function(i) max(which(HasCityTypeBool[RowsWithCityMatchIndex[i],])))
#     for (i in 1:nRowsWithMatch){
#       MatchRegex <- paste("\\<",CityKeys[MatchedCityIndex[i]],"\\>",sep="")
#       MatchRegexOf <- paste(MatchRegex,"[[:space:]]{1,2}\\<of\\>",sep="")
#       HasOfBool <- grepl(MatchRegexOf,FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"])
#       if (HasOfBool){
#         match_start <- regexpr(MatchRegexOf, FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"])
#         temp_entry <- substr(FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"],match_start,nchar(FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"]))
#         temp_entry <- unlist(strsplit(unlist(strsplit(temp_entry,"[[:punct:]]")), "[[:digit:]]"))[1] 
#         entry <- str_trim(gsub(MatchRegexOf,"",temp_entry))
#         to_delete <- MatchRegexOf
#       }
#       else{
#         match_start <- regexpr(MatchRegex, FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"])
#         match_last  <- match_start + attr(match_start,"match.length") - 1
#         temp_entry <- substr(FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"],1,match_last)
#         substrings <- unlist(strsplit(temp_entry,"[[:punct:]]"))
#         temp_entry <- substrings[length(substrings)]
#         entry <- str_trim(gsub(MatchRegex,"",temp_entry))
#         to_delete <- MatchRegex
#       }
#       HasKnownCityBool <- unlist(sapply(StateCities$CITY_NAME, grepl, temp_entry))
#       if (all(!HasKnownCityBool)){ # means an unknown township, so record now (otherwise will catch later)
#         FailDataFrame[RowsWithCityMatch[i],"ROAD_NEAR"] <- paste(FailDataFrame[RowsWithCityMatch[i],"ROAD_NEAR"], entry, ls.JurisdictionKeys[[CityKeyIndex[MatchedCityIndex[i]]]][1], "-", sep=" ")
#         FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"] <- gsub(temp_entry,"",FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"])
#       }
#       else{
#         FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"] <- gsub(to_delete,"",FailDataFrame[RowsWithCityMatch[i],"ROAD_NAME"])
#       }
#     }  
#   }
#   # not-explicitly-named cities
#   if (nrow(StateCities)==0) {
#     warning(paste("No cities in state #",as.character(j),"-",as.character(df.States[df.States==j,"STATE_CODE"]),sep = " "))
#     return(FailDataFrame)
#   }
#   HasCityStringBool <- sapply(StateCities$CITY_NAME,grepl,FailDataFrame[rowsForState,"ROAD_NAME"]) 
#   if (nRowsState==1) HasCityStringBool <- t(HasCityStringBool)
#   RowsWithCityMatch <- which(rowSums(HasCityStringBool) != 0)
#   nRowsWithMatch <- length(RowsWithCityMatch)
#   if (nRowsWithMatch > 0){
#     MatchedCityIndex            <- lapply(RowsWithCityMatch, function(i) which(HasCityStringBool[i,]))
#     nRowsWithMatchesRow                    <- integer(nRowsState)
#     nRowsWithMatchesRow[RowsWithCityMatch] <- sapply(1:nRowsWithMatch, function(i) length(MatchedCityIndex[[i]]))
#     for (i in 1:nRowsWithMatch){
#       for (k in 1:nRowsWithMatchesRow[RowsWithCityMatch[i]]){
#         FailDataFrame[rowsForState[RowsWithCityMatch[i]],LocProcessColsOut] <- GetAndRemoveLocationData(FailDataFrame[rowsForState[RowsWithCityMatch[i]], LocProcessColsIn], StateCities[MatchedCityIndex[[i]][k],"CITY_NAME"], LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, TRUE, FALSE)
#         FailDataFrame[rowsForState[RowsWithCityMatch[i]],CityOutCols[[1]][k]] <- StateCities[MatchedCityIndex[[i]][k],"CITY_NAME"]
#         FailDataFrame[rowsForState[RowsWithCityMatch[i]],CityOutCols[[2]][k]] <- StateCities[MatchedCityIndex[[i]][k],"FIPS"]
#         FailDataFrame[rowsForState[RowsWithCityMatch[i]],CityOutCols[[3]][k]] <- StateCities[MatchedCityIndex[[i]][k],"ANSICODE"]
#         FailDataFrame[rowsForState[RowsWithCityMatch[i]],CityOutCols[[4]][k]] <- StateCities[MatchedCityIndex[[i]][k],"GNIS_ID"] 
#         #print(paste("Matched city", StateCities[MatchedCityIndex[[i]][k],"CITY_NAME"],"to entry", FailDataFrame[rowsForState[i],"LOCATION"],sep=" "))
#       } 
#     }  
#   }
#   print("    Finished city identification")
#   
#   # PROCESS STATE NAMES
#   State <- tolower(df.States[df.States$STFIPS==j,"STATE_FULL"])
#   RowsWithState <- rowsForState[grep(State, FailDataFrame[rowsForState,"ROAD_NAME"])]
#   nRowsWithMatch <- length(RowsWithState)
#   if (nRowsWithMatch > 0){
#     for (i in RowsWithState){
#       FailDataFrame[i,LocProcessColsOut] <- GetAndRemoveLocationData(FailDataFrame[i, LocProcessColsIn], State, LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, TRUE, FALSE)
#     }
#   }
#   # abbreviated version
#   State <- tolower(df.States[df.States$STFIPS==j,"STATE_CODE"])
#   RowsWithState <- rowsForState[grep(paste("\\<",State,"\\>",sep=""), FailDataFrame[rowsForState,"ROAD_NAME"])]
#   nRowsWithMatch <- length(RowsWithState)
#   if (nRowsWithMatch > 0){
#     for (i in RowsWithState){
#       FailDataFrame[i,LocProcessColsOut] <- GetAndRemoveLocationData(FailDataFrame[i, LocProcessColsIn], State, LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, TRUE, FALSE)
#     }
#   }
#   # assume that other mentioned states are "near" statements
#   HasStateBool      <- sapply(tolower(df.States$STATE_FULL), grepl, FailDataFrame[rowsForState,"ROAD_NAME"])
#   if (nRowsState==1) HasStateBool <- t(HasStateBool)
#   RowsWithStateIndex  <- which(rowSums(HasStateBool) != 0)
#   RowsWithStateMatch <- rowsForState[RowsWithStateIndex]
#   nRowsWithMatch <- length(RowsWithStateMatch)
#   if (nRowsWithMatch > 0){
#     MatchedStateIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasStateBool[RowsWithStateIndex[i],])))
#     if (class(MatchedStateIndex)=="list") MatchedStateIndex <- unlist(MatchedStateIndex)
#     StateMatch <- sapply(1:nRowsWithMatch, function(i) tolower(df.States$STATE_FULL[MatchedStateIndex[i]]))
#     for (i in 1:nRowsWithMatch){
#       FailDataFrame[RowsWithStateMatch[i],LocProcessColsOut] <- GetAndRemoveLocationData(FailDataFrame[RowsWithStateMatch[i], LocProcessColsIn], StateMatch[i], LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, TRUE, FALSE)
#     }
#   }
#   
#   # RE-PROCESS COUNTIES, NON-STRICT
#   HasCountyStringBool <- sapply(StateCounties$COUNTY_NAME,grepl,FailDataFrame[rowsForState,"ROAD_NAME"])
#   if (nRowsState==1) HasCountyStringBool <- t(HasCountyStringBool)
#   RowsWithCountyIndex <- which(rowSums(HasCountyStringBool) != 0)
#   RowsWithCountyMatch <- rowsForState[RowsWithCountyIndex]
#   nRowsWithMatch <- length(RowsWithCountyMatch)
#   if (nRowsWithMatch > 0){
#     # assume only one county name remaining after city matching
#     MatchedCountyIndex  <- sapply(1:nRowsWithMatch, function(i) min(which(HasCountyStringBool[RowsWithCountyIndex[i],])))
#     for (i in 1:nRowsWithMatch){
#       FailDataFrame[RowsWithCountyMatch[i],LocProcessColsOut]     <- GetAndRemoveLocationData(FailDataFrame[RowsWithCountyMatch[i], LocProcessColsIn], StateCounties[MatchedCountyIndex[[i]][k],"COUNTY_NAME"], LocProcessColsOut, ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys, FALSE, FALSE)
#       #print(paste("Matched county", StateCounties[MatchedCountyIndex[[i]][k],"COUNTY_NAME"],"to entry", FailDataFrame[rowsForState[i],"LOCATION"],sep=" "))
#     }
#   }
#   
#   # RIVER DATA - MIDDLE FORK, ETC
#   HasTribStringBool <- sapply(paste("\\<",TribRelatKeys,"\\>",sep=""), grepl, gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",FailDataFrame[rowsForState, "ROAD_NAME"]), perl=TRUE))
#   if (nRowsState==1) HasTribStringBool <- t(HasTribStringBool)
#   RowsWithTribIndex <- which(rowSums(HasTribStringBool) != 0)
#   RowsWithTribMatch <- rowsForState[RowsWithTribIndex]
#   nRowsWithMatch <- length(RowsWithTribMatch)
#   if (nRowsWithMatch > 0){
#     MatchedTribIndex    <- sapply(1:nRowsWithMatch, function(i) min(which(HasTribStringBool[RowsWithTribIndex[i],])))
#     # nRowsWithMatchesRow <- sapply(1:nRowsWithMatch, function(i) length(MatchedTribIndex[[i]]))
#     for (i in 1:nRowsWithMatch){
#       temp_entry  <- gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]","",FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"]), perl=TRUE)
#       match_start <- regexpr(paste("\\<",TribRelatKeys[MatchedTribIndex[i]],"\\>",sep=""),temp_entry)
#       match_last  <- match_start + attr(match_start,"match.length") - 1
#       HasRoadBool   <- sapply(paste("\\<",RoadKeys,"\\>",sep=""), grepl, substr(temp_entry,match_last+1,match_last+5))
#       if (all(!HasRoadBool)){
#         FailDataFrame[RowsWithTribMatch[i],"STREAM_TRIB"] <- paste(FailDataFrame[RowsWithTribMatch[i],"STREAM_TRIB"], TribRelatKeys[MatchedTribIndex[i]], "-", sep = " ")
#         FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"] <- gsub(paste("\\<",TribRelatKeys[MatchedTribIndex[i]],"\\>",sep=""),"",gsub("^ *|(?<= ) | *$", "",gsub("[[:punct:]]"," ",FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"]),perl=TRUE))
#       }
#     }
#   }
#   
#   # STREAM NAMES AS CAN BE INFERRED, REMOVE IF NO ROAD WORD
#   HasStreamStringBool <- sapply(paste("[[:print:]]{3,}\\<",StreamKeys,"\\>",sep=""), grepl, FailDataFrame[rowsForState,"LOCATION"])  
#   if (nRowsState==1) HasStreamStringBool <- t(HasStreamStringBool)
#   RowsWithStreamMatch <- which(rowSums(HasStreamStringBool) != 0)
#   nRowsWithMatch <- length(RowsWithStreamMatch)
#   if (nRowsWithMatch > 0){
#     MatchedStreamIndex             <- lapply(RowsWithStreamMatch, function(i) which(HasStreamStringBool[i,]))
#     nRowsWithMatchesRow                    <- integer(nRowsState)
#     nRowsWithMatchesRow[RowsWithStreamMatch] <- sapply(1:nRowsWithMatch, function(i) length(MatchedStreamIndex[[i]]))
#     for (i in 1:nRowsWithMatch){
#       for (k in 1:nRowsWithMatchesRow[RowsWithStreamMatch[i]]){
#         FailDataFrame[rowsForState[RowsWithStreamMatch[i]],LocProcessColsOut] <- GetAndRemoveLocationForStream(FailDataFrame[rowsForState[RowsWithStreamMatch[i]],LocProcessColsIn], StreamKeys[MatchedStreamIndex[[i]][k]], ls.StreamKeys[[StreamKeyIndex[MatchedStreamIndex[[i]][k]]]][1], LocProcessColsOut,ls.JurisdictionKeys, ls.RoadKeys, ls.StreamKeys)
#       }
#     }
#   }
#   print("    Finished stream (in location field) identification")
#   
#   #  REST OF PARENTHETICALS
#   MatchRegex <- "\\([[:print:]]{1,15}\\)"
#   RowsWithParentheticalMatchIndex <- grep(MatchRegex, FailDataFrame[rowsForState,"ROAD_NAME"])
#   RowsWithParentheticalMatch <- rowsForState[RowsWithParentheticalMatchIndex]
#   nRowsWithMatch <- length(RowsWithParentheticalMatch)
#   if (nRowsWithMatch >= 1){ 
#     for (i in 1:nRowsWithMatch){
#       match_start <- regexpr(MatchRegex,FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"])
#       match_last  <- match_start + attr(match_start,"match.length") - 1
#       FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NEAR"] <- paste(FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NEAR"],substr(FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"],match_start+1,match_last-1),"-",sep=" ")
#       FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"] <- gsub(MatchRegex,"",FailDataFrame[RowsWithParentheticalMatch[i],"ROAD_NAME"])
#     }
#   }
#   
#   # FINAL TRIBS CLEANUP
#   HasTribStringBool <- sapply(paste("\\<",unlist(ls.TribsKeys),"\\>",sep=""), grepl, gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",FailDataFrame[rowsForState, "ROAD_NAME"]), perl=TRUE))
#   if (nRowsState==1) HasTribStringBool <- t(HasTribStringBool)
#   RowsWithTribIndex <- which(rowSums(HasTribStringBool) != 0)
#   RowsWithTribMatch <- rowsForState[RowsWithTribIndex]
#   nRowsWithMatch <- length(RowsWithTribMatch)
#   if (nRowsWithMatch > 0){
#     MatchedTribIndex    <- sapply(1:nRowsWithMatch, function(i) min(which(HasTribStringBool[RowsWithTribIndex[i],])))
#     for (i in 1:nRowsWithMatch){
#       temp_entry  <- gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]","",FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"]), perl=TRUE)
#       match_start <- regexpr(paste("\\<",unlist(ls.TribsKeys)[MatchedTribIndex[i]],"\\>",sep=""),temp_entry)
#       match_last  <- match_start + attr(match_start,"match.length") - 1
#       HasRoadBool   <- sapply(paste("\\<",RoadKeys,"\\>",sep=""), grepl, substr(temp_entry,match_last+1,match_last+5))
#       if (all(!HasRoadBool)){
#         FailDataFrame[RowsWithTribMatch[i],"STREAM_TRIB"] <- paste(FailDataFrame[RowsWithTribMatch[i],"STREAM_TRIB"], unlist(ls.TribsKeys)[MatchedTribIndex[i]], "-", sep = " ")
#         FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"] <- gsub(paste("\\<",unlist(ls.TribsKeys)[MatchedTribIndex[i]],"\\>",sep=""),"",gsub("^ *|(?<= ) | *$", "",gsub("[[:punct:]]"," ",FailDataFrame[RowsWithTribMatch[i],"ROAD_NAME"]),perl=TRUE))
#       }
#     }
#   }
#   
#   # RAILWAYS
#   HasRailStringBool <- sapply(paste("\\<",RailKeys,"\\>",sep=""), grepl, gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",FailDataFrame[rowsForState, "ROAD_NAME"]), perl=TRUE))
#   if (nRowsState==1) HasRailStringBool <- t(HasRailStringBool)
#   RowsWithRailIndex <- which(rowSums(HasRailStringBool) != 0)
#   RowsWithRailMatch <- rowsForState[RowsWithRailIndex]
#   nRowsWithMatch <- length(RowsWithRailMatch)
#   if (nRowsWithMatch > 0){
#     MatchedRailIndex    <- sapply(1:nRowsWithMatch, function(i) min(which(HasRailStringBool[RowsWithRailIndex[i],])))
#     for (i in 1:nRowsWithMatch){
#       temp_entry  <- gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]","",FailDataFrame[RowsWithRailMatch[i],"LOCATION"]), perl=TRUE)
#       match_start <- regexpr(paste("\\<",RailKeys[MatchedRailIndex[i]],"\\>",sep=""),temp_entry)
#       match_last  <- match_start + attr(match_start,"match.length") - 1
#       HasRoadBool   <- sapply(paste("\\<",RoadKeys,"\\>",sep=""), grepl, substr(temp_entry,match_last+1,match_last+5))
#       if (all(!HasRoadBool)){
#         FailDataFrame[RowsWithRailMatch[i],"ROAD_NAME"] <- gsub(paste("\\<",RailKeys[MatchedRailIndex[i]],"\\>",sep=""),"",gsub("^ *|(?<= ) | *$", "",gsub("[[:punct:]]"," ",FailDataFrame[RowsWithRailMatch[i],"ROAD_NAME"]),perl=TRUE))
#         FailDataFrame[RowsWithRailMatch[i],"RAIL_NAME"] <- substr(temp_entry, 1, match_start - 1)
#         if (nchar(FailDataFrame[RowsWithRailMatch[i],"RAIL_NAME"]) > 1){
#           RailName <- unlist(strsplit(FailDataFrame[RowsWithRailMatch[i],"RAIL_NAME"]," ")) 
#           for (j in 1:length(RailName)){
#             FailDataFrame[RowsWithRailMatch[i],"ROAD_NAME"] <- sub(paste("\\<",RailName[j],"\\>", sep=""),"", FailDataFrame[RowsWithRailMatch[i],"ROAD_NAME"])
#           }
#         }
#         else{
#           FailDataFrame[RowsWithRailMatch[i],"RAIL_NAME"] <- "railway"
#         }
#       }
#     }
#   }
#   
#   # PROCESS ROAD NAMES
#   FailDataFrame[rowsForState, "ROAD_NAME"] <- str_trim(gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",FailDataFrame[rowsForState, "ROAD_NAME"]), perl = TRUE))
#   NonEmptyRows <- rowsForState[FailDataFrame[rowsForState, "ROAD_NAME"]!=""]
#   if (length(NonEmptyRows) >= 1){
#     HasRoadTypeBool <- sapply(paste("\\<",RoadKeys,"\\>",sep=""), grepl, FailDataFrame[NonEmptyRows,"ROAD_NAME"])
#     if (length(NonEmptyRows)==1) HasRoadTypeBool <- t(HasRoadTypeBool) 
#     RowsWithRoadTypeIndex   <- which(rowSums(HasRoadTypeBool) != 0)
#     RowsWithRoadTypeMatch   <- NonEmptyRows[RowsWithRoadTypeIndex]
#     nRowsWithMatch         <- length(RowsWithRoadTypeMatch)
#     RowsWithNoRoadTypeIndex   <- which(rowSums(HasRoadTypeBool) != 1)
#     RowsWithNoRoadTypeMatch   <- NonEmptyRows[RowsWithNoRoadTypeIndex]
#     nRowsWithNoMatch         <- length(RowsWithNoRoadTypeMatch)
#     if (nRowsWithMatch >= 1){
#       MatchedRoadTypeIndex  <- sapply(1:length(RowsWithRoadTypeMatch), function(i) max(which(HasRoadTypeBool[RowsWithRoadTypeIndex[i],])))     
#       FailDataFrame[RowsWithRoadTypeMatch,"ROAD_TYPE"]   <- sapply(1:length(RowsWithRoadTypeMatch), function(i) ls.RoadKeys[[RoadKeyIndex[MatchedRoadTypeIndex[i]]]][1])
#       for (i in 1:nRowsWithMatch){
#         temp_entry  <- FailDataFrame[RowsWithRoadTypeMatch[i],"ROAD_NAME"]
#         match_start <- regexpr(paste("\\<",RoadKeys[MatchedRoadTypeIndex[i]],"\\>", sep = ""), temp_entry)
#         match_last  <- match_start + attr(match_start,"match.length") - 1
#         temp_length <- nchar(temp_entry)
#         if ((match_start-2) > 1) FailDataFrame[RowsWithRoadTypeMatch[i],"ROAD_NAME"]   <- substr(temp_entry, 1, match_start - 2)
#         if ((match_last+2) < temp_length) FailDataFrame[RowsWithRoadTypeMatch[i],"ROAD_NEAR"]   <- paste(FailDataFrame[RowsWithRoadTypeMatch[i],"ROAD_NEAR"] , substr(temp_entry, match_last+2, temp_length), sep=" ")
#       }
#     } 
#     if (nRowsWithNoMatch >= 1){     
#       FailDataFrame[RowsWithNoRoadTypeMatch,"ROAD_NEAR"]   <- sapply(RowsWithNoRoadTypeMatch, function(i) paste(FailDataFrame[i,"ROAD_NEAR"], FailDataFrame[i,"ROAD_NAME"],sep=" "))
#       FailDataFrame[RowsWithNoRoadTypeMatch, "ROAD_NAME"] <- ""
#     }
#   }
#   FailDataFrame[rowsForState,"ROAD_NEAR"] <- str_trim(FailDataFrame[rowsForState,"ROAD_NEAR"])
# }
# FailDataFrame <- merge(FailDataFrame,df.RoadType, all.x = TRUE, all.y = FALSE, sort = FALSE)
# FailDataFrame <- FailDataFrame[order(FailDataFrame$STATE_CODE,FailDataFrame$ID),]
# save(FailDataFrame, file="FailDataFrameProcessedCountyCityRoadRte.RData")
# print("Finished all county and city identification")


# if I'm not getting sufficient matches from ITEM5D and ITEM7, try these:
#  ITEM4 census place code, probably ANSI_CODE
#  ITEM5D ROUTE NUMBER - ACTUAL ROUTE NUMBER - as "00088" or whatever, so will have to drop leading zeros and then check
#  ITEM5E DIRECTIONAL SUFFIX: 0 NA, 1 NORTH, 2 EAST, 3 SOUTH, 4 WEST
#  ITEM9 location - NARRATIVE, PLAIN TEXT, DISTANCE, MI/KM, OF CITY/JUNCTION/... (uses mile as mile, mi, m)
#  ITEM5C DESIGNATED LEVEL OF SERVICE:
#    0 NONE OF THE BELOW, 1 MAINLINE, 2 ALTERNATE, 3 BYPASS, 4 SPUR, 6 BUSINESS, 7 RAMP, CONNECTOR, ..., 8 SERVICE OR FRONTAGE ROAD

# # STEP 4: PROCESS "FEAT_UND" FIELD: STREAM NAME, TYPE ------------------------------------------------------------------------------------
# # For FailDataFrame
# NoStreamDataIndex  <- which(FailDataFrame$FEAT_UND == "")
# HasStreamDataIndex <- subset(FailRowNames, !(FailRowNames %in% NoStreamDataIndex) )

# for entries with a waterway, separate all non-name data (e.g., river, creek, left fork, ...)
# FailDataFrame$STREAM_NAME  <- FailDataFrame$FEAT_UND
# FailDataFrame$STREAM_TYPE  <- character(nFail)
# FailDataFrame$STREAM_OTHER <- character(nFail)
nbiDataFrame$STREAM_TYPE   <- character(nNBI)
nbiDataFrame$STREAM_OTHER <- character(nNBI)
nbiDataFrame$STREAM_NAME <- nbiDataFrame$ITEM6A

# only use relational keys through 14 (don't want to remove "mile" from "ten mile creek")
print("searching and replacing relational keys")
LookForRelationals <- function(RelationalKeys){
    print(ls.RelationalKeys[1])
    Streams <- nbiDataFrame$STREAM_NAME
    Others  <- nbiDataFrame$STREAM_OTHER
    key_index <- sapply(paste("\\<",RelationalKeys,"\\>", sep = ""),grepl,Streams)
    match_keys <- which(apply(key_index, MARGIN = 1, any))
    Streams[match_keys] <- sapply(match_keys,function (i) gsub(paste("\\<",RelationalKeys[which(key_index[i,])[1]],"\\>", sep = ""),"",Streams[i]))
    Others[match_keys]  <- sapply(match_keys,function (i) paste(Others[i], RelationalKeys[1], sep = " "))
    return(data.frame(STREAM_NAME=Streams,STREAM_OTHER=Others))
}
nbiDataFrame[,c("STREAM_NAME","STREAM_OTHER")] <- sapply(ls.RelationalKeys[1:2], LookForRelationals, nbiDataFrame[,c("STREAM_NAME","STREAM_OTHER")])
# for (j in c(1:14)){
#   print(ls.RelationalKeys[[j]][1])
#   # key_index <- sapply(paste("\\<",ls.RelationalKeys[[j]],"\\>", sep = ""),grepl,FailDataFrame$STREAM_NAME)
#   key_index <- sapply(paste("\\<",ls.RelationalKeys[[j]],"\\>", sep = ""),grepl,nbiDataFrame$STREAM_NAME)
#   match_keys <- which(apply(key_index, MARGIN = 1, any))
#   nbiDataFrame[nbiRowNames[matchkeys],"STREAM NAME"] <- sapply(match_keys,function (i) gsub(paste("\\<",ls.RelationalKeys[[j]][which(key_index[i,])[1]],"\\>", sep = ""),"",nbiDataFrame[nbiRowNames[i],"STREAM_NAME"]))
#   nbiDataFrame[nbiRowNames[matchkeys],"STREAM OTHER"] <- sapply(match_keys,function (i) paste(nbiDataFrame[nbiRowNames[i],"STREAM_OTHER"], ls.RelationalKeys[[j]][1], sep = " "))
# }
# 
print("searching and replacing cardinal keys")
LookForCardinals <- function(CardinalKeys){
  key_index <- sapply(paste("\\<",CardinalKeys,"\\>", sep = ""),grepl,nbiDataFrame$STREAM_NAME)
  match_keys <- which(apply(key_index, MARGIN = 1, any))
  nbiDataFrame[nbiRowNames[match_keys],"STREAM NAME"] <- sapply(match_keys,function (i) gsub(paste("\\<",CardinalKeys[which(key_index[i,])[1]],"\\>", sep = ""),"",nbiDataFrame[nbiRowNames[i],"STREAM_NAME"]))
  nbiDataFrame[nbiRowNames[match_keys],"STREAM OTHER"] <- sapply(match_keys,function (i) paste(nbiDataFrame[nbiRowNames[i],"STREAM_OTHER"], CardinalKeys[1], sep = " "))
}
nbiTest <- sapply(ls.CardinalKeys[[1]],LookForCardinals)

for (j in c(1:length(ls.CardinalKeys))){
  print(ls.CardinalKeys[[j]][1])
  # key_index <- sapply(paste("\\<",ls.CardinalKeys[[j]],"\\>", sep = ""),grepl,FailDataFrame$STREAM_NAME)
  
#   for (i in match_keys){
# #     FailDataFrame[FailRowNames[i],"STREAM_NAME"] <- gsub(paste("\\<",ls.CardinalKeys[[j]][which(key_index[i,])[1]],"\\>", sep = ""),"",FailDataFrame[FailRowNames[i],"STREAM_NAME"])
# #     FailDataFrame[FailRowNames[i],"STREAM_OTHER"] <- paste(FailDataFrame[FailRowNames[i],"STREAM_OTHER"], ls.CardinalKeys[[j]][1], sep = " ")
#     nbiDataFrame[nbiRowNames[i],"STREAM_NAME"] <- gsub(paste("\\<",ls.CardinalKeys[[j]][which(key_index[i,])[1]],"\\>", sep = ""),"",nbiDataFrame[nbiRowNames[i],"STREAM_NAME"])
#     nbiDataFrame[nbiRowNames[i],"STREAM_OTHER"] <- paste(nbiDataFrame[nbiRowNames[i],"STREAM_OTHER"], ls.CardinalKeys[[j]][1], sep = " ")
#   }
}

print("Beginning processing of STREAM TYPE data")
LookForStreamType <- function(StreamKeys){
  print(paste("Looking for matches to STREAM TYPE", StreamKeys[1],sep=" "))
  key_index <- sapply(paste("\\<",StreamKeys,"\\>", sep = ""),grepl,nbiDataFrame$STREAM_NAME)
  match_keys <- unlist(which(apply(key_index, MARGIN = 1, any)))
  print(head(match_keys))
  if (length(match_keys)!=0){
    print(nbiDataFrame[nbiRowNames[match_keys[1]],"STREAM_NAME"])
    print(paste("\\<",StreamKeys[which(key_index[match_keys[1],])[1]],"\\>", sep = ""))
    print(sapply(match_keys,function (i) gsub(paste("\\<",StreamKeys[which(key_index[i,])[1]],"\\>", sep = ""),"",nbiDataFrame[nbiRowNames[i],"STREAM_NAME"])))
    nbiDataFrame[nbiRowNames[match_keys],"STREAM NAME"] <- sapply(match_keys,function (i) gsub(paste("\\<",StreamKeys[which(key_index[i,])[1]],"\\>", sep = ""),"",nbiDataFrame[nbiRowNames[i],"STREAM_NAME"]))
    nbiDataFrame[nbiRowNames[match_keys],"STREAM TYPE"] <- sapply(match_keys,function (i) paste(nbiDataFrame[nbiRowNames[i],"STREAM_TYPE"], StreamKeys[1], sep = " "))}
}
nbiTest <- sapply(ls.StreamKeys["creek"],LookForStreamType)
# for (j in c(1:length(ls.StreamKeys))){
#   # for failure data
#   print(paste("Looking for matches to STREAM TYPE", ls.StreamKeys[[j]][1],sep=" "))
#   key_index <- sapply(paste("\\<",ls.StreamKeys[[j]],"\\>", sep = ""),grepl,FailDataFrame$STREAM_NAME)
#   match_keys <- which(apply(key_index, MARGIN = 1, any))
#   for (i in match_keys){
#     FailDataFrame[FailRowNames[i],"STREAM_NAME"] <- gsub(paste("\\<",ls.StreamKeys[[j]][which(key_index[i,])[1]],"\\>", sep = ""),"",FailDataFrame[FailRowNames[i],"STREAM_NAME"])
#     FailDataFrame[FailRowNames[i],"STREAM_TYPE"] <- paste(FailDataFrame[FailRowNames[i],"STREAM_TYPE"], ls.StreamKeys[[j]][1], sep = " ")
#   }
#   print("     Completed for FailDataFrame")
  # for NBI data
#   key_index <- sapply(paste("\\<",ls.StreamKeys[[j]],"\\>", sep = ""),grepl,nbiDataFrame$STREAM_NAME)
#   match_keys <- which(apply(key_index, MARGIN = 1, any))
#   nbiDataFrame[nbiRowNames[matchkeys],"STREAM NAME"] <- sapply(match_keys,function (i) gsub(paste("\\<",ls.StreamKeys[[j]][which(key_index[i,])[1]],"\\>", sep = ""),"",nbiDataFrame[nbiRowNames[i],"STREAM_NAME"]))
#   nbiDataFrame[nbiRowNames[matchkeys],"STREAM TYPE"] <- sapply(match_keys,function (i) paste(nbiDataFrame[nbiRowNames[i],"STREAM_TYPE"], ls.StreamKeys[[j]][1], sep = " "))
#   for (i in match_keys) {
#     nbiDataFrame[nbiRowNames[i],"STREAM_NAME"] <- gsub(paste("\\<",ls.StreamKeys[[j]][which(key_index[i,])[1]],"\\>", sep = ""),"",nbiDataFrame[nbiRowNames[i],"STREAM_NAME"])
#     nbiDataFrame[nbiRowNames[i],"STREAM_TYPE"] <- paste(nbiDataFrame[nbiRowNames[i],"STREAM_TYPE"], ls.StreamKeys[[j]][1], sep = " ")
#   }
 # print("     Completed for nbiDataFrame")
# }
return(nbiDataFrame)
}
# for (j in c(1:length(ls.TribsKeys))){
#   key_index <- sapply(paste("\\<",ls.TribsKeys[[j]],"\\>", sep = ""),grepl,FailDataFrame$STREAM_NAME)
#   key_index <- sapply(paste("\\<",ls.TribsKeys[[j]],"\\>", sep = ""),grepl,nbiDataFrame$STREAM_NAME)
#   match_keys <- which(apply(key_index, MARGIN = 1, any))
#   for (i in match_keys){
#     FailDataFrame[FailRowNames[i],"STREAM_NAME"] <- gsub(paste("\\<",ls.TribsKeys[[j]][which(key_index[i,])[1]],"\\>", sep = ""),"",FailDataFrame[FailRowNames[i],"STREAM_NAME"])
#     FailDataFrame[FailRowNames[i],"STREAM_TRIB"] <- paste(FailDataFrame[FailRowNames[i],"STREAM_TRIB"], ls.TribsKeys[[j]][1], "-", sep = " ")
#     nbiDataFrame[nbiRowNames[i],"STREAM_NAME"] <- gsub(paste("\\<",ls.TribsKeys[[j]][which(key_index[i,])[1]],"\\>", sep = ""),"",nbiDataFrame[nbiRowNames[i],"STREAM_NAME"])
#     nbiDataFrame[nbiRowNames[i],"STREAM_TRIB"] <- paste(nbiDataFrame[nbiRowNames[i],"STREAM_TRIB"], ls.TribsKeys[[j]][1], "-", sep = " ")
#   }
# }

# FailDataFrame[,c("STREAM_NAME","STREAM_OTHER","STREAM_TYPE", "STREAM_TRIB")] <- sapply(c("STREAM_NAME","STREAM_OTHER","STREAM_TYPE", "STREAM_TRIB"), function(j)  str_trim(gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",FailDataFrame[,j]), perl=TRUE)))
# nbiDataFrame[,c("STREAM_NAME","STREAM_OTHER","STREAM_TYPE", "STREAM_TRIB")] <- sapply(c("STREAM_NAME","STREAM_OTHER","STREAM_TYPE", "STREAM_TRIB"), function(j)  str_trim(gsub("^ *|(?<= ) | *$", "", gsub("[[:punct:]]"," ",nbiDataFrame[,j]), perl=TRUE)))
# 
# print("Finished identifying stream names")
# then for bridges with counties attempt to match
# nf = north fork - so if can't find 2-letter, try separating

# save(FailDataFrame, file = "FailDataFrameWithStream.RData")
# save(nbiDataFrame, file = file.path(dirs$DataDirFrequent,paste(gsub("-","",Sys.Date()),"nbiDataFrameAllStreamNames.RData", sep="_")))

# LOAD NBI AND FAILURE DATA FRAMES COMPLETELY PROCESSED THROUGH STREAM NAME
# load("/Users/MM/Documents/R/nbiDataFrameWithStream.RData")
# load("/Users/MM/Documents/R/FailDataFrameWithStream.RData")
# nbiDataFrame <- nbiDataFrame[seq(1,nrow(nbiDataFrame),by=nbiDataSeqBy),]
# nbiRowNames  <- rownames(nbiDataFrame)
# 
# FailDataFrame      <- FailDataFrame[seq(1,nFail,by=FailDataSeqBy),]
# FailDataFrame.Char <- c("LOCATION", "FEAT_UND", "TYPE", "MAT", "FAIL_CAUS")
# FailRowNames       <- rownames(FailDataFrame)
# FailStates         <- unique(FailDataFrame$STFIPS)
# print("Loaded NBI and Failure data frames; pre-processed through stream name and type")

# STEP 5: BEGIN MATCHING  -------------------------------------------------------------
# FailStates are coded as STFIPS, FailCountiesKnown in FIPS with ST prefix

# match roads
# mclapply(FailRowNames, function(i) MatchEntry(FailDataFrame[i,c("STFIPS", "FIPS", "FIPS_FROM_CITY", "LOCATION", "ROAD_NAME", "ROAD_TYPE", "ITEM5B")], nbiDataFrame[,c("STFIPS", "FIPS", "ITEM7", "ITEM5B")], "road"), mc.cores = 3)
# print("Finished first attempt at road matching")
# 
# # match streams
# mclapply(FailRowNames, function(i) MatchEntry(FailDataFrame[i,c("STFIPS", "FIPS", "FIPS_FROM_CITY", "FEAT_UND", "STREAM_NAME", "STREAM_TYPE")], nbiDataFrame[,c("STFIPS", "FIPS", "ITEM6A")], "stream"), mc.cores = 3)
# print("Finished first attempt at stream matching")
# 
# PossibleMatchRowsAll <- list()
# for (i in 1:nFail){
#   PossibleMatchRowsAll[[i]] <- character(24000)
# }
# for (i in 1:nFail){
#   load(paste("/Users/MM/Documents/Research/Climate_Scour_Local/R_data/Output/",FailRowNames[i],".RData",sep=""))
#   PossibleMatchRowsAll[[i]] <- PossibleMatchRows
# }
# save(PossibleMatchRowsAll,file = "PossibleMatchesAllAfterStreamMatch.RData")
# NumberMatches <- sapply(1:length(PossibleMatchRowsAll), function(l) ifelse(any(PossibleMatchRowsAll[[l]][2:length(PossibleMatchRowsAll[[l]])]>0),length(PossibleMatchRowsAll[[l]][2:length(PossibleMatchRowsAll[[l]])][PossibleMatchRowsAll[[l]][2:length(PossibleMatchRowsAll[[l]])]>0]),-1*as.numeric(max(PossibleMatchRowsAll[[l]][2:length(PossibleMatchRowsAll[[l]])][PossibleMatchRowsAll[[l]][2:length(PossibleMatchRowsAll[[l]])]<0]))))
# #print(NumberMatches)
# #NumberMatches <- as.numeric(NumberMatches)
# hist(NumberMatches, plot=TRUE)
# #return(nbiDataFrame) ###########
# return(FailDataFrame) ###########
# STEPS TO MATCH THE STRINGS ------------

# 6. Match first on state, 
#         then on "special" portion of stream name (e.g., "Gila),
#         then on tags - perhaps need to match 1 or greater?
# 7. If have multiple matches, move onto county
#         if have county data for both, see if those match
#         if only one matches, done, if not ...
# 8. If still have multiple matches, move onto road
#         separate "named" portion of road - route number, etc. to match
#         table for tags, etc.
# 9. If still multiple matches, spit out to do by hand - if have lots
#    then will ???

### PROCESS THE BRIDGE DATA TO MATCH
# county codes are in ITEM3 for NBI data - need to use FIPS to match

# ### NOW MATCH - SEPARATELY BY WORD IN THE BRIDGE DATA - ALL MUST MATCH SAME ROW OF DAM DATA
# # first see if in same state - dams may span two states, so use both 
# # (assumes that pass data from one state at a time)
# MatchStateCode = BridgeDataFrame[1,"STATE_CODE_001"]
# if (MatchStateCode==6) {MatchState=="California"
# else
# 	if (MatchStateCode==4) {MatchState=="Arizona"
# 	else  if (MatchStateCode==35) {MatchState=="New Mexico" else return("Not coded for given state")   }
# 	}
# }
# PossibleMatchDams <-subset(DamDataFrame,ADMIN_UNIT==MatchState | SEC_ADMIN==MatchState)
# for (i in 1:nrow(BridgeDataFrame)){
# 	PossibleMatch <- PossibleMatchDams
# 	ls.names <- strsplit(BridgeDataFrame[i,"RIVERMATCH"]," ")
# 	for (j in 1:length(ls.names)) {
# 		DamMatch <- grep(ls.names[1][j],PossibleMatch)
# 		if (DamMatch != NA) {
# 			PossibleMatch <- PossibleMatch[DamMatch,]
# 		else next
# 		}
# 	}
# 	# if went through all names in Bridge River and found one or more matches, then check elevation
# 	if (nrow(PossibleMatch)!=0) {
# 		#################### PROBABLY GOING TO NEED TOPO FILE	
# 		for (j in 1:nrow(PossibleMatch)){
# 			
# 		}
# 		
# 	}
# 	
# }
# 	
# 
# ### CHECK WHETHER OR NOT DAM IS UPSTREAM, AND DISTANCE
# RETURN -------------------------------------
# return(nbiDataFrame)
# }
