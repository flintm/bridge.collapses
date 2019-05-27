Match_Failed_Bridges <- function(FailRowsIn = NA, nbiRowsIn = NA, FailDataSeqBy = 1, nbiDataSeqBy = 1){
 # Match failed bridges to locations of bridges in NBI database
 # Location needed to analyze climate data, and later to look at impacts (replacement value, traffic disruption)
 # Updated to load state and county data from CSV files rather than hard-codes
 # Updated on 2014-11-17 to load pre-processed data
 # Updated on 2014-11-21 to compute matching in parallel (over states)
 # Updated on 2014-12-04 with new method for matching cities and counties
 # Updated on 2015-01-05 just to do matching (processing has been completed previously)
 
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
 #     (5) match bridges using subset from stream
 #     (6) for bridges with no good match, re-run matching without subset from stream
 #     (7) for unRowsWithMatched in steps (6)-(9), use alternate approach (recursive)
 #     (8) return merged dataframe and report on un-matched and un-located failed bridges
 #
 # To-do list:
  #     (1) check ID70 - had 1 match from road, but then overwrote in stream (presumably b/c did not match)...
  #         so if the 2nd match fails, need to loop again with all in state?
  #     (2) pre-allocate the possibleMatchAll matrix, but only after figuring out how big it needs (i.e., # bridges in state - 1)
  #         and optionally, if have too many just put in the state info
  #     (3) write code for step (6), (7) - if necessary
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
  OrigDataDir <- "/Users/MM/Documents/Research/Climate_Scour_Local/Data_original_files"
  source('~/Documents/R/convert.df.classes.R')
  source('~/Documents/R/clean.df.strings.R')
  source('~/Documents/Research/Climate_Scour_Local/R_data/20141124_MatchEntry.R')
  source('~/Documents/Research/Climate_Scour_Local/R_data/20141124_PerformMatch.R') 
  source('~/Documents/Research/Climate_Scour_Local/R_data/20141205_GetAndRemoveLocationData_CityCounty.R')
  source('~/Documents/Research/Climate_Scour_Local/R_data/20141208-GetAndRemoveLocationForStream.R')

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
df.Cities <- read.csv(file.path(OrigDataDir,"PoliticalBoundaries","CityData.csv", header = TRUE, 
                      sep = ",", quote = "\"", dec = ".", 
                      colClasses = c("character", "character", "character", "character", "character", "numeric", "numeric"),
                      col.names = c("GNIS_ID", "ANSICODE", "CITY_NAME", "FIPS", "STFIPS","LATDD","LONGDD"))
df.Cities$FIPS <- as.integer(paste(df.Cities$STFIPS,df.Cities$FIPS,sep=""))
df.Cities$STFIPS <- as.integer(df.Cities$STFIPS)

# STEP 1-4: LOAD NBI AND FAILURE DATA FRAMES COMPLETELY PROCESSED THROUGH STREAM NAME ------------
load("/Users/MM/Documents/R/nbiDataFrameWithStream.RData")
if (all(!is.na(nbiRowsIn))){
  nbiDataFrame <- nbiDataFrame[nbiRowsIn,]
}
nbiDataFrame <- nbiDataFrame[seq(1,nrow(nbiDataFrame),by=nbiDataSeqBy),]
nbiRowNames  <- rownames(nbiDataFrame)
nNBI         <- nrow(nbiDataFrame)

load("~/Documents/R/FailDataFrame-Processed-RoadCountyStream.RData")
if (all(!is.na(FailRowsIn))){
  FailDataFrame <- FailDataFrame[FailRowsIn,]
}
FailDataFrame  <- FailDataFrame[seq(1,nrow(FailDataFrame),by=FailDataSeqBy),]
FailRowNames   <- rownames(FailDataFrame)
FailStates     <- unique(FailDataFrame$STFIPS)
nFail          <- nrow(FailDataFrame)
# print(FailRowNames)
# print(nFail)
print("Loaded NBI and Failure data frames; pre-processed through stream name and type")

# STEP 5: BEGIN MATCHING  -------------------------------------------------------------
# FailStates are coded as STFIPS, FailCountiesKnown in FIPS with ST prefix

# if I'm not getting sufficient matches from ITEM5D and ITEM7, try these:
#  ITEM4 census place code, probably ANSI_CODE
#  ITEM5D ROUTE NUMBER - ACTUAL ROUTE NUMBER - as "00088" or whatever, so will have to drop leading zeros and then check
#  ITEM5E DIRECTIONAL SUFFIX: 0 NA, 1 NORTH, 2 EAST, 3 SOUTH, 4 WEST
#  ITEM9 location - NARRATIVE, PLAIN TEXT, DISTANCE, MI/KM, OF CITY/JUNCTION/... (uses mile as mile, mi, m)
#  ITEM5C DESIGNATED LEVEL OF SERVICE:
#    0 NONE OF THE BELOW, 1 MAINLINE, 2 ALTERNATE, 3 BYPASS, 4 SPUR, 6 BUSINESS, 7 RAMP, CONNECTOR, ..., 8 SERVICE OR FRONTAGE ROAD


# match roads
lapply(FailRowNames, function(i) MatchEntry(FailDataFrame[i,c("ID", "STFIPS", "FIPS", "FIPS_2", "FIPS_FROM_CITY", "FIPS_FROM_CITY_2", "FIPS_FROM_CITY_3", "LOCATION", "ROAD_NAME", "ROAD_TYPE", "ROUTE_NO", "ITEM5B")], 
                                            nbiDataFrame[,c("STFIPS", "FIPS", "ITEM7", "ITEM5B", "ITEM5D")], "road", FALSE, NA))
# mclapply(FailRowNames, function(i) MatchEntry(FailDataFrame[i,c("STFIPS", "FIPS", "FIPS_2", "FIPS_FROM_CITY", "FIPS_FROM_CITY_2", "FIPS_FROM_CITY_3", "LOCATION", "ROAD_NAME", "ROAD_TYPE", "ROUTE_NO", "ITEM5B")], nbiDataFrame[,c("STFIPS", "FIPS", "ITEM7", "ITEM5B")], "road", FALSE, NA), mc.cores = 3)
#print("Finished first attempt at road matching")

# match routes

# match streams
lapply(FailRowNames, function(i) MatchEntry(FailDataFrame[i,c("ID", "STFIPS", "FIPS", "FIPS_2", "FIPS_FROM_CITY", "FIPS_FROM_CITY_2", "FIPS_FROM_CITY_3", "FEAT_UND", "STREAM_NAME", "STREAM_TYPE")], 
                                           nbiDataFrame[,c("STFIPS", "FIPS", "ITEM6A")], "stream", TRUE, "road"))
# mclapply(FailRowNames, function(i) MatchEntry(FailDataFrame[i,c("ID", "STFIPS", "FIPS", "FIPS_2", "FIPS_FROM_CITY", "FIPS_FROM_CITY_2", "FIPS_FROM_CITY_3", "FEAT_UND", "STREAM_NAME", "STREAM_TYPE")], 
#                                               nbiDataFrame[,c("STFIPS", "FIPS", "ITEM6A")], "stream", FALSE, "road"), mc.cores = 3)
print("Finished first attempt at stream matching")

IDs <- FailDataFrame$ID
rm(FailDataFrame, nbiDataFrame)
PossibleMatchRowsAll <- list()
for (i in 1:nFail){
  PossibleMatchRowsAll[[i]] <- character(24000)
}

MatchTypeToSum <- "stream"
NumberMatches<- integer(nFail)
for (i in 1:nFail){
  load(paste("/Users/MM/Documents/Research/Climate_Scour_Local/R_data/Output/ID",IDs[i],"-",MatchTypeToSum,".RData",sep=""))
  PossibleMatchRowsAll[[i]] <- PossibleMatchRows
  NumberMatches[i] <- length(PossibleMatchRows[!grepl("[[:alpha:]]",PossibleMatchRows) & !grepl("-", PossibleMatchRows)])
  if (NumberMatches[i]==0){
    if(any(grepl("STFIPS",PossibleMatchRows))) {
      if (length(PossibleMatchRows > 3)) {
        CountyPoss <-  abs(as.numeric(PossibleMatchRows[grepl("^-",PossibleMatchRows)]))
        NumberMatches[i] <- sum(CountyPoss[1:length(CountyPoss)])
      }
      else NumberMatches[i] <- -1*(as.numeric(PossibleMatchRows[length(PossibleMatchRows)]))
    }
    else NumberMatches[i] <- sum(abs(as.numeric(PossibleMatchRows[grepl("^-",PossibleMatchRows)])))
  }  
}
save(NumberMatches, file = paste("NumberMatchesAfter-",MatchTypeToSum,".RData",sep=""))
save(PossibleMatchRowsAll,file = paste("PossibleMatchesAllAfter-",MatchTypeToSum,".RData",sep=""))
#NumberMatches <- sapply(1:length(PossibleMatchRowsAll), function(l) ifelse(any(PossibleMatchRowsAll[[l]][2:length(PossibleMatchRowsAll[[l]])]>0),length(PossibleMatchRowsAll[[l]][2:length(PossibleMatchRowsAll[[l]])][PossibleMatchRowsAll[[l]][2:length(PossibleMatchRowsAll[[l]])]>0]),-1*as.numeric(max(PossibleMatchRowsAll[[l]][2:length(PossibleMatchRowsAll[[l]])][PossibleMatchRowsAll[[l]][2:length(PossibleMatchRowsAll[[l]])]<0]))))
#print(NumberMatches)
#NumberMatches <- as.numeric(NumberMatches)
hist(NumberMatches, plot=TRUE)

# RETURN -------------------------------------
return(PossibleMatchRowsAll)
}