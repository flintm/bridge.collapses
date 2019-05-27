# Match failed bridges to locations of bridges in NBI database

# Location needed to analyze climate data, and later to look at impacts (replacement value, traffic disruption)
# Updated to load state and county data from CSV files rather than hard-codes
# Updated on 2014-11-17 to load pre-processed data
# Updated on 2014-11-21 to compute matching in parallel (over states)
# Updated on 2014-12-04 with new method for matching cities and counties
# Updated on 2015-01-05 just to do matching (processing has been completed previously)
# Updated on 2015-01-06 to be used in Sherlock Cluster (matching only)

# ENVIRONMENTAL VARIABLES, LIBRARIES & SUBFUNCTIONS ----------
HOME              <- Sys.getenv("HOME") # do I need to use scan with this?
SCRATCH           <- Sys.getenv("SCRATCH")
DataDirectory     <- file.path(HOME,"matchbridges") # assumes directories have been made
ScriptsDirectory  <- file.path(HOME,"matchbridges")
OutputDirectory   <- "$SCRATCH/matchbridges"
nodelist          <- scan(Sys.getenv("MYNODELIST", ""), what="", sep="\n")
nCores            <- length(nodelist)
library(stringr)
library(stringdist)
library(parallel)
source(file.path(ScriptsDirectory,"MatchEntry.R")) 
source(file.path(ScriptsDirectory,"PerformMatch.R"))

# SET OPTIONS ------------
RunRoads          <- TRUE
FailRowsInRoad    <- c(301:1127) # set rows to be analyzed
RunStreams        <- FALSE
FailRowsInStream  <- c(1:1127)   # set to be analyzed for stream matching
LoadFromFile      <- TRUE        # streams will pull matches from road/route match
RunCompileMatches <- FALSE
MatchTypeToSum    <- "road"


# LOAD NBI AND FAILURE DATA FRAMES COMPLETELY PROCESSED THROUGH STREAM NAME ------------

load(file.path(DataDirectory,"nbiDataFrameWithStream.RData"))
nbiRowNames  <- rownames(nbiDataFrame)
nNBI         <- nrow(nbiDataFrame)

load(file.path(DataDirectory,"FailDataFrame-Processed-RoadCountyStream.RData"))
FailRowNames     <- rownames(FailDataFrame)
nFail            <- nrow(FailDataFrame)
FailRowNamesRoad <- FailRowNames[FailRowsInRoad]
nFailRoad        <- length(FailRowsInRoad)
FailRowNamesStream <- FailRowNames[FailRowsInStream]
nFailStream        <- length(FailRowsInStream)

# MATCHING  -------------------------------------------------------------
# match roads and routes
if (RunRoads){
  mclapply(FailRowNamesRoad, function(i) MatchEntry(FailDataFrame[i,c("ID","STFIPS", "FIPS", "FIPS_2", "FIPS_FROM_CITY", "FIPS_FROM_CITY_2", "FIPS_FROM_CITY_3", "LOCATION", "ROAD_NAME", "ROAD_TYPE", "ROUTE_NO", "ITEM5B")], 
                                              nbiDataFrame[,c("STFIPS", "FIPS", "ITEM7", "ITEM5B", "ITEM5D")], "road", FALSE, NA), mc.cores = nCores)
}
# match streams
if (RunStreams){
  mclapply(FailRowNamesStream, function(i) MatchEntry(FailDataFrame[i,c("ID", "STFIPS", "FIPS", "FIPS_2", "FIPS_FROM_CITY", "FIPS_FROM_CITY_2", "FIPS_FROM_CITY_3", "FEAT_UND", "STREAM_NAME", "STREAM_TYPE")], 
                                              nbiDataFrame[,c("STFIPS", "FIPS", "ITEM6A")], "stream", LoadFromFile, "road"), mc.cores = nCores)
}
# COMPILE MATCHES ------------------
if (RunCompileMatches){
  IDs <- FailDataFrame$ID
  rm(FailDataFrame, nbiDataFrame)
  PossibleMatchRowsAll <- list()
  for (i in 1:nFail){
    PossibleMatchRowsAll[[i]] <- character(21000)
  }
  
  NumberMatches<- integer(nFail)
  for (i in 1:nFail){
    pattern <- ifelse(MatchTypeToSum=="road", paste("ID",IDs[i],"-r",sep=""), paste("ID",IDs[i],"-stream",sep=""))
    matchFile <- list.files(path=OutputDirectory, pattern = pattern)[1]
    load(file.path(OutputDirectory,matchFile))
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
  save(NumberMatches, file = file.path(OutputDirectory,paste("NumberMatchesAfter-",MatchTypeToSum,".RData",sep="")))
  save(PossibleMatchRowsAll,file = file.path(OutputDirectory,paste("PossibleMatchesAllAfter-",MatchTypeToSum,".RData",sep="")))
}
