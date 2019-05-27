# Match failed bridges to locations of bridges in NBI database

# Location needed to analyze climate data, and later to look at impacts (replacement value, traffic disruption)
# Updated to load state and county data from CSV files rather than hard-codes
# Updated on 2014-11-17 to load pre-processed data
# Updated on 2014-11-21 to compute matching in parallel (over states)
# Updated on 2014-12-04 with new method for matching cities and counties
# Updated on 2015-01-05 just to do matching (processing has been completed previously)
# Updated on 2015-01-06 to be used in Sherlock Cluster (matching only)
# Updated on 2015-10-10 to use Rmpi for parallelization (compatible with Sherlock)

# ENVIRONMENTAL VARIABLES, LIBRARIES & SUBFUNCTIONS ----------
HOME              <- "/home/flint" 
SCRATCH           <- "/scratch/users/flint"
DataDirectory     <- file.path(HOME,"matchbridges")
ScriptsDirectory  <- file.path(HOME,"matchbridges")
OutputDirectory   <- file.path(SCRATCH,"matchbridges")

library(stringr)

source(file.path(ScriptsDirectory,"MatchEntry.R")) 
source(file.path(ScriptsDirectory,"PerformMatch.R"))

# SET OPTIONS ------------
VERBOSE           <- FALSE
RunRoads          <- TRUE
FailRowsInRoad    <- c(378:380)  #1
# FailRowsInRoad    <- c(528:677)  #2
# FailRowsInRoad    <- c(678:827)  #3
# FailRowsInRoad    <- c(828:977)  #4
# FailRowsInRoad    <- c(978:1127) #5
RunStreams        <- FALSE
FailRowsInStream  <- c(1:1127)   # set to be analyzed for stream matching
LoadFromFileStream<- TRUE        # streams will pull matches from road/route match
RunCompileMatches <- FALSE
MatchTypeToSum    <- "road"

# LOAD NBI AND FAILURE DATA FRAMES COMPLETELY PROCESSED THROUGH STREAM NAME ------------

load(file.path(DataDirectory,"nbiDataFrameWithStream.RData"))
nbiRowNames     <- rownames(nbiDataFrame)
nNBI            <- nrow(nbiDataFrame)
nbiColsForMatch <- c("STFIPS", "FIPS", "ITEM7", "ITEM5B", "ITEM5D", "ITEM6A")
nbiDataFrame    <- nbiDataFrame[,nbiColsForMatch]

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
  MatchType       <- "road"
  LoadFromFile    <- FALSE
  MatchTypeToLoad <- NA
  lapply(FailRowNamesRoad, MatchEntry)
}
# match streams
if (RunStreams){
  MatchType       <- "stream"
  LoadFromFile    <- LoadFromFileStream
  MatchTypeToLoad <- "road"
  lapply(FailRowNamesStream, MatchEntry)
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
