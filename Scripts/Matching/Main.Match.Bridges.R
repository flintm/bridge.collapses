# MAIN function for processing and matching of collapsed bridges from NYSDOT Bridge 
# Failure Database and National Bridge Inventory and USGS Gauge Databases
# Copyright Madeleine Flint, 2016
# ODC-ODbl Open Database License, see http://opendatacommons.org/licenses/odbl/summary/

# Main function completes 8 main steps:
#   (1) Load and format input data
#   (2) Pre-process and clean data
#   (3) Sort data to determine appropriate matching strategies
#   (4) Identify potential matching information in entries
#   (5) Perform preliminary matching in either sequential or parallel mode
#   (6) Rank and confirm matches (automated)
#   (7) Support supervised match confirmation (interactive)
#   (8) Obtain additional data
# Each step calls a subfunction that will load input data from the
# previous step and write output data. As long as the output data from
# prior steps is not deleted it is therefore possible to continue the
# analysis at a later point in time using the stored data.

# USER CONTROLS ---------------------------------------------------------------
# RUN determines which of the steps above will run. SAVE determines when
# intermediate results will be saved & subsequently loaded.
RUN         <- c(rep(TRUE, 7), FALSE)
SAVE        <- c(rep(FALSE, 3), TRUE, TRUE, FALSE, FALSE, FALSE)

# Should the matching gradually winnow down matches (sequential, SEQ) or
# should all possible matches be maintained even if some information is
# non-matching or contradictory (parallel, PAR)?
MATCH_STYLE <- "SEQ"
MATCH_TYPES <- c("road","stream")

# CHECK CONTROLS AND SET UP DIRECTORIES ----------------------------------------
if (!is.logical(RUN) | !is.logical(SAVE) | length(RUN)!=8 | length(SAVE)!=8){
  warning('RUN and SAVE must be defined as length 8 logicals')
  stop()
}
if(RUN[5] & !(MATCH_STYLE %in% c("SEQ", "PARALLEL"))){
  warning('To perform matching, MATCH_STYLE must be defined as SEQ or PAR')
  stop()
}
if(RUN[5] & !(all(MATCH_TYPES %in% c("road", "stream")))){
  warning('Only matching by road (includes route) and stream supported.')
  stop()
}
dirM     <- getwd()
dirsGit <- list(dir         = dirM,
                Scripts     = file.path(dirM,"Scripts"),
                Data        = file.path(dirM,"Data"),
                ScriptsPlot = file.path(dirM,"Scripts","Plotting"),
                Plots       = file.path(dirM,"Plots"),
                Analysis    = file.path(dirM,"Analysis"))

folders <- list.dirs(path = dirM, full.names = TRUE, recursive = TRUE)
dirsCreate <- unlist(dirsGit[2:length(dirsGit)])[!(unlist(dirsGit[2:length(dirsGit)]) %in% folders)]
if (any(!(sapply(dirsGit[2:3], function(d) d %in% folders)))){
  warning("Scripts and Data directories and files must be present")
  stop()
}
if (length(dirsCreate)!=0) sapply(dirsCreate, function(d) dir.create(d))

analysis.folders <- list.dirs(path = dirsGit$Analysis, full.name = TRUE, recursive = FALSE)
analysis.dirs    <- c("Processed","Temp","Results","PostProcessed")
dirsCreate       <- analysis.dirs[!(analysis.dirs %in% analysis.folders)] 
if (length(dirsCreate)!=0) sapply(dirsCreate, function(d) dir.create(file.path(dirsGit$Analysis,d)))

rm(dirM,folders,dirsCreate,analysis.dirs,analysis.folders)


# (1) LOAD AND FORMAT INPUT DATA ----------------------------------------------------
# Input files in CSV, TAB, XLSX, XLS, RData, or GIS (NBI only) are read in and 
# converted into a dataframe with appropriate data classes and names
if (RUN[1]){
  input.files <- c(fail = file.path(dirsGit$Data,"FailDataFrame.RData"),
                   nbi  = file.path(dirsGit$Data,"nbiDataFrame.RData"))
  
  source(file.path(dirsGit$Scripts,"Load Input","Load.Format.Failed.Bridges.R"))
  df.Fail <- Load.Format.Failed.Bridges(input.files["fail"])
  source(file.path(dirsGit$Scripts,"Load Input","Load.Format.NBI.Bridges.R"))
  df.NBI  <- Load.Format.NBI.Bridges(input.files["nbi"])
  if (SAVE[1]){
    save(df.Fail, file = paste0(file.path(dirsGit$Analysis,"Processed"),"Formatted.Failed.Bridges.RData"))
    save(df.NBI,  file = paste0(file.path(dirsGit$Analysis,"Processed"),"Formatted.NBI.Bridges.RData"))
    rm(df.Fail, df.NBI)
  }
  rm(input.files, Load.Format.Failed.Bridges, Load.Format.NBI.Bridges)
}

# (2) PRE-PROCESS AND CLEAN DATA ----------------------------------------------------
# Failed bridges "LOCATION", "FEAT_UND", "MAT", AND "TYPE" entries are processed for 
# misspellings and other possibly erroneous information. NBI bridges have their
# entries cleaned and checked for errors.
if (RUN[2]){
  source(file.path(dirsGit$Scripts,"Process Input","PreProcess.Failed.Bridges.R"))
  df.Fail <- ifelse("df.Fail" %in% ls(),
                    PreProcess.Failed.Bridges(df.Fail),
                    PreProcess.Failed.Bridges(NA,file.path(dirsGit$Analysis,"Processed","Formatted.Failed.Bridges.RData")))
  source(file.path(dirsGit$Scripts,"Process Input","PreProcess.NBI.Bridges.R"))
  df.NBI  <- ifelse("df.NBI" %in% ls(),
                    PreProcess.Failed.Bridges(df.NBI),
                    PreProcess.Failed.Bridges(NA,file.path(dirsGit$Analysis,"Processed","Formatted.NBI.Bridges.RData")))
  if (SAVE[2]){
    save(df.Fail, file = file.path(dirsGit$Analysis,"Processed","Cleaned.Failed.Bridges.RData"))
    save(df.NBI,  file = file.path(dirsGit$Analysis,"Processed","Cleaned.NBI.Bridges.RData"))
    rm(df.Fail, df.NBI)
  }
  rm(PreProcess.Failed.Bridges,PreProcess.NBI.Bridges)
}

# (3) SORT DATA FOR MATCHING -------------------------------------------------------
# Failed bridges are sorted into over-water and/or over-route categories based on
# "FAIL_CAUS". NBI bridges are separated by state and stored in the "Temp" folder.
if (RUN[3]){
  source(file.path(dirsGit$Scripts,"Process Input","Sort.Failed.Bridges.R"))
  ls.Fail.Sorted <- ifelse(df.NBI %in% ls(), # will contain IDs and list of states for NBI
                           Sort.Failed.Bridges(df.Fail),
                           Sort.Failed.Bridges(NA,file.path(dirsGit$Analysis,"Processed","Cleaned.Failed.Bridges.RData"))) 
  source(file.path(dirsGit$Scripts,"Process Input","Sort.NBI.Bridges.State.R"))
  Sort.NBI.Bridges.State(ls.Fail.Sorted[["States"]])
  if (SAVE[3]){
    save(ls.Fail.Sorted, file = file.path(dirsGit$Analysis,"Processed","Sorted.Failed.Bridges.RData"))
    DeleteBridgesByState <- FALSE
    save(DeleteBridgesByState,  file = file.path(dirsGit$Analysis,"Processed","DeleteTempNBI.RData"))
    rm(DeleteBridgesByState, df.Fail, ls.Fail.Sorted)
  }
  rm(Sort.Failed.Bridges, Sort.NBI.Bridges.State)
}

# (4) IDENTIFY MATCHING INFORMATION IN DATA ENTRIES-----------------------------------------
# Failed bridge "LOCATION" and "FEAT_UND" are searched for county, city, road, route,
# and stream names. Other identifying information (e.g., "3.5 miles from") is also
# separated. Possible-match NBI bridges have stream information identified in "ITEM6A",
# which describes the feature under the bridge.
if (RUN[4]){
  source(file.path(dirsGit$Scripts,"Process Input","Find.Information.Failed.Bridges.R"))
  Check   <- all(c("df.Fail", "ls.Fail.Sorted") %in% ls())
  if(!Check){
    if(!("df.Fail" %in% ls())) load(file.path(dirsGit$Analysis,"Processed","Sorted.Failed.Bridges.RData"))
    if(!("ls.Fail.Sorted" %in% ls())) load(file.path(dirsGit$Analysis,"Processed","Sorted.Failed.Bridges.RData"))
  }
  df.Fail <- Find.Information.Failed.Bridges(df.Fail, ls.Fail.Sorted)
  source(file.path(dirsGit$Scripts,"Process Input","Find.Stream.NBI.Bridges.R"))
  Find.Stream.NBI.Bridges.State(ls.Fail.Sorted[["States"]])
  if (SAVE[4]){
    save(df.Fail, file = file.path(dirsGit$Analysis,"Processed","Match.Information.Failed.Bridges.RData"))
    DeleteBridgesByState <- FALSE
    save(DeleteBridgesByState,  file = file.path(dirsGit$Analysis,"Processed","DeleteTempNBI.RData"))
    rm(DeleteBridgesByState, df.Fail, ls.Fail.Sorted)
  }
  rm(Sort.Failed.Bridges, Sort.NBI.Bridges.State, Check)
}

# (5) PERFORM PRELIMINARY MATCHING ----------------------------------------------------------------------------------
# Identify NBI bridges that could be linked to failed bridges. Matching style and type of matching
# controlled by MATCH_STYLE and MATCH_TYPES. Sequential matching will first search for matches to the first
# type, and will consider only those possible matching when searching for matches to the second (or later) types.
# Parallel matching searches for the types separately and then combines the match pools. Currently supported
# types are "road" (which includes searching for route numbers) and "stream". County and city information is
# automatically searched.
if (RUN[5]){
  source(file.path(dirsGit$Scripts,"Matching","Match.Failed.Bridges.R"))
  Check   <- all(c("df.Fail", "ls.Fail.Sorted") %in% ls())
  if(!Check){
    if(!("df.Fail" %in% ls())) load(file.path(dirsGit$Analysis,"Processed","Match.Information.Failed.Bridges.RData"))
    if(!("ls.Fail.Sorted" %in% ls())) load(file.path(dirsGit$Analysis,"Processed","Sorted.Failed.Bridges.RData"))
  }
  ls.Matches <- Match.Failed.Bridges(df.Fail, ls.Fail.Sorted)
  if (SAVE[5]){
    save(ls.Matches, file = file.path(dirsGit$Analysis,"Results","Possible.Matches.Failed.Bridges.RData"))
    rm(ls.Matches, df.Fail, ls.Fail.Sorted)
  }
  rm(Match.Failed.Briges)
}

# (6) AUTOMATED RANKING AND CONFIRMATION OF MATCHES ---------------------------------------------------------------
# Use quality of match as well as other information, such as date constructed, date failed, material, and type to
# rank possible matches.
if (RUN[6]){
  source(file.path(dirsGit$Scripts,"Matching","Rank.Matched.Bridges.R"))
  Check   <- all(c("df.Fail", "ls.Matches") %in% ls())
  if(!Check){
    if(!("df.Fail" %in% ls())) load(file.path(dirsGit$Analysis,"Processed","Match.Information.Failed.Bridges.RData"))
    if(!("ls.Matches" %in% ls())) load(file.path(dirsGit$Analysis,"Processed","Possible.Matches.Failed.Bridges.RData"))
  }
  ls.Matches <- Rank.Matched.Bridges(ls.Matches, df.Fail)
  if (SAVE[6]){
    save(ls.Matches, file = file.path(dirsGit$Analysis,"Results","Ranked.Matches.Failed.Bridges.RData"))
    rm(ls.Matches, df.Fail, ls.Fail.Sorted)
  }
  rm(Rank.Matched.Briges)
}

# (7) SUPERVISED MATCH CONFIRMATION --------------------------------------------------------
# Shows a series of maps and data entry such that possible "true" matches can be identified.
if (RUN[7]){
  source(file.path(dirsGit$Scripts,"Matching","Confirm.Matched.Bridges.R"))
  Check   <- all(c("df.Fail", "ls.Matches") %in% ls())
  if(!Check){
    if(!("df.Fail" %in% ls())) load(file.path(dirsGit$Analysis,"Processed","Match.Information.Failed.Bridges.RData"))
    if(!("ls.Matches" %in% ls())) load(file.path(dirsGit$Analysis,"Processed","Possible.Matches.Failed.Bridges.RData"))
  }
  ls.Matches <- Confirm.Matched.Bridges(ls.Matches, df.Fail)
  if (SAVE[7]){
    save(ls.Matches, file = file.path(dirsGit$Analysis,"Results","Confirmed.Matches.Failed.Bridges.RData"))
    rm(ls.Matches, df.Fail)
  }
  rm(Confirm.Matched.Briges)
}

# (8) LOAD AND INCORPORATE ADDITIONAL INFORMATION -------------------------------
# Currently supports searching for hurricanes, dams, and USGS gauges.


# CLEANUP ----------------------------------------------------------------------
if("DeleteTempNBI.RData" %in% list.files(path = file.path(dirsGit$Analysis,"Processed"))){
  load(file.path(dirsGit$Analysis,"Processed","DeleteTempNBI.RData"))
} else{ DeleteBridgesByState <- TRUE}
if (DeleteBridgesByState){
  DeleteList <- list.files(path = file.path(dirsGit$Analysis,"Temp"), pattern = "\\<NBIbridges", full.names = TRUE)
  file.remove(DeleteList)
}
DeleteList <- list.files(path = file.path(dirsGit$Analysis,"Temp"), pattern = "temp", full.names = TRUE)
file.remove(DeleteList)
