# Note: relies on MatchTarget source data existing in global environment

GenerateMatches <- function(MatchEntry,            # 1-row data.frame for which to find matches
                            MatchType,             # type of match to be performed (bin, stream, road, route, gage, dam)
                            MatchSource  = "fail", # source of MatchEntry data
                            MatchTarget  = "nbi",  # source of MatchTarget data (potential matches)
                            maxStringDist = 3,     # max string distance passed to PerformMatch
                            SAVE         = FALSE,  # save PossibleMatchRows to file if TRUE
                            OutPath      = getwd(),# path to save output files of potential matches
                            LoadFromFile = FALSE,  # loads potential matches from existing file if TRUE
                            LoadType     = "r",    # will look for ID#-"LoadType", so e.g., this will find "ID#-route" and "ID#-road"
                            LoadPath     = getwd(),# path to loading potential matches from existing file
                            capPossPct   = 0.5,    # pass to PerformMatch, if nPossMatch > capPossPct*nMatchRows, return "-"
                            capN         = 200,    # pass to PerformMatch, if nPossMatch > capN, return "-"
                            VERBOSE      = FALSE){ # print progress to screen 
require(stringdist)

# check input data ----------------
  if(class(MatchEntry)!="data.frame") MatchEntry <- as.data.frame(MatchEntry)
  if(nrow(MatchEntry)!=1){
    warning("Only one entry may be matched at a time.")
    stop()
  }
  
  MatchTypes <- c("bin", "stream", "road", "route", "gage", "dam")
  if(all(!(grepl(MatchType,MatchTypes)))){
    warning("Match type not supported.")
    stop()
  }
  MatchSources <- c("fail", "nbi", "gage", "dam")
  if(all(!(grepl(MatchSource,MatchSources)))){
    warning("Match source not supported.")
    stop()
  }
  MatchTargets <- c("fail", "nbi", "gage", "dam")
  if(all(!(grepl(MatchTarget,MatchTargets)))){
    warning("Match target not supported.")
    stop()
  }


  # setup matching particulars depending on data source and match type -------
  MatchColOrigName  <-   c(bin     = "BIN",
                           stream  = "FEAT_UND",
                           road    = "LOCATION",
                           route   = "ROUTE_NO", # noting not the original for this b/c many don't have a route
                           gage    = "FEAT_UND",
                           dam     = "FEAT_UND",
                           nbiGage = "STANAME")[MatchType]
  MatchIDENT    <- c(fail    = "ID",
                     gage    = "STAID",
                     dam     = "ID",
                     nbi     = "NBI_ROW")[MatchSource] 
  MatchEntry$IDENT <- MatchEntry[1,MatchIDENT[MatchSource]]
  CountyCols <- list(fail    = c("FIPS","FIPS_2", "FIPS_FROM_CITY", "FIPS_FROM_CITY_2", "FIPS_FROM_CITY_3"),
                     gage    = NA,
                     dam     = NA,
                     nbi     = "ITEM3")[[MatchSource]]
  CountyCols <- CountyCols[MatchEntry[,CountyCols]!="" & !is.na(MatchEntry[,CountyCols]) & !is.null(MatchEntry[,CountyCols])]

if (VERBOSE) print(paste0("ID of MatchEntry is: ",MatchEntry$IDENT, ", and MatchType is: ", MatchType))
  
  # set up match targets dataframe ---------
  MatchTargetData <- c(fail    = "df.Fail",
                       gage    = "df.USgages",
                       dam     = "df.GRanD",
                       nbi     = "df.NBI")[MatchTarget]
  if(!(MatchTargetData %in% ls(globalenv()))){
    warning(paste0('Data frame of match target,',MatchTargetData,', not present in environment'))
    stop()
  }
  assign("MatchTargetData",get(MatchTargetData))
  PossibleMatches <- ""
  
  
  # load possible match rows from file if specified --------
  if(LoadFromFile){
    if(VERBOSE) print("  Loading from file")
    pattern   <- paste0("IDENT",MatchEntry$IDENT,"-",LoadType)
    matchFile <- list.files(path=LoadPath, pattern = pattern)[1]
    load(file.path(LoadPath,matchFile)) # loads PossibleMatchRows
    if(!(PossibleMatchRows %in% ls())){
      warning('Unable to find file from which to load.')
      stop()
    }
    SubsetPossibleMatches <- PossibleMatchRows[!grepl("IDENT",PossibleMatchRows) & !grepl("-",PossibleMatches)]
    if(length(SubsetPossibleMatches==0)) {
      PossibleMatchRows <- paste0("IDENT",MatchEntry$IDENT)
      }
    else{ #PossibleMatchRows remains as-is
      MatchTargetData <- MatchTargetData[gsub("\\<[[:alnum:]]{2}[.]","",SubsetPossibleMatches),]
      if(VERBOSE) print("  Successfully used subset from file.")
    }
  }
  else   PossibleMatchRows <- paste0("IDENT",MatchEntry$IDENT)
  
  # check that data is present before proceeding -------------
  if(is.na(MatchEntry[1,MatchColOrigName]) | MatchEntry[1,MatchColOrigName] == "" | is.null(MatchEntry[1,MatchColOrigName])){
    if(VERBOSE) print(" No data in original field")
    if(!LoadFromFile){
      PossibleMatchRows <- c(PossibleMatchRows, "-")
    }
    if(SAVE) save(PossibleMatchRows, file=file.path(OutPath,paste0(MatchTarget,"-","IDENT",MatchEntry$ID,"-",MatchType,".RData")))
    if (VERBOSE) print("*****")
    return(PossibleMatchRows)
  }
  
  # limit possible match rows to state-only unless gage or dam (should not affect load from file)---------
  TargetStates      <- ifelse(MatchType %in% c("bin","road","route","stream"),
                              MatchEntry$STFIPS,
                              c(MatchEntry$STFIPS,unlist(ls.Adj.STFIPS[as.character(MatchEntry$STFIPS)])))
  MatchTargetData <- MatchTargetData[MatchTargetData$STFIPS %in% TargetStates,]
  # MatchTargetNames  <- rownames(MatchTargetData)
PossibleMatches <- ""
  # start with county matches (will skip if no counties present)
  for (j in CountyCols){
    if(VERBOSE) print(paste("Checking county with FIPS", MatchEntry[1,j]))
    MatchTargetsCounty   <- MatchTargetData[MatchTargetData$FIPS == as.integer(sub(MatchEntry$STFIPS,"",MatchEntry[,j])),]
    if(nrow(MatchTargetsCounty)==0){
      if(VERBOSE) print(" No bridges in county")
      next
    }
    PossibleMatchRows<- c(PossibleMatchRows, paste0("IDENT", MatchEntry$IDENT,"-FIPS",MatchEntry[,j]))
    PossibleMatches <- PerformMatch(MatchEntry, MatchTargetsCounty, MatchType, 
                                    maxStringDist = maxStringDist, capPossPct = capPossPct, capN = capN,
                                    VERBOSE = VERBOSE)
    
    if(grepl("-",PossibleMatches[1]) & VERBOSE)  print(" No county matches")
    
    # record matches
    PossibleMatchRows <- c(PossibleMatchRows, PossibleMatches)
  }

# if no matches from county (or no county), try state(s)
if (all(grepl("IDENT",PossibleMatchRows) | grepl("^-",PossibleMatchRows))){
  if(PossibleMatches[1]=="" | grepl("-",PossibleMatches[1])){
    if(VERBOSE) print(" No county-matches, checking state")
    PossibleMatchRows<- c(PossibleMatchRows, paste0("IDENT", MatchEntry$IDENT,"-STFIPS"))
    PossibleMatches <- PerformMatch(MatchEntry, MatchTargetData, MatchType, 
                                    maxStringDist = maxStringDist, capPossPct = capPossPct, capN = capN,
                                    VERBOSE = VERBOSE)
  }
  
  if((PossibleMatches[1]=="" | grepl("-",PossibleMatches[1])) & VERBOSE)  print(paste(" No matches in state", MatchEntry[1,"STATE_CODE"]))
  
  # record matches
  PossibleMatchRows <- c(PossibleMatchRows, PossibleMatches)
}
  

if (VERBOSE) print("*****")
if (all(grepl("IDENT",PossibleMatchRows) | grepl("^-",PossibleMatchRows)) & LoadFromFile == TRUE){
  # if had matches, but then the second match run was unsuccessful, return the original matches
  if (length(SubsetPossibleMatchRows>=1)) load(file.path(LoadPath,matchFile))
}
if(SAVE) save(PossibleMatchRows, file=file.path(OutPath,paste0(MatchTarget,"-","IDENT",MatchEntry$ID,"-",MatchType,".RData")))
return(PossibleMatchRows)
}
