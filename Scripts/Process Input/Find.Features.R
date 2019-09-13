Find.Features <- function(Data,
                          Features, # Types of features to be extracted and their associated field (column) name (e.g., CITY = "LOCATION_COL_NAME")
                          DATASET = "Fail",
                          VERBOSE = VERBOSE){
  # source(file.path("Scripts","Process Input","Feature.Detect.R"))
  # source(file.path("Scripts","Helper","str_clout.R"))
  # source(file.path("Scripts","Process Input","Find.State.Specific.R"))
  # source(file.path("Scripts","Process Input","Find.Aux.R"))

  ## Determine which types of features are to be detected
  Rows  <- rownames(Data)
  Feat.Ord <- c("COUNTY","CITY","BRIDGE","RAIL","LOCATION","ROUTE","ROAD","STREAM")
  Feat.Ord <- Feat.Ord[Feat.Ord %in% names(Features)]
  Features <- Features[Feat.Ord]
  FLAG  <- rep(FALSE, length(unique(Features)))
  names(FLAG) <- unique(Features)
  
  if(DATASET=="Fail"){
    n.cols.out <- c(COUNTY   = 3,
                    CITY     = 53,
                    BRIDGE   = 2,
                    RAIL     = 2,
                    LOCATION = 2,
                    ROAD     = 2,
                    ROUTE    = 2,
                    STREAM   = 2)[names(Features)]
    ls.cols.out <- sapply(names(Features), function(f)
      as.vector(sapply(1:n.cols.out[f], function(i) 
        paste(ls.Fail.Ont[["Feature"]][[f]], i, sep = "_"))))
      
  }
  else stop("NBI Feature Detection not currently supported.")
  
  ## Loop over features
  for(f in names(Features)){
    if(VERBOSE) print(paste("CHECKING FEATURE:",f,"--------------"))
    COL        <- Features[f]
    cols.out   <- ls.cols.out[[f]]
    names.out  <- unique(sub("_[[:digit:]]+","",cols.out))
    n.dup.cols <- n.cols.out[f]
    Data[,cols.out] <- NA_character_
   
    # State-specific special processing from ontologies
    Data <- Find.State.Specific(Data, f, COL, VERBOSE = VERBOSE)
    
    # Auxilliary phrases
    if(f %in% c("LOCATION", "STREAM")){ 
      Data[,c(COL,cols.out[
             grepl("AUX",cols.out)])] <- 
        Find.Aux(Data, 
                 f, 
                 COL, 
                 cols.out[grepl("AUX",cols.out)], 
                 n.dup.cols = sum(grepl("AUX",cols.out)),
                 DELETION = TRUE, VERBOSE = VERBOSE)
    }
    
    # Feature-specific checks: state-specific for county, city, and route
    # Features deleted unless first-pass searching for city/county
    Find.Fun <- switch(f,
                       COUNTY   = "Find.Localities",
                       CITY     = "Find.Localities",
                       LOCATION = "Find.Localities",
                       BRIDGE   = "Find.Bridges",
                       RAIL     = "Find.Rails",
                       ROUTE    = "Find.Routes",
                       ROAD     = "Find.Roads",
                       STREAM   = "Find.Streams")
    source(file.path("Scripts","Process Input", paste0(Find.Fun,".R")))
    args <- list(Data       = Data, 
                 Feature    = f,
                 col.in     = COL, 
                 cols.out   = cols.out, 
                 n.dup.cols = n.dup.cols,
                 DELETION   = !(f %in% c("CITY","COUNTY")),
                 VERBOSE    = VERBOSE)
    if(f!="LOCATION"){
      Data[,c(COL,cols.out)] <- do.call(Find.Fun, args, quote = TRUE)
      rm(Find.Fun)
    }
    else{
      if(!FLAG[COL]){
        print("Moving to feature deletion of already-detected place names:")
        FLAG[COL] <- TRUE # only need to do this once per each original field
        Data[,COL] <- Find.Localities(Data,"CITY",COL,COL,1, DELETION = TRUE, VERBOSE = VERBOSE)
        Data[,COL] <- Find.Localities(Data,"COUNTY",COL,COL,1, DELETION = TRUE, VERBOSE = VERBOSE)
      }
    }
  }
  # rm(Feature.Detect, envir = .GlobalEnv)
  return(Data) #---------
}
  