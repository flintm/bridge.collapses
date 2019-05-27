# Function to load MatchData or TargetData
# Currently compatible with NYSDOT Bridge Failure Database (2014)
# and US Data Atlas NBI data (2012)

Load.Format.Input.Data <- function(filePath,            # relative to .Rproj root
                                   DATA_SET,            # type of data controls steps, supports Fail and NBI
                                   REDUCE_SIZE = FALSE, # Fields eliminated to reduce size if true
                                   STFIPS = TRUE,       # Add extra fields for STFIPS/STATE, FIPS/COUNTY
                                   HEADER = TRUE,       # file has column headers
                                   VERBOSE = FALSE){      

  ## Checks and setup: modify for new data types -----------
  if(!(DATA_SET %in% c("Fail", "NBI"))) stop("Only Fail and NBI datasets supported.")
  
  fieldName <- ifelse(DATA_SET=="Fail",
                      "ls.Fail.Fields",
                      "ls.NBI.Fields")
  
  if(!ls(pattern = fieldName, envir = .GlobalEnv)==fieldName){
    stop(paste("Fields for DATA_SET",DATA_SET,"must be present in environment."))
  }

  ## Load data: modify for new formats --------------------
  extension <- unlist(strsplit(filePath,".",fixed = TRUE))
  extension <- tolower(extension[length(extension)])
  if(!(extension %in% c('rdata','csv','txt','tab','geojson','xlsx','xls'))){
    stop("File format not supported.")
  }
  
  # Load if RData
  if(extension=="rdata"){
    vars <- ls()
    load(filePath)
    new.vars <- ls()[!(ls() %in% vars)]
    new.vars <- new.vars[!(new.vars %in% c("vars","new.vars"))]
    if (length(new.vars)>1 | class(get(new.vars[1]))!="data.frame"){
      stop('Error: If using RData format, can only load one dataframe')
    }
    else{
      if (new.vars != "Data"){
        assign("Data",get(new.vars))
      }
    }
  }
  else{ # Read if not RData
    read.pkg  <- switch(extension,
                        csv     = NULL,
                        txt     = NULL,
                        tab     = NULL,
                        geojson = 'geojsonR',
                        xlsx    = 'xlsx',
                        xls     = 'XLconnect')
    
    read.func  <- switch(extension,
                         csv     = 'read.csv',
                         txt     = 'read.table',
                         tab     = 'read.table',
                         geojson = 'FROM_GeoJson',
                         xlsx    = 'read.xlsx',
                         xls     = 'readWorksheetFromFile')
    
    read.args <- switch(extension,
                        csv     = list(file = filePath,
                                       stringsAsFactors = TRUE,
                                       header = HEADER),
                        txt     = list(file = filePath,
                                       stringsAsFactors = TRUE,
                                       header = HEADER),
                        tab     = list(file = filePath,
                                       stringsAsFactors = TRUE,
                                       header = HEADER),
                        geojson = list(file = filePath),
                        xlsx    = list(file = filePath,
                                       sheet = 1),
                        xls     = list(file = filePath,
                                       colTypes = rep("character",2)))
    
    # check for package and read
    if(!is.null(read.pkg)){
      install <- !require(read.pkg)
      if(install) {
        install.packages(read.pkg)
        library(read.pkg)
      }
    }
    Data <- do.call(read.func, args = read.args)
  }
  if(VERBOSE) print("Finished loading file.")
 
  ## Define and check column names and formats --------------
  colLab       <- ifelse(DATA_SET=="Fail",
                         "colNames",
                         ifelse(extension %in% c("csv", "geojson"),
                                "colNamesF",
                                "colNamesI"))
  colsExpected <- get(fieldName, envir = .GlobalEnv)[[colLab]]
  colClasses   <- get(fieldName, envir = .GlobalEnv)[["colClasses"]]
  names(colClasses) <- colsExpected
  ontName <- ifelse(DATA_SET=="Fail",
                    "ls.Fail.Ont",
                    "ls.NBI.Ont")
  
  if(REDUCE_SIZE & !(ls(pattern = ontName, envir = .GlobalEnv)==ontName)){
    stop(paste("Ontology for DATA_SET",DATA_SET,"must be present in environment."))
  }
  if (!all(colnames(Data) %in% colsExpected)){
    warning('Column names loaded do not match expected--errors may occur.')
  }
  if(!all(colsExpected %in% colnames(Data))){
    warning('Expected columns are missing in loaded data--errors may occur.')
    colsExpected <- colsExpected[colsExpected %in% colnames(Data)]
    colClasses   <- colClasses[colsExpected]
  }
  # re-order in case different
  Data <- Data[,colsExpected]
  
  ## Renaming of columns with DATA_SET; delete unneccessary columns -----------
  Data       <- Data[,!is.na(colClasses)]
  colClasses <- colClasses[!is.na(colClasses)]
  colClassOrig <- sapply(names(colClasses), function(j) class(Data[,j]))

  if(REDUCE_SIZE){
    ontName <- ls(pattern = ontName, envir = .GlobalEnv)[1]
    ont <- get(ontName)
    colsToKeep <- unique(unlist(ont))
    colsToKeep <- colsToKeep[colsToKeep %in% colnames(Data)]
    Data <- Data[,colsToKeep]
    colClasses <- colClasses[colsToKeep]
  }
  if(VERBOSE) print("Finished processing column names.")
  ## Format columns and clean up as needed ----------------- 
  # Formats
  # C is character, no changes. c is character, lowercase. cnp is character, lowercase, no punctuation.
  # i is interger, n is numeric, n# is numeric multiplied by # (so must be divided by #).
  # f is factor (default), fi is integer factor, rf is re-factor (re-encode levels and labels).
  # l is logical, yn is if the logical is encoded as YES/NO in the input.
  # D is Date, mmyy is month-year converted to Date, y is year converted to Date.
  
  # Character, 'C' (and all others to change except logical): change to character
  # "^$|^ $" is for a string of just a space or nothing
  cols         <- names(colClasses)[!(colClasses %in% c('f','rf','l','i','n'))]
  Data[, cols] <- sapply(cols, function(col) as.character(Data[,col]))
  Data[, cols] <- sapply(cols, function(col) gsub("^$|^ $", NA,Data[,col]))
  
  # state federal id, 'stfips' and county, 'fips'
  cols  <- names(colClasses)[grepl('fips',colClasses)]
  colst <- names(colClasses)[colClasses=="stfips"][1] 
  for(col in cols){
    Data[,col] <- switch(colClasses[col],
                         'stfips' = switch(class(Data[,col]),
                                           'character' = str_pad(Data[,col], 2, side = "left", pad = "0"),
                                           'integer' = sprintf("%02d", Data[,col])),
                         'fips' = switch(class(Data[,col]),
                                         'character' = str_pad(Data[,col], 3, side = "left", pad = "0"),
                                         'integer' = sprintf("%03d", Data[,col] %% 1000)))
  }
  for(col in cols){ # now factor
    Data[, col] <- switch(colClasses[col],
                          'stfips' = factor(Data[,col],
                                            levels = df.States$STFIPS_C, 
                                            labels = df.States$STFIPS_C),
                          'fips' = factor(paste0(Data[,colst],Data[,col]),
                                          levels = df.Counties$FIPS_C, 
                                          labels = df.Counties$FIPS_C))
  }
  
  # factor, 'f'
  cols         <- names(colClasses)[colClasses=='f' & colClassOrig!='factor']
  for(col in cols){
    Data[, col] <- factor(Data[,col],
                             levels = order(unique(Data[,col])), 
                             labels = unique(Data[,col])[!is.na(order(unique(Data[,col])))])
  }
  
  # re-factor, 'rf'
  cols         <- names(colClasses)[colClasses=='rf']
  for(col in cols){
    Data[, col] <- factor(Data[,col],
                          levels = order(unique(Data[,col])), 
                          labels = unique(Data[,col])[!is.na(order(unique(Data[,col])))])
  }
  
  # integer factor, 'fi'
  cols         <- names(colClasses)[colClasses=='fi']
  for(col in cols){
    Data[, col] <- factor(as.integer(Data[,col]))
  }
  
  # character, 'c': lower-case
  cols         <- names(colClasses)[colClasses %in% c('c', 'cnp')]
  Data[, cols] <- sapply(cols, function(col) gsub("^$|^ $", NA,tolower(Data[,col])))
  
  # character, 'cnp': lower-case, no punctuation
  cols         <- names(colClasses)[colClasses %in% c('cnp', 'D')]
  Data[, cols] <- sapply(cols, function(col) gsub("[[:punct:]]"," ",Data[,col]))
  Data[, cols] <- sapply(cols, function(col) gsub("\\s+"," ",Data[,col]))
  Data[, cols] <- sapply(cols, function(col) trimws(Data[,col]))
  
  # integer, 'i'
  cols         <- names(colClasses)[colClasses=='i'& colClassOrig!='integer']
  Data[, cols] <- sapply(cols, function(col) as.integer(Data[,col]))
  
  # numeric, 'n', or variations
  cols         <- names(colClasses)[substr(colClasses,1,1)=='n' & colClassOrig!='numeric']
  Data[, cols] <- sapply(cols, function(col) as.numeric(Data[,col]))
  
  # numeric divided by 10, 'n10'
  cols         <- names(colClasses)[colClasses=='n10']
  Data[, cols] <- sapply(cols, function(col) 0.1*Data[,col])
  
  # numeric divided by 100, 'n100'
  cols         <- names(colClasses)[colClasses=='n100']
  Data[, cols] <- sapply(cols, function(col) 0.01*Data[,col])
  
  # numeric divided by 1000, 'n1000'
  cols         <- names(colClasses)[colClasses=='n1000']
  Data[, cols] <- sapply(cols, function(col) 0.001*Data[,col])
  
  # logical, 'l'
  cols         <- names(colClasses)[colClasses=='l' & colClassOrig!='logical']
  Data[, cols] <- sapply(cols, function(col) gsub("N",NA_character_,Data[,col]))
  Data[, cols] <- sapply(cols, function(col) as.logical(as.integer(Data[,col])))
  
  # yes-no logical, 'yn'
  cols         <- names(colClasses)[colClasses=='yn']
  Data[, cols] <- sapply(cols, function(col) as.logical(factor(Data[,col], levels = c("Y","N"), labels = c("TRUE","FALSE"))))
  
  # year Date, 'y'
  cols         <- names(colClasses)[colClasses=='y']
  Data[, cols] <- sapply(cols, function(col) as.Date(paste0(Data[,col],"-01-01"), format = "%Y-%m-%d"))
  for (col in cols){ # can't get display and format to work in an sapply
    Data[,col] <- as.Date(Data[,col], "1970-01-01", format = "%Y-%m-%d")
  }
  
  # monthyear Date, 'mmyy'
  cols         <- names(colClasses)[colClasses=='mmyy']
  Data[, cols] <- sapply(cols, function(col) as.Date(paste0("01",as.character(Data[,col])),format = "%d%m%y"))
  for (col in cols){ # can't get display and format to work in an sapply
    Data[,col] <- as.Date(Data[,col], "1970-01-01", format = "%Y-%m-%d")
  }
  
  # Date, 'D'
  cols         <- names(colClasses)[colClasses=='D' & class(colClassOrig)!="Date"]
  for (col in cols){
    check.format <-  try(as.Date(Data[,col]),silent = TRUE)
    if (attributes(check.format)$class == "Date"){
      Data[,col] <- as.Date(Data[,col])
      Data[,col] <- as.Date(Data[,col], "1970-01-01", format = "%Y-%m-%d")
    }
    else{
      if(VERBOSE) warning(paste('Warning:', col,'in ambiguous format--trying another format'))
      check.format <-  try(as.Date(Data[,col], format = "%d %b %Y"))
      if (attributes(check.format)$class == "Date"){
        Data[,col] <- as.Date(Data[,col], format = "%d %b %Y")
        Data[,col] <- as.Date(Data[,col], "1970-01-01", format = "%Y-%m-%d")
      }
      else{
        stop(paste('Error unable to determine format of:', col))
      }
    }
  }
  
  ## Add column for STFIPS/STATE_CODE --------
  if(STFIPS){
    if(DATA_SET == "Fail"){
      Data <- merge(Data,df.States[,c("STATE_CODE","STFIPS_C")],by="STATE_CODE")
      colnames(Data)[colnames(Data)=="STFIPS_C"]<- "STFIPS"
    }
    if(DATA_SET == "NBI"){
      Data$STFIPS_C <- Data[,colst]
      Data <- merge(Data,df.States[,c("STATE_CODE","STFIPS_C")],by="STFIPS_C")
      Data$STATE_CODE <- factor(Data$STATE_CODE,
                                levels = df.States$STATE_CODE, 
                                labels = df.States$STATE_CODE)
      colnames(Data)[colnames(Data)=="STFIPS_C"] <- "STFIPS"
    }
  }

  
  ## Return-------
  return(Data)
}