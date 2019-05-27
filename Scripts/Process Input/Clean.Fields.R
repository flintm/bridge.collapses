# Function to process NYSDOT Bridge Failure Database or NBI Database (e.g., fix mis-spellings)

Clean.Fields <- function(Data = NULL,     # Use if passing Data directly, otherwise NULL
                         FilePath = NULL, # Use if loading from file, otherwise NULL
                         DATA_SET,        # Supported options are "Fail" and "NBI"
                         FieldNames,        # FieldNames is a character of form c(MAT = "MAT_COL_NAME", TYPE = "TYPE_COL_NAME", etc.)
                         VERBOSE = FALSE){# Print intermediate messages to screen
                                     
  if(is.null(Data)){
    if(!is.null(FilePath)){
      x <- ls()
      load(FilePath)
      x <- ls()[!(ls() %in% x)]
      Data <- get(x)
      }
    else{
      stop('Error: path to Data to be preprocessed is invalid')
    }
  }
  
  
  # Correct place abbreviations and mis-spellings in location
  if("LOCATION" %in% names(FieldNames)){
    source(file.path("Scripts","Process Input","PreProcess.Location.R"))
    Data <- PreProcess.Location(Data, DATA_SET = DATA_SET, FieldNames = FieldNames, VERBOSE = VERBOSE)
    if(VERBOSE) print(paste("Finished cleaning location field for",DATA_SET,"data."))
  }
  
  # Correct abbreviations and mis-spellings in stream
  if("STREAM" %in% names(FieldNames)){
    source(file.path("Scripts","Process Input","PreProcess.Stream.R"))
    Data <- PreProcess.Stream(Data, DATA_SET = DATA_SET, FieldNames = FieldNames, VERBOSE = VERBOSE)
    if(VERBOSE) print(paste("Finished cleaning stream field for",DATA_SET,"data."))
  }
  
  # Standardize numeric fields
  if(any(c("BIN", "ROUTE") %in% names(FieldNames))){
    source(file.path("Scripts","Process Input","PreProcess.Numeric.R"))
    Data <- PreProcess.Numeric(Data, FieldNames = FieldNames, VERBOSE = VERBOSE)
    if(VERBOSE) print(paste("Finished numeric fields for",DATA_SET,"data."))
  }

  # Standardize structure material and type
  if(any(c("MAT", "TYPE") %in% names(FieldNames))){
    source(file.path("Scripts","Process Input","PreProcess.Structure.Fields.R"))
    Data <- PreProcess.Structure.Fields(Data, FieldNames = FieldNames, VERBOSE = VERBOSE)
    if(VERBOSE) print(paste("Finished cleaning structure material and type for",DATA_SET,"data."))
  }
  return(Data)
}