# Function to load NYSDOT Bridge Failure Database
# Copyright Madeleine Flint, 2016
# ODC-ODbl Open Database License, see http://opendatacommons.org/licenses/odbl/summary/

# NOTE: Assumes that file has header with column names
Load.Format.Failed.Bridges <- function(FailFilePath){
  
  # load failed bridge database
  extension <- unlist(strsplit(FailFilePath,".",fixed = TRUE))
  extension <- tolower(extension[length(extension)])
  if (extension == "csv"){
    df.Failed.Bridges <- read.csv(FailFilePath,
                                  stringsAsFactors = TRUE)
  }
  else{
    if (extension == "xls"){
      install <- !require(XLConnect)
      if(install) {
        install.packages("XLConnect")
        library(XLConnect)
        }
      df.Failed.Bridges <- readWorksheetFromFile(FailFilePath,
                                                 colTypes = rep("character",2))
    }
    else{
      if (extension == "xlsx"){
        install <- !require("xlsx")
        if(install){ 
          install.packages("xlsx")
          library(xlsx)
          }
        df.Failed.Bridges <- read.xlsx(FailFilePath,1, colClasses = rep("character",2))
      }
      else{
        if (extension == "tab"){
          df.Failed.Bridges <- read.table(FailFilePath, header = TRUE,
                                          stringsAsFactors = TRUE)
        }
        else{
          if (extension == "rdata"){
            vars <- ls()
            load(FailFilePath)
            new.vars <- ls()[!(ls() %in% vars) & ls()!="vars"]
            if (length(new.vars)>1 | class(get(new.vars[1]))!="data.frame"){
              warning('Error: If using RData for Failed Bridges, can only load one dataframe')
              stop()
            }
            else{
              if (new.vars != "df.Failed.Bridges"){
                assign("df.Failed.Bridges",get(new.vars))
              }
            }
          }
          else{
            warning('Error: Only csv, xlsx, xls, and RData file types supported')
            stop()
          }
        }
      }
    }
  }
  
  # check column names
  colsExpected <- c("ID", "BIN", "LOCATION", "FEAT_UND", "STATE_CODE", "TYPE", "MAT", "YR_BLT", "YR_FAIL", "DATE_FAIL", "FAIL_TYPE", "FAIL_CAUS", 
                    "FATAL", "INJURY", "SOURCE", "COMMENTS")
  if (!all(colnames(df.Failed.Bridges) %in% colsExpected)){
    warning('Error: Column names loaded do not match expected. Check that column names in input file are as follows:')
    print(colsExpected)
    stop()
  }
  
  # format columns and clean up as needed (based on 2014 FAIL_WIN format)
  colClasses <- c(ID         = "integer",
                  BIN        = "character",
                  LOCATION   = "character",
                  FEAT_UND   = "character",
                  STATE_CODE = "Character",
                  TYPE       = "character",
                  MAT        = "character",
                  YR_BLT     = "integer",
                  YR_FAIL    = "integer",
                  DATE_FAIL  = "Date",
                  FAIL_TYPE  = "factor",
                  FAIL_CAUS  = "character",
                  FATAL      = "integer",
                  INJURY     = "integer",
                  SOURCE     = "character",
                  COMMENTS   = "character")
  colsChangeClass <- colnames(df.Failed.Bridges)[sapply(colnames(df.Failed.Bridges), 
                                                        function(col) class(df.Failed.Bridges[,col])!=colClasses[col])]
  df.Failed.Bridges[, colsChangeClass] <-sapply(colsChangeClass,
                                                function(col) as.character(df.Failed.Bridges[,col]))
  df.Failed.Bridges[, colsChangeClass[colClasses[colsChangeClass]=="character"]] <- 
    sapply(colsChangeClass[colClasses[colsChangeClass]=="character"], function(col) tolower(df.Failed.Bridges[,col]))
  df.Failed.Bridges[, colsChangeClass[colClasses[colsChangeClass]=="integer"]] <- 
    sapply(colsChangeClass[colClasses[colsChangeClass]=="integer"], function(col) as.integer(df.Failed.Bridges[,col]))

  df.Failed.Bridges[, colsChangeClass[colClasses[colsChangeClass]=="Date"]] <- 
            sapply(colsChangeClass[colClasses[colsChangeClass]=="Date"], function(col) gsub("^$|^ $", NA,gsub("\\s+", " ",gsub("[[:punct:]]"," ",df.Failed.Bridges[,col]))))
 

  
  df.Failed.Bridges[,names(colClasses)[colClasses=="character"]] <- as.data.frame(apply(df.Failed.Bridges[,names(colClasses)[colClasses=="character"]], 
                                                                                        2, 
                                                                                        function(x) gsub("^$|^ $", NA, x)),
                                                                                  stringsAsFactors = FALSE)
  df.Failed.Bridges[,c("TYPE","MAT","FAIL_CAUS","SOURCE")] <- sapply(c("TYPE","MAT","FAIL_CAUS","SOURCE"), function(col) gsub("[[:punct:]]"," ",df.Failed.Bridges[,col]))
  df.Failed.Bridges[,c("TYPE","MAT","FAIL_CAUS","SOURCE")] <- sapply(c("TYPE","MAT","FAIL_CAUS","SOURCE"), function(col) gsub("\\s+", " ",df.Failed.Bridges[,col]))
  
  df.Failed.Bridges$SOURCE <- factor(df.Failed.Bridges$SOURCE, levels = unique(df.Failed.Bridges$SOURCE), labels =  unique(df.Failed.Bridges$SOURCE)[!is.na(unique(df.Failed.Bridges$SOURCE))])
  df.Failed.Bridges$STATE_CODE <- factor(df.Failed.Bridges$STATE_CODE, levels = unique(df.Failed.Bridges$STATE_CODE), labels = unique(df.Failed.Bridges$STATE_CODE))
  
  check.format <-  try(as.Date(df.Failed.Bridges$DATE_FAIL),silent = TRUE)
  if (attributes(check.format)$class == "Date"){
    df.Failed.Bridges$DATE_FAIL <- as.Date(df.Failed.Bridges$DATE_FAIL)
  }
  else{
    warning('Warning: DATE_FAIL in ambiguous format--trying another format')
    check.format <-  try(as.Date(df.Failed.Bridges$DATE_FAIL, format = "%d %b %Y"))
    if (attributes(check.format)$class == "Date"){
      df.Failed.Bridges$DATE_FAIL <- as.Date(df.Failed.Bridges$DATE_FAIL, format = "%d %b %Y")
      df.Failed.Bridges$DATE_FAIL <- as.Date(df.Failed.Bridges$DATE_FAIL, "1970-01-01", format = "%Y-%m-%d")
    }
    else{
      warning('Error: Cannot determine format of DATE_FAIL column')
      stop()
    }
  }
  
  return(df.Failed.Bridges)
}