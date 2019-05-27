# Function to process NYSDOT Bridge Failure Database (e.g., fix mis-spellings)
# Copyright Madeleine Flint, 2016
# ODC-ODbl Open Database License, see http://opendatacommons.org/licenses/odbl/summary/

PreProcess.Failed.Bridges <- function(df.Fail, FailPath = NA){
  if(is.na(df.Fail)){
    if(!is.na(FailPath)) load(FailPath)
    else{
      warning('Error: path to df.Fail invalid')
      stop()
    }
  }
  
  # Identify material and type according to NBI
  source(file.path(dirsGit$Scripts,"Process Input","PreProcess.Mat.Type.R"))
  df.Fail <- PreProcess.Mat.Type(df.Fail)
  
  # Identify abbreviations and mis-spellings in LOCATION
  source(file.path(dirsGit$Scripts,"Process Input","PreProcess.Location.R"))
  df.Fail <- PreProcess.Location(df.Fail)
  
  # Identify abbreviations and mis-spellings in Stream
  source(file.path(dirsGit$Scripts,"Process Input","PreProcess.Location.R"))
  df.Fail <- PreProcess.Location(df.Fail)
  
  return(df.Fail)
}