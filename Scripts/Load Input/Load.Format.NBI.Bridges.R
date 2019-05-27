# Function to load NYSDOT Bridge Failure Database
# Copyright Madeleine Flint, 2016
# ODC-ODbl Open Database License, see http://opendatacommons.org/licenses/odbl/summary/

# NOTE: Strongly suggested that NBI data be loaded from US Data Atlas GIS files,
# which have had locations vetted and/or automatically generated where NBI data seemed
# erroneous
Load.Format.NBI.Bridges <- function(nbiFilePath){
  
  # load NBI database
  extension <- unlist(strsplit(nbiFilePath,".",fixed = TRUE))
  if (length(extension)==1) extension = "gis" # assume that file directory with GIS files was used.
  else extension <- tolower(extension[length(extension)])
  
  if (extension == "csv"){
    df.NBI.Bridges <- read.csv(nbiFilePath,
                                  stringsAsFactors = TRUE)
  }
  else{
    if (extension == "xls"){
      install <- !require(XLConnect)
      if(install) {
        install.packages("XLConnect")
        library(XLConnect)
      }
      df.NBI.Bridges <- readWorksheetFromFile(nbiFilePath,
                                                 colTypes = rep("character",2))
    }
    else{
      if (extension == "xlsx"){
        install <- !require("xlsx")
        if(install){ 
          install.packages("xlsx")
          library(xlsx)
        }
        df.NBI.Bridges <- read.xlsx(nbiFilePath,1, colClasses = rep("character",2))
      }
      else{
        if (extension == "tab"){
          df.NBI.Bridges <- read.table(nbiFilePath, header = TRUE,
                                          stringsAsFactors = TRUE)
        }
        else{
          if (extension == "rdata"){
            vars <- ls()
            load(nbiFilePath)
            new.vars <- ls()[!(ls() %in% vars) & ls()!="vars"]
            if (length(new.vars)>1 | class(new.vars)!="data.frame"){
              warning('If using RData for NBI Bridges, can only load one dataframe')
              stop()
            }
            else{
              if (new.vars != "df.NBI.Bridges"){
                assign(df.NBI.Bridges,new.vars)
              }
            }
          }
          else{
            if(extension = "gis"){
              install <- !require("rgdal")
              if(install){ 
                install.packages("grdal")
                library(rgdal)
                ds.NBI.bridges <- readOGR(dsn=nbiFilePath,layer="nbi")
                df.NBI.bridges <- as(ds.NBI.bridges,"data.frame")
                rm()
              }
            }
            else{
              warning('Only GIS, csv, xlsx, xls, and RData file types supported')
              stop()
            }
          }
        }
      }
    }
  }
  
  # format columns
  colNames <- c('STFIPS',   'REGION',   'ITEM8',    'ITEM5A',  'ITEM5B',  'ITEM5C',  'ITEM5D',    'ITEM5E',  'ITEM2',  #1
                'ITEM3',    'ITEM4',    'ITEM6A',   'ITEM6B',  'ITEM7',   'ITEM9',   'ITEM10',    'ITEM11',  'ITEM12', #2
                'ITEM13A',  'ITEM13B',  'ITEM16',   'ITEM17',  'ITEM19',  'ITEM20',  'ITEM21',    'ITEM22',  'ITEM26', #3
                'ITEM27',   'ITEM28A',  'ITEM28B',  'ITEM29',  'ITEM30',  'ITEM31',  'ITEM32',    'ITEM33',  'ITEM34', #4
                'ITEM35',   'ITEM36A',  'ITEM36B',  'ITEM36C', 'ITEM36D', 'ITEM37',  'ITEM38',    'ITEM39',  'ITEM40', #5
                'ITEM41',   'ITEM42A',  'ITEM42B',  'ITEM43A', 'ITEM43B', 'ITEM44A', 'ITEM44B',   'ITEM45',  'ITEM46', #6
                'ITEM47',   'ITEM48',   'ITEM49',   'ITEM50A', 'ITEM50B', 'ITEM51',  'ITEM52',    'ITEM53',  'ITEM54A',#7
                'ITEM54B',  'ITEM55A',  'ITEM55B',  'ITEM56',  'ITEM58',  'ITEM59',  'ITEM60',    'ITEM61',  'ITEM62', #8
                'ITEM63',   'ITEM64',   'ITEM65',   'ITEM66',  'ITEM67',  'ITEM68',  'ITEM69',    'ITEM70',  'ITEM71', #9
                'ITEM72',   'ITEM75A',  'ITEM75B',  'ITEM76',  'ITEM90',  'ITEM91',  'ITEM92A',   'ITEM92B', 'ITEM92C',#10
                'ITEM93A',  'ITEM93B',  'ITEM93C',  'ITEM94',  'ITEM95',  'ITEM96',  'ITEM97',    'ITEM98A', 'ITEM98B',#11
                'ITEM99',   'ITEM100',  'ITEM101',  'ITEM102', 'ITEM103', 'ITEM104', 'ITEM105',   'ITEM106', 'ITEM107',#12
                'ITEM108A', 'ITEM108B', 'ITEM108C', 'ITEM109', 'ITEM110', 'ITEM111', 'ITEM112',   'ITEM113', 'ITEM114',#13
                'ITEM115',  'ITEM116',  'FUNDED',   'FEDERAL', 'WO',      'DT',      'WO_2',      'STAT',    'SR1',    #14
                'SR2',      'EXTRA',    'STATUS',   'DATE',    'LONGDD',  'LATDD',   'coords.x1', 'coords.x2')         #15
                
  colClasses <- c('f',      'f',        'c',        'f',       'f',       'f',       'c',          'f',       'f',     #1
                  'f',      'f',        'c',        NA,        'c',       'c',       'n',          'n',       'l',     #2
                  'c',      'c',        'n',        'n',       'i',       'f',       'f',          'f',       'f',     #3
                  'y',      'i',        'i',        'i',       'y',       'f',       'n',          'f',       'i',     #4
                  'l',      'l',        'l',        'l',       'l',       'f',       'l',          'n',       'n',     #5
                  'f',      'f',        'f',        'f',       'f',       'f',       'f',          'i',       'i',     #6
                  'n',      'n',        'n',        'n',       'n',       'n',       'n',          'n',       'f',     #7
                  'n',      'f',        'n',        'n',       'f',       'f',       'f',          'f',       'f',     #8
                  'f',      'n',        'f',        'n',       'f',       'f',       'f',          'f',       'f',     #9
                  'f',      'f',        'f',        'n',       'mmyy',    'i',       'c',          'c',       'c',     #10
                  'mmyy',   'mmyy',     'mmyy',     'i',       'i',       'i',       'y',          'c',       'n',     #11
                  'c',      'f',        'f',        'f',       'f',       'l',       'f',          'y',       'f',     #12
                  'f',      'f',        'f',        'n',       'l',       'f',       'yn',         'f',       'n',     #13
                  'y',      'n',        'yn',       'yn',      'f',       'f',       'f',          'f',       'f',     #14
                  'f',      'f',        'f',        'mmyy',    'n',       'n',       NA,           NA)                 #15
  names(colClasses) <- colNames
  if (!all(colnames(df.NBI.bridges) %in% colNames)){
    warning('Column names loaded do not match expected. Check that column names in input file are as follows:')
    print(colNames)
    print('Note that not all column names must be present')
    stop()
  }
  # remove NA columns
  cols <- colNames[colNames %in% colnames(df.NBI.bridges)]
  classes <- colClasses[cols]
  df.NBI.bridges <- df.NBI.bridges[,!is.na(classes[colnames(df.NBI.bridges)])]
  cols <- colNames[colNames %in% colnames(df.NBI.bridges)]
  classes <- colClasses[cols]
  
  # integers
  i <- names(classes[classes == 'i'])
  df.NBI.bridges[,i] <- sapply(i, function(i) as.integer(as.character(df.NBI.bridges[,i])))
  
  # numerics
  i <- names(classes[classes == 'n'])
  df.NBI.bridges[,i] <- sapply(i, function(i) as.numeric(as.character(df.NBI.bridges[,i])))
  df.NBI.bridges[,"ITEM10"] <- df.NBI.bridges[,"ITEM10"]/100 # vertical clearance in XX.XXm
  df.NBI.bridges[,"ITEM11"] <- df.NBI.bridges[,"ITEM11"]/1000 # kilipoint XXXX.XXXkm
  df.NBI.bridges[,"ITEM16"] <- as.numeric(substr(df.NBI.bridges[,"ITEM16"],1,2)) + 
                               as.numeric(substr(df.NBI.bridges[,"ITEM16"],3,4))/60 +
                               as.numeric(substr(df.NBI.bridges[,"ITEM16"],5,8))/100/3600 # latitude ddmmss.ss
  df.NBI.bridges[,"ITEM17"] <- as.numeric(substr(df.NBI.bridges[,"ITEM17"],1,3)) + 
    as.numeric(substr(df.NBI.bridges[,"ITEM17"],4,5))/60 +
    as.numeric(substr(df.NBI.bridges[,"ITEM17"],6,9))/100/3600 # longitude dddmmss.ss
  df.NBI.bridges[,"ITEM39"] <- df.NBI.bridges[,"ITEM39"]/10 # nav. vertical clearance in XXX.Xm
  df.NBI.bridges[,"ITEM40"] <- df.NBI.bridges[,"ITEM40"]/10 # nav. horiz. clearance in XXXX.Xm
  df.NBI.bridges[,"ITEM47"] <- df.NBI.bridges[,"ITEM47"]/10 # inventory route total horiz. clearance XX.Xm
  df.NBI.bridges[,"ITEM48"] <- df.NBI.bridges[,"ITEM48"]/10 # max span in XXXX.Xm
  df.NBI.bridges[,"ITEM49"] <- df.NBI.bridges[,"ITEM48"]/10 # structure length in XXXXXX.Xm
  df.NBI.bridges[,"ITEM50A"] <- df.NBI.bridges[,"ITEM50A"]/10 # sidewalk width XX.Xm
  df.NBI.bridges[,"ITEM50B"] <- df.NBI.bridges[,"ITEM50B"]/10 # sidewalk width XX.Xm
  df.NBI.bridges[,"ITEM51"] <- df.NBI.bridges[,"ITEM51"]/10 # roadway width curb-curb XXX.Xm
  df.NBI.bridges[,"ITEM52"] <- df.NBI.bridges[,"ITEM52"]/10 # deck width out-out XXX.Xm
  df.NBI.bridges[,"ITEM53"] <- df.NBI.bridges[,"ITEM53"]/100 # min. vert. clearance over XX.XXm
  df.NBI.bridges[,"ITEM55B"] <- df.NBI.bridges[,"ITEM55B"]/10 # min lateral underclearance XX.Xm
  df.NBI.bridges[,"ITEM56"] <- df.NBI.bridges[,"ITEM56"]/10 # min lateral underclearance XX.Xm
  df.NBI.bridges[,"ITEM64"] <- df.NBI.bridges[,"ITEM64"]/10 # operating rating, XX.Xmt
  df.NBI.bridges[,"ITEM66"] <- df.NBI.bridges[,"ITEM66"]/10 # inventory rating, XX.Xmt
  df.NBI.bridges[,"ITEM76"] <- df.NBI.bridges[,"ITEM76"]/10 # length of structure improve, XXXXX.Xm
  df.NBI.bridges[,"ITEM76"] <- df.NBI.bridges[,"ITEM76"]/10 # length of structure improve, XXXXX.Xm
  df.NBI.bridges[,"ITEM109"] <- df.NBI.bridges[,"ITEM109"]/100 # ADT, %
  df.NBI.bridges[,"ITEM116"] <- df.NBI.bridges[,"ITEM116"]/10 # min nav. clearance vertical lift bridge, XXX.Xm
  
  # characters
  i <- names(classes[classes == 'c'])
  df.NBI.bridges[,i] <- sapply(i, function(i) as.character(df.NBI.bridges[,i]))
  
  # logical
  i <- names(classes[classes == 'l'])
  df.NBI.bridges[,i] <- sapply(i, function(i) as.logical(df.NBI.bridges[,i]))
  
  # yes-no logical
  i <- names(classes[classes == 'yn'])
  df.NBI.bridges[,i] <- sapply(i, function(i) as.logical(factor(as.character(df.NBI.bridges[,i]),levels = c("Y","N"), labels = c("TRUE","FALSE"))))
  
  # year
  i <- names(classes[classes == 'y'])
  df.NBI.bridges[,i] <- sapply(i, function(i) as.Date(paste(as.character(df.NBI.bridges[,i]),"01-01",sep="-")))
  for (ii in i){
    df.NBI.bridges[,ii] <- as.Date(df.NBI.bridges[,ii], "1970-01-01", format = "%Y-%m-%d")
  }
  
  # mmyy
  i <- names(classes[classes == 'mmyy'])
  df.NBI.bridges[,i] <- sapply(i, function(i) as.Date(paste0("01",as.character(df.NBI.bridges[,i])),format = "%d%m%y"))
  for (ii in i){
    df.NBI.bridges[,ii] <- as.Date(df.NBI.bridges[,ii], "1970-01-01", format = "%Y-%m-%d")
  }
 return(df.NBI.bridges)
}