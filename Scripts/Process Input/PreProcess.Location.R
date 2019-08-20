# Sub-function to process NYSDOT Bridge Failure Database LOCATION fields

PreProcess.Location <- function(Data,             
                                DATA_SET,         # Main supported option is Fail
                                FieldNames,       # FieldNames is a character of form c(LOCATION = "LOCATION_COL_NAME", ROAD = "ROAD_COL_NAME")
                                VERBOSE = FALSE){ #
  
  if(!("LOCATION") %in% names(FieldNames)) stop("Location column name not properly specified.")
  if(!("ID") %in% names(FieldNames)) stop("Entry ID column not properly specified.")
  LOC  <- FieldNames["LOCATION"]
  ROAD <- FieldNames["ROAD"]
  Rows <- row.names(Data)
  for(col in c("LOC","ROAD")){
    if(!is.na(get(col))){
      Data$TEMP <- Data[,get(col)]
      
      # Basic cleanup, including symbols that will cause regex errors
      # get rid of all possessives ("stabler's road")
      Data$TEMP <- sub("\\'s\\>","s", Data$TEMP)
      punct <- c("'","&","*")
      Data$TEMP    <- str_squish(gsub("[\\&\\*]+"," ", gsub("'","",Data$TEMP)))
      
      # Dataset-specific corrections
      ls.Keys   <- get(paste0("ls.",sub("Data","",DATA_SET),".Keys"))
      if(length(ls.Keys)>0){
        ls.TEMP <- ls.Keys[sapply(1:length(ls.Keys), function(i) col %in% ls.Keys[[i]])]
        if(length(ls.TEMP)>0){
          if(any(names(ls.TEMP) %in% Data[,FieldNames["ID"]])){
            IDs    <- names(ls.TEMP)[names(ls.TEMP) %in% Data[,FieldNames["ID"]]]
            ls.TEMP <- ls.TEMP[IDs]
            rowsID <- Rows[sapply(IDs, function(i) which(Data[,FieldNames["ID"]]==i))]
            Data[rowsID,"TEMP"] <- sapply(1:length(ls.TEMP), 
                                          function(i) sub(ls.TEMP[[i]][1],ls.TEMP[[i]][2],Data[rowsID[i],"TEMP"]))
          }
        }
      }
      
      if(DATA_SET=="FAIL"){
        # Special, variable duplicated county abbreviations
        ls.GreenCounty <- sapply(df.States$STFIPS, function(s) df.Counties[df.Counties$STFIPS==s & 
                                                                             grepl("green",df.Counties$COUNTY_NAME,ignore.case = TRUE),]$COUNTY_NAME,
                                 USE.NAMES = TRUE, simplify = FALSE)
        ls.GreenCounty <- ls.GreenCounty[sapply(1:length(ls.GreenCounty), function(i) length(ls.GreenCounty[[i]])>0)]
        ls.GreenCounty <- sapply(names(ls.GreenCounty), function(i) tolower(ls.GreenCounty[[i]][order(nchar(ls.GreenCounty[[i]]),ls.GreenCounty[[i]], decreasing = TRUE)]),
                                 simplify = FALSE, USE.NAMES = TRUE)
        ls.WebCounty <- sapply(df.States$STFIPS, function(s) df.Counties[df.Counties$STFIPS==s & 
                                                                           grepl("web",df.Counties$COUNTY_NAME,ignore.case = TRUE),]$COUNTY_NAME,
                               USE.NAMES = TRUE, simplify = FALSE)
        ls.WebCounty <- ls.WebCounty[sapply(1:length(ls.WebCounty), function(i) length(ls.WebCounty[[i]])>0)]
        ls.WebCounty <- sapply(names(ls.WebCounty), function(i) tolower(ls.WebCounty[[i]][order(nchar(ls.WebCounty[[i]]),ls.WebCounty[[i]], decreasing = TRUE)]),
                               simplify = FALSE, USE.NAMES = TRUE)
        ls.FredCounty <- sapply(df.States$STFIPS, function(s) df.Counties[df.Counties$STFIPS==s & 
                                                                            grepl("fred",df.Counties$COUNTY_NAME,ignore.case = TRUE),]$COUNTY_NAME,
                                USE.NAMES = TRUE, simplify = FALSE)
        ls.FredCounty <- ls.FredCounty[sapply(1:length(ls.FredCounty), function(i) length(ls.FredCounty[[i]])>0)]
        ls.FredCounty <- sapply(names(ls.FredCounty), function(i) tolower(ls.FredCounty[[i]][order(nchar(ls.FredCounty[[i]]),ls.FredCounty[[i]], decreasing = TRUE)]),
                                simplify = FALSE, USE.NAMES = TRUE)
        
        # substitute special abbreviations in county names (Green/Greene/etc., Web/Webster/Webb/etc.)
        StatesWithCounty <- unique(c(names(ls.GreenCounty), names(ls.WebCounty), names(ls.FredCounty)))
        for(k in StatesWithCounty){
          Counties <- list(green = ls.GreenCounty[[k]], web = ls.WebCounty[[k]], fred =ls.FredCounty[[k]])
          Counties <- Counties[sapply(names(Counties), function(i) length(Counties[[i]])>0)]
          for(j in names(Counties)){
            rowsForState <- Rows[Data$STFIPS == k]
            key.index    <- grepl(paste0("\\<",j,"[[:alpha:]]?\\>[[:punct:]]?[[:space:]]?\\<co"), Data[rowsForState,"TEMP"],ignore.case = TRUE)
            match.keys   <- which(key.index)
            for(i in match.keys){ # has a match to a partial county name
              key.index  <- sapply(Counties[[j]], function(cn) grepl(paste0("\\<",cn,"[[:alpha:]]?\\>[[:punct:]]?[[:space:]]?\\<co"), 
                                                                     Data[rowsForState[i],"TEMP"],ignore.case = TRUE))
              no.match   <- which(!key.index)
              for(n in no.match){ # does not have a full county name
                if(length(no.match)==1){ # only one matching county, safe to fill in
                  Data[rowsForState[n],"TEMP"] <- sub(paste0("\\<",j,"[[:alpha:]]?\\>[[:punct:]]?[[:space:]]?\\<co[unty]?[[:punct:]]?"), 
                                                      paste(Counties[[j]][1],"county"),
                                                      Data[rowsForState[i],"TEMP"],
                                                      ignore.case = TRUE)
                }
                else{ # multiple possible counties, check for closest
                  warning(paste("had ambiguous abbreviated county name match in:",Data[rowsForState[i],"TEMP"]))
                }
              }
            }
          }
        }
        
        # other misspellings and abbreviations for counties
        for (j in c(1:length(ls.CountyKeys))){
          StatesWithCounty <- unique(df.Counties[apply(sapply(ls.CountyKeys[[j]],grepl,df.Counties$COUNTY_NAME,ignore.case=TRUE), MARGIN = 1, any),"STFIPS"])
          rowsForState <- Rows[Data$STFIPS %in% StatesWithCounty]
          key.index <- sapply(paste0("\\<",ls.CountyKeys[[j]][c(2:length(ls.CountyKeys[[j]]))],"\\>[[:punct:]]?"),grepl,Data[rowsForState,"TEMP"],ignore.case = TRUE)
          match.keys <- switch(as.character(length(dim(key.index))),
                               "2" = which(apply(key.index, MARGIN = 1, any)),
                               "1" = which(key.index))
          for (i in match.keys){  
            Data[rowsForState[i],"TEMP"] <- sub(paste0("\\<",ls.CountyKeys[[j]][which(key.index[i,])[1]+1],"\\>[[:punct:]]?[[:space:]]?\\<[county[:punct:]]{2,6}"),
                                                paste0(ls.CountyKeys[[j]][1]," county"),
                                                Data[rowsForState[i],"TEMP"],ignore.case = TRUE)
          }
        }
        
        # for places
        for (j in c(1:length(ls.PlaceKeys))){
          key.index <- sapply(paste("\\<",ls.PlaceKeys[[j]][c(2:length(ls.PlaceKeys[[j]]))],"\\>", sep = ""),grepl,Data$TEMP)
          match.keys <- switch(as.character(length(dim(key.index))),
                               "2" = which(apply(key.index, MARGIN = 1, any)),
                               "1" = which(key.index))
          for (i in match.keys){
            Data[i,"TEMP"] <- sub(paste("\\<",ls.PlaceKeys[[j]][which(key.index[i,])[1]+1],"\\>", sep = ""),ls.PlaceKeys[[j]][1],Data[i,"TEMP"])
          }
        }
        
        # for bridges--may occassionally make an erroneous identification of a river "branch" in the LOCATION field
        pattern <- "\\<br[[:punct:]]?\\>[[:space:]]?[[:punct:]]?[[:digit:]]?[[:punct:]]"
        key.index  <- grepl(pattern, Data$TEMP, ignore.case = T)
        match.keys <- which(key.index)
        Data[match.keys,"TEMP"] <- sapply(match.keys, function(i) sub("\\<br[\\.]?,","bridge,",Data[i,"TEMP"]))
        Data[match.keys,"TEMP"] <- sapply(match.keys, function(i) sub("\\<br\\>[\\.]?","bridge",Data[i,"TEMP"]))
      }
      
      # final cleanup for Fail or NBI data
      Data[,"TEMP"] <- str_squish(gsub("[[:blank:]]{1}[\\,]",",",Data[,"TEMP"]))
      colnames(Data)[colnames(Data)=="TEMP"] <- col
    }
  }
  return(Data)
}
