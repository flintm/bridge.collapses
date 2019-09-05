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
      
      # Basic cleanup, including symbols that will cause regex errors and other
      # problems down the line
      # get rid of all possessives ("stabler's road")
      Data$TEMP <- sub("\\'s\\>","s", Data$TEMP)
      Data$TEMP <- str_squish(gsub("[\\&\\*]+"," ", gsub("'","",Data$TEMP)))
      Data$TEMP <- sub("co. rd.","co rd", Data$TEMP, fixed = TRUE)
      Data$TEMP <- sub("co.rd.","co rd", Data$TEMP, fixed = TRUE)
      
      # Dataset-specific corrections
      if(VERBOSE) print("Implementing dataset-specific corrections for location.")
      ls.Keys   <- get(paste0("ls.",sub("Data","",DATA_SET),".Keys"))
      if(length(ls.Keys)>0){
        ls.TEMP <- ls.Keys[sapply(1:length(ls.Keys), function(i) col %in% ls.Keys[[i]])]
        if(length(ls.TEMP)>0){
          if(any(names(ls.TEMP) %in% Data[,FieldNames["ID"]])){
            IDs    <- names(ls.TEMP)[names(ls.TEMP) %in% Data[,FieldNames["ID"]]]
            ls.TEMP <- ls.TEMP[IDs]
            Data[IDs,"TEMP"] <- sapply(IDs, 
                                     function(i) sub(ls.TEMP[[i]][1],
                                                     ls.TEMP[[i]][2],
                                                     Data[i,"TEMP"], 
                                                     fixed = TRUE))
          }
        }
      }
      
      if(DATA_SET=="Fail"){
        if(VERBOSE) print("Checking for ambiguous county abbreviations (green/greene, etc.)")
        # Special, variable duplicated county abbreviations
        ls.GreenCounty <- sapply(df.States$STFIPS_C, function(s) df.Counties[df.Counties$STFIPS_C==s & 
                                                                             grepl("green",df.Counties$COUNTY_NAME,ignore.case = TRUE),]$COUNTY_NAME,
                                 USE.NAMES = TRUE, simplify = FALSE)
        ls.GreenCounty <- ls.GreenCounty[sapply(1:length(ls.GreenCounty), function(i) length(ls.GreenCounty[[i]])>0)]
        ls.GreenCounty <- sapply(names(ls.GreenCounty), function(i) tolower(ls.GreenCounty[[i]][order(nchar(ls.GreenCounty[[i]]),ls.GreenCounty[[i]], decreasing = TRUE)]),
                                 simplify = FALSE, USE.NAMES = TRUE)
        ls.WebCounty <- sapply(df.States$STFIPS_C, function(s) df.Counties[df.Counties$STFIPS_C==s & 
                                                                           grepl("web",df.Counties$COUNTY_NAME,ignore.case = TRUE),]$COUNTY_NAME,
                               USE.NAMES = TRUE, simplify = FALSE)
        ls.WebCounty <- ls.WebCounty[sapply(1:length(ls.WebCounty), function(i) length(ls.WebCounty[[i]])>0)]
        ls.WebCounty <- sapply(names(ls.WebCounty), function(i) tolower(ls.WebCounty[[i]][order(nchar(ls.WebCounty[[i]]),ls.WebCounty[[i]], decreasing = TRUE)]),
                               simplify = FALSE, USE.NAMES = TRUE)
        ls.FredCounty <- sapply(df.States$STFIPS_C, function(s) df.Counties[df.Counties$STFIPS_C==s & 
                                                                            grepl("fred",df.Counties$COUNTY_NAME,ignore.case = TRUE),]$COUNTY_NAME,
                                USE.NAMES = TRUE, simplify = FALSE)
        ls.FredCounty <- ls.FredCounty[sapply(1:length(ls.FredCounty), function(i) length(ls.FredCounty[[i]])>0)]
        ls.FredCounty <- sapply(names(ls.FredCounty), function(i) tolower(ls.FredCounty[[i]][order(nchar(ls.FredCounty[[i]]),ls.FredCounty[[i]], decreasing = TRUE)]),
                                simplify = FALSE, USE.NAMES = TRUE)
        
        # substitute special abbreviations in county names (Green/Greene/etc., Web/Webster/Webb/etc.)
        StatesWithCounty <- unique(c(names(ls.GreenCounty), names(ls.WebCounty), names(ls.FredCounty)))
        for(k in StatesWithCounty){
          Counties <- list(green = ls.GreenCounty[[k]], web = ls.WebCounty[[k]], fred =ls.FredCounty[[k]])
          Counties <- Counties[sapply(names(Counties), function(i) length(Counties[[i]])>0 & !is.null(Counties[[i]][1]))]
          for(j in names(Counties)){
            rowsForState <- Rows[Data$STFIPS == k]
            key.index    <- grepl(paste0("\\<",j,"[[:alpha:]]?\\>[.,-]?[[:space:]]?\\<co"), Data[rowsForState,"TEMP"],ignore.case = TRUE)
            match.keys   <- which(key.index)
            for(i in match.keys){ # has a match to a partial county name
              key.index.2  <- sapply(Counties[[j]], function(cn) grepl(paste0("\\<",cn,"[[:alpha:]]?\\>[.,-]?[[:space:]]?\\<co"), 
                                                                     Data[rowsForState[i],"TEMP"],ignore.case = TRUE))
              
              no.match   <- which(!key.index.2) # does not have a full county name
              if(length(no.match)==1){ # only one matching county, safe to fill in
                Data[rowsForState[i],"TEMP"] <- sub(paste0("\\<",j,"[[:alpha:]]?\\>(([.,-]?[[:space:]]?co([unty.]){0,6})?)"), 
                                                    paste(Counties[[j]][1],"county"),
                                                    Data[rowsForState[i],"TEMP"])
              }
              else{ # multiple possible counties, check for closest
                warning(paste("had ambiguous abbreviated county name match in:",Data[rowsForState[i],"TEMP"]))
              }
            }
          }
        }
        
        # other misspellings and abbreviations for counties
        if(VERBOSE) print("Checking for county mispellings and abbreviations from dictionary.")
        for (j in c(1:length(ls.CountyKeys))){
          StatesWithCounty <- unique(df.Counties[grepl(sub("_"," ", names(ls.CountyKeys)[j], fixed = TRUE),
                                                       df.Counties$county),"STFIPS_C"])
          StatesWithCounty <- StatesWithCounty[StatesWithCounty %in% Data$STFIPS]
          for(state in StatesWithCounty){
            rowsForState <- Rows[Data$STFIPS %in% state]
            if(length(rowsForState)>0){
              key.index <- sapply(paste0("\\<",
                                         ls.CountyKeys[[j]][c(2:length(ls.CountyKeys[[j]]))],
                                         "\\>[.,-]?( co[unty.]{0,4}\\>)?"),
                                  grepl,
                                  Data[rowsForState,"TEMP"])
              if(sum(key.index)==0){
                key.index <- sapply(ls.CountyKeys[[j]][c(2:length(ls.CountyKeys[[j]]))],
                                    function(cn) grepl(cn, Data[rowsForState,"TEMP"], fixed = TRUE) &
                                      !grepl(ls.CountyKeys[[j]][1], Data[rowsForState,"TEMP"], fixed = TRUE) &
                                      grepl(paste0("\\<",cn), Data[rowsForState,"TEMP"]))
                useFix <- TRUE
              }
              else useFix <- FALSE
              dim(key.index) <- c(length(rowsForState), length(ls.CountyKeys[[j]])-1)
              match.keys <- switch(as.character(length(dim(key.index))),
                                   "2" = which(apply(key.index, MARGIN = 1, any)),
                                   "1" = which(key.index))
              for (i in match.keys){  
                pattern <- ifelse(useFix,
                                  ls.CountyKeys[[j]][which(key.index[i,])[1]+1],
                                  paste0("\\<",
                                         ls.CountyKeys[[j]][which(key.index[i,])[1]+1],
                                         "\\>"))
                str     <- regmatches(Data[rowsForState[i],"TEMP"], 
                                      regexpr(pattern, 
                                              Data[rowsForState[i],"TEMP"], fixed = useFix))
                if(grepl(paste0(str,"[.]? city"), Data[rowsForState[i],"TEMP"]) |
                   length(str)==0) next # not a county
                str.out <- ls.CountyKeys[[j]][1]
                if(grepl("\\<co[unty.]{0,5}\\>", Data[rowsForState[i],"TEMP"]) |
                   grepl(paste0(str,"co\\>"), Data[rowsForState[i],"TEMP"])){ 
                  str.out <- paste(str.out,
                                   tolower(df.Counties[df.Counties$STFIPS_C==state &
                                                         grepl(sub("_"," ", names(ls.CountyKeys)[j]),
                                                               df.Counties$county, 
                                                               ignore.case = TRUE) &
                                                         df.Counties$COUNTY_SUFFIX!="City","COUNTY_SUFFIX"]))
                }
                if(length(str.out)>1) str.out <- str.out[1]
                Data[rowsForState[i],"TEMP"] <- ifelse(useFix,
                                                       sub(str,
                                                           str.out,
                                                           Data[rowsForState[i],"TEMP"],
                                                           fixed = TRUE),
                                                       sub(paste0("(",str,"[,.-]?)(( co\\>)|( county\\>)|( co.))?"),
                                                           str.out,
                                                           Data[rowsForState[i],"TEMP"]))
              }
            }
          }
        }
        
        # for places
        if(VERBOSE) print("Checking for place mispellings and abbreviations from dictionary.")
        for (j in c(1:length(ls.PlaceKeys))){
              key.index <- sapply(paste0("\\<",
                                         ls.PlaceKeys[[j]][c(2:length(ls.PlaceKeys[[j]]))],
                                         "\\>[.]?( ci[ty.]{0,2}\\>)?"),
                                  grepl,
                                  Data$TEMP,ignore.case = TRUE)
              if(sum(key.index)==0){ 
                key.index <- sapply(ls.PlaceKeys[[j]][c(2:length(ls.PlaceKeys[[j]]))],
                                    function(cn) grepl(cn, Data$TEMP, fixed = TRUE) & #
                                      !grepl(ls.PlaceKeys[[j]][1], Data$TEMP, fixed = TRUE)) # rowsForState
                useFix <- TRUE
                }
              else useFix <- FALSE
              dim(key.index) <- c(nrow(Data), length(ls.PlaceKeys[[j]])-1)
              match.keys <- switch(as.character(length(dim(key.index))),
                                   "2" = which(apply(key.index, MARGIN = 1, any)),
                                   "1" = which(key.index))
              for (i in match.keys){
                pattern <- ifelse(useFix,
                                  ls.PlaceKeys[[j]][which(key.index[i,])[1]+1],
                                   paste0("\\<",
                                          ls.PlaceKeys[[j]][which(key.index[i,])[1]+1],
                                          "\\>"))
                str     <- regmatches(Data[i,"TEMP"], 
                                      regexpr(pattern, 
                                              Data[i,"TEMP"],fixed = useFix))
                if(grepl(paste0(str,"[.]? co"), 
                         Data[i,"TEMP"]) |
                        length(str)==0) next # no match or not a city
                str.out <- ls.PlaceKeys[[j]][1]
                Data[i,"TEMP"] <- ifelse(useFix,
                                                       sub(str,
                                                           str.out,
                                                           Data[i,"TEMP"],
                                                           fixed = TRUE),
                                                       sub(paste0("(",str,"[.]?)"),
                                                    str.out,
                                                    Data[i,"TEMP"]))
          }
        }
        
        # for bridges--may occassionally make an erroneous identification of a river "branch" in the LOCATION field
        pattern <- "\\<br\\>[.,\\( noum]{1,4}[[:digit:]]?"
        key.index  <- grepl(pattern, Data$TEMP, ignore.case = T)
        match.keys <- which(key.index)
        Data[match.keys,"TEMP"] <- sapply(match.keys, function(i) sub("\\<br\\>[.]?","bridge",Data[i,"TEMP"]))
      }
      
      # final cleanup for Fail or NBI data
      Data$TEMP <- gsub("(\\A[.,-]+)|([.,-]+\\Z)","",Data$TEMP, perl = TRUE)
      Data$TEMP <- str_squish(gsub("[[:blank:]]{1}[\\,]",",",Data$TEMP))
      
      colnames(Data)[colnames(Data)=="TEMP"] <- col
    }
  }
  return(Data)
}
