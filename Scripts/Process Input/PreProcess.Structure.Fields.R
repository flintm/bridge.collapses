# Sub-function to process structural data from NYSDOT Bridge Failure Database
# (MAT and TYPE fields) and link to National Bridge Inventory Material (ITEM43A)
# and Type (ITEM43B) fields.

PreProcess.Structure.Fields <- function(Data,             # Supported option is NYSDOT Failure data
                                        FieldNames,         # FieldNames is a character of form c(MAT = "MAT_COL_NAME", TYPE = "TYPE_COL_NAME", etc.)
                                        VERBOSE = FALSE){ # 
  
  if(!all(c("MAT","TYPE") %in% names(FieldNames))) stop("Material and type column names not properly specified.")
  MAT  <- FieldNames["MAT"]
  TYPE <- FieldNames["TYPE"]
  
  # clean up and separate material and support type (simple or continuous)
  Data[,c("MATER","TYPE_STRUCT")] <- sapply(c(MAT,TYPE), function(j) str_squish(Data[,j]))
  cols <- c("SUPPORT","ITEM43A","ITEM43A_ALT","ITEM43B","ITEM43B_ALT")
  Data[,cols] <-  NA_character_

  # FIX ERRORS AND CROSS-LISTINGS----------
  # Dataset-specific corrections
  Rows  <- rownames(Data)
  DATA_TYPE <- names(DATA_SETS)[sapply(DATA_SETS,"[[",1)==DATA_SET]
  ls.Keys   <- get(paste0("ls.",sub("Data","",DATA_TYPE),"Keys"))
  
  ls.Mat <- ls.Keys[sapply(1:length(ls.Keys), function(i) "MAT" %in% ls.Keys[[i]])]
  if(length(ls.Mat)>0){
    rowsID <- Rows[sapply(names(ls.Mat), function(i) which(Data[Rows,FieldNames["ID"]]==i))]
    Data[rowsID,"MATER"] <- sapply(1:length(ls.Mat), 
                                          function(i) ifelse(!is.na(Data[rowsID[i],"MATER"]),
                                                             sub(ls.Mat[[i]][1],ls.Mat[[i]][2],Data[rowsID[i],"MATER"]),
                                                             ls.Mat[[i]][2]))
  }
  
  # first remove types from material and append to type column
  nonEmptyRows <- Rows[Data$MATER!="" & !is.na(Data$MATER)]
  for (j in 1:length(ls.TypeKeys)){
    key.index  <- sapply(paste0("\\<",ls.TypeKeys[[j]],"\\>"),grepl,Data[nonEmptyRows,"MATER"])
    match.keys <- which(apply(key.index, MARGIN = 1, any))
    for (i in match.keys){
      key <- ls.TypeKeys[[j]][min(which(key.index[i,]))]
      Data[nonEmptyRows[i],"MATER"] <- sub(paste0("\\<",key,"\\>"),"",Data[nonEmptyRows[i],"MATER"])
      # only paste into TYPE if TYPE does not already contain the same information
      if (is.na(Data[nonEmptyRows[i],"TYPE_STRUCT"]) | !any(sapply(ls.TypeKeys[[j]],grepl,Data[nonEmptyRows[i],"TYPE_STRUCT"]))){
        Data[nonEmptyRows[i],"TYPE_STRUCT"] <- paste(Data[nonEmptyRows[i],"TYPE_STRUCT"],key)
      }
    }
  }
  if(VERBOSE) print("Finished error-checking for Data MAT")
  
  # remove materials from types
  nonEmptyRows <- Rows[Data$TYPE_STRUCT!="" & !is.na(Data$TYPE_STRUCT)]
  for (j in 1:length(ls.MatKeys)){
    key.index  <- sapply(paste0("\\<",ls.MatKeys[[j]],"\\>"),grepl,Data[nonEmptyRows,"TYPE_STRUCT"])
    match.keys <- which(apply(key.index, MARGIN = 1, any))
    for (i in match.keys){
      key <- ls.MatKeys[[j]][min(which(key.index[i,]))]
      Data[nonEmptyRows[i],"TYPE_STRUCT"] <- sub(paste0("\\<",key,"\\>"),"",Data[nonEmptyRows[i],"TYPE_STRUCT"])
      # add to material if providing new or more precise information (e.g., about span being simple or continuous)
      if (is.na(Data[nonEmptyRows[i],"MATER"]) | !any(sapply(ls.MatKeys[[j]],grepl,Data[nonEmptyRows[i],"MATER"])) | key %in% as.character(1:6)){
        Data[nonEmptyRows[i],"MATER"] <- paste(key,Data[nonEmptyRows[i],"MATER"])
      }
    }
  }
  if(VERBOSE) print("Finished error-checking for Data TYPE")
  
  # IDENTIFY SUPPORT (SIMPLE OR CONTINUOUS)----------
  # remove support (simple or continuous) from types
  nonEmptyRows <- Rows[Data$TYPE_STRUCT!="" & !is.na(Data$TYPE_STRUCT)]
  for (j in 1:length(ls.SupKeys)){
    key.index <- sapply(paste0("\\<",ls.SupKeys[[j]],"\\>"),grepl,Data[nonEmptyRows,"TYPE_STRUCT"])
    match.keys <- which(apply(key.index, MARGIN = 1, any))
    for (i in match.keys){
      key <- ls.SupKeys[[j]][min(which(key.index[i,]))]
      Data[nonEmptyRows[i],"TYPE_STRUCT"] <- sub(paste0("\\<",key,"\\>"),"",Data[nonEmptyRows[i],"TYPE_STRUCT"])
      if (is.na(Data[nonEmptyRows[i],"MATER"]) | !any(sapply(ls.SupKeys[[j]],grepl,Data[i,"MATER"]))){
        Data[nonEmptyRows[i],"MATER"] <- paste(Data[nonEmptyRows[i],"MATER"],key)
      }
    }
  }
  if(VERBOSE) print("Finished identifying support condition for Data TYPE")
  
  # pull out support condition for rows where present
  nonEmptyRows <- Rows[Data$MATER!="" & !is.na(Data$MATER)]
  key.index <- sapply(paste0("\\<",unlist(ls.SupKeys),"\\>"),grepl,Data[nonEmptyRows,"MATER"])
  match.keys <- which(apply(key.index, MARGIN = 1, any))
  for (i in match.keys){
    key <- unlist(ls.SupKeys)[min(which(key.index[i,]))]
    Data[nonEmptyRows[i],"MATER"]   <- sub(paste0("\\<",key,"\\>"),"",Data[nonEmptyRows[i],"MATER"])
    Data[nonEmptyRows[i],"SUPPORT"] <- names(ls.SupKeys)[sapply(names(ls.SupKeys), function(i) key %in% ls.SupKeys[[i]])]
  }
  Data$MATER <- str_squish(Data$MATER)
  
  if(VERBOSE) print("Finished separation of TYPE, MATER, and SUPPORT")
  
  # ASSIGN TYPES BASED ON NBI CLASSIFICATION -------
  nonEmptyRows <- Rows[Data$TYPE_STRUCT!="" & !is.na(Data$TYPE_STRUCT)]
  for (j in 1:length(ls.TypeKeys)){
    key.index  <- sapply(paste0("\\<",ls.TypeKeys[[j]],"\\>"),grepl,Data[nonEmptyRows,"TYPE_STRUCT"])
    match.keys <- which(apply(key.index, MARGIN = 1, any))
    type <- names(ls.ITEM43B.Ont)[j]
    for (i in match.keys){
      col  <- ifelse(is.na(Data[nonEmptyRows[i],"ITEM43B"]),
                     "ITEM43B",
                     "ITEM43B_ALT")
      Data[nonEmptyRows[i],col] <- type
    }
  }
  # for truss and arch, ALT may be duplicative
  Data[grepl("_",Data$ITEM43B_ALT),"ITEM43B_ALT"] <- NA_character_
  if(VERBOSE) print("Finished assignment of ITEM43B")
  
  # ASSIGN MATERIAL BASED ON NBI CLASSIFICATION (INCLUDES SUPPORT INFORMTION)-------
  nonEmptyRows     <- Rows[Data$MATER!="" & !is.na(Data$MATER)]
  matsWithSupport  <- c("pc","conc","steel")
  for (j in names(ls.MatKeys)){
    key.index  <- sapply(paste0("\\<",ls.MatKeys[[j]],"\\>"),grepl,Data[nonEmptyRows,"MATER"])
    match.keys <- which(apply(key.index, MARGIN = 1, any))
    for (i in match.keys){
      col  <- ifelse(is.na(Data[nonEmptyRows[i],"ITEM43A"]),
                     "ITEM43A",
                     "ITEM43A_ALT")
      # first tackle those with support explicitly described in NBI and for rows where included
      if(j %in% matsWithSupport){
        if(!is.na(Data[nonEmptyRows[i],"SUPPORT"])){
          # use fact that materials with simple or continuous have simple listed as a lower number
          Data[nonEmptyRows[i],col] <- ifelse(any(grepl(Data[nonEmptyRows[i],"SUPPORT"],ls.SupKeys[["simple"]])),
                                                    ls.MatKeys[[j]][grepl("[[:digit:]]",ls.MatKeys[[j]])][1], # simple 
                                                    ls.MatKeys[[j]][grepl("[[:digit:]]",ls.MatKeys[[j]])][2]) # must be continuous
        }
        else{ # no support condition listed for a support-needing NBI material type
          Data[nonEmptyRows[i],col]     <- paste(ls.MatKeys[[j]][grepl("[[:digit:]]",ls.MatKeys[[j]])],collapse = "_")
        }
      }
      else{ # must be wood, metal, masonry or other (i.e., no support needed)
        Data[nonEmptyRows[i],col] <- ls.MatKeys[[j]][grepl("[[:digit:]]",ls.MatKeys[[j]])]
      }
    }
    
  }
  if(VERBOSE) print("Finished assignment of ITEM43A")
  
  # Factor standard columns
  Data[,cols[2:5]] <- sapply(cols[2:5], function(j) factor(Data[,j]))
  return(Data)
}
