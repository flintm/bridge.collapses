MakeBridgeLabel <- function(BridgesDataFrame, MAIN_FIELDS = c("Build","Fail","State"), SUPER_FIELDS = NA, SEP = "-", PLOT_MATH = FALSE, PAD_RIGHT = FALSE, PAD_LEFT = FALSE){
  
  labs <- character(nrow(BridgesDataFrame))
  sep  <- ifelse(PLOT_MATH, paste("~",SEP,"~",sep=""), paste(" ",SEP," ",sep=""))
  BridgesDataFrame$AREA_RATIO_NHD <- BridgesDataFrame$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM/BridgesDataFrame$DRAIN_SQKM
  for (FIELD in MAIN_FIELDS){
     addLab <- switch(FIELD,
                      "Build" = sapply(as.character(as.Date(BridgesDataFrame$YR_BLT_EST,"1970-01-01")), function(i) ifelse(!is.na(i),substr(i,1,4), "N.A.")),
                      "Fail"  = substr(as.character(as.Date(BridgesDataFrame$YR_FAIL,"1970-01-01")),1,4),
                      "State" = sapply(1:nrow(BridgesDataFrame), function(i) df.States[df.States$STFIPS==BridgesDataFrame[i,"STFIPS"],"STATE_CODE"]),
                      "Hurr"  = sapply(1:nrow(BridgesDataFrame), function(i) ifelse(BridgesDataFrame[i,"BOOL_WAS_HURRICANE"] & !is.na(BridgesDataFrame[i,"BOOL_WAS_HURRICANE"]),"H","")),
                      "Area"  = sapply(1:nrow(BridgesDataFrame), function(i) ifelse(BridgesDataFrame[i,"AREA_RATIO_NHD"]>1.4,
                                                                                    "++",
                                                                                    ifelse(BridgesDataFrame[i,"AREA_RATIO_NHD"]>1.2,
                                                                                           "+",
                                                                                           ifelse(BridgesDataFrame[i,"AREA_RATIO_NHD"]<0.6,
                                                                                                  "- -",
                                                                                                  ifelse(BridgesDataFrame[i,"AREA_RATIO_NHD"]<0.8,
                                                                                                         "-",
                                                                                                         ""))))),
                      "Inter"    = factor(BridgesDataFrame$ITEM5B=="1", levels = c(TRUE,FALSE), labels = c("I","")),
                      "FailDate" = factor(BridgesDataFrame$BOOL_KNOWN_FAIL_DATE, levels = c("KNOWN","UNKNOWN"), labels = c("","*")),
                      "STAID"    = paste("STAID",BridgesDataFrame$STAID),
                      "GageArea" = paste(round(BridgesDataFrame$DRAIN_SQKM),"km2",sep=""),
                      "Cause"    = BridgesDataFrame$FAIL_CAUS_CODE)
    labs <- paste(labs, addLab, sep = sep)
  }
  labs <- sapply(labs, function(i) substr(i,nchar(sep)+1,nchar(i)))
  
  super <- ""
  for (FIELD in SUPER_FIELDS){
    addLab <- switch(FIELD,
                     "Build" = sapply(as.character(as.Date(BridgesDataFrame$YR_BLT_EST,"1970-01-01")), function(i) ifelse(!is.na(i),substr(i,1,4), "N.A.")),
                     "Fail"  = substr(as.character(as.Date(BridgesDataFrame$YR_FAIL,"1970-01-01")),1,4),
                     "State" = sapply(1:nrow(BridgesDataFrame), function(i) df.States[df.States$STFIPS==BridgesDataFrame[i,"STFIPS"],"STATE_CODE"]),
                     "Hurr"  = sapply(1:nrow(BridgesDataFrame), function(i) ifelse(BridgesDataFrame[i,"BOOL_WAS_HURRICANE"] & !is.na(BridgesDataFrame[i,"BOOL_WAS_HURRICANE"]),"H","")),
                     "Area"  = sapply(1:nrow(BridgesDataFrame), function(i) ifelse(BridgesDataFrame[i,"AREA_RATIO_NHD"]>1.4,
                                                                                   "++",
                                                                                   ifelse(BridgesDataFrame[i,"AREA_RATIO_NHD"]>1.2,
                                                                                          "+",
                                                                                          ifelse(BridgesDataFrame[i,"AREA_RATIO_NHD"]<0.6,
                                                                                                 "- -",
                                                                                                 ifelse(BridgesDataFrame[i,"AREA_RATIO_NHD"]<0.8,
                                                                                                        "-",
                                                                                                        ""))))),
                     "Inter" = factor(BridgesDataFrame$ITEM5B=="1",levels = c(TRUE,FALSE), labels = c("I","")),
                     "FailDate" = factor(BridgesDataFrame$BOOL_KNOWN_FAIL_DATE, levels = c("KNOWN","UNKNOWN"), labels = c("","*")),
                     "STAID" = BridgesDataFrame$STAID)
    super <- paste(super, addLab, sep = "")
  }

  if(PAD_RIGHT){
    super[super==""] <- " "
    padLength <- max(nchar(super))
    needPad   <- (1:length(super))[nchar(super)<padLength]
    super[needPad] <- sapply(needPad, function(i) paste0(super[i],paste0(rep("  ",padLength-nchar(super[i])),collapse="")))
    hasH <- grep("H",super)
    super[hasH] <- sub("  ","",super[hasH])
  }
  
  
  if (PLOT_MATH==FALSE) {
    labs[super!=""] <- paste(labs[super!=""],"${}^{",super[super!=""],"}$",sep="")
    if (PAD_LEFT) labs[grepl("N.A.",labs)] <- sub("N.A."," N.A.",labs[grepl("N.A.",labs)])
    }  
  else {
    # labs[super!=""] <- paste(labs[super!=""],"^'",super[super!=""],"'",sep="")
    labs <- paste(labs,"^'",super,"'",sep="")
    if (PAD_LEFT) labs[grepl("N.A.",labs)] <- sub("N.A.","~N.A.",labs[grepl("N.A.",labs)])
    }
  return(labs)
}
