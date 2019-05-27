write.mod.watstore <- function(dates, # dates or years of flow data
                           Q, # regular peaks
                           QINT = NA, # any interval as dataframe 
                           STAID, # to lookup correct header info
                           GENSKEW, #SKEW STATION
                           SKEWSD, #SKEW STANDARD DEVIATION 
                           GAGEBASE, #GAGE BASE
                           PPALPHA, 
                           LOTHRESH, #LOW OUTLIERS TRESHHOLD
                           STANAME_APPEND = "", # append to stationname, e.g., "VIC data"
                           file.out="",
                           folder.out=getwd()){
  
  #LAST UPDATED ON 10-23-2018
  
  #SCRIPT THAT TAKES IN THE FLOOD DATA FROM STREAM GAUGES, FORMATS AND OUTPUTS A .SPC FILE FOR THE USE WITH PEAKFQSA TO PERFORM A BULLETIN 17C ANALYSIS.
  
  # REQUIRED INPUT 
  # dates (Dates of Peaks) is a [nDate x 1] of class Date or character (e.g. dates<-seq(as.Date("1910/1/1"), as.Date("1999/1/1"), "years"))
  # Q (FLow) must be as a [nDate x 1] of class numeric or integer vector (e.g. flow<-(4000:4500))
  # QINT (FLow Interval), if present, must be a data.frame of [nInt x 3], with columns [year min-thresh max-thresh]
  # STAID (Station Identification) is a character [1x1] (Station ID number for USGS gauge) used to obtain header information 
  #GENSKEW (General Skew) is [1x1] of numeric or integer class 
  #SKEWSD (Standard Deviation for Flood Skew) is [1x1] of numeric or integer class 
  #GAGESABE (Gage Base) is [1x1] of numeric or integer class
  #PPALPHA is [1x1] of numeric or integer class
  #LOTHRESH (Low outlier tHreshold) is [1x1] of numeric or integer class
  
  
  # OPTIONAL INPUT
  # STANAME_APPEND is a character [1x1] with additional text to append to the STAID in naming the output file
  #       e.g., "VICG_RAPID" or "FROM_DAILY_MEANS""-
  # file.out is a character [1x1] that can be used to produce a standard end of the output file name, e.g. "peaks.psf"
  # folder.out is a character [1x1] filepath for where the output file should be saved
  
  # OUTPUT
  # A .spc file for use with the USACE distribution of PeakfqSA, Bulletin 17C flood frequency analysis
  
  if (length(dates)!=length(Q)){
    stop("Length of dates and peaks must match", immediate. = TRUE)
  }
  require(gdata) 
  require(stringr)
 if (("importPKFs" %in% ls())) source(file.path(dirsGit$Scripts,"Load Input","importPKFs.R"))
#   "agency_cd	site_no	peak_yr	peak_month	peak_day	peak_tm	peak_va	peak_cd	gage_ht	gage_ht_cd	year_last_pk	ag_dt	ag_tm	ag_gage_ht	ag_gage_ht_cd"
#   "5s	15s	4n	2n	2n	6s	8s	27s	8s	13s	4s	10d	6s	8s	11s"
  # widthLimits <- c(0,1,16,20,22,24,31,43,51,55,59,75,80)
  # widths      <- widthLimits[2:length(widthLimits)] - widthLimits[1:(length(widthLimits)-1)]
  
#Format the dates and yields only the year   
  dates <- switch(class(dates),
                  "character" =  as.Date(dates),
                  "numeric"   =  as.Date(dates,"1970-01-01"),
                  "integer"   =  as.Date(dates,"1970-01-01"),
                  "Date"      =  dates)
  year  <- format.Date(dates,"%Y")

  Q <- signif(Q, digits = 6)
  Q <- round(Q)
  
 PKF.header <- importPKFs(STAID)[1:4,1]
  
 #Adds the required headings and information needed to run the analysis (station name, file name,skew option, skew values)
  out <- character()
  
  #STATION NAME
  STATIONNAME<-substr(PKF.header[3],16,nchar(PKF.header[3]))
  STATIONID <- str_pad(as.character(STATIONNAME), width = 37, side = "left", pad = " ")
  out[1] <- paste0("STATION",STATIONID,STANAME_APPEND,collapse = "")
  
  #FILENAME NECESSARY FOR PEAKFQSA TO READ AND PROCESS OUTPUT FILE
  FILENAME <- str_pad(as.character(STAID,STANAME_APPEND), width = 15, side = "left", pad = " ")
  out[2] <-paste0("I",FILENAME,"peaks.spc",collapse="")
  
  #STATION ID
  STATIONID <- str_pad(as.character("STATION"), width = 13, side = "left", pad = " ")
  out[3] <- paste0("SKEWOPT",STATIONID,STANAME_APPEND,collapse = "")
  
  #GENERAL SKEW
  GENSKEW <-sprintf("%.3f", round(GENSKEW,4))
  GENSKEW <- str_pad(as.character(GENSKEW), width = 11, side = "left", pad = " ")
  out[4] <- paste0("GENSKEW",GENSKEW,collapse = "")
  
  #SKEW STANDARD DEVIATION
  SKEWSD <-sprintf("%.3f", round(SKEWSD,4))
  SKEWSD <- str_pad(as.character(SKEWSD), width = 12, side = "left", pad = " ")
  out[5] <- paste0("SKEWSD",SKEWSD,collapse = "")
  
  #GAGEBASE
  GAGEBASE <-sprintf("%.3f", round(GAGEBASE,4))
  GAGEBASE <- str_pad(as.character(GAGEBASE), width = 10, side = "left", pad = " ")
  out[6] <- paste0("GAGEBASE",GAGEBASE,collapse = "")
  
  #SKEW OPTION
  SKEWOPT <- str_pad(as.character("EMA"), width = 4, side = "left", pad = " ")
  out[7] <- paste0("A_S_SKEW_OPT",SKEWOPT,collapse = "")
  
  #PP ALPHA
  PPALPHA <-sprintf("%.3f", round(PPALPHA,4))
  PPALPHA <- str_pad(as.character(PPALPHA), width = 10, side = "left", pad = " ")
  out[8] <- paste0("PP_ALPHA",PPALPHA,collapse = "")
  
  #LOW OUTLIER TYPE
  SKEWOPT <- str_pad(as.character("MGBT"), width = 9, side = "left", pad = " ")
  out[9] <- paste0("LOMETHOD",SKEWOPT,collapse = "")
  
  #LOW OUTLIER TRESHOLD 
  LOTHRESH <-sprintf("%.3f", round(LOTHRESH,4))
  LOTHRESH <- str_pad(as.character(LOTHRESH), width = 10, side = "left", pad = " ")
  out[10] <- paste0("LOTHRESH",LOTHRESH,collapse = "")
  
  #CSV
  CSV <- str_pad(as.character("YES"), width = 13, side = "left", pad = " ")
  out[11] <- paste0("CSV",CSV,collapse = "")
  
  #BEGINNING YEAR OF FLOOD DATA
  BEGYEAR <-min(year)
  BEGYEAR <- str_pad(as.character(BEGYEAR), width = 10, side = "left", pad = " ")
  out[12] <- paste0("BEGYEAR",BEGYEAR,collapse = "")
  
  #END YEAR OF FLOOD DATA
  ENDYEAR <-max(year)
  ENDYEAR <- str_pad(as.character(ENDYEAR), width = 10, side = "left", pad = " ")
  out[13] <- paste0("ENDYEAR",ENDYEAR,collapse = "")
  
  #THRESHOLD 
  #IT HAS BEEN CODED TO USE THE MIN YEAR AND THE MAX YEAR AS BOUNDARIES, FLOW OF "0" HAS BEEN HARDCODED IN LINE 124
  sf<-as.character("1.00E+010")
  zero<-sprintf("%.0f", round(0,1))
  miny <- str_pad(as.character(min(year)), width = 6, side = "left", pad = " ")
  maxy <- str_pad(as.character(max(year)), width = 7, side = "left", pad = " ")
  zero <- str_pad(as.character(zero), width = 5, side = "left", pad = " ")
  sf <- str_pad(as.character(sf), width = 11, side = "left", pad = " ")
  out[14] <- paste0("THRESHOLD",miny,maxy,zero,sf,sep="\t",collapse = "")
  
  if(class(QINT)=="data.frame"){
    colnames(QINT) <- c("year", "MIN", "MAX")
    QINT[,"year"] <- switch(class(QINT[,"year"]),
                    "character" = as.Date(QINT[,"year"], format = "%Y"),
                    "numeric"   = as.Date(QINT[,"year"],origin = "1970-01-01", format = "%Y"),
                    "integer"   = as.Date(QINT[,"year"], origin = "1970-01-01", format = "%Y"),
                    "Date"      = format.Date(QINT[,"year"], "%Y"))
    QINT$MIN <- str_pad(as.character(QINT[,"MIN"]), width = 7, side = "left", pad = " ")
    QINT$MAX <- str_pad(as.character(QINT[,"MAX"]), width = 11, side = "left", pad = " ")
    out.temp <- sapply(1:nrow(QINT),function(i)
                                  paste("QINT",QINT$year[i],QINT$MIN[i],QINT$MAX[i],sep="\t",collapse = ""))
    out <- c(out, out.temp)
  }
  Q <- as.character(Q)
  out.temp <- sapply(1:length(Q),function(i) paste("Q      ",year[i], str_pad(Q[i],width = 10, side = "left", pad = " "),sep="",collapse = ""))
  out <- c(out, out.temp)


  # write
  if (nchar(file.out)==0) file.out <- paste0(STAID,STANAME_APPEND,"peaks")
  if (!grepl(".spc\\>",file.out)) file.out <- paste0(file.out[1],".spc")

   write.table(out, file = file.path(folder.out,file.out) , quote = FALSE, sep = "", col.names = FALSE, row.names = FALSE, eol = "\r") # eol gives the ^M$ return used in the USGS watstore files
}  
