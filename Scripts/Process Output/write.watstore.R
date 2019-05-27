write.watstore <- function(STAID,dates,peaks, file=""){
  if (length(dates)!=length(peaks)){
    warning("Length of dates and peaks must match", immediate. = TRUE)
    return()
  }
  require(gdata) 
  if (!("importPKFs" %in% ls())) source(file.path(dirsGit$Scripts,"Load Input","importPKFs.R"))
#   "agency_cd	site_no	peak_yr	peak_month	peak_day	peak_tm	peak_va	peak_cd	gage_ht	gage_ht_cd	year_last_pk	ag_dt	ag_tm	ag_gage_ht	ag_gage_ht_cd"
#   "5s	15s	4n	2n	2n	6s	8s	27s	8s	13s	4s	10d	6s	8s	11s"
  widthLimits <- c(0,1,16,20,22,24,31,43,51,55,59,75,80)
  widths      <- widthLimits[2:length(widthLimits)] - widthLimits[1:(length(widthLimits)-1)]
  
  dates <- switch(class(dates),
                  "character" = as.Date(dates),
                  "numeric"   =  as.Date(dates,"1970-01-01"),
                  "integer"   =  as.Date(dates,"1970-01-01"),
                  "Date"      =  dates)
  year  <- format.Date(dates,"%Y")
  month <- format.Date(dates,"%m")
  day   <- format.Date(dates,"%d")

  peaks <- signif(peaks, digits = 6)
  
  x     <- data.frame(code = 3, STAID = STAID, year = year, month = month, day = day, peaks = peaks, stringsAsFactors = FALSE)
  x.out <- paste(x[,"code"],
                 sapply(1:nrow(x), function(i) paste(x[i,"STAID"], paste(rep(" ", widths[2] - nchar(x[i,"STAID"])),sep="",collapse=""),sep="")),
                 x[,"year"],
                 x[,"month"],
                 x[,"day"],
                 sapply(1:nrow(x), function(i) paste(paste(rep(" ",widths[6] - nchar(x[i,"peaks"])),sep="",collapse=""),x[i,"peaks"],sep="")),
                 paste(rep(" ", 40),sep="",collapse=""),
                 sep="")
  x.header <- importPKFs(STAID)[1:4,1]
  
  x.out <- c(x.header,x.out)

  if (nchar(file)==0) file <- paste(STAID,"peaks.pkf",sep="")
  if (!grepl(".pkf\\>",file) & !grepl(".txt\\>",file)) file <- paste(file,".pkf",sep="")
  
  write.table(x.out, file = file, quote = FALSE, sep = "", col.names = FALSE, row.names = FALSE, eol = "\r\n") # eol gives the ^M$ return used in the USGS watstore files
}
