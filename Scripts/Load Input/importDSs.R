# Modified version of importDVs from waterData package
# modified on 2015-05-12 by Madeleine Flint to support queries for USGS
# daily statistical distributions (DSs)
# Daily starts are given in RDB format, not XML

# example without dates:
# http://waterservices.usgs.gov/nwis/stat/?format=rdb&sites=01611500&statReportType=daily&statTypeCd=all&parameterCd=00060

# example with dates:
# http://waterservices.usgs.gov/nwis/stat/?format=rdb&sites=01611500&startDT=2015-05-03&endDT=2015-05-12&statReportType=daily&statTypeCd=all&parameterCd=00060
importDSs <- function(staid, code = "00060", sdate = NA, edate = NA) 
{
  if (is.character(staid) == FALSE) 
    stop("staid needs to have quotes around it")
  if (nchar(staid) < 8) 
    stop("staid must be at least 8 characters")
  if (!is.na(sdate)){
    if (ia.na(edate)){
      edate <- as.Date(Sys.Date(), format = "%Y-%m-%d")
    }
  }
  base_url <- "http://waterservices.usgs.gov/nwis/stat/?format=rdb&"
  url <- paste(base_url, "sites=", staid, sep="")
  if (!is.na(sdate)){
    url <- paste(url, "&startDT=", sdate, "&endDT=", edate, sep="")
  }
  url <- paste(url, "&statReportType=daily&statTypeCd=all&parameterCd=", code, 
               sep = "")

  skip <- 49
  colClassesDist <- c("character", "character","character", "character", 
                      "character", "integer","integer","integer",
                      "integer", rep("numeric",15))
  
  colNamesDist  <- c("agency_cd",  "site_no",  "parameter_cd","dd_nu",	
                     "loc_web_ds",	"month_nu",	"day_nu",	"begin_yr",	
                     "end_yr",	"count_nu",	"max_va_yr",	"max_va",
                     "min_va_yr",	"min_va",	"mean_va",	"p05_va",	
                     "p10_va",	"p20_va",	"p25_va",	"p50_va",	
                     "p75_va",	"p80_va",	"p90_va",	"p95_va")
  df <- read.table(url, header=FALSE, sep = "\t", col.names = colNamesDist,
                   colClasses = colClassesDist, skip = skip,
                   fill = TRUE, comment.char = "#")
  
  
#   
#   doc <- xmlTreeParse(url, getDTD = FALSE, useInternalNodes = TRUE)
#   r <- xmlRoot(doc)
#   i <- 1
#   val <- vector(mode = "numeric", length = 1)
#   while (xmlName(r[[2]][[3]][[i]]) == "value") {
#     val[i] <- as.numeric(xmlValue(r[[2]][[3]][[i]]))
#     i <- i + 1
#   }
#   Attribute <- xmlApply(r[[2]][[3]], xmlAttrs)
#   N <- length(val)
#   NoDataValue <- xmlValue(r[["timeSeries"]][["variable"]][["NoDataValue"]])
#   NoDataValue <- as.integer(NoDataValue)
#   dates <- vector(mode = "character", length = 1)
#   qualcode <- vector(mode = "character", length = 1)
#   if (N > 1) {
#     for (z in 1:N) {
#       dates[z] <- as.character(strsplit(Attribute[z][[1]][[2]], 
#                                         "T")[[1]][1])
#       qualcode[z] <- Attribute[z][[1]][[1]]
#     }
#     dates <- as.Date(dates, "%Y-%m-%d")
#     df <- data.frame(staid, val, dates, qualcode)
#     beginDate <- df$dates[1]
#     endDate <- df$dates[dim(df)[[1]]]
#     myDates <- as.data.frame(seq.Date(beginDate, endDate, 
#                                       by = 1))
#     dimnames(myDates)[[2]][1] <- "dates"
#     ndays <- dim(myDates)[1]
#     nobs <- dim(df)[1]
#     if (nobs < ndays) {
#       sitedat <- df
#       fixedData <- merge(myDates, sitedat, all.x = TRUE)
#       fixedData$staid <- sitedat$staid[1]
#       fixedData <- fixedData[, c("staid", "val", "dates", 
#                                  "qualcode")]
#       df <- fixedData
#     }
#   }
#   else {
#     df <- data.frame(staid = character(0), val = numeric(0), 
#                      dates = character(0), qualcode = character(0))
#     my.message <- paste("No data returned for site", staid, 
#                         "parameter code", code, "statistics code", stat, 
#                         sdate, "to", edate, sep = " ")
#     message(my.message)
#   }
#   attributes(df)$code <- code
#   attributes(df)$stat <- stat
#   df
}
