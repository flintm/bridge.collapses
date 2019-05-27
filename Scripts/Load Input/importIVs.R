# Modified version of importDVs from waterData package
# modified on 2015-05-12 by Madeleine Flint to support queries for USGS
# instantaneous values (IVs)
importIVs <- function(staid, code = "00061", sdate = "1851-01-01", 
          edate = as.Date(Sys.Date(), format = "%Y-%m-%d")) 
{
  if (is.character(staid) == FALSE) 
    stop("staid needs to have quotes around it")
  if (nchar(staid) < 8) 
    stop("staid must be at least 8 characters")
  base_url <- "http://waterservices.usgs.gov/nwis/iv?"
  url <- paste(base_url, "site=", staid, "&parameterCd=", code, 
               sep = "")
  url <- paste(url, "&startDt=", sdate, "&endDt=", edate, sep = "")
  doc <- xmlTreeParse(url, getDTD = FALSE, useInternalNodes = TRUE)
  r <- xmlRoot(doc)
  i <- 1
  val <- vector(mode = "numeric", length = 1)
  while (xmlName(r[[2]][[3]][[i]]) == "value") {
    val[i] <- as.numeric(xmlValue(r[[2]][[3]][[i]]))
    i <- i + 1
  }
  Attribute <- xmlApply(r[[2]][[3]], xmlAttrs)
  N <- length(val)
  NoDataValue <- xmlValue(r[["timeSeries"]][["variable"]][["NoDataValue"]])
  NoDataValue <- as.integer(NoDataValue)
  dates <- vector(mode = "character", length = 1)
  qualcode <- vector(mode = "character", length = 1)
  if (N > 1) {
    for (z in 1:N) {
      dates[z] <- as.character(strsplit(Attribute[z][[1]][[2]], 
                                        "T")[[1]][1])
      qualcode[z] <- Attribute[z][[1]][[1]]
    }
    dates <- as.Date(dates, "%Y-%m-%d")
    df <- data.frame(staid, val, dates, qualcode)
    beginDate <- df$dates[1]
    endDate <- df$dates[dim(df)[[1]]]
    myDates <- as.data.frame(seq.Date(beginDate, endDate, 
                                      by = 1))
    dimnames(myDates)[[2]][1] <- "dates"
    ndays <- dim(myDates)[1]
    nobs <- dim(df)[1]
    if (nobs < ndays) {
      sitedat <- df
      fixedData <- merge(myDates, sitedat, all.x = TRUE)
      fixedData$staid <- sitedat$staid[1]
      fixedData <- fixedData[, c("staid", "val", "dates", 
                                 "qualcode")]
      df <- fixedData
    }
  }
  else {
    df <- data.frame(staid = character(0), val = numeric(0), 
                     dates = character(0), qualcode = character(0))
    my.message <- paste("No data returned for site", staid, 
                        "parameter code", code, "statistics code", stat, 
                        sdate, "to", edate, sep = " ")
    message(my.message)
  }
  attributes(df)$code <- code
  attributes(df)$stat <- stat
  df
}
