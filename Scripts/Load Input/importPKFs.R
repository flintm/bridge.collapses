# Modified version of importDVs from waterData package
# modified on 2015-12-15 by Madeleine Flint to support queries for USGS
# peak flow values in watstore format

# example:
# http://nwis.waterdata.usgs.gov/nwis/peak?site_no=12358500&agency_cd=USGS&format=hn2

importPKFs <- function(staid) 
{
  if (is.character(staid) == FALSE) 
    stop("staid must be a character")
  if (nchar(staid) < 8) 
    stop("staid must be at least 8 characters")
  base_url <- "http://nwis.waterdata.usgs.gov/nwis/peak?"
  url <- paste(base_url, "site_no=", staid, sep="")
  url <- paste(url, "&agency_cd=USGS&format=hn2", 
               sep = "")

  df <- read.table(url, header=FALSE, sep = "\t",
                   colClasses = "character", skip = 0)

}
