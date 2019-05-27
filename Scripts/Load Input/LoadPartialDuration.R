# Function to load partial duration analysis data from OCR'ed USGS output file
# Written by Madeleine Flint, 2018-07-06

LoadPartialDuration <- function(filename, folder.out, colNames = c("Pt", "Val")){
 
  gageIDs   <- substr(filenames,4,11)
  ls.PartialDur <- list()
  PartialDurN   <- rep(NA_integer_,length(gageIDs)) 
  ExcVals <- c(0,0.05,0.10,0.25,0.5,0.75,0.90,0.95,1)
  PartDur <- read.table(file.path(folder.out,filename),
                                     sep = " ",
                                     skip = 5,
                                     stringsAsFactors = FALSE,
                                     colClasses = c("character","numeric"),
                                     col.names = colNames)
    PartDur$ExcVal <- ExcVals
    nPart      <- read.table(file.path(folder.out,filename),
                             sep = " ",
                             skip = 2,
                             stringsAsFactors = FALSE,
                             nrows = 1,
                             colClasses = c("character","numeric"),
                             col.names = colNames)[2]
    PartDur <- PartDur[,2:3]
    colnames(PFA) <- c("Flow","Freq")
    attr(PartDur,nPart = nPart)
    return(PartDur)
}