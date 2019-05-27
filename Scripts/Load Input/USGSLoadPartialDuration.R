# Setup for Partial Duration Analysis
# 2015-07-16 
# Madeleine Flint
# Uploads USGS text files giving points along partial
# duration curve for a given station. Text files were
# created using OCR on WaterWatch .png files

filepath  <- file.path(dirs$OrigDataDir,"StreamGages","PartialDuration")
filenames <- list.files(path=filepath,pattern="pg_[[:digit:]]{8}_all.txt")
nFiles    <- length(filenames)
gageIDs   <- substr(filenames,4,11)
ls.PartialDur <- list()
PartialDurN   <- rep(NA_integer_,length(gageIDs)) 
ExcVals <- c(0,0.05,0.10,0.25,0.5,0.75,0.90,0.95,1)
for (i in 1:nFiles){
  ls.PartialDur[[i]] <- read.table(file.path(filepath,filenames[i]),
                                   sep = " ",
                                   skip = 5,
                                   stringsAsFactors = FALSE,
                                   colClasses = c("character","numeric"),
                                   col.names = c("Pt","Val"))
  ls.PartialDur[[i]]$ExcVal <- ExcVals
  PartialDurN[i]      <- read.table(file.path(filepath,filenames[i]),
                                       sep = " ",
                                       skip = 2,
                                       stringsAsFactors = FALSE,
                                       nrows = 1,
                                       colClasses = c("character","numeric"),
                                       col.names = c("Pt","Val"))[2]
}
names(ls.PartialDur) <- gageIDs
names(PartialDurN) <- gageIDs

savefile <- file.path(dirs$DataDirFrequent,
                      paste(gsub("-","",Sys.Date()),
                            "PartialDurationData.RData",sep="_"))
save(ls.PartialDur,PartialDurN,file=savefile)
rm(savefile)
