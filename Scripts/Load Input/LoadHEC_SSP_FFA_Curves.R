# Loads Flood-Frequency-Analysis (FFA) curves created using HEC-SSP
# software Bulletin 17 analysis

load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))

colNames <- c("Ordinate",  "FREQ",	"FLOW Computed Curve",	"FLOW Expected Prob Curve",	"FLOW 5Pct Conf",	"FLOW 95Pct Conf",	"FLOW Observed (Median)",	"FLOW Historic Data",	"FLOW High Outlier",	"FLOW Low Outlier")
skip <- 5

## PROCESSING DISTRIBUTION DATA FOR PEAK FLOWS (PEAK FLOW ANALYSIS = PFA) FOR USGS ANNUAL PEAKS DATA -------------
data_folder <- file.path(dirs$OrigDataDir,"HEC","20160107_Combined_HEC_USGS_Pk")
filenames   <- list.files(data_folder)
gage_sites  <- substr(filenames,10,17)
HEC_PFA     <- lapply(1:length(filenames), function(i) read.table(file.path(data_folder,filenames[i]),
                                                                  header=FALSE,na.strings="NA",sep = "\t",
                                                                  skip=skip, col.names = colNames,
                                                                  stringsAsFactors = FALSE))
names(HEC_PFA) <- gage_sites

savefile <- paste(gsub("-","",Sys.Date()),"HEC_PFA_41_Bridges.RData",sep="_")
save(HEC_PFA, file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

## PROCESSING DISTRIBUTION DATA FORANNUAL MAX DAILY MEAN  FLOWS (PEAK FLOW ANALYSIS = PFA) FOR USGS DATA -------------
gage_sites  <- df.Fail.NBI.Gage[rowsToView,"STAID"]
filenames   <- paste(gage_sites,".tb",sep="")
data_folder <- file.path(dirs$OrigDataDir,"HEC","20151208_USGSmaxAnnualDailyMean_HEC","Table")
skip <- 3
HEC_PFA_AnMaxDayMean    <- lapply(1:length(filenames), function(i) read.table(file.path(data_folder,filenames[i]),
                                                                  header=FALSE,na.strings="NA",sep="\t",
                                                                  skip=skip, col.names = colNames,
                                                                  colClasses = 
                                                                  stringsAsFactors = FALSE))
names(HEC_PFA_AnMaxDayMean) <- gage_sites
probGages <- gage_sites[sapply(gage_sites, function(i) HEC_PFA_AnMaxDayMean[[i]][1,1]=="Units")]
for (i in probGages){
  # HEC_PFA_AnMaxDayMean[[i]] <- HEC_PFA_AnMaxDayMean[[i]][3:nrow(HEC_PFA_AnMaxDayMean[[i]]),]
  colClasses <- sapply(colnames(HEC_PFA_AnMaxDayMean[[i]]), function(j) class(HEC_PFA_AnMaxDayMean[[i]][,j]))
  cols       <- colnames(HEC_PFA_AnMaxDayMean[[i]])[!(colClasses %in% (c("numeric","integer")))]
  HEC_PFA_AnMaxDayMean[[i]][,cols] <- sapply(cols, function(j) as.numeric(HEC_PFA_AnMaxDayMean[[i]][,j]))
}

savefile <- paste(gsub("-","",Sys.Date()),"HEC_PFA_AnMaxDayMean_36_Bridges.RData",sep="_")
save(HEC_PFA_AnMaxDayMean, file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

## PROCESSING DISTRIBUTION DATA FOR ANNUAL MAX DAILY MEAN FLOWS (PEAK FLOW ANALYSIS = PFA) FOR VIC-GAGE DATA -------------
rowsToAnalyzeVIC  <- rownames(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView & df.Fail.NBI.Gage$YR_FAIL_EST >= "1980-01-01",])
gage_sites        <- df.Fail.NBI.Gage[rowsToAnalyzeVIC,"STAID"]
filenames         <- paste(gage_sites,".tb",sep="")
data_folder       <- file.path(dirs$OrigDataDir,"HEC","20151001-VIC-gauge-Bulletin17Bresults","Tables")
skip <- 3
HEC_PFA_VICG      <- lapply(1:length(filenames), function(i) read.table(file.path(data_folder,filenames[i]),
                                                                  header=FALSE,na.strings="NA",sep="\t",
                                                                  skip=skip, col.names = colNames,
                                                                  stringsAsFactors = FALSE))
names(HEC_PFA_VICG) <- gage_sites
probGages <- gage_sites[sapply(gage_sites, function(i) HEC_PFA_VICG[[i]][1,1]=="Units")]
for (i in probGages){
  HEC_PFA_VICG[[i]] <- HEC_PFA_VICG[[i]][3:nrow(HEC_PFA_VICG[[i]]),]
  colClasses <- sapply(colnames(HEC_PFA_VICG[[i]]), function(j) class(HEC_PFA_VICG[[i]][,j]))
  cols       <- colnames(HEC_PFA_VICG[[i]])[!(colClasses %in% (c("numeric","integer")))]
  HEC_PFA_VICG[[i]][,cols] <- sapply(cols, function(j) as.numeric(HEC_PFA_VICG[[i]][,j]))
}

savefile <- paste(gsub("-","",Sys.Date()),"HEC_PFA_VICG.RData",sep="_")
save(HEC_PFA_VICG, file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

rm(HEC_PFA,HEC_PFA_VICG,rowsToAnalyzeVIC,filenames,colNames,skip,data_folder,gage_sites)
