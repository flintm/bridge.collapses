`# Write NLDAS output in Watstore format for use with PeakFQ
# Written on 2015-12-17 by Madeleine Flint
# Modified 2018-06-20 by Madeleine Flint to load updated (re-routed) Daymet-VIC data

require(nsRFA)
source(file.path(dirsGit$Scripts,"Process Output","write.watstore.R"))
source(file.path(dirsGit$Scripts,"Process Output","MakePeakFQspecs.R"))

# NLDAS --------------------------------------------------------------------------------------------------
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirs$DataDirAnalysis,"20151217_NLDASGageAnnualMaxes.RData"))
load(file.path(dirs$DataDirAnalysis,"20151217_NLDASBridgeAnnualMaxes.RData"))

years                 <- as.character(1979:2014)
dates                 <- seq.Date(as.Date("1979-01-01"),as.Date("2014-01-01"),by="year")
rowsToAnalyzeNLDAS    <- rowsToView[format.Date(df.Fail.NBI.Gage[rowsToView,"YR_FAIL_EST"],"%Y") %in% years]
STAIDtoAnalyzeNLDAS   <- as.character(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"STAID"])
IDsToAnalyzeNLDAS     <- as.character(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"ID"])
nSites                <- length(rowsToAnalyzeNLDAS)

Skews     <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"REGIONAL_SKEW"]

# NLDAS is in kg/m2 so need to integrate over drainage area and then convert from kg to cubic feet
# it's also averaged hourly
# and need to account for the area of the grid cell
DrainAreaG <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DRAIN_SQKM"]*1000^2 # in sq-meters
DrainAreaB <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"]*1000^2 # in sq-meters
kgCfs      <- 0.035315*(1/3600) # 1 kg is 0.035315 ft^3 of water, then average over 1 hour to per second
# 1 kg water = 1 L water
# 1 L = 0.0353147 ft^3
# 1000^2 m^2 = 1 km^2
# 3600 s = 1 hr
# so should be 0.0353147*1000^2/3600


file.out <- file.path(dirs$OrigDataDir,"PeakFQ","NLDAS")
for (i in 1:nSites){
  # Gage
  fileN  <- file.path(paste(file.out,"g",sep=""),
                      paste(STAIDtoAnalyzeNLDAS[i],"NLDASg.pkf",sep=""))
#   peaks  <- ls.NLDAS.Gage.AnnualMax[[STAIDtoAnalyzeNLDAS[i]]]*DrainAreaG[i]*kgCfs
#   write.watstore(STAIDtoAnalyzeNLDAS[i], dates[1:length(peaks)], peaks, file = fileN)
  # with interpolated skew from PeakFQ  
  # MakePeakFQspecs(STAIDtoAnalyzeNLDAS[i], paste(STAIDtoAnalyzeNLDAS[i],"NLDASg.pkf",sep=""), 1979, 2014, filePath = paste(file.out,"g",sep=""))
  # with regional skew
  MakePeakFQspecs(STAIDtoAnalyzeNLDAS[i], paste(STAIDtoAnalyzeNLDAS[i],"NLDASg.pkf",sep=""), 1979, 2014, skew = Skews[i], filePath = paste(file.out,"g",sep=""))
  
  
  # Bridge
  fileN  <- file.path(paste(file.out,"b",sep=""),
                      paste(IDsToAnalyzeNLDAS[i],"NLDASb.pkf",sep=""))
#   peaks  <- ls.NLDAS.Bridge.AnnualMax[[IDsToAnalyzeNLDAS[i]]]*DrainAreaB[i]*kgCfs
#   write.watstore(STAIDtoAnalyzeNLDAS[i], dates[1:length(peaks)], peaks, file = fileN)
  # with interpolated skew from PeakFQ  
  # MakePeakFQspecs(STAIDtoAnalyzeNLDAS[i], paste(IDsToAnalyzeNLDAS[i],"NLDASb.pkf",sep=""), 1979, 2014, filePath = paste(file.out,"b",sep=""))
  # with regional skew
  MakePeakFQspecs(STAIDtoAnalyzeNLDAS[i], paste(IDsToAnalyzeNLDAS[i],"NLDASb.pkf",sep=""), 1979, 2014, skew = Skews[i], filePath = paste(file.out,"b",sep=""))
}

# Gage batch file
BatchText <- paste("PKFQBat.exe ",STAIDtoAnalyzeNLDAS,"NLDASg.PSF", sep="")
write.table(BatchText, file = file.path(paste(file.out,"g",sep=""),"NLDASg.bat"), quote = FALSE,
            row.names = FALSE, col.names = FALSE)

# Bridge batch file
BatchText <- paste("PKFQBat.exe ",IDsToAnalyzeNLDAS,"NLDASb.PSF", sep="")
write.table(BatchText, file = file.path(paste(file.out,"b",sep=""),"NLDASb.bat"), quote = FALSE,
            row.names = FALSE, col.names = FALSE)
rm(list=ls(pattern = "ls."))

# DAYMET-VIC --------------------------------------------------------------------------------------------------
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
#load(file.path(dirs$DataDirAnalysis,"20151203_GageRoutingVICDaymetAnnualMaxes.RData"))
#load(file.path(dirs$DataDirAnalysis,"20151203_BridgeRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirsGit$Data,"20180620_GageRapidRoutingVICDaymetAnnualMaxes.RData"))

years                 <- as.character(1980:2015)
dates                 <- seq.Date(as.Date("1980-01-01"),as.Date("2015-01-01"),by="year")
# rowsToAnalyzeVIC      <- c("218","171","242")
rowsToAnalyzeVIC      <- rowsToView[format.Date(df.Fail.NBI.Gage[rowsToView,"YR_FAIL_EST"],"%Y") %in% years]
STAIDtoAnalyzeVIC     <- as.character(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"STAID"])
IDsToAnalyzeVIC       <- as.character(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"ID"])

nSites                <- length(STAIDtoAnalyzeVIC)

Skews     <- df.Fail.NBI.Gage[rowsToAnalyzeVIC,"REGIONAL_SKEW"]

file.out <- file.path(dirs$OrigDataDir,"PeakFQ","VICRapid")
for (i in 1:nSites){
  # Gage
    fileN  <- file.path(paste(file.out,"g",sep=""),
                        paste(STAIDtoAnalyzeVIC[i],"VICg.pkf",sep=""))
    peaks  <- ls.GageRapidRoutedAnnualMax[[STAIDtoAnalyzeVIC[i]]]
    write.watstore(STAIDtoAnalyzeVIC[i], dates[1:length(peaks)], peaks, file = fileN)
      # interpolated skews   
#     MakePeakFQspecs(STAIDtoAnalyzeVIC[i], paste(STAIDtoAnalyzeVIC[i],"VICg.pkf",sep=""), 1980, 2013, filePath = paste(file.out,"g",sep=""))
     # with regional skew
    MakePeakFQspecs(STAIDtoAnalyzeVIC[i], paste(STAIDtoAnalyzeVIC[i],"VICg.pkf",sep=""), 1980, 2015, skew = Skews[i], filePath = paste0(file.out,"g"))
  
  # Bridge
  #   if (IDsToAnalyzeVIC[[i]]=="1164") next()
  # fileN  <- file.path(paste(file.out,"b",sep=""),
  #                     paste(IDsToAnalyzeVIC[i],"VICb.pkf",sep=""))
#   peaks  <- ls.BridgeRoutedAnnualMax[[IDsToAnalyzeVIC[i]]]
#   write.watstore(STAIDtoAnalyzeVIC[i], dates[1:length(peaks)], peaks, file = fileN)
  # interpolated skew
  # MakePeakFQspecs(STAIDtoAnalyzeVIC[i], paste(IDsToAnalyzeVIC[i],"VICb.pkf",sep=""), 1980, 2013, filePath = paste(file.out,"b",sep=""))
  # with regional skew
  # MakePeakFQspecs(STAIDtoAnalyzeVIC[i], paste(IDsToAnalyzeVIC[i],"VICb.pkf",sep=""), 1980, 2013, skew = Skews[i], filePath = paste(file.out,"b",sep=""))
}

# Gage batch file
BatchText <- paste("PKFQBat.exe ",STAIDtoAnalyzeVIC,"VICg.PSF", sep="")
write.table(BatchText, file = file.path(paste(file.out,"g",sep=""),"VICg.bat"), quote = FALSE,
            row.names = FALSE, col.names = FALSE)

# Bridge batch file
BatchText <- paste("PKFQBat.exe ",IDsToAnalyzeVIC[IDsToAnalyzeVIC!="1164"],"VICb.PSF", sep="")
write.table(BatchText, file = file.path(paste(file.out,"b",sep=""),"VICb.bat"), quote = FALSE,
            row.names = FALSE, col.names = FALSE)

rm(list=ls(pattern = "ls."))

# USGS --------------------------------------------------------------------------------------------------
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirs$DataDirFrequent,"20150729_Discharge_39Gages.RData"))
rm(ls.Discharge.Inst,ls.Discharge.Dist,ls.Discharge)
STAIDtoView <- unique(df.Fail.NBI.Gage[rowsToView,"STAID"])
nSites                <- length(STAIDtoView)
ls.USGS.Discharge.36   <- sapply(STAIDtoView, function(i) ls.Discharge.All[[i]][,c("dates","val")],
                                 simplify = FALSE,
                                 USE.NAMES = TRUE)
rm(ls.Discharge.All)
ls.USGS.AnnualMax.36 <- list()
for (i in STAIDtoView){
  years <- unique(format.Date(ls.USGS.Discharge.36[[i]]$dates,"%Y"))
  yearIndex <- sapply(years, function(y) which(format.Date(ls.USGS.Discharge.36[[i]]$dates,"%Y")==y)[1])
  maxIndex  <- sapply(years, function(y) ifelse(sum(is.na(ls.USGS.Discharge.36[[i]][format.Date(ls.USGS.Discharge.36[[i]]$dates,"%Y")==y,"val"]))<20,
                                                which.max(ls.USGS.Discharge.36[[i]][format.Date(ls.USGS.Discharge.36[[i]]$dates,"%Y")==y,"val"]),
                                                NA)
  )
  maxIndex  <- maxIndex + yearIndex - 1
  ls.USGS.AnnualMax.36[[i]] <- data.frame(peak_dt = as.Date(sapply(maxIndex, function(j) ls.USGS.Discharge.36[[i]][j,"dates"]),"1970-01-01"),
                                          peak_va = sapply(maxIndex, function(j) ls.USGS.Discharge.36[[i]][j,"val"]))
  ls.USGS.AnnualMax.36[[i]] <- ls.USGS.AnnualMax.36[[i]][!is.na(ls.USGS.AnnualMax.36[[i]]$peak_va),]
}
rm(ls.USGS.Discharge.36)

Skews     <- df.Fail.NBI.Gage[rowsToView,"REGIONAL_SKEW"]

file.out <- file.path(dirs$OrigDataDir,"PeakFQ","USGS")
for (i in 1:nSites){
  # Daily mean annual maxes
  print(STAIDtoView[i])
  fileN  <- file.path(paste(file.out,"d",sep=""),
                      paste(STAIDtoView[i],"USGSd.pkf",sep=""))
#   peaks  <- ls.USGS.AnnualMax.36[[STAIDtoView[i]]]$peak_va
#   dates  <- ls.USGS.AnnualMax.36[[STAIDtoView[i]]]$peak_dt
#   write.watstore(STAIDtoView[i], dates, peaks, file = fileN)
  # interpolated skew
#   MakePeakFQspecs(STAIDtoView[i], paste(STAIDtoView[i],"USGSd.pkf",sep=""),
#                   as.numeric(format.Date(ls.USGS.AnnualMax.36[[i]]$peak_dt[1],"%Y")), 
#                   as.numeric(format.Date(ls.USGS.AnnualMax.36[[i]]$peak_dt[nrow(ls.USGS.AnnualMax.36[[i]])],"%Y")), 
#                   filePath = paste(file.out,"d",sep=""))
  # regional skew
  MakePeakFQspecs(STAIDtoView[i], paste(STAIDtoView[i],"USGSd.pkf",sep=""),
                  as.numeric(format.Date(ls.USGS.AnnualMax.36[[i]]$peak_dt[1],"%Y")), 
                  as.numeric(format.Date(ls.USGS.AnnualMax.36[[i]]$peak_dt[nrow(ls.USGS.AnnualMax.36[[i]])],"%Y")), 
                  skew = Skews[i],
                  filePath = paste(file.out,"d",sep=""))
  
  # Peaks
  fileN  <- file.path(paste(file.out,"p",sep=""),
                      paste(STAIDtoView[i],"USGSp.pkf",sep=""))
#   peaks  <- ls.Discharge.Peaks[[STAIDtoView[i]]]$peak_va
#   dates  <- ls.Discharge.Peaks[[STAIDtoView[i]]]$peak_dt
#   write.watstore(STAIDtoView[i], dates, peaks, file = fileN)
  # interpolated skew
#   MakePeakFQspecs(STAIDtoView[i], paste(STAIDtoView[i],"USGSp.pkf",sep=""),
#                   as.numeric(format.Date(ls.Discharge.Peaks[[i]]$peak_dt[1],"%Y")), 
#                   as.numeric(format.Date(ls.Discharge.Peaks[[i]]$peak_dt[nrow(ls.Discharge.Peaks[[i]])],"%Y")), 
#                   filePath = paste(file.out,"p",sep=""))
  # regional skew
  MakePeakFQspecs(STAIDtoView[i], paste(STAIDtoView[i],"USGSp.pkf",sep=""),
                  as.numeric(format.Date(ls.Discharge.Peaks[[i]]$peak_dt[1],"%Y")), 
                  as.numeric(format.Date(ls.Discharge.Peaks[[i]]$peak_dt[nrow(ls.Discharge.Peaks[[i]])],"%Y")), 
                  skew = Skews[i],
                  filePath = paste(file.out,"p",sep=""))
}

# Daily mean batch file
BatchText <- paste("PKFQBat.exe ",STAIDtoView,"USGSd.PSF", sep="")
write.table(BatchText, file = file.path(paste(file.out,"d",sep=""),"USGSd.bat"), quote = FALSE,
            row.names = FALSE, col.names = FALSE)

# Peaks batch file
BatchText <- paste("PKFQBat.exe ",STAIDtoView,"USGSp.PSF", sep="")
write.table(BatchText, file = file.path(paste(file.out,"p",sep=""),"USGSp.bat"), quote = FALSE,
            row.names = FALSE, col.names = FALSE)

