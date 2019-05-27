# Create text files with format required by HEC-SSP software for Bulletin 17B analysis
require(plyr)

# Daymet-VIC gauge data
load(file.path(dirs$DataDirAnalysis,"20151203_GageRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))

ls.GageRoutedAnnualMax.36 <- ls.GageRoutedAnnualMax[names(ls.GageRoutedAnnualMax) %in% df.Fail.NBI.Gage[rowsToView,"STAID"]]
peak_dt <- seq.Date(as.Date("1980-01-01"),as.Date("2013-01-01"), "1 year")
peak_dt <- format.Date(peak_dt,"%d%b%Y")
for (i in 1:length(ls.GageRoutedAnnualMax.36)){
  df <- data.frame(peak_dt = peak_dt, peak_va = round(ls.GageRoutedAnnualMax.36[[i]]))
  write.table(df, 
              file = file.path(dirs$OrigDataDir,"StreamGages","DaymetVICpeaks",paste(names(ls.GageRoutedAnnualMax.36)[i],"DaymetVICpeak.txt",sep="_")),
              sep = ",",
              quote = FALSE,
              row.names = FALSE,
              col.names = FALSE)
}
rm(df,ls.GageRoutedAnnualMax.36,ls.GageRoutedAnnualMax,i,peak_dt)

# USGS daily mean data
load(file.path(dirs$DataDirFrequent,"20150729_Discharge_39Gages.RData"))
rm(ls.Discharge.Inst,ls.Discharge.Dist,ls.Discharge,ls.Discharge.Peaks)
STAIDtoView <- unique(df.Fail.NBI.Gage[rowsToView,"STAID"])
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
  write.table(ls.USGS.AnnualMax.36[[i]], 
              file = file.path(dirs$OrigDataDir,"StreamGages","USGSmaxAnnualDailyMean",paste(i,"USGSAnMaxDailyMean.txt",sep="_")),
              sep = ",",
              quote = FALSE,
              row.names = FALSE,
              col.names = FALSE)                    
}
