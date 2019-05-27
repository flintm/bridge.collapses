# 2015-07-29 Trend analysis of peak sites
# using Mann-Kendall test

library(Kendall)

load(file.path(dirs$DataDirFrequent,"20150729_Discharge_39Gages.RData"))

MKtau <- function(z) MannKendall[z]$tau
MKp   <- function(z) MannKendall[z]$sl

PeakMK    <- data.frame()

for (i in 1:length(ls.Discharge.Peaks)){
  r            <- MannKendall(ls.Discharge.Peaks[[i]]$peak_va)
  PeakMK[i,"tau"] <- r$tau
  PeakMK[i,"p"]   <- r$sl
}
rownames(PeakMK) <- names(ls.Discharge.Peaks)
rm(r)

stationsSignifTrends <- rownames(PeakMK[PeakMK$p<0.05,])

# Now find out the actual trend by performing a linear fit
peakLinearFits <- list()
for (i in 1:length(stationsSignifTrends)){
  peakLinearFits[[i]] <- lm(peak_va ~ peak_dt, ls.Discharge.Peaks[[stationsSignifTrends[i]]])
  peakLinearFits[[i]]$R_Sq_Mult <- summary(peakLinearFits[[i]])$r.squared
}
names(peakLinearFits) <- stationsSignifTrends
Rsq <- sapply(1:10, function(i) peakLinearFits[[i]]$R_Sq_Mult)
plot(peakLinearFits[[1]])

i<-i+1
dates <- as.Date(c(ls.Discharge.Peaks[[stationsSignifTrends[i]]]$peak_dt[1],ls.Discharge.Peaks[[stationsSignifTrends[i]]]$peak_dt[length(ls.Discharge.Peaks[[stationsSignifTrends[i]]]$peak_dt)]))
plot(ls.Discharge.Peaks[[stationsSignifTrends[i]]]$peak_dt,ls.Discharge.Peaks[[stationsSignifTrends[i]]]$peak_va)
lines(dates, predict.lm(peakLinearFits[[i]],data.frame(peak_dt=dates)))

# in general, significant, but poor fits with regard to large residuals
save(peakLinearFits,PeakMK,stationsSignifTrends,file=c("20150729_TrendAnalysisResults.RData"))
