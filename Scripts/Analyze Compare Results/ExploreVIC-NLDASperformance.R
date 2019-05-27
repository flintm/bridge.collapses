# 20150731 testing correlation of T estimates

load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))

source(file.path(dirsGit$Scripts,'LinearRegressionT1T2.R'))


require(stats)
require(Kendall)
require(SuppDists)
require(pspearman)
require(nsRFA)
require(ggplot2)
require(grid)
require(gridExtra)

rowsToAnalyzeVIC  <- rownames(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView & df.Fail.NBI.Gage$YR_FAIL_EST >= "1980-01-01",])
rowsToAnalyzeVICtest <- rowsToAnalyzeVIC[df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"]>100 & df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_HEC_DVICG"]>50 & 
                                           df.Fail.NBI.Gage[rowsToAnalyzeVIC,"NASH_M_DVICG"]>0]
rowsToAnalyzeVICtest <- rowsToAnalyzeVIC[abs(1-df.Fail.NBI.Gage[rowsToAnalyzeVIC,"AREA_RATIO_DVICG"])< 0.25  & df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_HEC_DVICG"]>50]
rowsToAnalyzeVICtest <- rowsToAnalyzeVIC[df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"]>1000]

# How well-correlated are estimated of Q_fail
# USGS and VIC


#      (A5) How well-correlated are the various estimates of T_fail? ------------------


#      (C7) Are the return periods produced by Daymet-VIC vs USGS correlated? (population) -------------------------------------
# Daymet-VIC GAUGE HEC AND FITTED vs USGS HEC AND FITTED ---------
bases <- c("HECdHECd",                       "HECdHECdPM2",                    "HECipHECdPM2",
           "HECdLP3d",                       "HECdGEVd",                       "HECdP3d",                       "LP3dLP3d",                       "GEVdGEVd",
           "HECdLP3dPM2",                    "HECdGEVdPM2",                    "HECdP3dPM2",                    "LP3dLP3dPM2",                    "GEVdGEVdPM2")
T1ls  <- c("T_FAIL_D_HECD_USGS",                     "T_FAIL_D_HECD_USGS",                     "T_FAIL_IP_HECP_USGS",
           "T_FAIL_D_HECD_USGS",                     "T_FAIL_D_HECD_USGS",                     "T_FAIL_D_HECD_USGS",                    "T_FAIL_DAILY_LP3_USGS",          "T_FAIL_DAILY_GEV_USGS",
           "T_FAIL_D_HECD_USGS",                     "T_FAIL_D_HECD_USGS",                     "T_FAIL_D_HECD_USGS",                    "T_FAIL_DAILY_LP3_USGS",          "T_FAIL_DAILY_GEV_USGS")
T2ls  <- c("T_FAIL_HEC_DVICG",     "T_FAILPM2_HEC_DVICG", "T_FAILPM2_HEC_DVICG",     
           "T_FAIL_LP3_DAYMET_VIC_GAGE",     "T_FAIL_GEV_DAYMET_VIC_GAGE",     "T_FAIL_P3_DAYMET_VIC_GAGE",     "T_FAIL_LP3_DAYMET_VIC_GAGE",     "T_FAIL_GEV_DAYMET_VIC_GAGE",
           "T_FAIL_LP3_PM2_DAYMET_VIC_GAGE", "T_FAIL_GEV_PM2_DAYMET_VIC_GAGE", "T_FAIL_P3_PM2_DAYMET_VIC_GAGE", "T_FAIL_LP3_PM2_DAYMET_VIC_GAGE", "T_FAIL_GEV_PM2_DAYMET_VIC_GAGE")
ls.corrs.USGS.VICg <- list()
for (i in 1:length(bases)){
  ls.corrs.USGS.VICg[[paste(bases[i],"P",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T2ls[i]],
                                                                  method="pearson",
                                                                  conf.level=0.95,
                                                                  exact=TRUE)
  ls.corrs.USGS.VICg[[paste(bases[i],"K",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T2ls[i]],
                                                                  method="kendall",
                                                                  conf.level=0.95,
                                                                  exact=TRUE)
  ls.corrs.USGS.VICg[[paste(bases[i],"Papprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T2ls[i]],
                                                                        method="pearson",
                                                                        conf.level=0.95,
                                                                        exact=FALSE)
  ls.corrs.USGS.VICg[[paste(bases[i],"Kapprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T2ls[i]],
                                                                        method="kendall",
                                                                        conf.level=0.95,
                                                                        exact=FALSE)
  ls.corrs.USGS.VICg[[paste(bases[i],"LR",sep="")]] <- LinearRegressionT1T2(df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T2ls[i]])
}


P <- sapply(names(ls.corrs.USGS.VICg)[grepl("P\\>",names(ls.corrs.USGS.VICg))], function(i) ls.corrs.USGS.VICg[[i]]$estimate ,USE.NAMES = TRUE, simplify = TRUE)
which(P>0.9)
K <- sapply(names(ls.corrs.USGS.VICg)[grepl("K\\>",names(ls.corrs.USGS.VICg))], function(i) ls.corrs.USGS.VICg[[i]]$estimate ,USE.NAMES = TRUE, simplify = TRUE)
which(K>0.7)
LRm <- sapply(names(ls.corrs.USGS.VICg)[grepl("LR\\>",names(ls.corrs.USGS.VICg))], function(i) ls.corrs.USGS.VICg[[i]]$coefficients[2] ,USE.NAMES = TRUE, simplify = TRUE)

ls.corrs.USGS.VICg[grepl("HECdHECdPM2P\\>",names(ls.corrs.USGS.VICg)) | grepl("HECdHECdPM2K\\>",names(ls.corrs.USGS.VICg)) | grepl("HECdHECdPM2LR",names(ls.corrs.USGS.VICg))]
plot(df.Fail.NBI.Gage[rowsToAnalyzeVICtest,"T_FAIL_D_HECD_USGS"],df.Fail.NBI.Gage[rowsToAnalyzeVICtest,"T_FAILPM2_HEC_DVICG"])
# initial is rho = 0.61 tau = 0.49 m = 0.28

# with basins over 100km, some improvement
# getting rid of negative monthly NSE makes it worse
# only positive daily NSE and greater than 100km gets rho to 0.89, tau 0.6, LR 0.27
# if have experienced a 50-year event + positive monthly, rho 0.955, tau 0.733, slope 0.81
# regulation doesn't seem to help much

# area ratio within 25% and 50-year also ok,  not quite as good

# just having a 50-year event does pretty well on its own
cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_HEC_DVICG"],df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_MAX_GEV_DAYMET_VIC_GAGE"])

ls.corrs.USGS.VICg[grepl("LP3dLP3dPM2P\\>",names(ls.corrs.USGS.VICg)) | grepl("LP3dLP3dPM2K\\>",names(ls.corrs.USGS.VICg)) | grepl("LP3dLP3dPM2LR",names(ls.corrs.USGS.VICg))]
plot(df.Fail.NBI.Gage[rowsToAnalyzeVICtest,"T_FAIL_DAILY_LP3_USGS"],df.Fail.NBI.Gage[rowsToAnalyzeVICtest,"T_FAIL_LP3_PM2_DAYMET_VIC_GAGE"])



# Daymet-VIC Gauge HEC Max-fail-yr vs USGS t-best
bases <- c("HECbHECy","HECdHECy")
T1ls  <- c("T_FAIL_BEST_HEC","T_FAIL_D_HECD_USGS")
T2ls  <- c("T_FAILYRMAX_HEC_DVICG","T_FAILYRMAX_HEC_DVICG")
ls.corrs.Best.MaxYr <- list()
for (i in 1:length(bases)){
  ls.corrs.Best.MaxYr[[paste(bases[i],"P",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T2ls[i]],
                                                               method="pearson",
                                                               conf.level=0.95,
                                                               exact=TRUE)
  ls.corrs.Best.MaxYr[[paste(bases[i],"K",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T2ls[i]],
                                                               method="kendall",
                                                               conf.level=0.95,
                                                               exact=TRUE)
  ls.corrs.Best.MaxYr[[paste(bases[i],"Papprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                                        method="pearson",
                                                                        conf.level=0.95,
                                                                        exact=FALSE)
  ls.corrs.Best.MaxYr[[paste(bases[i],"Kapprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                                        method="kendall",
                                                                        conf.level=0.95,
                                                                        exact=FALSE)
  ls.corrs.Best.MaxYr[[paste(bases[i],"LR",sep="")]] <- LinearRegressionT1T2(df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T2ls[i]])
}

P <- sapply(names(ls.corrs.Best.MaxYr)[grepl("P\\>",names(ls.corrs.Best.MaxYr))], function(i) ls.corrs.Best.MaxYr[[i]]$estimate ,USE.NAMES = TRUE, simplify = TRUE)
which(P>0.6)
K <- sapply(names(ls.corrs.Best.MaxYr)[grepl("K\\>",names(ls.corrs.Best.MaxYr))], function(i) ls.corrs.Best.MaxYr[[i]]$estimate ,USE.NAMES = TRUE, simplify = TRUE)
which(K>0.7)
LRm <- sapply(names(ls.corrs.Best.MaxYr)[grepl("LR\\>",names(ls.corrs.Best.MaxYr))], function(i) ls.corrs.Best.MaxYr[[i]]$coefficients[2] ,USE.NAMES = TRUE, simplify = TRUE)
# 0.95, 0.6, 0.773
# in 3 of 6 cases the max event is the same event as the fail event (more or less) - not substantially different from rest




# Daymet-VIC BRIDGE FITTED vs USGS HEC AND FITTED --------
bases <- c("HECdLP3d",                         "HECdGEVd",                         "HECdP3d",                         "LP3dLP3d",                         "GEVdGEVd",
           "HECdLP3dPM2",                      "HECdGEVdPM2",                      "HECdP3dPM2",                      "LP3dLP3dPM2",                      "GEVdGEVdPM2")
T1ls  <- c("T_FAIL_D_HECD_USGS",                       "T_FAIL_D_HECD_USGS",                       "T_FAIL_D_HECD_USGS",                      "T_FAIL_DAILY_LP3_USGS",            "T_FAIL_DAILY_GEV_USGS",
           "T_FAIL_D_HECD_USGS",                       "T_FAIL_D_HECD_USGS",                       "T_FAIL_D_HECD_USGS",                      "T_FAIL_DAILY_LP3_USGS",            "T_FAIL_DAILY_GEV_USGS")
T2ls  <- c("T_FAIL_LP3_DAYMET_VIC_BRIDGE",     "T_FAIL_GEV_DAYMET_VIC_BRIDGE",     "T_FAIL_P3_DAYMET_VIC_BRIDGE",     "T_FAIL_LP3_DAYMET_VIC_BRIDGE",     "T_FAIL_GEV_DAYMET_VIC_BRIDGE",
           "T_FAIL_LP3_PM2_DAYMET_VIC_BRIDGE", "T_FAIL_GEV_PM2_DAYMET_VIC_BRIDGE", "T_FAIL_P3_PM2_DAYMET_VIC_BRIDGE", "T_FAIL_LP3_PM2_DAYMET_VIC_BRIDGE", "T_FAIL_GEV_PM2_DAYMET_VIC_BRIDGE")
ls.corrs.USGS.VICb <- list()
for (i in 1:length(bases)){
  ls.corrs.USGS.VICb[[paste(bases[i],"P",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVICtest,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVIC,T2ls[i]],
                                                               method="pearson",
                                                               conf.level=0.95,
                                                               exact=TRUE)
  ls.corrs.USGS.VICb[[paste(bases[i],"K",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVIC,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVIC,T2ls[i]],
                                                               method="kendall",
                                                               conf.level=0.95,
                                                               exact=TRUE)
  ls.corrs.USGS.VICb[[paste(bases[i],"Papprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                                        method="pearson",
                                                                        conf.level=0.95,
                                                                        exact=FALSE)
  ls.corrs.USGS.VICb[[paste(bases[i],"Kapprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                                        method="kendall",
                                                                        conf.level=0.95,
                                                                        exact=FALSE)
  ls.corrs.USGS.VICb[[paste(bases[i],"LR",sep="")]] <- LinearRegressionT1T2(df.Fail.NBI.Gage[rowsToAnalyzeVIC,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVIC,T2ls[i]])
}

#      (C8) Are the return periods produced by partial duration curves from Daymet-VIC and USGS correlated? (population) -------
bases <- c("USGSpartVICGpart",            "USGSpartVICBpart",                "USGSpartVICGpartPM2",           "USGSpartVICBpartPM2", 
           "VICGpartVICBpart",            "VICGpartPM2VICBpartPM2",          "VICGpartVICGpartPM2",           "VICBpartVICBpartPM2")
T1ls  <- c("T_PARTIAL_USGS",                   "T_PARTIAL_USGS",                       "T_PARTIAL_USGS",                     "T_PARTIAL_USGS",           
           "T_FAIL_PARTDUR_DVICG",   "T_FAILPM2_PARTDUR_DVICG",   "T_FAIL_PARTDUR_DVICG",     "T_FAIL_PARTDUR_DVICB")
T2ls  <- c("T_FAIL_PARTDUR_DVICG",   "T_FAIL_PARTDUR_DVICB",     "T_FAILPM2_PARTDUR_DVICG", "T_FAILPM2_PARTDUR_DVICB",          
           "T_FAIL_PARTDUR_DVICB", "T_FAILPM2_PARTDUR_DVICB", "T_FAILPM2_PARTDUR_DVICG", "T_FAILPM2_PARTDUR_DVICB")

ls.corrs.partial <- list()
for (i in 1:length(bases)){
  ls.corrs.partial[[paste(bases[i],"P",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVIC,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVIC,T2ls[i]],
                                                                  method="pearson",
                                                                  conf.level=0.95,
                                                                  exact=TRUE)
  ls.corrs.partial[[paste(bases[i],"K",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVIC,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVIC,T2ls[i]],
                                                                  method="kendall",
                                                                  conf.level=0.95,
                                                                  exact=TRUE)
  ls.corrs.partial[[paste(bases[i],"Papprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                                        method="pearson",
                                                                        conf.level=0.95,
                                                                        exact=FALSE)
    ls.corrs.partial[[paste(bases[i],"Kapprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                                        method="kendall",
                                                                        conf.level=0.95,
                                                                        exact=FALSE)
  ls.corrs.partial[[paste(bases[i],"LR",sep="")]] <- LinearRegressionT1T2(df.Fail.NBI.Gage[rowsToAnalyzeVIC,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVIC,T2ls[i]])
}


#      (D4) Are the return periods of failures obtained using Daymet-VIC bridge/gauge correlated? (population) ----------------
# Daymet-VIC gauge internal -----
bases <- c("LP3dLP3dPM2",                    "GEVdGEVdPM2",                    "P3dP3dPM2",                     "LP3dPM2GEVdPM2",                 "LP3dPM2P3dPM2",                    "GEVdPM2P3dPM2",
           "HECdPM2partial")
T1ls  <- c("T_FAIL_LP3_DAYMET_VIC_GAGE",     "T_FAIL_GEV_DAYMET_VIC_GAGE",     "T_FAIL_P3_DAYMET_VIC_GAGE",     "T_FAIL_LP3_PM2_DAYMET_VIC_GAGE", "T_FAIL_LP3_PM2_DAYMET_VIC_GAGE",   "T_FAIL_GEV_PM2_DAYMET_VIC_GAGE",
           "T_FAILPM2_HEC_DVICG")
T2ls  <- c("T_FAIL_LP3_PM2_DAYMET_VIC_GAGE", "T_FAIL_GEV_PM2_DAYMET_VIC_GAGE", "T_FAIL_P3_PM2_DAYMET_VIC_GAGE", "T_FAIL_GEV_PM2_DAYMET_VIC_GAGE", "T_FAIL_P3_PM2_DAYMET_VIC_GAGE",    "T_FAIL_P3_PM2_DAYMET_VIC_GAGE",
           "T_FAILPM2_PARTDUR_DVICG")
ls.corrs.VICg <- list()
for (i in 1:length(bases)){
  ls.corrs.VICg[[paste(bases[i],"P",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVIC,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVIC,T2ls[i]],
                                                               method="pearson",
                                                               conf.level=0.95,
                                                               exact=TRUE)
  ls.corrs.VICg[[paste(bases[i],"K",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVIC,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVIC,T2ls[i]],
                                                               method="kendall",
                                                               conf.level=0.95,
                                                               exact=TRUE)
  ls.corrs.VICg[[paste(bases[i],"Papprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                                        method="pearson",
                                                                        conf.level=0.95,
                                                                        exact=FALSE)
  ls.corrs.VICg[[paste(bases[i],"Kapprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                                        method="kendall",
                                                                        conf.level=0.95,
                                                                        exact=FALSE)
  ls.corrs.VICg[[paste(bases[i],"LR",sep="")]] <- LinearRegressionT1T2(df.Fail.NBI.Gage[rowsToAnalyzeVIC,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVIC,T2ls[i]])
}

# Daymet-VIC bridge internal -----
bases <- c("LP3dLP3dPM2",                      "GEVdGEVdPM2",                      "P3dP3dPM2",                       "LP3dPM2GEVdPM2",                   "LP3dPM2P3dPM2",                      "GEVdPM2P3dPM2")
T1ls  <- c("T_FAIL_LP3_DAYMET_VIC_BRIDGE",     "T_FAIL_GEV_DAYMET_VIC_BRIDGE",     "T_FAIL_P3_DAYMET_VIC_BRIDGE",     "T_FAIL_LP3_PM2_DAYMET_VIC_BRIDGE", "T_FAIL_LP3_PM2_DAYMET_VIC_BRIDGE",   "T_FAIL_GEV_PM2_DAYMET_VIC_BRIDGE")
T2ls  <- c("T_FAIL_LP3_PM2_DAYMET_VIC_BRIDGE", "T_FAIL_GEV_PM2_DAYMET_VIC_BRIDGE", "T_FAIL_P3_PM2_DAYMET_VIC_BRIDGE", "T_FAIL_GEV_PM2_DAYMET_VIC_BRIDGE", "T_FAIL_P3_PM2_DAYMET_VIC_BRIDGE",    "T_FAIL_P3_PM2_DAYMET_VIC_BRIDGE")

ls.corrs.VICb <- list()
for (i in 1:length(bases)){
  ls.corrs.VICb[[paste(bases[i],"P",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVIC,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVIC,T2ls[i]],
                                                          method="pearson",
                                                          conf.level=0.95,
                                                          exact=TRUE)
  ls.corrs.VICb[[paste(bases[i],"K",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVIC,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVIC,T2ls[i]],
                                                          method="kendall",
                                                          conf.level=0.95,
                                                          exact=TRUE)
  ls.corrs.VICb[[paste(bases[i],"Papprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                            method="pearson",
                                                            conf.level=0.95,
                                                            exact=FALSE)
  ls.corrs.VICb[[paste(bases[i],"Kapprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                            method="kendall",
                                                            conf.level=0.95,
                                                            exact=FALSE)
  ls.corrs.VICb[[paste(bases[i],"LR",sep="")]] <- LinearRegressionT1T2(df.Fail.NBI.Gage[rowsToAnalyzeVIC,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVIC,T2ls[i]])
}

# Daymet-VIC bridge vs Daymet-VIC gauge ------
bases <- c("LP3GdLP3Bd",                       "GEVGdGEVBd",                       "P3GdP3Bd",                        "LP3GdPM2LP3BdPM2",                 "GEVGdPM2GEVBdPM2",                   "P3GdPM2P3BdPM2")
T1ls  <- c("T_FAIL_LP3_DAYMET_VIC_GAGE",       "T_FAIL_GEV_DAYMET_VIC_GAGE",       "T_FAIL_P3_DAYMET_VIC_GAGE",       "T_FAIL_LP3_PM2_DAYMET_VIC_GAGE",   "T_FAIL_GEV_PM2_DAYMET_VIC_GAGE",   "T_FAIL_P3_PM2_DAYMET_VIC_GAGE")
T2ls  <- c("T_FAIL_LP3_DAYMET_VIC_BRIDGE",     "T_FAIL_GEV_DAYMET_VIC_BRIDGE",     "T_FAIL_P3_DAYMET_VIC_BRIDGE",     "T_FAIL_LP3_PM2_DAYMET_VIC_BRIDGE", "T_FAIL_GEV_PM2_DAYMET_VIC_BRIDGE", "T_FAIL_P3_PM2_DAYMET_VIC_BRIDGE")

ls.corrs.VICg.VICb <- list()
for (i in 1:length(bases)){
  ls.corrs.VICg.VICb[[paste(bases[i],"P",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVIC,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVIC,T2ls[i]],
                                                               method="pearson",
                                                               conf.level=0.95,
                                                               exact=TRUE)
  ls.corrs.VICg.VICb[[paste(bases[i],"K",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToAnalyzeVIC,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVIC,T2ls[i]],
                                                               method="kendall",
                                                               conf.level=0.95,
                                                               exact=TRUE)
  ls.corrs.VICg.VICb[[paste(bases[i],"Papprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                            method="pearson",
                                                            conf.level=0.95,
                                                            exact=FALSE)
  ls.corrs.VICg.VICb[[paste(bases[i],"Kapprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                            method="kendall",
                                                            conf.level=0.95,
                                                            exact=FALSE)
  ls.corrs.VICg.VICb[[paste(bases[i],"LR",sep="")]] <- LinearRegressionT1T2(df.Fail.NBI.Gage[rowsToAnalyzeVIC,T1ls[i]],df.Fail.NBI.Gage[rowsToAnalyzeVIC,T2ls[i]])
}

# (E1) NLDAS BRIDGE FITTED vs USGS HEC AND FITTED --------
bases <- c("HECdLP3d",                         "HECdGEVd",                              "LP3dLP3d",                         "GEVdGEVd",
             "HECdLP3dPM2",                      "HECdGEVdPM2",                       "LP3dLP3dPM2",                      "GEVdGEVdPM2")
T1ls  <- c("T_FAIL_D_HECD_USGS",                       "T_FAIL_D_HECD_USGS",                                  "T_FAIL_DAILY_LP3_USGS",            "T_FAIL_DAILY_GEV_USGS",
           "T_FAIL_D_HECD_USGS",                       "T_FAIL_D_HECD_USGS",                                 "T_FAIL_DAILY_LP3_USGS",            "T_FAIL_DAILY_GEV_USGS")
T2ls  <- c("T_FAIL_LP3_NLDAS_BRIDGE",     "T_FAIL_GEV_NLDAS_BRIDGE",      "T_FAIL_LP3_NLDAS_BRIDGE",     "T_FAIL_GEV_NLDAS_BRIDGE",
           "T_FAIL_LP3_PM2_NLDAS_BRIDGE", "T_FAIL_GEV_PM2_NLDAS_BRIDGE", "T_FAIL_LP3_PM2_NLDAS_BRIDGE", "T_FAIL_GEV_PM2_NLDAS_BRIDGE")
ls.corrs.USGS.NLDASb <- list()
for (i in 1:length(bases)){
  ls.corrs.USGS.NLDASb[[paste(bases[i],"P",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                               method="pearson",
                                                               conf.level=0.95,
                                                               exact=TRUE)
  ls.corrs.USGS.NLDASb[[paste(bases[i],"K",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                               method="kendall",
                                                               conf.level=0.95,
                                                               exact=TRUE)
  ls.corrs.USGS.NLDASb[[paste(bases[i],"Papprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                            method="pearson",
                                                            conf.level=0.95,
                                                            exact=FALSE)
  ls.corrs.USGS.NLDASb[[paste(bases[i],"Kapprox",sep="")]] <- cor.test(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]],
                                                            method="kendall",
                                                            conf.level=0.95,
                                                            exact=FALSE)
  ls.corrs.USGS.NLDASb[[paste(bases[i],"LR",sep="")]] <- LinearRegressionT1T2(df.Fail.NBI.Gage[rowsToView,T1ls[i]],df.Fail.NBI.Gage[rowsToView,T2ls[i]])
}

## SAVE ############ ------
savefile <- paste(gsub("-","",Sys.Date()),"CorrelationsOfReturnPeriods.RData", sep="_")
files    <- ls(pattern = "ls.corrs")
save(list = files, file = file.path(dirs$DataDirAnalysis,savefile))
rm(savefile, files)

rm(list=ls(pattern="^ls.corrs"))
rm(T1ls,T2ls,i,bases)
rm(LinearRegressionT1T2)


# NLDAS
rowsToAnalyzeNLDAS  <- rownames(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView & df.Fail.NBI.Gage$YR_FAIL_EST >= "1979-01-01",])

sum(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"Q_MAX_NLDASB"]==df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"Q_MAX_NLDASG"]) # 18 of 26 are the same
rowsToAnalyzeNLDASb <- rowsToAnalyzeNLDAS[df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"Q_MAX_NLDASB"]!=df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"Q_MAX_NLDASG"]]

source(file.path(dirsGit$Scripts,"CorrelationOfFailures.R"))
ls.corrs.fail <- CorrelationOfFailures(df.Fail.NBI.Gage[rowsToAnalyzeNLDASb,],SAVE = FALSE, VERBOSE = TRUE)
p.vals <- sapply(names(ls.corrs.fail[["NLDASg-NLDASb"]])[grepl("P\\>",names(ls.corrs.fail[["NLDASg-NLDASb"]]))],
                 function(i) ls.corrs.fail[["NLDASg-NLDASb"]][[i]]$p.value,
                 simplify = TRUE,
                 USE.NAMES = TRUE)

source(file.path(dirsGit$ScriptsPlot,"PlotAllCorrs.R"))
p <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,], ls.corrs.fail, TYPE = "NLDASg-NLDASb", TEXT=TRUE, SAVE=FALSE, ONLY=c("PM2"), ANY=c("HEC","PKFQ"),NOT=c("LP3","GEV","[dpi]P3"),
                  ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                  SCALE = "LOG", LEGEND = "NONE", SIZE = c(12,8), outputType = "PRINT",ANNOTATE_LOC = "UL",
                  SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))


# neural network
library(neuralnet)
source(file.path(dirsGit$Scripts,"logError.R"))
rowsToAnalyzeVIC  <- rownames(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView & df.Fail.NBI.Gage$YR_FAIL_EST >= "1980-01-01",])
BridgesDataFrame <- df.Fail.NBI.Gage[rowsToAnalyzeVIC,]

# clean up
factorCols <- c("STFIPS","ITEM_43A_FAIL","ITEM_43B_FAIL")
BridgesDataFrame[,factorCols] <- as.data.frame(sapply(factorCols, function(j) factor(as.character(BridgesDataFrame[,j]))))
BridgesDataFrame$YR_FAIL_EST  <- as.numeric(BridgesDataFrame$YR_FAIL_EST)


# get error cols
BridgesDataFrame$MAX_ERR   <- logError(BridgesDataFrame$Q_MAX_D_USGS,BridgesDataFrame$Q_MAX_DVICG)
BridgesDataFrame$FAIL_ERR  <- logError(BridgesDataFrame$Q_FAIL_D_USGS,BridgesDataFrame$Q_FAILPM2_DVICG)
BridgesDataFrame$TMAX_ERR  <- logError(BridgesDataFrame$T_MAX_D_PKFQD_USGS,BridgesDataFrame$T_MAX_PKFQD_DVICG)
BridgesDataFrame$TFAIL_ERR <- logError(BridgesDataFrame$T_FAIL_D_PKFQD_USGS,BridgesDataFrame$T_FAILPM2_PKFQD_DVICG)

# get predictive cols
predColsFail    <- c("STFIPS", "ITEM_43A_FAIL","ITEM_43B_FAIL","YR_BLT_EST","YR_FAIL_EST","FAIL_CAUS_CODE","BOOL_WAS_HURRICANE",
                     "BOOL_HAS_FAIL_DATE","BOOL_REGULATION","LATR","LONGR")
predColsVIC     <- c("DRAIN_AREA_DVICG_SQKM","AREA_RATIO_DVICG","NSE_D_DVICG","NASH_M_DVICG","R_SQUARED_M_DVICG",
                     "MAE_M_CFS_DVICG","RMSE_M_CFS_DVICG")
predColsUSGS    <- c("DIST_TO_GAGE","DRAIN_SQKM","REGIONAL_SKEW","COUNT_P_USGS","LATR_GAGE","LONGR_GAGE")
predColsResults <- c("Q_MAX_D_USGS","Q_FAIL_D_USGS","T_MAX_D_PKFQD_USGS","T_FAIL_D_PKFQD_USGS","Q_FAILPM2_DVICG",
                     "Q_MAX_DVICG","T_FAILPM2_PKFQD_DVICG","T_MAX_PKFQD_DVICG")
predColsAll     <- c(predColsFail,predColsVIC,predColsUSGS,predColsResults)
predColsAllNoNA <- predColsAll[sapply(predColsAll, function(j) !any(is.na(BridgesDataFrame[,j])))]

predColsMinAvailG <- c("STFIPS","YR_FAIL_EST","FAIL_CAUS_CODE","BOOL_HAS_FAIL_DATE","LATR","LONGR","DRAIN_SQKM","COUNT_P_USGS",
                       "LATR_GAGE","LONGR_GAGE","REGIONAL_SKEW",
                      "DRAIN_AREA_DVICG_SQKM","AREA_RATIO_DVICG","NSE_D_DVICG","NASH_M_DVICG","R_SQUARED_M_DVICG",
                      "MAE_M_CFS_DVICG","RMSE_M_CFS_DVICG","Q_FAILPM2_DVICG",
                      "Q_MAX_DVICG","T_FAILPM2_PKFQD_DVICG","T_MAX_PKFQD_DVICG","Q_MAX_D_USGS","T_MAX_D_PKFQD_USGS")

# first try with all columns available that aren't NA
trainRatio <- 0.3
ls.hidden  <- list(c(5,3),c(2,2))
n          <- 20
ls.MSE     <- list()
ls.MAE     <- list()
for (hid in 1:length(ls.hidden)){
  ls.MSE[[hid]] <- list()
  ls.MAE[[hid]] <- list()
  for (i in 1:n){
    res <- CompareLMvNN(BridgesDataFrame,predColsAllNoNA, trainRatio = trainRatio, hidden = ls.hidden[[hid]])
    ls.MSE[[hid]][[i]] <- res$MSE
    ls.MAE[[hid]][[i]] <- res$MAE
  }
}

MSE.means <- t(sapply(1:length(ls.hidden),
                    function(hid) sapply(1:4,
                                         function(i) mean(unlist(lapply(ls.MSE[[hid]],"[[",i))))))
colnames(MSE.means) <- c("LM.test","LM.train","NN.test","NN.train")

MAE.means <- t(sapply(1:length(ls.hidden),
                      function(hid) sapply(1:4,
                                           function(i) mean(unlist(lapply(ls.MAE[[hid]],"[[",i))))))
colnames(MAE.means) <- c("LM.test","LM.train","NN.test","NN.train")

# try reducing variables
predColsLessRed <- c("STFIPS", "FAIL_CAUS_CODE", "BOOL_HAS_FAIL_DATE",  "LATR" ,            
                     "LONGR",   "AREA_RATIO_DVICG" , "NSE_D_DVICG", "NASH_M_DVICG",  
                     "DIST_TO_GAGE" ,         "DRAIN_SQKM"     ,       "REGIONAL_SKEW"   ,  
                     "T_MAX_D_PKFQD_USGS"  ,    "T_FAILPM2_PKFQD_DVICG" , "T_MAX_PKFQD_DVICG" )
trainRatio <- 0.7
ls.hidden  <- list(c(5,3),c(4,4),c(2,2))
n          <- 20
ls.MSE     <- list()
ls.MAE     <- list()
for (hid in 1:length(ls.hidden)){
  ls.MSE[[hid]] <- list()
  ls.MAE[[hid]] <- list()
  for (i in 1:n){
    res <- CompareLMvNN(BridgesDataFrame,predColsLessRed, trainRatio = trainRatio, hidden = ls.hidden[[hid]])
    ls.MSE[[hid]][[i]] <- res$MSE
    ls.MAE[[hid]][[i]] <- res$MAE
  }
}

MSE.means <- t(sapply(1:length(ls.hidden),
                      function(hid) sapply(1:4,
                                           function(i) mean(unlist(lapply(ls.MSE[[hid]],"[[",i))))))
colnames(MSE.means) <- c("LM.test","LM.train","NN.test","NN.train")

MAE.means <- t(sapply(1:length(ls.hidden),
                      function(hid) sapply(1:4,
                                           function(i) mean(unlist(lapply(ls.MAE[[hid]],"[[",i))))))
colnames(MAE.means) <- c("LM.test","LM.train","NN.test","NN.train")


