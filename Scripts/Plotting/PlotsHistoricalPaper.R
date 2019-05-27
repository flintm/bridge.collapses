# Makes all plots for Historical Failures Paper

require(ggplot2)
require(grid)
require(gridExtra)
require(plyr)
require(reshape2)
source(file.path(dirsGit$ScriptsPlotDir,"SetupEncoding.R"))

#####################################################################################################

# DESCRIPTION OF PLOTS AND CALLS TO PLOTTING FUNCTIONS
# (1) Map of US bridges ----------------------------------------------------------------------------
#     encoding:
#        fill = age - see type for options
#        shape = circle
#        alpha = fom alphasP$BridgeDesign
#        type  = one of {"1991", "ageContinuous", "ageDiscrete", "scourCrit"}
source(file.path(dirsGit$ScriptsPlotDir,"PlotAllBridgesMap.R"))
load(file.path(dirs$DataDirFrequent,"NBI_bridges_over_water.RData"))
df.USbridges.rivers <- df.USbridges.rivers[,c("LATDD","LONGDD","ITEM27", "ITEM113")]
df.USbridges.rivers$ITEM27 <- as.numeric(as.character(df.USbridges.rivers$ITEM27))
AllBridgesMap <- PlotAllBridgesMap(df.USbridges.rivers, ggmap = FALSE, type = "ageDiscrete",SAVE=TRUE)
rm(df.USbridges.rivers,AllBridgesMap,PlotAllBridgesMap)

# (2) Map of failed bridges ----------------------------------------------------------------------
#     encoding:
#        fill  = fail cause {flood, scour, hurricane, other}
#        color (outline) = hurricane 1/0
#        shape = square with fill and outline
#        alpha = fail date 1/0
#        text  = bridge/gage drainsqkm over bridge drainsqkm - NA**
#        size = one of {"scaleLog","continuous","area","none}
source(file.path(dirsGit$ScriptsPlotDir,"PlotFailedBridgesMap.R"))
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
FailedMap <- PlotFailedBridgesMap(df.Fail.NBI.Gage[rowsToView,], ggmap = FALSE, size = "area",SAVE=TRUE,insets=TRUE )

FailedMap <- PlotFailedBridgesMap(df.Fail.NBI.Gage[NA,], ggmap = FALSE, size = "area",SAVE=TRUE)
FailedMap <- PlotFailedBridgesMap(df.Fail.NBI.Gage[,], ggmap = FALSE, size = "area",LEGEND=TRUE,SAVE=TRUE)
rm(df.Fail.NBI.Gage,FailedMap)
s
# (3) Material and types
source(file.path(dirsGit$ScriptsPlotDir,'PlotMatTypesBarChart.R'))
load(file.path(dirs$DataDirFrequent,"nbiDataFramePreProcessedAllFields.RData"))
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
BridgesDataFrame <- df.Fail.NBI.Gage[rowsToView,]
nbiDataFrame <- nbiDataFrame[,c("ITEM8","STFIPS","ITEM27","ITEM43A","ITEM43B","ITEM106","LATDD","LONGDD")]

BarP <- PlotMatTypesBarChart(BridgesDataFrame, nbiDataFrame, SAVE = TRUE, size = c(3.5,3.5))
rm(BridgesDataFrame,nbiDataFrame,df.Fail.NBI.Gage,BarP,PlotMatTypesBarChart,IDsToView,rowsToView)

# (3) 1x3 panel of distribution fits to USGS ---------------------------------------------------------
#     [best]-[median]-[worst] (by K-S or A2)
#      encoding:
#         lty   = distribution type {HEC,LP3,GEV,P3} ->
#         lw    = distribution type {HEC,LP3,GEV,P3} ->
#         color = distribution type {HEC,LP3,GEV,P3} -> 
#         text  = A2 and KS values for all fits
#         textcolor = distribution type {LP3,GEV,P3} -> 
require(nsRFA)
source('~/hydraulic.failures/hydraulic.failures/Scripts/Plotting/PlotDistributionFits.R')
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirs$DataDirFrequent,"20150729_Discharge_39Gages.RData"))
load(file.path(dirs$DataDirAnalysis,"20150813_USGSgagePeaks_LP3_GEV_Param.RData"))
load(file.path(dirs$DataDirAnalysis,"20150813_USGSgagePeaks_LP3_GEV_GoodnessFit.RData"))
ls.df <- list()
ID <- "1004" # Tennessee, 0.37km to gage, drain sqkm 5967, area ratio vic 0.997, pass 2-sample KS for all data or same dates, 
#PearsonRho = 0.83 p-0, tau = 0.68p=0, Nash 0.63
STAID <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"STAID"]
# USGS
RUNOFF_CFS <- ls.Discharge.Peaks[[STAID]]$peak_va
df <- data.frame(Q    = sort(RUNOFF_CFS),
                 GEV = F.GEV(sort(RUNOFF_CFS),mlUSGSgagesPeaks[["GEV"]][[STAID]][1],mlUSGSgagesPeaks[["GEV"]][[STAID]][2],mlUSGSgagesPeaks[["GEV"]][[STAID]][3]),
                 LP3 = F.gamma(sort(log(RUNOFF_CFS)),mlUSGSgagesPeaks[["LP3"]][[STAID]][1],mlUSGSgagesPeaks[["LP3"]][[STAID]][2],mlUSGSgagesPeaks[["LP3"]][[STAID]][3])
)
ls.df[["USGS"]] <- list(df=df, 
                        AD = list(LP3p = pADUSGSgagesPeaks[["LP3"]][paste(STAID,"p(A2)",sep=".")],
                                  LP3  = ADUSGSgagesPeaks[["LP3"]][paste(STAID,"A2",sep=".")],
                                  GEVp = pADUSGSgagesPeaks[["GEV"]][paste(STAID,"p(A2)",sep=".")],
                                  GEV  = ADUSGSgagesPeaks[["GEV"]][paste(STAID,"A2",sep=".")]))
rm(list=ls(pattern="ls.Discharge.[IAD]"))
# rm(mlUSGSgagesPeaks,paramMaxLikelihoodUSGSgagePeaks,pADbridgesAnnualMax,ADUSGSgagesPeaks)
# VICG
load(file.path(dirs$DataDirAnalysis,"20150909_GageRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirs$DataDirAnalysis,"20150909_Gage_Daymet_VIC_Annual_Max_P3_LP3_GEV_Param.RData"))
load(file.path(dirs$DataDirAnalysis,"20150813_Gage_Daymet_VIC_AnnualMax_P3_LP3_GEV_GoodnessFit_Gage.RData"))
RUNOFF_CFS <- ls.GageRoutedAnnualMax[[STAID]]
df <- data.frame(Q    = sort(RUNOFF_CFS),
                 GEV = F.GEV(sort(RUNOFF_CFS),mlGagesAnnualMax[["GEV"]][[STAID]][1],mlGagesAnnualMax[["GEV"]][[STAID]][2],mlGagesAnnualMax[["GEV"]][[STAID]][3]),
                 LP3 = F.gamma(sort(log(RUNOFF_CFS)),mlGagesAnnualMax[["LP3"]][[STAID]][1],mlGagesAnnualMax[["LP3"]][[STAID]][2],mlGagesAnnualMax[["LP3"]][[STAID]][3])
)
ls.df[["VICg"]] <- list(df=df, 
                        AD = list(LP3p = pADGagesAnnualMax[["LP3"]][paste(as.integer(STAID),"p(A2)",sep=".")],
                                  LP3  = ADGagesAnnualMax[["LP3"]][paste(as.integer(STAID),"A2",sep=".")],
                                  GEVp = pADGagesAnnualMax[["GEV"]][paste(as.integer(STAID),"p(A2)",sep=".")],
                                  GEV  = ADGagesAnnualMax[["GEV"]][paste(as.integer(STAID),"A2",sep=".")]))
# rm(mlGagesAnnualMax,paramMaxLikelihoodGageAnnualMax,ls.GageRoutedAnnualMax,pADGagesAnnualMax,ADGagesAnnualMax)
# VICB
load(file.path(dirs$DataDirAnalysis,"20150810_BridgeRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirs$DataDirAnalysis,"20150813_Bridge_Daymet_VIC_Annual_Max_P3_LP3_GEV_Param.RData"))
load(file.path(dirs$DataDirAnalysis,"20150826_Bridge_Daymet_VIC_AnnualMax_P3_LP3_GEV_GoodnessFit_Bridge.RData"))
RUNOFF_CFS <- ls.BridgeRoutedAnnualMax[[ID]]
df <- data.frame(Q    = sort(RUNOFF_CFS),
                 GEV = F.GEV(sort(RUNOFF_CFS),mlBridgesAnnualMax[["GEV"]][[STAID]][1],mlBridgesAnnualMax[["GEV"]][[STAID]][2],mlBridgesAnnualMax[["GEV"]][[STAID]][3]),
                 LP3 = F.gamma(sort(log(RUNOFF_CFS)),mlBridgesAnnualMax[["LP3"]][[STAID]][1],mlBridgesAnnualMax[["LP3"]][[STAID]][2],mlBridgesAnnualMax[["LP3"]][[STAID]][3])
)
ls.df[["VICb"]] <- list(df=df, 
                        AD = list(LP3p = pADbridgesAnnualMax[["LP3"]][paste(ID,"p(A2)",sep=".")],
                                  LP3  = ADbridgesAnnualMax[["LP3"]][paste(ID,"A2",sep=".")],
                                  GEVp = pADbridgesAnnualMax[["GEV"]][paste(ID,"p(A2)",sep=".")],
                                  GEV  = ADbridgesAnnualMax[["GEV"]][paste(ID,"A2",sep=".")]))
# rm(mlBridgesAnnualMax,paramMaxLikelihoodBridgeAnnualMax,ls.BridgeRoutedAnnualMax)

# PLOT
# PlotDistFits <- PlotDistributionFits(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,],ls.df,TYPE = c("USGS","VICg","VICb"),SAVE=TRUE)
PlotDistFits <- PlotDistributionFits(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,],ls.df,TYPE = c("USGS","VICg"),SIZE=c(4,8.5),SAVE=TRUE)

rm(df.Fail.NBI.Gage,ID,STAID,ls.df)

# (4) 1x3 panel of distribution of failure return period (from bootstraps), one site ------------------------------
#     [bootstrap realizations with verticalPDF]-[Tr for all dists]-[Tr all data sets]
#      encoding:
#         lty   = distribution type {HEC,LP3,GEV,P3}
#         lw    = distribution type {HEC,LP3,GEV,P3}
#         color = distribution type {HEC,LP3,GEV,P3}, panel 3 {USGS,VIC-gage,VIC-bridge}
#         text  = mean, sd in panels 2 and 3
#         textcolor = distribution type {HEC,LP3,GEV,P3}, panel 3 data type
source(file.path(dirsGit$ScriptsPlotDir,"PlotTrBootstraps.R"))
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
# load(file.path(dirs$DataDirAnalysis,"DaymetVICbootstrapping","20150824_pExc_USGS_Annual_FailQ_Daily_SampleLengthRatio0.8_nSamples1000.RData"))
load(file.path(dirs$PlotsDirScripts,"20150925_Plotting.pExcDist_USGS_Daily_Annual_SampleLength43_nSamples1000.RData"))
load(file.path(dirs$PlotsDirScripts,"20150925_Plotting.pExcDist_Daymet_VIC_Annual_SampleLength25_nSamples1000.RData"))

Dists     <- list(TRbootLP3 = plotDataTrDistUSGS$median$param$LP3)
PlotBoots <- PlotTrBootstraps(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==IDboot,], paramsBoot, pExc, Dists, SAVE = TRUE, 
                              plotTypes = c("LP3boot","TrAllDists","TrAllData"), outputType = "PRINT", SIZE = c(4,8.5))
PlotBoots <- PlotTrBootstraps(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==IDboot,], paramsBoot, pExc, Dists, SAVE = TRUE, 
                              plotTypes = c("TrAllDists","TrAllData"), outputType = "PRINT", SIZE = c(4,8.5))
rm(df.Fail.NBI.Gage,IDsToView,PlotTrBootsraps,PlotBoots)

# (5) 1x3 panel of flow timeseries ----------
#     [best]-[median]-[worst] (by N-S or rho)
#      encoding:
#         lty   = data type {USGS,Daymet-VIC gage,Daymet-VIC Bridge} -> {,,}
#         lw    = data type {USGS,Daymet-VIC} -> {,}
#         color = data type {USGS,Daymet-VIC} -> {,}
#         text  = table: rho, tau, R^2, absE, MSE, N-S
#         textcolor =  black
source(file.path(dirsGit$ScriptsPlotDir,'PlotTimeSeries.R'))
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
# load(file.path(dirs$DataDirAnalysis,"20150827_USGS_DaymetVICbridge_CorrelationAndTestStatistics.RData"))
load(file.path(dirs$DataDirAnalysis,"20151205_USGS_DaymetVIC_CorrelationAndTestStatistics.RData"))
# load(file.path(dirs$DataDirAnalysis,"20150827_USGS_DaymetVICbridgeGage_CorrelationAndTestStatistics.RData"))
load(file.path(dirs$DataDirAnalysis,"20150909_GoodBadNeitherIDsForTimeSeriesCorrelationPlotting.RData"))
load(file.path(dirs$DataDirFrequent,"20150729_Discharge_39Gages.RData"))
rm(ls.Discharge.Inst,ls.Discharge.Dist)
# load(file.path(dirs$DataDirAnalysis,"20150810_BridgeRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirs$DataDirAnalysis,"20151203_GageRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirs$DataDirFrequent,"20151203_GageRoutingVICDaymet.RData"))
# load(file.path(dirs$DataDirFrequent,"20150807_BridgeRoutingVICDaymet.RData"))
load(file.path(dirs$DataDirFrequent,"StateCountyCityData.RData"))

IDsToPlot <- c(IDsWithGoodAgreement[1],IDsWithNeitherGoodNorBadAgreement[1],IDsWithBadAgreement[1])
STAIDtoPlot <- sapply(IDsToPlot, function(i) df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID == i,"STAID"])

# need annual peaks from daily mean data
# tempUSGS      <- ls.Discharge.All[[i]][,c("dates","val")]
# tempUSGS$type <- "USGS"
# colnames(tempUSGS)[1:2] <- c("Date","RUNOFF_CFS")
FlowList <- sapply(STAIDtoPlot, 
                  function(i) list(USGS = data.frame(Qu = ls.Discharge.All[[i]]$val,
                                                     t = ls.Discharge.All[[i]]$dates #,
                                                     # type = "USGS"
                  ),
                  VIC  = data.frame(Qv = ls.GageRouted[[i]]$day$RUNOFF_CFS,
                                    t = ls.GageRouted[[i]]$day$Date #,
                                    # type = "VIC"
                  )
                  ),
                  USE.NAMES = TRUE,
                  simplify = FALSE
)
StatsList <- sapply(STAIDtoPlot, 
                    function(i) DaymetVICperformance[[i]],
                    USE.NAMES = TRUE,
                    simplify = FALSE
)
TimeSeriesPanelNoLeg <- PlotTimeSeries(FlowList = FlowList,
                                  StatsList = StatsList, 
                                  df.Fail.NBI.Gage[c(which(df.Fail.NBI.Gage$ID == IDsToPlot[1]),
                                                     which(df.Fail.NBI.Gage$ID == IDsToPlot[2]),
                                                     which(df.Fail.NBI.Gage$ID == IDsToPlot[3])),],
                                  titles = c("GOOD","AVERAGE","POOR"),
                                  SAVE = FALSE, 
                                  TEXT = TRUE,
                                  SIZE = c(7.5,7.5),
                                  legend = FALSE,
                                  saveName = "USGS-VICgDailyTimeSeries")
TimeSeriesPanelLeg <- PlotTimeSeries(FlowList = FlowList,
                                       StatsList = StatsList, 
                                       df.Fail.NBI.Gage[c(which(df.Fail.NBI.Gage$ID == IDsToPlot[1]),
                                                          which(df.Fail.NBI.Gage$ID == IDsToPlot[2]),
                                                          which(df.Fail.NBI.Gage$ID == IDsToPlot[3])),],
                                       titles = c("GOOD","AVERAGE","POOR"),
                                       SAVE = FALSE, 
                                       TEXT = TRUE,
                                       SIZE = c(7.5,7.5),
                                       legend = TRUE,
                                       saveName = "USGS-VICgDailyTimeSeries")

SIZE <- c(1.5,6)
for (i in 1:2){
  saveName <- paste("USGS-VICgDailyTimeSeries",i,sep="")
  ggsave(filename = file.path(dirsGit$dir,"Plots",paste(saveName,"pdf",sep=".")), 
         plot = TimeSeriesPanelNoLeg[[i]],
         height = SIZE[1], width = SIZE[2])
}
i <- 3
saveName <- paste("USGS-VICgDailyTimeSeries",i,sep="")
ggsave(filename = file.path(dirsGit$dir,"Plots",paste(saveName,"pdf",sep=".")), 
       plot = TimeSeriesPanelLeg[[i]],
       height = SIZE[1]+0.5, width = SIZE[2])

rm(list = ls(pattern = "\\<ls."))
rm(list = ls(pattern = "\\<IDs"))
rm(list = ls(pattern = "\\<Daymet"))
rm(list = ls(pattern = "\\<df."))
rm(IDsToPlot,STAIDtoPlot,TimeSeriesPanel,PlotTimeSeries,rowsToView)

# (6) 1x3 panel of failure flow magnitude --------------------------------------------------------------------
#     [log][relative to max][relative to max instantaneous]
source(file.path(dirsGit$ScriptsPlotDir,"PlotFailMaxFlowsBar.R"))
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirs$DataDirFrequent,"StateCountyCityData.RData"))
HEIGHT <- 5.5

# 
FlowMagsPanel <- PlotFailMaxFlowsBar(df.Fail.NBI.Gage[rowsToView,], SAVE = TRUE, plotTypes = "pLogD", ONLY = NA, LEGEND = TRUE, 
                                     outputType = "PRINT",size=c(HEIGHT,7.5), LABELS=TRUE,
                                     SUPER_FIELDS = c("Hurr","Area","FailDate"))

rm(PlotFailMaxFlowsBar,FlowMagsPanel,df.Fail.NBI.Gage,HEIGHT,df.Cities,df.Counties,df.States)

# (7) 1x3 panel of return period of major events (failure, max) ----------------------------------------------
#     [USGS HEC/LP3][DAYMET-VIC-G LP3][DAYMET-VIC-B LP3]
#     encoding:
#        fill  = fail cause {flood, scour, hurricane, other}
#        color (outline) = hurricane 1/0
#        shapes = shapesP$Qtype - FAIL_Q (daily, inst/peak?), MAX_Q (daily, inst/peak?), MAX_PRE_FAIL (daily, inst/peak?)
#        alpha = fail date 1/0
#        text  = Fail state, year built, year failed
#        group = fail cause (used for sorting)
source(file.path(dirsGit$ScriptsPlotDir,"PlotReturnPeriodsFailMax.R"))
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirs$DataDirFrequent,"StateCountyCityData.RData"))
Breaks <- c(1,5,10,50,100,500,1000)
HEIGHT <- 5.5
ReturnPeriodsPanel <- PlotReturnPeriodsFailMax(df.Fail.NBI.Gage[rowsToView,], confInt = FALSE,Breaks=Breaks,
                                               SAVE=TRUE,plotTypes = "pHECd",LABELS = TRUE,LEGEND=FALSE,
                                               size = c(HEIGHT,3.5), outputType = "PRINT")
ReturnPeriodsPanel <- PlotReturnPeriodsFailMax(df.Fail.NBI.Gage[rowsToView,], confInt = FALSE,Breaks=Breaks,
                                               SAVE=TRUE,plotTypes = "pHECip",LABELS = FALSE,LEGEND=FALSE,
                                               size = c(HEIGHT,3))
ReturnPeriodsPanel <- PlotReturnPeriodsFailMax(df.Fail.NBI.Gage[rowsToView,], confInt = FALSE,Breaks=Breaks,
                                               SAVE=TRUE,plotTypes = "pVICgHEC",LABELS = FALSE,LEGEND = TRUE,
                                               size = c(HEIGHT,4.5), outputType = "PRINT")

rm(list = ls(pattern="\\<df."),ReturnPeriodsPanel,rowsToView,IDsToView,PlotReturnPeriodsFailMax,Breaks,HEIGHT,df.Cities,df.Counties,df.States)

# (8) [var.]x[var] panels of correlations ------------------------------------------------------------------------------
#     [?][?][?]...
#     encoding:
#        fill  = fail cause {flood, scour, hurricane, other}
#        shapes = annotation on circle for state of stream regulation
#        alpha = fail date 1/0
#        text  = correlation information
#        colors = on axis labels for data source
SAVE <- TRUE
# TYPE <- "DAILY-SQUARE"
# SIZE <- c(7.5,7.5)
# VEBOSE <- FALSE
# source(file.path(dirsGit$ScriptsPlotDir,"PlotCorrsMaxesMakeGrid.R"))
# 
# TYPE <- "DAILY-4x2"
# SIZE <- c(10,5)
# source(file.path(dirsGit$ScriptsPlotDir,"PlotCorrsMaxesMakeGrid.R"))

# TYPE <- "DAILY-3x2"
# SIZE <- c(10.5,7.5)
# source(file.path(dirsGit$ScriptsPlotDir,"PlotCorrsMaxesMakeGrid.R"))

TYPE <- "DAILY-USGS-3x2"
SIZE <- c(10.5,7.5)
source(file.path(dirsGit$ScriptsPlotDir,"PlotCorrsMaxesMakeGrid.R"))

rm(SIZE,TYPE,SAVE,bases,rowsToAnalyzeVIC)
dev.off()

# (9) Trends in peaks map---------------------------------------------------------------------------------------------
source(file.path(dirsGit$ScriptsPlotDir,"PlotGageTrendsMap.R"))
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
GageTrendMap <- PlotGageTrendsMap(df.Fail.NBI.Gage[rowsToView,], ggmap = FALSE, size = "area",SAVE=TRUE)

