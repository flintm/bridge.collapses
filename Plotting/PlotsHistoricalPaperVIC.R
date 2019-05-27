# Makes all plots for Historical Failures Paper

require(ggplot2)
require(grid)
require(gridExtra)
require(plyr)
require(reshape2)
# source(file.path(dirsGit$ScriptsPlot,"SetupEncoding.R"))

dirM     <- getwd()
dirsGit <- list(dir         = dirM,
                Scripts     = file.path(dirM,"Scripts"),
                Data        = file.path(dirM,"Data"),
                ScriptsPlot = file.path(dirM,"Scripts","Plotting"),
                Plots       = file.path(dirM,"Plots"))
rm(dirM)

#####################################################################################################

# DESCRIPTION OF PLOTS AND CALLS TO PLOTTING FUNCTIONS
# (2) Map of failed bridges ----------------------------------------------------------------------
#     encoding:
#        fill  = fail cause {flood, scour, hurricane, other}
#        color (outline) = hurricane 1/0
#        shape = square with fill and outline
#        alpha = fail date 1/0
#        text  = bridge/gage drainsqkm over bridge drainsqkm - NA**
#        size = one of {"scaleLog","continuous","area","none}
source(file.path(dirsGit$ScriptsPlot,"PlotFailedBridgesMap.R"))
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
FailedMap <- PlotFailedBridgesMap(df.Fail.NBI.Gage[,], ggmap = FALSE, 
                                  outputType = "PRES", SIZE = c(11,7),
                                  SHOW_HURR = FALSE, size = "scaleLog", plotSites = c("BRIDGE"),
                                  SAVE = TRUE)
rm(df.Fail.NBI.Gage,FailedMap)

# (4a) failure flow magnitude ------------------------------------------------------------------------------
#     [USGS-DAILY]
#     encoding:
#        color  = fail cause {flood, scour, hurricane, other}
#        shapes = type (failure, max, ...)
#        alpha  = fail date 1/0
#        labels = Fail state, year built, year failed, hurricane, drainage ratio
#     options:
#        plotTypes = one of {pLogD, pLogIP} = plots daily mean or inst/peak flows on log scales
#        ONLY         = allows to plot only {"FAIL","MAX","PREFMAX","Q100","Q500"} values
#        SEGMENTS     = show line segments linking max pre-fail and max, fail and max-pre-fail, and Q100-Q500
#        LABELS       = TRUE/FALSE = plot labels
#        SUPER_FIELDS = if LABELS is TRUE, superscript includes (in order) any of
#                       {"Build" (yr),"Fail" (yr),"State","Hurr","Area" (ratio),"Inter" (interstate),
#                        "FailDate" (known or unknown)}
#        LEGEND       = TRUE/FALSE = show legend
#        outputType   = {"PRES","PRINT"} = should fonts be sized for presentation or paper 
source(file.path(dirsGit$ScriptsPlot,"PlotFailMaxFlows.R"))
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirsGit$Data,"StateCountyCityData.RData"))
HEIGHT <- 5.5

# 
FlowMagsPanel <- PlotFailMaxFlowsBar(df.Fail.NBI.Gage[rowsToView,], SAVE = TRUE, plotTypes = "pLogD", ONLY = "FAIL", LEGEND = TRUE, 
                                     outputType = "PRES",size=c(HEIGHT,7.5), LABELS=TRUE,
                                     SUPER_FIELDS = NA)

rm(PlotFailMaxFlowsBar,FlowMagsPanel,df.Fail.NBI.Gage,HEIGHT,df.Cities,df.Counties,df.States)

# (4b,c) 1x2 panel of return period of major events (failure, max) ----------------------------------------------
#     [USGS HEC-DAILY][USGS HEC-INST/PEAK]
#     encoding:
#        color  = fail cause {flood, scour, hurricane, other}
#        shapes = type (failure, max, ...)
#        alpha  = fail date 1/0
#        labels = Fail state, year built, year failed, hurricane, drainage ratio
#     options:
#        plotTypes      = one of {pHECd, pHECip} = plots daily mean or inst/peak return periods
#        ONLY           = allows to plot only {"FAIL","MAX","PREFMAX","Q100","Q500"} values
#        LABELS         = TRUE/FALSE = plot labels
#        MAIN_FIELDS    = if LABELS is TRUE, label includes (in order) any of
#                         {"Build" (yr),"Fail" (yr),"State","Hurr","Area" (ratio),"Inter" (interstate),
#                          "FailDate" (known or unknown)}
#        SUPER_FIELDS   = if LABELS is TRUE, superscript includes (in order) any of
#                         {"Build" (yr),"Fail" (yr),"State","Hurr","Area" (ratio),"Inter" (interstate),
#                          "FailDate" (known or unknown)}
#        LEGEND         = TRUE/FALSE = show legend
#        SEGMENTS       = show line segments linking max pre-fail and max, fail and max-pre-fail
#        T_REPORT       = T/F = show failure return period estimates from various reports
#        SHOW_Q100_SYMB = if LEGEND is TRUE, show Q100 and Q500 shapes in legend (for uses with Fig. 4a-c)
#        outputType     = {"PRES","PRINT"} = should fonts be sized for presentation or paper 
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirsGit$Data,"StateCountyCityData.RData"))
source(file.path(dirsGit$ScriptsPlot,"PlotReturnPeriodsFailMax.R"))
Breaks <- c(1,5,10,50,100,500,1000)
HEIGHT <- 7
df.Fail.NBI.Gage.temp <- df.Fail.NBI.Gage
df.Fail.NBI.Gage.temp[,grep("MAX",colnames(df.Fail.NBI.Gage.temp))] <- sapply(colnames(df.Fail.NBI.Gage.temp)[grep("MAX",colnames(df.Fail.NBI.Gage.temp))], function(j) rep(NA,nrow(df.Fail.NBI.Gage.temp)))
ReturnPeriodsPanel <- PlotReturnPeriodsFailMax(df.Fail.NBI.Gage.temp[rowsToView,], confInt = FALSE,Breaks=Breaks,
                                               SAVE=TRUE,plotTypes = "pHECd",LABELS = TRUE,LEGEND = FALSE, SEGMENTS = FALSE, T_REPORT = FALSE,
                                               SIZE = c(HEIGHT,7), outputType = "PRES")

rm(list = ls(pattern="\\<df."),ReturnPeriodsPanel,rowsToView,IDsToView,PlotReturnPeriodsFailMax,Breaks,HEIGHT,df.Cities,df.Counties,df.States)


# (5) 1x3 panel of flow timeseries ----------
#     [best]-[median]-[worst] (by N-S or rho)
#      encoding:
#         lty   = data type {USGS,Daymet-VIC gage,Daymet-VIC Bridge} -> {,,}
#         lw    = data type {USGS,Daymet-VIC} -> {,}
#         color = data type {USGS,Daymet-VIC} -> {,}
#         text  = table: rho, tau, R^2, absE, MSE, N-S
#         textcolor =  black
source(file.path(dirsGit$ScriptsPlot,'PlotTimeSeries.R'))
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirsGit$Data,"StateCountyCityData.RData"))

# USGS
load(file.path(dirs$DataDirFrequent,"20150729_Discharge_39Gages.RData"))
rm(ls.Discharge.Inst,ls.Discharge.Dist)

# VIC
load(file.path(dirs$DataDirAnalysis,"20160211_USGS_DaymetVIC_CorrelationAndTestStatistics.RData"))
load(file.path(dirs$DataDirAnalysis,"20150909_GoodBadNeitherIDsForTimeSeriesCorrelationPlotting.RData"))
load(file.path(dirs$DataDirAnalysis,"20151203_GageRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirs$DataDirFrequent,"20151203_GageRoutingVICDaymet.RData"))

# NLDAS
load(file.path(dirs$DataDirAnalysis,"20151217_NLDASGageAnnualMaxes.RData"))
load(file.path(dirs$DataDirAnalysis,"20151217_NLDASGageDailyMean.RData"))
load(file.path(dirs$DataDirAnalysis,"20160204_USGS_NLDASgage_CorrelationAndTestStatistics.RData"))
kgCfs          <- 0.035315*(1/3600)

IDsToPlot   <- c(IDsWithGoodAgreement[1],IDsWithNeitherGoodNorBadAgreement[1],IDsWithBadAgreement[1])
STAIDtoPlot <- sapply(IDsToPlot, function(i) df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID == i,"STAID"])

# need annual peaks from daily mean data
# tempUSGS      <- ls.Discharge.All[[i]][,c("dates","val")]
# tempUSGS$type <- "USGS"
# colnames(tempUSGS)[1:2] <- c("Date","RUNOFF_CFS")
FlowList <- sapply(STAIDtoPlot, 
                   function(i) list(USGS = data.frame(Qu = ls.Discharge.All[[i]]$val,
                                                      t  = ls.Discharge.All[[i]]$dates #,
                                                      # type = "USGS"
                                                      ),
                                    VIC  = data.frame(Qv = ls.GageRouted[[i]]$day$RUNOFF_CFS,
                                                      t  = ls.GageRouted[[i]]$day$Date #,
                                                      # type = "VIC"
                                                      ),
                                    NLDAS = data.frame(Qn = ls.NLDAS.Gage[[i]]$SSRUN*kgCfs*df.Fail.NBI.Gage[df.Fail.NBI.Gage$STAID==i,"DRAIN_SQKM"][1]*1000^2,
                                                       t  = ls.NLDAS.Gage[[i]]$Date #,
                                                      # type = "NLDAS"
                                    )
                   ),
                   USE.NAMES = TRUE,
                   simplify = FALSE
)
StatsList <- sapply(STAIDtoPlot, 
                    function(i) list(VIC   = DaymetVICperformance[[i]],
                                     NLDAS = NLDASperformance[[i]]),
                    USE.NAMES = TRUE,
                    simplify = FALSE
)
TimeSeriesPanelNoLeg <- PlotTimeSeries(FlowList = FlowList,
                                       StatsList = StatsList, 
                                       df.Fail.NBI.Gage[c(which(df.Fail.NBI.Gage$ID == IDsToPlot[1]),
                                                          which(df.Fail.NBI.Gage$ID == IDsToPlot[2]),
                                                          which(df.Fail.NBI.Gage$ID == IDsToPlot[3])),],
                                       titles = NA,
                                       SAVE = TRUE, 
                                       TEXT = TRUE,
                                       SIZE = c(7.5,10),
                                       legend = FALSE,
                                       saveName = "USGS-VICg-NLDASg-DailyTimeSeries")
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
                                     saveName = "USGS-VICg-NLDASg-DailyTimeSeries")

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
rm(IDsToPlot,STAIDtoPlot,TimeSeriesPanel,PlotTimeSeries,rowsToView,TimeSeriesPanelLeg)

# (6) [3]x[2] panels of correlations ------------------------------------------------------------------------------
#     encoding:
#        fill   = fail cause {flood, scour, hurricane, other}
#        shapes = annotation on circle for state of stream regulation
#        alpha  = variable
#        text   = correlation information
#        size   = drainage area
#     options:
#        OUTPUT_TYPE = {"DAILY-USGS-3x2"} controls which figures used in panel. Additional output types need to
#                      be defined in PlotsCorrsMakeGrid.R
SAVE <- TRUE
SIZE <- c(10.5,7.5)

OUTPUT_TYPE <- "DAILY-USGS-VICG-3x2"
source(file.path(dirsGit$ScriptsPlot,"PlotCorrsMakeGrid.R"))

OUTPUT_TYPE <- "DAILY-USGS-VICG-3x2-PKFQ"
source(file.path(dirsGit$ScriptsPlot,"PlotCorrsMakeGrid.R"))

OUTPUT_TYPE <- "DAILY-USGS-NLDASG-3x2"
source(file.path(dirsGit$ScriptsPlot,"PlotCorrsMakeGrid.R"))

OUTPUT_TYPE <- "DAILY-USGS-VICG-NLDASG-3x2-PKFQ"
source(file.path(dirsGit$ScriptsPlot,"PlotCorrsMakeGrid.R"))

OUTPUT_TYPE <- "DAILY-USGS-VICG-NLDASG-3x3-PKFQ"
SIZE <- c(8,8)
source(file.path(dirsGit$ScriptsPlot,"PlotCorrsMakeGrid.R"))
rm(TYPE, SAVE, SIZE)


