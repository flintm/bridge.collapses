# Makes all plots for "Historical Analysis of Hydraulic Bridge Failures in the Continental United States"
# Copyright Madeleine Flint, 2016
# ODC-ODbl Open Database License, see http://opendatacommons.org/licenses/odbl/summary/

# Plots will be saved as .pdf files in the "Plots" folder
# General plotting controls (e.g., color, font size) can be modified in SetupEncoding.R

# See the wiki at https://git.it.vt.edu/mflint/Flint.et.al.2016.Historical.Hydraulic.Bridge.Failures for more
# plotting controls

gs_path = "/usr/local/bin/gs" #path to ghostscript (only necessary if you want to embed fonts in PDFs)

require(ggplot2)
require(grid)
require(gridExtra)
require(plyr)
require(reshape2)

dirM     <- getwd()
dirsGit <- list(dir         = dirM,
                Scripts     = file.path(dirM,"Scripts"),
                Data        = file.path(dirM,"Data"),
                ScriptsPlot = file.path(dirM,"Scripts","Plotting"),
                Plots       = file.path(dirM,"Plots"))

folders <- list.dirs(path = dirM, full.names = TRUE, recursive = TRUE)
dirsCreate <- unlist(dirsGit[2:length(dirsGit)])[!(unlist(dirsGit[2:length(dirsGit)]) %in% folders)]
if (any(!(sapply(dirsGit[2:3], function(d) d %in% folders)))){
  warning("Scripts, Data, and ScriptsPlot directories and files must be present")
  stop()
}
if (length(dirsCreate)!=0) sapply(dirsCreate, function(d) dir.create(d))
rm(dirM,folders,dirsCreate)

# (1) Map of US bridges ----------------------------------------------------------------------------
#     encoding:
#        fill = age - see options for type
#     options:
#        ggmap = TRUE/FALSE = use background map image from Google Maps
#        type  = one of {"1991", "ageContinuous", "ageDiscrete", "scourCrit"}
source(file.path(dirsGit$ScriptsPlot,"PlotAllBridgesMap.R"))
load(file.path(dirsGit$Data,"nbiBridgesOverWater.RData"))
df.USbridges.rivers        <- df.USbridges.rivers[,c("LATDD","LONGDD","ITEM27", "ITEM113")]
df.USbridges.rivers$ITEM27 <- as.numeric(as.character(df.USbridges.rivers$ITEM27))
AllBridgesMap              <- PlotAllBridgesMap(df.USbridges.rivers, ggmap = FALSE, type = "ageDiscrete",SAVE=TRUE)
rm(df.USbridges.rivers,AllBridgesMap,PlotAllBridgesMap)

# (2) Map of failed bridges ----------------------------------------------------------------------
#     encoding:
#        fill  = fail cause {flood, scour, hurricane, other}
#        shape = "H" for hurricane
#        alpha = fail date 1/0
#        size  = drainage area, see options for size
#     options:
#        ggmap = TRUE/FALSE = use background map image from Google Maps
#        size  = one of {"scaleLog","continuous","area","none"}
source(file.path(dirsGit$ScriptsPlot,"PlotFailedBridgesMap.R"))
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
# FailedMap <- PlotFailedBridgesMap(df.Fail.NBI.Gage, ggmap = FALSE, size = "none", 
#                                   FOR_INSET = FALSE, INSET = "", SHOW_HURR = FALSE, USE_SHAPE = TRUE, SAVE=TRUE)
FailedMap <- PlotFailedBridgesMap(df.Fail.NBI.Gage[rowsToView,], ggmap = FALSE, size = "area", 
                                  FOR_INSET = FALSE, INSET = "inset", USE_SHAPE = TRUE, SAVE=FALSE)
FailedMapInset1 <- PlotFailedBridgesMap(df.Fail.NBI.Gage[rowsToView,], ggmap = FALSE, size = "area", FOR_INSET = TRUE, 
                                        INSET = "MidAtlantic", USE_SHAPE = TRUE, LEGEND = FALSE, SAVE=FALSE)
FailedMapInset2 <- PlotFailedBridgesMap(df.Fail.NBI.Gage[rowsToView,], ggmap = FALSE, size = "area", FOR_INSET = TRUE, 
                                        INSET = "NewEngland", USE_SHAPE = TRUE, LEGEND = FALSE, SAVE=FALSE)
require(gridExtra)
pdf(file=file.path(dirsGit$Plots,"FailedMapWithInsetsAuto.pdf"), width = 7, height = 5, title = "Fig 2 Map of Collapsed Bridges",
    useDingbats = F)
grid.newpage()
v0 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) # main map
v1 <- viewport(width = 0.25, height = 0.25, x = 0.7065, y = 0.3727, clip = "on") #MidAtlantic
v2 <- viewport(width = 0.3, height = 0.3, x = 0.25, y = 0.37, clip = "on") #NewEngland
print(FailedMap, vp = v0)
print(FailedMapInset1, vp = v1)
print(FailedMapInset2, vp = v2)
dev.off()
require(extrafont)
Sys.setenv(R_GSCMD="/usr/local/bin/gs") #path to ghostscript
embed_fonts(file.path(dirsGit$Plots,"FailedMapWithInsetsAuto.pdf")) 

# FailedMap <- PlotFailedBridgesMap(df.Fail.NBI.Gage[rowsToView,], ggmap = FALSE, size = "scaleLog", FOR_INSET = FALSE, SHOW_HURR = FALSE, SAVE=TRUE)
rm(df.Fail.NBI.Gage,rowsToView,IDsToView,FailedMap)

# (3) Material and types bar chart ------------------------------------------------------------
#     encoding:
#        fill  = bridge material (steel, concrete, ...)
source(file.path(dirsGit$ScriptsPlot,'PlotMatTypesBarChart.R'))
load(file.path(dirsGit$Data,"nbiDataFramePreProcessedAllFields.RData"))
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
nbiDataFrame <- nbiDataFrame[,c("ITEM8","STFIPS","ITEM27","ITEM43A","ITEM43B","ITEM106","LATDD","LONGDD")]
BarP         <- PlotMatTypesBarChart(df.Fail.NBI.Gage[rowsToView,], nbiDataFrame, SAVE = TRUE, SIZE = c(3.5,3.5), TOP_STATES = FALSE)
rm(nbiDataFrame,df.Fail.NBI.Gage,BarP,PlotMatTypesBarChart,IDsToView,rowsToView)

# (4a) failure flow magnitude ------------------------------------------------------------------------------
#     [USGS HEC-DAILY]
#     encoding:
#        color  = fail cause {flood, scour, hurricane, other}
#        shapes = type (failure, max, ...)
#        alpha  = fail date 1/0
#        labels = Fail state, year built, year failed, hurricane, drainage ratio
#     options:
#        plotTypes = one of {pLogD, pLogIP} = plots daily mean or inst/peak flows on log scales
#        ONLY         = allows to plot only {"FAIL","MAX","PREFMAX","Q100","Q500"} values
#        LABELS       = TRUE/FALSE = plot labels
#        SUPER_FIELDS = if LABELS is TRUE, superscript includes (in order) any of
#                       {"Build" (yr),"Fail" (yr),"State","Hurr","Area" (ratio),"Inter" (interstate),
#                        "FailDate" (known or unknown)}
#        LEGEND       = TRUE/FALSE = show legend
#        outputType   = {"PRES","PRINT"} = should fonts be sized for presentation or paper   
source(file.path(dirsGit$ScriptsPlot,"PlotFailMaxFlows.R"))
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirsGit$Data,"StateCountyCityData.RData"))
HEIGHT        <- 6.1
FlowMagsPanel <- PlotFailMaxFlows(df.Fail.NBI.Gage[rowsToView,], SAVE = TRUE, plotTypes = "pLogD", 
                                  ONLY = NA, LABELS=TRUE, SUPER_FIELDS = c("Hurr","Area","FailDate"), 
                                  LEGEND = FALSE,  outputType = "PRINT", SIZE=c(HEIGHT,4))
rm(PlotFailMaxFlows,FlowMagsPanel)

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
#        SUPER_FIELDS   = if LABELS is TRUE, superscript includes (in order) any of
#                         {"Build" (yr),"Fail" (yr),"State","Hurr","Area" (ratio),"Inter" (interstate),
#                          "FailDate" (known or unknown)}
#        LEGEND         = TRUE/FALSE = show legend
#        SHOW_Q100_SYMB = if LEGEND is TRUE, show Q100 and Q500 shapes in legend (for uses with Fig. 4a-c)
#        outputType     = {"PRES","PRINT"} = should fonts be sized for presentation or paper 
source(file.path(dirsGit$ScriptsPlot,"PlotReturnPeriodsFailMax.R"))
Breaks             <- c(1,5,10,50,100,500,1000)
# ReturnPeriodsPanel <- PlotReturnPeriodsFailMax(df.Fail.NBI.Gage[rowsToView,], confInt = FALSE,Breaks=Breaks,
#                                                SAVE=TRUE,plotTypes = "pHECd",LABELS = FALSE,LEGEND=FALSE,
#                                                SIZE = c(HEIGHT,1.82), outputType = "PRINT")
# ReturnPeriodsPanel <- PlotReturnPeriodsFailMax(df.Fail.NBI.Gage[rowsToView,], confInt = FALSE,Breaks=Breaks,
#                                                SAVE=TRUE,plotTypes = "pHECip",LABELS = FALSE,LEGEND=TRUE,
#                                                SIZE = c(HEIGHT,3), SHOW_Q100_SYMB = TRUE)
# ReturnPeriodsPanel <- PlotReturnPeriodsFailMax(df.Fail.NBI.Gage[rowsToView,], confInt = FALSE,Breaks=Breaks,
#                                                SAVE=TRUE,plotTypes = "pHECd",LABELS = TRUE,LEGEND=FALSE, T_REPORT = FALSE,
#                                                SIZE = c(8,8), outputType = "PRES")
ReturnPeriodsPanel <- PlotReturnPeriodsFailMax(df.Fail.NBI.Gage[rowsToView,], confInt = FALSE,Breaks=Breaks,
                                               SAVE=FALSE,plotTypes = "pHECdip",LABELS = FALSE,LEGEND=TRUE,
                                               SIZE = c(HEIGHT,4), SHOW_Q100_SYMB = TRUE)
rm(list = ls(pattern="\\<df."),ReturnPeriodsPanel,rowsToView,IDsToView,PlotReturnPeriodsFailMax,Breaks,HEIGHT)

# (5) [3]x[2] panels of correlations ------------------------------------------------------------------------------
#     encoding:
#        fill   = fail cause {flood, scour, hurricane, other}
#        shapes = annotation on circle for state of stream regulation
#        alpha  = variable
#        text   = correlation information
#        size   = drainage area
#     options:
#        OUTPUT_TYPE = {"DAILY-USGS-3x2"} controls which figures used in panel. Additional output types need to
#                      be defined in PlotsCorrsMakeGrid.R
OUTPUT_TYPE <- "DAILY-USGS-3x2"
OUTPUT_TYPE <-"DAILY-USGS-VICG-3x2-PKFQ"
OUTPUT_TYPE <- "DAILY-USGS-VICG-3x2"
SAVE        <- TRUE
SAVE <- FALSE
SIZE        <- c(10.5,7.5)

OUTPUT_TYPE <- "DAILY-USGS-2x2-INDIV"
SIZE        <- c(7.5,7.5)
source(file.path(dirsGit$ScriptsPlot,"PlotCorrsMakeGrid.R"))
rm(OUTPUT_TYPE,SAVE,SIZE,grp)

# (6) Trends in peaks map---------------------------------------------------------------------------------------------
#     encoding:
#        color   = Mann-Kendall tau value
#        size    = drainage area, one of {"scaleLog","continuous","area","none"}
#        alpha   = significance of MK tau
#     options:
#        ggmap = TRUE/FALSE = use background map image from Google Maps
#        size  = one of {"scaleLog","continuous","area","none"}
source(file.path(dirsGit$ScriptsPlot,"PlotGageTrendsMap.R"))
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
GageTrendMap <- PlotGageTrendsMap(df.Fail.NBI.Gage[rowsToView,], ggmap = FALSE, size = "area", SAVE=T)
rm(GageTrendMap,df.Fail.NBI.Gage,PlotGageTrendsMap,rowsToView,IDsToView)

# (7) Supplemental bridge & gauge maps ------------------------------------------------------------------------------
#     encoding:
#        fill   = fail cause {flood, scour, hurricane, other, gauge (black)}
#        shapes = circle for bridge, square for gauge
#        color outline = fail date known (black), unknown (white)
#     options:
#        rowsToPlot = rows in df.Fail.NBI.Gage to plot
#        BridgeOnly = plot only bridge, not gauges
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
rowsToPlot   <- rowsToView
BridgeOnly   <- FALSE
source(file.path(dirsGit$ScriptsPlot,"PlotIndividualBridges.R"))
rm(list=ls())