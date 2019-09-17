# Makes all plots for "Historical Analysis of Hydraulic Bridge Collapses in the Continental United States"
# Accepted by ASCE Journal of Infrastructure Systems, October 19, 2016.
# Copyright Madeleine Flint, 2016
# ODC-ODbl Open Database License, see http://opendatacommons.org/licenses/odbl/summary/

# Plots will be saved as .pdf files in the "Plots" folder
# General plotting controls (e.g., color, font size) can be modified in SetupEncoding.R

# See the wiki at https://git.it.vt.edu/mflint/Flint.et.al.2016.Historical.Hydraulic.Bridge.Collapses for more
# plotting controls, list of required packages, etc.

gs_path    <- "/usr/local/bin/gs" #path to ghostscript (only necessary if you want to embed fonts in PDFs)
embedFonts <- TRUE # only possible with ghostscript installation

require(ggplot2)
require(grid)
require(gridExtra)
require(plyr)
require(reshape2)

dirM     <- getwd()
dirsGit <- list(dir         = dirM,
                Scripts     = file.path(dirM,"Scripts"),
                Data        = file.path(dirM,"Data"),
                ScriptsPlot = file.path(dirM,"Plotting"),
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
#        fill         = age - see options for type
#     options:
#        ggmap        = TRUE/FALSE = use background map image from Google Maps
#        type         = one of {"1991", "ageContinuous", "ageDiscrete", "scourCrit"}
#        outputType   = {"PRES","PRINT"} = should fonts be sized for presentation or paper
#        outputFormat = {"pdf","png", or other} = png is much quicker to process
#        embedFonts   = TRUE/FALSE = embeds fonts if using PDF format
#        gs_path      = path to Ghostscript installation (necessary if embedding fonts)
#        SAVE         = whether or not to save to file
#        SIZE         = c(width, height) in inches

source(file.path(dirsGit$ScriptsPlot,"PlotAllBridgesMap.R"))
load(file.path(dirsGit$Data,"df.USbridges.water.RData"))
df.USbridges.water        <- df.USbridges.water[,c("LATDD","LONGDD","ITEM27", "ITEM113")]
df.USbridges.water$ITEM27 <- format.Date(df.USbridges.water$ITEM27,"%Y")
df.USbridges.water$ITEM27 <- as.numeric(as.character(df.USbridges.water$ITEM27))
AllBridgesMap             <- PlotAllBridgesMap(df.USbridges.water, ggmap = FALSE, type = "ageContinuous",
                                                outputFormat = "pdf",SAVE=TRUE, gs_path = gs_path)
rm(df.USbridges.water,AllBridgesMap,PlotAllBridgesMap)

# (2) Map of collapsed bridges with insets -------------------------------------------------------------------
# Function takes 3 calls to produce main plot and two inset plots, which are then combined and printed to PDF
# Modify "controls" list below to make sure all changes are passed to each inset figure
#     encoding:
#        fill  = collapse cause {flood, scour, hurricane, other}
#        shape = "H" for linked hurricane, filled circle/unfilled circle for known collapse date T/F
#        alpha = known collapse date T/F (if shape not used)
#        size  = drainage area, see options for size
#     options:
#        ggmap      = TRUE/FALSE = use background map image from Google Maps
#        size       = one of {"scaleLog","continuous","area","none"}
#        legendPos  = {"RIGHT", "BOTTOM"} = legend position
#        SHOW_HURR  = TRUE/FALSE = superimpose "H" on hurricane sites
#        USE_SHAPE  = encode known failure date by shape (TRUE) or transparency (FALSE)
#        FOR_INSET  = TRUE/FALSE = controls for showing zoomed-in portions of map (note that only "area" and
#                                  "continuous" size options have all features for insets)
#        INSET      = {"MidAtlantic", "NewEngland"} = regions for zoomed-in insets 
#        outputType = {"PRES","PRINT"} = should fonts be sized for presentation or paper 
#        SAVE       = TRUE/FALSE = save file
#        SIZE       = c(width, height) in inches
#        EPS        = TRUE/FALSE = save as eps rather than pdf

source(file.path(dirsGit$ScriptsPlot,"PlotFailedBridgesMap.R"))
controls <- list(size = "area", ggmap = FALSE, USE_SHAPE = TRUE, embedFonts = embedFonts, SIZE = c(7, 5))

load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.RData"))
FailedMapAllShiny       <- PlotFailedBridgesMap (df.Fail.NBI.Gage[!is.na(df.Fail.NBI.Gage$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM),], ggmap = TRUE,size = "area", LEGEND = TRUE, 
outputType = "PRES", legendPos = "RIGHT", SHOW_HURR = TRUE)

FailedMapAll       <- PlotFailedBridgesMap (df.Fail.NBI.Gage[!is.na(df.Fail.NBI.Gage$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM),], ggmap = TRUE, 
                                            size = controls$size, FOR_INSET = TRUE,
                                            INSET = "MidAtlantic", USE_SHAPE = FALSE, SAVE=FALSE, LEGEND = TRUE, plotSites = "BRIDGE",
  outputType = "PRINT", legendPos = "BOTTOM", SHOW_HURR = FALSE,SIZE = c(7,5), EPS = FALSE)

FailedMap       <- PlotFailedBridgesMap(df.Fail.NBI.Gage[rowsToView,], ggmap = controls$ggmap, size = controls$size, FOR_INSET = FALSE,
                                        INSET = "inset", USE_SHAPE = controls$USE_SHAPE, SAVE=FALSE)
FailedMapInset1 <- PlotFailedBridgesMap(df.Fail.NBI.Gage[rowsToView,], ggmap = controls$ggmap, size = controls$size, FOR_INSET = TRUE, 
                                        INSET = "MidAtlantic", USE_SHAPE = controls$USE_SHAPE, LEGEND = FALSE, SAVE=FALSE)
FailedMapInset2 <- PlotFailedBridgesMap(df.Fail.NBI.Gage[rowsToView,], ggmap = controls$ggmap, size = controls$size, FOR_INSET = TRUE, 
                                        INSET = "NewEngland", USE_SHAPE = controls$USE_SHAPE, LEGEND = FALSE, SAVE=FALSE)

# make plot with insets
pdf(file=file.path(dirsGit$Plots,"Fig2.pdf"), width = controls$SIZE[1], height = controls$SIZE[2], title = "Fig 2 Map of Collapsed Bridges")
grid.newpage()
v0 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) # main map
v1 <- viewport(width = 0.25, height = 0.25, x = 0.7065, y = 0.3727, clip = "on") #MidAtlantic
v2 <- viewport(width = 0.3, height = 0.3, x = 0.25, y = 0.37, clip = "on") #NewEngland
print(FailedMap, vp = v0)
print(FailedMapInset1, vp = v1)
print(FailedMapInset2, vp = v2)
dev.off()
if(controls$embedFonts){
  require(extrafont)
  Sys.setenv(R_GSCMD=gs_path) 
  embed_fonts(file.path(dirsGit$Plots,"Fig2.pdf"))
}

rm(df.Fail.NBI.Gage,rowsToView,IDsToView,FailedMap,FailedMapInset1,FailedMapInset2,v0,v1,v2, controls)

# (3) Material and types column chart ------------------------------------------------------------
#     encoding:
#        fill  = bridge material (steel, concrete, ...)
#     options:
#        PERCENT      = TRUE/FALSE = y-axis shows %, not #
#        TOP_STATES   = TRUE/FALSE = show only bridges in VA, MD, and NY
#        embedFonts   = TRUE/FALSE = embeds fonts if using PDF format
#        gs_path      = path to Ghostscript installation (necessary if embedding fonts)
#        SAVE         = TRUE/FALSE = save file
#        SIZE         = c(width, height) in inches

source(file.path(dirsGit$ScriptsPlot,"PlotMatTypesBarChart.R"))
load(file.path(dirsGit$Data,"df.USbridges.water.RData"))
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.RData"))
df.USbridges.water <- df.USbridges.water[,c("ITEM8","STFIPS","ITEM27","ITEM43A","ITEM43B","ITEM106","LATDD","LONGDD")]
BarP               <- PlotMatTypesBarChart(df.Fail.NBI.Gage[rowsToView,], df.USbridges.water, SAVE = TRUE, SIZE = c(3.5,3.5), embedFonts = embedFonts, gs_path = gs_path)
rm(df.USbridges.water,df.Fail.NBI.Gage,BarP,PlotMatTypesBarChart,IDsToView,rowsToView)

# (4a) failure flow magnitude ------------------------------------------------------------------------------
#     [USGS-DAILY]
#     encoding:
#        color  = collapse cause {flood, scour, hurricane, other}
#        shapes = type of data (collapse, max, 500-year flood, ...)
#        alpha  = collapse date known T/F
#        labels = year built, year collapsed, state, hurricane, drainage ratio, unknown collapse date
#     options:
#        plotTypes    = one of {pLogD, pLogIP} = plots daily mean or inst/peak flows on log scales
#        Q_units      = {"m3s","cfs"} = units for plotting flows, Q (m3s = meters^3/second, cfs = cubic feet per second)
#        SEGMENTS     = show line segments linking max pre-fail and max, fail and max-pre-fail, and Q100-Q500
#        LABELS       = TRUE/FALSE = plot labels
#        MAIN_FIELDS  = if LABELS is TRUE, label includes (in order) any of
#                       {"Build" (yr),"Collapse" (yr),"State","Hurr","Area" (ratio),"Inter" (interstate),
#                        "FailDate" (known or unknown)}
#        SUPER_FIELDS = if LABELS is TRUE, superscript includes (in order) any of
#                       {"Build" (yr),"Collapse" (yr),"State","Hurr","Area" (ratio),"Inter" (interstate),
#                        "FailDate" (known or unknown)}
#        LEGEND       = TRUE/FALSE = show legend
#        outputType   = {"PRES","PRINT"} = should fonts be sized for presentation or paper  
#        embedFonts   = TRUE/FALSE = embeds fonts if using PDF format
#        gs_path      = path to Ghostscript installation (necessary if embedding fonts)
#        SAVE         = TRUE/FALSE = save file
#        SIZE         = c(width, height) in inches

source(file.path(dirsGit$ScriptsPlot,"PlotFailMaxFlows.R"))
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.RData"))
load(file.path(dirsGit$Data,"StateCountyCityData.RData"))
HEIGHT        <- 6.1
FlowMagsPanel <- PlotFailMaxFlows(df.Fail.NBI.Gage[rowsToView,], SAVE = TRUE, plotTypes = "pLogD", Q_units = "m3s",
                                  ONLY = NA, LABELS=TRUE, SUPER_FIELDS = c("Hurr","Area","FailDate"), 
                                  LEGEND = FALSE,  outputType = "PRINT", SIZE=c(4, HEIGHT), embedFonts = embedFonts,
                                  gs_path = gs_path)
rm(PlotFailMaxFlowsBar,FlowMagsPanel)

# (4b) Return period of major events (failure, max) ----------------------------------------------
#     [USGS HEC-DAILY & HEC-INST/PEAK]
#     encoding:
#        color  = collapse cause {flood, scour, hurricane, other}
#        shapes = type (collapse, max, ...)
#        alpha  = collapse date known T/F
#        labels = year built, year failed, state, hurricane, drainage ratio
#     options:
#        plotTypes      = one of {pHECd, pHECip, pHECdip} = plots daily mean and/or inst/peak return periods
#        LABELS         = TRUE/FALSE = plot labels
#        MAIN_FIELDS    = if LABELS is TRUE, label includes (in order) any of
#                         {"Build" (yr),"Fail" (yr),"State","Hurr","Area" (ratio),"Inter" (interstate),
#                          "FailDate" (known or unknown)}
#        SUPER_FIELDS   = if LABELS is TRUE, superscript includes (in order) any of
#                         {"Build" (yr),"Fail" (yr),"State","Hurr","Area" (ratio),"Inter" (interstate),
#                          "FailDate" (known or unknown)}
#        LEGEND         = TRUE/FALSE = show legend
#        SEGMENTS       = show line segments linking max pre-fail and max, fail and max-pre-fail
#        T_REPORT       = T/F = show failure return period estimates from various (external) reports
#        SHOW_Q100_SYMB = if LEGEND is TRUE, show Q100 and Q500 shapes in legend (to combine with Fig. 4a)
#        outputType     = {"PRES","PRINT"} = should fonts be sized for presentation or paper 
#        embedFonts     = TRUE/FALSE = embeds fonts if using PDF format
#        gs_path        = path to Ghostscript installation (necessary if embedding fonts)
#        SAVE           = TRUE/FALSE = save file
#        SIZE           = c(width, height) in inches
source(file.path(dirsGit$ScriptsPlot,"PlotReturnPeriodsFailMax.R"))
Breaks             <- c(1,5,10,50,100,500,1000)
ReturnPeriodsPanel <- PlotReturnPeriodsFailMax(df.Fail.NBI.Gage[rowsToView,],Breaks=Breaks,
                                               SAVE=TRUE,plotTypes = "pHECdip",LABELS = FALSE,LEGEND=TRUE,
                                               SIZE = c(4, HEIGHT), SHOW_Q100_SYMB = TRUE, embedFonts = embedFonts,
                                               gs_path = gs_path)
rm(list = ls(pattern="\\<df."),ReturnPeriodsPanel,rowsToView,IDsToView,PlotReturnPeriodsFailMax,Breaks,HEIGHT)

# (5) [2]x[2] panels of correlations ------------------------------------------------------------------------------
#     encoding:
#        color      = collapse cause {flood, scour, hurricane, other}
#        shapes     = filled/unfilled circle for whether or not collapse date is known
#        annotation = annotation on circle for state of stream regulation
#        alpha      = variable (see PlotCorrsMakeGrid.R for documentation)
#        text       = correlation information
#        size       = drainage area (scaled to be consistent across sub-plots)
#     options:
#        OUTPUT_TYPE = {"DAILY-USGS-2x2", "DAILY-USGS-2x2-INDIV"}
#                      controls which figures used in panel. INDIV generates individual subplots rather than a
#                      single PDF of all subplots. Additional output types need to be defined in PlotsCorrsMakeGrid.R
#        Q_units      = {"m3s","cfs"} = units for plotting flows, Q (m3s = meters^3/second, cfs = cubic feet per second)
#        embedFonts   = TRUE/FALSE = embeds fonts if using PDF format
#        gs_path      = path to Ghostscript installation (necessary if embedding fonts)
#        SAVE         = TRUE/FALSE = save file
#        SIZE         = c(width, height) in inches
OUTPUT_TYPE <- "DAILY-USGS-2x2-INDIV"
SAVE        <- TRUE
SIZE        <- c(7.5,7.5) # used if plotting all together
Q_units     <- "m3s"
source(file.path(dirsGit$ScriptsPlot,"PlotCorrsMakeGrid_RAPID.R"))
rm(OUTPUT_TYPE, SAVE, SIZE, Q_units,grp)

# (6) Trends in peaks map---------------------------------------------------------------------------------------------
#     encoding:
#        color   = Mann-Kendall tau value
#        size    = drainage area, (if size = "area")
#        fill    = significance of MK tau
#     options:
#        size         = one of {"area","none"}
#        colorType    = one of {"CONT", "DISCRETE"} = use gradient or discrete values to relate color to MK trend
#                       ("CONT" goes from blue through purple to red, "DISCRETE" is shades of blue and shades of red)
#        embedFonts   = TRUE/FALSE = embeds fonts if using PDF format
#        gs_path      = path to Ghostscript installation (necessary if embedding fonts)
#        SAVE         = whether or not to save to file
#        SIZE         = c(width, height) in inches

source(file.path(dirsGit$ScriptsPlot,"PlotGageTrendsMap.R"))
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.RData"))
GageTrendMap <- PlotGageTrendsMap(df.Fail.NBI.Gage[rowsToView,], size = "area", colorType = "CONT", SAVE=TRUE, embedFonts = embedFonts,
                                  gs_path = gs_path)
rm(GageTrendMap,df.Fail.NBI.Gage,PlotGageTrendsMap,rowsToView,IDsToView)

# (7) Supplemental bridge & gauge maps ------------------------------------------------------------------------------
#     encoding:
#        color   = fail cause {flood, scour, hurricane, other, gauge (black)}
#        shapes  = circle for bridge, square for gauge
#        fill    = fail date known (filled), unknown (not filled)
#     options:
#        rowsToPlot = rows in df.Fail.NBI.Gage to plot
#        BridgeOnly = plot only bridge, not gauges
#        saveFormat = {"pdf", "eps"}
#        embedFonts   = TRUE/FALSE = embeds fonts if using PDF format
#        gs_path      = path to Ghostscript installation (necessary if embedding fonts)

load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.RData"))
rowsToPlot   <- rowsToView
BridgeOnly   <- FALSE
saveFormat   <- "pdf"
source(file.path(dirsGit$ScriptsPlot,"PlotIndividualBridges.R"))

rm(list=ls())