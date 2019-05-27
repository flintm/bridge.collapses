# Fig 5a-d for "Historical Analysis of Hydraulic Bridge Collapses in the Continental United States"
# Makes individual correlation plots for maxes and failure values and makes and saves gridded plot
# Copyright Madeleine Flint, 2016

# A series of individual correlation plots are made and then put together in various configurations based
# on OUTPUT_TYPE. The individual encodings are determined in the calls to PlotAllCorrs. In particular, the
# following can be encoded for each plot:
#     SCALE            = {"LOG", "LINEAR"} x-y scale of entire plot
#     TEXT             = {text.full, text.m, text.m.short} as defined below (~ line 25)
#     ANNOTATE_LOC     = {"UL", "BR"} for upper-left, bottom-right
#     SCALE_VAR        = variable to be used to determine area of circle (DRAIN_SQKM used in paper). NA for no encoding.
#     SCALE_CORRECTION = corrects the maximum circle radius used in case the points plotted are not consistent
#                        across the sub-plots--needs to be manually defined based on the plots being generated
#     ALPHA_VAR        = variable that controls transparency:
#                         FAIL_IS_MAX = collapse and maximum recorded flow coincide
#                         BOOL_KNOWN_FAIL_DATE = collapse date known
#                         FROM_INST = the flow shown is from an instantaneous value
#                         BOOL_REGULATION = there is regulation at the gauge site
#     SHAPE_VAR        = {"BOOL_KNOWN_FAIL_DATE", NA } = variable to be used to control use of filled/unfilled circle.
#                        Currently only set up for known fail date but PlotT1T2corrRegress.R could be modified to support
#                        other options
#     JITTER           = T/F, offset letter showing state of regulation (helps to see small points)
#
# Note that OUTPUT_TYPEs can be defined--look at "Bases.R" to see which variables can be plotted against one another

source(file.path(dirsGit$ScriptsPlot,"MakeGridPlot.R"))
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.RData"))
source(file.path(dirsGit$ScriptsPlot,"PlotAllCorrs.R"))

text.full <- c("LRlog","LR","Rho","Tau")
text.m    <- c("mLog","m","Rho","Tau")
text.m.short <- c("m","Rho","Tau")

CorrPlots <- list()

scaleCorrectionFailIP <- sqrt(max(df.Fail.NBI.Gage[rowsToView[!is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"])],"DRAIN_SQKM"]))/sqrt(max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
controls <- list(SCALE      = "LOG",
                 TEXT       = text.full,
                 LEGEND     = "NONE",
                 SHAPE_VAR  = "BOOL_KNOWN_FAIL_DATE",
                 SCALE_VAR  = "DRAIN_SQKM",
                 outputType = "PRINT",
                 SAVE       = FALSE,
                 ALPHA_VAR  = "FAIL_IS_MAX")

# FAILURE VALUES AND CORRELATIONS ------------------------------
source(file.path(dirsGit$Scripts,"CorrelationOfFailures.R"))
ls.corrs.fail <- CorrelationOfFailures(SAVE = FALSE)

if (grepl("USGS",OUTPUT_TYPE)){
  CorrPlots[["USGS.Q.shape"]]      <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToView,], ls.corrs.fail, TYPE = "FAILQ", ONLY=c("ip"), ANNOTATE_LOC = "BR",
                                                   TEXT = controls$TEXT, ALPHA_VAR = controls$ALPHA_VAR, SHAPE_VAR = controls$SHAPE_VAR,
                                                   SCALE = controls$SCALE, LEGEND = controls$LEGEND, SCALE_VAR = controls$SCALE_VAR, 
                                                   SCALE_CORRECTION = scaleCorrectionFailIP)
  CorrPlots[["USGS.T.shape"]]      <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToView,], ls.corrs.fail, TYPE = "USGS",  ONLY="HECdHECip", ANNOTATE_LOC = "BR",
                                                   TEXT = controls$TEXT, ALPHA_VAR = controls$ALPHA_VAR, SHAPE_VAR = controls$SHAPE_VAR,
                                                   SCALE = controls$SCALE, LEGEND = controls$LEGEND, SCALE_VAR = controls$SCALE_VAR, 
                                                   SCALE_CORRECTION = scaleCorrectionFailIP)
  CorrPlots[["USGS.partd.shape"]]  <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToView,], ls.corrs.fail, TYPE = "USGS",  ONLY="HECdPartInterpd", ANNOTATE_LOC = "TL",
                                                   TEXT = controls$TEXT, ALPHA_VAR = controls$ALPHA_VAR, SHAPE_VAR = controls$SHAPE_VAR,
                                                   SCALE = controls$SCALE, LEGEND = controls$LEGEND, SCALE_VAR = controls$SCALE_VAR, 
                                                   SCALE_CORRECTION = 1)
  CorrPlots[["USGS.partip.shape"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToView,], ls.corrs.fail, TYPE = "USGS",  ONLY="HECipPartInterpip", ANNOTATE_LOC = "BR",
                                                   TEXT = controls$TEXT, ALPHA_VAR = controls$ALPHA_VAR, SHAPE_VAR = controls$SHAPE_VAR,
                                                   SCALE = controls$SCALE, LEGEND = controls$LEGEND, SCALE_VAR = controls$SCALE_VAR, 
                                                   SCALE_CORRECTION = scaleCorrectionFailIP)
}

# CORRELATIONS PART 3:MAKE COMBINED PLOT ------------------------------
if (OUTPUT_TYPE == "DAILY-USGS-2x2"){
  bases <- c("USGSdUSGSip",     "HECdHECip", 
             "HECdPartInterpd", "HECipPartInterpip")
  
  grp   <- list(ggplot_gtable(ggplot_build(CorrPlots[["USGS.Q.shape"]][[bases[1]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["USGS.T.shape"]][[bases[2]]])),
                
                ggplot_gtable(ggplot_build(CorrPlots[["USGS.partd.shape"]][[bases[3]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["USGS.partip.shape"]][[bases[4]]]))              
  )
}

if (OUTPUT_TYPE == "DAILY-USGS-2x2-INDIV"){
  bases <- c("USGSdUSGSip",     "HECdHECip", 
             "HECdPartInterpd", "HECipPartInterpip")
  pmarg <- c(0.02,0.02,0.02,0.02)
  p <-CorrPlots[["USGS.Q.shape"]][[bases[1]]] + theme(plot.margin   = unit(pmarg, "in"))
  ggsave(filename = file.path(dirsGit$Plots,"Fig5a.pdf"), plot = p,width=3.5,height=3.4)
  p <-CorrPlots[["USGS.T.shape"]][[bases[2]]] + theme(plot.margin   = unit(pmarg, "in"))
  ggsave(filename = file.path(dirsGit$Plots,"Fig5b.pdf"), plot = p,width=3.5,height=3.4)
  p <-CorrPlots[["USGS.partd.shape"]][[bases[3]]] + theme(plot.margin   = unit(pmarg, "in"))
  ggsave(filename = file.path(dirsGit$Plots,"Fig5c.pdf"), plot = p,width=3.5,height=3.4)
  p <-CorrPlots[["USGS.partip.shape"]][[bases[4]]] + theme(plot.margin   = unit(pmarg, "in"))
  ggsave(filename = file.path(dirsGit$Plots,"Fig5d.pdf"), plot = p,width=3.5,height=3.4)
  if (embedFonts){
    require(extrafont)
    Sys.setenv(R_GSCMD=gs_path)
    for (file in paste0("Fig5",c("a","b","c","d"),".pdf")){
      embed_fonts(file.path(dirsGit$Plots,file))
    }
  }
}


if (!grepl("INDIV",OUTPUT_TYPE)){
  saveName <- "Fig5"
  MakeGridPlot(grp, SCREEN = TRUE, SAVE = SAVE, SIZE = SIZE, SAVENAME = saveName,
               embedFonts = embedFonts, gs_path = gs_path)
}

rm(list = ls(pattern = "ls."))
rm(p,grp,saveName,MakeGridPlot,PlotAllCors,df.Fail.NBI.Gage,rowsToView,rowsToAnalyzeNLDAS,rowsToAnalyzeVIC,
   CorrelationOfFailures,CorrelationOfMaxes,PlaceAnnotation,PlotT1T2corrRegress,IDsToView,Bases,CorrPlots,
   bases)
rm(list = ls(pattern = "P\\>"))
