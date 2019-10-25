# Fig 5a-d for "Historical Analysis of Hydraulic Bridge Collapses in the Continental United States"
# Makes individual correlation plots for maxes and failure values and makes and saves gridded plot
# Copyright Madeleine Flint, 2016

# A series of individual correlation plots are made and then put together in various configurations based
# on OUTPUT_TYPE. The individual encodings are determined in the calls to PlotAllCorrs. In particular, the
# following can be encoded for each plot:
#     AXES            = {"LOG", "LINEAR"} x-y scale of entire plot
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
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
source(file.path(dirsGit$ScriptsPlot,"PlotAllCorrs_RAPID.R"))

text.full <- c("LRlog","LR","Rho","Tau")
text.m    <- c("mLog","m","Rho","Tau")
text.m.short <- c("m","Rho","Tau")

CorrPlots <- list()

scaleCorrectionFailIP <- sqrt(max(df.Fail.NBI.Gage[rowsToView[!is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"])],"DRAIN_SQKM"]))/sqrt(max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
controls <- list(AXES      = "LOG",
                 TEXT       = text.full,
                 LEGEND     = "NONE",
                 SHAPE_VAR  = "BOOL_KNOWN_FAIL_DATE",
                 SCALE_VAR  = "DRAIN_SQKM",
                 outputType = "PRINT",
                 SAVE       = FALSE,
                 ALPHA_VAR  = "FAIL_IS_MAX")

# CORRELATIONS PART 1: MAX VALUES AND CORRELATIONS ------------------------------
source(file.path(dirsGit$ScriptsPlot,"PlotAllCorrs_RAPID.R"))
source(file.path(dirsGit$Scripts,"Analyze Compare Results","CorrelationsFailMaxEvents.R"))
ls.corrs.max <- CorrelationsFailMaxEvents(TYPES = c("MAXQ","USGS-TMAX","USGS-VICR-TMAX"), SAVE = FALSE)

CorrPlots <- list()

# if (grepl("USGS",OUTPUT_TYPE)){
#   CorrPlots[["USGS.Max"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToView,], ls.corrs.max, TYPE = "MAXQ", TEXT = text.full, SAVE=FALSE, ONLY=NA, ANY=NA, NOT = "VIC",
#                                           ALPHA_VAR = "FAIL_IS_MAX", 
#                                           AXES = "LOG", LEGEND = "NONE", SIZE = c(4,13), outputType = "PRINT",ANNOTATE_LOC = "BR",
#                                           SCALE_VAR = "DRAIN_SQKM", 
#                                           SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToView[!is.na(df.Fail.NBI.Gage[rowsToView,"Q_MAX_P_USGS"])],"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
#   CorrPlots[["USGS.Tmax"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToView,], ls.corrs.max, TYPE = "USGS-TMAX",  SAVE=FALSE, ONLY="HECdHECp", ANY = NA,
#                                            TEXT = text.full,
#                                            ALPHA_VAR = "FAIL_IS_MAX", 
#                                            AXES = "LOG", LEGEND = "NONE", SIZE = c(8.5,8.5), outputType = "PRINT",ANNOTATE_LOC = "BR",
#                                            SCALE_VAR = "DRAIN_SQKM", 
#                                            SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToView[!is.na(df.Fail.NBI.Gage[rowsToView,"Q_MAX_P_USGS"])],"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
# }

if (grepl("VIC",OUTPUT_TYPE)){
  CorrPlots[["VIC.Max"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeVIC,], ls.corrs.max, TYPE = "MAXQ",  SAVE=FALSE, ONLY="VIC", ANY=NA,
                                         TEXT = text.full,
                                         ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                         AXES = "LOG", LEGEND = "NONE", SIZE = c(4,13), outputType = "PRINT",ANNOTATE_LOC = "TL",
                                         SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  CorrPlots[["VIC.Tmax"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeVIC,], ls.corrs.max, TYPE = "USGS-VIC-TMAX",  SAVE=FALSE, ONLY=NA, ANY=c("HEC","PKFQ"),NOT = c("LP3","GEV"),
                                          TEXT = text.m.short, ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                          AXES = "LOG", LEGEND = "NONE", SIZE = c(4,13), outputType = "PRINT",ANNOTATE_LOC = "TL",
                                          SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))                                     
  CorrPlots["VICR.Max"] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeVIC,], ls.corrs.max, TYPE = "MAXQ",  SAVE=FALSE, ONLY="VIC", ANY=NA,
                                        TEXT = text.full,
                                        ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                        AXES = "LOG", LEGEND = "NONE", SIZE = c(4,13), outputType = "PRINT",ANNOTATE_LOC = "TL",
                                        SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  }

if (grepl("NLDAS",OUTPUT_TYPE)){
  CorrPlots[["NLDAS.Max"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,], ls.corrs.max, TYPE = "MAXQ",  SAVE=FALSE, ONLY="NLDAS", ANY=NA,
                                           TEXT = text.full, ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                           AXES = "LOG", LEGEND = "NONE", SIZE = c(4,13), outputType = "PRINT",ANNOTATE_LOC = "TL",
                                           SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  CorrPlots[["NLDAS.Tmax"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,], ls.corrs.max, TYPE = "USGS-NLDAS-TMAX",  SAVE=FALSE, ONLY=NA, ANY=c("HEC","PKFQ"),
                                            TEXT = text.m.short,ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                            AXES = "LOG", LEGEND = "NONE", SIZE = c(4,13), outputType = "PRINT",ANNOTATE_LOC = "BL",
                                            SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))                                     
}
if (grepl("NLDAS",OUTPUT_TYPE) & grepl("VIC",OUTPUT_TYPE)){
  CorrPlots[["VIC.NLDAS.Max"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeVIC,], ls.corrs.max, TYPE = "MAXQ",  SAVE=FALSE, ONLY=c("VIC","NLDAS"), ANY=NA,
                                               TEXT = text.full,ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                               AXES = "LOG", LEGEND = "NONE", SIZE = c(4,13), outputType = "PRINT",ANNOTATE_LOC = "BR",
                                               SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  CorrPlots[["VIC.NLDAS.Tmax"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeVIC,], ls.corrs.max, TYPE = "VIC-NLDAS-TMAX",  SAVE=FALSE, ONLY=NA, ANY=c("PKFQ","HEC"),
                                                TEXT = text.m.short,ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                                AXES = "LOG", LEGEND = "NONE", SIZE = c(4,13), outputType = "PRINT",ANNOTATE_LOC = "BR",
                                                SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
}

# PART 2: FAILURE VALUES AND CORRELATIONS ------------------------------
# source(file.path(dirsGit$Scripts,"CorrelationOfFailures.R"))
# ls.corrs.fail <- CorrelationOfFailures(SAVE = FALSE)
TYPE <- "USGS-VICGRapid"
ls.corrs.fail <- CorrelationsFailMaxEvents(df.Fail.NBI.Gage[rowsToView,],SAVE = FALSE, TYPES = TYPE)

if (grepl("USGS",OUTPUT_TYPE)){
  CorrPlots[["USGS.Q.shape"]]      <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToView,], ls.corrs.fail, TYPE = "FAILQ", ONLY=c("ip"), ANNOTATE_LOC = "BR",
                                                   TEXT = character(0), ALPHA_VAR = controls$ALPHA_VAR, SHAPE_VAR = controls$SHAPE_VAR,
                                                   AXES = controls$AXES, LEGEND = controls$LEGEND, SCALE_VAR = controls$SCALE_VAR, 
                                                   SCALE_CORRECTION = scaleCorrectionFailIP)
  CorrPlots[["USGS.T.shape"]]      <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToView,], ls.corrs.fail, TYPE = "USGS",  ONLY="HECdHECip", ANNOTATE_LOC = "BR",
                                                   TEXT = character(0), ALPHA_VAR = controls$ALPHA_VAR, SHAPE_VAR = controls$SHAPE_VAR,
                                                   AXES = controls$AXES, LEGEND = controls$LEGEND, SCALE_VAR = controls$SCALE_VAR, 
                                                   SCALE_CORRECTION = scaleCorrectionFailIP)
  CorrPlots[["USGS.partd.shape"]]  <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToView,], ls.corrs.fail, TYPE = "USGS",  ONLY="HECdPartInterpd", ANNOTATE_LOC = "TL",
                                                   TEXT = controls$TEXT, ALPHA_VAR = controls$ALPHA_VAR, SHAPE_VAR = controls$SHAPE_VAR,
                                                   AXES = controls$AXES, LEGEND = controls$LEGEND, SCALE_VAR = controls$SCALE_VAR, 
                                                   SCALE_CORRECTION = 1)
  CorrPlots[["USGS.partip.shape"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToView,], ls.corrs.fail, TYPE = "USGS",  ONLY="HECipPartInterpip", ANNOTATE_LOC = "BR",
                                                   TEXT = controls$TEXT, ALPHA_VAR = controls$ALPHA_VAR, SHAPE_VAR = controls$SHAPE_VAR,
                                                   AXES = controls$AXES, LEGEND = controls$LEGEND, SCALE_VAR = controls$SCALE_VAR, 
                                                   SCALE_CORRECTION = scaleCorrectionFailIP)
}
if (grepl("VIC",OUTPUT_TYPE)){
  CorrPlots[["VIC.Q"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeVIC,], ls.corrs.fail, TYPE = "FAILQ",  SAVE=FALSE, ONLY=c("VIC","PM2"), ANY=NA,NOT=NA,
                                       TEXT = text.full,ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                       AXES = "LOG", LEGEND = "NONE", SIZE = c(4,4), outputType = "PRINT",ANNOTATE_LOC = "TL", 
                                       SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  CorrPlots[["VIC.T"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeVIC,], ls.corrs.fail, TYPE = "USGS-VICG",  SAVE=FALSE, ONLY=c("PM2"), ANY=c("HEC","PKFQ"),NOT=c("LP3","GEV","[dpi]P3"),
                                       TEXT = text.full,ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                       AXES = "LOG", LEGEND = "NONE", SIZE = c(12,8), outputType = "PRINT",ANNOTATE_LOC = "TL",
                                       SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  #   CorrPlots[["VIC.T.GB"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeVIC,], ls.corrs.fail, TYPE = "VICg-VICb",  SAVE=FALSE, ONLY=c("PM2"), ANY=c("HEC","PKFQ"),NOT=c("LP3","GEV","[dpi]P3"),
  #                                           TEXT = text.full,ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
  #                                   AXES = "LOG", LEGEND = "NONE", SIZE = c(12,8), outputType = "PRINT",ANNOTATE_LOC = "TL",
  #                                   SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  
  CorrPlots[["VIC.T.PKFQ"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeVIC,], ls.corrs.fail, TYPE = "USGS-VICG",  SAVE=FALSE, ONLY=c("PKFQ","PM2"), ANY=NA,NOT=c("LP3","GEV","[dpi]P3"),
                                            TEXT = text.m.short, ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                            AXES = "LOG", LEGEND = "NONE", SIZE = c(4,8.5), outputType = "PRINT",ANNOTATE_LOC = "TL",
                                            SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  CorrPlots[["VICR.T.PKFQ"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeVIC,], ls.corrs.fail, TYPE = "USGS-VICGRapid",  SAVE=FALSE,
                                       TEXT = text.full,ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                       AXES = "LOG", LEGEND = "NONE", SIZE = c(12,8), outputType = "PRINT",ANNOTATE_LOC = "TL",
                                       SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  
  CorrPlots[["VIC.part"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeVIC,], ls.corrs.fail, TYPE = "USGS-PART-VICg-PART",  SAVE=FALSE, ONLY=c("VICG","PM2"), ANY=NA,NOT="VICB",
                                          TEXT = text.m.short, ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                          AXES = "LOG", LEGEND = "NONE", SIZE = c(4,8.5), outputType = "PRINT",ANNOTATE_LOC = "TL",
                                          SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  CorrPlots[["VIC.best"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeVIC,], ls.corrs.fail, TYPE = "USGS-BEST-VICg",  SAVE=FALSE, ONLY=NA, ANY=NA,NOT=NA,
                                          TEXT = text.full,ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                          AXES = "LOG", LEGEND = "NONE", SIZE = c(4,4), outputType = "PRINT", ANNOTATE_LOC = "TL",
                                          SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
}

if (grepl("NLDAS",OUTPUT_TYPE)){
  CorrPlots[["NLDAS.Q"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,], ls.corrs.fail, TYPE = "FAILQ",  SAVE=FALSE, ONLY=c("NLDAS","PM2"), ANY=NA,NOT=NA,
                                         TEXT = text.full,ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                         AXES = "LOG", LEGEND = "NONE", SIZE = c(4,4), outputType = "PRINT",ANNOTATE_LOC = "TL", 
                                         SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  CorrPlots[["NLDAS.T"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,], ls.corrs.fail, TYPE = "USGS-NLDAS",  SAVE=FALSE, ONLY=c("PM2"), ANY=c("HEC","PKFQ"),NOT=c("Part","part"),
                                         TEXT = text.m.short,ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                         AXES = "LOG", LEGEND = "NONE", SIZE = c(4,8.5), outputType = "PRINT",ANNOTATE_LOC = "TL",
                                         SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  CorrPlots[["NLDAS.T.GB"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,], ls.corrs.fail, TYPE = "NLDASg-NLDASb",  SAVE=FALSE, ONLY=c("PM2"), ANY=c("HEC","PKFQ"),NOT=c("LP3","GEV","[dpi]P3"),
                                            TEXT = text.full,ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                            AXES = "LOG", LEGEND = "NONE", SIZE = c(12,8), outputType = "PRINT",ANNOTATE_LOC = "TL",
                                            SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  CorrPlots[["NLDAS.part"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,], ls.corrs.fail, TYPE = "USGS-NLDAS",  SAVE=FALSE, ONLY=c("NLDAS","PM2","g"), ANY=c("Part","part"),NOT=NA,
                                            TEXT = text.m.short,ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                            AXES = "LOG", LEGEND = "NONE", SIZE = c(4,8.5), outputType = "PRINT",ANNOTATE_LOC = "TL",
                                            SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  CorrPlots[["NLDAS.best"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,], ls.corrs.fail, TYPE = "USGS-BEST-NLDAS",  SAVE=FALSE, ONLY=NA, ANY=NA,NOT=NA,
                                            TEXT = text.full,ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                            AXES = "LOG", LEGEND = "NONE", SIZE = c(4,4), outputType = "PRINT",ANNOTATE_LOC = "TL",
                                            SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
}

if (grepl("NLDAS",OUTPUT_TYPE) & grepl("VIC",OUTPUT_TYPE)){
  CorrPlots[["VIC.NLDAS.Q"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeVIC,], ls.corrs.fail, TYPE = "FAILQ",  SAVE=FALSE, ONLY=c("NLDAS","PM2","VIC"), ANY=NA,NOT=NA,
                                             TEXT = text.full, ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                             AXES = "LOG", LEGEND = "NONE", SIZE = c(4,4), outputType = "PRINT",ANNOTATE_LOC = "TL", 
                                             SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  CorrPlots[["VIC.NLDAS.T"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeVIC,], ls.corrs.fail, TYPE = "VIC-NLDAS",  SAVE=FALSE, ONLY=c("PM2"), ANY=c("HEC","PKFQ"),NOT=c("Part","part"),
                                             TEXT = text.m.short,ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                             AXES = "LOG", LEGEND = "NONE", SIZE = c(4,8.5), outputType = "PRINT",ANNOTATE_LOC = "TR",
                                             SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
  CorrPlots[["VIC.NLDAS.Part"]] <- PlotAllCorrs(df.Fail.NBI.Gage[rowsToAnalyzeVIC,], ls.corrs.fail, TYPE = "VIC-NLDAS",  SAVE=FALSE, ONLY=NA, ANY=c("Part","part"),NOT=c("HEC","PKFQ"),
                                                TEXT = text.m.short,ALPHA_VAR = "BOOL_NSE_D_DVICG_POS", 
                                                AXES = "LOG", LEGEND = "NONE", SIZE = c(4,8.5), outputType = "PRINT",ANNOTATE_LOC = "BR",
                                                SCALE_VAR = "DRAIN_SQKM", SCALE_CORRECTION = max(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"DRAIN_SQKM"])/max(df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]))
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

if (OUTPUT_TYPE == "DAILY-USGS-3x2"){
  bases <- c("USGSdUSGSp",      "HECdHECp",            
             "USGSdUSGSip",     "HECdHECip", 
             "HECdPartInterpd", "HECipPartInterpip")
  
  grp   <- list(ggplot_gtable(ggplot_build(CorrPlots[["USGS.Max"]][[bases[1]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["USGS.Tmax"]][[bases[2]]])),
                
                ggplot_gtable(ggplot_build(CorrPlots[["USGS.Q"]][[bases[3]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["USGS.T"]][[bases[4]]])),
                
                ggplot_gtable(ggplot_build(CorrPlots[["USGS.partd"]][[bases[5]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["USGS.partip"]][[bases[6]]]))              
  )
}

if (OUTPUT_TYPE == "DAILY-USGS-3x2-INDIV"){
  bases <- c("USGSdUSGSp",      "HECdHECp",            
             "USGSdUSGSip",     "HECdHECip", 
             "HECdPartInterpd", "HECipPartInterpip")
  p <- CorrPlots[["USGS.Max"]][[bases[1]]] + theme(plot.margin   = unit(c(0,0.02,0,0.02), "in"))
  ggsave(filename = file.path(dirsGit$Plots,"Fig5a.pdf"), plot = p,width=3.5,height=3.3)
  p <- CorrPlots[["USGS.Tmax"]][[bases[2]]] + theme(plot.margin   = unit(c(0,0.02,0,0.02), "in"))
  ggsave(filename = file.path(dirsGit$Plots,"Fig5b.pdf"), plot = p,width=3.5,height=3.3)
  p <- CorrPlots[["USGS.Q"]][[bases[3]]] + theme(plot.margin   = unit(c(0,0.02,0,0.02), "in"))
  ggsave(filename = file.path(dirsGit$Plots,"Fig5c.pdf"), plot = p,width=3.5,height=3.3)
  p <- CorrPlots[["USGS.T"]][[bases[4]]] + theme(plot.margin   = unit(c(0,0.02,0,0.02), "in"))
  ggsave(filename = file.path(dirsGit$Plots,"Fig5d.pdf"), plot = p,width=3.5,height=3.3)
  p <-  CorrPlots[["USGS.partd"]][[bases[5]]] + theme(plot.margin   = unit(c(0,0.02,0,0.02), "in"))
  ggsave(filename = file.path(dirsGit$Plots,"Fig5e.pdf"), plot =p,width=3.5,height=3.3)
  p <- CorrPlots[["USGS.partip"]][[bases[6]]] + theme(plot.margin   = unit(c(0,0.02,0,0.02), "in"))
  ggsave(filename = file.path(dirsGit$Plots,"Fig5f.pdf"), plot = p,width=3.5,height=3.3)
  
}

if (OUTPUT_TYPE == "DAILY-USGS-VICG-3x2"){
  bases <- c("USGSdVICGd","HECdHECd",            
             "USGSdVICgdPM2","HECdHECdPM2", 
             "HECdHECy",  "USGSpartVICGpartPM2")
  
  grp   <- list(ggplot_gtable(ggplot_build(CorrPlots[["VIC.Max"]][[bases[1]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.Tmax"]][[bases[2]]])),
                
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.Q"]][[bases[3]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.T"]][[bases[4]]])),
                
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.best"]][[bases[5]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.part"]][[bases[6]]]))
                
  )
}

if (OUTPUT_TYPE == "DAILY-USGS-VICG-3x2-PKFQ"){
  bases <- c("USGSdVICGd","PKFQdVICgPKFQd",            
             "USGSdVICgdPM2","PKFQdVICgPKFQdPM2", 
             "PKFQdVICgPKFQy",  "USGSpartVICGpartPM2")
  
  grp   <- list(ggplot_gtable(ggplot_build(CorrPlots[["VIC.Max"]][[bases[1]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.Tmax"]][[bases[2]]])),
                
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.Q"]][[bases[3]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.T"]][[bases[4]]])),
                
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.best"]][[bases[5]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.part"]][[bases[6]]]))
                
  )
}

if (OUTPUT_TYPE == "DAILY-USGS-VICGRAPID-3x2-PKFQ"){
  bases <- c("USGSdVICGRd","PKFQdVICRgPKFQd",            
             "USGSdVICRgdPM2","PKFQdVICRgPKFQdPM2", 
             "PKFQdVICRgPKFQy",  "USGSpartVICRGpartPM2")
  grp   <- list(ggplot_gtable(ggplot_build(CorrPlots[["VIC.Max"]][[bases[1]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.Tmax"]][[bases[2]]])),
                
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.Q"]][[bases[3]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.T"]][[bases[4]]])),
                
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.best"]][[bases[5]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.part"]][[bases[6]]]))
                
  )
}

if (OUTPUT_TYPE == "DAILY-USGS-NLDASG-3x2"){
  bases <- c("USGSdNLDASGd",    "HECdNLDASgHECd",            
             "USGSdNLDASgPM2",  "HECdNLDASgHECPM2", 
             "HECdNLDASgHECy",  "USGSpartNLDASgPartPM2")
  grp   <- list(ggplot_gtable(ggplot_build(CorrPlots[["NLDAS.Max"]][[bases[1]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["NLDAS.Tmax"]][[bases[2]]])),
                
                ggplot_gtable(ggplot_build(CorrPlots[["NLDAS.Q"]][[bases[3]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["NLDAS.T"]][[bases[4]]])),
                
                ggplot_gtable(ggplot_build(CorrPlots[["NLDAS.best"]][[bases[5]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["NLDAS.part"]][[bases[6]]]))
                
  )
}

if (OUTPUT_TYPE == "DAILY-USGS-VICG-NLDASG-3x2-PKFQ"){
  bases <- c("PKFQdVICgPKFQd","PKFQdNLDASgPKFQd",            
             "PKFQdVICgPKFQdPM2","PKFQdNLDASgPKFQdPM2", 
             "VICgPartPM2NLDASgPartPM2",  "VICgPKFQdPM2NLDASgPKFQdPM2")
  
  grp   <- list(ggplot_gtable(ggplot_build(CorrPlots[["VIC.Tmax"]][[bases[1]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["NLDAS.Tmax"]][[bases[2]]])),
                
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.T.PKFQ"]][[bases[3]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["NLDAS.T"]][[bases[4]]])),
                
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.NLDAS.Part"]][[bases[5]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.NLDAS.T"]][[bases[6]]]))              
  )
}

if (OUTPUT_TYPE == "DAILY-USGS-VICG-NLDASG-3x3-PKFQ"){
  bases <- c("PKFQdVICgPKFQd","PKFQdNLDASgPKFQd", "VICgPKFQdNLDASgPKFQd",           
             "PKFQdVICgPKFQdPM2","PKFQdNLDASgPKFQdPM2", "VICgPKFQdPM2NLDASgPKFQdPM2",
             "USGSpartVICGpartPM2","USGSpartNLDASgPartPM2","VICgPartPM2NLDASgPartPM2")
  
  grp   <- list(ggplot_gtable(ggplot_build(CorrPlots[["VIC.Tmax"]][[bases[1]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["NLDAS.Tmax"]][[bases[2]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.NLDAS.Tmax"]][[bases[3]]])),
                
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.T.PKFQ"]][[bases[4]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["NLDAS.T"]][[bases[5]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.NLDAS.T"]][[bases[6]]])),
                
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.part"]][[bases[7]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["NLDAS.part"]][[bases[8]]])),
                ggplot_gtable(ggplot_build(CorrPlots[["VIC.NLDAS.Part"]][[bases[9]]]))
  )              
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
