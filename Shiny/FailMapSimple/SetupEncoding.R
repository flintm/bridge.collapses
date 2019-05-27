# Set up encoding for plots and tables for "Historical analysis of hydraulic bridge collapses in the continental United States"
# Copyright Madeleine Flint, 2016

# COLOR AND FILL PALETTES AND ALPHAS (TRANSPARENCY)
colorsP <- list()
fillsP  <- list()
alphasP <- list()
# SHAPES AND SHAPE SIZES
shapesP       <- list()
sizesP        <- list()
# SETUP LISTS OF LINE TYPES AND WIDTHS 
linesP       <- list()
lwP          <- list()
# SETUP LISTS OF LEVELS AND LABELS (AXES, LEGENDS)
labelsP   <- list()
levelsP   <- list()
legendsP  <- list()
# SETUP LIST OF TEXT SIZE 
textP <- list()

# COLLAPSE CAUSES, DATES, HURRICANES, DEGREE OF REGULATION, AND RATIOS OF DRAINAGE AREA ----------
# Collapse cause
# red/blue/green/purple/orange intense 
#e41a1c #377eb8 #4daf4a #984ea3 #ff7f00
legendsP$Fail  <- "COLLAPSE CAUSE"
labelsP$Fail   <- c("FLOOD","SCOUR","HURRICANE","OTHER")
colorsP$Fail   <-  c(FLOOD = "#377eb8", SCOUR = "#e41a1c", HURRICANE = "#984ea3", OTHER = "#ff7f00")
fillsP$Fail    <- colorsP$Fail

# Collapse date
legendsP$Date    <- "COLLAPSE DATE"
labelsP$Date     <- c("KNOWN","UNKNOWN")
alphasP$FailDate <- c(KNOWN = 0.95, UNKNOWN = 0.5)
shapesP$Date     <- c(KNOWN = 16, UNKNOWN = 1)

# Hurricane
labelsP$Hurr <- c("YES","N.A.")
colorsP$Hurr <- c(YES = "#984ea3", N.A. = NA)
fillsP$Hurr  <- colorsP$Hurr
alphasP$Hurr <- c(HURRICANE = 1, NOH = 0)

# Regulation status
legendsP$Reg <- "FLOW ALTERED"
labelsP$Reg  <- c("REGULATED", "UNREGULATED", "UNKNOWN")
alphasP$Reg  <- c(REGULATED = 0.5, UNGREGULATED = 1, UNKNOWN = 0.3)
colorsP$Reg  <- c(COLLAPSE="white",FAIL="white",MAX="black",PARTIAL="white")
shapesP$Reg  <- c(REGULATED="R", UNREGULATED="U", UNKNOWN="N")
sizesP$Reg   <- c(PRINT = 2, PRES = 4, NOTE = 3)

# From instantaneous or peak data
alphasP$INST  <- c(0.4, 0.95)
legendsP$INST <- "DAILY OR INST/PK DATA"

# Fail and Max coincide
alphasP$FAIL_IS_MAX <- c(0.45, 0.95)
legendsP$FAIL_IS_MAX <- "FAIL AND MAX COINCIDE"

# POINT TYPE (COLLAPSE, MAX, ETC), INCL. CORRELATION PLOTS --------
# Flow and return periods
legendsP$Qtype   <- "DATA"
labelsP$Qtype    <- c("COLLAPSE", "PRE-C MAX", "MAX", "Q100", "Q500","PARTIAL")
# shapesP$Qtype    <- c(COLLAPSE = 19, "PRE-C MAX" = 1, MAX = 1,    "Q100" = 5, "Q500" = 5,"REPORTED" = 73, PARTIAL=19)
shapesP$Qtype    <- c(COLLAPSE = 16, "PRE-C MAX" = 1, MAX = 21,    "Q100" = 0, "Q500" = 0,"REPORTED" = 73, PARTIAL=19)
sizesP$Qtype     <- 2*c(COLLAPSE = 0.4, "PRE-C MAX" = 1, MAX = 1.6,  "Q100" = 1, "Q500" = 1.4, "REPORTED" = 1.2, PARTIAL=0.4)
# shapesP$Qtype    <- c(COLLAPSE = NA, "PRE-C MAX" = NA, MAX = NA,    "Q100" = 5, "Q500" = 5)
# sizesP$Qtype     <- 4*c(COLLAPSE = 1, "PRE-C MAX" = 1, MAX = 1.6,  "Q100" = 1, "Q500" = 1.4)

legendsP$QtypeDIP <- "DATA"
labelsP$QtypeDIP  <- c("COLLAPSE D", "PRE-C MAX D", "MAX D", 
                       "COLLAPSE I/P", "PRE-C MAX I/P", "MAX I/P", 
                       "Q100", "Q500","PARTIAL")
shapesP$QtypeDIP  <- c("COLLAPSE D" = 16, "PRE-C MAX D" = 1, "MAX D" = 21, 
                       "COLLAPSE I/P" = 18, "PRE-C MAX I/P" = 5, "MAX I/P" = 5,
                       "Q100" = 5, "Q500" = 5,"REPORTED" = 73, PARTIAL=19)
sizesP$QtypeDIP   <- 2*c("COLLAPSE D" = 0.4, "PRE-C MAX D" = 1, "MAX D" = 1.6,  
                         "COLLAPSE I/P" = 0.55, "PRE-C MAX I/P" = 0.9, "MAX I/P" = 1.5,
                         "Q100" = 1, "Q500" = 1.4, "REPORTED" = 1.2, PARTIAL=0.4)

# Correlation plots
sizesP$Failure    <- 5
shapesP$Failure   <- 16 #22
levelsP$Corrs     <- c("Q_FAIL_D_USGS",     "Q_FAIL_I_USGS", "Q_FAIL_P_USGS", "Q_FAIL_IP_USGS",
                       "T_FAIL_D_HECD_USGS","T_FAIL_I_HECP_USGS", "T_FAIL_P_HECP_USGS", "T_FAIL_IP_HECP_USGS",
                       "T_FAIL_D_PARTDUR_USGS","T_FAIL_IP_PARTDUR_USGS",
                       "Q_MAX_D_USGS","Q_MAX_P_USGS",
                       "T_MAX_D_HECD_USGS",  "T_MAX_P_HECP_USGS")
labelsP$Corrs     <- c("COLLAPSE DAILY MEAN FLOW [cfs]",  "COLLAPSE INST. [cfs]", "COLLAPSE PEAK [cfs]", "COLLAPSE INST/PEAK FLOW [cfs]",
                       "COLLAPSE DAILY BULLETIN 17B [yr]", "COLLAPSE INST BULLETIN 17B [yr]","COLLAPSE PEAK BULLETIN 17B [yr]", "COLLAPSE I/P BULLETIN 17B [yr]", 
                       "COLLAPSE DAILY PARTIAL DURATION", "COLLAPSE I/P PARTIAL DURATION",
                       "MAX DAILY MEAN [cfs]", "MAX PEAK [cfs]",
                       "MAX DAILY MEAN BULLETIN 17B [yr]", "MAX PEAK BULLETIN 17B [yr]")
sizesP$FailCorrMax   <- c(PRINT = 16, PRES = 20, NOTE = 18)
sizesP$FailCorrMin   <- c(PRINT = 1, PRES = 2, NOTE = 2)

# HYDROLOGICAL DATA TYPES --------------
# teal/orange/purple/pink - data (maybe just use teal and orange)
#1b9e77 #d95f02 #7570b3 #e7298a
legendsP$Data  <- "DATA"
labelsP$Data   <- c("USGS")
levelsP$Data   <- c("USGS","HEC")
colorsP$Data <- c(USGS = "black")
fillsP$Data  <- colorsP$Data
linesP$Data  <- c(USGS = "solid")
lwP$Data     <- c(USGS = 2)

# DISTRIBUTION TYPES -----------
# 4 color code in blue/green family - distributions
#a6cee3 #1f78b4 #b2df8a #33a02c
legendsP$Dists <- "DISTRIBUTION"
labelsP$Dists  <- c(ECDF="ECDF")
colorsP$Dists  <- c(HEC = "#a6cee3",ECDF="#1b9e77")
fillsP$Dists   <- colorsP$Dists
linesP$Dists   <- c(HEC = "solid",ECDF="solid",USGS="solid")
lwP$Dists      <- c(HEC = 2)

# BRIDGE AGE/DESIGN -----------
# for age of bridges - 9 from red -> yellow -> blue
#d73027 #f46d43 #fdae61 #fee090 #ffffbf #e0f3f8 #abd9e9 #74add1 #4575b4
colorsP$BridgeAge9    <- c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", 
                           "#e0f3f8", "#abd9e9", "#74add1", "#4575b4")
alphasP$BridgeDesign  <- c("PRE-1991" = 0.4, "POST-1991" = 0.4)
colorsP$BridgeDesign  <- c("PRE-1991" = colorsP$BridgeAge9[1], "POST-1991" = colorsP$BridgeAge9[9])
shapesP$BridgeDesign <- c(PRE91 = 20, POST91 = 20)
sizesP$BridgeDesign  <- c(PRE91 = 1,  POST91 = 1)

# MATERIAL AND TYPES OF BRIDGES
colorsP$MatTypes <- c("3"='#1f78b4',"1"='#33a02c', "5"='#b2df8a',"7"='#ff7f00',"8"='#fdbf6f',"9"='#a6cee3',"0"='#cab2d6',"U"='gray')
levelsP$Mat      <- c(as.character(c(3,1,5,7,8,9,0)),"U")
labelsP$Mat      <- c("3" = "steel",
                      "1" = "concrete",
                      "5" = "prestressed concrete",
                      "7" = "wood",
                      "8" = "masonry",
                      "9" = "metal",
                      "0" = "other",
                      "U" = "unknown")
names(colorsP$MatTypes) <- labelsP$Mat
levelsP$Type     <- as.character(c(1:17,19:22,0))
labelsP$Type     <- c("1" = "slab", 
                      "2" = "stringer", 
                      "3" = "floorbeam",
                      "4" = "t beam", 
                      "5" = "box multiple", 
                      "6" = "box single",
                      "7" = "frame", 
                      "8" = "orthotropic", 
                      "9" = "truss deck",
                      "10" = "truss thru", 
                      "11" = "arch deck", 
                      "12" = "arch thru",
                      "13" = "suspension", 
                      "14" = "stayed girder", 
                      "15" = "lift",
                      "16" = "bascule", 
                      "17" = "swing", 
                      "19" = "culvert", 
                      "20" = "approach", 
                      "21" = "segmental box",
                      "22" = "channel beam", 
                      "0" = "other")
legendsP$Mat <- "MATERIAL"

# MISCELLANEOUS FOR PLOTS ------------
# Background grays
fillsP$grays  <- c("white",gray(seq(0.95,0.85,length.out=2)))
alphasP$grays <- 0.5

# Significance for trend plot
alphasP$MK   <- c("FALSE" = 0.4, "TRUE" = 0.95)
colorsP$MK   <- c("POS" = colorsP$BridgeAge9[9], "NEG" = colorsP$BridgeAge9[1])
labelsP$MK   <- c("POS","NEG")

# Annotation and header text size
textP$head     <- c(PRINT = 8, PRES = 20, NOTE = 12)
textP$reg      <- c(PRINT = 8, PRES = 14, NOTE = 12)
textP$sub      <- c(PRINT = 8, PRES = 18, NOTE = 12)
textP$labels   <- c(PRINT = 8, PRES = 12, NOTE = 12)
textP$annotate <- c(PRINT = 2.8, PRES = 6, NOTE = 4)
textP$labelcex <- 0.7
