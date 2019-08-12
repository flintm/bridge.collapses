# Set up encoding for plots and tables for historical bridge failures manuscript

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
# Failure cause
# red/blue/green/purple/orange intense 
#e41a1c
#377eb8
#4daf4a
#984ea3
##ff7f00
legendsP$Fail  <- "COLLAPSE CAUSE"
labelsP$Fail <- c("FLOOD","SCOUR","HURRICANE","OTHER")
colorsP$Fail <-  c(FLOOD = "#377eb8", SCOUR = "#e41a1c", HURRICANE = "#984ea3", OTHER = "#ff7f00")
fillsP$Fail  <- colorsP$Fail
# Failure date
legendsP$Date  <- "COLLAPSE DATE"
labelsP$Date <- c("KNOWN","UNKNOWN")
alphasP$FailDate <- c(KNOWN = 0.95, UNKNOWN = 0.5)
shapesP$Date <- c(KNOWN = 16, UNKNOWN = 1)
# Hurricane
labelsP$Hurr <- c("YES","N.A.")
colorsP$Hurr <- c(YES = "#984ea3", N.A. = NA)
fillsP$Hurr  <- colorsP$Hurr
alphasP$Hurr <- c(HURRICANE = 1, NOH = 0)
# Regulation status
legendsP$Reg   <- "FLOW ALTERED"
labelsP$Reg   <- c("REGULATED", "UNREGULATED", "UNKNOWN")
alphasP$Reg  <- c(REGULATED = 0.5, UNGREGULATED = 1, UNKNOWN = 0.3)
colorsP$Reg  <- c(COLLAPSE="white",FAIL="white",MAX="black",PARTIAL="white")
shapesP$Reg  <- c(REGULATED="R", UNREGULATED="U", UNKNOWN="N")
sizesP$Reg   <- c(PRINT = 2, PRES = 4, SHINY = 2, NOTE = 2)
# Drainage Area ratios
legendsP$VICgRat <- "VIC DRAIN AREA /\n USGS DRAIN AREA"
# NSE
alphasP$NSE_D_DVICG <- c(0.4, 0.95)
legendsP$NSE_D_DVICG <- "NSE > 0"
# From instantaneous or peak data
alphasP$INST <- c(0.4, 0.95)
legendsP$INST <- "DAILY OR INST/PK DATA"
# Fail and Max coincide
alphasP$FAIL_IS_MAX <- c(0.45, 0.95)
legendsP$FAIL_IS_MAX <- "FAIL AND MAX COINCIDE"

# POINT TYPE (COLLAPSE, MAX, ETC), INCL. CORRELATION PLOTS --------
legendsP$Qtype <- "DATA"
labelsP$Qtype <- c("COLLAPSE", "PRE-C MAX", "MAX", "Q100", "Q500","PARTIAL")
# shapesP$Qtype  <- c(COLLAPSE = 19, "PRE-C MAX" = 1, MAX = 1,    "Q100" = 5, "Q500" = 5,"REPORTED" = 73, PARTIAL=19)
shapesP$Qtype  <- c(COLLAPSE = 16, "PRE-C MAX" = 1, MAX = 21,    "Q100" = 0, "Q500" = 0,"REPORTED" = 73, PARTIAL=19)
sizesP$Qtype   <- 2*c(COLLAPSE = 0.4, "PRE-C MAX" = 1, MAX = 1.6,  "Q100" = 1, "Q500" = 1.4, "REPORTED" = 1.2, PARTIAL=0.4)
# shapesP$Qtype  <- c(COLLAPSE = NA, "PRE-C MAX" = NA, MAX = NA,    "Q100" = 5, "Q500" = 5)
# sizesP$Qtype   <- 4*c(COLLAPSE = 1, "PRE-C MAX" = 1, MAX = 1.6,  "Q100" = 1, "Q500" = 1.4)

legendsP$QtypeDIP <- "DATA"
labelsP$QtypeDIP <- c("COLLAPSE D", "PRE-C MAX D", "MAX D", 
                      "COLLAPSE I/P", "PRE-C MAX I/P", "MAX I/P", 
                      "Q100", "Q500","PARTIAL")
shapesP$QtypeDIP  <- c("COLLAPSE D" = 16, "PRE-C MAX D" = 1, "MAX D" = 21, 
                       "COLLAPSE I/P" = 18, "PRE-C MAX I/P" = 5, "MAX I/P" = 5,
                       "Q100" = 5, "Q500" = 5,"REPORTED" = 73, PARTIAL=19)
sizesP$QtypeDIP   <- 2*c("COLLAPSE D" = 0.4, "PRE-C MAX D" = 1, "MAX D" = 1.6,  
                         "COLLAPSE I/P" = 0.55, "PRE-C MAX I/P" = 0.9, "MAX I/P" = 1.5,
                         "Q100" = 1, "Q500" = 1.4, "REPORTED" = 1.2, PARTIAL=0.4)

sizesP$Failure    <- 5
shapesP$Failure <- 16 #22
levelsP$Corrs  <- c("Q_FAIL_D_USGS",     "Q_FAIL_I_USGS", "Q_FAIL_P_USGS", "Q_FAIL_IP_USGS","Q_FAILPM2_DVICG",
                    "T_FAIL_D_HECD_USGS", 
                    "T_FAIL_I_HECP_USGS", "T_FAIL_P_HECP_USGS", "T_FAIL_IP_HECP_USGS",
                    "T_PARTIAL_USGS","T_FAIL_D_PARTDUR_USGS","T_FAIL_IP_PARTDUR_USGS",
                    "T_FAILPM2_HEC_DVICG", 
                    "T_FAILPM2_HEC_NLDASB","T_FAILPM2_HEC_NLDASG","T_FAILPM2_PARTDUR_NLDASB", "T_FAILPM2_PARTDUR_NLDASG", "T_FAILYRMAX_HEC_NLDASB","T_FAILYRMAX_HEC_NLDASG",
                    "Q_FAILPM2_NLDASB","Q_FAILPM2_NLDASG","Q_MAX_NLDASB","Q_MAX_NLDASG","Q_FAILYRMAX_NLDASB","Q_FAILYRMAX_NLDASG",
                    "Q_MAX_D_USGS","Q_MAX_P_USGS","Q_MAX_DVICG",
                    "T_MAX_D_HECD_USGS",  "T_MAX_P_HECP_USGS", 
                    "T_MAX_HEC_DVICG", 
                    "T_FAILPM2_PARTDUR_DVICG", "T_FAILYRMAX_HEC_DVICG",
                    "T_FAIL_D_PKFQD_USGS", "T_FAIL_P_PKFQP_USGS","T_MAX_D_PKFQD_USGS", "T_MAX_PKFQ_PEAK", 
                    "T_FAILPM2_PKFQD_DVICG", "T_MAX_PKFQD_DVICG","T_FAILPM2_PKFQD_DVICB", "T_MAX_PKFQD_DVICB",
                    "T_FAILPM2_PKFQD_NLDASG", "T_MAX_PKFQD_NLDASG","T_FAILPM2_PKFQD_NLDASB", "T_MAX_PKFQD_NLDASB",
                    "Q_FAIL_DVICG_RAPID", "Q_FAILPM2_DVICG_RAPID", "Q_FAILYRMAX_DVICG_RAPID",
                    "Q_MAX_DVICG_RAPID", "Q_MAXPREFAIL_DVICG_RAPID",
                    "T_FAIL_PKFQD_DVICG_RAPID", "T_FAILPM2_PKFQD_DVICG_RAPID", "T_FAILYRMAX_PKFQD_DVICG_RAPID",
                    "T_MAX_PKFQD_DVICG_RAPID", "T_MAXPREFAIL_PKFQD_DVICG_RAPID")

labelsP$Corrs  <- c("COLLAPSE DAILY MEAN FLOW [cfs]",  "COLLAPSE INST. [cfs]", "COLLAPSE PEAK [cfs]", "COLLAPSE INST/PEAK FLOW [cfs]", "DAYMET-VIC COLLAPSE DAILY MEAN [cfs]",
                    "COLLAPSE DAILY BULLETIN 17B [yr]", 
                    "COLLAPSE INST BULLETIN 17B [yr]","COLLAPSE PEAK BULLETIN 17B [yr]", "COLLAPSE I/P BULLETIN 17B [yr]", 
                    "COLLAPSE PARTIAL DURATION LM", "COLLAPSE DAILY PARTIAL DURATION", "COLLAPSE I/P PARTIAL DURATION",
                    "DAYMET-VIC COLLAPSE DAILY BULLETIN 17B [yr]",
                    "NLDAS-VIC-B COLLAPSE BULLETIN 17B [yr]", "NLDAS-VIC COLLAPSE B.17B [yr]", "NLDAS-VIC-B COLLAPSE PARTIAL DURATION [yr]", "NLDAS-VIC COLLAPSE PARTIAL DURATION [yr]","NLDAS-VIC-B MAX COLLAPSE YR. BULLETIN 17B [yr]","NLDAS-VIC MAX COLLAPSE YR. B.17B [yr]",
                    "NLDAS-VIC-B COLLAPSE [cfs]", "NLDAS-VIC COLLAPSE [cfs]", "NLDAS-VIC-BRIDGE MAX [cfs]", "NLDAS-VIC-GAGE MAX [cfs]", "NLDAS-VIC-BRIDGE MAX COLLAPSE YEAR [cfs]", "NLDAS-VIC-GAGE MAX COLLAPSE YEAR [cfs]",
                    "MAX DAILY MEAN [cfs]", "MAX PEAK [cfs]", "DAYMET-VIC MAX DAILY MEAN [cfs]",
                    "MAX DAILY MEAN BULLETIN 17B [yr]", "MAX PEAK BULLETIN 17B [yr]",
                    "DAYMET-VIC MAX DAILY MEAN BULLETIN 17B [yr]",
                    "DAYMET-VIC PARTIAL DURATION", "DAYMET-VIC MAX COLLAPSE YR. B.17B [yr]",
                    "COLLAPSE DAILY B.17B [yr]", "COLLAPSE PEAK B.17B [yr]", "MAX DAILY B.17B [yr]", "MAX PEAK B.17B [yr]",
                    "DAYMET-VIC COLLAPSE DAILY B.17B [yr]", "DAYMET-VIC MAX DAILY B.17B [yr]", "DAYMET-VIC-B COLLAPSE DAILY B.17B [yr]", "DAYMET-VIC-B MAX DAILY B.17B [yr]",
                    "NLDAS-VIC COLLAPSE DAILY B.17B [yr]", "NLDAS-VIC MAX DAILY B.17B [yr]", "NLDAS-VIC-B COLLAPSE DAILY B.17B [yr]","NLDAS-VIC-B MAX DAILY B.17B [yr]",
                    "DAYMET-VIC-RAPID COLLAPSE DAILY MEAN FLOW [cfs]","DAYMET-VIC-RAPID COLLAPSE DAILY MEAN FLOW [cfs]","DAYMET-VIC-RAPID MAX COLLAPSE YR. DAILY MEAN FLOW [cfs]",
                    "DAYMET-VIC-RAPID MAX DAILY MEAN FLOW [cfs]", "DAYMET-VIC-RAPID MAX PRE-COLLAPSE DAILY MEAN FLOW [cfs]",
                    "DAYMET-VIC-RAPID COLLAPSE DAILY B.17B [yr]", "DAYMET-VIC-RAPID COLLAPSE DAILY B.17B [yr]", "DAYMET-VIC-RAPID MAX COLLAPSE YR. B.17B [yr]",
                    "DAYMET-VIC-RAPID MAX DAILY B.17B [yr]", "DAYMET-VIC-RAPID MAX PRE-C DAILY B.17B [yr]")
sizesP$FailCorr   <- c(PRINT = 4, PRES = 8, SHINY = 8, NOTE = 6)
sizesP$FailCorrMin   <- c(PRINT = 1, PRES = 2, SHINY = 2, NOTE = 2)
sizesP$FailCorrMax   <- c(PRINT = 4, PRES = 8, SHINY = 8, NOTE = 6)

# HYDROLOGICAL DATA TYPES --------------
# teal/orange/purple/pink - data (maybe just use teal and orange)
#1b9e77
#d95f02
#7570b3
#e7298a
legendsP$Data  <- "DATA"
labelsP$Data   <- c("USGS", "VICG","VICB","VICG","VICB","VIC", "NLDAS","USGS")
levelsP$Data   <- c("USGS","VIC_GAGE","VIC_BRIDGE","VICG","VICB","VIC","NLDAS","HEC")
# colorsP$Data <- c(USGS = "#1b9e77", VICG = "#d95f02", VICB = "#e7298a", VIC = "#d95f02", NLDAS = "#7570b3")
colorsP$Data <- c(USGS = "black", VICG = "#d95f02", VICB = "#e7298a", VIC = "#d95f02", NLDAS = "#7570b3")
fillsP$Data  <- colorsP$Data
linesP$Data  <- c(USGS = "solid",  VICG = "dashed", VICB = "dotted",VIC = "dashed", NLDAS = "dotted")
lwP$Data     <- c(USGS = 2,  VICG = 1, VICB = 1, VIC = 1, NLDAS = 1)

# FLOW FREQUENCY ANALYSIS METHOD TYPES -----------
# 4 color code in blue/green family 
#a6cee3
#1f78b4
#b2df8a
#33a02c
legendsP$Method <- "METHOD"
labelsP$Method  <- c(HEC = "HEC", PKFQ="PKFQ",HECD = "HECD", PKFQD="PKFQD",HECP = "HECP", PKFQP = "PKFQP",ECDF="ECDF")
colorsP$Method <- c(HEC = "#a6cee3", PKFQ = "#1f78b4", ECDF="#1b9e77", VICG = "#d95f02", VICB = "#e7298a")
fillsP$Method  <- colorsP$Method
linesP$Method <- c(HEC = "solid", PKFQ = "dotted", ECDF="solid", USGS="solid")
lwP$Method    <- c(HEC = 2, PKFQ = 1, B17C = 1)

# BRIDGE AGE/DESIGN -----------
# for age of bridges - 9 from red -> yellow -> blue
#d73027
#f46d43
#fdae61
#fee090
#ffffbf
#e0f3f8
#abd9e9
#74add1
#4575b4
colorsP$BridgeAge9    <- c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", 
                           "#e0f3f8", "#abd9e9", "#74add1", "#4575b4")
alphasP$BridgeDesign  <- c("PRE-1991" = 0.4, "POST-1991" = 0.4)
colorsP$BridgeDesign  <- c("PRE-1991" = colorsP$BridgeAge9[1], "POST-1991" = colorsP$BridgeAge9[9])
shapesP$BridgeDesign <- c(PRE91 = 20, POST91 = 20)
sizesP$BridgeDesign  <- c(PRE91 = 1,  POST91 = 1)

# MATERIAL AND TYPES OF BRIDGES
colorsP$MatTypes <- c("3"='#1f78b4',"1"='#33a02c', "5"='#b2df8a',"7"='#ff7f00',"8"='#fdbf6f',"9"='#a6cee3',"0"='#cab2d6')#,"7"='#6a3d9a')
levelsP$Mat      <- as.character(c(3,1,5,7,8,9,0))
labelsP$Mat      <- c("3" = "steel",
                      "1" = "concrete",
                      "5" = "prestressed concrete",
                      "7" = "wood",
                      "8" = "masonry",
                      "9" = "metal",
                      "0" = "other")
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

# Distribution / analysis type
colorsP$Dist <- c(nominal = "black", median = "gray", kernel = "#606060")
linesP$Dist <- c(nominal = "dashed", median = "dotted",kernel = "solid")
labelsP$Dist <- c(nominal = "Nominal",median = "Median", kernel ="Kernel")
labelsP$Delta <- c(nominal = "Nominal",median = "Median", kernel ="Kernel", hazard = "Hazard")

# MISCELLANEOUS FOR PLOTS ------------
# Background grays
# fillsP$grays          <- gray(seq(0.3,0.8,length.out = 3))
fillsP$grays <- c("white",gray(seq(0.95,0.85,length.out=2)))
alphasP$grays          <- 0.5
alphasP$emph <- c(emph = 1, back = 0.6)
linesP$emph <- c(emph = 2, back = 1)
sizesP$emph <- c(emph = 1.25, back = 0.75)
# Significance for trend plot
alphasP$MK   <- c("FALSE" = 0.4, "TRUE" = 0.95)
colorsP$MK   <- c("POS" = colorsP$BridgeAge9[9], "NEG" = colorsP$BridgeAge9[1])
labelsP$MK   <- c("POS","NEG")
# Annotation and header text size
textP$head <- c(PRINT = 8, PRES = 20, SHINY = 12, NOTE = 10)
textP$reg  <- c(PRINT = 8, PRES = 14, SHINY = 12, NOTE = 10)
textP$sub  <- c(PRINT = 8, PRES = 18, SHINY = 12, NOTE = 10)
textP$labels <- c(PRINT = 8, PRES = 12, SHINY = 12, NOTE = 10)
textP$labelcex <- 0.7
textP$annotate <- c(PRINT = 2.8, PRES = 6, SHINY = 4, NOTE = 10)
