# Make tables for manuscript on historical bridge failures
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
source(file.path(dirsGit$ScriptsPlotDir,"SetupEncoding.R"))

library(stargazer)

BridgesDataFrame <- df.Fail.NBI.Gage[rowsToView,]

BridgesDataFrame$LABEL <- paste(sapply(BridgesDataFrame$YR_BLT_EST, function(i) ifelse(!is.na(i),substr(i,1,4), "N.A.")),
                                "--",
                                substr(BridgesDataFrame[,"YR_FAIL"],1,4),
                                "--",
                                sapply(1:nrow(BridgesDataFrame), function(i) df.States[df.States$STFIPS==BridgesDataFrame[i,"STFIPS"],"STATE_CODE"]),
                                sep="")
BridgesDataFrame <- BridgesDataFrame[order(BridgesDataFrame$FAIL_CAUS_CODE,BridgesDataFrame$YR_BLT_EST, decreasing = FALSE, na.last = TRUE),]

BridgesDataFrame$FAIL_CAUS_CODE  <- as.character(BridgesDataFrame$FAIL_CAUS_CODE)
BridgesDataFrame$FAIL_CAUS_CODE  <- factor(BridgesDataFrame$FAIL_CAUS_CODE, levels=c("flood","scour","hurricane","other hydraulic"), labels = labelsP$Fail)

BridgesDataFrame$BOOL_KNOWN_FAIL_DATE <- factor(BridgesDataFrame$BOOL_HAS_FAIL_DATE, levels = c(TRUE, FALSE), labels = labelsP$Date)

BridgesDataFrame$BOOL_WAS_HURRICANE                                 <- nchar(BridgesDataFrame$COMMENT_LINKED_HURRICANE)!=0
BridgesDataFrame$HURRICANE                                     <- labelsP$Hurr[2]
BridgesDataFrame[BridgesDataFrame$BOOL_WAS_HURRICANE, "HURRICANE"]  <- labelsP$Hurr[1]
BridgesDataFrame$HURRICANE                                     <- factor(BridgesDataFrame$HURRICANE, levels = labelsP$Hurr, labels = labelsP$Hurr)
BridgesDataFrame$FROM_INST <- !is.na(BridgesDataFrame$Q_FAIL_I_USGS)

# set up hurricane, drainage area, and fail date symbols in label
rows <- BridgesDataFrame$BOOL_WAS_HURRICANE & !is.na(BridgesDataFrame$BOOL_WAS_HURRICANE)
labs <- character(nrow(BridgesDataFrame))
labs[rows] <- "H"
# ratio of drainage area bridge to gauge
rows <- BridgesDataFrame$AREA_RATIO_NHD >= 1.2
labs[rows] <- paste(labs[rows],"+",sep="")
# >= 1.4
rows <- BridgesDataFrame$AREA_RATIO_NHD >= 1.4
labs[rows] <- paste(labs[rows],"+",sep="")
# <= 0.8
rows <- BridgesDataFrame$AREA_RATIO_NHD <= 0.8
labs[rows] <- paste(labs[rows],"-",sep="")
# <= 0.6
rows <- BridgesDataFrame$AREA_RATIO_NHD <= 0.6
labs[rows] <- paste(labs[rows],"-",sep="")
# fail date
rows <- BridgesDataFrame$BOOL_KNOWN_FAIL_DATE == "UNKNOWN"
labs[rows] <- paste(labs[rows],"*",sep="")

posslabs <- unique(labs[labs!=""])
labs[!labs==""] <- paste("${}^{",labs[!labs==""],"}$",sep = "")
BridgesDataFrame$LABEL <- paste(BridgesDataFrame$LABEL,labs,sep=" ")

dataTypeLab <- substr(BridgesDataFrame[,"T_FAIL_BEST_HEC_SOURCE"],1,1)
dataTypeLab[dataTypeLab!="D"] <- paste("${}^{",dataTypeLab[dataTypeLab!="D"],"}$",sep = "")
dataTypeLab[dataTypeLab=="D"] <- ""

# Table 1: failure and max data and return periods -------------------------------------------------------------------------------------------------------------------
cols <- c("LABEL", "STAID", "Q_FAIL_D_USGS",    "Q_FAIL_IP_USGS",    "Q_MAX_D_USGS",     "Q_MAX_P_USGS",     "Q_MAXPREFAIL_D_USGS","Q_MAXPREFAIL_P_USGS",
                   "T_FAIL_D_HECP_USGS","T_FAIL_IP_HECP_USGS","T_MAX_D_HECP_USGS", "T_MAX_P_HECP_USGS", "T_MAXPREFAIL_D_HECP_USGS", "T_MAXPREFAIL_P_HECP_USGS", "FAIL_CAUS_CODE")
colNames <- c("BUILT - FAIL - STATE","USGS STAID","DAILY MEAN","INST/PEAKS","DAILY MEAN","INST/PEAKS","DAILY MEAN","INST/PEAKS",
              "DAILY MEAN","INST/PEAKS","DAILY MEAN","INST/PEAKS","DAILY MEAN","INST/PEAKS","CAUSE")
colNamesShort <- c("BUILT--FAIL--STATE","USGS STAID",rep(c("D","I/P","D","P","D","P"),2),"CAUSE")
toptopCols <- " & & \\multicolumn{6}{c}{Q [cfs]} & \\multicolumn{6}{c}{$T_R$ [yr]}  \\\\"
topCols  <- " & & \\multicolumn{2}{c}{FAILURE} & \\multicolumn{2}{c}{MAXIMUM} & \\multicolumn{2}{c}{MAX PRE-FAIL} & \\multicolumn{2}{c}{FAILURE} & \\multicolumn{2}{c}{MAXIMUM} & \\multicolumn{2}{c}{MAX PRE-FAIL} & \\\\"
tabQT <- BridgesDataFrame[,cols]
tabQT$FAIL_CAUS_CODE <- as.character(tabQT$FAIL_CAUS_CODE)

tabLaTeX <- stargazer(tabQT,summary = FALSE, rownames = FALSE,
                      digits = 1, digits.extra = 2,
                      align = TRUE,
                      title = "Flow values and return periods of failure, maximum, and maximum pre-fail events for USGS data, with Nash-Sutcliffe Efficiency (NSE) from Daymet-VIC reanalysis at matching dates and highest $\\alpha$ passed using all available annual maximum daily mean flow data.",
                      column.sep.width = "1pt", font.size = "tiny",
                      label = "tab:FailMaxVals",
                      # notes = "D = daily mean; I = instantaneous; P = peak; ${}^H$ Linked to hurricane; ${}^{+ (++)}$ bridge drainage area $>1.2(1.4)$ times greater than gauge drainage area; ${}^{- (- -)}$ denotes bridge drainage area $<0.8(0.6)$ times smaller than gauge drainage area.;${}^*$ Fail date assumed",
                      notes = "D = daily mean; I = instantaneous; P = peak;${}^*$ fail date assumed; ${}^{H+-}$ see Fig. \\ref{fig:FailMaxFlows}",
                      float.env = "sidewaystable",
                      multicolumn = FALSE
                      )
tabLaTeX[12] <- paste("\\multicolumn{1}{l}{",paste(colNamesShort, collapse= "} & \\multicolumn{1}{r}{"), "} \\\\",sep="")
tabLaTeX     <- c(tabLaTeX[1:11],
                  "\\noalign{\\smallskip}",
                  toptopCols,
                  "\\noalign{\\smallskip} \\cline{3-8} \\cline{9-14} \\noalign{\\smallskip}",
                  topCols, 
                  "\\noalign{\\smallskip} \\cline{3-4} \\cline{5-6} \\cline{7-8} \\cline{9-10} \\cline{11-12} \\cline{13-14} \\noalign{\\smallskip}",
                  tabLaTeX[12:length(tabLaTeX)])
tabLaTeX     <- c(tabLaTeX[1:8], "\\resizebox{0.8\\textheight}{!}{", tabLaTeX[9:(length(tabLaTeX)-1)],"}",tabLaTeX[length(tabLaTeX)])

# fix labels
pattern <- " [${hatmkern6u\\}]{22,25}"
tabLaTeX[grep(pattern,tabLaTeX)] <- sapply(grep(pattern,tabLaTeX), function(i) sub(pattern,"${}^{",tabLaTeX[i]))
pattern <- "[\\texasrikcnd]{21}"
tabLaTeX[grep(pattern,tabLaTeX)] <- sapply(grep(pattern,tabLaTeX), function(i) sub(pattern,"*",tabLaTeX[i]))
pattern <- "[$\\}]{4}"
tabLaTeX[grep(pattern,tabLaTeX)] <- sapply(grep(pattern,tabLaTeX), function(i) sub(pattern,"}$",tabLaTeX[i]))

# fix decimal places
pattern <- "D{.}{.}{-1}"
tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)[1]] <- gsub(pattern,"D{.}{.}{-0}",tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)[1]],fixed = TRUE)
pattern <- "D{.}{.}{-0} }"
tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)[1]] <- gsub(pattern,"D{.}{.}{-1} }",tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)[1]],fixed = TRUE)
pattern <- "\\.[[:digit:]]{1} &"
tabLaTeX[grep(pattern,tabLaTeX)] <- sapply(grep(pattern,tabLaTeX), function(i) gsub(pattern," &",tabLaTeX[i]))

# add Inst/Pk labels to Q
pattern <- "[^&]{10,43} & [^&]{10,43} & [0123456789,]{3,6} & [0123456789,]{3,7}"
matchRows <- grep(pattern, tabLaTeX)
dataRows  <- which(dataTypeLab!="")
lastMatch <- sapply(matchRows, function(i) regexpr(pattern,tabLaTeX[i]) + attr(regexpr(pattern,tabLaTeX[i]),"match.length") - 1)
tabLaTeX[matchRows] <- sapply(1:length(matchRows), function(i) paste(substr(tabLaTeX[matchRows[i]],1,lastMatch[i]),dataTypeLab[dataRows[i]],substr(tabLaTeX[matchRows[i]],lastMatch[i]+1,nchar(tabLaTeX[matchRows[i]])),sep=""))
pattern <- "\\begin{tabular}{@{\\extracolsep{1pt}} D{.}{.}{-0} D{.}{.}{-0} D{.}{.}{-0} "
matchRows <- grep(pattern, tabLaTeX,fixed=TRUE)
lastMatch <- sapply(matchRows, function(i) regexpr(pattern,tabLaTeX[i],fixed=TRUE) + attr(regexpr(pattern,tabLaTeX[i],fixed=TRUE),"match.length") - 1)
tabLaTeX[matchRows] <- sapply(1:length(matchRows), function(i) paste(substr(tabLaTeX[matchRows[i]],1,lastMatch[i]),"r",substr(tabLaTeX[matchRows[i]],lastMatch[i]+12,nchar(tabLaTeX[matchRows[i]])),sep=""))

# add Inst/Pk labels to T
pattern <- "[^&]{10,43} & [^&]{10,43} & [^&]{3,43} & [^&]{0,30} & [^&]{0,30} & [^&]{0,30} & [^&]{0,30} & [^&]{0,30} & [^&]{0,30} & [0123456789,]{1,7}"
matchRows <- grep(pattern, tabLaTeX[20:55]) + 19
dataRows  <- which(dataTypeLab!="")
lastMatch <- sapply(matchRows, function(i) regexpr(pattern,tabLaTeX[i]) + attr(regexpr(pattern,tabLaTeX[i]),"match.length") - 1)
tabLaTeX[matchRows] <- sapply(1:length(matchRows), function(i) paste(substr(tabLaTeX[matchRows[i]],1,lastMatch[i]),dataTypeLab[dataRows[i]],substr(tabLaTeX[matchRows[i]],lastMatch[i]+1,nchar(tabLaTeX[matchRows[i]])),sep=""))
pattern <- "\\begin{tabular}{@{\\extracolsep{1pt}} D{.}{.}{-0} D{.}{.}{-0} D{.}{.}{-0} r D{.}{.}{-0} D{.}{.}{-0} D{.}{.}{-0} D{.}{.}{-0} D{.}{.}{-0} "
matchRows <- grep(pattern, tabLaTeX,fixed=TRUE)
lastMatch <- sapply(matchRows, function(i) regexpr(pattern,tabLaTeX[i],fixed=TRUE) + attr(regexpr(pattern,tabLaTeX[i],fixed=TRUE),"match.length") - 1)
tabLaTeX[matchRows] <- sapply(1:length(matchRows), function(i) paste(substr(tabLaTeX[matchRows[i]],1,lastMatch[i]),"r",substr(tabLaTeX[matchRows[i]],lastMatch[i]+12,nchar(tabLaTeX[matchRows[i]])),sep=""))

tabLaTeX[20:length(tabLaTeX)] <- sapply(20:length(tabLaTeX), function(i) sub("{c}","{l}",tabLaTeX[i],fixed=TRUE))
tabLaTeX[20:length(tabLaTeX)] <- sapply(20:length(tabLaTeX), function(i) sub("{c}","{r}",tabLaTeX[i],fixed=TRUE))
write(tabLaTeX, file = file.path(dirsGit$Plots,"tabQT_new.tex"))

# Table 2: statistics of bridges
rowsToAnalyzeVIC  <- rownames(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView & df.Fail.NBI.Gage$YR_FAIL_EST >= "1980-01-01",])
cols     <- c("YR_BLT_EST","YR_FAIL","DRAIN_SQKM","DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM","DIST_TO_GAGE",
               "COUNT_P_USGS")
meanStats <- sapply(cols, function(col) c(mean(df.Fail.NBI.Gage[rowsToView,col],na.rm=TRUE), mean(df.Fail.NBI.Gage[rowsToAnalyzeVIC,col],na.rm=TRUE)))


# Table 3: correlations and NSE, and KS values? ----------------------
# USGS data horizontal, Daymet-VIC data vertical
load(file.path(dirs$DataDirAnalysis,"20151014_CorrelationsOfReturnPeriods.RData"))
rowDataTypes <- c("Fail Q Daily", "Fail T Daily","Max Q Daily", "Max T Daily") # all USGS
colDataTypes <- c("Fail Q I/P", "Fail T I/P", "Max Q P", "Max T P", "VIC Fail Q Daily", "VIC Fail T Daily", "VIC Max Q Daily", "VIC Max T Daily")
ls.corrs.all <- c(ls.corrs.FailQ[grepl("USGSdUSGSip",names(ls.corrs.FailQ)) | grepl("USGSdVICgdPM2",names(ls.corrs.FailQ)) | grepl("USGSipVICGdPM2",names(ls.corrs.FailQ))],
                  ls.corrs.max[grepl("USGSdUSGSp",names(ls.corrs.max)) | grepl("USGSdVICGd",names(ls.corrs.max)) | grepl("USGSpVICGd",names(ls.corrs.max))],
                  ls.corrs.USGS[grepl("HECdHECip",names(ls.corrs.USGS))],
                  ls.corrs.Tmax.USGS[grepl("HECdHECp",names(ls.corrs.Tmax.USGS))],
                  ls.corrs.USGS.VICg[grepl("HECdHECdPM2",names(ls.corrs.USGS.VICg)) | grepl("HECipHECdPM2",names(ls.corrs.USGS.VICg))],
                  ls.corrs.Tmax.USGS.VICG[grepl("HECdHECd",names(ls.corrs.Tmax.USGS.VICG)) | grepl("HECpHECd",names(ls.corrs.Tmax.USGS.VICG))])
# then want LR (R^2), rho (p), tau (p) for all of these
# row1: USGS d: USGS-IP-[Qf Qm] VIC-d-[Qf Qm]   -> ls.corrs.FailQ[[USGSdUSGSip]] ls.corrs.max[[USGSdUSGSp]]    ls.corrs.FailQ[[USGSdVICgdPM2]] ls.corrs.max[[USGSdVICGd]]
# row2: USGS IP: - -            VIC-d[Qf Qm]    ->      -              -                                       ls.corrs.FailQ[[USGSipVICGdPM2]]       ls.corrs.max[[USGSpVICGd]]
# row3 : USGS d: USGS-IP-[Tf Tm] VIC-d[Tf Tm]   -> ls.corrs.USGS [[HECdHECip]] ls.corrs.Tmax.USGS[[HECdHECp]]  ls.corrs.USGS.VICg[[HECdHECdPM2]] ls.corrs.Tmax.USGS.VICG[[HECdHECd]]
# row4: USGS IP: - -            VIC-d[Tf Tm]    -> -                                -                          ls.corrs.USGS.VICg[[HECipHECdPM2]] ls.corrs.Tmax.USGS.VICG[[HECpHECd]]
rownames <- c(paste("USGS_D",c("Q_LR","Q_rho","Q_tau","T_LR","T_rho","T_tau"),sep="_"),paste("USGS_IP",c("Q_LR","Q_rho","Q_tau","T_LR","T_rho","T_tau"),sep="_"))
rows     <- data.frame(first = c("USGSdUSGSip",NA_character_,"HECdHECip",NA_character_),
                       sec   = c("USGSdUSGSp",NA_character_,"HECdHECp",NA_character_),
                       third = c("USGSdVICgdPM2","USGSipVICGdPM2","HECdHECdPM2","HECipHECdPM2"),
                       fourth = c("USGSdVICGd","USGSpVICGd","HECdHECdPM2","HECpHECd"),
                       stringsAsFactors = FALSE)
types <- c("LR","P","K","Papprox","Kapprox")
subtypes <- list(LR=c("coefficients","r.squared"),
                 P=c("estimate","p.value"),
                 K=c("estimate","p.value"))
colnames <- c(paste("USGS_IP",c("Fail_Q_val","Fail_Q_p","Fail_Q_p_approx","Max_Q_val","Max_Q_p","Max_Q_p_approx",
                                "Fail_T_val","Fail_T_p","Fail_T_p_approx","Max_T_val","Max_T_p","Max_T_p_approx"),sep="_"),
              paste("VIC_D",c("Fail_Q_val","Fail_Q_p","Fail_Q_p_approx","Max_Q_val","Max_Q_p","Max_Q_p_approx","Fail_T_val","Fail_T_p","Fail_T_p_approx","Max_T_val","Max_T_p","Max_T_p_approx"),sep="_"))
corrTable <- data.frame(numeric(length(rownames)),row.names=rownames)
colnames(corrTable) <- colnames[1]
for (col in 1:ncol(rows)){
  for (row in 1:nrow(rows)){
    for (i in which(!is.na(rows[[row]]))){
      corrs <- ls.corrs.all[grep(rows[[row]][i],names(ls.corrs.all))]
      corrTable[3*(row-1)+1,2*(col-1)+1] <- corrs[[names(corrs)[grep(types[1],names(corrs))]]][[subtypes[[types[1]]][1]]][2] # slope
      corrTable[3*(row-1)+1,2*(col-1)+2] <- summary(corrs[[names(corrs)[grepl(types[1],names(corrs))]]])[[subtypes[[types[1]]][2]]] # R^2
      corrTable[3*(row-1)+2,2*(col-1)+1] <- corrs[[names(corrs)[grep(types[2],names(corrs))[1]]]][[subtypes[[types[2]]][1]]] # rho estimate
      corrTable[3*(row-1)+2,2*(col-1)+2] <- corrs[[names(corrs)[grep(types[2],names(corrs))[1]]]][[subtypes[[types[2]]][2]]] # p
      if(corrTable[3*(row-1)+2,2*(col-1)+2] < 5e-4){ # try approx
        corrTable[3*(row-1)+2,2*(col-1)+2] <- corrs[[names(corrs)[grep(types[4],names(corrs))[1]]]][[subtypes[[types[2]]][2]]] # p-approx
        corrTable[3*(row-1)+2,2*(col-1)+3] <- "*"
      }
      corrTable[3*(row-1)+2,2*(col-1)+4] <- corrs[[names(corrs)[grep(types[2],names(corrs))[1]]]][[subtypes[[types[2]]][1]]] # rho estimate
      corrTable[3*(row-1)+2,2*(col-1)+5] <- corrs[[names(corrs)[grep(types[2],names(corrs))[1]]]][[subtypes[[types[2]]][2]]] # p
      if(corrTable[3*(row-1)+2,2*(col-1)+5] < 5e-4){ # try approx
        corrTable[3*(row-1)+2,2*(col-1)+5] <- corrs[[names(corrs)[grep(types[4],names(corrs))[1]]]][[subtypes[[types[2]]][2]]] # p-approx
        corrTable[3*(row-1)+2,2*(col-1)+6] <- "*"
      }
    }
  }
}
# corrTable[colnames[1]] <- sapply(c("LR","P","K") function(i) c(ls.corrs.FailQ[grepl(names())))

# USGSdataTypes <- c("Fail Q Daily", "T Fail Daily","Fail Q Inst/Pk", "T Fail Inst/Pk","T Fail Best",
#                    "Max Q Daily", "Max Q Pk","T Max Daily", "T Max Pk")
# VICdataTypes  <- c("Fail Q Daily", "T Fail Daily","Max Q Daily", "T Max Daily")
# 
# tabCorrs <- data.frame(STAID=character(nrow(BridgesDataFrame)), stringsAsFactors = FALSE)

# Table 3: bridge data ------------------------------------
BridgesDataFrame$LABEL <- paste(sapply(BridgesDataFrame$YR_BLT_EST, function(i) ifelse(!is.na(i),substr(i,1,4), "N.A.")),
                                "--",
                                substr(BridgesDataFrame[,"YR_FAIL"],1,4),
                                "--",
                                sapply(1:nrow(BridgesDataFrame), function(i) df.States[df.States$STFIPS==BridgesDataFrame[i,"STFIPS"],"STATE_CODE"]),
                                sep="")
BridgesDataFrame[BridgesDataFrame$ID==1631,"COMMENT_T_REPORT_NAME"] <- substr(BridgesDataFrame[BridgesDataFrame$ID==1631,"COMMENT_T_REPORT_NAME"],1,18)
BridgesDataFrame[!BridgesDataFrame$BOOL_HAS_FAIL_DATE,"DATE_FAIL_EST_USGS"] <- paste(BridgesDataFrame[!BridgesDataFrame$BOOL_HAS_FAIL_DATE,"DATE_FAIL_EST_USGS"],"${}^*$",sep="")
cols <- c("LABEL","DATE_FAIL_EST_USGS","FAIL_CAUS_CODE","COMMENT_LINKED_HURRICANE","STAID","DRAIN_SQKM","AREA_RATIO_NHD","DIST_TO_GAGE","COUNT_P_USGS","COMMENT_T_REPORT_NAME","NSE_D_DVICG","BOOL_KS_ALL_DATA_DVICG_PASS")
FirstColNames <- c("","","","","","AREA","AREA RATIO","DIST. GAGE","","","DAYMET-VIC", "")
SecColNames <- c("BUILT--FAIL--STATE","FAIL DATE","CAUSE","HURRICANE","STAID","[SQKM]","BR:GA","[KM]","NO. PEAKS","REPORT","NSE_D_DVICG", "KS $\\alpha$")
tabBridges <- BridgesDataFrame[,cols]
tabBridges$FAIL_CAUS_CODE <- as.character(tabBridges$FAIL_CAUS_CODE)

tabLaTeX <- stargazer(tabBridges,summary = FALSE, rownames = FALSE,
                      digits = 1, digits.extra = 3,
                      align = TRUE,
                      title = "Failure and gage site information",
                      column.sep.width = "1pt", font.size = "tiny",
                      label = "tab:BridgeData",
                      notes = "${}^*$ fail date assumed",
                      float.env = "sidewaystable",
                      multicolumn = FALSE
)
tabLaTeX[12] <- paste("\\multicolumn{1}{l}{",paste(FirstColNames[1:(length(FirstColNames)-2)], collapse= "} & \\multicolumn{1}{r}{"),"} & \\multicolumn{2}{c}{",FirstColNames[length(FirstColNames)-1] ,"}  \\\\",sep="")
tabLaTeX <- c(tabLaTeX[c(1:12)],paste("\\multicolumn{1}{c}{",paste(SecColNames, collapse= "} & \\multicolumn{1}{c}{"), "} \\\\",sep="") ,tabLaTeX[c(13:length(tabLaTeX))])

# fix labels
pattern <- "[${hatmkern6u\\}]{22,25}"
tabLaTeX[15:(length(tabLaTeX)-4)][grep(pattern,tabLaTeX[15:(length(tabLaTeX)-4)])] <- sapply(grep(pattern,tabLaTeX[15:(length(tabLaTeX)-4)]), function(i) gsub(pattern,"${}^{",tabLaTeX[15:(length(tabLaTeX)-4)][i]))
pattern <- "[texasrikcnd]{19,20}"
tabLaTeX[15:(length(tabLaTeX)-4)][grep(pattern,tabLaTeX[15:(length(tabLaTeX)-4)])] <- sapply(grep(pattern,tabLaTeX[15:(length(tabLaTeX)-4)]), function(i) gsub(pattern,"*",tabLaTeX[15:(length(tabLaTeX)-4)][i]))
pattern <- "[$\\}]{3}"
rows <- which(regexpr(pattern,tabLaTeX) > 0 & regexpr(pattern,tabLaTeX) < 75)
rows <- rows[rows>=15 & rows < (length(tabLaTeX) - 3)]
tabLaTeX[rows] <- sapply(rows, function(i) sub(pattern,"}$}",tabLaTeX[i]))

# fix decimal places for area
pattern <- "\\begin{tabular}{@{\\extracolsep{1pt}} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1}"
tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)[1]] <- sub(pattern,"\\begin{tabular}{@{\\extracolsep{1pt}} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-0}",tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)[1]],fixed = TRUE)
pattern <- "[^&]{10,43} & [^&]{10,60} & [^&]{3,60} & [^&]{0,60} & [^&]{1,30} & [0123456789,]{0,3}[[:space:]]{0,1}[0123456789]{1,3}."
matchRows <- grep(pattern, tabLaTeX)
lastMatch <- sapply(matchRows, function(i) regexpr(pattern,tabLaTeX[i]) + attr(regexpr(pattern,tabLaTeX[i]),"match.length") - 1)
tabLaTeX[matchRows] <- sapply(1:length(matchRows), function(i) paste(substr(tabLaTeX[matchRows[i]],1,lastMatch[i]-1),substr(tabLaTeX[matchRows[i]],lastMatch[i]+2,nchar(tabLaTeX[matchRows[i]])),sep=""))

tabLaTeX[15:length(tabLaTeX)] <- sapply(15:length(tabLaTeX), function(i) sub("c}","l}",tabLaTeX[i]))
tabLaTeX[15:length(tabLaTeX)] <- sapply(15:length(tabLaTeX), function(i) sub("c}","r}",tabLaTeX[i]))
write(tabLaTeX, file = file.path(dirsGit$Plots,"tabBridges_new.tex"))
