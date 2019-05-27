# Make tables for manuscript on historical bridge failures
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirs$DataDirFrequent,"StateCountyCityData.RData"))
source(file.path(dirsGit$ScriptsPlotDir,"SetupEncoding.R"))

library(stargazer)
require(plyr)

BridgesDataFrame <- df.Fail.NBI.Gage[rowsToView,]

BridgesDataFrame$LABEL <- paste(sapply(as.character(as.Date(BridgesDataFrame$YR_BLT_EST,"1970-01-01")), function(i) ifelse(!is.na(i),substr(i,1,4), "N.A.")),
                                "--",
                                substr(as.character(as.Date(BridgesDataFrame$YR_FAIL,"1970-01-01")),1,4),
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

BridgesDataFrame$LABEL <- MakeBridgeLabel(BridgesDataFrame, SUPER_FIELDS = c("Hurr","Area","FailDate"))

dataTypeLab <- substr(BridgesDataFrame[,"T_FAIL_BEST_HEC_SOURCE"],1,1)
dataTypeLab[dataTypeLab!="D"] <- paste("${}^{",dataTypeLab[dataTypeLab!="D"],"}$",sep = "")
dataTypeLab[dataTypeLab=="D"] <- ""

BridgesDataFrame[is.na(BridgesDataFrame$NSE_D_DVICG),"AREA_RATIO_DVICG"] <- NA_real_


# Table 1v3: failure and max data and return periods with VIC, no max pre-fail, and report citation -------------------------------------------------------------------------------------------------------------------
cols <- c("LABEL", "STAID", "Q_FAIL_D_USGS",    "Q_FAIL_IP_USGS",  "Q_FAILPM2_DVICG",  "Q_MAX_D_USGS",     "Q_MAX_P_USGS",    "Q_MAX_DVICG",
          "T_FAIL_D_HECP_USGS","T_FAIL_IP_HECP_USGS", "T_FAILPM2_HEC_DVICG","T_REPORT","T_MAX_D_HECP_USGS", "T_MAX_P_HECP_USGS", "T_MAX_HEC_DVICG", "FAIL_CAUS_CODE")
colNamesShort <- c("","","D","I/P","D","D","P","D","D","I/P","D","","D","P","D","")
topCols <- "\\multicolumn{1}{l}{BUILT--FAIL--STATE} & \\multicolumn{1}{c}{STAID} & \\multicolumn{6}{c}{Q [cfs]} & \\multicolumn{7}{c}{$T_R$ [yr]} & \\multicolumn{1}{c}{CAUSE} \\\\"
secCols  <- " & & \\multicolumn{3}{c}{FAILURE} & \\multicolumn{3}{c}{MAXIMUM} & \\multicolumn{4}{c}{FAILURE} & \\multicolumn{3}{c}{MAXIMUM}  & \\\\"
thirdCols      <- "& & \\multicolumn{2}{c}{USGS} & \\multicolumn{1}{c}{VIC} & \\multicolumn{2}{c}{USGS} & \\multicolumn{1}{c}{VIC} & \\multicolumn{2}{c}{USGS} & \\multicolumn{1}{c}{VIC} & \\multicolumn{1}{c}{REPORT} & \\multicolumn{2}{c}{USGS} & \\multicolumn{1}{c}{VIC}  & \\\\"
tabQT <- BridgesDataFrame[,cols]
tabQT$FAIL_CAUS_CODE <- as.character(tabQT$FAIL_CAUS_CODE)
ConfNonOverlap <- BridgesDataFrame$T_FAIL_PM2_CONF_VIC_GAGE_LARGER | BridgesDataFrame$T_FAIL_PM2_CONF_USGS_LARGER
ConfNonOverlapSym <- rep("",nrow(BridgesDataFrame))
ConfNonOverlapSym[ConfNonOverlap] <- "${}^{\\dagger}$"
text <- paste(round(tabQT$T_FAILPM2_HEC_DVICG),ConfNonOverlapSym,sep="")
text[text=="NA"] <- ""
tabQT$T_FAILPM2_HEC_DVICG <- text
tabQT$T_REPORT <- paste(tabQT$T_REPORT,BridgesDataFrame$COMMENT_T_REPORT_CITE)

load(file.path(dirs$DataDirFrequent,"20151203_T_Fail_Max_Stats.RData"))
Medians <- round(c(T_stats[["FAIL"]]["median",colnames(T_stats[["FAIL"]]) %in% cols], T_stats[["MAX"]]["median",colnames(T_stats[["MAX"]]) %in% cols]))
Means   <- round(c(T_stats[["FAIL"]]["mean",colnames(T_stats[["FAIL"]]) %in% cols], T_stats[["MAX"]]["mean",colnames(T_stats[["MAX"]]) %in% cols]))
tabQT[nrow(tabQT)+1,names(Medians)] <- Medians # will have to add mean later
tabQT[nrow(tabQT),"LABEL"] <- "MEDIAN (MEAN)"

tabLaTeX <- stargazer(tabQT,summary = FALSE, rownames = FALSE,
                      digits = 1, digits.extra = 2,
                      align = TRUE,
                      title = "Flow values $Q$ and return periods $T_R$ of failure and maximum events using USGS and Daymet-VIC data and the Bulletin 17B methodology.",
                      column.sep.width = "1pt", font.size = "tiny",
                      label = "tab:FailMaxVals",
                      notes = "Note: D = daily mean; I = instantaneous; P = peak.",
                      float.env = "sidewaystable",
                      multicolumn = FALSE
)
tabLaTeX[12] <- paste("\\multicolumn{1}{l}{",paste(colNamesShort, collapse= "} & \\multicolumn{1}{r}{"), "} \\\\",sep="")
tabLaTeX     <- c(tabLaTeX[1:11],
                  "\\noalign{\\smallskip}",
                  topCols,
                  "\\noalign{\\smallskip} \\cline{3-8} \\cline{9-15} \\noalign{\\smallskip}",
                  secCols, 
                  "\\noalign{\\smallskip} \\cline{3-5} \\cline{6-8} \\cline{9-12} \\cline{13-15} \\noalign{\\smallskip}",
                  thirdCols,
                  "\\noalign{\\smallskip} \\cline{3-4} \\cline{5-5} \\cline{6-7} \\cline{8-8} \\cline{9-10} \\cline{11-11} \\cline{12-12} \\cline{13-14} \\cline{15-15} \\noalign{\\smallskip}",
                  tabLaTeX[12:length(tabLaTeX)])
tabLaTeX     <- c(tabLaTeX[1:8], "\\resizebox{\\textheight}{!}{", tabLaTeX[9:(length(tabLaTeX)-1)],"}",tabLaTeX[length(tabLaTeX)])

# make multirows
pattern <- "\\multicolumn{1}{l}{BUILT--FAIL--STATE} & \\multicolumn{1}{c}{STAID}"
rep     <- "\\multirow{5}{*}{BUILT--FAIL--STATE} & \\multirow{5}{*}{STAID}"
tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)] <- sapply(grep(pattern,tabLaTeX, fixed = TRUE), function(i) sub(pattern,rep,tabLaTeX[i], fixed = TRUE))
pattern <- "\\multicolumn{1}{c}{CAUSE}"
rep <- "\\multirow{5}{*}{CAUSE}"
tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)] <- sapply(grep(pattern,tabLaTeX, fixed = TRUE), function(i) sub(pattern,rep,tabLaTeX[i], fixed = TRUE))


# fix labels
pattern <- " [${hatmkern6u\\}]{22,25}"
tabLaTeX[grep(pattern,tabLaTeX)] <- sapply(grep(pattern,tabLaTeX), function(i) sub(pattern,"${}^{",tabLaTeX[i]))
pattern <- "[\\texasrikcnd]{21}"
tabLaTeX[grep(pattern,tabLaTeX)] <- sapply(grep(pattern,tabLaTeX), function(i) sub(pattern,"*",tabLaTeX[i]))
pattern <- "[$\\}]{4}"
tabLaTeX[grep(pattern,tabLaTeX)] <- sapply(grep(pattern,tabLaTeX), function(i) sub(pattern,"}$",tabLaTeX[i]))
pattern <- "\\$\\{\\}$\\hat{\\mkern6mu}$\\{\\textbackslash dagger\\}\\$"
tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)] <- sapply(grep(pattern,tabLaTeX, fixed = TRUE), function(i) sub(pattern,"${}^{\\dagger}$",tabLaTeX[i], fixed = TRUE))
pattern <- "\\$\\{}$hat{\\mkern6mu}$\\{\\textbackslash dagger\\}\\$"
tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)] <- sapply(grep(pattern,tabLaTeX, fixed = TRUE), function(i) sub(pattern,"${}^{\\dagger}$",tabLaTeX[i], fixed = TRUE))

# fix citations
pattern <- "\\textbackslash cite\\{"
tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)] <- sapply(grep(pattern,tabLaTeX, fixed = TRUE), function(i) sub(pattern,"\\cite{",tabLaTeX[i], fixed = TRUE))
pattern <- "\\}}"
tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)] <- sapply(grep(pattern,tabLaTeX, fixed = TRUE), function(i) sub(pattern,"}}",tabLaTeX[i], fixed = TRUE))


# fix decimal places
pattern <- "D{.}{.}{-1}"
tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)[1]] <- gsub(pattern,"D{.}{.}{-0}",tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)[1]],fixed = TRUE)
pattern <- "D{.}{.}{-0} }"
tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)[1]] <- gsub(pattern,"D{.}{.}{-1} }",tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)[1]],fixed = TRUE)
pattern <- "\\.[[:digit:]]{1} &"
tabLaTeX[grep(pattern,tabLaTeX)] <- sapply(grep(pattern,tabLaTeX), function(i) gsub(pattern," &",tabLaTeX[i]))

# add Inst/Pk labels to Q
pattern <- "[^&]{10,37} & [^&]{25,37} & [0123456789,]{3,6} & [0123456789,]{5,7}"
matchRows <- grep(pattern, tabLaTeX[1:57])
dataRows  <- which(dataTypeLab!="")
lastMatch <- sapply(matchRows, function(i) regexpr(pattern,tabLaTeX[i]) + attr(regexpr(pattern,tabLaTeX[i]),"match.length") - 1)
tabLaTeX[matchRows] <- sapply(1:length(matchRows), function(i) paste(substr(tabLaTeX[matchRows[i]],1,lastMatch[i]),dataTypeLab[dataRows[i]],substr(tabLaTeX[matchRows[i]],lastMatch[i]+1,nchar(tabLaTeX[matchRows[i]])),sep=""))
pattern <- "\\begin{tabular}{@{\\extracolsep{1pt}} D{.}{.}{-0} D{.}{.}{-0} D{.}{.}{-0} "
matchRows <- grep(pattern, tabLaTeX[1:57],fixed=TRUE)
lastMatch <- sapply(matchRows, function(i) regexpr(pattern,tabLaTeX[i],fixed=TRUE) + attr(regexpr(pattern,tabLaTeX[i],fixed=TRUE),"match.length") - 1)
tabLaTeX[matchRows] <- sapply(1:length(matchRows), function(i) paste(substr(tabLaTeX[matchRows[i]],1,lastMatch[i]),"r",substr(tabLaTeX[matchRows[i]],lastMatch[i]+12,nchar(tabLaTeX[matchRows[i]])),sep=""))

# add Inst/Pk labels to T
pattern <- "[^&]{20,37} & [^&]{25,37} & [^&]{3,43} & [^&]{0,30} & [^&]{0,30} & [^&]{0,30} & [^&]{0,30} & [^&]{0,30} & [^&]{0,30} & [0123456789,]{1,7}"
matchRows <- grep(pattern, tabLaTeX[22:57]) + 21
dataRows  <- which(dataTypeLab!="")
lastMatch <- sapply(matchRows, function(i) regexpr(pattern,tabLaTeX[i]) + attr(regexpr(pattern,tabLaTeX[i]),"match.length") - 1)
tabLaTeX[matchRows] <- sapply(1:length(matchRows), function(i) paste(substr(tabLaTeX[matchRows[i]],1,lastMatch[i]),dataTypeLab[dataRows[i]],substr(tabLaTeX[matchRows[i]],lastMatch[i]+1,nchar(tabLaTeX[matchRows[i]])),sep=""))
pattern <- "\\begin{tabular}{@{\\extracolsep{1pt}} D{.}{.}{-0} D{.}{.}{-0} D{.}{.}{-0} r D{.}{.}{-0} D{.}{.}{-0} D{.}{.}{-0} D{.}{.}{-0} D{.}{.}{-0} "
matchRows <- grep(pattern, tabLaTeX,fixed=TRUE)
lastMatch <- sapply(matchRows, function(i) regexpr(pattern,tabLaTeX[i],fixed=TRUE) + attr(regexpr(pattern,tabLaTeX[i],fixed=TRUE),"match.length") - 1)
tabLaTeX[matchRows] <- sapply(1:length(matchRows), function(i) paste(substr(tabLaTeX[matchRows[i]],1,lastMatch[i]),"r",substr(tabLaTeX[matchRows[i]],lastMatch[i]+12,nchar(tabLaTeX[matchRows[i]])),sep=""))

# fix T_R info
pattern <- "10\\textless TR\\textless 25"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"$10<T_R<25$",tabLaTeX[i],fixed=TRUE))
pattern <- "Storm 500"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"500 (storm)",tabLaTeX[i],fixed=TRUE))

# add mean data
pattern      <- " [[:digit:]]{1,4} "
matchLims    <- c(regexpr(pattern,tabLaTeX[58]), regexpr(pattern,tabLaTeX[58]) + attr(regexpr(pattern,tabLaTeX[58]),"match.length")-1)
tabLaTeX[58] <- paste(substr(tabLaTeX[58],1,matchLims[1]),"\\multicolumn{1}{c}{",substr(tabLaTeX[58],matchLims[1]+1,matchLims[2]-1)," (",Means[1],")} ",substr(tabLaTeX[58],matchLims[2]+1,nchar(tabLaTeX[58])),sep="")
pattern      <- "& [[:digit:]]{1,4} "
matchLims    <- c(regexpr(pattern,tabLaTeX[58]), regexpr(pattern,tabLaTeX[58]) + attr(regexpr(pattern,tabLaTeX[58]),"match.length")-1)
tabLaTeX[58] <- paste(substr(tabLaTeX[58],1,matchLims[1])," \\multicolumn{1}{c}{",substr(tabLaTeX[58],matchLims[1]+2,matchLims[2]-1)," (",Means[2],")} ",substr(tabLaTeX[58],matchLims[2]+1,nchar(tabLaTeX[58])),sep="")
pattern      <- "[[:digit:]]{1,4}} & "
matchLims    <- c(regexpr(pattern,tabLaTeX[58]), regexpr(pattern,tabLaTeX[58]) + attr(regexpr(pattern,tabLaTeX[58]),"match.length")-1)
tabLaTeX[58] <- paste(substr(tabLaTeX[58],1,matchLims[2]-4)," (",Means[3],")} & ",substr(tabLaTeX[58],matchLims[2]+1,nchar(tabLaTeX[58])),sep="")
pattern      <- "[[:digit:]]{1,4} & "
matchLims    <- c(regexpr(pattern,tabLaTeX[58]), regexpr(pattern,tabLaTeX[58]) + attr(regexpr(pattern,tabLaTeX[58]),"match.length")-1)
tabLaTeX[58] <- paste(substr(tabLaTeX[58],1,matchLims[1]-1)," \\multicolumn{1}{c}{",substr(tabLaTeX[58],matchLims[1],matchLims[2]-3)," (",Means[4],")} & ",substr(tabLaTeX[58],matchLims[2]+1,nchar(tabLaTeX[58])),sep="")
matchLims    <- c(regexpr(pattern,tabLaTeX[58]), regexpr(pattern,tabLaTeX[58]) + attr(regexpr(pattern,tabLaTeX[58]),"match.length")-1)
tabLaTeX[58] <- paste(substr(tabLaTeX[58],1,matchLims[1]-1)," \\multicolumn{1}{c}{",substr(tabLaTeX[58],matchLims[1],matchLims[2]-3)," (",Means[5],")} & ",substr(tabLaTeX[58],matchLims[2]+1,nchar(tabLaTeX[58])),sep="")
matchLims    <- c(regexpr(pattern,tabLaTeX[58]), regexpr(pattern,tabLaTeX[58]) + attr(regexpr(pattern,tabLaTeX[58]),"match.length")-1)
tabLaTeX[58] <- paste(substr(tabLaTeX[58],1,matchLims[1]-1)," \\multicolumn{1}{c}{",substr(tabLaTeX[58],matchLims[1],matchLims[2]-3)," (",Means[6],")} & ",substr(tabLaTeX[58],matchLims[2]+1,nchar(tabLaTeX[58])),sep="")

# alignment
tabLaTeX[22:length(tabLaTeX)] <- sapply(22:length(tabLaTeX), function(i) sub("{c}","{l}",tabLaTeX[i],fixed=TRUE))
tabLaTeX[22:length(tabLaTeX)] <- sapply(22:length(tabLaTeX), function(i) sub("{c}","{r}",tabLaTeX[i],fixed=TRUE))

# add footnotes:
tabLaTeX <- c(tabLaTeX[1:(length(tabLaTeX)-3)],
              "\\multicolumn{16}{l}{${}^\\mathrm{H}$ link to hurricane at time of failure; ${}^{+ (++)}$ bridge drainage area $>1.2(1.4)$ times greater than gauge drainage area; ${}^{- (- -)}$ bridge drainage area $<0.8(0.6)$ times smaller than gauge drainage area.} \\\\",
              "\\multicolumn{16}{l}{${}^*$ fail date assumed.} \\\\",
              "\\multicolumn{16}{l}{${}^{\\dagger}$ Non-overlapping USGS daily mean and Daymet-VIC daily mean 5-95\\% confidence intervals.} \\\\",
              tabLaTeX[(length(tabLaTeX)-2):length(tabLaTeX)])
write(tabLaTeX, file = file.path(dirsGit$Plots,"tabQT_new_v3.tex"))

# Table 2: statistics of bridges ---------------------
rowsToAnalyzeVIC  <- rownames(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView & df.Fail.NBI.Gage$YR_FAIL_EST >= "1980-01-01",])
cols     <- c("YR_BLT_EST","YR_FAIL","DRAIN_SQKM","DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM","AREA_RATIO_NHD","AREA_RATIO_DVICG","DIST_TO_GAGE",
              "COUNT_P_USGS")
df.Fail.NBI.Gage$COUNT_P_USGS <- as.integer(df.Fail.NBI.Gage$COUNT_P_USGS)
dateCols             <- c("YR_BLT_EST","YR_FAIL")
meanStats            <- sapply(cols, function(col) c(mean(df.Fail.NBI.Gage[rowsToView,col],na.rm=TRUE), mean(df.Fail.NBI.Gage[rowsToAnalyzeVIC,col],na.rm=TRUE)))
meanStats[,dateCols] <- sapply(dateCols, function(col) substr(as.character(as.Date(meanStats[,col],as.Date("1970-01-01"))),1,4))
meanStats[,cols[!(cols %in% dateCols)]] <- sapply(cols[!(cols %in% dateCols)], function(col) signif(as.numeric(meanStats[,col]),3))
stdStats             <- sapply(cols, function(col) c(sd(df.Fail.NBI.Gage[rowsToView,col],na.rm=TRUE), sd(df.Fail.NBI.Gage[rowsToAnalyzeVIC,col],na.rm=TRUE)))
stdStats[,dateCols]  <- sapply(dateCols, function(col) signif(stdStats[,col]/365.25,2))
stdStats[,cols[!(cols %in% dateCols)]] <- sapply(cols[!(cols %in% dateCols)], function(col) signif(as.numeric(stdStats[,col]),3))
rangeStats           <- sapply(cols, function(col) c(range(df.Fail.NBI.Gage[rowsToView,col],na.rm=TRUE), range(df.Fail.NBI.Gage[rowsToAnalyzeVIC,col],na.rm=TRUE)))
rangeStats[,dateCols] <- sapply(dateCols, function(col) substr(as.character(as.Date(rangeStats[,col],as.Date("1970-01-01"))),1,4))
rangeStats[,cols[!(cols %in% dateCols)]] <- sapply(cols[!(cols %in% dateCols)], function(col) signif(as.numeric(rangeStats[,col]),3))
USGStab <- sapply(cols, function(col) paste("$",meanStats[1,col],"\\pm",stdStats[1,col],'(',rangeStats[1,col],"-",rangeStats[2,col],")$",sep=""))
USGStab["AREA_RATIO_DVICG"] <- "-"
VICtab  <- sapply(cols, function(col) paste("$",meanStats[2,col],"\\pm",stdStats[2,col],'(',rangeStats[3,col],"-",rangeStats[4,col],")$",sep=""))

rows <- c("Year built", "Year failed", "Gauge drainage area [sqkm]","Bridge drainage area [sqkm]", "Bridge / gage area","Daymet-VIC / USGS gauge area","Distance bridge-gauge [km]","No. annual peaks")
tab     <- data.frame(rows, USGStab, VICtab)

tabLaTeX <- stargazer(tab,summary = FALSE, rownames = FALSE,
                      digits = 3, digits.extra = 3,
                      align = TRUE,
                      title = "Comparison of bridge data used in study and subset used in Daymet-VIC study. Mean $\\pm$ standard deviation and range are given.",
                      column.sep.width = "1pt", font.size = "footnotesize",
                      label = "tab:BridgeStatsData",
                      # notes = "${}^{a}$ p-value is approximate",
                      # float.env = "sidewaystable",
                      multicolumn = FALSE
)

tabLaTeX[12] <- "& \\multicolumn{1}{c}{All bridges (36)} & \\multicolumn{1}{c}{Daymet-VIC bridges (27)} \\\\ "

#fix labels
pattern <- "\\$"
tabLaTeX[grep(pattern,tabLaTeX,fixed = TRUE)] <-  sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) gsub(pattern,"$",tabLaTeX[i],fixed=TRUE))
pattern <- "{c}"
tabLaTeX[grep(pattern,tabLaTeX,fixed = TRUE)] <-  sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) gsub(pattern,"{l}",tabLaTeX[i],fixed=TRUE))
pattern <- "\\textbackslash pm"
tabLaTeX[grep(pattern,tabLaTeX,fixed = TRUE)] <-  sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) gsub(pattern,"\\pm",tabLaTeX[i],fixed=TRUE))

write(tabLaTeX, file = file.path(dirsGit$Plots,"tabBridgeStats.tex"))

# Table 3: correlations ----------------------
# USGS data horizontal, Daymet-VIC data vertical
load(file.path(dirs$DataDirAnalysis,"20151203_CorrelationsOfMaxes.RData"))
load(file.path(dirs$DataDirAnalysis,"20151203_CorrelationsOfReturnPeriods.RData"))

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
# row2 : USGS d: USGS-IP-[Tf Tm] VIC-d[Tf Tm]   -> ls.corrs.USGS [[HECdHECip]] ls.corrs.Tmax.USGS[[HECdHECp]]  ls.corrs.USGS.VICg[[HECdHECdPM2]] ls.corrs.Tmax.USGS.VICG[[HECdHECd]]
# row3: USGS IP: - -            VIC-d[Qf Qm]    ->      -              -                                       ls.corrs.FailQ[[USGSipVICGdPM2]]       ls.corrs.max[[USGSpVICGd]]
# row4: USGS IP: - -            VIC-d[Tf Tm]    -> -                                -                          ls.corrs.USGS.VICg[[HECipHECdPM2]] ls.corrs.Tmax.USGS.VICG[[HECpHECd]]
rownames <- c(paste("USGS_D",c("Q_LR","Q_rho","Q_tau","T_LR","T_rho","T_tau"),sep="_"),paste("USGS_IP",c("Q_LR","Q_rho","Q_tau","T_LR","T_rho","T_tau"),sep="_"))
rows     <- data.frame(first = c("USGSdUSGSip","HECdHECip",NA_character_,NA_character_),
                       sec   = c("USGSdUSGSp","HECdHECp",NA_character_,NA_character_),
                       third = c("USGSdVICgdPM2","HECdHECdPM2","USGSipVICGdPM2","HECipHECdPM2"),
                       fourth = c("USGSdVICGd","HECdHECd","USGSpVICGd","HECpHECd"),
                       stringsAsFactors = FALSE)
types <- c("LR","P","K","Papprox","Kapprox")
subtypes <- list(LR=c("coefficients","r.squared"),
                 P=c("estimate","p.value"),
                 K=c("estimate","p.value"))
colnames <- c(paste("USGS_IP",c("Fail_val","Fail_p","Fail_p_approx","Max_val","Max_p","Max_p_approx"),sep="_"),
              paste("VIC_D",c("Fail_val","Fail_p","Fail_p_approx","Max_val","Max_p","Max_p_approx"),sep="_"))

tabCorr <- data.frame(matrix(vector(), length(rownames), length(colnames),
                        dimnames=list(rownames, colnames)),
                        stringsAsFactors=F)
for (row in 1:nrow(rows)){
  for (i in which(!is.na(rows[[row]]))){
    print(paste("row =",row,"i =",i,"looking for data type =",rows[[row]][i]))
    corrs <- ls.corrs.all[grepl(rows[[row]][i],names(ls.corrs.all))]
    if(!grepl("PM2",rows[[row]][i])) corrs <- corrs[!grepl("PM2",names(corrs))]
    # LR and R^2
    tabCorr[1+3*(i-1),(row-1)*3+1] <- corrs[[names(corrs)[grep(types[1],names(corrs))]]][[subtypes[[types[1]]][1]]][2] # slope
    tabCorr[1+3*(i-1),(row-1)*3+2] <- summary(corrs[[names(corrs)[grepl(types[1],names(corrs))]]])[[subtypes[[types[1]]][2]]] # R^2
    # rho and p
    tabCorr[2+3*(i-1),(row-1)*3+1] <- corrs[[names(corrs)[grep(types[2],names(corrs))[1]]]][[subtypes[[types[2]]][1]]] # rho estimate
    tabCorr[2+3*(i-1),(row-1)*3+2] <- corrs[[names(corrs)[grep(types[2],names(corrs))[1]]]][[subtypes[[types[2]]][2]]] # p
    if(tabCorr[2+3*(i-1),(row-1)*3+2] < 5e-4){ # try approx
      tabCorr[2+3*(i-1),(row-1)*3+2] <- corrs[[names(corrs)[grep(types[4],names(corrs))[1]]]][[subtypes[[types[2]]][2]]] # p-approx
      tabCorr[2+3*(i-1),(row-1)*3+3] <- "a"
    }
    # tau and p
    tabCorr[3+3*(i-1),(row-1)*3+1] <- corrs[[names(corrs)[grep(types[3],names(corrs))[1]]]][[subtypes[[types[3]]][1]]] # tau estimate
    tabCorr[3+3*(i-1),(row-1)*3+2] <- corrs[[names(corrs)[grep(types[3],names(corrs))[1]]]][[subtypes[[types[3]]][2]]] # p
    if(tabCorr[3+3*(i-1),(row-1)*3+2] < 5e-4){ # try approx
      tabCorr[3+3*(i-1),(row-1)*3+2] <- corrs[[names(corrs)[grep(types[5],names(corrs))[1]]]][[subtypes[[types[3]]][2]]] # p-approx
      tabCorr[3+3*(i-1),(row-1)*3+3] <- "a"
    }
  }
}
roundCols <- colnames[c(1:2,4:5,7:8,10:11)]
for (col in roundCols){
  tabCorr[,col][tabCorr[,col]>10 & !is.na(tabCorr[,col])] <- round(tabCorr[,col][tabCorr[,col]>10& !is.na(tabCorr[,col])])
  tabCorr[,col][tabCorr[,col]>1 & tabCorr[,col]<10& !is.na(tabCorr[,col])] <- round(tabCorr[,col][tabCorr[,col]>1 & tabCorr[,col]<10 & !is.na(tabCorr[,col])],2)
  tabCorr[,col][tabCorr[,col]>10 & !is.na(tabCorr[,col])] <- round(tabCorr[,col][tabCorr[,col]>10& !is.na(tabCorr[,col])])
  tabCorr[,col][tabCorr[,col]<1 & !is.na(tabCorr[,col])] <- round(tabCorr[,col][tabCorr[,col]<1 & !is.na(tabCorr[,col])],3)
  tabCorr[,col] <- as.character(tabCorr[,col])
  tabCorr[,col][tabCorr[,col]=="0"] <- "$<1\\text{\\sc{e}-}3$"
  tabCorr[,col][tabCorr[,col]=="NA"] <- ""
}
approxCols <- colnames[c(3,6,9,12)]
tabCorr[,approxCols] <- sapply(approxCols, function(col) sub("a","${}^{a}$",tabCorr[,col]))
for (col in approxCols){
  tabCorr[,col][is.na(tabCorr[,col])] <- ""
}
collapseCols <- c(1,4,7,10)
for (col in collapseCols){
  tabCorr[,col] <- paste(tabCorr[,col]," (",tabCorr[,col+1],")",tabCorr[,col+2],sep="")
  tabCorr[,col][tabCorr[,col]==" ()" | tabCorr[,col]=="NA (NA)"] <- ""
}
tabCorr <- tabCorr[,collapseCols]
tabCorr$LABEL <- rep(c("$m$ ($R^2$)","$\\rho$ ($p$)","$\\tau$ ($p$)"),4)
tabCorr$DATA  <- c("USGS Daily","","","","","","USGS I/P","","","","","")
tabCorr$VALUES <- rep(c("$Q$","","","$T_R$","",""),2)
tabCorr <- tabCorr[,c(6,7,5,1:4)]

tabLaTeX <- stargazer(tabCorr,summary = FALSE, rownames = FALSE,
                      digits = 3, digits.extra = 3,
                      align = TRUE,
                      title = "Slope of linear regression ($m$) and measures of correlation (Pearson $\\rho$, Kendall $\\tau$) of failure and max flows $Q$ and return period estimates $T_R$.",
                      column.sep.width = "1pt", font.size = "tiny",
                      label = "tab:CorrData",
                      notes = "${}^{a}$ p-value is approximate",
                      # float.env = "sidewaystable",
                      multicolumn = FALSE
)
# make top column names and row names
toptopCols   <- "& & & \\multicolumn{2}{c}{USGS I/P} & \\multicolumn{2}{c}{Daymet-VIC Daily}  \\\\"
tabLaTeX     <- c(tabLaTeX[c(1:11)],toptopCols,tabLaTeX[c(12:length(tabLaTeX))])
tabLaTeX[13] <- "& & & \\multicolumn{1}{r}{FAILURE} & \\multicolumn{1}{r}{MAX} & \\multicolumn{1}{r}{FAILURE} & \\multicolumn{1}{r}{MAX}  \\\\"
pattern <- "\\multicolumn{1}{c}{USGS Daily}"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"\\multirow{6}{*}{USGS Daily}",tabLaTeX[i],fixed=TRUE))
pattern <- "\\multicolumn{1}{c}{USGS I/P}"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"\\multirow{6}{*}{USGS I/P}",tabLaTeX[i],fixed=TRUE))
pattern <- "\\multicolumn{1}{c}{\\$Q\\$}"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"\\multirow{3}{*}{$Q$}",tabLaTeX[i],fixed=TRUE))
pattern <- "\\multicolumn{1}{c}{\\$T\\_R\\$}"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"\\multirow{3}{*}{$T_R$}",tabLaTeX[i],fixed=TRUE))


# make horizontal lines
# tabLaTeX    <- c(tabLaTeX[1:8], "\\resizebox{0.8\\textheight}{!}{", tabLaTeX[9:(length(tabLaTeX)-1)],"}",tabLaTeX[length(tabLaTeX)])
insertCline  <- "\\cline{2-7} \\\\[-1.8ex]"
insertHline  <- "\\hline \\\\[-1.8ex]"
tabLaTeX    <- c(tabLaTeX[1:17], insertCline, tabLaTeX[18:20], insertHline, tabLaTeX[21:23],insertCline,tabLaTeX[24:length(tabLaTeX)])


#fix labels
pattern <- "\\$\\{\\}$\\hat{\\mkern6mu}$\\{a\\}\\$}"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) gsub(pattern,"${}^{a}$}",tabLaTeX[i],fixed=TRUE))
pattern <- "(\\$R$\\hat{\\mkern6mu}$2\\$)" # (\\$R$\\hat{\\mkern6mu}$2\\$)
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) gsub(pattern,"(${R}^{2}$)",tabLaTeX[i],fixed=TRUE))
pattern <- "\\multicolumn{1}{c}"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"\\multicolumn{1}{l}",tabLaTeX[i],fixed=TRUE))
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) gsub(pattern,"\\multicolumn{1}{r}",tabLaTeX[i],fixed=TRUE))
pattern <- "\\$\\textless 1\\textbackslash text\\{\\textbackslash sc\\{e\\}-\\}3\\$"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) gsub(pattern,"$<1\\text{\\sc{e}-3}$",tabLaTeX[i],fixed=TRUE))
pattern <- "\\$\\textbackslash rho\\$ (\\$p\\$)"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"$\\rho$ ($p$)",tabLaTeX[i],fixed=TRUE))
pattern <- "\\$\\textbackslash tau\\$ (\\$p\\$)"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"$\\tau$ ($p$)",tabLaTeX[i],fixed=TRUE))
pattern <- "\\$m\\$ (${R}^{2}$)"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"$m$ (${R}^{2}$)",tabLaTeX[i],fixed=TRUE))

write(tabLaTeX, file = file.path(dirsGit$Plots,"tabCorrs.tex"))

# Table 4: bridge data ------------------------------------
load(file.path(dirs$DataDirAnalysis,"20150615_FailDataFrameWithNBImatAndTypeCodes.RData"))
# load(file.path(dirs$DataDirAnalysis,"20151205_USGS_DaymetVIC_CorrelationAndTestStatistics.RData"))
BridgesDataFrame$LABEL <- MakeBridgeLabel(BridgesDataFrame,SEP = "--")
BridgesDataFrame[BridgesDataFrame$ID==1631,"COMMENT_T_REPORT_NAME"] <- substr(BridgesDataFrame[BridgesDataFrame$ID==1631,"COMMENT_T_REPORT_NAME"],1,18)
BridgesDataFrame[!BridgesDataFrame$BOOL_HAS_FAIL_DATE,"DATE_FAIL_EST_USGS"] <- paste(BridgesDataFrame[!BridgesDataFrame$BOOL_HAS_FAIL_DATE,"DATE_FAIL_EST_USGS"],"${}^*$",sep="")
BridgesDataFrame <- join(BridgesDataFrame[,!(colnames(BridgesDataFrame) %in% c("TYPE"))],FailDataFrame[,c("ID","TYPE","MATER")],by="ID")

cols <- c("LABEL","DATE_FAIL_EST_USGS","FAIL_CAUS","COMMENT_SHORT","COMMENT_LINKED_HURRICANE","TYPE","MATER","STAID","DRAIN_SQKM","AREA_RATIO_NHD","DIST_TO_GAGE","COUNT_P_USGS","COMMENT_REGULATION_SHORT","MK_TAU","AREA_RATIO_DVICG","NASH_M_DVICG","NSE_D_DVICG","BOOL_KS_ALL_DATA_DVICG_PASS")
FirstColNames <- c("","","","","","","","AREA","AREA RATIO","DIST. GAGE","","","MK","DAYMET-VIC", "","","")
SecColNames <- c("BUILT--FAIL--STATE","FAIL DATE","CAUSE","COMMENT","HURRICANE","TYPE","MAT","STAID","[SQKM]","BR:GA","[KM]","NO. PEAKS","BOOL_REGULATION","TREND","AREA RATIO","NSE-MONTH","NSE-DAY", "KS $\\alpha$")
tabBridges <- BridgesDataFrame[,cols]
tabBridges$MK_TAU <- factor(tabBridges$MK_TAU > 0, levels = c(TRUE, FALSE), labels = c("POS","NEG"))
tabBridges$MK_TAU <- as.character(tabBridges$MK_TAU)
tabBridges[BridgesDataFrame$BOOL_MK_SIGNIF==TRUE,"MK_TAU"] <- paste(tabBridges[BridgesDataFrame$BOOL_MK_SIGNIF==TRUE,"MK_TAU"],"${}^{\\dagger}$",sep="")

tabLaTeX <- stargazer(tabBridges,summary = FALSE, rownames = FALSE,
                      digits = 1, digits.extra = 3,
                      align = TRUE,
                      title = "Failure and gage site information with results from Mann-Kendall (MK) trend test and Daymet-VIC reanalysis monthly and daily Nash-Sutcliffe Efficiency (NSE) and highest Kolmogorov-Smirnov (KS) $\\alpha$ passed using all available annual maximum daily mean flow data. Regulation data from USGS.",
                      column.sep.width = "1pt", font.size = "tiny",
                      label = "tab:BridgeData",
                      notes = "${}^*$ Failure date unknown and assumed to coincide with date of maximum daily mean in failure year.",
                      float.env = "sidewaystable",
                      multicolumn = FALSE
)
tabLaTeX[12] <- paste("\\multicolumn{1}{l}{",paste(FirstColNames[1:(length(FirstColNames)-4)], collapse= "} & \\multicolumn{1}{r}{"),"} & \\multicolumn{4}{c}{",FirstColNames[length(FirstColNames)-3] ,"}  \\\\",sep="")
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

# fix MK significance
pattern  <- "${}^{\\textbackslash dagger\\}\\$"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"${}^{\\dagger}$",tabLaTeX[i],fixed=TRUE))

# fix decimal places for area
pattern <- "\\begin{tabular}{@{\\extracolsep{1pt}} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1}  D{.}{.}{-1}"
tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)[1]] <- sub(pattern,"\\begin{tabular}{@{\\extracolsep{1pt}} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-0}",tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)[1]],fixed = TRUE)
pattern <- "[^&]{10,43} & [^&]{10,60} & [^&]{3,60} & [^&]{0,60} & [^&]{0,60} & [^&]{1,30} & [0123456789,]{0,3}[[:space:]]{0,1}[0123456789]{1,3}."
matchRows <- grep(pattern, tabLaTeX)
lastMatch <- sapply(matchRows, function(i) regexpr(pattern,tabLaTeX[i]) + attr(regexpr(pattern,tabLaTeX[i]),"match.length") - 1)
tabLaTeX[matchRows] <- sapply(1:length(matchRows), function(i) paste(substr(tabLaTeX[matchRows[i]],1,lastMatch[i]-1),substr(tabLaTeX[matchRows[i]],lastMatch[i]+2,nchar(tabLaTeX[matchRows[i]])),sep=""))

tabLaTeX[15:length(tabLaTeX)] <- sapply(15:length(tabLaTeX), function(i) sub("c}","l}",tabLaTeX[i]))
tabLaTeX[15:length(tabLaTeX)] <- sapply(15:length(tabLaTeX), function(i) sub("c}","r}",tabLaTeX[i]))

# additional footnote
tabLaTeX <- c(tabLaTeX[1:(length(tabLaTeX)-2)],
              "\\multicolumn{16}{l}{${}^\\dagger$ Statistically significant ($\\alpha = 0.05$).} \\\\",
              tabLaTeX[(length(tabLaTeX)-1):length(tabLaTeX)])

tabLaTeX    <- c(tabLaTeX[1:8], "\\resizebox{\\textheight}{!}{", tabLaTeX[9:(length(tabLaTeX)-1)],"}",tabLaTeX[length(tabLaTeX)])


write(tabLaTeX, file = file.path(dirsGit$Plots,"tabBridges_new2.tex"))
