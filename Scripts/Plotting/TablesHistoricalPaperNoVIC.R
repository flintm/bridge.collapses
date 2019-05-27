# Make tables for manuscript on historical bridge failures
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

load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirsGit$Data,"StateCountyCityData.RData"))
source(file.path(dirsGit$ScriptsPlot,"SetupEncoding.R"))
source(file.path(dirsGit$ScriptsPlot,"MakeBridgeLabel.R"))

library(stargazer)
require(plyr)

BridgesDataFrame           <- df.Fail.NBI.Gage[rowsToView,]
BridgesDataFrame           <- BridgesDataFrame[order(BridgesDataFrame$FAIL_CAUS_CODE,BridgesDataFrame$YR_BLT_EST, decreasing = FALSE, na.last = TRUE),]
BridgesDataFrame$FROM_INST <- !is.na(BridgesDataFrame$Q_FAIL_I_USGS)
BridgesDataFrame$LABEL     <- MakeBridgeLabel(BridgesDataFrame, SUPER_FIELDS = c("Hurr","Area","FailDate"))

dataTypeLab                   <- sapply(1:nrow(BridgesDataFrame), 
                                        function(i)
                                          ifelse(!is.na(BridgesDataFrame[i,"T_FAIL_P_HECP_USGS"]),
                                                 "P",
                                                 ifelse(!is.na(BridgesDataFrame[i,"T_FAIL_I_HECP_USGS"]),
                                                        "I",
                                                        "D")))
dataTypeLab[dataTypeLab!="D"] <- paste("${}^{",dataTypeLab[dataTypeLab!="D"],"}$",sep = "")
dataTypeLab[dataTypeLab=="D"] <- ""

BridgesDataFrame[is.na(BridgesDataFrame$NSE_D_DVICG),"AREA_RATIO_DVICG"] <- NA_real_

BridgesDataFrame <- BridgesDataFrame[order(BridgesDataFrame$FAIL_CAUS_CODE,BridgesDataFrame$YR_BLT_EST,BridgesDataFrame$YR_FAIL, na.last = TRUE),]

# Table 1: bridge data ------------------------------------
BridgesDataFrame$LABEL <- MakeBridgeLabel(BridgesDataFrame,SEP = "--")
BridgesDataFrame[BridgesDataFrame$ID==1631,"COMMENT_T_REPORT_NAME"]       <- substr(BridgesDataFrame[BridgesDataFrame$ID==1631,"COMMENT_T_REPORT_NAME"],1,18)
BridgesDataFrame[!BridgesDataFrame$BOOL_HAS_FAIL_DATE,"DATE_FAIL_EST_USGS"] <- paste(BridgesDataFrame[!BridgesDataFrame$BOOL_HAS_FAIL_DATE,"DATE_FAIL_EST_USGS"],"${}^*$",sep="")

colsAll           <- c("LABEL","DATE_FAIL_EST_USGS","FAIL_CAUS","COMMENT_SHORT","COMMENT_LINKED_HURRICANE","TYPE","MATER","STAID","DRAIN_SQKM","AREA_RATIO_NHD","DIST_TO_GAGE","COUNT_P_USGS","REGIONAL_SKEW","COMMENT_REGULATION_SHORT","MK_TAU")
FirstColNames     <- c("","","","","","","","","AREA","AREA RATIO","DIST. GAGE","","","","MK")
SecColNames       <- c("BUILT--FAIL--STATE","FAIL DATE","CAUSE","COMMENT","HURRICANE","TYPE","MAT","STAID","[KM${}^2$]","BR:GA","[KM]","NO. PEAKS","SKEW","REGULATION","TREND")
tabBridges        <- BridgesDataFrame[,colsAll]
tabBridges$MK_TAU <- factor(tabBridges$MK_TAU > 0, levels = c(TRUE, FALSE), labels = c("POS","NEG"))
tabBridges$MK_TAU <- as.character(tabBridges$MK_TAU)
tabBridges[BridgesDataFrame$BOOL_MK_SIGNIF==TRUE,"MK_TAU"] <- paste(tabBridges[BridgesDataFrame$BOOL_MK_SIGNIF==TRUE,"MK_TAU"],"${}^{\\dagger}$",sep="")

#statistics
cols             <- c("YR_BLT_EST","YR_FAIL","DRAIN_SQKM","AREA_RATIO_NHD","DIST_TO_GAGE", "COUNT_P_USGS")
rowsToAnalyzeVIC <- rownames(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView & df.Fail.NBI.Gage$YR_FAIL_EST >= "1980-01-01",])

df.Fail.NBI.Gage$COUNT_P_USGS <- as.integer(df.Fail.NBI.Gage$COUNT_P_USGS)
dateCols             <- c("YR_BLT_EST","YR_FAIL")
meanStats            <- sapply(cols, function(col) c(mean(df.Fail.NBI.Gage[rowsToView,col],na.rm=TRUE), mean(df.Fail.NBI.Gage[rowsToAnalyzeVIC,col],na.rm=TRUE)))
meanStats[,dateCols] <- sapply(dateCols, function(col) substr(as.character(as.Date(meanStats[,col],as.Date("1970-01-01"))),1,4))
meanStats[,cols[!(cols %in% dateCols)]] <- sapply(cols[!(cols %in% dateCols)], function(col) signif(as.numeric(meanStats[,col]),3))
stdStats             <- sapply(cols, function(col) c(sd(df.Fail.NBI.Gage[rowsToView,col],na.rm=TRUE), sd(df.Fail.NBI.Gage[rowsToAnalyzeVIC,col],na.rm=TRUE)))
stdStats[,dateCols]  <- sapply(dateCols, function(col) signif(stdStats[,col]/365.25,2))
stdStats[,cols[!(cols %in% dateCols)]] <- sapply(cols[!(cols %in% dateCols)], function(col) signif(as.numeric(stdStats[,col]),3))
rangeStats            <- sapply(cols, function(col) c(range(df.Fail.NBI.Gage[rowsToView,col],na.rm=TRUE), range(df.Fail.NBI.Gage[rowsToAnalyzeVIC,col],na.rm=TRUE)))
rangeStats[,dateCols] <- sapply(dateCols, function(col) substr(as.character(as.Date(rangeStats[,col],as.Date("1970-01-01"))),1,4))
rangeStats[,cols[!(cols %in% dateCols)]] <- sapply(cols[!(cols %in% dateCols)], function(col) signif(as.numeric(rangeStats[,col]),3))
USGStab <- t(as.matrix(sapply(cols, function(col) paste("$",meanStats[1,col],"\\pm",stdStats[1,col],'(',rangeStats[1,col],"-",rangeStats[2,col],")$",sep=""))))
USGStab <- as.data.frame(USGStab, stringsAsFactors = FALSE)
USGStab[1,"LABEL"]         <- paste("BLT:",USGStab[1,"YR_BLT_EST"])
USGStab[1,"DATE_FAIL_EST_USGS"] <- paste("FAIL:", USGStab[1,"YR_FAIL"])
USGStab[,colsAll[!(colsAll %in% colnames(USGStab))]] <- sapply(1: length(colsAll[!(colsAll %in% colnames(USGStab))]), function(j) NA)
USGStab  <- USGStab[,colsAll]
StatsRow <- paste("\\multicolumn{1}{l}{",
                 paste(USGStab[1,], collapse= "} & \\multicolumn{1}{l}{"),
                 "}  \\\\",sep="")
StatsRow <- gsub("NA","",StatsRow)
StatsRow <- paste(substr(StatsRow,1,63),"2",substr(StatsRow,65,99),substr(StatsRow,123,nchar(StatsRow)),sep="")

tabLaTeX <- stargazer(tabBridges,summary = FALSE, rownames = FALSE,
                      digits = 1, digits.extra = 3,
                      align = TRUE,
                      title = "Failed bridge and related gage site information with results from Mann-Kendall (MK) trend test. Regulation data from USGS. Regional skews from Bulletin 17B. Statistics for the entire set are presented in the last row as mean $\\pm$ standard deviation (minimum - maximum).",
                      column.sep.width = "1pt", font.size = "tiny",
                      label = "tab:BridgeData",
                      notes = "${}^*$ Failure date unknown and assumed to coincide with date of maximum daily mean in failure year.",
                      float.env = "sidewaystable",
                      multicolumn = FALSE
)
tabLaTeX[12] <- paste("\\multicolumn{1}{l}{",
                      paste(FirstColNames[1:length(FirstColNames)], collapse= "} & \\multicolumn{1}{c}{"),
                      "}  \\\\",sep="")
tabLaTeX <- c(tabLaTeX[c(1:12)],paste("\\multicolumn{1}{c}{",paste(SecColNames, collapse= "} & \\multicolumn{1}{c}{"), "} \\\\",sep="") ,tabLaTeX[c(13:length(tabLaTeX))])

# fix labels
pattern <- "[${hatmkern6u\\}]{22,25}"
tabLaTeX[15:(length(tabLaTeX)-4)][grep(pattern,tabLaTeX[15:(length(tabLaTeX)-4)])] <- sapply(grep(pattern,tabLaTeX[15:(length(tabLaTeX)-4)]), function(i) gsub(pattern,"${}^{",tabLaTeX[15:(length(tabLaTeX)-4)][i]))
pattern <- "[texasrikcnd]{19,20}"
tabLaTeX[15:(length(tabLaTeX)-4)][grep(pattern,tabLaTeX[15:(length(tabLaTeX)-4)])] <- sapply(grep(pattern,tabLaTeX[15:(length(tabLaTeX)-4)]), function(i) gsub(pattern,"*",tabLaTeX[15:(length(tabLaTeX)-4)][i]))
pattern <- "\\$}"
rows <- which(regexpr(pattern,tabLaTeX,fixed = TRUE) > 0 & regexpr(pattern,tabLaTeX,fixed=TRUE) < 100)
rows <- rows[rows>=15 & rows < (length(tabLaTeX) - 3)]
tabLaTeX[rows] <- sapply(rows, function(i) sub(pattern,"}$}",tabLaTeX[i], fixed = TRUE))

# fix MK significance
pattern  <- "${}^{\\textbackslash dagger\\}\\$"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"${}^{\\dagger}$",tabLaTeX[i],fixed=TRUE))

# fix decimal places for area
pattern   <- "\\begin{tabular}{@{\\extracolsep{1pt}} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1}  D{.}{.}{-1}"
tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)[1]] <- sub(pattern,"\\begin{tabular}{@{\\extracolsep{1pt}} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-1} D{.}{.}{-0}",tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)[1]],fixed = TRUE)
pattern   <- "[^&]{10,43} & [^&]{10,60} & [^&]{3,60} & [^&]{0,60} & [^&]{0,60} & [^&]{1,30} & [0123456789,]{0,3}[[:space:]]{0,1}[0123456789]{1,3}."
matchRows <- grep(pattern, tabLaTeX)
lastMatch <- sapply(matchRows, function(i) regexpr(pattern,tabLaTeX[i]) + attr(regexpr(pattern,tabLaTeX[i]),"match.length") - 1)
tabLaTeX[matchRows] <- sapply(1:length(matchRows), function(i) paste(substr(tabLaTeX[matchRows[i]],1,lastMatch[i]-1),substr(tabLaTeX[matchRows[i]],lastMatch[i]+2,nchar(tabLaTeX[matchRows[i]])),sep=""))

# stats
tabLaTeX <- c(tabLaTeX[1:50],
              StatsRow,
              "\\hline \\\\[-1.8ex] ",
              tabLaTeX[51:length(tabLaTeX)])

tabLaTeX[15:length(tabLaTeX)] <- sapply(15:length(tabLaTeX), function(i) gsub("{1}{c}","{1}{l}",tabLaTeX[i],fixed = TRUE))
# tabLaTeX[15:length(tabLaTeX)] <- sapply(15:length(tabLaTeX), function(i) gsub("c}","r}",tabLaTeX[i]))

# additional footnote
tabLaTeX <- c(tabLaTeX[1:(length(tabLaTeX)-2)],
              "\\multicolumn{8}{l}{${}^\\dagger$ Statistically significant ($\\alpha = 0.05$).} \\\\",
              tabLaTeX[(length(tabLaTeX)-1):length(tabLaTeX)])
tabLaTeX <- c(tabLaTeX[1:8], "\\resizebox{\\textheight}{!}{", tabLaTeX[9:(length(tabLaTeX)-1)],"}",tabLaTeX[length(tabLaTeX)])

write(tabLaTeX, file = file.path(dirsGit$Plots,"tabBridges_new3.tex"))

# Table 2: failure and max data and return periods, no max pre-fail, and report citation -------------------------------------------------------------------------------------------------------------------
BridgesDataFrame$LABEL <- MakeBridgeLabel(BridgesDataFrame, SUPER_FIELDS = c("Hurr","Area","FailDate"))
cfs2m3s <- 1/35.3147
Q_cols <- colnames(df.Fail.NBI.Gage)[grepl("\\<Q_",colnames(df.Fail.NBI.Gage))]
Q_cols_m3s <- paste(Q_cols, "m3s",sep="_")
df.Fail.NBI.Gage[,Q_cols_m3s] <- sapply(Q_cols, function(j) df.Fail.NBI.Gage[,j]*cfs2m3s)
cols <- c("LABEL", "STAID", 
          "Q_FAIL_D_USGS_m3s",    "Q_FAIL_IP_USGS_m3s",  "Q_MAX_D_USGS_m3s",     "Q_MAX_P_USGS_m3s",
          "T_FAIL_D_HECD_USGS","T_FAIL_IP_HECP_USGS","T_REPORT","T_MAX_D_HECD_USGS", "T_MAX_P_HECP_USGS", 
          "FAIL_CAUS_CODE")
colNamesShort <- c("","","D","I/P","D","P","D","I/P","","D","P","")
topCols       <- "\\multicolumn{1}{l}{Year built--year collapsed--state} & \\multicolumn{1}{c}{STAID} & \\multicolumn{4}{c}{Q [m${}^3$/s]} & \\multicolumn{5}{c}{$T_R$ [yr]} & \\multicolumn{1}{c}{Cause} \\\\"
secCols       <- " & & \\multicolumn{2}{c}{Collapse} & \\multicolumn{2}{c}{Maximum} & \\multicolumn{3}{c}{Collapse} & \\multicolumn{2}{c}{Maximum}  & \\\\"
tabQT         <- BridgesDataFrame[,cols]
tabQT$FAIL_CAUS_CODE <- as.character(tabQT$FAIL_CAUS_CODE)
# ConfNonOverlap <- BridgesDataFrame$T_FAIL_PM2_CONF_VIC_GAGE_LARGER | BridgesDataFrame$T_FAIL_PM2_CONF_USGS_LARGER
# ConfNonOverlapSym <- rep("",nrow(BridgesDataFrame))
# ConfNonOverlapSym[ConfNonOverlap] <- "${}^{\\dagger}$"
# text <- paste(round(tabQT$T_FAILPM2_HEC_DVICG),ConfNonOverlapSym,sep="")
# text[text=="NA"] <- ""
# tabQT$T_FAILPM2_HEC_DVICG <- text
tabQT$T_REPORT <- paste(tabQT$T_REPORT,BridgesDataFrame$COMMENT_T_REPORT_CITE)

source(file.path(dirsGit$Scripts,"AnalyzeReturnPeriodDist.R"))
ls.Stats <- AnalyzeReturnPeriodDist(BridgesDataFrame, Tls = c("T_FAIL_D_HECD_USGS","T_FAIL_IP_HECP_USGS","T_MAX_D_HECD_USGS", "T_MAX_P_HECP_USGS"))
T_stats  <- ls.Stats[["T_stats"]]
Medians  <- round(c(T_stats[["FAIL"]]["median",colnames(T_stats[["FAIL"]]) %in% cols], T_stats[["MAX"]]["median",colnames(T_stats[["MAX"]]) %in% cols]))
Means    <- round(c(T_stats[["FAIL"]]["mean",colnames(T_stats[["FAIL"]]) %in% cols], T_stats[["MAX"]]["mean",colnames(T_stats[["MAX"]]) %in% cols]))
tabQT[nrow(tabQT)+1,names(Medians)] <- Medians # add mean later
tabQT[nrow(tabQT),"LABEL"] <- "MEDIAN (MEAN)"

tabLaTeX <- stargazer(tabQT,summary = FALSE, rownames = FALSE,
                      digits = 1, digits.extra = 2,
                      align = TRUE,
                      title = "Flow values $Q$ and return periods $T_R$ of failure and maximum events using USGS data and the Bulletin 17B methodology.",
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
                  "\\noalign{\\smallskip} \\cline{3-6} \\cline{7-11} \\noalign{\\smallskip}",
                   secCols, 
                  "\\noalign{\\smallskip} \\cline{3-4} \\cline{5-6} \\cline{7-9} \\cline{10-11} \\noalign{\\smallskip}",
                  tabLaTeX[12:length(tabLaTeX)])
tabLaTeX     <- c(tabLaTeX[1:8], "\\resizebox{\\textheight}{!}{", tabLaTeX[9:(length(tabLaTeX)-1)],"}",tabLaTeX[length(tabLaTeX)])

# make multirows
pattern <- "\\multicolumn{1}{l}{BUILT--FAIL--STATE} & \\multicolumn{1}{c}{STAID}"
rep     <- "\\multirow{4}{*}{BUILT--FAIL--STATE} & \\multirow{4}{*}{STAID}"
tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)] <- sapply(grep(pattern,tabLaTeX, fixed = TRUE), function(i) sub(pattern,rep,tabLaTeX[i], fixed = TRUE))
pattern <- "\\multicolumn{1}{c}{CAUSE}"
rep     <- "\\multirow{5}{*}{CAUSE}"
tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)] <- sapply(grep(pattern,tabLaTeX, fixed = TRUE), function(i) sub(pattern,rep,tabLaTeX[i], fixed = TRUE))


# fix labels
pattern <- "[${hatmkern6u\\}]{22,25}"
tabLaTeX[grep(pattern,tabLaTeX)] <- sapply(grep(pattern,tabLaTeX), function(i) sub(pattern,"${}^{",tabLaTeX[i]))
pattern <- "[\\texasrikcnd]{21}"
tabLaTeX[grep(pattern,tabLaTeX)] <- sapply(grep(pattern,tabLaTeX), function(i) sub(pattern,"*",tabLaTeX[i]))
pattern <- "[$\\}]{4}"
tabLaTeX[grep(pattern,tabLaTeX)] <- sapply(grep(pattern,tabLaTeX), function(i) sub(pattern,"}$",tabLaTeX[i]))
# pattern <- "\\$\\{\\}$\\hat{\\mkern6mu}$\\{\\textbackslash dagger\\}\\$"
# tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)] <- sapply(grep(pattern,tabLaTeX, fixed = TRUE), function(i) sub(pattern,"${}^{\\dagger}$",tabLaTeX[i], fixed = TRUE))
# pattern <- "\\$\\{}$hat{\\mkern6mu}$"
# tabLaTeX[grep(pattern,tabLaTeX, fixed = TRUE)] <- sapply(grep(pattern,tabLaTeX, fixed = TRUE), function(i) sub(pattern,"${}^{\\dagger}$",tabLaTeX[i], fixed = TRUE))

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
pattern   <- "[^&]{10,37} & [^&]{25,37} & [0123456789,]{3,7} & [0123456789,]{5,7}"
matchRows <- grep(pattern, tabLaTeX[1:54])
dataRows  <- which(dataTypeLab!="")
lastMatch <- sapply(matchRows, function(i) regexpr(pattern,tabLaTeX[i]) + attr(regexpr(pattern,tabLaTeX[i]),"match.length") - 1)
tabLaTeX[matchRows] <- sapply(1:length(matchRows), function(i) paste(substr(tabLaTeX[matchRows[i]],1,lastMatch[i]),dataTypeLab[dataRows[i]],substr(tabLaTeX[matchRows[i]],lastMatch[i]+1,nchar(tabLaTeX[matchRows[i]])),sep=""))
pattern   <- "\\begin{tabular}{@{\\extracolsep{1pt}} D{.}{.}{-0} D{.}{.}{-0} D{.}{.}{-0} "
matchRows <- grep(pattern, tabLaTeX[1:54],fixed=TRUE)
lastMatch <- sapply(matchRows, function(i) regexpr(pattern,tabLaTeX[i],fixed=TRUE) + attr(regexpr(pattern,tabLaTeX[i],fixed=TRUE),"match.length") - 1)
tabLaTeX[matchRows] <- sapply(1:length(matchRows), function(i) paste(substr(tabLaTeX[matchRows[i]],1,lastMatch[i]),"r",substr(tabLaTeX[matchRows[i]],lastMatch[i]+12,nchar(tabLaTeX[matchRows[i]])),sep=""))

# add Inst/Pk labels to T
pattern   <- "[^&]{20,37} & [^&]{25,37} & [^&]{3,43} & [^&]{0,30} & [^&]{0,30} & [^&]{0,30} & [^&]{0,30} & [0123456789,]{1,7}"
matchRows <- grep(pattern, tabLaTeX[20:54]) + 19
dataRows  <- which(dataTypeLab!="")
lastMatch <- sapply(matchRows, function(i) regexpr(pattern,tabLaTeX[i]) + attr(regexpr(pattern,tabLaTeX[i]),"match.length") - 1)
tabLaTeX[matchRows] <- sapply(1:length(matchRows), function(i) paste(substr(tabLaTeX[matchRows[i]],1,lastMatch[i]),dataTypeLab[dataRows[i]],substr(tabLaTeX[matchRows[i]],lastMatch[i]+1,nchar(tabLaTeX[matchRows[i]])),sep=""))
pattern   <- "\\begin{tabular}{@{\\extracolsep{1pt}} D{.}{.}{-0} D{.}{.}{-0} D{.}{.}{-0} r D{.}{.}{-0} D{.}{.}{-0} D{.}{.}{-0} "
matchRows <- grep(pattern, tabLaTeX,fixed=TRUE)
lastMatch <- sapply(matchRows, function(i) regexpr(pattern,tabLaTeX[i],fixed=TRUE) + attr(regexpr(pattern,tabLaTeX[i],fixed=TRUE),"match.length") - 1)
tabLaTeX[matchRows] <- sapply(1:length(matchRows), function(i) paste(substr(tabLaTeX[matchRows[i]],1,lastMatch[i]),"r",substr(tabLaTeX[matchRows[i]],lastMatch[i]+12,nchar(tabLaTeX[matchRows[i]])),sep=""))

# fix T_R info
pattern <- "10\\textless TR\\textless 25"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"$10<T_R<25$",tabLaTeX[i],fixed=TRUE))
pattern <- "Storm 500"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"500 (storm)",tabLaTeX[i],fixed=TRUE))

# add mean data
pattern   <- "& [[:digit:]]{2,4} "
for (i in 1:length(Means)){
  matchLims    <- c(regexpr(pattern,tabLaTeX[55]), regexpr(pattern,tabLaTeX[55]) + attr(regexpr(pattern,tabLaTeX[55]),"match.length")-1)
  tabLaTeX[55] <- paste(substr(tabLaTeX[55],1,matchLims[1]+1)," \\multicolumn{1}{c}{",substr(tabLaTeX[55],matchLims[1]+2,matchLims[2]-1)," (",Means[i],")} ",substr(tabLaTeX[55],matchLims[2],nchar(tabLaTeX[55])),sep="")
}


# alignment
tabLaTeX[20:55] <- sapply(20:55, function(i) gsub("{c}","{l}",tabLaTeX[i],fixed=TRUE))
# tabLaTeX[20:55] <- sapply(20:55, function(i) gsub("{c}","{r}",tabLaTeX[i],fixed=TRUE))
tabLaTeX <- c(tabLaTeX[1:54], "\\hline \\\\[-1.8ex] ", tabLaTeX[55:length(tabLaTeX)])

# add footnotes:
tabLaTeX <- c(tabLaTeX[1:(length(tabLaTeX)-3)],
              "\\multicolumn{12}{l}{${}^\\mathrm{H}$ link to hurricane at time of failure.} \\\\",
              "\\multicolumn{12}{l}{${}^{+ (++)}$ bridge drainage area $>1.2(1.4)$ times greater than gauge drainage area.} \\\\",
              "\\multicolumn{12}{l}{${}^{- (- -)}$ bridge drainage area $<0.8(0.6)$ times smaller than gauge drainage area.} \\\\",
              "\\multicolumn{12}{l}{${}^*$ fail date assumed.} \\\\",
              tabLaTeX[(length(tabLaTeX)-2):length(tabLaTeX)])
write(tabLaTeX, file = file.path(dirsGit$Plots,"tabQT_new_v4.tex"))


# Table 3: correlations -----------------------------------------------------------------------------------------
# USGS data horizontal, Daymet-VIC data vertical
source(file.path(dirsGit$Scripts,"CorrelationOfMaxes.R"))
ls.corrs.max <- CorrelationOfMaxes(TYPES = c("MAXQ","USGS-TMAX"),SAVE = FALSE)
source(file.path(dirsGit$ScriptsDir,"CorrelationOfFailures.R"))
ls.corrs.fail <- CorrelationOfFailures(TYPES = c("FAILQ","USGS"), SAVE = FALSE)
rm(CorrelationOfMaxes, CorrelationOfFailures)

rowDataTypes <- c("D", "I/P") # all USGS
colDataTypes <- c("Fail Q I/P",   "Max Q P","Fail T I/P", "Max T P", "Fail T Partial D", "Fail T Partial I/P")
ls.corrs.all <- c(ls.corrs.fail[["FAILQ"]][grepl("USGSdUSGSip",names(ls.corrs.fail[["FAILQ"]]))],
                  ls.corrs.max[["MAXQ"]][grepl("USGSdUSGSp",names(ls.corrs.max[["MAXQ"]]))],
                  ls.corrs.fail[["USGS"]][grepl("HECdHECip",names(ls.corrs.fail[["USGS"]])) | grepl("HECdPartInterpd",names(ls.corrs.fail[["USGS"]])) | 
                                            grepl("HECipPartInterpip",names(ls.corrs.fail[["USGS"]])) | grepl("HECipPartInterpd",names(ls.corrs.fail[["USGS"]])) | 
                                            grepl("HECdPartInterpip",names(ls.corrs.fail[["USGS"]]))],
                  ls.corrs.max[["USGS-TMAX"]][grepl("HECdHECp",names(ls.corrs.max[["USGS-TMAX"]]))]
                  )
# then want LR (R^2), rho (p), tau (p) for all of these
# row1: USGS d: USGS-IP-[Qf Qm Tf Tm]  Partial[Tfd Tfip] -> ls.corrs.FailQ[[USGSdUSGSip]] ls.corrs.max[[USGSdUSGSp]] ls.corrs.USGS [[HECdHECip]]  ls.corrs.Tmax.USGS[[HECdHECp]]  ls.corrs.USGS[[HECdPartInterpd]] ls.corrs.USGS[[HECdPartInterpip]]
# row2 : USGS d: USGS-IP-[Tf Tm] Partial[Tf-Day] Partial[TF-IP]   ->  - - - - ls.corrs.USGS[[HECipPartInterpd]] ls.corrs.USGS[[HECipPartInterpip]]
rownames <- c(paste("USGS_D",c("LR","rho","tau"),sep="_"),paste("USGS_IP",c("LR","rho","tau"),sep="_"))
rows     <- data.frame(first = c("USGSdUSGSip", "USGSdUSGSp","HECdHECip","HECdHECp",  "HECdPartInterpd", "HECdPartInterpip"),
                       sec   = c(rep(NA_character_,4) ,                               "HECipPartInterpd", "HECipPartInterpip"),
                       stringsAsFactors = FALSE)
types <- c("LR","P","K","Papprox","Kapprox")
subtypes <- list(LR=c("coefficients","r.squared"),
                 P=c("estimate","p.value"),
                 K=c("estimate","p.value"))
colnames <- c(paste("USGS_IP_Q",c("Fail_val","Fail_p","Fail_p_approx","Max_val","Max_p","Max_p_approx"),sep="_"),paste("USGS_IP_T",c("Fail_val","Fail_p","Fail_p_approx","Max_val","Max_p","Max_p_approx"),sep="_"),
              paste("PARTIAL_D",c("Fail_val","Fail_p","Fail_p_approx"),sep="_"),paste("PARTIAL_IP",c("Fail_val","Fail_p","Fail_p_approx"),sep="_"))

tabCorr <- data.frame(matrix(vector(), length(rownames), length(colnames),
                        dimnames=list(rownames, colnames)),
                        stringsAsFactors=F)
for (row in 1:ncol(rows)){
  for (i in which(!is.na(rows[,row]))){
    print(paste("row =",row,"i =",i,"looking for data type =",rows[,row][i]))
    corrs <- ls.corrs.all[grepl(rows[,row][i],names(ls.corrs.all))]
    # LR and R^2
    tabCorr[1+3*(row-1),(i-1)*3+1] <- corrs[[names(corrs)[grep(types[1],names(corrs))]]][[subtypes[[types[1]]][1]]][2] # slope
    tabCorr[1+3*(row-1),(i-1)*3+2] <- summary(corrs[[names(corrs)[grepl(types[1],names(corrs))]]])[[subtypes[[types[1]]][2]]] # R^2
    # rho and p
    tabCorr[2+3*(row-1),(i-1)*3+1] <- corrs[[names(corrs)[grep(types[2],names(corrs))[1]]]][[subtypes[[types[2]]][1]]] # rho estimate
    tabCorr[2+3*(row-1),(i-1)*3+2] <- corrs[[names(corrs)[grep(types[2],names(corrs))[1]]]][[subtypes[[types[2]]][2]]] # p
    if(tabCorr[2+3*(row-1),(i-1)*3+2] < 5e-4){ # try approx
      tabCorr[2+3*(row-1),(i-1)*3+2] <- corrs[[names(corrs)[grep(types[4],names(corrs))[1]]]][[subtypes[[types[2]]][2]]] # p-approx
      tabCorr[2+3*(row-1),(i-1)*3+3] <- "a"
    }
    # tau and p
    tabCorr[3+3*(row-1),(i-1)*3+1] <- corrs[[names(corrs)[grep(types[3],names(corrs))[1]]]][[subtypes[[types[3]]][1]]] # tau estimate
    tabCorr[3+3*(row-1),(i-1)*3+2] <- corrs[[names(corrs)[grep(types[3],names(corrs))[1]]]][[subtypes[[types[3]]][2]]] # p
    if(tabCorr[3+3*(row-1),(i-1)*3+2] < 5e-4){ # try approx
      tabCorr[3+3*(row-1),(i-1)*3+2] <- corrs[[names(corrs)[grep(types[5],names(corrs))[1]]]][[subtypes[[types[3]]][2]]] # p-approx
      tabCorr[3+3*(row-1),(i-1)*3+3] <- "a"
    }
  }
}
roundCols <- colnames[c(1:2,4:5,7:8,10:11,13:14,16:17)]
for (col in roundCols){
  tabCorr[,col][tabCorr[,col]>10 & !is.na(tabCorr[,col])] <- round(tabCorr[,col][tabCorr[,col]>10& !is.na(tabCorr[,col])])
  tabCorr[,col][tabCorr[,col]>1 & tabCorr[,col]<10& !is.na(tabCorr[,col])] <- round(tabCorr[,col][tabCorr[,col]>1 & tabCorr[,col]<10 & !is.na(tabCorr[,col])],2)
  tabCorr[,col][tabCorr[,col]>10 & !is.na(tabCorr[,col])] <- round(tabCorr[,col][tabCorr[,col]>10& !is.na(tabCorr[,col])])
  tabCorr[,col][tabCorr[,col]<1 & !is.na(tabCorr[,col])] <- round(tabCorr[,col][tabCorr[,col]<1 & !is.na(tabCorr[,col])],3)
  tabCorr[,col] <- as.character(tabCorr[,col])
  tabCorr[,col][tabCorr[,col]=="0"] <- "$<1\\text{\\sc{e}-}3$"
  tabCorr[,col][tabCorr[,col]=="NA"] <- ""
}
approxCols <- colnames[c(3,6,9,12,15,18)]
tabCorr[,approxCols] <- sapply(approxCols, function(col) sub("a","${}^{a}$",tabCorr[,col]))
for (col in approxCols){
  tabCorr[,col][is.na(tabCorr[,col])] <- ""
}
collapseCols <- c(1,4,7,10,13,16)
for (col in collapseCols){
  tabCorr[,col] <- paste(tabCorr[,col]," (",tabCorr[,col+1],")",tabCorr[,col+2],sep="")
  tabCorr[,col][tabCorr[,col]==" ()" | tabCorr[,col]=="NA (NA)"] <- ""
}
tabCorr <- tabCorr[,collapseCols]
tabCorr$LABEL <- rep(c("$m$ ($R^2$)","$\\rho$ ($p$)","$\\tau$ ($p$)"),2)
tabCorr$DATA  <- c("D","","","I/P","","")
tabCorr <- tabCorr[,c(8,7,1:6)]

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
toptopCols   <- "& & \\multicolumn{2}{c}{USGS I/P, $Q$} & \\multicolumn{2}{c}{BULL 17B. I/P, $T\\_R$} & \\multicolumn{1}{c}{PART. DUR. D, $T\\_R$} & \\multicolumn{1}{c}{PART. DUR. I/P $T\\_R$}  \\\\"
tabLaTeX     <- c(tabLaTeX[c(1:11)],toptopCols,tabLaTeX[c(12:length(tabLaTeX))])
tabLaTeX[13] <- "& & \\multicolumn{1}{r}{FAIL} & \\multicolumn{1}{r}{MAX} & \\multicolumn{1}{r}{FAIL} & \\multicolumn{1}{r}{MAX} & \\multicolumn{1}{r}{FAIL} & \\multicolumn{1}{r}{FAIL} \\\\"
pattern <- "\\multicolumn{1}{c}{D}"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"\\multirow{3}{*}{D}",tabLaTeX[i],fixed=TRUE))
pattern <- "\\multicolumn{1}{c}{I/P}"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"\\multirow{3}{*}{I/P}",tabLaTeX[i],fixed=TRUE))
# pattern <- "\\multicolumn{1}{c}{\\$Q\\$}"
# tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) sub(pattern,"\\multirow{3}{*}{$Q$}",tabLaTeX[i],fixed=TRUE))
pattern <- "$T\\_R$"
tabLaTeX[grep(pattern,tabLaTeX,fixed=TRUE)] <- sapply(grep(pattern,tabLaTeX,fixed=TRUE), function(i) gsub(pattern,"$T_R$",tabLaTeX[i],fixed=TRUE))

# make horizontal lines
# tabLaTeX    <- c(tabLaTeX[1:8], "\\resizebox{0.8\\textheight}{!}{", tabLaTeX[9:(length(tabLaTeX)-1)],"}",tabLaTeX[length(tabLaTeX)])
insertHline  <- "\\hline \\\\[-1.8ex]"
tabLaTeX    <- c(tabLaTeX[1:17], insertHline, tabLaTeX[18:length(tabLaTeX)])

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

tabLaTeX <- c(tabLaTeX[1:22],"\\multicolumn{8}{l}{Note: D = daily mean; I = instantaneous; P = peak.} \\\\",tabLaTeX[23:length(tabLaTeX)])

write(tabLaTeX, file = file.path(dirsGit$Plots,"tabCorrs.tex"))

# reliability analysis results ----------------
source(file.path(dirsGit$Scripts,"FailureReliabilityAnalysis.R"))
rm(FailureReliabilityAnalysis)
load(file.path(dirsGit$Data, "FailureReliabilityComparison.RData"))
topCols <- c("","$T_Q$", "$p_f | T_Q$",  "$p_{f,annual}$", "$\%\Delta p_{f,annual}|T_R\downarrow$", "$\%\Delta p_{f,annual}|T_R\uparrow$")
pFa <- pFannualEstimates
rm(pFannualEstimates)
df <- data.frame(type = character(9),
                 T_Q  = character(9),
                 PFQ  = character(9),
                 PFa  = character(9),
                 PFa10p  = character(9),
                 PFa10l  = character(9))
df$type <- c("Nominal lifetime, $\beta = 3.5$", "Nominal event, $\beta = 1.75$", "Nominal event, $\beta = 1.75$",
             "Median D", "Median I/P","Mean D",
             "Mean I/P", "Kernel D", "Kernel I/P")
df$T_Q     <- c("-", "50", "100", 
                signif(c(pTdata[["d"]]["median"], pTdata[["ip"]]["median"], pTdata[["d"]]["mean"],
                pTdata[["ip"]]["mean"]),3),"all", "all")
df$PFQ     <- c("-",signif(c(pnorm(-1.75),pnorm(-1.75)),2),
                1,1,1,
                1, "varies","varies")
df$PFa     <- signif(c(pnorm(-3.5),pFa[["pFrel"]]["50y1pt75"], pFa[["pFrel"]]["100yr1pt75"],
                pFa[["pFD"]]["median"], pFa[["pFIP"]]["median"],pFa[["pFD"]]["mean"],
                pFa[["pFIP"]]["mean"], pFa[["pFD"]]["kernel"], pFa[["pFIP"]]["kernel"]),2)
df$PFa10p     <- signif(c(NA,pFa[["pFrel10p"]]["50y1pt75"], pFa[["pFrel10p"]]["100yr1pt75"],
                       pFa[["pFD10p"]]["median"], pFa[["pFIP10p"]]["median"],pFa[["pFD10p"]]["mean"],
                       pFa[["pFIP10p"]]["mean"], pFa[["pFD10p"]]["kernel"], pFa[["pFIP10p"]]["kernel"]),2)
df$PFa10l     <- signif(c(NA,pFa[["pFrel10l"]]["50y1pt75"], pFa[["pFrel10l"]]["100yr1pt75"],
                          pFa[["pFD10l"]]["median"], pFa[["pFIP10l"]]["median"],pFa[["pFD10l"]]["mean"],
                          pFa[["pFIP10l"]]["mean"], pFa[["pFD10l"]]["kernel"], pFa[["pFIP10l"]]["kernel"]),2)
df$PFa10pRel <- (df$PFa10p-df$PFa)/df$PFa
df$PFa10lRel <- (df$PFa10l-df$PFa)/df$PFa
df$Pf75yr    <- 1-exp(-df$PFa*75)
#df$PctDif  <- (df$PFa-df[1,"PFa"])/df[1,"PFa"]

tabLaTeX <- stargazer(df,summary = FALSE, rownames = FALSE,
                      digits = 5, digits.extra = 3,
                      align = TRUE,
                      title = "Analysis of annual failure probability using nominal lifetime and flood-event reliability, central values of estimated collapse flow return periods, and kernel-smoothed distribution of collapse flow return periods.",
                      column.sep.width = "1pt",
                      label = "tab:ReliabilityData",
                      multicolumn = FALSE
)      
tabLaTeX[11] <- paste("\\multicolumn{1}{l}{",
                      paste(topCols[1:(length(topCols))], collapse= "} & \\multicolumn{1}{c}{"),
                      "}  \\\\",sep="")
write(tabLaTeX, file = file.path(dirsGit$Plots,"tabReliability.tex"))
