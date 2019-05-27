# rename all variables in standard formt:
# VARIABLE _ EVENT (incl. if +/- days)_ DATA INTERVAL _ <ANALYSIS (with data source as D/P)> _ <CONFIDENCE INTERVAL> _ DATA SOURCE

# examples:
# T_FAIL_D_HECD_USGS
# Q_MAXPREFAIL_P_USGS
# T_FAILPM2_D_05_HECD_VICG

# all options
# VARIABLES: Q, T, DATE, BOOL, COMMENT (get rid of EXCEED), COUNT (for counts), AREA_RATIO, 
# EVENTS: FAIL, FAILPM2, MAX, MAXPREFAIL, MAXFAILYEAR
# DATA INTERVAL: D, I, P, IP
# ANALYSIS: HECP, HECD, GEV, LP3, P3, PART
# CONFIDENCE INTERVAL: 01, 05, 95, 99
# DATA SOURCE: USGS, DVICG, DVICB, NLDASG, NLDASB

load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))

# begin correction
pattern <- "DAYMET_VIC_GAGE"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"DVICG",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])
pattern <- "VIC_GAGE"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"DVICG",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])
pattern <- "_VICG"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"_DVICG",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

pattern <- "DAYMET_VIC_BRIDGE"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"DVICB",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])
pattern <- "VIC_BRIDGE"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"DVICB",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])
pattern <- "_VICB"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"_DVICB",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

pattern <- "NLDAS_BRIDGE"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"NLDASB",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])
pattern <- "NLDAS_GAGE"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"NLDASG",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

pattern <- "FAIL_Q"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"Q_FAIL",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])
pattern <- "MAX_Q"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"Q_MAX",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])
pattern <- "MAX_Q_PRE_FAIL"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"Q_MAXPREFAIL",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

pattern <- "FAIL_DATE"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"DATE_FAIL",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])
pattern <- "MAX_DATE"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"DATE_MAX",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])
pattern <- "_DATE"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

pattern <- "FAIL_PM2"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"FAILPM2",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])
pattern <- "MAX_FAIL_YR"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"FAILYRMAX",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

BoolCols <- c("HAS_DATE_FAIL","GAGE_ON_BRIDGE","Q_FAIL_AVAIL_EITHER","GAGE_ON_BRIDGE_EITHER","STREAM_NAME_MATCH","CONFIRM_STREAM","CONFIRM_FORK","CONFIRM_STREAM_ALT",
              "CONFIRM_FORK_ALT","NLDAS_MAX_SAME_EVENT","NLDAS_PRE_F_MAX_SAME","NLDAS_PRE_F_MAX_SAME_C","ONE_AREA_WIN_10PCT","ONE_AREA_WIN_20PCT","REGULATION","MK_SIGNIF","NSE_POS",
              "FORK_MATCH_EITHER","DATE_KNOWN_FAIL", "WAS_HURRICANE")
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% BoolCols] <- paste("BOOL",colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% BoolCols],sep="_")

RemoveCols <- c("_LP3","_P3","_GEV", "BEST","SAME")
for (i in 1:length(RemoveCols)){
  df.Fail.NBI.Gage <- df.Fail.NBI.Gage[,!grepl(RemoveCols[i],colnames(df.Fail.NBI.Gage))]
}

CommentCols <- c("Q_FAIL_COMMENT","Q_FAIL_COMMENT_ALT","REGULATION_COMMENT","T_REPORT_NAME","LINKED_HURRICANE","LOCAL_DAM","T_REPORT_CITE","REGULATION_SHORT")
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% CommentCols] <- paste("COMMENT",colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% CommentCols],sep="_")
CommentCols <- paste("COMMENT",CommentCols,sep="_")
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% CommentCols] <- sub("_COMMENT","",colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% CommentCols])

DateCols <- colnames(df.Fail.NBI.Gage)[grepl("DATE",colnames(df.Fail.NBI.Gage))]
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% DateCols] <- paste("DATE",colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% DateCols],sep="_")
DateCols <- paste("DATE",DateCols,sep="_")
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% DateCols] <- sub("_DATE","",colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% DateCols])

pattern <- "MAX_PRE_FAIL"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"MAXPREFAIL",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])
pattern <- "PRE_F_MAX"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"MAXPREFAIL",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

pattern <- "INST_PEAK"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"IP",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])
pattern <- "INST"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"I",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])
pattern <- "PEAK"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"P",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])
pattern <- "DAY"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"D",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])
pattern <- "DAILY"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"D",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

RemoveCols <- c("T_FAILPM2_CONF_DVICG_LARGER", "T_FAILPM2_CONF_USGS_LARGER", "T_PARTIAL_USGS")
df.Fail.NBI.Gage <- df.Fail.NBI.Gage[,!(colnames(df.Fail.NBI.Gage) %in% RemoveCols)]

pattern <- "_INTERP"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

pattern <- "PARTIAL"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"FAIL_PARTDUR",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

pattern <- "_C\\>"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"_C_NLDASB",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

pattern <- "HEC_D\\>"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"HECD_D",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

pattern     <- "HEC"
notPattern1 <- "VIC"
notPattern2 <- "NLDAS"
USGScols    <- colnames(df.Fail.NBI.Gage)[grepl(pattern,colnames(df.Fail.NBI.Gage)) & !grepl(notPattern1,colnames(df.Fail.NBI.Gage))  & !grepl(notPattern2,colnames(df.Fail.NBI.Gage))]
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% USGScols] <- paste(USGScols,"USGS",sep="_")

pattern     <- "PKFQ"
notPattern1 <- "VIC"
notPattern2 <- "NLDAS"
USGScols    <- colnames(df.Fail.NBI.Gage)[grepl(pattern,colnames(df.Fail.NBI.Gage)) & !grepl(notPattern1,colnames(df.Fail.NBI.Gage))  & !grepl(notPattern2,colnames(df.Fail.NBI.Gage))]
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% USGScols] <- paste(USGScols,"USGS",sep="_")

pattern <- "PKFQ_P_"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"PKFQP_",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

pattern <- "PKFQ_D_"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"PKFQD_",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

pattern <- "PKFQ_"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"PKFQD_",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "P_COUNT"] <- "COUNT_P_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "DATE_P_BEGIN"] <- "DATE_P_BEGIN_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "DATE_P_END"] <- "DATE_P_END_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "P_BEGIN"] <- "DATE_P_BEGIN_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "P_END"] <- "DATE_P_END_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "P_COUNT_ALT"] <- "COUNT_P_ALT_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "DATE_P_BEGIN_ALT"] <- "DATE_P_BEGIN_ALT_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "DATE_P_END_ALT"] <- "DATE_P_END_ALT_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "P_BEGIN_ALT"] <- "DATE_P_BEGIN_ALT_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "P_END_ALT"] <- "DATE_P_END_ALT_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "DATE_BOOL_HAS_FAIL"] <- "BOOL_HAS_FAIL_DATE"

USGScols <- c("TIMES_Q_EXCEED", "COMMENT_Q_FAIL", "Q_MAX", "Q_FAIL","DATE_Q_MAX","Q_FAIL_AVAIL","Q_FAIL_AVAIL_ALT","COMMENT_Q_FAIL_ALT",
              "Q_MAX_ALT", "DATE_Q_MAX_ALT","BOOL_Q_FAIL_AVAIL_EITHER", "Q_MAXPREFAIL", "DATE_Q_MAX_PRE_FAIL", "DATE_FAIL_EST", "DATE_FAIL_I_EST",
              "FAIL_I_Q","Q_MAX_P","DATE_Q_MAX_P","Q_FAIL_P","Q_MAXPREFAIL_P", "DATE_Q_MAXPREFAIL_P","Q_FAILYRMAX","DATE_Q_FAILYRMAX","Q_FAIL_IP")
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% USGScols] <- paste(colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% USGScols],"USGS",sep="_")

BoolCols <- c("Q_FAIL_AVAIL_USGS", "GAGE_ON_BRIDGE_ALT","STREAM_NAME_MATCH_ALT","STREAM_NAME_MATCH_EITHER","FORK_MATCH","FORK_MATCH_ALT",
              "RELATION_TO_GAGE", "VIC_KS_ALL_DATA_PASS", "WAS_HURRICANE","Q_FAIL_AVAIL_ALT_USGS")
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% BoolCols] <- paste("BOOL",colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) %in% BoolCols],sep="_")

RemoveCols <- c("T_DES","EXC")
for (i in 1:length(RemoveCols)){
  df.Fail.NBI.Gage <- df.Fail.NBI.Gage[,!grepl(RemoveCols[i],colnames(df.Fail.NBI.Gage))]
}

colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_FAIL_USGS"] <- "Q_FAIL_D_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "DATE_Q_MAX_USGS"] <- "DATE_Q_MAX_D_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "DATE_Q_MAX_USGS"] <- "DATE_Q_MAX_D_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_MAX_ALT_USGS"] <- "Q_MAX_D_ALT_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "DATE_Q_MAX_ALT_USGS"] <- "DATE_Q_MAX_D_ALT_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_MAXPREFAIL_USGS"] <- "Q_MAXPREFAIL_D_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "DATE_Q_MAX_PRE_FAIL_USGS"] <- "DATE_Q_MAXPREFAIL_D_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "DATE_FAIL_EST_D_USGS"] <- "DATE_FAIL_D_EST_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q100_HEC_USGS"] <- "Q100_HECP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q500_HEC_USGS"] <- "Q500_HECP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) ==  "FAIL_I_Q_USGS"] <-  "FAIL_Q_I_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_MAX_P_HEC_USGS"] <- "T_MAX_P_HECP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_MAX_HEC_USGS"] <- "T_MAX_D_HECP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) ==  "T_MAXPREFAIL_HEC_USGS"] <-  "T_MAXPREFAIL_D_HECP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HEC_USGS"] <- "T_FAIL_D_HECP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HEC_P_USGS"] <- "T_FAIL_P_HECP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "DATE_Q_MAX_PRE_FAIL_DVICG"] <- "DATE_Q_MAXPREFAIL_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HEC_95_USGS"] <- "T_FAIL_D_95_HECP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HEC_05_USGS"] <- "T_FAIL_D_05_HECP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PARTDUR_PM2_DVICB"] <- "T_FAILPM2_PARTDUR_DVICB"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PARTDUR_PM2_DVICG"] <- "T_FAILPM2_PARTDUR_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "AREA_VIC_AREA_NHD_BRIDGE_RATIO"] <- "AREA_RATIO_VIC_AREA_NHD_BRIDGE"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_FAILYRMAX_USGS"] <- "Q_FAILYRMAX_D_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "DATE_Q_FAILYRMAX_USGS"] <- "DATE_Q_FAILYRMAX_D_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_MAXPREFAIL_P_HEC_USGS"] <- "T_MAXPREFAIL_P_HECP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "VIC_AREA_GAGE_SQKM"] <- "DRAIN_AREA_DVICG_SQKM"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "VIC_AREA_BRIDGE_SQKM"] <- "DRAIN_AREA_DVICB_SQKM"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "BRIDGE_NHDFLOWPLUS_SQKM"] <- "DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HEC_PM2_DVICG"] <- "T_FAILPM2_HEC_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HEC_IP_USGS"] <- "T_FAIL_HECP_IP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HEC_DVICG_95"] <- "T_FAIL_95_HEC_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HEC_DVICG_05"] <- "T_FAIL_05_HEC_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAILPM2_HEC_DVICG_95"] <-  "T_FAILPM2_95_HEC_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAILPM2_HEC_DVICG_05"] <- "T_FAILPM2_05_HEC_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_MAXPREFAIL_HEC_PM2_DVICG"] <- "T_MAXPREFAILPM2_HEC_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "MK_P"] <- "MK_PVAL"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PARTDUR_USGS"] <- "T_FAIL_D_PARTDUR_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "NSE"] <- "NSE_D_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "BOOL_NSE_POS"] <- "BOOL_NSE_D_DVICG_POS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "BOOL_VIC_KS_ALL_DATA_PASS"] <- "BOOL_KS_ALL_DATA_DVICG_PASS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HECD_D_USGS"] <- "T_FAIL_D_HECD_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_MAX_D_HEC_USGS"] <- "T_MAX_D_HECD_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_MAXPREFAIL_D_HEC_USGS" ] <- "T_MAXPREFAIL_D_HECD_USGS" 
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HEC_P_95_USGS"] <- "T_FAIL_P_95_HECP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HEC_P_05_USGS"] <- "T_FAIL_P_05_HECP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HEC_D_95_USGS"] <- "T_FAIL_D_95_HECD_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HEC_D_05_USGS"] <- "T_FAIL_D_05_HECD_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PARTDUR_USGS_IP"] <- "T_FAIL_IP_PARTDUR_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q100_HECD_D_USGS" ] <- "Q100_D_HECD_USGS" 
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q500_HECD_D_USGS" ] <- "Q500_D_HECD_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "NASH_DVICG"] <- "NASH_M_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "R_SQUARED_DVICG"] <- "R_SQUARED_M_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "MAE_CFS_DVICG"] <- "MAE_M_CFS_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "RMSE_CFS_DVICG"] <- "RMSE_M_CFS_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "DATE_MAX_PRE_FAIL_NLDASB"] <- "DATE_MAXPREFAIL_NLDASB"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "DATE_MAX_PRE_FAIL_NLDASG"] <- "DATE_MAXPREFAIL_NLDASG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PARTDUR_PM2_NLDASB"] <- "T_FAILPM2_PARTDUR_NLDASB"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PARTDUR_PM2_NLDASG"] <- "T_FAILPM2_PARTDUR_NLDASG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PKFQD_USGS"] <- "T_FAIL_D_PKFQD_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q100_PKFQD_USGS"] <- "Q100_D_PKFQD_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q500_PKFQD_USGS"] <- "Q500_D_PKFQD_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PKFQP_USGS"] <- "T_FAIL_P_PKFQP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_MAX_P_PKFQD_USGS"] <- "T_MAX_P_PKFQP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_MAXPREFAIL_P_PKFQD_USGS"] <- "T_MAXPREFAIL_P_PKFQP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q100_PKFQP_USGS"] <- "Q100_P_PKFQP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q500_PKFQP_USGS"] <- "Q500_P_PKFQP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PKFQD_PM2_DVICG"] <- "T_FAILPM2_PKFQD_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PKFQD_PM2_DVICB"] <- "T_FAILPM2_PKFQD_DVICB"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PKFQD_PM2_NLDASG"] <- "T_FAILPM2_PKFQD_NLDASG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PKFQD_PM2_NLDASB"] <- "T_FAILPM2_PKFQD_NLDASB"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PARTDUR_FAILYRMAX_DVICB"] <- "T_FAILYRMAX_PARTDUR_DVICB"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PARTDUR_FAILYRMAX_DVICG"] <- "T_FAILYRMAX_PARTDUR_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PARTDUR_FAILYRMAX_NLDASB"] <- "T_FAILYRMAX_PARTDUR_NLDASB"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_PARTDUR_FAILYRMAX_NLDASG"] <- "T_FAILYRMAX_PARTDUR_NLDASG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "BOOL_DATE_KNOWN_FAIL"] <- "BOOL_KNOWN_FAIL_DATE"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "DS_RECORDED_YR_FAIL"] <- "DAYS_RECORDED_YR_FAIL"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_MAX_USGS"] <- "Q_MAX_D_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HECP_I_USGS"] <- "T_FAIL_I_HECP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "HAS_FAIL"] <- "BOOL_HAS_FAIL_DATE"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_MAXPREFAIL.1"] <- "DATE_MAXPREFAIL_D_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_MAX.1"] <- "DATE_MAX_D_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_MAX_P.1"] <- "DATE_MAX_P_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "FAIL_I_EST"] <- "DATE_FAIL_I_EST_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_MAX_DVICB.1"] <- "DATE_MAX_DVICB"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_MAXPREFAIL_DVICB.1"] <- "DATE_MAXPREFAIL_DVICB"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_FAILYRMAX_DVICB.1"] <- "DATE_FAILYRMAX_DVICB"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_MAX_DVICG.1"] <- "DATE_MAX_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_MAXPREFAIL_DVICG.1"] <- "DATE_MAXPREFAIL_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_MAXPREFAIL_P.1"] <- "DATE_MAXPREFAIL_P_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_FAILYRMAX.1"] <- "DATE_FAILYRMAX_D_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_FAILYRMAX_DVICG.1"] <- "DATE_FAILYRMAX_DVICG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_FAILPM2_NLDASB.1"] <- "DATE_FAILPM2_NLDASB"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_MAX_NLDASB.1"] <- "DATE_MAX_NLDASB"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_FAILYRMAX_NLDASB.1"] <- "DATE_FAILYRMAX_NLDASB"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_MAXPREFAIL_NLDASB.1"] <- "DATE_MAXPREFAIL_NLDASB"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_FAILPM2_NLDASG.1"] <- "DATE_FAILPM2_NLDASG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_FAILPM2_NLDASB.1"] <- "DATE_FAILPM2_NLDASG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_MAX_NLDASG.1"] <- "DATE_MAX_NLDASG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_FAILYRMAX_NLDASG.1"] <- "DATE_FAILYRMAX_NLDASG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_MAXPREFAIL_NLDASG.1"] <- "DATE_MAXPREFAIL_NLDASG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "Q_FAILYRMAX_NLDASG.1"] <- "DATE_FAILYRMAX_NLDASG"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "KNOWN_FAIL"] <- "BOOL_KNOWN_FAIL_DATE"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "FAIL_YR_EST"] <- "YR_FAIL_EST"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "FAIL_Q_I_USGS"] <- "Q_FAIL_I_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HEC_I_USGS"] <- "T_FAIL_I_HECP_USGS"
colnames(df.Fail.NBI.Gage)[colnames(df.Fail.NBI.Gage) == "T_FAIL_HECP_IP_USGS"] <- "T_FAIL_IP_HECP_USGS"

pattern <- "DATE_Q"
colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))] <- sub(pattern,"DATE",colnames(df.Fail.NBI.Gage)[grep(pattern,colnames(df.Fail.NBI.Gage))])

RemoveCols <- c("Q_FAIL_ALT",  "COMMENT_Q_FAIL_ALT_USGS",	"Q_MAX_D_ALT_USGS",	"Q_MAX_ALT.1","Q_MAX_DIST",  "Q_MAX_DIST.1","COMMMENTS_SHORT","DATE_FAILPM2_NLDASB.1")
for (i in 1:length(RemoveCols)){
  df.Fail.NBI.Gage <- df.Fail.NBI.Gage[,colnames(df.Fail.NBI.Gage)!=RemoveCols[i]]
}

savefile <- "df.Fail.NBI.Gage.Active.CleanVars.RData"
save(df.Fail.NBI.Gage, rowsToView, IDsToView, file=file.path(dirsGit$Data,savefile))
rm(savefile)
