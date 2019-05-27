# Compare Confidence intervals for failure return period distributions
# note that HEC confidence intervals are stored as exceedences
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
rowsToAnalyzeVIC <- rownames(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView & df.Fail.NBI.Gage$YR_FAIL_EST >= "1980-01-01",])
rowsWithPeak <- rownames(df.Fail.NBI.Gage[!is.na(df.Fail.NBI.Gage$Q_FAIL_IP_USGS),])

# USGS HEC DAY vs HEC PEAK
sum((df.Fail.NBI.Gage[rowsWithPeak,"T_FAIL_D_05_HECD_USGS"] <= df.Fail.NBI.Gage[rowsWithPeak,"T_FAIL_P_95_HECP_USGS"]) & 
      (df.Fail.NBI.Gage[rowsWithPeak,"T_FAIL_D_95_HECD_USGS"] >= df.Fail.NBI.Gage[rowsWithPeak,"T_FAIL_P_05_HECP_USGS"])) # 0

# USGS HEC DAY vs PARTIAL DURATION
sum((df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_05_HECD_USGS"] <= df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_PARTDUR_USGS"]) | 
      (df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_95_HECD_USGS"] >= df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_PARTDUR_USGS"])) # 21

# USGS HEC IP VS PARTIAL DURATION IP
sum((df.Fail.NBI.Gage[rowsWithPeak,"T_FAIL_P_05_HECP_USGS"] <= df.Fail.NBI.Gage[rowsWithPeak,"T_FAIL_IP_PARTDUR_USGS"]) | 
      (df.Fail.NBI.Gage[rowsWithPeak,"T_FAIL_P_95_HECP_USGS"] >= df.Fail.NBI.Gage[rowsWithPeak,"T_FAIL_IP_PARTDUR_USGS"])) # 20

# want 5% HEC smaller than 5 or 1% GEV (bounded upper) and 95% > (bounded lower) - after further thought, this
# # is not robust. Flip the assumptions and look at whether or not they overlap AT ALL.
# sum((df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_05_HECP_USGS"] <= df.Fail.NBI.Gage[rowsToView,"T_FAIL_LP3_USGS_05"]) & 
#       (df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_95_HECP_USGS"] >= df.Fail.NBI.Gage[rowsToView,"T_FAIL_LP3_USGS_95"])) # 5
# sum(df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_05_HECP_USGS"] <= df.Fail.NBI.Gage[rowsToView,"T_FAIL_LP3_USGS_01"]) & 
#       (df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_95_HECP_USGS"] >= df.Fail.NBI.Gage[rowsToView,"T_FAIL_LP3_USGS_99"])) #10
# sum(df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_05_HECP_USGS"] <= df.Fail.NBI.Gage[rowsToView,"T_FAIL_GEV_USGS_05"]) & 
#       (df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_95_HECP_USGS"] >= df.Fail.NBI.Gage[rowsToView,"T_FAIL_GEV_USGS_95"])) # 5, also
# sum((df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_05_HECP_USGS"] <= df.Fail.NBI.Gage[rowsToView,"T_FAIL_GEV_USGS_01"]) & 
#       (df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_95_HECP_USGS"] >= df.Fail.NBI.Gage[rowsToView,"T_FAIL_GEV_USGS_99"])) #10, also

# from VIC-gauge
# sum((1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_05_HECP_USGS"] <= df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_LP3_DAYMET_VIC_GAGE_05"]) & 
#       (1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_95_HECP_USGS"] >= df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_LP3_DAYMET_VIC_GAGE_95"]),na.rm=TRUE) # 1
# sum((1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_05_HECP_USGS"] <= df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_LP3_DAYMET_VIC_GAGE_01"]) & 
#       (1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_95_HECP_USGS"] >= df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_LP3_DAYMET_VIC_GAGE_99"]),na.rm=TRUE) # 0
# sum((1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_05_HECP_USGS"] <= df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_GEV_DAYMET_VIC_GAGE_05"]) & 
#       (1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_95_HECP_USGS"] >= df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_GEV_DAYMET_VIC_GAGE_95"]),na.rm=TRUE) # 1
# sum((1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_05_HECP_USGS"] <= df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_GEV_DAYMET_VIC_GAGE_01"]) & 
#       (1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_95_HECP_USGS"] >= df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_GEV_DAYMET_VIC_GAGE_99"]),na.rm=TRUE) # 2

# looking for lack of overlap (note that they're encoded backwards)
# sum((1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_05_HECP_USGS"] < df.Fail.NBI.Gage[rowsToView,"T_FAIL_LP3_USGS_95"]) | 
#       (1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_95_HECP_USGS"] > df.Fail.NBI.Gage[rowsToView,"T_FAIL_LP3_USGS_05"])) # 16 don't overlap of 36
# sum((1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_05_HECP_USGS"] < df.Fail.NBI.Gage[rowsToView,"T_FAIL_GEV_USGS_95"]) | 
#       (1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_95_HECP_USGS"] > df.Fail.NBI.Gage[rowsToView,"T_FAIL_GEV_USGS_05"])) # 17

# from VIC-gauge HEC-HEC
sum((df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_D_05_HECP_USGS"] < df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILPM2_95_HEC_DVICG"]) | 
      (df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_D_95_HECP_USGS"] > df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILPM2_05_HEC_DVICG"]),na.rm=TRUE) # 17 don't overlap - 6 from first, 11 from second
sum((df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_D_05_HECP_USGS"] < df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_95_HEC_DVICG"]) | # VIC entire range LARGER 5 times, HEC range LARGER 11 times (of 25)
      (df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_D_95_HECP_USGS"] > df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_05_HEC_DVICG"]),na.rm=TRUE) # 22

df.Fail.NBI.Gage$T_FAIL_PM2_CONF_VIC_GAGE_LARGER <- df.Fail.NBI.Gage$T_FAIL_D_05_HECP_USGS < df.Fail.NBI.Gage$T_FAIL_PM2_HEC_DAYMET_VIC_GAGE_95
df.Fail.NBI.Gage$T_FAIL_PM2_CONF_USGS_LARGER <- df.Fail.NBI.Gage$T_FAIL_D_95_HECP_USGS > df.Fail.NBI.Gage$T_FAIL_PM2_HEC_DAYMET_VIC_GAGE_05

# from VIC-gauge HEC-MLE
# sum((1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_05_HECP_USGS"] < df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_LP3_DAYMET_VIC_GAGE_95"]) | 
#       (1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_95_HECP_USGS"] > df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_LP3_DAYMET_VIC_GAGE_05"]),na.rm=TRUE) # 18
# sum((1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_05_HECP_USGS"] < df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_GEV_DAYMET_VIC_GAGE_95"]) | 
#       (1/df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_95_HECP_USGS"] > df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_GEV_DAYMET_VIC_GAGE_05"]),na.rm=TRUE) # 18 of 25

# VICG MLE-MLE
# sum((df.Fail.NBI.Gage[rowsToView,"T_FAIL_LP3_USGS_05"] < df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_LP3_DAYMET_VIC_GAGE_95"]) | 
#       (df.Fail.NBI.Gage[rowsToView,"T_FAIL_LP3_USGS_95"] > df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_LP3_DAYMET_VIC_GAGE_05"]),na.rm=TRUE) # 18
# sum((df.Fail.NBI.Gage[rowsToView,"T_FAIL_GEV_USGS_05"] < df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_GEV_DAYMET_VIC_GAGE_95"]) | 
#       (df.Fail.NBI.Gage[rowsToView,"T_FAIL_GEV_USGS_95"] > df.Fail.NBI.Gage[rowsToView,"T_FAIL_PM2_GEV_DAYMET_VIC_GAGE_05"]),na.rm=TRUE) # 19 of 25
