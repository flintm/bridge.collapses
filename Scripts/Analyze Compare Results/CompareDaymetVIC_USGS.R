# 2015-08-12 Compare the distributions of routed data for the paired subset failed bridges and gages (i.e., for Historical Failures paper)
# Want to know (generally):
#      (A) Can the extreme value distributions of the USGS gauge station reasonably estimate the return periods of failure events?
#      (B) Can the extreme value distributions of the Daymet-VIC data reasonably estimate the return periods of failure events?
#      (C) Does the Daymet-VIC reanalysis acceptably recreate the flow at the gauged sites?
#      (D) If the answer to (C) is "yes," then are the values/distributions at the bridge and gauge sites sufficiently similar 
#          that using the gauge data will give an accurate estimate of the bridge return period?
# More specifically:
#      (A1) What are the results of goodness-of-fit tests for USGS distributions?
#      (A2) What is the range of T_fail produced by bootstrapping parameters from USGS distributions?
#      (A3) What are the range of T_fail produced by different USGS distributions?
#      (A4) Is the fitted LP3 distribution consistent with the HEC-SSP distribution?
#      (A5) How well-correlated are the various estimates of Q_fail?
#      (B1) What are the results of goodness-of-fit tests for Daymet-VIC distributions?
#      (B2) What is the range of T_fail produced by bootstrapping parameters from Daymet-VIC distributions?
#      (B3) What are the range of T_fail produced by different Daymet-VIC distributions?
#      (C1) How well-correlated/sufficient are Daymet-VIC gauge and USGS? (individual)
#      (C2) How well-correlated/sufficient are Daymet-VIC bridge and USGS? (individual)
#      (C3) Are the gauge distributions produced by Daymet-VIC consistent with the USGS distribution (empirical)? (individual)
#      (C4) Are the bridge distributions produced by Daymet-VIC consistent with the USGS distribution (empirical)? (individual)
#      (C5) Are the gauge distributions produced by Daymet-VIC consistent with the USGS distributions (parametric)? (individual)
#      (C6) Are the bridge distributions produced by Daymet-VIC consistent with the USGS distributions (parametric)? (individual)
#      (C7) Are the return periods produced by Daymet-VIC vs USGS correlated? (population)
#      (C8) Are the return periods produced by partial duration curves from Daymet-VIC and USGS correlated? (population)
#      (D1) How well-correlated are Daymet-VIC bridge/gage? (individual)
#      (D2) Internal to Daymet-VIC, how well do runoff distributions at the gauge (empirical) 
#           represent distributions at the bridge? (individual)
#      (D3) Internal to Daymet-VIC, how well do runoff distributions at the gauge (fitted) 
#           represent distributions at the bridge? (individual)
#      (D4) Are the return periods of failures obtained using Daymet-VIC bridge/gauge correlated? (population)

# (A1, A3) handled in other function ______
# (A2) in QfailDistributionUSGS_Bootstrapping.R
# (A4) in CompareHEC_FittedLP3.R
# (A5, C7, C8, D4) in CorrelationOfReturnPeriodEstimates.R
# (B1, B3) covered in Distribution_VIC_Daymet_Routed.R
# (B2) in QfailDistributionDaymetVIC_Bootstrapping.R
# (C1, C2, C3, C4, D1, D2) in USGS_VIC_Timeseries_Correlation.R
# (C5, C6, D3) is USGS_VIC_Fitted_Distributions_Comparison.R

# SETUP
require(stats)
require(nsRFA)
require(ggplot2)
require(plyr)
require(hydroGOF)

load(file.path(dirs$DataDirAnalysis,"20150813_Gage_Daymet_VIC_Annual_Max_P3_LP3_GEV_Param.RData"))
load(file.path(dirs$DataDirAnalysis,"20150813_Bridge_Daymet_VIC_Annual_Max_P3_LP3_GEV_Param.RData"))
load(file.path(dirs$DataDirFrequent,"20150807_GageRoutingVICDaymet.RData"))
load(file.path(dirs$DataDirFrequent,"20150807_BridgeRoutingVICDaymet.RData"))
load(file.path(dirs$DataDirFrequent,"20150729_Discharge_39Gages.RData"))
rm(ls.Discharge.Inst,ls.Discharge.Dist)


# COMPARE DAYMET-VIC LP3 AND GEV BETWEEN BRIDGE AND LINKED GAGE USING ??? TEST. SEE IF DATA UNDERLYING DISTRIBUTION THE SAME?

# COMPARE DAYMET-VIC LP3 WITH HEC-SSP LP3

# COMPARE ECDFS OF DAYMET-VIC AT BRIDGE AND GAGE USING 2-SAMPLE KS AND AD

# COMPARE ECDFS OF DAYMET-VIC AND USGS USING 2-SAMPLE KS AND AD

# OBTAIN (DAILY) CORRELATION OF DAYMET-VIC AND USGS AT GAUGES, MANN KENDALL?

# FIND RETURN PERIOD OF FAILURE EVENTS FROM DAYMET-VIC AT BRIDGE AND GAGE

# CORRELATION OF RETURN PERIODS BETWEEN GAGE/BRIDGE (ALL BRIDGES) AND WITH HEC-SSP

# FIND EXCEEDENCE PROBABILITY (PARTIAL DURATION) FROM DAYMET-VIC AND CORRELATE WITH USGS PARTIAL DURATION (ALL BRIDGES, ALL GAGES)
