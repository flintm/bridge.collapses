# 2015-06-04 finding additional bridges with potential for high-quality stream data
# was looking for additional post-1981 bridges for Moetasim to do routing, and came across a bunch that I could probably be using
# for the historical study

IDsAdditionalToStudy <- c(195,493,809,901,1044,1160,1251,1424,1551,1631,1673,1675,1740,3518,3528,3610,3629)
rowsAdditionalStudy  <- which(df.Fail.NBI.Gage$ID %in% IDsAdditionalToStudy)

# after looking for uniqueness (lack of matching BIN), these IDs require additional scrutiny:
IDsToCheck <- c(493,1160,1251,1675,1740,3518,3528)

# 493 only one bridge on kehrs mill and caulks creek, and caulks creek only crosses kehrs mill once
# 1160 according to USGS in WV, Nov4-5, 1985, remains of Hurricane Juan took out 500 bridges, so even if this is not the perfect match I think it's acceptable
# 1251 visual confirmation in google map that only 1 crossing of rte 649 by little dan river, NBI bridge built in 1951 but has same type/material
# 1675, Hickahala, could be 1 of 3, but one built the year after failure - not the one currently listed, so need to update df - we want the one that ends in "31", row "283656"
# 1740 was located by hand by checking for a branch of Maries river in Osage county - could check to see if can find replacement since failed in 2005
# 3518 is the only double match, built 2 years after failure - but location needs to be corrected
# 3528 there is another bridge closer to lexington, but built in 2011 after 2011 total collapse and BIN matches exactly

# corrected 1675 entry and recomputed DIST_TO_GAGE

savefile <- paste(gsub("-","",Sys.Date()),"BridgesGagesDataFrameWithFlowAnalysis25withInstantaneousAndMaxFromPeakAndNewBridgesToStudy.RData",sep="_")
save(df.Fail.NBI.Gage, rowsAdditionalStudy,file=file.path(DataDir,"Analysis_Results_In_Progress","FindingFailureReturnPeriod-Exploratory",savefile))
rm(savefile)




