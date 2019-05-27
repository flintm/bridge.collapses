cols <- c("ID", "STATE_CODE", "YR_FAIL_EST", "DATE_FAIL_EST_USGS", "HURRICANE", "COMMENT_REGULATION_SHORT", "DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM",
          "LATDD","LONGDD","STAID","DRAIN_SQKM","LAT_GAGE","LNG_GAGE","DIST_TO_GAGE","STAID_ALT","DRAIN_SQKM_ALT","LAT_GAGE_ALT","LNG_GAGE_ALT","DIST_TO_GAGE_ALT",
          "BOOL_ONE_AREA_WIN_20PCT")
# df.Fail.NBI.Gage$DATE_FAIL_EST_USGS <- df.Fail.NBI.Gage[,"DATE_FAILYRMAX_D_USGS"]
# df.Fail.NBI.Gage[!is.na(df.Fail.NBI.Gage$DATE_FAIL),"DATE_FAIL_EST_USGS"] <- df.Fail.NBI.Gage[!is.na(df.Fail.NBI.Gage$DATE_FAIL),"DATE_FAIL"]

View(df.Fail.NBI.Gage[,cols])
View(df.Fail.NBI.Gage[rowsToView,cols])

write.csv(df.Fail.NBI.Gage[,cols],"BridgeGaugeDataAll.csv", row.names = F)
write.csv(df.Fail.NBI.Gage[rowsToView,cols],"BridgeGaugeData35Gauged.csv", row.names = F)

bestID <- c("1482","3528", "3535")

write.csv(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% bestID,cols],"BridgeGaugeData3Best.csv", row.names = F)

df.Fail.NBI.Gage$DATE_FAIL_I_EST_USGS <- as.Date(df.Fail.NBI.Gage$DATE_FAIL_I_EST_USGS,"1970-01-01")
View(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% bestID,c("STAID","DATE_FAIL_EST_USGS","Q_FAIL_D_USGS","DATE_FAIL_I_EST_USGS","Q_FAIL_I_USGS")])


write.table(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% bestID,c("STAID","DATE_FAIL_EST_USGS","Q_FAIL_D_USGS","DATE_FAIL_I_EST_USGS","Q_FAIL_I_USGS")], row.names = F)
write.table(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% bestID,c("STAID","DATE_FAIL_EST_USGS","Q_FAIL_D_USGS")], row.names = F)
