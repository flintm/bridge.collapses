# code to make clean tables for the possible matched bridges to facilitate comparison
library(stringr)
# load general data

load("~/hydraulic.failures/Data/df.Fail.NBI.Gage.Active.RData")
load("~/hydraulic.failures/FailandNBIforMatching.RData")
colnames(df.Fail)[grepl("ITEM5B",colnames(df.Fail))] <- "ITEM5B_FAIL"

# load specific data for bridge ID of interest
IDforTable <- "1164"
PossRowsSource <- "stream"
folderOpts <- c(stream = "matchbridges_stream_20150130", roadstream = "matchbridges__serial_road_stream_20150126/Output")
load(file.path("/Users/mflint/Documents/Research/Climate_Scour_Local/Analysis_Results",folderOpts[PossRowsSource],paste0("ID",IDforTable,"-",PossRowsSource,".RData")))
MatchRows <- PossibleMatchRows[!grepl("[[:alpha:]]",PossibleMatchRows)]

# merge dataframes
df.Fail.ID <- df.Fail[df.Fail$ID==IDforTable,]
df.Fail.ID[,c("YR_FAIL","FAIL_TYPE","YR_BLT","MAT","MATER","ITEM_43A_FAIL","TYPE","ITEM_43B_FAIL")] <-
  df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==IDforTable,c("YR_FAIL","FAIL_TYPE","YR_BLT","MAT","MATER","ITEM_43A_FAIL","TYPE","ITEM_43B_FAIL")]
df.NBI.ID  <- df.NBI[MatchRows,]
df.NBI.ID$ID <- IDforTable
df.Merged <- merge(df.Fail.ID,df.NBI, by = "ID", all = TRUE)
df.Merged$YR_BLT <- format.Date(df.Merged$YR_BLT,"%Y")
df.Merged$YR_FAIL <- format.Date(df.Merged$YR_FAIL,"%Y")
df.Merged$ITEM27 <-  format.Date(df.Merged$ITEM27,"%Y")
df.Merged$SPACE <- character(nrow(df.Merged))

# setup output format
colsList  <- list(colsRow01 = c("BIN","BIN","SPACE","SPACE","ITEM8", "ITEM5D","SPACE"),
                  colsRow02 = c("COUNTY_NAME","COUNTY_NAME_2","FIPS", "FIPS_FROM_CITY", "ITEM3","SPACE"),
                  colsRow03 = c("ROUTE_NO","RTE_DIRECTION","SPACE","SPACE","ITEM5D","ITEM5E"),
                  colsRow04 = c("ROAD_NAME","ROAD_TYPE","ITEM5B_FAIL","SPACE","ITEM7","ITEM5B"),
                  colsRow05 = c("STREAM_NAME","STREAM_TYPE","SPACE","SPACE","ITEM6A"),"SPACE",
                  colsRow06 = c("YR_FAIL","FAIL_TYPE","SPACE","SPACE","ITEM27","SPACE"),
                  colsRow07 = c("YR_BLT","SPACE","SPACE","SPACE","ITEM27","SPACE"),
                  colsRow08 = c("RD_OTHER","ROAD_NEAR","SPACE","SPACE","ITEM9","SPACE"),
                  colsRow09 = c("STREAM_TRIB","STREAM_OTHER","SPACE","SPACE","ITEM6A","SPACE"),
                  colsRow10 = c("MAT","MATER","ITEM_43A_FAIL","SPACE","ITEM43A","SPACE"),
                  colsRow11 = c("TYPE","ITEM_43B_FAIL","SPACE","SPACE","ITEM43B","SPACE"))
colCharWidths <- c(20, 20, 20, 20, 20, 20)
# df.Merged.Fw  <- sapply(1:6, function(j) str_pad(df.Merged.Fw)


nRep  <- 1+nrow(df.Merged)
Lines <- character(nRep*length(colsList))
for (i in 1:length(colsList)){
  headerLine <- sapply(1:6, function(j) str_trunc(str_pad(colsList[[i]][j],colCharWidths[j],"right"),colCharWidths[j], side = "right", ellipsis = ""))
  headerLine <- paste0(headerLine, collapse = "")
  # Lines[nRep*i-nRep+1] <- paste(,sep = "\t",collapse = "\t")
  entryLines <- sapply(1:6, function(j) str_trunc(str_pad(colsList[[i]][j],colCharWidths[j],"right"),colCharWidths[j], side = "right", ellipsis = ""))
  Lines[(nRep*i-(nRep-1)+1):(nRep*i)] <- sapply(1:(nRep-1), function(j) paste0(df.Merged[j,colsList[[i]]],collapse="")) # **** unfinished
}

# write file
filename <- file.path(getwd(),"Analysis","MatchTables",paste0("ID",IDforTable,"-",PossRowsSource,"-Table.csv"))
fileConn<-file(filename)
writeLines(Lines,fileConn)
close(fileConn)




