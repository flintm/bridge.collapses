# 2015-08-05 Load VIC routed data (from Daymet-VIC dataset)
load(file.path(dirs$DataDirFrequent,"df.Fail.NBI.Gage.Active.RData"))

# first link ORNL numbering of bridges and gauges to our ID/STAID

BridgeGageList <- read.table(file.path(dirs$OrigDataDir,"ORNL_Area_Calcs","20150610_SitesForRouting.txt"),
                             header = TRUE,
                             fill = TRUE,
                             sep = "\t",
                             colClasses = c("character","numeric","numeric","character","numeric"))

ORNLbridgeGageList <- read.csv(file.path(dirs$OrigDataDir,"ORNL_Area_Calcs","Routing_allsites_area_ORNL.csv"))

# create index data.frames
BridgeIndex <- data.frame(ID=as.integer(BridgeGageList[BridgeGageList$TYPE == "bridge","ID"]))
BridgeIndex$ORNL_ID <- as.character(ORNLbridgeGageList[,"Bridge.Id"])

GageIndex <- data.frame(STAID=as.character(BridgeGageList[BridgeGageList$TYPE == "gauge","ID"]))
GageIndex$ORNL_ID <- as.character(ORNLbridgeGageList[c(1:nrow(GageIndex)),"Gauge.ID"])

rm(ORNLbridgeGageList,BridgeGageList)


# get list of files in the folder and list of sites
routingFilesBridges <- list.files(path = file.path(dirs$OrigDataDir,"VIC_Dayment_Routed"),
                                  pattern = "\\<B")
routingFilesGages <- list.files(path = file.path(dirs$OrigDataDir,"VIC_Dayment_Routed"),
                                  pattern = "\\<G")
RoutedBridges <- unique(substr(routingFilesBridges,1,5))
RoutedGages <- unique(substr(routingFilesGages,1,5))

# loop over files for each unique site, using the name of the bridgeID or STAID (and putting in appropriate list)
# bridges
colnamesRouted  <- list(day      = c ("YEAR","MONTH","DAY","RUNOFF_CFS"),
                        day_mm   = c("YEAR","MONTH","DAY","RUNOFF_MM"),
                        month    = c("YEAR","MONTH","RUNOFF_CFS"),
                        month_mm = c("YEAR","MONTH","RUNOFF_MM"),
                        year     = c("COL1","COL2"),
                        year_mm  = c("COL1","COL2")
                        )

ls.BridgeRouted <- list()
for (i in 1:length(RoutedBridges)){
  files <- list.files(path = file.path(dirs$OrigDataDir,"VIC_Dayment_Routed"),
                      pattern = paste("\\<",RoutedBridges[i],sep=""),include.dirs = FALSE)
  ID    <- as.character(BridgeIndex[BridgeIndex$ORNL_ID==RoutedBridges[i],"ID"])
  for (j in 1:length(files)){
    fileType <- substr(files[j],7,nchar(files[j]))
    ls.BridgeRouted[[ID]][[fileType]] <- read.table(file.path(dirs$OrigDataDir,"VIC_Dayment_Routed",files[j]),
                                                   col.names = colnamesRouted[[fileType]],
                                                   header = FALSE)
    if (fileType %in% c("day","day_mm")){
      ls.BridgeRouted[[ID]][[fileType]]$Date <- as.Date(paste(ls.BridgeRouted[[ID]][[fileType]]$YEAR,ls.BridgeRouted[[ID]][[fileType]]$MONTH,ls.BridgeRouted[[ID]][[fileType]]$DAY,sep="-"))
    }
  }
}
rm(ID,RoutedBridges)

savefile <- paste(gsub("-","",Sys.Date()),"BridgeRoutingVICDaymet.RData",sep="_")
save(ls.BridgeRouted,file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

ls.GageRouted <- list()
for (i in 1:length(RoutedGages)){
  files <- list.files(path = file.path(dirs$OrigDataDir,"VIC_Dayment_Routed"),
                      pattern = paste("\\<",RoutedGages[i],sep=""))
  STAID    <- as.character(GageIndex[GageIndex$ORNL_ID==RoutedGages[i],"STAID"])
  for (j in 1:length(files)){
    fileType <- substr(files[j],7,nchar(files[j]))
    ls.GageRouted[[STAID]][[fileType]] <- read.table(file.path(dirs$OrigDataDir,"VIC_Dayment_Routed",files[j]),
                                                      col.names = colnamesRouted[[fileType]],
                                                      header = FALSE)
    if (fileType %in% c("day","day_mm")){
      ls.GageRouted[[STAID]][[fileType]]$Date <- as.Date(paste(ls.GageRouted[[STAID]][[fileType]]$YEAR,ls.GageRouted[[STAID]][[fileType]]$MONTH,ls.GageRouted[[STAID]][[fileType]]$DAY,sep="-"))
    }
  }
}
rm(STAID,i,j,fileType,colnamesRouted,files,RoutedGages)

savefile <- paste(gsub("-","",Sys.Date()),"GageRoutingVICDaymet.RData",sep="_")
save(ls.GageRouted,file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

rm(GageIndex,BridgeIndex,routingFilesGages,routingFilesBridges)

rm(ls.BridgeRouted,ls.GageRouted)
