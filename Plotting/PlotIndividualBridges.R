# Makes supplemental plots for "Historical Analysis of Hydraulic Bridge Collapses in the Continental United States"
# Individual plots of bridge and gauge locations using google maps background image
# Copyright Madeleine Flint, 2016

library(ggmap)
library(grid)
library(gridExtra)
library(plyr)
library(reshape2)
if(embedFonts & saveFormat == "pdf"){
  require(extrafont)
  Sys.setenv(R_GSCMD=gs_path)
}

source(file.path(dirsGit$ScriptsPlot,"createScale.R"))
source(file.path(dirsGit$Scripts,"gcd_slc.R"))
source(file.path(dirsGit$ScriptsPlot,"SetupEncoding.R"))
source(file.path(dirsGit$ScriptsPlot,"MakeBridgeLabel.R"))

load(file.path(dirsGit$Data,"StateCountyCityData.RData"))
       
# SETUP COLORS, SHAPES, ETC.
shapes <- c(BRIDGE_KNOWN=16,BRIDGE_UNKNOWN=1,GAGE=22)
fills  <- c(colorsP$Fail,GAGE="white")
ptspermm <- 2.83464567 
maptype <- "satellite"

# MAKE MAPS FOR EACH LOCATION
nSites       <- length(rowsToPlot)

print("You must press [enter] when prompted to continue generating plots.")
print("This is to slow down the process as the Google API tends to deny")
print("access if the requests are viewed as repeated.")
for (i in 2:nSites){
  data <- df.Fail.NBI.Gage[rowsToPlot[i],c("ID","LATDD","LONGDD","LAT_GAGE","LNG_GAGE","BOOL_HAS_FAIL_DATE","FAIL_CAUS_CODE")]
  dist <- gcd_slc(df.Fail.NBI.Gage[rowsToPlot[i],"LONGR"],df.Fail.NBI.Gage[rowsToPlot[i],"LATR"],df.Fail.NBI.Gage[rowsToPlot[i],"LONGR_GAGE"],df.Fail.NBI.Gage[rowsToPlot[i],"LATR_GAGE"])
  if (dist <= 1)             zoom = 14
  if (dist > 1 & dist < 5)   zoom = 13
  if (dist >=5 & dist < 8)   zoom = 12
  if (dist >=8 & dist < 12)  zoom = 11
  if (dist >=12 & dist < 20) zoom = 10
  if (dist >=20 & dist < 60) zoom = 9
  if (dist >=60)             zoom = 8
  
  data.Melt <- melt(data[,c("ID","LATDD","LONGDD","LAT_GAGE","LNG_GAGE")],id.vars="ID")
  data.Melt[data.Melt$variable=="LATDD" | data.Melt$variable=="LONGDD","TYPE"] <- ifelse(data$BOOL_HAS_FAIL_DATE=="KNOWN","BRIDGE_KNOWN","BRIDGE_UNKNOWN")
  data.Melt[is.na(data.Melt$TYPE),"TYPE"] <- "GAGE"
  data.Melt$LAT  <- NA
  data.Melt$LONG <- NA
  data.Melt[data.Melt$variable=="LATDD","LAT"]     <- data.Melt[data.Melt$variable=="LATDD","value"]
  data.Melt[data.Melt$variable=="LATDD","LONG"]    <- data.Melt[data.Melt$variable=="LONGDD","value"]
  data.Melt[data.Melt$variable=="LAT_GAGE","LAT"]  <- data.Melt[data.Melt$variable=="LAT_GAGE","value"]
  data.Melt[data.Melt$variable=="LAT_GAGE","LONG"] <- data.Melt[data.Melt$variable=="LNG_GAGE","value"]
  data.Melt <- data.Melt[!is.na(data.Melt$LAT),]
  
  data.Melt[data.Melt$variable=="LATDD","FAIL_CAUS_CODE"]   <- data[,"FAIL_CAUS_CODE"]
  data.Melt$TYPE <- factor(data.Melt$TYPE)
  
  data.Melt      <- data.Melt[,c(1,4:7)]
  data.Melt$FAIL_CAUS_CODE <- as.character(data.Melt$FAIL_CAUS_CODE)
  data.Melt[is.na(data.Melt$FAIL_CAUS_CODE),"FAIL_CAUS_CODE"] <- "GAGE"
  data.Melt$FAIL_CAUS_CODE <- factor(x=data.Melt$FAIL_CAUS_CODE, c(labelsP$Fail,"GAGE"))
  
  plotTitle <- MakeBridgeLabel(df.Fail.NBI.Gage[rowsToPlot[i],], MAIN_FIELDS = c("State","Build","Fail"), SUPER_FIELDS = NA, PLOT_MATH = FALSE)
  
  # map centroid
  lon <- 0.5*(data[,"LONGDD"]+data[,"LNG_GAGE"])
  lat <- 0.5*(data[,"LATDD"]+data[,"LAT_GAGE"])
  
  # get map and scale
  if(BridgeOnly==TRUE){
    site <- get_map(location = c(lon=data$LONGDD,lat=data$LATDD),zoom=15, maptype = maptype)
  }
  if(BridgeOnly==FALSE){ 
    site <- get_map(location = c(lon=lon,lat=lat),zoom=zoom, maptype = maptype)
  }
  bb   <- attr(site,"bb")
  sbar <- createScale(bb, ROUND = TRUE)
  
  # plot
  map <- ggmap(site,darken=c(0.2,"black"),legend="bottom",base_layer = ggplot(data=data.Melt, aes(x=LONG,y=LAT)))
  map <- map + xlab("") + ylab("") + geom_point(aes(fill=FAIL_CAUS_CODE,size=8,shape=TYPE,color=FAIL_CAUS_CODE),stroke = 1.2)  
  map <- map + scale_fill_manual(values=fills[data.Melt$FAIL_CAUS_CODE]) + scale_shape_manual(values=shapes)
  map <- map + scale_color_manual(values=fills[data.Melt$FAIL_CAUS_CODE]) + ggtitle(plotTitle)
  map <- map + guides(size=FALSE,
                      shape=FALSE,
                      color=FALSE,
                      fill=FALSE)
  map <- map + geom_segment(data = sbar,
                            aes(x = lon.start,
                                xend = lon.end,
                                y = lat.start,
                                yend = lat.end),
                            color = "white")
  map <- map +  geom_text(data = sbar,
                          aes(x = (lon.start + lon.end)/2,
                              y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),
                              label = paste(format(distance, 
                                                   digits = 4,
                                                   nsmall = 2),
                                            'km')),
                          hjust = 0.5,
                          vjust = 0,
                          size = 10/ptspermm,
                          color = "white")
  map + theme_bw()
  savename <- switch(saveFormat,
                      eps = file.path(dirsGit$Plots,"Supplemental",paste("ID",df.Fail.NBI.Gage[rowsToPlot[i],"ID"],".eps",sep="")),
                      pdf = file.path(dirsGit$Plots,"Supplemental",paste("ID",df.Fail.NBI.Gage[rowsToPlot[i],"ID"],".pdf",sep=""))
                     )
  ggsave(savename, plot=map, height = 5, width = 5)
  if(embedFonts & saveFormat == "pdf"){
    embed_fonts(savename) 
  }
  cat ("Press [enter] to continue")
  line <- readline()
}      

