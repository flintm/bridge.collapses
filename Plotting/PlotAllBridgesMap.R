# Map of US with locations of all bridges over rivers, differentiated for pre- or post-1991
# design and scour criticality
# BridgesDataFrame should include only rows to be plotted
# ggmap controls basemap used -- ggmap or plain outline of states
# type should be one of {"1991", "ageContinuous", "ageDiscrete", "scourCrit"}
# Copyright Madeleine Flint, 2016

PlotAllBridgesMap <- function(BridgesDataFrame, ggmap = FALSE, type = "1991", SAVE = FALSE, 
                              SIZE = c(3.5,1.9), outputType = "PRINT"){

  if(ggmap) require(ggmap)
  require(rgdal)
  require(grid)
  require(gridExtra)
  require(plyr)
  require(reshape2)
  source(file.path(dirsGit$ScriptsPlot,"SetupEncoding.R"))
  
  # SET UP ENCODINGS AS FACTORS
  BridgesDataFrame$DESIGN_CATEGORY <- factor(BridgesDataFrame$ITEM27>=1991, levels = c("TRUE","FALSE"), labels = c("POST-1991", "PRE-1991"))
  
  BridgesDataFrame$SCOUR_CRITICAL  <- as.character(BridgesDataFrame$ITEM113)
  BridgesDataFrame[BridgesDataFrame$SCOUR_CRITICAL %in% c("0","1","2","3"),"SCOUR_CRITICAL"]                 <- "CRITICAL"
  BridgesDataFrame[BridgesDataFrame$SCOUR_CRITICAL %in% c("4","5","6","7","8","9","T","N"),"SCOUR_CRITICAL"] <- "NOT CRITICAL"
  BridgesDataFrame[BridgesDataFrame$SCOUR_CRITICAL %in% c("U"), "SCOUR_CRITICAL"]                            <- "UNKNOWN FOUNDATION"
  BridgesDataFrame$SCOUR_CRITICAL <- factor(BridgesDataFrame$SCOUR_CRITICAL)
  
  BridgesDataFrame$ITEM27[BridgesDataFrame$ITEM27 <= 1700] <- NA
  ageBreaks   <- c(1751, 1851, 1901, 1951, 1991, 2001, 2011, 2016)
  ageBreaks   <- c(ageBreaks[ageBreaks[1:7] > min(BridgesDataFrame$ITEM27, na.rm = T) & ageBreaks[1:7] < max(BridgesDataFrame$ITEM27, na.rm = T)], ageBreaks[8])
  
  BridgesDataFrame$lat   <- BridgesDataFrame$LATDD
  BridgesDataFrame$long  <- BridgesDataFrame$LONGDD
  BridgesDataFrame$group <- factor(2)
  
  # SET UP BASEMAP AND PLOT - GGMAP OPTION
  if (ggmap){
    basemap <- get_map(location="United States",zoom=3,maptype="road",source="google",color="bw")
    map <- ggmap(basemap, extent="normal", legend="bottom")+#, base_layer = ggplot(BridgesDataFrame, aes(x=lat,y=long))) +
      ggplot(BridgesDataFrame)+
                 ylim(25,52) + xlim(-125,-60)
  }
  else{

    gisDataDir  <- file.path(dirs$OrigDataDir,"GIS data", "states_21basic")
    ds.USstates <- readOGR(dsn=gisDataDir,layer="states")
    df.USstates <- fortify(ds.USstates)
    map <- ggplot(df.USstates, aes(x = long, y = lat, group = group)) + geom_polygon(color = "gray", fill = NA, size = 0.25) + 
      coord_fixed(ratio = 1, xlim = c(-125,-66), ylim = c(24,50))
  }

  if (type == "1991"){
    print("making plot")
    map <- map + 
      geom_point(data = BridgesDataFrame,
                 aes(color = DESIGN_CATEGORY), 
                 shape = shapesP$BridgeDesign[1],
                 size  = 0.5,
                 alpha = alphasP$BridgeDesign[1],
                 stroke = 0)  +
      scale_color_manual(values = colorsP$BridgeDesign,  name = "DESIGN YEAR")
  }
  
  if (type == "ageContinuous"){
    map <- map + 
      geom_point(data = BridgesDataFrame,
                 aes(color = ITEM27), 
                 shape = shapesP$BridgeDesign[1],
                 size  = 0.5,
                 alpha = alphasP$BridgeDesign[1],
                 stroke = 0)  +
      scale_color_gradientn(colours = colorsP$BridgeAge9, values = ageBreaks, rescaler = function(ITEM27,...) ITEM27, oob = identity, name = "YEAR BUILT")
  }
  
  if (type == "ageDiscrete"){ 
    BridgesDataFrame$AGE_CATEGORY <- unlist(sapply(1:nrow(BridgesDataFrame), 
                                                   function(i) ageBreaks[1:7][BridgesDataFrame[i,"ITEM27"] <= ageBreaks[2:8] & BridgesDataFrame[i,"ITEM27"] >= ageBreaks[c(1:7)]][1]))
    BridgesDataFrame$AGE_CATEGORY <- factor(BridgesDataFrame$AGE_CATEGORY, labels = c("< 1851", "1851-1901", "1901-1951", "1951-1991","1991-2001", 
                                                                                      "2001-2011", "> 2011"))
    map <- map + 
          geom_point(data = BridgesDataFrame,
                 aes(color = AGE_CATEGORY), 
                 shape = shapesP$BridgeDesign[1],
                 size  = 0.5,
                 alpha = alphasP$BridgeDesign[1],
                 stroke = 0)  +
      scale_color_manual(values = colorsP$BridgeAge9[c(1:4,7:9)], name = "")
  }
  map <- map + guides(color = guide_legend(nrow = 2,
                                           byrow= TRUE,
                                           override.aes = list(size = 3))) 
  map <- map +
    theme(panel.background = element_rect(fill = "white"),
          legend.key = element_rect(fill = "white"),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.margin       = unit(c(-0.25,-0.25,-0.25,-0.25),"in"),
          axis.text.x  = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y  = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.text  = element_text(color = "black", size = textP$sub[outputType]),
          legend.title = element_text(color = "black", size = textP$sub[outputType]),
          legend.key.height=unit(0.4,"line"),
          legend.key.width=unit(0.5,"line"),
          legend.position = "bottom",
          legend.margin = unit(-0.1,"in"),
          plot.margin     = unit(c(-0.1,-0.1,0,-0.1), "in")
    )
  if (SAVE==TRUE){
    pdf(file=file.path(dirsGit$Plots,paste("BridgesMap",type,"pdf",sep=".")), width=SIZE[1],height=SIZE[2], title = "Fig 1 Map of US Bridges Over Water",
        useDingbats = F)
    print(map)
    dev.off()
    require(extrafont)
    Sys.setenv(R_GSCMD="/usr/local/bin/gs") #path to ghostscript
    embed_fonts(file.path(dirsGit$Plots,paste("BridgesMap",type,"pdf",sep="."))) 
     # ggsave(filename = file.path(dirsGit$Plots,paste("BridgesMap",type,"pdf",sep=".")),width=SIZE[1],height=SIZE[2])
  }
  return(map)
}
