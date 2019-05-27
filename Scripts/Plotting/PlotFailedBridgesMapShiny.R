# Map of US with failed bridge locations and encoding of failure cause, link to hurricanes, availability
# of failure date, and drainage area information
# BridgesDataFrame should include only rows to be plotted
# ggmap controls basemap used -- ggmap or plain outline of states
# size must be in {"scaleLog","continuous","area","none"}
# Copyright Madeleine Flint, 2016

PlotFailedBridgesMap <- function(BridgesDataFrame, ggmap = FALSE, size = "area", LEGEND = TRUE, 
                                 outputType = "PRES", legendPos = "RIGHT", SHOW_HURR = TRUE){
  
  if(ggmap==TRUE) require(ggmap)
  require(ggplot2)
  require(rgdal)
  require(grid)
  require(gridExtra)
  require(plyr)
  require(reshape2)
  source("SetupEncoding.R")
  
  # SET UP ENCODINGS AS FACTORS
  if (nrow(BridgesDataFrame)<=2){
    size <- "none"
    # print("going to none")
    SizeBreaks <- rep(sizesP$Failure,5)
  }
  else{
    AreaBreaks   <- exp(seq(log(min(BridgesDataFrame$DRAIN_SQKM)), log(max(BridgesDataFrame$DRAIN_SQKM)), length.out = 5))
    AreaBreaks[1] <- AreaBreaks[1] - 0.1
    AreaBreaks[5] <- AreaBreaks[5] + 1
    LengthBreaks <- sqrt(AreaBreaks)
    SizeBreaks   <- round(log(LengthBreaks),1)
    if (size == "scaleLog"){
      if (nrow(BridgesDataFrame) >= 5){
        BridgesDataFrame$DRAIN_AREA_CATEGORY <- sapply(1:nrow(BridgesDataFrame), 
                                                       function(i) SizeBreaks[1:4][BridgesDataFrame[i,"DRAIN_SQKM"] <= AreaBreaks[2:5] & BridgesDataFrame[i,"DRAIN_SQKM"] >= AreaBreaks[1:4]])
        BridgesDataFrame$DRAIN_AREA_CATEGORY <- factor(BridgesDataFrame$DRAIN_AREA_CATEGORY, 
                                                       labels = c(paste("<",round(AreaBreaks[2])),
                                                                  paste(round(AreaBreaks[2]),"-",round(AreaBreaks[3])),
                                                                  paste(round(AreaBreaks[3]),"-",round(AreaBreaks[4])),
                                                                  paste(">",round(AreaBreaks[4]))),
                                                       ordered = TRUE)
      }
      else{
        size <- "none"
      }
    }
  }
  legPointSize <- max(sizesP$Failure,SizeBreaks[5],na.rm = TRUE)
  
  BridgesDataFrame$lat   <- BridgesDataFrame$LATDD
  BridgesDataFrame$lon  <- BridgesDataFrame$LONGDD
  BridgesDataFrame$group <- factor(2)
  
  
  # SET UP BASEMAP AND PLOT - GGMAP OPTION
  if (ggmap){
    basemap <- get_map(location="United States",zoom=3)#,source="stamen",maptype = "toner")
    map <- ggmap(basemap, extent="normal", legend="bottom")+#, base_layer = ggplot(BridgesDataFrame, aes(x=lon,y=lat,group=group))) + 
      ylim(25,52) + xlim(-125,-60) + xlab("") + ylab("")
  }
  else { # USE GIS DATA OF STATES AS BACKGROUND FOR PLOTTING
    load("df.USstates.RData")
    colnames(df.USstates)[1] <- "lon"
    map <- ggplot(df.USstates, aes(x = lon, y = lat, group = group)) + geom_polygon(color = "gray", fill = NA, size=0.25) + 
      coord_fixed(ratio = 1, xlim = c(-125,-66), ylim = c(24,50)) + xlab("") + ylab("")
  }
  
  
  if (size == "none"){
    map <- map +
      geom_point(data = BridgesDataFrame,
                 aes(color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE), 
                 size=sizesP$Failure, 
                 shape=shapesP$Failure)
    if (SHOW_HURR){
      map <- map +
        geom_point(data = subset(BridgesDataFrame, HURRICANE=="YES"),size=sizesP$Failure,shape = "H",color="white") 
    }
    map <- map +
      scale_fill_manual(values = fillsP$Fail,       name = "FAILURE CAUSE") + 
      scale_color_manual(values = fillsP$Fail,       name = "FAILURE CAUSE") + 
      scale_alpha_manual(values = alphasP$FailDate, name = "FAILURE DATE")
    if (LEGEND == TRUE){
      map <- map +
        guides(color = guide_legend(order = 2,
                                    override.aes = list(size = legPointSize)),
               fill = FALSE,
               alpha = guide_legend(order = 3,
                                    override.aes = list(size  = legPointSize)),
               size = FALSE
        )
    }
    else{
      map <- map +
        guides(fill  = FALSE,
               color = FALSE,
               alpha = FALSE,
               size  = FALSE
        ) 
    }
  }
  else{
    if (size == "scaleLog"){
      map <-map + 
        geom_point(data = BridgesDataFrame,
                   aes(color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE, size = DRAIN_AREA_CATEGORY), 
                   shape=shapesP$Failure) 
      if (SHOW_HURR){
        map <- map +
          geom_point(data = subset(BridgesDataFrame, HURRICANE=="YES"), aes(size = DRAIN_AREA_CATEGORY),shape = "H",color="white")
      }
      map <- map +
        scale_fill_manual(values = fillsP$Fail,       name = "FAILURE CAUSE") + 
        scale_color_manual(values = fillsP$Fail,       name = "FAILURE CAUSE") + 
        scale_alpha_manual(values = alphasP$FailDate, name = "FAILURE DATE") +
        scale_size_manual(values = SizeBreaks[2:5],   name = "AREA (KM2)")
      if (LEGEND==TRUE){
        map <- map +
          guides(color = guide_legend(order = 2,
                                      override.aes = list(size = legPointSize)),
                 fill = FALSE,
                 alpha = guide_legend(order = 3,
                                      override.aes = list(size  = legPointSize)),
                 size  = guide_legend(order = 4,
                                      override.aes = list(shape = 1,
                                                          color = "black",
                                                          fill = "gray",
                                                          alpha = 1))
          ) 
      }
      else{
        map <- map +
          guides(fill  = FALSE,
                 color = FALSE,
                 alpha = FALSE,
                 size  = FALSE
          ) 
      }
    }
    else{
      map <- map + 
        geom_point(data = BridgesDataFrame,
                   aes(color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE, size = DRAIN_SQKM), 
                   shape=shapesP$Failure) 
      if (SHOW_HURR){
        map <- map +
          geom_point(data = subset(BridgesDataFrame, HURRICANE=="YES"), aes(size = DRAIN_SQKM),shape = "H",color="white") 
      }
      map <- map +
        # coord_fixed(ratio = 1, xlim = c(-125,-66), ylim = c(24,50)) +
        scale_fill_manual(values = fillsP$Fail,       name = "FAILURE CAUSE") + 
        scale_color_manual(values = fillsP$Fail,       name = "FAILURE CAUSE") + 
        scale_alpha_manual(values = alphasP$FailDate, name = "FAILURE DATE")
      if (size == "area"){
        map <- map +
          scale_size_area(max_size = 8, breaks = c(80, 800, 8000),  name = bquote("AREA (KM2)"))
      }
      if (size == "continuous"){
        map <- map +
          scale_size_continuous(range = c(2,8), name = bquote("AREA (KM2)"))
      }
      if (LEGEND==TRUE){
        map <- map +
          guides(color = guide_legend(order = 2,
                                      override.aes = list(size = legPointSize)),
                 fill = FALSE,
                 alpha = guide_legend(order = 3,
                                      override.aes = list(size  = legPointSize)),
                 size  = guide_legend(order = 4,
                                      override.aes = list(shape = 1,
                                                          color = "black",
                                                          fill = "gray",
                                                          alpha = 1))
          ) 
      }
      else{
        map <- map +
          guides(fill  = FALSE,
                 color = FALSE,
                 alpha = FALSE,
                 size  = FALSE
          ) 
      }
    }
    
    # legend
    
  }
  
  # SET THEME
  map <- map +
    theme(panel.background = element_rect(fill = "white"),
          legend.key = element_rect(fill = "white"),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x  = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y  = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.text  = element_text(color = "black", size = textP$sub[outputType]),
          legend.title = element_text(color = "black", size = textP$sub[outputType]),
          legend.key.height=unit(0.7,"line"),
          plot.margin     = unit(c(0,0,0,0), "lines")
    )
  if (legendPos == "BOTTOM"){
    map <- map + 
      theme(legend.position = "bottom")
  }
  
  return(map)
}