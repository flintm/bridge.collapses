# Figure 2 in "Historical analysis of hydraulic bridge collapses in the continental United States"
# Map of US with failed bridge or gauge locations and encoding of failure cause, link to hurricanes, availability
# of failure date, and drainage area information
# BridgesDataFrame should include only rows to be plotted
# ggmap controls basemap used -- ggmap or plain outline of states
# size must be in {"scaleLog","continuous","area","none"}
# Copyright Madeleine Flint, 2016

PlotFailedBridgesMap <- function(BridgesDataFrame, ggmap = FALSE, size = "area", LEGEND = TRUE, plotSites = "BRIDGE",
                                 outputType = "PRINT", legendPos = "RIGHT", SHOW_HURR = TRUE, FOR_INSET = FALSE, INSET = NA, USE_SHAPE = FALSE,
                                 SAVE = FALSE, SIZE = c(7,5), EPS = FALSE){
  
  if(ggmap==TRUE) require(ggmap)
  require(ggplot2)
  require(rgdal)
  require(grid)
  require(gridExtra)
  require(plyr)
  require(reshape2)
  source(file.path(dirsGit$ScriptsPlot,"SetupEncoding.R"))
  
  if (FOR_INSET)  alphasP$FailDate <- c(KNOWN = 0.7, UNKNOWN = 0.4)
  else  alphasP$FailDate <- c(KNOWN = 0.8, UNKNOWN = 0.4)
  shapes <- c(BRIDGE = 19, GAUGE = 1)[plotSites]
  strokes <- c(BRIDGE = 0, GAUGE = 0.1)[plotSites]
  names(colorsP$Data)[1] <- "GAUGE"
  colors <- colorsP$Data[plotSites[plotSites!="BRIDGE"]]
  if ("BRIDGE" %in% plotSites){
    colors <- c(colorsP$Fail, colors)
    if (length(plotSites) > 1){
      names(colors)[1:4] <- paste(names(colors)[1:4],"FAILURE")
    }
  }
  if(size!="none"){
    BridgesDataFrame$DRAIN_AREA_NLDAS_SQKM <- 140
    BridgesDataFrame$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM_RED <- 0.5*BridgesDataFrame$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM
    BridgesDataFrame[BridgesDataFrame$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM_RED<400,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM_RED"] <- 0.5*BridgesDataFrame[BridgesDataFrame$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM_RED<400,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM_RED"]}
    if (length(plotSites) > 1) BridgesDataFrame$color <- paste(as.character(BridgesDataFrame$FAIL_CAUS_CODE),"FAILURE")
    else BridgesDataFrame$color <-BridgesDataFrame$FAIL_CAUS_CODE
    BridgesDataFrame$alpha <- BridgesDataFrame$BOOL_KNOWN_FAIL_DATE
    BridgesDataFrame$lat   <- BridgesDataFrame$LATDD
    BridgesDataFrame$lon   <- BridgesDataFrame$LONGDD
    BridgesDataFrame$group <- factor(2)
    BridgesDataFrame$variable <- "BRIDGE"
  
    # SET UP ENCODINGS AS FACTORS
    if (nrow(BridgesDataFrame)<=2){
      size <- "none"
      SizeBreaks <- rep(sizesP$Failure,5)
    }
    else{
      maxArea       <- max(c(BridgesDataFrame$DRAIN_SQKM, BridgesDataFrame$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM),na.rm = TRUE)
      minArea       <- min(c(BridgesDataFrame$DRAIN_SQKM, BridgesDataFrame$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM),na.rm = TRUE)
      if(minArea==0) minArea <- 0.1
      AreaBreaks    <- exp(seq(log(minArea), log(max(maxArea)), length.out = 5))
      AreaBreaks    <- signif(AreaBreaks, 1)
      AreaBreaks[1] <- AreaBreaks[1] - 0.1
      AreaBreaks[5] <- AreaBreaks[5] + 1
      LengthBreaks  <- sqrt(AreaBreaks)
      SizeBreaks   <- round(log(LengthBreaks),2)

      if (size == "scaleLog"){
        if (nrow(BridgesDataFrame) >= 5){
          BridgesDataFrame$size <- as.numeric(sapply(1:nrow(BridgesDataFrame), 
                                                    function(i) SizeBreaks[1:4][BridgesDataFrame[i,"DRAIN_SQKM"] <= c(AreaBreaks[2:4], maxArea+1) & BridgesDataFrame[i,"DRAIN_SQKM"] >= AreaBreaks[1:4]]))
          BridgesDataFrame$size <- factor(BridgesDataFrame$size, 
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
  
    if (length(plotSites)==1){
      BridgesDataFrame$lat   <- switch(plotSites,
                                      BRIDGE = BridgesDataFrame$LATDD,
                                      GAUGE   = BridgesDataFrame$LAT_GAGE)
      BridgesDataFrame$lon   <- switch(plotSites,
                                      BRIDGE = BridgesDataFrame$LONGDD,
                                      GAUGE   = BridgesDataFrame$LNG_GAGE)
      BridgesDataFrame$area   <- switch(plotSites,
                                        BRIDGE = BridgesDataFrame$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM,
                                        GAUGE   = BridgesDataFrame$DRAIN_SQKM)

      BridgesDataFrame$variable <- plotSites
      df.melt <- BridgesDataFrame
    
  }
  else{
    colsLat <- c(BRIDGE = "LATDD",
                 GAUGE   = "LAT_GAGE")[plotSites]
    colsLong <- c(BRIDGE = "LONGDD",
                  GAUGE   = "LNG_GAGE")[plotSites]
    colsArea <- c(BRIDGE = "DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM",
                  GAUGE   = "DRAIN_SQKM")[plotSites]
    dfLat <- BridgesDataFrame[,c("ID",colsLat)]
    colnames(dfLat) <- c("ID",plotSites)
    dfLong <- BridgesDataFrame[,c("ID",colsLong)]
    colnames(dfLong) <- c("ID",plotSites)
    dfArea <- BridgesDataFrame[,c("ID",colsArea)]
    colnames(dfArea) <- c("ID",plotSites)
    dfLat.melt  <- melt(dfLat,id.vars = "ID",measure.vars = plotSites, value.name = "lat")
    dfLong.melt <- melt (dfLong,id.vars = "ID",measure.vars = plotSites, value.name = "lon")
    dfArea.melt <- melt(dfArea,id.vars = "ID",measure.vars = plotSites, value.name = "area")
    rm(dfLat,dfLong,dfArea)
    df.melt <- join(dfLat.melt, dfLong.melt,by = c("ID","variable"))
    df.melt <- join(df.melt, dfArea.melt, by = c("ID","variable"))
    rm(dfLat.melt,dfLong.melt,dfArea.melt)
    df.melt$color <- character(nrow(df.melt))
    for (i in plotSites){
      df.melt[df.melt$variable==i,"color"] <- switch(i,
                                                     BRIDGE = as.character(BridgesDataFrame$color),
                                                     GAUGE   = "GAUGE")
    }
    df.melt$color <- factor(df.melt$color, levels = names(colors), ordered = TRUE)
    df.melt$group <- factor(2)
    df.melt <- join(df.melt,BridgesDataFrame[,c("ID","alpha")])
    if (size == "scaleLog"){
      df.melt$size <- sapply(1:nrow(df.melt), 
                             function(i) SizeBreaks[1:4][df.melt[i,"area"] <= AreaBreaks[2:5] & df.melt[i,"area"] >= AreaBreaks[1:4]])
      df.melt[grepl("NA",df.melt$size),"size"] <- NA_character_
      df.melt$size <- unlist(df.melt$size)
      df.melt$size <- factor(df.melt$size, 
                             labels = c(paste("<",round(AreaBreaks[2])),
                                        paste(round(AreaBreaks[2]),"-",round(AreaBreaks[3])),
                                        paste(round(AreaBreaks[3]),"-",round(AreaBreaks[4])),
                                        paste(">",round(AreaBreaks[4]))),
                             ordered = TRUE)
    }
  }
  
  # SET UP BASEMAP AND PLOT - GGMAP OPTION
  if (ggmap){
    # basemap <- get_map(location="United States",zoom=3)#,source="stamen",maptype = "toner")
    basemap <- get_map(location="Virginia",zoom=5)#,source="stamen",maptype = "toner")
    map <- ggmap(basemap, extent="normal", legend="bottom")+
       xlab("") + ylab("") # +
      # ylim(25,52) + xlim(-125,-60)
  }
  else { # USE GIS DATA OF STATES AS BACKGROUND FOR PLOTTING
    load(file.path(dirsGit$Data,"df.USstates.RData"))
    colnames(df.USstates)[1] <- "lon"
    sizeI <- ifelse(FOR_INSET, 0.15,0.15)
    if (FOR_INSET){
      
      if (INSET=="MidAtlantic"){
        xlims <- c(-80.2, -76.4)
        ylims <- c(37.15,40.5)
      }
      if (INSET=="NewEngland"){
        xlims <- c(-78.5,-73.8)
        ylims <- c(40.5,43.2)
      }
      if(!(INSET %in% c("MidAtlantic","NewEngland")))
        warning("Inset must be MidAtlantic or NewEngland")
      return
    }
    else{
      xlims <- c(-125,-66)
      ylims <- c(24,50)
    }
    map <- ggplot(df.USstates, aes(x = lon, y = lat, group = group)) + geom_polygon(color = "gray", fill = NA, size=sizeI) + 
      coord_fixed(ratio = 1, xlim = xlims, ylim = ylims) + xlab("") + ylab("")
    if(!is.na(INSET)){
      if (FOR_INSET == FALSE & INSET == "inset"){
        df.inset1 <- data.frame(xmin = -80.2, xmax = -76.4, ymin = 37.15, ymax = 40.5)
        map <- map + geom_rect(inherit.aes = FALSE, data = df.inset1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "black", fill = NA, size = 0.25)
        df.inset2 <- data.frame(xmin = -78.5, xmax = -73.8, ymin = 40.5, ymax = 43.2)
        map <- map + geom_rect(inherit.aes = FALSE, data = df.inset2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "black", fill = NA, size = 0.25)
      }
      if (FOR_INSET & INSET == "MidAtlantic"){
        df.inset1 <- data.frame(xmin = -80.19, xmax = -76.41, ymin = 37.16, ymax = 40.49)
        map <- map + geom_rect(inherit.aes = FALSE, data = df.inset1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "black", fill = NA, size = 0.3)
      }
      if (FOR_INSET & INSET == "NewEngland"){
        df.inset2 <- data.frame(xmin = -78.49, xmax = -73.81, ymin = 40.51, ymax = 43.19)
        map <- map + geom_rect(inherit.aes = FALSE, data = df.inset2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "black", fill = NA, size = 0.3)
      }
    }
    
  }
  
  # section for no scaling ---------------------------------------
  if (size == "none"){
    if (USE_SHAPE){
      alphaS <- ifelse(FOR_INSET,0.4,0.95)
      strokeS <- ifelse(FOR_INSET,0.8,0.8)
      map <- map + 
        geom_point(data = subset(df.melt,variable=="BRIDGE"),
                   aes(color = color, shape = BOOL_KNOWN_FAIL_DATE),
                   stroke = strokeS, alpha = alphaS, size = sizesP$Failure) +
        scale_shape_manual(values = c(16,1), name = "COLLAPSE DATE") +
        scale_x_continuous(expand = c(0,0))+
        scale_y_continuous(expand = c(0,0))
    }
    else{
      map <- map +
        geom_point(data = subset(df.melt,variable=="BRIDGE"),
                   aes(color = color, alpha = alpha),
                   shape = shapes["BRIDGE"],
                   size=sizesP$Failure,
                   stroke = strokes["BRIDGE"]) +
        geom_point(data = subset(df.melt,variable!="BRIDGE"),
                   aes(color = color, alpha = alpha),
                   shape = 1,
                   size=sizesP$Failure,
                   stroke = 0.25) +
        scale_shape_manual(values = shapes)  + 
        scale_alpha_manual(values = alphasP$FailDate, name = "COLLAPSE DATE")
    }
    if (SHOW_HURR & "BRIDGE" %in% plotSites){
      if (USE_SHAPE){
        map <- map +
          geom_point(data = subset(BridgesDataFrame, HURRICANE=="YES" & BOOL_KNOWN_FAIL_DATE=="UNKNOWN"), 
                     aes(color = color), size = sizesP$Failure,shape = "H",alpha = alphaS) +
          geom_point(data = subset(BridgesDataFrame, HURRICANE=="YES" & BOOL_KNOWN_FAIL_DATE=="KNOWN"), 
                     size = sizesP$Failure,shape = "H",color="white",alpha = 1) 
      }
      else{
        map <- map +
          geom_point(data = subset(BridgesDataFrame, HURRICANE=="YES"),size=sizesP$Failure,shape = "H",color="white",alpha = 0.7) 
      }
    }
    map <- map +
      scale_fill_manual(values = colors,       name = "COLLAPSE CAUSE") + 
      scale_color_manual(values = colors,       name = "COLLAPSE CAUSE")
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
               size  = FALSE,
               shape = FALSE
        ) 
    }
  }
  
  # section for scaling ---------------------------------
  else{
    if (size == "scaleLog"){
      if (USE_SHAPE){
        alphaS <- ifelse(FOR_INSET,0.4,0.95)
        strokeS <- ifelse(FOR_INSET,0.8,0.8)
        map <- map + 
          geom_point(data = subset(df.melt,variable=="BRIDGE"),
                     aes(color = color, shape = BOOL_KNOWN_FAIL_DATE, size = size),
                     stroke = strokeS, alpha = alphaS) +
          scale_shape_manual(values = c(16,1), name = "COLLAPSE DATE") +
          scale_x_continuous(expand = c(0,0))+
          scale_y_continuous(expand = c(0,0))
      }
      else{
        map <- map +
          geom_point(data = subset(df.melt,variable=="BRIDGE"),
                     aes(color = color, alpha = alpha),
                     shape = shapes["BRIDGE"],
                     size=sizesP$Failure,
                     stroke = strokes["BRIDGE"]) +
          geom_point(data = subset(df.melt,variable!="BRIDGE"),
                     aes(color = color, alpha = alpha, size = size),
                     shape = 1,
                     stroke = 0.25) +
          scale_alpha_manual(values = alphasP$FailDate, name = "COLLAPSE DATE")
      }
      if (SHOW_HURR & "BRIDGE" %in% plotSites){
        if (USE_SHAPE){
          map <- map +
            geom_point(data = subset(BridgesDataFrame, HURRICANE=="YES" & BOOL_KNOWN_FAIL_DATE=="UNKNOWN"), 
                       aes(color = color, size = size), shape = "H",alpha = alphaS) +
            geom_point(data = subset(BridgesDataFrame, HURRICANE=="YES" & BOOL_KNOWN_FAIL_DATE=="KNOWN"), 
                       aes(size = size),shape = "H",color="white",alpha = 1) 
        }
        else{
          map <- map +
            geom_point(data = subset(BridgesDataFrame, HURRICANE=="YES"), aes(size = size),shape = "H",color="white",alpha = 0.7) 
        }
      }
      map <- map +
        scale_fill_manual(values = colors,      name = "COLLAPSE CAUSE") + 
        scale_color_manual(values = colors,       name = "COLLAPSE CAUSE") + 
        scale_size_manual(values = SizeBreaks[2:5],   expression(atop(bold(DRAINAGE),bold(AREA~(KM^2)))))
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
                 size  = FALSE,
                 shape = FALSE
          ) 
      }
    }
    else{ # size is either area or continuous
      if (USE_SHAPE){
        alphaS <- ifelse(FOR_INSET,0.4,0.95)
        strokeS <- ifelse(FOR_INSET,0.8,0.8)
        map <- map + 
          geom_point(data = subset(df.melt,variable=="BRIDGE"),
                     aes(color = color, shape = BOOL_KNOWN_FAIL_DATE, size = area),
                     stroke = strokeS, alpha = alphaS) +
          scale_shape_manual(values = c(16,1), name = "COLLAPSE DATE") +
          scale_x_continuous(expand = c(0,0))+
          scale_y_continuous(expand = c(0,0))
      }
      else{
        map <- map + 
          geom_point(data = subset(df.melt,variable=="BRIDGE"),
                     aes(color = color, alpha = alpha, size = area),
                     shape = shapes["BRIDGE"],
                     stroke = 0)  +
          scale_alpha_manual(values = alphasP$FailDate, name = "COLLAPSE DATE")
      }
      map <- map + geom_point(data = subset(df.melt,variable!="BRIDGE"),
                              aes(color = color, alpha = alpha, size = area),
                              shape = 1,
                              stroke = 0.25)
      
      
      if (SHOW_HURR & "BRIDGE" %in% plotSites){
        if (FOR_INSET){
          if (USE_SHAPE){
            map <- map +
              geom_point(data = subset(BridgesDataFrame, HURRICANE=="YES" & BOOL_KNOWN_FAIL_DATE=="UNKNOWN"), 
                         aes(size = DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM_RED,color = color),shape = "H",alpha = alphaS) +
              geom_point(data = subset(BridgesDataFrame, HURRICANE=="YES" & BOOL_KNOWN_FAIL_DATE=="KNOWN"), 
                         aes(size = DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM_RED),shape = "H",color="white",alpha = 1) 
          }
          else{
            map <- map +
              geom_point(data = subset(BridgesDataFrame, HURRICANE=="YES"), 
                         aes(size = DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM_RED),shape = "H",color="white", alpha = 0.7)
          }
        }
        else{ 
          if (USE_SHAPE){
            map <- map +
              geom_point(data = subset(BridgesDataFrame, HURRICANE=="YES" & BOOL_KNOWN_FAIL_DATE=="UNKNOWN"), 
                         aes(size = DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM_RED,color = color),shape = "H",alpha = alphaS) +
              geom_point(data = subset(BridgesDataFrame, HURRICANE=="YES" & BOOL_KNOWN_FAIL_DATE=="KNOWN"), 
                         aes(size = DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM_RED),shape = "H",color="white") 
          }
          else {
            map <- map +
              geom_point(data = subset(BridgesDataFrame, HURRICANE=="YES"), 
                         aes(size = DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM_RED),shape = "H",color="white") 
          }
        }
      }
      map <- map +
        scale_fill_manual(values = colors,       name = "COLLAPSE CAUSE") + 
        scale_color_manual(values = colors,       name = "COLLAPSE CAUSE") 
      if (size == "area"){
        maxSize <- ifelse(FOR_INSET,
                          ifelse(INSET=="NewEngland",
                                 round(8*4.65),  # assumes viewport replotted at 30% of total figure
                                 round(8*4.138)  # assumes viewpoint at 25%
                          ),
                          8)
        map <- map +
          scale_size_area(max_size = maxSize, breaks = c(50, 500, 5000), name =  expression(atop(bold(DRAINAGE),bold(AREA~(KM^2)))))
      }
      if (size == "continuous"){
        map <- map +
          scale_size_continuous(range = c(2,8), name = expression(atop(bold(DRAINAGE),bold(AREA~(KM^2)))))
      }
      
      if (LEGEND==TRUE){
        map <- map +
          guides(color = guide_legend(order = 1,
                                      override.aes = list(shape = 16,
                                                          size = legPointSize)),
                 fill = FALSE,
                 size  = guide_legend(order = 4,
                                      override.aes = list(shape = 1,
                                                          color = "black",
                                                          alpha = 1)),
                 shape = guide_legend(order = 2,
                                      override.aes = list(size = 5))
          ) 
      }
      else{
        map <- map +
          guides(fill  = FALSE,
                 color = FALSE,
                 alpha = FALSE,
                 size  = FALSE,
                 shape = FALSE
          ) 
      }
    }
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
          legend.title = element_text(color = "black", size = textP$sub[outputType], face = "bold"),
          legend.key.height=unit(0.7,"line"),
          plot.margin     = unit(c(0,0,0,0), "in"),
          panel.margin = unit(c(0,0,0,0), "lines")
    )
  if (legendPos == "BOTTOM"){
    map <- map + 
      theme(legend.position = "bottom")
  }
  if (SAVE==TRUE){
    filename <- file.path(dirsGit$Plots,paste("FailedBridgesMap","pdf",sep="."))
    if (FOR_INSET) filename <- sub("FailedBridgesMap","FailedBridgesMapForInset",filename)
    if (!EPS) ggsave(filename = filename,width=SIZE[1],height=SIZE[2])
    else ggsave(filename = sub("pdf","eps",filename),width=SIZE[1],height=SIZE[2])
  }
  return(map)
}