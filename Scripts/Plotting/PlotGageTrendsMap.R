# Map of US with gages sites linked to failed bridge locations and encoding of trends in annual peaks
# GagesDataFrame should include only rows to be plotted
# MK contains values of Mann-Kendall test and significance
# ggmap controls basemap used -- ggmap or plain outline of states
# size must be in {"scaleLog","continuous","area","none}
# Copyright Madeleine Flint, 2016

PlotGageTrendsMap <- function(GagesDataFrame, size = "area", LEGEND = TRUE, SAVE = FALSE, 
                              SIZE = c(3.5,2.5), outputType = "PRINT", colorType = "CONT", embedFonts = FALSE,
                              gs_path = "/usr/local/bin/gs"){

  require(ggplot2)
  require(rgdal)
  require(grid)
  require(gridExtra)
  require(plyr)
  require(reshape2)
  source(file.path(dirsGit$ScriptsPlot,"SetupEncoding.R"))
  
  # SET UP ENCODINGS AS FACTORS
  GagesDataFrame$MK_POS <- factor(GagesDataFrame$MK_TAU > 0, levels = c(TRUE,FALSE), labels = labelsP$MK)
  GagesDataFrame$lat    <- GagesDataFrame$LAT_GAGE
  GagesDataFrame$lon    <- GagesDataFrame$LNG_GAGE
  

 # USE GIS DATA OF STATES AS BACKGROUND FOR PLOTTING
  GagesDataFrame$group <- factor(2)
  load(file.path(dirsGit$Data,"df.USstates.RData"))
  colnames(df.USstates)[1] <- "lon"
  map <- ggplot(df.USstates, aes(x = lon, y = lat, group = group)) + geom_polygon(color = "gray", fill = NA, size = 0.1) + 
    coord_fixed(ratio = 1, xlim = c(-125,-66), ylim = c(24,50))
  
  if (size == "none"){
    if (colorType == "CONT"){
      map <- map + 
        geom_point(data = GagesDataFrame,
                   aes(color = MK_TAU, 
                       shape = BOOL_MK_SIGNIF), stroke = 0.5)  +
        coord_fixed(ratio = 1, xlim = c(-125,-66), ylim = c(24,50)) +
        scale_shape_manual(values = c(1,16), name = "MK p < 0.05") +
        scale_color_gradient2(low = colorsP$MK[2], mid = "#9970ab", high = colorsP$MK[1],   
                              guide = "colourbar",  name = expression(bold('MK'~tau)), breaks = c(-0.3, 0, 0.3))
    }
    else {
      if (colorType == "DISCRETE"){
        map <- map + 
          geom_point(data = GagesDataFrame,
                     aes(color = MK_POS, 
                         alpha = BOOL_MK_SIGNIF), shape = 19, stroke = 0)  +
          coord_fixed(ratio = 1, xlim = c(-125,-66), ylim = c(24,50)) +
          scale_alpha_manual(values = alphasP$MK, name = "MK p < 0.05") +
          scale_color_manual(values = c(colorsP$MK),     name = expression(bold('MK'~tau)))
      }
      else{
        warning('colorType must be one of CONT or DISCRETE')
        return()
      }
    }
  }
  
  if (size == "area"){
    if (colorType == "CONT"){
    map <- map + 
      geom_point(data = GagesDataFrame,
                 aes(color = MK_TAU, 
                     shape = BOOL_MK_SIGNIF,
                     size = DRAIN_SQKM), stroke = 0.5)  +
      coord_fixed(ratio = 1, xlim = c(-125,-66), ylim = c(24,50)) +
      scale_shape_manual(values = c(1,16), name = "MK p < 0.05") +
      scale_size_area(max_size = 8, breaks = c(50, 500, 5000),  name = expression(bold(AREA~(KM^2)))) +
      scale_color_gradient2(low = colorsP$MK[2], mid = "#9970ab", high = colorsP$MK[1],   
                            guide = "colourbar",  name = expression(bold('MK'~tau)), breaks = c(-0.3, 0, 0.3))
    }
    else {
      if (colorType == "DISCRETE"){
        map <- map + 
          geom_point(data = GagesDataFrame,
                     aes(color = MK_POS, 
                         alpha = BOOL_MK_SIGNIF, 
                         size = DRAIN_SQKM), shape = 19, stroke = 0)  +
          coord_fixed(ratio = 1, xlim = c(-125,-66), ylim = c(24,50)) +
          scale_alpha_manual(values = alphasP$MK, name = "MK p < 0.05") +
          scale_size_area(max_size = 8, breaks = c(50, 500, 5000),  name = expression(bold(AREA~(KM^2)))) +
          scale_color_manual(values = c(colorsP$MK),     name = expression(bold('MK'~tau)))
      }
      else{
        warning('colorType must be one of CONT or DISCRETE')
        return()
      }
}
     
    if (LEGEND==TRUE){
      map <- map +
        guides(color = guide_legend(order = 1,
                                    override.aes = list(size = 5)),
               shape = guide_legend(order = 2,
                     override.aes = list(size  = 5))
        ) 
      if (size == "area"){
        map <- map +
          guides(size  = guide_legend(order = 3,
                                      override.aes = list(shape = 1,
                                                          color = "black",
                                                          fill = "gray",
                                                          alpha = 1))
          ) 
      }
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
  
  
  # SET THEME
  map <- map +
    theme(panel.background = element_rect(fill = "white"),
          legend.key = element_rect(fill = "white"),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.margin       = unit(c(0,0,0,0),"in"),
          axis.text.x  = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y  = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.text  = element_text(color = "black", size = textP$sub[outputType]),
          legend.title = element_text(color = "black", size = textP$sub[outputType], face = "bold"),
          legend.key.height=unit(0.4,"cm"),
          legend.key.width=unit(0.1,"cm"),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.direction = "vertical",
          plot.margin     = unit(c(0,0,0,0), "in")
    )
  if (SAVE==TRUE){
    ggsave(filename = file.path(dirsGit$Plots,paste("Fig6","pdf",sep=".")),width=SIZE[1],height=SIZE[2])
    if(embedFonts){
      require(extrafont)
      Sys.setenv(R_GSCMD=gs_path) 
      embed_fonts(file.path(dirsGit$Plots,"Fig6.pdf"))
    }
    
  }
  return(map)
}
