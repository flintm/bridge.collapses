# Plots showing magnitude of failure and max flows
# Copyright Madeleine Flint, 2016
PlotFailMaxFlows <- function(BridgesDataFrame, SAVE = FALSE, plotTypes = "pLogD", Q_units = "m3s",
                                ONLY = NA, LEGEND = TRUE, LABELS = TRUE, outputType = "PRINT",
                                MAIN_FIELDS = c("Build","Fail","State"),
                                SUPER_FIELDS = c("Hurr","Area"), SEGMENTS = TRUE,
                                SIZE = c(7,11), embedFonts = FALSE, gs_path = "/usr/local/bin/gs"){
  require(ggplot2)
  require(grid)
  require(gridExtra)
  require(plyr)
  require(reshape2)
  source(file.path(dirsGit$ScriptsPlot,"SetupEncoding.R"))
  source(file.path(dirsGit$ScriptsPlot,"MakeBridgeLabel.R"))
  source(file.path(dirsGit$ScriptsPlot,"MakeGridPlot.R"))
  
  # LABELS
  BridgesDataFrame$LABEL <- MakeBridgeLabel(BridgesDataFrame, MAIN_FIELDS = MAIN_FIELDS, SUPER_FIELDS = SUPER_FIELDS, PLOT_MATH = TRUE, PAD_LEFT = TRUE)
  BridgesDataFrame <- BridgesDataFrame[order(BridgesDataFrame$FAIL_CAUS_CODE,BridgesDataFrame$YR_BLT_EST,BridgesDataFrame$YR_FAIL, decreasing = TRUE, na.last = FALSE),]
  limits <- BridgesDataFrame[,"LABEL"]
  limits <- c(limits[1:5],"a",limits[6],"b",limits[7:22],"c",limits[23:35])
  colorsLab <- c(rep("black",5),"white","black","white",rep("black",16),"white",rep("black",13))


  p  <- list()
  nP <- length(plotTypes)
  if (length(LEGEND)!=nP) LEGEND <- rep(LEGEND,nP)
  if (length(LABELS)!=nP) LABELS <- rep(LABELS,nP)
  
  for (i in 1:nP){
    pT     <- plotTypes[i]
    if (Q_units == "cfs"){
      Breaks <- switch(pT,
                       pLogD  = c(600,1000,5000,10000,50000,100000, 122000),
                       pLogIP = c(50,100,500,1000,5000,10000,50000,100000, 500000)
      )
    }
    else{
      if (Q_units == "m3s"){
        Breaks <- switch(pT,
                         pLogD  = c(1.8, 5, 10, 50, 100, 500, 1000, 3300),
                         pLogIP = c(40,50,100,500,1000,5000,7000)
        )
      }
      else{
        warning('Only cfs and m3s are supported Q_units')
        return()
      }
    }
   
    cols <- switch(pT,
                   pLogD  = c("LABEL","Q_FAIL_D_USGS","Q_MAXPREFAIL_D_USGS","Q_MAX_D_USGS","Q100_D_HECD_USGS","Q500_D_HECD_USGS"),
                   pLogIP = c("LABEL","Q_FAIL_IP_USGS","Q_MAXPREFAIL_P_USGS","Q_MAX_P_USGS","Q100_HEC_D","Q500_HECP_USGS")
    )
    title <- switch(pT,
                    pLogD  = "USGS DAILY MEAN FLOW",
                    pLogIP = "USGS INST. FLOW"
                    )
    
    ylab <- switch(Q_units,
                    cfs = "Q [1000 cu ft/s]",
                    m3s = expression(paste("Q [",m^{3},"/s]"))
    )
    
#     BreaksLabels <- format(Breaks,scientific = TRUE)
#     BreaksLabels <- sub(".0","",BreaksLabels)
    if(Q_units == "cfs") BreaksLabels <- as.character(Breaks/1000) 
    else BreaksLabels <- as.character(Breaks)
# print(Breaks)
    df.subset <- BridgesDataFrame[,cols]
    colnames(df.subset)[2:6] <- c("FAIL", "PREFMAX", "MAX", "Q100", "Q500")
    
    if (Q_units == "m3s"){
      cfs2m3s <- 0.0283168
      Q_cols <- c("FAIL", "PREFMAX", "MAX", "Q100", "Q500")
      df.subset[,Q_cols] <- sapply(Q_cols, function(j) df.subset[,j]*cfs2m3s)
    }
    
    if (!any(is.na(ONLY)) & !any(is.null(ONLY))){
      colsOnly <- "LABEL"
      for(only in ONLY){
        colsOnly <- c(colsOnly, colnames(df.subset)[grepl(only,colnames(df.subset))])
      }
      df.subset <- BridgesDataFrame[,colsOnly]
    }
    
    TYPES     <- factor(colnames(df.subset)[2:ncol(df.subset)], levels = c("FAIL", "PREFMAX", "MAX", "Q100", "Q500"), labels = labelsP$Qtype[1:5])
    shapes <- shapesP$Qtype[TYPES]
    sizes  <- sizesP$Qtype[TYPES]
    
    df.melt   <- melt(df.subset, 
                      id.names = "LABEL", value.name = "Q", variable.name = "GROUP")
    df.melt       <- join(df.melt, BridgesDataFrame[,c("LABEL", "FAIL_CAUS_CODE", "HURRICANE", "BOOL_KNOWN_FAIL_DATE")], by = "LABEL")
    df.melt$TYPE  <- factor(df.melt$GROUP, levels = c("FAIL", "PREFMAX", "MAX","Q100","Q500"), labels = labelsP$Qtype[1:5])
    df.line       <- df.subset
    df.melt2      <- join(df.melt, df.line, by = "LABEL", match = "first")
    df.melt2$FAIL[df.melt2$FAIL < Breaks[1]] <- Breaks[1] + 1 
    df.melt2$Q100[df.melt2$Q100 > Breaks[length(Breaks)]] <- Breaks[length(Breaks)] - 1
    df.melt2$Q500[df.melt2$Q500 > Breaks[length(Breaks)]] <- Breaks[length(Breaks)] - 1
    df.melt2$MAX[df.melt2$MAX > Breaks[length(Breaks)]] <- Breaks[length(Breaks)] - 1
    df.melt2$PREFMAX[df.melt2$PREFMAX > Breaks[length(Breaks)]] <- Breaks[length(Breaks)] - 1
    # return(df.melt2)
    
    # make plot
    p[[pT]] <- ggplot(data=df.melt2, aes(x = LABEL, y = Q))
    p[[pT]] <- p[[pT]] + 
      geom_point(aes(shape = TYPE, size = TYPE, color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE))
    
    if (SEGMENTS){
      if(all(c("MAX","PRE-C MAX") %in% as.character(TYPES))){
        p[[pT]] <- p[[pT]] + 
          geom_linerange(data = subset(df.melt2, GROUP == "FAIL"),aes(ymin = PREFMAX, ymax = MAX, color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE), size = 0.25, lty="dashed") 
      }
      if(all(c("COLLAPSE","PRE-C MAX") %in% as.character(TYPES))){
        p[[pT]] <- p[[pT]] + 
          geom_linerange(data = subset(df.melt2, GROUP == "FAIL"),aes(ymin = FAIL, ymax = PREFMAX, color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE), size = 0.25, lty = "dotted")
      } 
      if(all(c("Q100","Q500") %in% as.character(TYPES))){
        p[[pT]] <- p[[pT]] + 
          geom_linerange(data = subset(df.melt2, GROUP == "FAIL"),aes(ymin = Q100, ymax = Q500, color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE), size = 0.25) 
      }
    }

    p[[pT]] <- p[[pT]] + 
      coord_flip() +  labs(y = ylab, title = title) +
      scale_x_discrete(limits = factor(limits, levels = limits, labels = limits), labels = as.expression(parse(text=levels(factor(limits, levels = limits, labels = limits))))) +
      # scale_y_log10(limits = c(Breaks[1],Breaks[length(Breaks)]), breaks = Breaks[2:(length(Breaks)-1)], minor_breaks = NULL, labels = BreaksLabels[2:(length(Breaks)-1)]) +
      scale_y_log10(breaks = c(5, 10, 50, 100, 500, 1000))+#breaks = Breaks[2:(length(Breaks)-1)])+#limits = c(Breaks[1],Breaks[length(Breaks)]), , minor_breaks = NULL) +
      scale_shape_manual(values = shapes,    name = legendsP$Qtype) +
      scale_color_manual(values = colorsP$Fail,     name = legendsP$Fail) +
      scale_size_manual(values = sizes) +
      scale_alpha_manual(values = alphasP$FailDate, name = legendsP$Date)
# return(p)
    if(LEGEND[i] == TRUE){
      p[[pT]] <- p[[pT]] +  guides(shape = guide_legend(order = 1,
                                                        override.aes = list(size = sizes)),
                                   color = guide_legend(order = 2,
                                                        override.aes = list(size = 4)),
                                   alpha = guide_legend(order = 3,
                                                        override.aes = list(shape = 15,
                                                                            size  = 4,
                                                                            linetype = "blank")),
                                   size  = FALSE,
                                   fill  = FALSE,
                                   group = FALSE
      ) 
    }
    else{
      p[[pT]] <- p[[pT]] + guides(shape = FALSE,
                                  color = FALSE,
                                  alpha = FALSE,
                                  size  = FALSE,
                                  fill  = FALSE
      ) 
    }
#     return(p)
    if (LABELS[i] == TRUE){
    # return(p)
      p[[pT]] <- p[[pT]] +
        theme(axis.text.y  = element_text(color = colorsLab,size = textP$reg[outputType], hjust = 0, vjust = 0)
        )
    }
    else{
      p[[pT]] <- p[[pT]] +
        theme( axis.text.y  = element_blank()
        )
    }
    # return(p)
    p[[pT]] <- p[[pT]] +
      theme(panel.background   = element_rect(fill = "white"),
            legend.key         = element_rect(fill = "white") ,
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.ticks.x = element_line(color = "black"),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.length=unit(-0.08, "cm"), 
            axis.text.x  = element_text(color = "black", size = textP$reg[outputType], angle = 90, vjust = 0.5, hjust = 1, margin  = margin(0.16,0,0.22,0, "cm")),
            axis.title.x = element_text(color = "black", size = textP$sub[outputType]),
            legend.text  = element_text(color = "black", size = textP$sub[outputType]),
            legend.title = element_text(color = "black", size = textP$sub[outputType]),
            legend.key.height=unit(0.7,"line"),
            plot.margin     = unit(c(0.1,0,0,0), "lines"),
            plot.title   = element_text(color = colorsP$Data["USGS"], size = textP$head[outputType])
            )
  }
  
  # GRID ARRANGE
  grp <- lapply(p, function(i) ggplot_gtable(ggplot_build(i)))
  if (SAVE) MakeGridPlot(grp, SCREEN = TRUE, SAVE = SAVE, SIZE = SIZE, SAVENAME = paste("Flow",paste(plotTypes,collapse = ""),"m3s",sep="-"))
  return(p)
  
}
