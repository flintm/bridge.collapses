# Fig. 4b for "Historical Analysis of Hydraulic Bridge Collapses in the Continental United States"
# Plots return periods of major events,
# including failure, the max event, and the max event pre-failure
# Copyright Madeleine Flint 2016

PlotReturnPeriodsFailMax <- function(BridgesDataFrame, Breaks = c(1,50,100,500,1000), 
                                     SAVE = FALSE, LEGEND = FALSE, LABELS = FALSE, 
                                     plotTypes = c("pHECd","pHECip"), outputType = "PRINT",
                                     MAIN_FIELDS = c("Build","Fail","State"),
                                     SUPER_FIELDS = c("Hurr","Area"), 
                                     SIZE = c(7,11), AREA_RATIO = TRUE,
                                     T_REPORT = TRUE, SEGMENTS = TRUE,
                                     SHOW_Q100_SYMB = FALSE,
                                     embedFonts = FALSE, gs_path = "/usr/local/bin/gs"){
  require(ggplot2)
  require(grid)
  require(gridExtra)
  require(plyr)
  require(reshape2)
  if(!("MakeBridgeLabel" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","MakeBridgeLabel.R"))
  if(!("MakeGridPlot" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","MakeGridPlot.R"))
  if(!("colorsP" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","SetupEncoding.R"))
  if(!("getTheme" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","getTheme.R"))
  
  labelmin <- "Min"
  labelmax <- "Max"
  BreaksLabels <- format(Breaks,scientific = FALSE, drop0trailing = TRUE)
  # BreaksLabels <- sapply(BreaksLabels, function(L) gsub(".[0]{0,4}[1,5]","",L))      
  
  # LABELS
  BridgesDataFrame$LABEL <- MakeBridgeLabel(BridgesDataFrame, MAIN_FIELDS = MAIN_FIELDS, SUPER_FIELDS = SUPER_FIELDS, PLOT_MATH = TRUE, PAD_LEFT = TRUE)
  BridgesDataFrame <- BridgesDataFrame[order(BridgesDataFrame$FAIL_CAUS_CODE,BridgesDataFrame$YR_BLT_EST,BridgesDataFrame$YR_FAIL, decreasing = TRUE, na.last = FALSE),]
  limits <- BridgesDataFrame[,"LABEL"]
  if(nrow(BridgesDataFrame)==35){
    limits <- c(limits[1:5],"a",limits[6],"b",limits[7:22],"c",limits[23:35])
    colorsLab <- c(rep("black",5),"white","black","white",rep("black",16),"white",rep("black",13))
  }
  else{
    colorsLab <- rep("black",nrow(BridgesDataFrame))
  }
  
  if(T_REPORT == TRUE){
    # clean up T_REPORT
    # if T_REPORT has text that is not "TR", it's related to the storm not the gauge - gray it out
    # If it has a >#, then need to use # to infinity
    # If it has #<TR<#, then bounded segment
    ys <- strsplit(BridgesDataFrame$T_REPORT,"[[:alpha:]]")
    storms <- sapply(1:length(ys), function(i) length(ys[[i]])>3)
    ymin <- unlist(lapply(ys,"[",1))
    ymax <- unlist(lapply(ys,"[",3))
    ymin[storms] <- sapply((1:length(ys))[storms], function(i) ys[[i]][length(ys[[i]])])
    ymax[grepl(">[[:digit:]]",ymin)] <- Breaks[length(Breaks)] - 1
    ymax <- as.numeric(gsub("[[:punct:]]","",ymax))
    ymin <- as.numeric(gsub("[[:punct:]]","",ymin))
    y    <- ymin
    ymin[is.na(ymax)] <- NA_real_
    y2   <- ymax
    y2[y2 == Breaks[length(Breaks)] - 1] <- NA_real_
    df.report <- data.frame(LABEL = BridgesDataFrame$LABEL, y = y, y2=y2, ymin = ymin, ymax= ymax)
    df.report <- melt(df.report,measure.vars = c("y","y2"), value.name = "T")
    df.report <- df.report[,colnames(df.report)!="variable"]
    df.report$GROUP <- "REPORTED"
    df.report$BOOL_KNOWN_FAIL_DATE <- "KNOWN"
    df.report[storms,"BOOL_KNOWN_FAIL_DATE"] <- "UNKNOWN"
    df.report <- df.report[!is.na(df.report$T) | !is.na(df.report$ymin),]
  }
  
  p <- list()
  nP <- length(plotTypes)
  if (length(LEGEND)!=nP) LEGEND <- rep(LEGEND,nP)
  if (length(LABELS)!=nP) LABELS <- rep(LABELS,nP)
  
  for (i in 1:nP){
    pT <- plotTypes[i]
    df <- switch(pT,
                 pHECd     = BridgesDataFrame[,c("LABEL","T_FAIL_D_HECD_USGS","T_MAX_D_HECD_USGS","T_MAXPREFAIL_D_HECD_USGS")],
                 pHECip    = BridgesDataFrame[,c("LABEL","T_FAIL_IP_HECP_USGS","T_MAX_P_HECP_USGS","T_MAXPREFAIL_P_HECP_USGS")],
                 pHECdip   = BridgesDataFrame[,c("LABEL","T_FAIL_D_HECD_USGS","T_MAX_D_HECD_USGS","T_MAXPREFAIL_D_HECD_USGS","T_FAIL_IP_HECP_USGS","T_MAX_P_HECP_USGS","T_MAXPREFAIL_P_HECP_USGS")]
    )
    title <- switch(pT,
                    pHECd     = "BULLETIN 17B - DAILY MEAN",
                    pHECip    = "BULLETIN 17B - INST/PEAK",
                    pHECdip   = "BULLETIN 17B - DAILY MEAN & INST/PEAK"
    )
    if (SHOW_Q100_SYMB == TRUE){ 
      df$Q100 <- NA
      df$Q500 <- NA
      df[1,"Q100"] <- Breaks[1]+1
      df[1,"Q500"] <- Breaks[1]+1
      extraCols <- c("Q100", "Q500")
    }
    else extraCols <- c()
    
    if (pT != "pHECdip"){
      colnames(df)[2:4] <- c("FAIL","MAX","PREFMAX")
      df.melt <- melt(df, id.names = "LABEL", value.name = "T", variable.name = "GROUP")
      df.melt$TYPE <- factor(df.melt$GROUP, levels = c("FAIL", "PREFMAX", "MAX", extraCols), labels = c(labelsP$Qtype[1:3], extraCols))
      df.melt      <- join(df.melt, BridgesDataFrame[,c("LABEL", "FAIL_CAUS_CODE", "HURRICANE", "BOOL_KNOWN_FAIL_DATE")], by = "LABEL")
      
      df.line      <- df[1:(nrow(df)-2),c("LABEL", "MAX","PREFMAX", "FAIL")]
      if (any(df.line$FAIL >= Breaks[length(Breaks)] | df.line$MAX >= Breaks[length(Breaks)] | df.line$PREFMAX >= Breaks[length(Breaks)])){
        print("One T larger than max break")
        df.line[df.line$FAIL >= Breaks[length(Breaks)] & !is.na(df.line$FAIL),"FAIL"] <- Breaks[length(Breaks)] - 1
        df.line[df.line$MAX >= Breaks[length(Breaks)] & !is.na(df.line$MAX),"MAX"] <- Breaks[length(Breaks)] - 1
        df.line[df.line$PREFMAX >= Breaks[length(Breaks)] & !is.na(df.line$PREFMAX),"PREFMAX"] <- Breaks[length(Breaks)] - 1
      }
      
      df.melt2     <- join(df.melt, df.line, by = "LABEL", match = "first")
      df.melt2$T[df.melt2$T > Breaks[length(Breaks)]] <- Breaks[length(Breaks)] - 1
      
      df.shading <- data.frame(xmin  = rep(limits[1], length(Breaks) - 1),
                               xmax  = rep(limits[length(limits)], length(Breaks) - 1),
                               ymin  = Breaks[1:(length(Breaks) - 1)],
                               ymax  = Breaks[2:length(Breaks)],
                               group = factor(c(rep(1,sum(Breaks<100)),2,rep(3,sum(Breaks > 500)))),
                               LABEL = rep(limits[1], length(Breaks) - 1),
                               T     = Breaks[1:(length(Breaks) - 1)],
                               stringsAsFactors = FALSE)
      
      shapes <- shapesP$Qtype[1:3]
      sizes  <- sizesP$Qtype[1:3]
      sizesLab <- sizesP$Qtype[c(1,3,2)]
    }
    else{
      cols <- c("FAILD","MAXD","PREFMAXD", "FAILIP", "MAXIP", "PREFMAXIP")
      colnames(df)[2:7] <- cols
      df.melt <- melt(df, id.names = "LABEL", value.name = "T", variable.name = "GROUP")
      df.melt$TYPE <- factor(df.melt$GROUP, levels = c("FAILD", "PREFMAXD", "MAXD", "FAILIP", "PREFMAXIP", "MAXIP", extraCols), labels = c(labelsP$QtypeDIP[1:6], extraCols))
      df.melt      <- join(df.melt, BridgesDataFrame[,c("LABEL", "FAIL_CAUS_CODE", "HURRICANE", "BOOL_KNOWN_FAIL_DATE")], by = "LABEL")
      
      df.line      <- df[1:(nrow(df)-2),c("LABEL", "FAILD","MAXD","PREFMAXD", "FAILIP", "MAXIP", "PREFMAXIP")]
      if (any(sapply(cols, function(j) df.line[,j] >= Breaks[length(Breaks)]))){
        df.line[df.line$FAILD >= Breaks[length(Breaks)] & !is.na(df.line$FAILD),"FAILD"] <- Breaks[length(Breaks)] - 1
        df.line[df.line$MAXD >= Breaks[length(Breaks)] & !is.na(df.line$MAXD),"MAXD"] <- Breaks[length(Breaks)] - 1
        df.line[df.line$PREFMAXD >= Breaks[length(Breaks)] & !is.na(df.line$PREFMAXD),"PREFMAXD"] <- Breaks[length(Breaks)] - 1
        df.line[df.line$FAILIP >= Breaks[length(Breaks)] & !is.na(df.line$FAILIP),"FAILIP"] <- Breaks[length(Breaks)] - 1
        df.line[df.line$MAXIP >= Breaks[length(Breaks)] & !is.na(df.line$MAXIP),"MAXIP"] <- Breaks[length(Breaks)] - 1
        df.line[df.line$PREFMAXIP >= Breaks[length(Breaks)] & !is.na(df.line$PREFMAXIP),"PREFMAXIP"] <- Breaks[length(Breaks)] - 1
      }
      
      df.melt2     <- join(df.melt, df.line, by = "LABEL", match = "first")
      df.melt2$T[df.melt2$T > Breaks[length(Breaks)]] <- Breaks[length(Breaks)] - 1
      
      df.shading <- data.frame(xmin  = rep(limits[1], length(Breaks) - 1),
                               xmax  = rep(limits[length(limits)], length(Breaks) - 1),
                               ymin  = Breaks[1:(length(Breaks) - 1)],
                               ymax  = Breaks[2:length(Breaks)],
                               group = factor(c(rep(1,sum(Breaks<100)),2,rep(3,sum(Breaks > 500)))),
                               LABEL = rep(limits[1], length(Breaks) - 1),
                               T     = Breaks[1:(length(Breaks) - 1)],
                               stringsAsFactors = FALSE)
      
      shapes <- shapesP$QtypeDIP[1:6]
      sizes  <- sizesP$QtypeDIP[1:6]
      sizesLab <- sizesP$QtypeDIP[c(1,3,2,4,6,5)]
    }
    if (SHOW_Q100_SYMB == TRUE & LEGEND[i] == TRUE){
      shapes  <- c(shapes, shapesP$Qtype[4:5])
      sizes <- c(sizes, sizesP$Qtype[4:5])
      sizesLab <- c(sizesLab, sizesP$Qtype[4:5])
    }
    
    # START PLOTTING
    p[[pT]]  <- ggplot(df.melt2, aes(x = LABEL, y = T))
    
    p[[pT]] <- p[[pT]] + 
      geom_rect(data = df.shading, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = group), color = NA, alpha = alphasP$grays) +
      geom_point(data = subset(df.melt2, !(TYPE %in% c("Q100","Q500"))), aes(shape = TYPE, size = TYPE, alpha = BOOL_KNOWN_FAIL_DATE, color = FAIL_CAUS_CODE), na.rm = TRUE) 
    if (SHOW_Q100_SYMB == TRUE) p[[pT]] <- p[[pT]] + geom_point(data = subset(df.melt2, TYPE %in% c("Q100","Q500")), aes(shape = TYPE, size = TYPE), color = "white", alpha = 0, na.rm = TRUE) 
    if (T_REPORT == TRUE){
      p[[pT]] <- p[[pT]] + 
        geom_point(data = subset(df.report, !is.na(T)),aes(shape = GROUP, size = GROUP, alpha = BOOL_KNOWN_FAIL_DATE), na.rm = TRUE) +  
        geom_linerange(data = subset(df.report, !is.na(ymin)), aes(x = LABEL, ymin = ymin, ymax = ymax), size = 0.25, na.rm = TRUE) 
      shapes <- c(shapes, shapesP$Qtype[6])
      sizes <- c(sizes, sizesP$Qtype[6])
      # sizesLab <- c( sizesLab)
    }
    if(SEGMENTS==TRUE & pT != "pHECdip"){
      p[[pT]] <- p[[pT]] +
        geom_linerange(aes(ymin = PREFMAX, ymax = MAX, color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE), size = 0.25, lty="dashed", na.rm = TRUE) +
        geom_linerange(aes(ymin = FAIL, ymax = PREFMAX, color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE), size = 0.25, lty = "dotted", na.rm = TRUE)
    }
    if(SEGMENTS==TRUE & pT == "pHECdip"){
      p[[pT]] <- p[[pT]] +
        geom_linerange(aes(ymin = PREFMAXD, ymax = MAXD, color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE), size = 0.25, lty="dashed", na.rm = TRUE) +
        geom_linerange(aes(ymin = FAILD, ymax = PREFMAXD, color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE), size = 0.25, lty = "dotted", na.rm = TRUE) +
        geom_linerange(aes(ymin = PREFMAXIP, ymax = MAXIP, color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE), size = 0.25, lty="dashed", na.rm = TRUE) +
        geom_linerange(aes(ymin = FAILIP, ymax = PREFMAXIP, color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE), size = 0.25, lty = "dotted", na.rm = TRUE)
    }
    
    p[[pT]] <- p[[pT]] +
      coord_flip() +  labs(title = title, y = expression(paste(T["R"]," [years]"))) + #expression(paste("Value is ", sigma,",", R^{2},'=0.6'))
      scale_y_log10(limits = c(Breaks[1],Breaks[length(Breaks)]), breaks = Breaks, minor_breaks = NULL, labels = BreaksLabels) +
      scale_x_discrete(limits = factor(limits, levels = limits, labels = limits), labels = as.expression(parse(text=levels(factor(limits, levels = limits, labels = limits))))) +
      
      scale_color_manual(values = colorsP$Fail,     name = legendsP$Fail) + 
      scale_fill_manual(values = fillsP$grays)  +
      scale_shape_manual(values = shapes,    name = legendsP$Qtype, breaks = names(shapes)) +
      scale_size_manual(values = sizes, breaks = names(sizes)) +
      scale_alpha_manual(values = alphasP$FailDate, name = legendsP$Date) 
    
    if(LEGEND[i]){
      p[[pT]] <- p[[pT]] + guides(color = guide_legend(order = 1,
                                                       override.aes = list(size = 4,
                                                                           shape = 15,
                                                                           linetype = "blank")),
                                  shape = guide_legend(order = 2,
                                                       override.aes = list(size = sizes,
                                                                           shape = shapes)),     
                                  
                                  alpha = guide_legend(order = 3,
                                                       override.aes = list(shape = 15,
                                                                           size  = 4,
                                                                           alpha = c(1,0.2),
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
    p[[pT]] <- p[[pT]] + getTheme(outputType) +
      theme(axis.text.x   = element_text(color = "black", size = textP$reg[outputType], 
                                         angle = 90, vjust = 0.5, hjust = 1, margin  = margin(0.16,0,0,0, "cm")), #margin is top, right, bottom, left
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            legend.margin = margin(t=-0.1,r=-0.1,b=-0.1,l=-0.1, "cm"),
            plot.margin   = unit(c(0.1,0.15,0,0), "lines"))
    
    if (LABELS[i] == TRUE){
      p[[pT]] <- p[[pT]] +
        theme(axis.text.y  = element_text(color = colorsLab,size = textP$reg[outputType], hjust = 0, vjust = 0)
        )
    }
    else{
      p[[pT]] <- p[[pT]] +
        theme( axis.text.y  = element_blank()
        )
    }
  }
  
  # GRID ARRANGE
  grp <- lapply(p, function(i) ggplot_gtable(ggplot_build(i)))
  MakeGridPlot(grp, SCREEN = TRUE, SAVE = SAVE, SIZE = SIZE, SAVENAME = "Fig4b", embedFonts = embedFonts, gs_path = gs_path)
  return(p)
}

