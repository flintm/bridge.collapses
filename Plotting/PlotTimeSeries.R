PlotTimeSeries <- function(FlowList, StatsList, BridgesDataFrame, titles = c("GOOD","AVERAGE","POOR"), SAVE = FALSE, 
                           TEXT = TRUE, outputType = "PRINT", SIZE = c(10,7),
                           saveName = "SeriesPlot",legend = FALSE, showAllX = FALSE, 
                           MAIN_FIELDS = c("STAID","State","GageArea","Build","Fail","Cause"),
                           SUPER_FIELDS = NA){
  # Q, Stats, and BridgesDataFrame should include only those IDs to be plotted (i.e, 3)
  
  require(ggplot2)
  require(plyr)
  require(grid)
  require(gridExtra)
  require(reshape2)
  
  source(file.path(dirsGit$ScriptsPlot,"MakeBridgeLabel.R"))
  source(file.path(dirsGit$ScriptsPlot,"SetupEncoding.R"))
  
  BridgesDataFrame$LABEL <- MakeBridgeLabel(BridgesDataFrame, MAIN_FIELDS = MAIN_FIELDS, SUPER_FIELDS = SUPER_FIELDS, PLOT_MATH = FALSE)
  
  # DATA TYPES
  DataTypes <- names(FlowList[[1]])[names(FlowList[[1]])!="USGS"]
  kgCfs          <- 0.035315*(1/3600)
  
  IDs  <- as.character(BridgesDataFrame[,"ID"])
  NSE  <- lapply(DataTypes, function(i) sapply(IDs, function(j) StatsList[[j]][[i]]$Nash))
  RMSE <- lapply(DataTypes, function(i) sapply(IDs, function(j) StatsList[[j]][[i]]$RMSE))
  MAE  <- lapply(DataTypes, function(i) sapply(IDs, function(j) StatsList[[j]][[i]]$MAE))
  KSsameDates <- lapply(DataTypes, function(i) sapply(IDs, function(j) StatsList[[j]][[i]]$KSsameDates$statistic))
  Rho         <- lapply(DataTypes, function(i) sapply(IDs, function(j) StatsList[[j]][[i]]$PearsonRho$estimate))
  p   <- list()
  grp <- list()
  
  minDate <- as.Date(min(sapply(1:length(FlowList), function(i) min(sapply(DataTypes,function(j) FlowList[[i]][[j]][1,"t"])))),"1970-01-01")
  maxDate <- as.Date(max(sapply(1:length(FlowList), function(i) max(sapply(DataTypes,function(j) FlowList[[1]][[j]][nrow(FlowList[[1]][[j]]),"t"])))),"1970-01-01")
  
  for(i in 1:length(FlowList)){
    df <- FlowList[[i]]$USGS
    for (j in DataTypes){
      df <- join(df,FlowList[[i]][[j]],by="t")
    }
    
    df <- df[order(as.numeric(df$t)),]
    df <- df[df$t>=minDate,]
    df <- df[df$t<=maxDate,]
    df.melt <- melt(df, "t", id.names = "t", value.name = "Q", variable.name = "DATA")
    df.melt$DATA <- factor(df.melt$DATA, levels = colnames(df)[colnames(df)!="t"], labels = unique(labelsP$Data[labelsP$Data %in% c("USGS",DataTypes)]))
    
    df2  <- data.frame(t = NA,
                       Q = NA,
                       type = factor(rep(c("FAILURE", "PRE-FAIL MAX", "MAX"),3)),
                       DATA = factor(c(rep("USGS",3), rep("VIC",3), rep("NLDAS",3))),
                       KNOWN_FAIL_DATE = BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"BOOL_KNOWN_FAIL_DATE"],
                       FAIL_CAUS_CODE  = BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"FAIL_CAUS_CODE"]
                        )
    df2$t    <- c(as.Date(BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"DATE_FAIL_EST_USGS"]),
                 as.Date(BridgesDataFrame[BridgesDataFrame$ID==IDs[i], "DATE_MAXPREFAIL_D_USGS"]),
                 as.Date(BridgesDataFrame[BridgesDataFrame$ID==IDs[i], "DATE_MAX_D_USGS"]), 
                 as.Date(BridgesDataFrame[BridgesDataFrame$ID==IDs[i], "DATE_FAILPM2_DVICG"],origin = "1970-01-01"),
                 as.Date(BridgesDataFrame[BridgesDataFrame$ID==IDs[i], "DATE_MAXPREFAIL_DVICG"],origin = "1970-01-01"),
                 as.Date(BridgesDataFrame[BridgesDataFrame$ID==IDs[i], "DATE_MAX_DVICG"],origin = "1970-01-01"), 
                 as.Date(BridgesDataFrame[BridgesDataFrame$ID==IDs[i], "DATE_FAILPM2_NLDASG"],origin = "1970-01-01"),
                 as.Date(BridgesDataFrame[BridgesDataFrame$ID==IDs[i], "DATE_MAXPREFAIL_NLDASG"],origin = "1970-01-01"),
                 as.Date(BridgesDataFrame[BridgesDataFrame$ID==IDs[i], "DATE_MAX_NLDASG"],origin = "1970-01-01"))
                         
    df2$Q   <- c(BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"Q_FAIL_D_USGS"],
                 BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"Q_MAXPREFAIL_D_USGS"],
                 BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"Q_MAX_D_USGS"],
                 BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"Q_FAILPM2_DVICG"], 
                 BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"Q_MAXPREFAIL_DVICG"], 
                 BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"Q_MAX_DVICG"],
                 BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"Q_FAILPM2_NLDASG"]*kgCfs*BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"DRAIN_SQKM"]*1000^2, 
                 BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"Q_MAXPREFAIL_NLDASG"]*kgCfs*BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"DRAIN_SQKM"]*1000^2, 
                 BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"Q_MAX_NLDASG"]*kgCfs*BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"DRAIN_SQKM"]*1000^2)
   title <- ifelse(is.na(titles),
                   BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"LABEL"],
                   paste(titles[i]," - ",
                        BridgesDataFrame[BridgesDataFrame$ID==IDs[i],"LABEL"],
                        sep = "")
                     )
   print(title)
    
    p[[IDs[i]]] <- ggplot(df.melt, aes(x = t, y = Q, color = DATA),alpha=0.5)
    p[[IDs[i]]] <- p[[IDs[i]]] + geom_line(aes(linetype = DATA),size = 0.25) +
      geom_point(data = df2, aes(shape = type, size = type, alpha = KNOWN_FAIL_DATE)) 

if (TEXT == TRUE){
  limitsx <- c(min(df$t,na.rm=TRUE),max(df$t,na.rm=TRUE))
  limitsy <- c(min(min(df.melt$Q,na.rm=TRUE),min(df2$Q,na.rm=TRUE)),
               max(max(df.melt$Q,na.rm=TRUE),max(df2$Q,na.rm=TRUE)))
  DataLabels <- c(VIC="Daymet~VIC",NLDAS="NLDAS~VIC")[c("VIC","NLDAS") %in% DataTypes]
  for (j in 1:length(DataTypes)){
    textLabel <- DataLabels[j]
    textRho <- paste("rho==",signif(Rho[[j]][i],3))
    textNSE <- paste("NSE==",signif(NSE[[j]][i],3))
    textMAE <- paste("MAE==",signif(MAE[[j]][i],3),"~cfs",sep="")
    textRMSE <- paste("RMSE==",signif(RMSE[[j]][i],3),"~cfs",sep="")
    placey <- limitsy[2] - 0.03*(limitsy[2] - limitsy[1])
    placey <- placey - 2*(j-1)*0.03*(limitsy[2] - limitsy[1])
#     print(seq(limitsx[1] + 0.2*(limitsx[2]-limitsx[1]),
#               limitsx[1] + 0.8*(limitsx[2]-limitsx[1]),length.out=5))
    # print(c(textLabel, textNSE, textRho, textMAE, textRMSE))
    # print(placey)
    p[[IDs[i]]] <- p[[IDs[i]]] + annotate("text",
                                          x     = seq(limitsx[1] + 0.2*(limitsx[2]-limitsx[1]),
                                                      limitsx[1] + 0.8*(limitsx[2]-limitsx[1]),length.out=5),
                                          y     = placey,
                                          label = c(textLabel, textNSE, textRho, textMAE, textRMSE),
                                          color = colorsP$Data[DataTypes[j]],
                                          size = textP$annotate[outputType],
                                          parse = TRUE
    ) 
  }
}
    
p[[IDs[i]]] <- p[[IDs[i]]] +     xlim(c(minDate,maxDate)) +
      labs(title = title, y = "Q [cfs]") +
      scale_color_manual(values = colorsP$Data[c("USGS",DataTypes)],     name = legendsP$Data) +
      scale_linetype_manual(values = linesP$Data[c("USGS",DataTypes)], name = legendsP$Data) + 
      scale_shape_manual(values = shapesP$Qtype[1:3],    name = legendsP$Qtype) +
      scale_size_manual(values = 1.2*sizesP$Qtype[1:3]) +
      scale_alpha_manual(values = alphasP$FailDate, name = legendsP$Date) 

  if (legend == TRUE){
    p[[IDs[i]]] <- p[[IDs[i]]] + guides(#shape = guide_legend(order = 1,
                                  # override.aes = list(size = sizesP$Qtype[1:3])),
            linetype  = guide_legend(order = 2,
                                  override.aes = list(colour = c(colorsP$Data[1],colorsP$Data[DataTypes]))),
#              alpha = guide_legend(order = 3,
#                                   override.aes = list(size  = 4)),
             size  = FALSE,
            shape = FALSE,
            alpha = FALSE,
            color = FALSE
      ) 
  }
else{
  p[[IDs[i]]] <- p[[IDs[i]]] +guides(shape = FALSE,
         color = FALSE,
         alpha = FALSE,
         size  = FALSE,
         linetype = FALSE
  ) 
}
if(showAllX == TRUE){
  p[[IDs[i]]] <- p[[IDs[i]]] + theme(panel.background   = element_rect(fill = "white"),
            legend.key         = element_rect(fill = "white") ,
            legend.text  = element_text(color = "black", size = textP$sub[outputType]),
            legend.title = element_text(color = "black", size = textP$sub[outputType]),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.margin       = unit(c(0,-0.25,0,-0.25),"in"),
            axis.text.y  = element_text(color = "black",size = textP$sub[outputType], margin = margin(r = -0.3,unit ="in")),
            axis.title.y = element_text(color = "black",size = textP$sub[outputType]) ,
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x  = element_text(color = "black",size = textP$sub[outputType], margin =margin(-0.1,unit ="in")),
            plot.title   = element_text(color = "black", size = textP$head[outputType]),
            # axis.ticks.margin = unit(-0.15,"in"),
            axis.title.x = element_blank(),
            plot.margin     = unit(c(0,0,0,0), "lines"),
            legend.position = "bottom",
            legend.margin     = unit(-0.3, "in")
      )
}
else{
  if (i < length(FlowList)){
  p[[IDs[i]]] <- p[[IDs[i]]] + theme(panel.background   = element_rect(fill = "white"),
                                     legend.key         = element_rect(fill = "white") ,
                                     legend.key.height=unit(0.4,"line"),
                                     legend.text  = element_text(color = "black", size = textP$sub[outputType]),
                                     legend.title = element_text(color = "black", size = textP$sub[outputType]),
                                     panel.grid.major.y = element_blank(),
                                     panel.grid.major.x = element_blank(),
                                     panel.grid.minor.y = element_blank(),
                                     panel.grid.minor.x = element_blank(),
                                     panel.margin       = unit(c(-0,-0.25,-0.1,-0.25),"in"),
                                     axis.text.y  = element_text(color = "black",size = textP$sub[outputType], margin = margin(r=-0.3,unit ="in")),
                                     axis.title.y = element_text(color = "black",size = textP$sub[outputType]) ,
                                     axis.ticks.y = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     # axis.ticks.margin = unit(-0.15,"in"),
                                     axis.text.x  =  element_blank(), #element_text(color = "black",size = textP$sub[outputType]),
                                     plot.title   = element_text(color = "black", size = textP$head[outputType]),
                                     axis.title.x = element_blank(),
                                     plot.margin     = unit(c(0,0,0,0), "lines"),
                                     legend.position = "bottom",
                                     #                       legend.box = "horizontal",
                                     #                       legend.direction = "vertical",
                                     legend.margin     = unit(-0.3, "in")    
  )
  }
  else{
    p[[IDs[i]]] <- p[[IDs[i]]] + theme(panel.background   = element_rect(fill = "white"),
                                       legend.key         = element_rect(fill = "white") ,
                                       legend.key.height=unit(0.4,"line"),
                                       legend.text  = element_text(color = "black", size = textP$sub[outputType]),
                                       legend.title = element_text(color = "black", size = textP$sub[outputType]),
                                       panel.grid.major.y = element_blank(),
                                       panel.grid.major.x = element_blank(),
                                       panel.grid.minor.y = element_blank(),
                                       panel.grid.minor.x = element_blank(),
                                       panel.margin       = unit(c(-0,-0.25,-0.15,-0.25),"in"),
                                       axis.text.y  = element_text(color = "black",size = textP$sub[outputType], margin = margin(r=-0.3,unit ="in")),
                                       axis.title.y = element_text(color = "black",size = textP$sub[outputType]) ,
                                       axis.ticks.y = element_blank(),
                                       axis.ticks.x = element_blank(),
                                       # axis.ticks.margin = unit(-0.05,"in"),
                                       axis.title.x = element_blank(),
                                       axis.text.x  = element_text(color = "black",size = textP$sub[outputType], margin = margin(-0.1,unit = "in")),
                                       plot.title   = element_text(color = "black", size = textP$head[outputType]),
                                       plot.margin     = unit(c(0,0,0,0), "lines") ,
                                       legend.position = "bottom",
                                       legend.margin     = unit(-0.3, "in")
                                       # axis.ticks.margin = unit(-0.1,"lines")
                                       #                       legend.box = "horizontal",
                                       #                       legend.direction = "vertical",
    )
    
  }
}

    grp[[i]] <- ggplot_gtable(ggplot_build(p[[i]]))
  }
  
#   maxHeight = unit.pmax(grp[[1]]$heights[2:3], grp[[2]]$heights[2:3], grp[[3]]$heights[2:3])
#   for (i in 1:length(FlowList)){
#     grp[[i]]$heights[2:3] <- maxHeight
#   }
  grid.arrange(grp[[1]],grp[[2]],grp[[3]], as.table=TRUE)
if (SAVE == TRUE){
  pdf(file = file.path(dirsGit$Plots,paste(saveName,"pdf",sep=".")),width=SIZE[2],height=SIZE[1])
  grid.arrange(grp[[1]],grp[[2]],grp[[3]], as.table=TRUE)
  dev.off()}
return(p)
  
}
