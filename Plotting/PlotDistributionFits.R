# Plot panel of 
PlotDistributionFits <- function(BridgesDataFrame,ls.df, TYPE = "USGS", SAVE = FALSE, TEXT = TRUE, ONLY = "LP3", ANY=NULL, labelsType = "yearsState",
                                 SCALE = "LINEAR", LEGEND = "NONE", outputType = "PRINT", SIZE = c(4,13), 
                                 ANNOTATE_LOC = "BR",HURR_LAB="*",AREA_RATIO=TRUE){
  # SETUP
  colorsPDists <- colorsP$Dists
  linesPDists  <- linesP$Dists
  limits       <- c(0,5000)
  ylab <- expression(paste("Cumulative density function of Q, ",F[Q]))
  # LABELS
  if (labelsType == "long"){
    BridgesDataFrame$LABEL <- paste("c.",
                                    substr(BridgesDataFrame[,"YR_BLT_EST"],1,4),
                                    ", f.",
                                    substr(BridgesDataFrame[,"YR_FAIL"],1,4),
                                    " - ",
                                    sapply(1:nrow(BridgesDataFrame), function(i) df.States[df.States$STFIPS==BridgesDataFrame[i,"STFIPS"],"STATE_CODE"]),
                                    sep="")
  }
  if (labelsType == "yearsState"){
    BridgesDataFrame$LABEL <- paste(substr(BridgesDataFrame[,"YR_BLT_EST"],1,4),
                                    " - ",
                                    substr(BridgesDataFrame[,"YR_FAIL"],1,4),
                                    " - ",
                                    sapply(1:nrow(BridgesDataFrame), function(i) df.States[df.States$STFIPS==BridgesDataFrame[i,"STFIPS"],"STATE_CODE"]),
                                    sep="")
  }
  BridgesDataFrame$BOOL_WAS_HURRICANE                                 <- nchar(BridgesDataFrame$COMMENT_LINKED_HURRICANE)!=0
  BridgesDataFrame$HURRICANE                                     <- labelsP$Hurr[2]
  BridgesDataFrame[BridgesDataFrame$BOOL_WAS_HURRICANE, "HURRICANE"]  <- labelsP$Hurr[1]
  BridgesDataFrame$HURRICANE                                     <- factor(BridgesDataFrame$HURRICANE, levels = labelsP$Hurr, labels = labelsP$Hurr) 
  
  labs <- character(nrow(BridgesDataFrame))
  if (HURR_LAB=="*"){
    rows <- BridgesDataFrame$BOOL_WAS_HURRICANE & !is.na(BridgesDataFrame$BOOL_WAS_HURRICANE)
    labs[rows] <- "H"
  }
  if (HURR_LAB=="COLOR"){
    labelHurrCol <- sapply(limits,function(i) ifelse(BridgesDataFrame[BridgesDataFrame$LABEL == i,"HURRICANE"]=="YES",
                                                     colorsP$Hurr[1],
                                                     "#000000"))
  }
  else{
    labelHurrCol <- "black"
  }
  
  if (AREA_RATIO==TRUE){
    # >= 1.2
    rows <- BridgesDataFrame$AREA_RATIO_NHD >= 1.2
    labs[rows] <- paste(labs[rows],"+",sep="")
    # >= 1.4
    rows <- BridgesDataFrame$AREA_RATIO_NHD >= 1.4
    labs[rows] <- paste(labs[rows],"+",sep="")
    # <= 0.8
    rows <- BridgesDataFrame$AREA_RATIO_NHD <= 0.8
    labs[rows] <- paste(labs[rows],"-",sep="")
    # <= 0.6
    rows <- BridgesDataFrame$AREA_RATIO_NHD <= 0.6
    labs[rows] <- paste(labs[rows]," -",sep="")
  }
  
  labs[!labs==""] <- paste("^'",labs[!labs==""],"'",sep = "")
  BridgesDataFrame$LABEL <- paste(BridgesDataFrame$LABEL,labs,sep="")
  
p <- list()

# PANEL 1: USGS
if ("USGS" %in% TYPE){
  df   <- ls.df[["USGS"]][["df"]]
  df$ECDF <- (df$GEV)
  df.melt <- melt(df,id.vars = "Q", variable.name = "GROUP", value.name = "F_Q")
  df.melt$GROUP <- factor(as.character(df.melt$GROUP), levels = labelsP$Dists[c("ECDF","LP3","GEV")], labels = labelsP$Dists[c("ECDF","LP3","GEV")])
  colorsPDists["ECDF"] <- colorsP$Data["USGS"]
  
  p1 <- ggplot(data=df.melt) + 
  stat_ecdf(data = subset(df.melt,GROUP=="ECDF"),aes(x=Q, color=GROUP, linetype=GROUP)) + 
  geom_line(data = subset(df.melt,GROUP!="ECDF"),aes(x=Q,y=F_Q,group=GROUP,color=GROUP,linetype=GROUP)) 
    if (TEXT==TRUE & ANNOTATE_LOC=="BR"){
      p1 <- p1 +
    annotate("text",
             x = 0.99*max(df$Q),
             y = c(0.08,0.04),
             label = c(paste("LP3: A2 = ",signif(ls.df[["USGS"]][["AD"]]$LP3,2)," (p = ",signif(ls.df[["USGS"]][["AD"]]$LP3p,2),")",sep=""),
                       paste("GEV: A2 = ",signif(ls.df[["USGS"]][["AD"]]$GEV,2)," (p = ",signif(ls.df[["USGS"]][["AD"]]$GEVp,2),")",sep="")),
             size   = 1*textP$annotate[outputType],
             hjust = 1)#,
#              parse = TRUE) 
    }
    p1 <- p1 +
    labs(x = "Annual Peak Flow, Q [cfs], USGS",y=ylab,title=BridgesDataFrame$LABEL) +
#     xlim(limits)+
  
    scale_color_manual(values = colorsPDists[c("ECDF","LP3","GEV")], name = legendsP$Dists, labels = c("USGS-ECDF","USGS-LP3","USGS-GEV")) +
    scale_linetype_manual(values = linesPDists[c("ECDF","LP3","GEV")]) +
    
    guides(color = guide_legend(order = 1,
                                override.aes = list(linetype=linesPDists[c("ECDF","LP3","GEV")]
                                                    )),
           linetype = FALSE) +
    
    theme(panel.background   = element_rect(fill = "white"),
          legend.key         = element_rect(fill = "white") ,
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks.length=unit(-0.08, "cm"), axis.ticks.margin=unit(0.16, "cm"),
          axis.text.x  = element_text(color = "black", size = textP$reg[outputType]),
          axis.title.x = element_text(color = "black", size = textP$sub[outputType]),
          axis.text.y  = element_text(color = "black", size = textP$reg[outputType]),
          axis.title.y  = element_text(color = "black", size = textP$sub[outputType]),
          legend.text  = element_text(color = "black", size = textP$sub[outputType]),
          legend.title = element_text(color = "black", size = textP$sub[outputType]),
          plot.title   = element_text(color = "black", size = textP$head[outputType])
    )
  p[[1]] <- p1
}

  # PANEL 2: VICG
if ("VICg" %in% TYPE){
df   <- ls.df[["VICg"]][["df"]]
df$ECDF2 <- (df$GEV)
df.melt <- melt(df,id.vars = "Q", variable.name = "GROUP", value.name = "F_Q")
df.melt$GROUP <- factor(as.character(df.melt$GROUP), levels = c("ECDF2","LP3","GEV"), labels = c("ECDF2","LP3","GEV"))
colorsPDists["ECDF2"] <- colorsP$Data["VICG"]
linesPDists["ECDF2"]  <- linesP$Dists["ECDF"]

p2 <- ggplot(data=df.melt) + 
  stat_ecdf(data = subset(df.melt,GROUP=="ECDF2"),aes(x=Q, color=GROUP, linetype=GROUP)) + 
  geom_line(data = subset(df.melt,GROUP!="ECDF2"),aes(x=Q,y=F_Q,group=GROUP,color=GROUP,linetype=GROUP))  
  if (TEXT==TRUE & ANNOTATE_LOC=="BR"){
    p2 <- p2 +
      annotate("text",
               x = 0.99*max(df$Q),
               y = c(0.08,0.04),
               label = c(paste("LP3: A2 = ",signif(ls.df[["VICg"]][["AD"]]$LP3,2)," (p = ",signif(ls.df[["VICg"]][["AD"]]$LP3p,2),")",sep=""),
                         paste("GEV: A2 = ",signif(ls.df[["VICg"]][["AD"]]$GEV,2)," (p = ",signif(ls.df[["VICg"]][["AD"]]$GEVp,2),")",sep="")),
               size = 1*textP$annotate[outputType],
               hjust = 1)#,
    #              parse = TRUE) 
  }
p2 <- p2 +
  labs(x = "Annual Maximum Daily Flow, Q [cfs], Daymet-VIC-Gauge",y=ylab,title=BridgesDataFrame$LABEL) +
#   xlim(limits)+
  
  scale_color_manual(values = colorsPDists[c("ECDF2","LP3","GEV")], name = legendsP$Dists, labels = c("VIC-GAUGE-ECDF","VIC-GAUGE-LP3","VIC-GAUGE-GEV")) +
  scale_linetype_manual(values = linesPDists[c("ECDF2","LP3","GEV")]) +
  
  guides(color = guide_legend(order = 1,
                              override.aes = list(linetype=linesPDists[c("ECDF2","LP3","GEV")]
                              )),
         linetype = FALSE) +
  
  theme(panel.background   = element_rect(fill = "white"),
        legend.key         = element_rect(fill = "white") ,
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(-0.08, "cm"), axis.ticks.margin=unit(0.16, "cm"),
        axis.text.x  = element_text(color = "black", size = textP$reg[outputType]),
        axis.title.x = element_text(color = "black", size = textP$sub[outputType]),
        axis.text.y  = element_text(color = "black", size = textP$reg[outputType]),
        axis.title.y  = element_text(color = "black", size = textP$sub[outputType]),
        legend.text  = element_text(color = "black", size = textP$sub[outputType]),
        legend.title = element_text(color = "black", size = textP$sub[outputType]),
        plot.title   = element_text(color = "black", size = textP$head[outputType])
  )
p[[length(p)+1]] <- p2
}

  
  # PANEL 3: VICB
if ("VICb" %in% TYPE){
  df   <- ls.df[["VICb"]][["df"]]
  df$ECDF3 <- (df$GEV)
  df.melt <- melt(df,id.vars = "Q", variable.name = "GROUP", value.name = "F_Q")
  df.melt$GROUP <- factor(as.character(df.melt$GROUP), levels = c("ECDF3","LP3","GEV"), labels = c("ECDF3","LP3","GEV"))
  colorsPDists["ECDF3"] <- colorsP$Data["VICB"]
  linesPDists["ECDF3"]  <- linesP$Dists["ECDF"]
  
  p3 <- ggplot(data=df.melt) + 
    stat_ecdf(data = subset(df.melt,GROUP=="ECDF3"),aes(x=Q, color=GROUP, linetype=GROUP)) + 
    geom_line(data = subset(df.melt,GROUP!="ECDF3"),aes(x=Q,y=F_Q,group=GROUP,color=GROUP,linetype=GROUP))
    if (TEXT==TRUE & ANNOTATE_LOC=="BR"){
      p3 <- p3 +
        annotate("text",
                 x = 0.99*max(df$Q),
                 y = c(0.08,0.04),
                 label = c(paste("LP3: A2 = ",signif(ls.df[["VICb"]][["AD"]]$LP3,2)," (p = ",signif(ls.df[["VICb"]][["AD"]]$LP3p,2),")",sep=""),
                           paste("GEV: A2 = ",signif(ls.df[["VICb"]][["AD"]]$GEV,2)," (p = ",signif(ls.df[["VICb"]][["AD"]]$GEVp,2),")",sep="")),
                 size = 0.9*textP$annotate[outputType],
                 hjust = 1)#,
      #              parse = TRUE) 
    }
  p3 <- p3 +
    labs(x = "Annual Maximum Daily Flow, Q [cfs], Daymet-VIC-Bridge",y=ylab,title=BridgesDataFrame$LABEL) +
    #   xlim(limits)+
    
    scale_color_manual(values = colorsPDists[c("ECDF3","LP3","GEV")], name = legendsP$Dists, labels = c("VIC-BRIDGE-ECDF","VIC-BRIDGE-LP3","VIC-BRIDGE-GEV")) +
    scale_linetype_manual(values = linesPDists[c("ECDF3","LP3","GEV")]) +
    
    guides(color = guide_legend(order = 1,
                                override.aes = list(linetype=linesPDists[c("ECDF3","LP3","GEV")]
                                )),
           linetype = FALSE) +
    
    theme(panel.background   = element_rect(fill = "white"),
          legend.key         = element_rect(fill = "white") ,
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks.length=unit(-0.08, "cm"), axis.ticks.margin=unit(0.16, "cm"),
          axis.text.x  = element_text(color = "black", size = textP$reg[outputType]),
          axis.title.x = element_text(color = "black", size = textP$sub[outputType]),
          axis.text.y  = element_text(color = "black", size = textP$reg[outputType]),
          axis.title.y  = element_text(color = "black", size = textP$sub[outputType]),
          legend.text  = element_text(color = "black", size = textP$sub[outputType]),
          legend.title = element_text(color = "black", size = textP$sub[outputType]),
          plot.title   = element_text(color = "black", size = textP$head[outputType])
    )
  p[[length(p)+1]] <- p3
}

# ARRANGE GRID
grp <- lapply(p, function(i) ggplot_gtable(ggplot_build(i)) )
if (length(p) == 3){
  maxHeight = unit.pmax(grp[[1]]$heights[2:3], grp[[2]]$heights[2:3],  grp[[3]]$heights[2:3])
  for (i in length(grp)){
    grp[[i]]$heights[2:3] <- maxHeight
  }
  grid.arrange(grp[[1]],grp[[2]],grp[[3]],ncol = 3)
  if (SAVE == TRUE){
    pdf(file = paste("Distributions","pdf",sep="."), height = SIZE[1], width = SIZE[2])
    grid.arrange(grp[[1]],grp[[2]],grp[[3]], ncol = 3)
    dev.off()
  }
}
  if (length(p) == 2){
    maxHeight = unit.pmax(grp[[1]]$heights[2:3], grp[[2]]$heights[2:3])
    for (i in length(grp)){
      grp[[i]]$heights[2:3] <- maxHeight
    }
    grid.arrange(grp[[1]],grp[[2]],ncol = 2)
    if (SAVE == TRUE){
      pdf(file = paste("Distributions","pdf",sep="."), height = SIZE[1], width = SIZE[2])
      grid.arrange(grp[[1]],grp[[2]], ncol = 2)
      dev.off()
    }
  }
  if (length(p) == 1){
    p[[1]]
    if (SAVE == TRUE){
      ggsave(filename = paste("Distributions","pdf",sep="."), height = SIZE[1], width = SIZE[2])
    }
}
return(p)
}
