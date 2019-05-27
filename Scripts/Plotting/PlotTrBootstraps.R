# Panel of 3 plots for bootstraps of T_R, 3 distributions of T_R from fits with confidence interval for HEC
# and same distribution from USGS, Daymet-VICg/b

PlotTrBootstraps <- function(BridgesDataFrame, paramsBoot, pExc,Dists, SAVE = FALSE ,labelsType = "yearsState", 
                             plotTypes = c("LP3boot","TrAllDists","TrAllData"), outputType = "PRINT",
                             SIZE = c(4,13),AREA_RATIO=TRUE,HURR_LAB="*"){
  require(ggplot2)
  require(grid)
  require(gridExtra)
  require(plyr)
  require(reshape2)
  require(nsRFA)
  
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
  
  # CLEAN UP FACTORS
  BridgesDataFrame$FAIL_CAUS_CODE  <- as.character(BridgesDataFrame$FAIL_CAUS_CODE)
  BridgesDataFrame$FAIL_CAUS_CODE  <- factor(BridgesDataFrame$FAIL_CAUS_CODE, levels=c("flood","scour","hurricane","other hydraulic"), labels = labelsP$Fail)
  
  BridgesDataFrame$BOOL_KNOWN_FAIL_DATE <- factor(BridgesDataFrame$BOOL_HAS_FAIL_DATE, levels = c(TRUE, FALSE), labels = labelsP$Date)
  
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
  
  # labs[!labs==""] <- paste("^'",labs[!labs==""],"'",sep = "")
  BridgesDataFrame$LABEL <- paste(BridgesDataFrame$LABEL,labs,sep=" - ")
  
  p <- list()
  
  # USGS LP3 BOOTSTRAPS -----------
  if("LP3boot" %in% plotTypes){
    Qrange   <- c(min(1,BridgesDataFrame$Q_FAIL_D_USGS),2*BridgesDataFrame$Q_MAX_D_USGS)
    Q        <- seq(Qrange[1],Qrange[2],length.out = 500)
    # Boots will be a matrix, of length(Q)xnSamples
    Boots    <- sapply(as.character(1:length(paramsBoot[["LP3"]])), function(i) 1/(1-F.gamma(log(Q), paramsBoot[["LP3"]][[i]]$ml[1],paramsBoot[["LP3"]][[i]]$ml[2],paramsBoot[["LP3"]][[i]]$ml[3])))
    df.Boots <- as.data.frame(Boots)
    df.Boots$Q <- Q
    colnames(df.Boots)[1:length(paramsBoot[["LP3"]])] <- paste("s",c(1:length(paramsBoot[["LP3"]])))
    df.Boots.melt <- melt(df.Boots,id.vars = "Q",variable.name="GROUP",value.name = "f_Q")
    
    TR_DISTparams <- unlist(Dists$TRbootLP3)
    vAxisLims     <- c(7,28)
    df.vert       <- data.frame(Q   = c(BridgesDataFrame$Q_FAIL_D_USGS,BridgesDataFrame$Q_FAIL_D_USGS),
                                f_Q = vAxisLims,
                                GROUP = c(1,1))
    d <- density(sort(1/unlist(pExc[["LP3"]])))
    f_Q_Tr <- seq(vAxisLims[1],vAxisLims[2],length.out = 1000)
    Q_Tr      <- BridgesDataFrame$Q_FAIL_D_USGS - 500*approx(d$x,d$y,f_Q_Tr)$y
    df.pdf        <- data.frame(Q = Q_Tr,
                                f_Q = f_Q_Tr,
                                # GROUP = c(rep(1,350),rep(2,100),rep(3,100),rep(4,100),rep(5,350)))
                                GROUP = 1)
    # return(df.pdf)

    pLP3boot <- ggplot(data=df.Boots.melt,aes(x = Q, y = f_Q, group = GROUP))
    pLP3boot <- pLP3boot + 
                geom_line(size = 0.1, alpha = 0.1, color = colorsP$Dists["LP3"], linetype = linesP$Dists["LP3"]) +
                geom_line(data = df.vert, color = "black", size = 0.5) +
                geom_line(data = df.pdf, color = "black", size = 0.5) +
                annotate("text",
                         x = BridgesDataFrame$Q_FAIL_D_USGS,
                         y = 6,
                         label = "f[T[R]](Q[fail])",
                         size = 3,
                         parse = TRUE) +
      
      labs(x = "Flow, Q [cfs]",y="Return Period of Q, T_R(Q), USGS-LP3 Bootstraps [years]",title=BridgesDataFrame$LABEL) +
      ylim(c(0,35))+
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
    p[[1]] <- pLP3boot
  }
 
# LP3 VS GEV FOR USGS, WITH HEC CONFIDENCE INTERVAL
if ("TrAllDists" %in% plotTypes){
  TRrange  <- c(BridgesDataFrame$T_FAIL_LP3_USGS_99, 1/BridgesDataFrame$T_FAIL_D_05_HECP_USGS)
  TR       <- seq(TRrange[1],TRrange[2],length.out = 500)
  
  d  <- density(sort(1/unlist(pExc[["LP3"]])))
  dG <- density(sort(1/unlist(pExc[["GEV"]])))
  df <- data.frame(TR = TR,
                   LP3 = approx(d$x,d$y,TR)$y,
                   GEV = approx(dG$x,dG$y,TR)$y
                  )
  df.melt     <- melt(df,id.vars = "TR", value.name = "f_TR", variable.name = "GROUP")
  
  f_HEC <- 0.001
  df2 <- data.frame(TR   = c(BridgesDataFrame$T_FAIL_D_HECP_USGS, BridgesDataFrame$T_FAIL_DAILY_LP3_USGS, BridgesDataFrame$T_FAIL_DAILY_GEV_USGS),
                    f_TR = c(f_HEC, approx(d$x,d$y,BridgesDataFrame$T_FAIL_DAILY_LP3_USGS)$y, approx(dG$x,dG$y,BridgesDataFrame$T_FAIL_DAILY_GEV_USGS)$y),
                    # f_TR = c(0.1,0.07,0.04),
                    GROUP = c("HEC","LP3","GEV"))
  
  df3 <- data.frame(TR = c( 1/BridgesDataFrame$T_FAIL_D_95_HECP_USGS, 1/BridgesDataFrame$T_FAIL_D_05_HECP_USGS),
#                             BridgesDataFrame$T_FAIL_LP3_USGS_95, BridgesDataFrame$T_FAIL_LP3_USGS_05,
#                             BridgesDataFrame$T_FAIL_GEV_USGS_95, BridgesDataFrame$T_FAIL_GEV_USGS_05),
                    f_TR = c(f_HEC, f_HEC), #rep(c(0.1,0.07,0.04),each=2),
                    GROUP = c("HEC","HEC"))#rep(c("HEC","LP3","GEV"),each =2) )
  
  pTrAllDists <- ggplot(df.melt,aes(x=TR,y=f_TR,group=GROUP)) +
                  geom_line(aes(color=GROUP, linetype = GROUP), size = 0.5) +
    
               labs(x=expression(paste("Return period of failure, ",T[R](Q[fail]),"[years]")),y="Probability density of failure return period, f_T_R(Q_fail)",
                    title=BridgesDataFrame$LABEL) +
    xlim(c(0,35))+
    scale_color_manual(values = colorsP$Dists, name = legendsP$Dists) +
    scale_linetype_manual(values = linesP$Dists) +
    geom_point(data=df2,aes(color=GROUP),shape=shapesP$Failure) +
    geom_line(data=df3, aes(color=GROUP,linetype=GROUP),size=0.5)+
    
    guides(color = guide_legend(order = 1,
                                override.aes = list(linetype=linesP$Dists[c("GEV","HEC","LP3")],
                                                    shape = shapesP$Failure)),
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
  p[[2]] <- pTrAllDists
}
  # GRID ARRANGE
  grp <- lapply(p, function(i) ggplot_gtable(ggplot_build(i)) )
  
if (length(plotTypes) == 2){
      maxHeight = unit.pmax(grp[[1]]$heights[2:3], grp[[2]]$heights[2:3])
    for (i in length(grp)){
      grp[[i]]$heights[2:3] <- maxHeight
    }
    grid.arrange(grp[[1]],grp[[2]],ncol = 2)
    if (SAVE == TRUE){
      pdf(file = paste("Bootstraps",paste(plotTypes,collapse = ""),"pdf",sep="."), height = SIZE[1], width = SIZE[2])
      grid.arrange(grp[[1]],grp[[2]], ncol = 2)
      dev.off()
      }
    }
  #   else{ #only 1
#   p[[1]]
#   if (SAVE == TRUE){
#     ggsave(filename = paste("Bootstraps",paste(plotTypes,collapse = ""),"pdf",sep="."), height = SIZE[1], width = SIZE[2])
#   }
  #   }
  # }
  return(p)
}

