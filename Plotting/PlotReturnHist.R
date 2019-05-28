# Histogram figure based on data from "Historical Analysis of Hydraulic Bridge Collapses 
# in the Continental United States". Plot not present in original paper.
# Copyright Madeleine Flint, 2019

PlotReturnHist <- function(BridgesDataFrame, SAVE = FALSE, plotTypes = "D",
                           LEGEND = TRUE, outputType = "PRES", SIZE = c(4,5), 
                           breaks = c(100,500,1000,1500)){
  require(ggplot2)
  require(grid)
  require(gridExtra)
  require(plyr)
  require(reshape2)
  if(!("colorsP" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","SetupEncoding.R"))
  if(!("getTheme" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","getTheme.R"))

  p <- list()
  pTheme <- getTheme(outputType, FALSE) +
    theme(axis.text.x   = element_text(color = "black", size = textP$reg[outputType], 
                                       angle = 90, vjust = 0.5, hjust = 1, margin  = margin(0.16,0,0,0, "cm")))
  for(i in 1:length(plotTypes)){
    pT <-plotTypes[i]
    T.col <- c(D="T_FAIL_D_HECD_USGS", IP="T_FAIL_IP_HECP_USGS")[pT]
    BridgesDataFrame$T <- BridgesDataFrame[,T.col]
    p[[pT]] <- ggplot(BridgesDataFrame, aes(T,fill=FAIL_CAUS_CODE)) + geom_histogram()+
      scale_fill_manual(values = colorsP$Fail,       name = "COLLAPSE CAUSE") + scale_x_continuous(breaks = breaks) +
      scale_y_continuous(breaks = c(5,10,15),limits=c(0,18)) +
      labs(title = "Bulletin 17B Analysis Using Daily Mean Flows", x = expression(paste(T["R"]," [years]")), y = "") 
    if(LEGEND){
      p[[pT]] <- p[[pT]] + guides(fill= guide_legend(override.aes = list(shape=15,size = 4, linetype = "blank")))
    }
    else{
      p[[pT]] <- p[[pT]] + guides(fill=FALSE)
    }
    p[[pT]] <- p[[pT]] + pTheme 
    if (SAVE){
      filename <- file.path("Plots",paste("TrHist",pT,"pdf",sep="."))
      ggsave(filename = filename,p[[pT]], width = SIZE[2], height = SIZE[1])
      }
  }
  return(p)
}
