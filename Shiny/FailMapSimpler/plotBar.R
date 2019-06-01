plotBar <- function(Data,
                    FailData = c("Nominal","Kernel"),
                    FailCause = c("FLOOD","SCOUR","HURRICANE","OTHER"),
                    t = seq(1,3000,by=1),
                    dt = 1,
                    t.F = 100, beta = 1.75,
                    grad.out = 30,
                    DELTA = FALSE,
                    delta = 0, # absolute, not relative
                    hazSelect = 1, Tshift = 100,
                    hazCol = "black"){
  
  # if(!("labelsP" %in% ls())) source("../../Plotting/SetupEncoding.R")
  # if(!("my.kernel.interp" %in% ls())) source("my.kernel.interp.R")
  
  # set up colors and what data types will be plotted-----
  nCaus <- length(FailCause)
  colMax <- 0.1*(nCaus-1)
  colValues <- c(seq(0,colMax, by = 0.1),0.5,0.75,1)
  medCol <- apply(col2rgb(colorsP$Fail[FailCause]),MARGIN = 1, mean)
  medCol <- rgb(medCol[1]/255,medCol[2]/255,medCol[3]/255)
  if(is.na(medCol)) medCol <- "black"
  colGrad   <- c(colorsP$Fail[FailCause], medCol, hazCol, "black")
  names(colGrad)[(nCaus+1):length(colGrad)] <- c("Median","Delta","Nominal")

  toPlot <- FailData
  d <- as.character(delta)
  
  levels.x <- c("Kernel Delta","Kernel","Median Delta", "Median","Nominal Delta", "Nominal","Hazard Delta","Hazard")
  levels.x <- switch(as.character(DELTA),
                     "TRUE"  = levels.x[!grepl("Delta",levels.x)],
                     "FALSE" = levels.x[1:6])
  labels.x <- switch(as.character(DELTA),
                     "TRUE"  = levels.x,
                     "FALSE" = switch(as.character(abs(delta)>0.02),
                                      "TRUE" = levels.x,
                                      "FALSE" = sub(".*Delta","",levels.x)))
  names(labels.x) <- levels.x
  labels.x[c("Kernel","Median","Nominal")][!(c("Kernel","Median","Nominal") %in% FailData)] <- ""
  print(levels.x)
  print(labels.x)
  
  if(DELTA){ 
    toPlot <- c(toPlot, "Hazard")
    ylims <- c(-60,60)
    title <- "Change in annual collapse\nprobability given hazard change [%]"
  }
  else{ 
    ylims <- c(0,0.04)
    title <- "Probability of a Bridge\nCollapsing Over One Year"
    if(abs(delta)>0.02) toPlot<- c(toPlot, paste(toPlot,"Delta"))
  }
  
  
  # Calculations for kernel smoothed and data frame creation-----
  lims <- range(Data$T_FAIL_D_HECD_USGS, na.rm = T)
  if(any(is.na(lims) | is.infinite(lims))){
    lims <- c(0,2000)
  }
  t.int <- t[t<2*lims[2]]
  T.kernel.interp <- my.kernel.interp(Data$T_FAIL_D_HECD_USGS, 
                                      to = 2*lims[2], 
                                      t.interp = t.int)
  T.kernel.interp$y <- T.kernel.interp$y/sum(T.kernel.interp$y*dt)
  pF <- data.frame(x = c("Hazard",labelsP$Dist),
                   group = "none",
                   "T" = c(NA,
                           t.F,
                           median(Data$T_FAIL_D_HECD_USGS),
                           NA),
                   ymin = rep(0,4),
                   row.names = c("Hazard",labelsP$Dist))
  pF$ymax <- c(delta,
               1/t.F*pnorm(-beta), 
                1/pF["Median","T"], 
                NA)
  pF["Kernel","ymax"] <-sum(T.kernel.interp$y/(T.kernel.interp$x)^2*dt)
  # if(abs(delta)>0.02){
    pF[,d] <- c(delta,
                switch(as.character(hazSelect),
                       "1" = 1/(t.F*(1-delta))*pnorm(-beta),
                       "2" = 1/(t.F-Tshift*delta)*pnorm(-beta)),
                switch(as.character(hazSelect),
                       "1" = 1/(pF["Median","T"]*(1-delta)),
                       "2" = 1/(pF["Median","T"]-Tshift*delta)),
                switch(as.character(hazSelect),
                       "1" = sum(T.kernel.interp$y/(T.kernel.interp$x*(1-delta))^2*dt),
                       "2" = NA))
    if(is.na(pF["Kernel",d])){
      T.kernel.interp$y <- T.kernel.interp$y[T.kernel.interp$x-Tshift*delta > 0]
      T.kernel.interp$x <- T.kernel.interp$x[T.kernel.interp$x-Tshift*delta > 0]
      T.kernel.interp$y <- T.kernel.interp$y/sum(T.kernel.interp$y*dt)
      pF["Kernel",d] <- sum(T.kernel.interp$y/(T.kernel.interp$x-Tshift*delta)^2*dt)
    }
  # }
  # else{
  #   pF[,d] <- pF$ymax
  # }
  if(DELTA){
    pF$change <- (pF[,d] - pF$ymax)/pF$ymax*100
    pF["Hazard","change"] <- delta*100
    pF$label <- paste0(signif(pF$change,2),"%")
    pF$hjust <- ifelse(delta>=0,0,1)
    pF$order <- 1:4
    
    # make necessary changes if beyond axis limits
    pF$label[abs(pF$change)>60] <- paste0("(",pF$label[abs(pF$change)>60],")")
    pF$hjust[abs(pF$change)>60] <- abs(pF$hjust[abs(pF$change)>60]-1)
    pF$change[abs(pF$change)>60] <- sign(pF$change[abs(pF$change)>60])*60
    
    pF$ymax <- pF$change
    pF <- pF[,!(colnames(pF) %in% c("change","T"))]
  }
  else{
   
    pF$label <- as.character(signif(pF$ymax,2))
    pF$hjust <- 0
    pF$order <- c(2:4,1)
    pF <- pF[,!(colnames(pF) %in% c("change","T"))]
    }
 
  # Melt data frame for plotting with gradient
  pF.melt <- melt(pF[,colnames(pF)!=d], id.vars = c("x","group","label", "order","hjust"), value.name = "y")
  pF.melt <- pF.melt[order(pF.melt$order,decreasing = !DELTA ),]
  row.names(pF.melt) <- seq(1,nrow(pF.melt))

  k.col.rows <- grep("Kernel",pF.melt$x)
  k.col.rows <- k.col.rows[pF.melt[k.col.rows,"group"]=="none"][1]
  k.col.rows <- k.col.rows[1]:(k.col.rows[1]+grad.out-1)
  pF.melt[k.col.rows,] <- pF.melt[k.col.rows[2],]
  pF.melt[k.col.rows,"y"] <- seq(0,pF.melt[k.col.rows[1],"y"],length.out = grad.out) 
  pF.melt[pF.melt$x=="Kernel","variable"] <- "ymin"
  pF.melt[nrow(pF.melt),"variable"] <- "ymax"
  
  
  pF.melt[pF.melt$x=="Nominal","col"] <- 1
  pF.melt[pF.melt$x=="Median","col"] <- 0.5
  pF.melt[pF.melt$x=="Hazard","col"] <- 0.75
  pF.melt[pF.melt$x=="Kernel","col"] <- seq(0,colMax,length.out = grad.out)
  
  # if(!DELTA){
    pF$group <- "delta"
    pF$label <- as.character(signif(pF[,d],2))
    pF$x     <- paste(pF$x,"Delta")
    pF.melt2 <- melt(pF[,colnames(pF)!="ymax"], id.vars = c("x","group","label", "order","hjust"), value.name = "y")
    pF.melt2$col <- 0.75 # maps to color of hazard
    
    # print(pF.melt2)
    pF.melt <- rbind(pF.melt, pF.melt2)
    pF.melt[pF.melt$variable==d,"variable"] <- "ymax"
    
  # }
    pF.melt$x <- as.character(pF.melt$x)
   
    pF.melt <- pF.melt[pF.melt$x %in% levels.x,]
    pF.melt$x <- factor(pF.melt$x, levels = levels.x, labels = labels.x,ordered = T)
  # print(levels(pF$x))
  if(DELTA & abs(delta)<0.02) pF.melt$col <- 1
  pF.melt <- pF.melt[pF.melt$x %in% toPlot,]
  pF.melt[pF.melt$variable=="ymin","label"] <- NA
  print(pF.melt)
  # make plot
  pB <- ggplot(data = pF.melt, aes(x=x,y=y, color = col)) + 
    geom_path(size = 4) + scale_x_discrete(limits = levels.x, drop = FALSE, labels = labels.x) +
    scale_color_gradientn(colours = colGrad, values = colValues, guide = FALSE) + ggtitle(title) +
    ylim(ylims)+
    geom_text(aes(y = y, label = label, hjust = hjust), size = textP$annotate["SHINY"]) +
    getTheme(outputType = "SHINY") + coord_flip()+
    theme(axis.text.y = element_text(size = textP$sub["SHINY"]),#14),#
          axis.text.x   = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank())
  if(DELTA){
    pB <- pB +  geom_hline(yintercept  = 0, color = "black") 
    if(delta!=0){
      pB <- pB + geom_hline(yintercept = delta*100, color = hazCol, linetype = "dashed") 
    }
  }
  pB
}