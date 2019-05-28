# Plot T1 vs T2 with 1:1 line and linear regression in both linear and log scale
# Madeleine Flint, 2015-09-02
# major updating on 2015-09-09 to improve visualization

PlotT1T2corrRegress <- function(df,T1l,T2l,ls.corrs, TEXT = c("LRlog","LR","Rho","Tau"), 
                                ALPHA_VAR = "BOOL_KNOWN_FAIL_DATE", REG = TRUE,
                                SHAPE_VAR = NA, FIT = TRUE, dataType = "T",
                                AXES = "LINEAR",  LEGEND = FALSE, 
                                outputType = "PRINT", ANNOTATE_LOC = "BR", 
                                SCALE_CORRECTION = 1, JITTER = FALSE, VERBOSE = FALSE){
  # input data df: T1, T2, SCALE, ALPHA, 
  # labels T1l and T2l and list of correlation and regression coefficients
  # data, ls.corrs
  
  require(ggplot2)
  if(!("colorsP" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","SetupEncoding.R"))
  if(!("PlaceAnnotation" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","PlaceAnnotation.R"))

  # SETUP PLOT -----------------
  TypeX <- labelsP$Data[sapply(levelsP$Data,grepl,T1l,ignore.case=TRUE)][1]
  if(length(TypeX) == 0 | is.na(TypeX)) TypeX <- "USGS"
  TypeY <- labelsP$Data[sapply(levelsP$Data,grepl,T2l,ignore.case=TRUE)][1]
  if(length(TypeY) == 0 | is.na(TypeY)) TypeY <- "USGS"
  if(VERBOSE==TRUE) { 
    print('-------')
    print(paste(T1l,TypeX,T2l,TypeY))
  }
  if (length(TEXT) >=1){
    PearsonRho     <- ls.corrs[[names(ls.corrs)[grepl("P\\>",names(ls.corrs))]]]$estimate
    PearsonRhoPval <- ls.corrs[[names(ls.corrs)[grepl("P\\>",names(ls.corrs))]]]$p.value
    if(PearsonRhoPval < 5e-4){
      PearsonRhoPval <- ls.corrs[[names(ls.corrs)[grepl("Papprox\\>",names(ls.corrs))]]]$p.value
      Papprox        <- TRUE
      if(VERBOSE==TRUE)  print("using approximate p for Pearson-Rho")
    }
    else Papprox <- FALSE
    Pzero          <- ifelse(PearsonRhoPval < 5e-4,TRUE,FALSE)
    KendallTau     <- ls.corrs[[names(ls.corrs)[grepl("K\\>",names(ls.corrs))]]]$estimate
    KendallTauPval <- ls.corrs[[names(ls.corrs)[grepl("K\\>",names(ls.corrs))]]]$p.value
    if(KendallTauPval < 5e-4){
      KendallTauPval <- ls.corrs[[names(ls.corrs)[grepl("Kapprox\\>",names(ls.corrs))]]]$p.value
      Kapprox        <- TRUE
      if(VERBOSE==TRUE) print("using approximate p for Kendall-Tau")
      
    }
    else Kapprox <- FALSE
    Kzero <- ifelse(KendallTauPval < 5e-4,TRUE,FALSE)
  }
  
  if(FIT){
    model       <- lm(T2 ~ T1, data = df)
    LRslope     <- model$coefficients[2]
    LRintercept <- model$coefficients[1]
    LRr2        <- summary(model)$r.squared
    
    model.log       <- lm(log10(T2) ~ log10(T1), data = df)
    LRslope.log     <- model.log$coefficients[2]
    LRintercept.log <- model.log$coefficients[1]
    LRr2.log        <- summary(model.log)$r.squared
  }
  
  # data
  inBoth   <- !is.na(df$T1) & !is.na(df$T2)
  roundVal <- 10
  limits   <- c(min(c(df$T1[inBoth]),df$T2[inBoth]),max(c(df$T1[inBoth],df$T2[inBoth])))
  if (AXES == "LINEAR") limits[1] <- min(abs(floor(limits[1]/roundVal)*roundVal - roundVal),5)
  if(AXES == "LOG" & limits[1]<=0) limits[1] <- 0.01
  limits[2] <- ceiling(limits[2]/roundVal)*roundVal+roundVal
  if(VERBOSE==TRUE) print(limits)
  
  breaks <- c(0.001,0.01,0.1,1,10,100,1000,10000,100000,1000000,10000000)
  breaks <- breaks[breaks>=limits[1] & breaks<=limits[2]]
  if(VERBOSE==TRUE) print(breaks)
  if(breaks[1]==10 & limits[1]<=5) {
    breaks <- c(1,breaks)
    limits[1] <- 0.99
  }
  if(breaks[length(breaks)]==100 & limits[2]>550){ 
    breaks[length(breaks)+1] <- 1000
    limits[2] <- 1001
  }
  if(breaks[1]==10000 & limits[1] < 8000){
    breaks <- c(1000,breaks)
    limits[1] <- 999.9
  }
  if(breaks[1]==100 & limits[1]<80){
    breaks <- c(10,breaks)
    limits[1] <- 9.999
  }
  breakLabels <- format(breaks,scientific = FALSE, drop0trailing = TRUE)
  if (VERBOSE) print(breakLabels)
  
  # set up fake dodge for labeling of small points
  if(JITTER == TRUE){
    smallAreas <- df$SCALE < 0.05*max(df$SCALE)
    df3 <- df
    df3$JITTER <- FALSE
    df3[smallAreas,"JITTER"] <- TRUE
    smallAreaRows <- which(smallAreas)
    df3[smallAreas,"T1"] <- df3[smallAreas,"T1"] - log10(df3[smallAreas,"T1"])^3 * abs(df3[smallAreas,"T1"] - exp(jitter(log(df3[smallAreas,"T1"]))))
    df3[smallAreas,"T2"] <- df3[smallAreas,"T2"] + log10(df3[smallAreas,"T2"])^2 * abs(df3[smallAreas,"T2"] - exp(jitter(log(df3[smallAreas,"T2"]))))
  }
  else{ 
    df3 <- df
    df3$JITTER <- FALSE
    smallAreas <- df$SCALE < 0.05*max(df$SCALE)
    df3$SMALL <- FALSE
    df3[smallAreas,"SMALL"] <- TRUE
  }
  
  # MAKE PLOT ----------------
  p <- ggplot(df,aes(T1,T2)) 
  p <- p +
    geom_abline(intercept = 0, slope = 1, color = "gray", alpha = 0.5, size = 0.25)
  if(is.na(SHAPE_VAR)){
    p <- p + geom_point(aes(color = FAIL_CAUS_CODE, size = SCALE, alpha = ALPHA_VAR),
                        shape = shapesP$Qtype[sapply(substr(names(shapesP$Qtype),1,3), function(i) any(grepl(i,T1l,ignore.case=TRUE)))][1],fill="lightgray")
    }
  else {
    p <- p + geom_point(aes(color = FAIL_CAUS_CODE, alpha = ALPHA_VAR, size = SCALE, shape = SHAPE_VAR), 
                        stroke = 1.15)
    
  }
  
  if(REG){
    if (any(df3$JITTER)){
      p <- p +
        geom_point(data = subset(df3, JITTER == TRUE  & BOOL_REGULATION == "REGULATED"),shape = "R",color="black",size = sizesP$Reg[outputType]) +
        geom_point(data = subset(df3, JITTER == TRUE  & BOOL_REGULATION == "UNREGULATED"),shape = "U",color="black",size = sizesP$Reg[outputType]) +
        geom_point(data = subset(df3, JITTER == TRUE  & BOOL_REGULATION == "UNKNOWN"),shape = "N",color="black",size = sizesP$Reg[outputType]) +
        geom_point(data = subset(df3, JITTER == FALSE & BOOL_REGULATION == "REGULATED"),shape = "R",color=colorsP$Reg[sapply(c("COLLAPSE","MAX","PARTIAL"), function(i) any(grepl(i,T1l)))][1],size = sizesP$Reg[outputType])+
        geom_point(data = subset(df3, JITTER == FALSE & BOOL_REGULATION == "UNREGULATED"),shape = "U",color=colorsP$Reg[sapply(c("COLLAPSE","MAX","PARTIAL"), function(i) any(grepl(i,T1l)))][1],size = sizesP$Reg[outputType])+
        geom_point(data = subset(df3, JITTER == FALSE & BOOL_REGULATION == "UNKNOWN"),shape = "N",color=colorsP$Reg[sapply(c("COLLAPSE","MAX","PARTIAL"), function(i) any(grepl(i,T1l)))][1],size = sizesP$Reg[outputType])
    }
    else{
      if(VERBOSE==TRUE) print("defaulting to non-jitter")
      p <- p + 
        geom_point(data = subset(df3, SMALL == TRUE  & BOOL_REGULATION == "REGULATED"),shape = "R",color="black",size = sizesP$Reg[outputType])+
        geom_point(data = subset(df3, SMALL == TRUE  & BOOL_REGULATION == "UNREGULATED"),shape = "U",color="black",size = sizesP$Reg[outputType])+
        geom_point(data = subset(df3, SMALL == TRUE  & BOOL_REGULATION == "UNKNOWN"),shape = "N",color="black",size = sizesP$Reg[outputType])
      if (!is.na(SHAPE_VAR)){
        p <- p +
          geom_point(data = subset(df3, SMALL == FALSE & BOOL_REGULATION == "REGULATED"   & SHAPE_VAR == "UNKNOWN"),shape = "R",color="black",size = sizesP$Reg[outputType]) +
          geom_point(data = subset(df3, SMALL == FALSE & BOOL_REGULATION == "UNREGULATED" & SHAPE_VAR == "UNKNOWN"),shape = "U",color="black",size = sizesP$Reg[outputType]) +
          geom_point(data = subset(df3, SMALL == FALSE & BOOL_REGULATION == "UNKNOWN"     & SHAPE_VAR == "UNKNOWN"),shape = "N",color="black",size = sizesP$Reg[outputType]) +
          geom_point(data = subset(df3, SMALL == FALSE & BOOL_REGULATION == "REGULATED"   & SHAPE_VAR == "KNOWN"),shape = "R",color="white",size = sizesP$Reg[outputType]) +
          geom_point(data = subset(df3, SMALL == FALSE & BOOL_REGULATION == "UNREGULATED" & SHAPE_VAR == "KNOWN"),shape = "U",color="white",size = sizesP$Reg[outputType]) +
          geom_point(data = subset(df3, SMALL == FALSE & BOOL_REGULATION == "UNKNOWN"     & SHAPE_VAR == "NOWN"),shape = "N",color="white",size = sizesP$Reg[outputType])
      }
      else{ # SHAPE_VAR is NA
        p <- p +
          geom_point(data = subset(df3, SMALL == FALSE & BOOL_REGULATION == "REGULATED"),shape = "R",color="white",size = sizesP$Reg[outputType]) +
          geom_point(data = subset(df3, SMALL == FALSE & BOOL_REGULATION == "UNREGULATED"),shape = "U",color="white",size = sizesP$Reg[outputType]) +
          geom_point(data = subset(df3, SMALL == FALSE & BOOL_REGULATION == "UNKNOWN"),shape = "N",color="white",size = sizesP$Reg[outputType])
      }
    }
  }
  
  if (AXES == "LINEAR"){
    p <- p + coord_equal(xlim = limits, ylim = limits)
    if(FIT){
      p <- p + geom_abline(intercept = LRintercept, slope = LRslope, color = "black",size=0.25)
    }
  }
  if (AXES == "LOG" | T2l == "T_PARTIAL"){
    p <- p + scale_x_log10(limits = limits, breaks=breaks, labels = breakLabels) + scale_y_log10(limits = limits, breaks=breaks, labels = breakLabels)
      if(FIT){
        p <- p + geom_abline(intercept = LRintercept.log, slope = LRslope.log, color = "black",size=0.25)
      }
  }

  p <- p   +
    xlab(T1l) + ylab(T2l) +

    scale_color_manual(values = colorsP$Fail,     name = legendsP$Fail) #+
  if (!is.na(SHAPE_VAR)) p <- p + scale_shape_manual(values = shapesP$Date, name = legendsP$Date)
  if (min(df$SCALE)!=max(df$SCALE)){ # if size scaling is used
    sizeLimits <- c(min(df[inBoth,"SCALE"]),max(df[inBoth,"SCALE"]))
    if(VERBOSE) print(sizeLimits)
    p <- p +
      scale_size(range = c(sizesP$FailCorrMin[outputType],SCALE_CORRECTION*sizesP$FailCorrMax[outputType]),
                 limits = c(min(df[inBoth,"SCALE"]),max(df[inBoth,"SCALE"])))
    if(VERBOSE==TRUE) print(paste("scaling max by factor",SCALE_CORRECTION,
                                  "to obtain max point size of",SCALE_CORRECTION*sizesP$FailCorrMax[outputType]))
  }
  else{ # if size scaling is not used
    p <- p + scale_size(max_size = 0.4*sizesP$FailCorrMax[outputType])
    if(VERBOSE==TRUE) print(paste("scale all 1 with max point size", sizesP$FailCorrMax[outputType]))
  }
  alphas <- switch(ALPHA_VAR,
                   BOOL_KNOWN_FAIL_DATE =  alphasP$FailDate,
                   FROM_INST = alphasP$INST,
                   BOOL_REGULATION = alphasP$Reg,
                   FAIL_IS_MAX = alphasP$FAIL_IS_MAX,
                   ALPHA = 0.9)
  alphaLeg <- switch(ALPHA_VAR,
                     BOOL_KNOWN_FAIL_DATE =  legendsP$Date,
                     FROM_INST = legendsP$INST,
                     BOOL_REGULATION = legendsP$Reg,
                     FAIL_IS_MAX = legendsP$FAIL_IS_MAX,
                     ALPHA = "")

  p <- p + scale_alpha_manual(values = alphas, name = alphaLeg)

  # add text annotations for statistics of correlation and linear regression
  if (length(TEXT) >=1){
    if(ANNOTATE_LOC == "BR" & any(TEXT == "LRlog")){
      TEXT[TEXT=="LRlog"] <- "LRlog1"
      TEXT <- c(TEXT[1:which(TEXT=="LRlog1")],"LRlog2",TEXT[(which(TEXT=="LRlog1")+1):length(TEXT)])
    }
    nRows <- length(TEXT)
    INDENT <- rep(FALSE,nRows)
    if(ANNOTATE_LOC == "BR") INDENT[TEXT=="LRlog1"] <- TRUE

    if(SCALE=="LOG"){
      df.text <- PlaceAnnotation(limits[1],limits[2],nRows, ANNOTATE_LOC = ANNOTATE_LOC, SCALE = SCALE, INDENT = INDENT)
    }
    if(SCALE=="LINEAR"){
      df.text <- PlaceAnnotation(limits[1],limits[2],nRows, ANNOTATE_LOC = ANNOTATE_LOC, SCALE = SCALE, INDENT = INDENT)
    }
    text <- character()
    for (i in TEXT){
      text[i] <- switch(i,
                        LRlog  = paste("log[10](",dataType,"2) == ",signif(LRslope.log,3),"*log[10](",dataType,"1) + ",signif(LRintercept.log,3),"~(R^{2} ==",round(LRr2.log,3),")",sep=""),
                        LRlog1 = paste("log[10](",dataType,"2) == ",signif(LRslope.log,3),"*log[10](",dataType,"1)",sep=""),
                        LRlog2 = paste("+ ",signif(LRintercept.log,3),"~(R^{2} ==",round(LRr2.log,3),")",sep=""),
                        LR  = paste(dataType,"2 == ",signif(LRslope,3),"*",dataType,"1 + ",signif(LRintercept,3),"~(R^{2} ==",round(LRr2,3),")",sep=""),
                        Rho = ifelse(!Papprox,
                                     paste("rho == ",signif(PearsonRho,3),"~(p == ",round(PearsonRhoPval,3),")",sep=""),
                                     ifelse(!Pzero,
                                            paste("rho == ",signif(PearsonRho,3),"~(p%=~%",round(PearsonRhoPval,3),")",sep=""),
                                            paste("rho == ",signif(PearsonRho,3),"~(p < 5e-4)",sep=""))
                        ),
                        Tau = ifelse(!Kapprox,
                                     paste("tau == ",signif(KendallTau,3),"~(p == ",round(KendallTauPval,3),")",sep=""),
                                     ifelse(!Kzero,
                                            paste("tau == ",signif(KendallTau,3),"~(p%=~%",round(PearsonRhoPval,3),")",sep=""),
                                            paste("tau == ",signif(KendallTau,3),"~(p<5e-4)",sep=""))
                        ),
                        m  = paste("m == ",signif(LRslope,3),"~(R^{2} ==",round(LRr2,3),")",sep=""),
                        mLog  = paste("m(log[10]-log[10]) == ",signif(LRslope.log,3),"~(R^{2} ==",round(LRr2.log,3),")",sep="")
      )
    }

    p <- p +
      annotate("text",
               x     = df.text$x,
               y     = df.text$y,
               label = text,
               color = "black",
               size   = textP$annotate[outputType],
               hjust = df.text$hjust,
               parse = TRUE
      )
  }

  if (LEGEND == TRUE){
    p <- p + guides(shape = guide_legend(order = 1,
                                         override.aes = list(color="black")),
                    alpha = guide_legend(order = 3,
                                         override.aes = list(size = 1*sizesP$FailCorr[outputType]))
    )
    if (min(df$SCALE)==max(df$SCALE)){
      p <- p + guides(size = FALSE)
    }
  }
  if (LEGEND == FALSE){
    p <- p +
      guides(color = FALSE,
             alpha = FALSE,
             shape = FALSE,
             size = FALSE
      )
  }
  
  p <- p + getTheme(outputType, FALSE) +
    theme(panel.background   = element_rect(fill = "white", color = "gray",size=0.5),
          plot.margin   = unit(c(0,0.05,0.1,0.1), "in"),
          axis.line = element_blank(),
          axis.text.x  = element_text(color = "black", size = textP$reg[outputType], margin = margin(0.16, unit = "cm")),
          axis.title.x = element_text(color = colorsP$Data[TypeX], size = textP$sub[outputType]),
          axis.text.y  = element_text(color = "black", size = textP$reg[outputType], margin = margin(r=0.16, unit = "cm")),
          axis.title.y = element_text(color = colorsP$Data[TypeY], size = textP$sub[outputType], margin = margin(r=0.05, unit = "cm"))
    )
  
  return(p)
}
