# Make individual panels of correlations
# Copyright Madeleine Flint, 2016

PlotAllCorrs  <- function(BridgesDataFrame, ls.corrs, TYPE = "USGS", SAVE = FALSE, SCREEN = FALSE, TEXT = c("LRlog","LR","Rho","Tau"), ONLY = NULL, ANY=NULL, NOT=NULL,
                          ALPHA_VAR = "BOOL_KNOWN_FAIL_DATE", AXES = "LINEAR", LEGEND = "NONE", ANNOTATE_LOC = "BR", SCALE_VAR = "DRAIN_SQKM", SHAPE_VAR = NA,
                          outputType = "PRINT", SIZE = c(7,11), SCALE_CORRECTION = 1, JITTER = FALSE, Q_units = "m3s"){
require(ggplot2)
# require(grid)
require(gridExtra)
source(file.path(dirsGit$Scripts,"Helper","Bases.R"))
source(file.path(dirsGit$ScriptsPlot,"PlotT1T2corrRegress_RAPID.R"))
source(file.path(dirsGit$ScriptsPlot,"SetupEncoding.R"))

  if(is.na(ALPHA_VAR)){
    ALPHA_VAR <- "ALPHA"
    BridgesDataFrame$ALPHA <- TRUE
  }
  BridgesDataFrame$BOOL_REGULATION      <- factor(BridgesDataFrame$BOOL_REGULATION, levels = c(TRUE,FALSE, NA), labels = labelsP$Reg, exclude = NULL)
  if (ALPHA_VAR == "FROM_INST"){
    BridgesDataFrame$FROM_INST <- BridgesDataFrame$T_FAIL_BEST_HEC_SOURCE == "INST"
  }
if (ALPHA_VAR == "FAIL_IS_MAX"){
  BridgesDataFrame$FAIL_IS_MAX <- BridgesDataFrame$Q_FAIL_D_USGS == BridgesDataFrame$Q_MAX_D_USGS
}

ls.Bases  <- Bases(TYPE)
# print(ls.Bases)
bases     <- ls.Bases$bases
T1ls      <- ls.Bases$T1ls
T2ls      <- ls.Bases$T2ls
# print(bases)
# print(T1ls)
# print(T1ls)
ls.corrs.all <- sapply(bases, function(base) ls.corrs[[TYPE]][names(ls.corrs[[TYPE]])[grepl(paste("\\<",base,"[A-Za-z]{1,7}\\>",sep=""),names(ls.corrs[[TYPE]]))]],
                       simplify = FALSE,
                       USE.NAMES = TRUE)
# print(head(ls.corrs.all))
kgCfs          <- 0.035315*(1/3600)

  # MAKE PLOTS -------------------------------------------------------------------
    # if ((!any(is.na(ONLY)) & !any(is.null(ONLY))) | (!any(is.na(ANY)) & !any(is.null(ANY))) | (!any(is.na(NOT) & !any(is.null(NOT))))){
    #   if (!any(is.na(ONLY)) & !any(is.null(ONLY))){
    #     for(only in ONLY){
    #       T1ls <- T1ls[grepl(only,bases)]
    #       T2ls <- T2ls[grepl(only,bases)]
    #       bases <- bases[grepl(only,bases)]
    #       
    #     }
    #   }
    #   if (!any(is.na(ANY)) & !any(is.null(ANY))){
    #   anyFlag <- rep(FALSE,length(bases))
    #   for(any in ANY){
    #     anyFlag_i <- grepl(any,bases)
    #     anyFlag <- anyFlag | anyFlag_i
    #   }
    #   bases <- bases[anyFlag]
    #   T1ls  <- T1ls[anyFlag]
    #   T2ls  <- T2ls[anyFlag]
    #   }
    #   if (!any(is.na(NOT)) & !any(is.null(NOT))){
    #     for (not in NOT){
    #       T1ls  <- T1ls[!grepl(not,bases)]
    #       T2ls  <- T2ls[!grepl(not,bases)]
    #       bases <- bases[!grepl(not,bases)]
    #     }
    #     
    #   }
    # }
    ls.corrs.all <- ls.corrs.all[bases]
    # print(bases)
    p     <- list()
    grp   <- list()
    for(i in 1:length(bases)){
      ls.corrs        <- ls.corrs.all[[i]]
      # print(head(ls.corrs))
      T1              <- BridgesDataFrame[,T1ls[i]]
      # print(T1)
      T2              <- BridgesDataFrame[,T2ls[i]]
      # Correct for bridge drainage area if using NLDAS data
      if (grepl("NLDASB",T1ls[i],ignore.case = TRUE) & 
          grepl("Q_",T1ls[i])){ 
        T1 <- T1*kgCfs*BridgesDataFrame$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM*1000^2
      }
      if (grepl("NLDASB",T2ls[i],ignore.case = TRUE) & 
          grepl("Q_",T2ls[i])){ 
        T2 <- T2*kgCfs*BridgesDataFrame$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM*1000^2
      }
      # Correct for gauge drainage area if using NLDAS data
      if (grepl("NLDASG",T1ls[i],ignore.case = TRUE) & 
          grepl("Q_",T1ls[i])){
        T1 <- T1*kgCfs*BridgesDataFrame$DRAIN_SQKM*1000^2
      }
      if (grepl("NLDASG",T2ls[i],ignore.case = TRUE) & 
          grepl("Q_",T2ls[i])){
        T2 <- T2*kgCfs*BridgesDataFrame$DRAIN_SQKM*1000^2
      }
      df <- data.frame(T1, T2)
      df[,c("FAIL_CAUS_CODE",ALPHA_VAR,"BOOL_REGULATION")] <- BridgesDataFrame[,c("FAIL_CAUS_CODE",ALPHA_VAR,"BOOL_REGULATION")]
      colnames(df)[c(1:2,4)] <- c("T1", "T2","ALPHA_VAR")
      if (!is.na(SHAPE_VAR)){
        df$SHAPE_VAR <- BridgesDataFrame[,SHAPE_VAR]
      }
      df$SCALE <- ifelse(is.na(SCALE_VAR), 1, BridgesDataFrame[,SCALE_VAR])
      if (LEGEND == "ALL" | (i == length(bases) & LEGEND == "LAST")) LEGEND_i <- TRUE
      else LEGEND_i <- FALSE
      
      T1l   <- factor(T1ls[i], levels = levelsP$Corrs, labels = labelsP$Corrs)
      T2l   <- factor(T2ls[i], levels = levelsP$Corrs, labels = labelsP$Corrs)
      if (TYPE == "MAXQ" | TYPE == "FAILQ"){
        dataType <- "Q"
        if (Q_units=="m3s"){
          cfs2m3s <- 0.0283168
          df$T1 <- df$T1*cfs2m3s
          df$T2 <- df$T2*cfs2m3s
          
          T1l   <-  sub(" [cfs]","", T1l, fixed = T)
          T1l   <- paste(T1l, "[")
          T1l   <- bquote(.(T1l)*m^3/s*"], Q1")
          
          T2l   <-  sub(" [cfs]","", T2l, fixed = T)
          T2l   <- paste(T2l, "[")
          T2l   <- bquote(.(T2l)*m^3/s*"], Q2")
        }
        else{ # units must be cfs
          T1l <- paste(T1l, ", Q1", sep="")
          T2l <- paste(T2l, ", Q2", sep="")
        }
      }
      else{
        dataType <- "T"
        T1l <- paste(T1l, ", T1", sep="")
        T2l <- paste(T2l, ", T2", sep="")
      }
# print(df)
      # p[[bases[i]]]   <- PlotT1T2corrRegress(df,T1l,T2l,ls.corrs, TEXT = TEXT, ALPHA_VAR = ALPHA_VAR, AXES = AXES, 
      #                                        LEGEND = LEGEND_i, ANNOTATE_LOC = ANNOTATE_LOC, outputType = outputType, dataType = dataType,
      #                                        SCALE_CORRECTION = SCALE_CORRECTION, JITTER = JITTER, SHAPE_VAR = SHAPE_VAR)#, Q_units = Q_units)
      p[[bases[i]]]   <- PlotT1T2corrRegress(df,T1l,T2l,ls.corrs, TEXT = TEXT, ALPHA_VAR = ALPHA_VAR , 
                                             LEGEND = LEGEND_i, ANNOTATE_LOC = ANNOTATE_LOC, AXES=AXES, outputType = outputType, dataType = dataType)#, Q_units = Q_units)
      if (SCREEN | SAVE) grp[[bases[i]]] <- ggplot_gtable(ggplot_build(p[[bases[i]]]))
    }
    
    if (SCREEN | SAVE) MakeGridPlot(grp, SCREEN = SCREEN, SAVE = SAVE, SIZE = SIZE)
    return(p)
}
