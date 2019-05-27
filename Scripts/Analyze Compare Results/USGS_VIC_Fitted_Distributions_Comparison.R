# Compare the fitted distributions of USGS vs Daymet-VIC bridge and gauge data
# Written by Madeleine Flint on 2015-08-28

require(stats)
require(nsRFA)
require(ggplot2)
require(plyr)
require(hydroGOF)

source(file.path(dirsGit$ScriptsDir,"ks_test_approx_MF.R"))

load(file.path(dirs$DataDirFrequent, "df.Fail.NBI.Gage.Active.RData"))
load(file.path(dirs$DataDirAnalysis,"20150813_Gage_Daymet_VIC_Annual_Max_P3_LP3_GEV_Param.RData"))
load(file.path(dirs$DataDirAnalysis,"20150813_Bridge_Daymet_VIC_Annual_Max_P3_LP3_GEV_Param.RData"))
load(file.path(dirs$DataDirAnalysis,"20150813_USGSgagePeaks_LP3_GEV_Param.RData"))
load(file.path(dirs$DataDirFrequent,"20150729_Discharge_39Gages.RData"))
rm(ls.Discharge.Inst,ls.Discharge.Dist,ls.Discharge.All,ls.Discharge)
load(file.path(dirs$DataDirAnalysis,"20150810_BridgeRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirs$DataDirAnalysis,"20150813_GageRoutingVICDaymetAnnualMaxes.RData"))
names(ls.GageRoutedAnnualMax) <- sapply(1:length(names(ls.GageRoutedAnnualMax)), 
                                        function(i) ifelse(nchar(names(ls.GageRoutedAnnualMax)[i])>=8,
                                                           names(ls.GageRoutedAnnualMax)[i],
                                                           paste("0",names(ls.GageRoutedAnnualMax)[i],sep="")))
for (dist_i in names(mlGagesAnnualMax)){
  names(mlGagesAnnualMax[[dist_i]]) <- sapply(1:length(names(mlGagesAnnualMax[[dist_i]])), 
                                              function(i) ifelse(nchar(names(mlGagesAnnualMax[[dist_i]])[i])>=8,
                                                                 names(mlGagesAnnualMax[[dist_i]])[i],
                                                                 paste("0",names(mlGagesAnnualMax[[dist_i]])[i],sep=""))
  )
}

######### (C5) Are the gauge distributions produced by Daymet-VIC consistent with the USGS distributions (parametric)? (individual) --------------
######### (C6) Are the bridge distributions produced by Daymet-VIC consistent with the USGS distributions (parametric)? (individual) -------------
######### (D3) Internal to Daymet-VIC, how well do runoff distributions at the gauge represent distributions at the bridge (parametric)? (individual) ------
IDsToAnalyzeVIC    <- as.character(IDsToView[!(IDsToView %in% c(1164,1166)) & df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"DATE_FAIL_EST_USGS"] >= as.Date("1980-01-01")])
gaugesToAnalyzeVIC <- unique(names(ls.Discharge.Peaks)[names(ls.Discharge.Peaks) %in% df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"STAID"]])
alphas <- c(0.01, 0.05, 0.1)
nIDs   <- length(IDsToAnalyzeVIC)
nQ     <- 1000

KSusgsVICg <- list()
KSusgsVICb <- list()
KSVICgVICb <- list()
for (i in 1:nIDs){
  ID    <- IDsToAnalyzeVIC[i]
  STAID <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"STAID"]
  name  <- paste(STAID,ID,sep="-")
  
  # (C5) USGS and VIC-gauge
  print("USGS - Daymet-VIC gauge")
  dists <- c(names(mlUSGSgagesPeaks), names(mlGagesAnnualMax))[duplicated(c(names(mlUSGSgagesPeaks), names(mlGagesAnnualMax)))]
  
  Qmin <- min(min(ls.Discharge.Peaks[[STAID]]$peak_va, na.rm = TRUE), min(ls.GageRoutedAnnualMax[[STAID]], na.rm = TRUE))
  Qmax <- max(max(ls.Discharge.Peaks[[STAID]]$peak_va, na.rm = TRUE), max(ls.GageRoutedAnnualMax[[STAID]], na.rm = TRUE))
  Q    <- seq(Qmin,Qmax,length.out = nQ)
  for (dist_i in dists){
    params <- mlUSGSgagesPeaks[[dist_i]][[STAID]]
    F_i <- switch(dist_i,
                  LP3 = F.gamma(log(Q), params[1],params[2],params[3]),
                  P3  = F.gamma(Q, params[1],params[2],params[3]),
                  GEV = F.GEV(Q, params[1],params[2],params[3]) 
    )
    if (any(is.na(F_i))){
      if (which(is.na(F_i))[sum(is.na(F_i))] == nQ){
        F_i[is.na(F_i)] <- 1
      }
      else{
        if (which(is.na(F_i))[1] == 1){
          F_i[is.na(F_i)] <- 0
        }
        }
    }
    n_i <- nrow(ls.Discharge.Peaks[[STAID]])
    
    params <- mlGagesAnnualMax[[dist_i]][[STAID]]
    F_j <- switch(dist_i,
                  LP3 = F.gamma(log(Q), params[1],params[2],params[3]),
                  P3  = F.gamma(Q, params[1],params[2],params[3]),
                  GEV = F.GEV(Q, params[1],params[2],params[3]) 
    )
    if (any(is.na(F_j))){
      if (which(is.na(F_j))[sum(is.na(F_j))] == nQ){
        F_j[is.na(F_j)] <- 1
      }
      else{ 
        if (which(is.na(F_j))[1] == 1){
          F_j[is.na(F_j)] <- 0
        }
        }
    }
    n_j <- length(ls.GageRoutedAnnualMax[[STAID]])
    
    Dn <-  max(abs(F_j - F_i))
    KSusgsVICg[[STAID]]   <- sapply(alphas, 
                           function(alpha) ks_test_approx_MF(Dn,n_i,alpha,TRUE,n_j,FALSE),
                           simplify = FALSE,
                           USE.NAMES = TRUE
    )
  }
  
  # (C6) USGS and VIC-bridge
  print("USGS - Daymet-VIC bridge")
  dists <- c(names(mlUSGSgagesPeaks), names(mlBridgesAnnualMax))[duplicated(c(names(mlUSGSgagesPeaks), names(mlBridgesAnnualMax)))]
  
  Qmin <- min(min(ls.Discharge.Peaks[[STAID]]$peak_va, na.rm = TRUE), min(ls.BridgeRoutedAnnualMax[[ID]], na.rm = TRUE))
  Qmax <- max(max(ls.Discharge.Peaks[[STAID]]$peak_va, na.rm = TRUE), max(ls.BridgeRoutedAnnualMax[[ID]], na.rm = TRUE))
  Q    <- seq(Qmin,Qmax,length.out = nQ)
  for (dist_i in dists){
    params <- mlUSGSgagesPeaks[[dist_i]][[STAID]]
    F_i <- switch(dist_i,
                  LP3 = F.gamma(log(Q), params[1],params[2],params[3]),
                  P3  = F.gamma(Q, params[1],params[2],params[3]),
                  GEV = F.GEV(Q, params[1],params[2],params[3]) 
    )
    if (any(is.na(F_i))){
      if (which(is.na(F_i))[sum(is.na(F_i))] == nQ){
        F_i[is.na(F_i)] <- 1
      }
      else{
        if (which(is.na(F_i))[1] == 1){
          F_i[is.na(F_i)] <- 0
        }
      }
    }
    n_i <- nrow(ls.Discharge.Peaks[[STAID]])
    
    params <- mlBridgesAnnualMax[[dist_i]][[ID]]
    F_j <- switch(dist_i,
                  LP3 = F.gamma(log(Q), params[1],params[2],params[3]),
                  P3  = F.gamma(Q, params[1],params[2],params[3]),
                  GEV = F.GEV(Q, params[1],params[2],params[3]) 
    )
    if (any(is.na(F_j))){
      if (which(is.na(F_j))[sum(is.na(F_j))] == nQ){
        F_j[is.na(F_j)] <- 1
      }
      else{ 
        if (which(is.na(F_j))[1] == 1){
          F_j[is.na(F_j)] <- 0
        }
      }
    }
    n_j <- length(ls.BridgeRoutedAnnualMax[[ID]])
    
    Dn <-  max(abs(F_j - F_i))
    KSusgsVICb[[name]]   <- sapply(alphas, 
                                    function(alpha) ks_test_approx_MF(Dn,n_i,alpha,TRUE,n_j,FALSE),
                                    simplify = FALSE,
                                    USE.NAMES = TRUE
    )
  }
  
  # (D3) VIC-gauge and VIC-bridge
  print("Daymet-VIC gauge - Daymet-VIC bridge")
  dists <- c(names(mlGagesAnnualMax), names(mlBridgesAnnualMax))[duplicated(c(names(mlGagesAnnualMax), names(mlBridgesAnnualMax)))]
  
  Qmin <- min(min(ls.GageRoutedAnnualMax[[STAID]], na.rm = TRUE), min(ls.BridgeRoutedAnnualMax[[ID]], na.rm = TRUE))
  Qmax <- max(max(ls.GageRoutedAnnualMax[[STAID]], na.rm = TRUE), max(ls.BridgeRoutedAnnualMax[[ID]], na.rm = TRUE))
  Q    <- seq(Qmin,Qmax,length.out = nQ)
  for (dist_i in dists){
    params <- mlGagesAnnualMax[[dist_i]][[STAID]]
    F_i <- switch(dist_i,
                  LP3 = F.gamma(log(Q), params[1],params[2],params[3]),
                  P3  = F.gamma(Q, params[1],params[2],params[3]),
                  GEV = F.GEV(Q, params[1],params[2],params[3]) 
    )
    if (any(is.na(F_i))){
      if (which(is.na(F_i))[sum(is.na(F_i))] == nQ){
        F_i[is.na(F_i)] <- 1
      }
      else{
        if (which(is.na(F_i))[1] == 1){
          F_i[is.na(F_i)] <- 0
        }
      }
    }
    n_i <- length(ls.GageRoutedAnnualMax[[STAID]])
    
    params <- mlBridgesAnnualMax[[dist_i]][[ID]]
    F_j <- switch(dist_i,
                  LP3 = F.gamma(log(Q), params[1],params[2],params[3]),
                  P3  = F.gamma(Q, params[1],params[2],params[3]),
                  GEV = F.GEV(Q, params[1],params[2],params[3]) 
    )
    if (any(is.na(F_j))){
      if (which(is.na(F_j))[sum(is.na(F_j))] == nQ){
        F_j[is.na(F_j)] <- 1
      }
      else{ 
        if (which(is.na(F_j))[1] == 1){
          F_j[is.na(F_j)] <- 0
        }
      }
    }
    n_j <- length(ls.BridgeRoutedAnnualMax[[ID]])
    
    Dn <-  max(abs(F_j - F_i))
    KSVICgVICb[[name]]   <- sapply(alphas, 
                                   function(alpha) ks_test_approx_MF(Dn,n_i,alpha,TRUE,n_j,FALSE),
                                   simplify = FALSE,
                                   USE.NAMES = TRUE
    )
  }
  
}

# Explore results --------
# USGS - Daymet-VIC gauge
KS <- unlist(lapply(KSusgsVICg,"[[",1))
sum(KS) # 4
which(KS)
# 10312000 07030050 02482550 04273800 
# 7       10       13       16 

# USGS - Daymet-VIC bridge
KS <- unlist(lapply(KSusgsVICb,"[[",1))
sum(KS) # 4
which(KS)
# 10312000-809 07030050-1004 02482550-1289 04273800-1550 
# 7            10            13            17 
# same as previous, so presumably in the same grid cell

KS <- unlist(lapply(KSVICgVICb,"[[",1))
sum(KS) # 19
which(KS)
# 01662800-136  01624800-195  01031500-476  01049500-479  01090800-799  10312000-809  04234000-864  04216500-901 07030050-1004 02021500-1044 01648000-1272 02482550-1289 06891500-1442 
# 1             2             4             5             6             7             8             9            10            11            12            13            15 
# 04273800-1550 02487500-1673 06927000-1740 07069500-3528 07075000-3535 01349810-3629 
# 17            19            21            22            23            26 

# from Timeseries Correlation:
# IDsWithGoodAgreement <- c("479","809","1004","1289","1550")
# These are the same as the ones where the parametric performed well, except this test does not pass ID 479
