# ANALYZE STATISTICS OF THE SET OF RETURN PERIODS
# Copyright Madeleine Flint, 2016
# dists must be in {"GEV","LN","EXP","LP3"}

AnalyzeReturnPeriodDist <- function(BridgesDataFrame, 
                                    Tls = c("T_FAIL_D_HECD_USGS","T_FAIL_D_HECP_USGS","T_FAIL_I_HECP_USGS","T_FAIL_P_HECP_USGS",
                                            "T_FAIL_IP_HECP_USGS","T_FAILPM2_HEC_DVICG","T_FAIL_D_PARTDUR_USGS","T_FAIL_PARTDUR_DVICG",
                                            "T_MAX_D_HECD_USGS","T_MAX_D_HECP_USGS","T_MAX_P_HECP_USGS","T_MAX_HEC_DVICG"),
                                    DIST = FALSE, MANN = TRUE, dists = c("LN","LP3","GEV","EXP"), SAVE = FALSE){
  
require(nsRFA)
require(MASS)
source(file.path(dirsGit$Scripts,"hydrologyDistrAssess.R"))
source(file.path(dirsGit$Scripts,"A2_GOFlaio_MF.R"))
source(file.path(dirsGit$Scripts,"ks_test_approx_MF.R"))

Out <- list()
# If want to try to fit distributions
if (DIST){
  # use nsRFA package to test a variety of distributions using the Anderson Darling test
  ADtestT <- sapply(Tls, function(T) sapply(dists, function(dist_i) hydrologyDistrAssess(BridgesDataFrame[,T], dist_i),
                                             simplify = FALSE,
                                             USE.NAMES = TRUE
                                             ),
                    simplify = FALSE,
                    USE.NAMES = TRUE
  )
  
  # analyze results of test (find minimum AD for each location, then look at distribution of selections)
  minADdist <- sapply(Tls, function(T) names(ADtestT[[T]])[which.min(unlist(ADtestT[[T]]))],
                      simplify = FALSE,
                      USE.NAMES = TRUE
  )
  print(minADdist)
  # GEV is the best, get params for all
  param_T <- sapply(Tls, function(T) sapply(dists, function(dist_i) hydrologyDistrAssess(BridgesDataFrame[,T], dist_i, paramFlag = TRUE, ADpFlag = FALSE),
                        simplify = FALSE,
                        USE.NAMES = TRUE
                         ),
                        simplify = FALSE,
                        USE.NAMES = TRUE
  )
  ml_T <- sapply(Tls, function(T) sapply(dists, 
                     function(dist_i) param_T[[T]][[dist_i]][["ml"]],
                     simplify = FALSE,
                     USE.NAMES = TRUE
  )
  )

  Dn_T     <- sapply(Tls, function(T) 
                              list( LP3 = max(abs(F.gamma(log(BridgesDataFrame[,T]), ml_T[[T]][["LP3"]][1],ml_T[[T]][["LP3"]][2],ml_T[[T]][["LP3"]][3]) - 
                                                    sapply(BridgesDataFrame[,T], function(i) which(T_HEC==i)[1]/length(T_HEC)) )),
                                    GEV = max(abs(F.GEV(BridgesDataFrame[,T], ml_T[[T]][["GEV"]][1],ml_T[[T]][["GEV"]][2],ml_T[[T]][["GEV"]][3]) - 
                                                    sapply(BridgesDataFrame[,T], function(i) which(BridgesDataFrame[,T]==i)[1]/length(BridgesDataFrame[,T])) )),
                                    LN =  max(abs(plnorm(log(BridgesDataFrame[,T]),  ml_T[[T]][["LN"]]$estimate[1], ml_T[[T]][["LN"]]$estimate[2]) - 
                                                    sapply(BridgesDataFrame[,T], function(i) which(BridgesDataFrame[,T]==i)[1]/length(BridgesDataFrame[,T])) )),
                                    EXP = max(abs(F.exp(BridgesDataFrame[,T], ml_T[[T]][["EXP"]]$xi,ml_T[[T]][["EXP"]]$alfa) - 
                                                    sapply(BridgesDataFrame[,T], function(i) which(BridgesDataFrame[,T]==i)[1]/length(BridgesDataFrame[,T])) ))
                                  ),
                         simplify = FALSE,
                         USE.NAMES = TRUE
  )
  print(Dn_T)
  Out[["minADdist"]] <- minADdist
  Out[["Dn_T"]]      <- Dn_T
}

if(MANN){
  T_stats <- list()
  stats <- c("mean","sd","median","skew")
  if(any(grepl("FAIL",Tls))){
    T_stats[["FAIL"]] <- sapply(Tls[grepl("FAIL",Tls)], function(T) 
                                     sapply(stats, 
                                            function(stat) 
                                              switch(stat,
                                                     mean   = mean(BridgesDataFrame[,T], na.rm = TRUE),
                                                     sd     = sd(BridgesDataFrame[,T], na.rm = TRUE),
                                                     median = median(BridgesDataFrame[,T], na.rm = TRUE),
                                                     skew   = skew(BridgesDataFrame[,T])) 
                                     )
    )
  }
  if(any(grepl("MAX",Tls))){
    T_stats[["MAX"]] <- sapply(Tls[grepl("MAX",Tls)], function(T) 
                                  sapply(stats, 
                                         function(stat) 
                                           switch(stat,
                                                  mean   = mean(BridgesDataFrame[,T], na.rm = TRUE),
                                                  sd     = sd(BridgesDataFrame[,T], na.rm = TRUE),
                                                  median = median(BridgesDataFrame[,T], na.rm = TRUE),
                                                  skew   = skew(BridgesDataFrame[,T])) 
                                  )
    )
  }
  Out[["T_stats"]] <- T_stats
  
  # compare subsets
  Causes <- levels(BridgesDataFrame$FAIL_CAUS_CODE)
  Causes <- Causes[Causes!="HURRICANE"]
  Checks <- c("two.sided","less","greater")
  T_MannWhitney <- list()
  T_MannWhitney[["Cause"]] <- sapply(Tls, 
                                     function(T) sapply(Causes, 
                                                        function(cause) sapply(Checks, 
                                                                               function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$FAIL_CAUS_CODE==cause,T],
                                                                                                           BridgesDataFrame[BridgesDataFrame$FAIL_CAUS_CODE!=cause,T],
                                                                                                           alternative = check),
                                                                               USE.NAMES = TRUE,
                                                                               simplify = FALSE
                                                                               
                                                        ),
                                                        USE.NAMES = TRUE,
                                                        simplify = FALSE
                                     ),
                                     USE.NAMES = TRUE,
                                     simplify = FALSE
  )
  
  T_MannWhitney[["Hurricane"]] <- sapply(Tls, 
                                         function(T) sapply(Checks, 
                                                            function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$COMMENT_LINKED_HURRICANE!="",T],
                                                                                        BridgesDataFrame[BridgesDataFrame$COMMENT_LINKED_HURRICANE=="",T],
                                                                                        alternative = check),
                                                            USE.NAMES = TRUE,
                                                            simplify = FALSE),
                                         USE.NAMES = TRUE,
                                         simplify = FALSE
  )
  T_MannWhitney[["DrainArea1000"]] <- sapply(Tls, 
                                             function(T) sapply(Checks, 
                                                                function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM>=1000,T],
                                                                                            BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM<1000,T],
                                                                                            alternative = check),
                                                                USE.NAMES = TRUE,
                                                                simplify = FALSE),
                                             USE.NAMES = TRUE,
                                             simplify = FALSE
  )
  T_MannWhitney[["DrainArea100"]] <- sapply(Tls, 
                                            function(T) sapply(Checks, 
                                                               function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM<100,T],
                                                                                           BridgesDataFrame[BridgesDataFrame$DRAIN_SQKM>=100,T],
                                                                                           alternative = check),
                                                               USE.NAMES = TRUE,
                                                               simplify = FALSE),
                                            USE.NAMES = TRUE,
                                            simplify = FALSE
  )
  T_MannWhitney[["Regulation"]] <- sapply(Tls, 
                                          function(T) sapply(Checks, 
                                                             function(check) wilcox.test(BridgesDataFrame[BridgesDataFrame$BOOL_REGULATION==TRUE,T],
                                                                                         BridgesDataFrame[BridgesDataFrame$BOOL_REGULATION==FALSE,T],
                                                                                         alternative = check),
                                                             USE.NAMES = TRUE,
                                                             simplify = FALSE),
                                          USE.NAMES = TRUE,
                                          simplify = FALSE
  )
  Out[["T_MannWhitney"]] <- T_MannWhitney
}

chisq.test(c(sum(BridgesDataFrame$FAIL_CAUS_CODE=="SCOUR" & 
                   (BridgesDataFrame$Q_MAX_D_USGS == BridgesDataFrame$Q_FAIL_D_USGS), na.rm = TRUE),
             sum(BridgesDataFrame$FAIL_CAUS_CODE=="SCOUR" & 
                   !(BridgesDataFrame$Q_MAX_D_USGS == BridgesDataFrame$Q_FAIL_D_USGS), na.rm = TRUE)),
           p = c(sum(BridgesDataFrame$FAIL_CAUS_CODE!="SCOUR" & 
                       (BridgesDataFrame$Q_MAX_D_USGS == BridgesDataFrame$Q_FAIL_D_USGS), na.rm = TRUE),
                 sum(BridgesDataFrame$FAIL_CAUS_CODE!="SCOUR" & 
                       !(BridgesDataFrame$Q_MAX_D_USGS == BridgesDataFrame$Q_FAIL_D_USGS), na.rm = TRUE))/19) # X-squared = 2.7301, df = 1, p-value = 0.0984
if (SAVE){
  savefile <- paste(gsub("-","",Sys.Date()),"T_Fail_Max_Stats.RData",sep="_")
  save(T_stats,T_MannWhitney, file=file.path(dirs$Data,savefile))
}
return(Out)
}
