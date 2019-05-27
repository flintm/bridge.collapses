# 2015-08-31 Comparison of HEC-SSP and Fitted LP3 results for gauges

load(file.path(dirs$DataDirAnalysis,"20150813_USGSgagePeaks_LP3_GEV_Param.RData"))
load(file.path(dirs$DataDirFrequent, "20150605_HEC_PFA_41_Bridges.RData"))
load(file.path(dirs$DataDirAnalysis,"20150825_MomentsHECandMLE-fitLP3dists.RData"))
load(file.path(dirs$DataDirFrequent,"20150729_Discharge_39Gages.RData"))
rm(ls.Discharge.Inst,ls.Discharge.Dist,ls.Discharge.All,ls.Discharge)

require(stats)
require(Kendall)
require(SuppDists)
require(pspearman)
require(nsRFA)
require(ggplot2)

#      (A4) Is the fitted LP3 distribution consistent with the HEC-SSP distribution? -------------------------------

# K-S tests --------
alphas <- c(0.01, 0.05, 0.1)
nIDs   <- length(IDsToView)

KS_HEC_LP3 <- list()
for (i in 1:nIDs){
  ID    <- IDsToView[i]
  STAID <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"STAID"]
  name  <- paste(STAID,ID,sep="-")
  
  n_i <- sum(!is.na(HEC_PFA[[STAID]]$FLOW.Observed..Median.))
  Q   <- HEC_PFA[[STAID]]$FLOW.Observed..Median.[!is.na(HEC_PFA[[STAID]]$FLOW.Observed..Median.)]
  F_i <- 1 - 0.01*HEC_PFA[[STAID]]$FREQ[!is.na(HEC_PFA[[STAID]]$FLOW.Observed..Median.)]
  
  dist_j <- "LP3"
  params <- mlUSGSgagesPeaks[[dist_i]][[STAID]]
  F_j <- switch(dist_j,
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
  n_j <- nrow(ls.Discharge.Peaks[[STAID]])
  
  Dn <-  max(abs(F_j - F_i))
  KS_HEC_LP3[[STAID]]   <- sapply(alphas, 
                                  function(alpha) ks_test_approx_MF(Dn,n_i,alpha,TRUE,n_j,FALSE),
                                  simplify = FALSE,
                                  USE.NAMES = TRUE
  )
}

# analyze
KS <- unlist(lapply(KS_HEC_LP3,"[[",1))
sum(KS) # 34 of 35 pass
which(!KS)
# 07277700 
# 29 
# according to the plot it really is way off. HEC is shifted significantly to the left except

# Compare moments using correlation tests -------------
HECmeans    <- unlist(lapply(HECmoments,"[[",1))
mleLP3means <- unlist(lapply(mleLP3mom,"[[",1))
plot(HECmeans,mleLP3means)
cor.test(HECmeans,mleLP3means)
# Pearson's product-moment correlation
# data:  HECmeans and mleLP3means
# t = -0.76642, df = 34, p-value = 0.4487
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.4400151  0.2070835
# sample estimates:
#        cor 
# -0.1303183 

HECstds    <- sqrt(unlist(lapply(HECmoments,"[[",2)))
mleLP3stds <- sqrt(unlist(lapply(mleLP3mom,"[[",2)))
plot(HECstds,mleLP3stds)
cor.test(HECstds,mleLP3stds)
# Pearson's product-moment correlation
# data:  HECstds and mleLP3stds
# t = -0.46001, df = 34, p-value = 0.6484
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.3969260  0.2565175
# sample estimates:
#   cor 
# -0.0786461 

HECskews    <- unlist(lapply(HECmoments,"[[",3))
mleLP3skews <- unlist(lapply(mleLP3mom,"[[",3))
plot(HECskews,mleLP3skews)
cor.test(HECskews,mleLP3skews)
# Pearson's product-moment correlation
# data:  HECskews and mleLP3skews
# t = 0.1596, df = 34, p-value = 0.8741
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.3039061  0.3527265
# sample estimates:
# cor 
# 0.02736147 

# PRETTY MUCH NO CORRELATION - HEC-SSP PROCEDURE IS SUBSTANTIALLY DIFFERENT THAN MLE
