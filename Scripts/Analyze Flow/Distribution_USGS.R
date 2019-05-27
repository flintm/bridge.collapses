# 2015-08-13 Fit distribution to USGS gauge data, based on script:
# 2015-08-07 Fit distributions to VIC-Daymet Routed Data

require(nsRFA)
require(MASS)
source(file.path(dirsGit$ScriptsDir,"hydrologyDistrAssess.R"))
source(file.path(dirsGit$ScriptsDir,"A2_GOFlaio_MF.R"))
source(file.path(dirsGit$ScriptsDir,"ks_test_approx_MF.R"))

load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))

# use nsRFA package to test a variety of distributions using the Anderson Darling test
dists <- c("LN","GUMBEL","GEV","P3","LP3")

########### ANDERSON-DARLING TEST USGS Gages ############################
# test full dataset -------------------------------------------------------------------
load(file.path(dirs$DataDirFrequent,"20150729_Discharge_39Gages.RData"))
rm(ls.Discharge.Inst,ls.Discharge.Dist)
ADtestUSGSgages <- sapply(dists, 
                        function(dist_i) sapply(names(ls.Discharge.All), 
                                                function(j) hydrologyDistrAssess(ls.Discharge.All[[j]]$val, dist_i, name = j),
                                                simplify = FALSE,
                                                USE.NAMES = TRUE
                                                ),
                        simplify = FALSE,
                        USE.NAMES = TRUE
                        )

# analyze results of test (find minimum AD for each location, then look at distribution of selections)
minADdist <- sapply(names(ls.Discharge.All), 
                    function(j) dists[which.min(c(ADtestUSGSgages[[dists[1]]][[j]]["AD"],
                                                  ADtestUSGSgages[[dists[2]]][[j]]["AD"],
                                                  ADtestUSGSgages[[dists[3]]][[j]]["AD"],
                                                  ADtestUSGSgages[[dists[4]]][[j]]["AD"],
                                                  ADtestUSGSgages[[dists[5]]][[j]]["AD"]))
                                      ],
                    simplify = "array")
table(factor(minADdist))
#GEV  LN LP3  P3 
#  7   5  24   1 
# looks like implementing LP3 is the only one that really makes sense, but will also do GEV
# because that's what I've done for Daymet-VIC

# TEST ANNUAL PEAKS -------------------------------------------------------------------
# perform test
ADtestUSGSgagesPeak <- sapply(dists, 
                              function(dist_i) sapply(names(ls.Discharge.Peaks), 
                                                      function(j) hydrologyDistrAssess(ls.Discharge.Peaks[[j]]$peak_va, dist_i, name = j),
                                                      simplify = FALSE,
                                                      USE.NAMES = TRUE
                              ),
                              simplify = FALSE,
                              USE.NAMES = TRUE
)

# analyze results of test (find minimum AD for each location, then look at distribution of selections)
minADdistPeak <- sapply(names(ls.Discharge.Peaks), 
                             function(j) dists[which.min(c(ADtestUSGSgagesPeak[[dists[1]]][[j]]["AD"],
                                                           ADtestUSGSgagesPeak[[dists[2]]][[j]]["AD"],
                                                           ADtestUSGSgagesPeak[[dists[3]]][[j]]["AD"],
                                                           ADtestUSGSgagesPeak[[dists[4]]][[j]]["AD"],
                                                           ADtestUSGSgagesPeak[[dists[5]]][[j]]["AD"]))
                                               ],
                             simplify = "array")
table(factor(minADdistPeak))
#GEV GUMBEL     LN    LP3     P3 
# 19      2      6      6      2  
# Interesting... Peaks is using instantaneous data and now the GEV is a better fit.
rm(ADtestUSGSgagesPeak,ADtestUSGSgages,minADdistPeak,minADdist)

######## OBTAIN PARAMETERS FOR SELECTED DISTRIBUTIONS AND ANALYZE GOODNESS OF FIT ######################
dists <- c("LP3","GEV")
alphas        <- c(0.01,0.05,0.1)

# USGSgages Daily ------------------------
paramMaxLikelihoodUSGSgage <- sapply(dists, 
                                   function(dist_i) sapply(names(ls.Discharge.All), 
                                                           function(j) hydrologyDistrAssess(ls.Discharge.All[[j]]$val, dist_i, name = j, paramFlag = TRUE, ADpFlag = TRUE),
                                                           simplify = FALSE,
                                                           USE.NAMES = TRUE
                                                           ),
                                   simplify = FALSE,
                                   USE.NAMES = TRUE
                                   )
mlUSGSgages <- sapply(dists, 
                    function(dist_i) sapply(names(paramMaxLikelihoodUSGSgage[[dist_i]]), 
                                            function(j) paramMaxLikelihoodUSGSgage[[dist_i]][[j]][["ml"]],
                                            simplify = FALSE,
                                            USE.NAMES = TRUE
                    ),
                    simplify = FALSE,
                    USE.NAMES = TRUE
)
savefile <- paste(gsub("-","",Sys.Date()),"USGSgage_LP3_GEV_Param.RData",sep="_")
save(paramMaxLikelihoodUSGSgage,mlUSGSgages,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)


ADUSGSgages <- sapply(dists, 
                    function(dist_i) sapply(names(paramMaxLikelihoodUSGSgage[[dist_i]]), 
                                            function(j) paramMaxLikelihoodUSGSgage[[dist_i]][[j]][["AD"]][1]
                                           ),
                    simplify = FALSE,
                    USE.NAMES = TRUE
                    )
pADUSGSgages <- sapply(dists, 
                    function(dist_i) sapply(names(paramMaxLikelihoodUSGSgage[[dist_i]]), 
                                            function(j) paramMaxLikelihoodUSGSgage[[dist_i]][[j]][["AD"]][2]
                                            ),
                    simplify = FALSE,
                    USE.NAMES = TRUE
                    )
nADUSGSgagesPassed <- sapply(dists, function(dist_i) sapply(alphas,
                                                          function(alpha) sum(pADUSGSgages[[dist_i]] <= alpha, na.rm = TRUE)))
# In general, the AD values are smaller than they were for the Daymet-VIC data (i.e., O(10) not O(100)), but still too large.
RUNOFF_CFS <- sapply(names(ls.Discharge.All), function(j) sort(ls.Discharge.All[[j]]$val),
                     simplify = FALSE,
                     USE.NAMES = TRUE
                      )
DnUSGSgages     <- sapply(dists, 
                        function(dist_i) sapply(1:length(paramMaxLikelihoodUSGSgage[[dist_i]]), 
                                                function(j) switch(dist_i,
                                                                   LP3 = max(abs(F.gamma(log(RUNOFF_CFS[[j]]), mlUSGSgages[[dist_i]][[j]][1],mlUSGSgages[[dist_i]][[j]][2],mlUSGSgages[[dist_i]][[j]][3]) - 
                                                                               sapply(RUNOFF_CFS[[j]], function(i) which(RUNOFF_CFS[[j]]==i)[1]/length(RUNOFF_CFS[[j]])) )),
                                                                   GEV = max(abs(F.GEV(RUNOFF_CFS[[j]], mlUSGSgages[[dist_i]][[j]][1],mlUSGSgages[[dist_i]][[j]][2],mlUSGSgages[[dist_i]][[j]][3]) - 
                                                                                   sapply(RUNOFF_CFS[[j]], function(i) which(RUNOFF_CFS[[j]]==i)[1]/length(RUNOFF_CFS[[j]])) )))
                                                ),
                        simplify = FALSE,
                        USE.NAMES = TRUE
                        )
KSUSGSgages   <- sapply(dists,
                      function(dist_i) sapply(alphas, 
                                              function(alpha) sapply(1:length(DnUSGSgages[[dist_i]]),
                                                                     function(j) ifelse(!is.null(DnUSGSgages[[dist_i]][j]) & !is.na(DnUSGSgages[[dist_i]][j]),
                                                                                        ks_test_approx_MF(DnUSGSgages[[dist_i]][j],length(RUNOFF_CFS[[j]]),alpha),
                                                                                        NA),
                                                                     USE.NAMES = TRUE
                                                                      ),
                                              simplify = FALSE,
                                              USE.NAMES = TRUE
                                              ),
                      simplify = FALSE,
                      USE.NAMES = TRUE
                      )
nKSUSGSgagesPassed <- sapply(dists, function(dist_i) sapply(1:length(alphas),
                                                          function(alpha) sum(KSUSGSgages[[dist_i]][[alpha]], na.rm = TRUE)))
# Only 2 pass (and could actually be one because two of the stations are the same)

# Check a few distributions by plotting to see what's going on
library(ggplot2)
j<-1
j <- j+1
df <- data.frame(x    = sort(RUNOFF_CFS[[j]]),
                 FGEV = F.GEV(sort(RUNOFF_CFS[[j]]),mlUSGSgages[["GEV"]][[j]][1],mlUSGSgages[["GEV"]][[j]][2],mlUSGSgages[["GEV"]][[j]][3]),
                 FLP3 = F.gamma(sort(log(RUNOFF_CFS[[j]])),mlUSGSgages[["LP3"]][[j]][1],mlUSGSgages[["LP3"]][[j]][2],mlUSGSgages[["LP3"]][[j]][3])
                 )
ggplot(data=df) + stat_ecdf(aes(x=x)) + geom_line(aes(x=x,y=FGEV),color="red") + geom_line(aes(x=x,y=FLP3),color="blue")
# some stations LP3 appears to fit VERY well, GEV generally less well. Some stations both poor


savefile <- paste(gsub("-","",Sys.Date()),"USGSgage_LP3_GEV_GoodnessFit.RData",sep="_")
save(ADUSGSgages,pADUSGSgages,nADUSGSgagesPassed,KSUSGSgages,nKSUSGSgagesPassed,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
rm(ADUSGSgages,DnUSGSgages,KSUSGSgages,mlUSGSgages,ls.Discharge.All,pADUSGSgages,paramMaxLikelihoodUSGSgage,RUNOFF_CFS,nADUSGSgagesPassed,nKSUSGSgagesPassed,df,j)

# USGSgages Peaks ------------------------
paramMaxLikelihoodUSGSgagePeaks <- sapply(dists, 
                                     function(dist_i) sapply(names(ls.Discharge.Peaks), 
                                                             function(j) hydrologyDistrAssess(ls.Discharge.Peaks[[j]]$peak_va, dist_i, name = j, paramFlag = TRUE, ADpFlag = TRUE),
                                                             simplify = FALSE,
                                                             USE.NAMES = TRUE
                                     ),
                                     simplify = FALSE,
                                     USE.NAMES = TRUE
)
mlUSGSgagesPeaks <- sapply(dists, 
                      function(dist_i) sapply(names(paramMaxLikelihoodUSGSgagePeaks[[dist_i]]), 
                                              function(j) paramMaxLikelihoodUSGSgagePeaks[[dist_i]][[j]][["ml"]],
                                              simplify = FALSE,
                                              USE.NAMES = TRUE
                      ),
                      simplify = FALSE,
                      USE.NAMES = TRUE
)
savefile <- paste(gsub("-","",Sys.Date()),"USGSgagePeaks_LP3_GEV_Param.RData",sep="_")
save(paramMaxLikelihoodUSGSgagePeaks,mlUSGSgagesPeaks,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)


ADUSGSgagesPeaks <- sapply(dists, 
                      function(dist_i) sapply(names(paramMaxLikelihoodUSGSgagePeaks[[dist_i]]), 
                                              function(j) paramMaxLikelihoodUSGSgagePeaks[[dist_i]][[j]][["AD"]][1]
                      ),
                      simplify = FALSE,
                      USE.NAMES = TRUE
)
pADUSGSgagesPeaks <- sapply(dists, 
                       function(dist_i) sapply(names(paramMaxLikelihoodUSGSgagePeaks[[dist_i]]), 
                                               function(j) paramMaxLikelihoodUSGSgagePeaks[[dist_i]][[j]][["AD"]][2]
                       ),
                       simplify = FALSE,
                       USE.NAMES = TRUE
)
nADUSGSgagesPassedPeaks <- sapply(dists, function(dist_i) sapply(alphas,
                                                            function(alpha) sum(pADUSGSgagesPeaks[[dist_i]] <= alpha, na.rm = TRUE)))
# In general, the AD values are actually very good, especially for LP3
mean(ADUSGSgagesPeaks[["LP3"]])
mean(ADUSGSgagesPeaks[["GEV"]])

# now K-S test
RUNOFF_CFS <- sapply(names(ls.Discharge.Peaks), function(j) sort(ls.Discharge.Peaks[[j]]$peak_va),
                     simplify = FALSE,
                     USE.NAMES = TRUE
)
DnUSGSgagesPeaks     <- sapply(dists, 
                          function(dist_i) sapply(1:length(paramMaxLikelihoodUSGSgagePeaks[[dist_i]]), 
                                                  function(j) switch(dist_i,
                                                                     LP3 = max(abs(F.gamma(log(RUNOFF_CFS[[j]]), mlUSGSgagesPeaks[[dist_i]][[j]][1],mlUSGSgagesPeaks[[dist_i]][[j]][2],mlUSGSgagesPeaks[[dist_i]][[j]][3]) - 
                                                                                     sapply(RUNOFF_CFS[[j]], function(i) which(RUNOFF_CFS[[j]]==i)[1]/length(RUNOFF_CFS[[j]])) )),
                                                                     GEV = max(abs(F.GEV(RUNOFF_CFS[[j]], mlUSGSgagesPeaks[[dist_i]][[j]][1],mlUSGSgagesPeaks[[dist_i]][[j]][2],mlUSGSgagesPeaks[[dist_i]][[j]][3]) - 
                                                                                     sapply(RUNOFF_CFS[[j]], function(i) which(RUNOFF_CFS[[j]]==i)[1]/length(RUNOFF_CFS[[j]])) )))
                          ),
                          simplify = FALSE,
                          USE.NAMES = TRUE
)
KSUSGSgagesPeaks   <- sapply(dists,
                        function(dist_i) sapply(alphas, 
                                                function(alpha) sapply(1:length(DnUSGSgagesPeaks[[dist_i]]),
                                                                       function(j) ifelse(!is.null(DnUSGSgagesPeaks[[dist_i]][j]) & !is.na(DnUSGSgagesPeaks[[dist_i]][j]),
                                                                                          ks_test_approx_MF(DnUSGSgagesPeaks[[dist_i]][j],length(RUNOFF_CFS[[j]]),alpha),
                                                                                          NA),
                                                                       USE.NAMES = TRUE
                                                ),
                                                simplify = FALSE,
                                                USE.NAMES = TRUE
                        ),
                        simplify = FALSE,
                        USE.NAMES = TRUE
)
nKSUSGSgagesPassedPeaks <- sapply(dists, function(dist_i) sapply(1:length(alphas),
                                                            function(alpha) sum(KSUSGSgagesPeaks[[dist_i]][[alpha]], na.rm = TRUE)))
# All 35 pass for alpha up to 0.1 for LP3, 34 pass for all GEV

# Check a few distributions by plotting to see what's going on
j<-1
j <- j+1
df <- data.frame(x    = sort(RUNOFF_CFS[[j]]),
                 FGEV = F.GEV(sort(RUNOFF_CFS[[j]]),mlUSGSgagesPeaks[["GEV"]][[j]][1],mlUSGSgagesPeaks[["GEV"]][[j]][2],mlUSGSgagesPeaks[["GEV"]][[j]][3]),
                 FLP3 = F.gamma(sort(log(RUNOFF_CFS[[j]])),mlUSGSgagesPeaks[["LP3"]][[j]][1],mlUSGSgagesPeaks[["LP3"]][[j]][2],mlUSGSgagesPeaks[["LP3"]][[j]][3])
)
ggplot(data=df) + stat_ecdf(aes(x=x)) + geom_line(aes(x=x,y=FGEV),color="red") + geom_line(aes(x=x,y=FLP3),color="blue")
# Very obvious in ECDF that there are few data points. I wonder if I'm being unfair to Daymet-VIC by running the tests on the full data,
# not just the annual peaks. I should go back and try that.


savefile <- paste(gsub("-","",Sys.Date()),"USGSgagePeaks_LP3_GEV_GoodnessFit.RData",sep="_")
save(ADUSGSgagesPeaks,pADUSGSgagesPeaks,nADUSGSgagesPassedPeaks,KSUSGSgagesPeaks,nKSUSGSgagesPassedPeaks,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
rm(ADUSGSgagesPeaks,DnUSGSgagesPeaks,KSUSGSgagesPeaks,mlUSGSgagesPeaks,ls.Discharge.Peaks,pADUSGSgagesPeaks,paramMaxLikelihoodUSGSgagePeaks,RUNOFF_CFS,
   nADUSGSgagesPassedPeaks,nKSUSGSgagesPassedPeaks,df,j,dists,alphas)
rm(A2_GOFlaio_MF,ks_test_approx_MF,hydrologyDistrAssess)

# OBTAIN EXPECTED VALUE OF T_FAIL, T_MAX, AND T_MAX_PRE_FAIL USING DAILY DATA--------------
pExc           <- list()
pExcMax        <- list()
pExcMaxPreFail <- list()
for (ID in as.character(IDsToView)){
  print(ID)
  STAID         <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID == ID, "STAID"]
  pExc[[ID]] <- list()
#   Option (1) to obtain Qfail from best estimate
#     Qfail  <- switch(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"T_BEST_SOURCE"],
#                      DAILY = df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAIL_D_USGS"],
#                      PEAK  = df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAIL_P_USGS"],
#                      INST  =df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAIL_I_USGS"]
#     )
  Qfail <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAIL_D_USGS"]
  Qmax         <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_MAX_D_USGS"]
  QmaxPreFail  <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_MAXPREFAIL_D_USGS"]
  for (dist_i in names(paramMaxLikelihoodUSGSgagePeaks)){
      params <- mlUSGSgagesPeaks[[dist_i]][[STAID]]
      pExc[[ID]][[dist_i]]   <- 1 - switch(dist_i,
                           LP3 = F.gamma(log(Qfail), params[1],params[2],params[3]),
                           P3  = F.gamma(Qfail, params[1],params[2],params[3]),
                           GEV = F.GEV(Qfail, params[1],params[2],params[3]) 
      )
      pExcMax[[ID]][[dist_i]]   <- 1 - switch(dist_i,
                                           LP3 = F.gamma(log(Qmax), params[1],params[2],params[3]),
                                           P3  = F.gamma(Qmax, params[1],params[2],params[3]),
                                           GEV = F.GEV(Qmax, params[1],params[2],params[3]) 
      )
      pExcMaxPreFail[[ID]][[dist_i]]   <- 1 - switch(dist_i,
                                              LP3 = F.gamma(log(QmaxPreFail), params[1],params[2],params[3]),
                                              P3  = F.gamma(QmaxPreFail, params[1],params[2],params[3]),
                                              GEV = F.GEV(QmaxPreFail, params[1],params[2],params[3]) 
      )
      
  }
}

# store in df.Fail.NBI.Gage
p_Exc_LP3 <- unlist(lapply(pExc,"[[",1))
p_Exc_GEV <- unlist(lapply(pExc,"[[",2))

df.Fail.NBI.Gage$T_FAIL_DAILY_LP3_USGS <- numeric(nrow(df.Fail.NBI.Gage))
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"T_FAIL_DAILY_LP3_USGS"] <- 1/p_Exc_LP3
df.Fail.NBI.Gage$T_FAIL_DAILY_GEV_USGS <- numeric(nrow(df.Fail.NBI.Gage))
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"T_FAIL_DAILY_GEV_USGS"] <- 1/p_Exc_GEV

p_Exc_LP3 <- unlist(lapply(pExcMax,"[[",1))
p_Exc_GEV <- unlist(lapply(pExcMax,"[[",2))

df.Fail.NBI.Gage$T_MAX_DAILY_LP3_USGS <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"T_MAX_DAILY_LP3_USGS"] <- 1/p_Exc_LP3 
df.Fail.NBI.Gage$T_MAX_DAILY_GEV_USGS <- numeric(nrow(df.Fail.NBI.Gage))
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"T_MAX_DAILY_GEV_USGS"] <- 1/p_Exc_GEV

p_Exc_LP3 <- unlist(lapply(pExcMaxPreFail,"[[",1))
p_Exc_GEV <- unlist(lapply(pExcMaxPreFail,"[[",2))

df.Fail.NBI.Gage$T_MAX_PRE_FAIL_DAILY_LP3_USGS <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"T_MAX_PRE_FAIL_DAILY_LP3_USGS"] <- 1/p_Exc_LP3 # a few of these needed to be updated
df.Fail.NBI.Gage$T_MAX_PRE_FAIL_DAILY_GEV_USGS <- numeric(nrow(df.Fail.NBI.Gage))
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"T_MAX_PRE_FAIL_DAILY_GEV_USGS"] <- 1/p_Exc_GEV

# OBTAIN EXPECTED VALUE OF T_FAIL USING INSTANTANEOUS DATA--------------
pExc <- list()
for (ID in as.character(IDsToView)){
  print(ID)
  STAID         <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID == ID, "STAID"]
  pExc[[ID]] <- list()

  Qfail <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAIL_I_USGS"]
  if(!is.na(Qfail)){
    for (dist_i in names(paramMaxLikelihoodUSGSgagePeaks)){
      params <- mlUSGSgagesPeaks[[dist_i]][[STAID]]
      pExc[[ID]][[dist_i]]   <- 1 - switch(dist_i,
                                              LP3 = F.gamma(log(Qfail), params[1],params[2],params[3]),
                                              P3  = F.gamma(Qfail, params[1],params[2],params[3]),
                                              GEV = F.GEV(Qfail, params[1],params[2],params[3]) 
      )
    }
  }
  else{
    pExc[[ID]] <- sapply(names(paramMaxLikelihoodUSGSgagePeaks), function(dist_i) NA_real_,
                            USE.NAMES = TRUE,
                            simplify = FALSE)
  }
}

# store in df.Fail.NBI.Gage
p_Exc_LP3 <- unlist(lapply(pExc,"[[",1))
p_Exc_GEV <- unlist(lapply(pExc,"[[",2))

df.Fail.NBI.Gage$T_FAIL_INST_LP3_USGS <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"T_FAIL_INST_LP3_USGS"] <- 1/p_Exc_LP3
df.Fail.NBI.Gage$T_FAIL_INST_GEV_USGS <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"T_FAIL_INST_GEV_USGS"] <- 1/p_Exc_GEV

# OBTAIN EXPECTED VALUE OF T_FAIL USING PEAKS DATA--------------
pExc <- list()
pExcMax        <- list()
pExcMaxPreFail <- list()
for (ID in as.character(IDsToView)){
  print(ID)
  STAID         <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID == ID, "STAID"]
  pExc[[ID]] <- list()
  
  Qfail        <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAIL_P_USGS"]
  Qmax         <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_MAX_P_USGS"]
  QmaxPreFail  <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_MAXPREFAIL_P_USGS"]
  if(!is.na(Qfail)){
  # if(!is.na(Qmax)){
    for (dist_i in names(paramMaxLikelihoodUSGSgagePeaks)){
      params <- mlUSGSgagesPeaks[[dist_i]][[STAID]]
      pExc[[ID]][[dist_i]]   <- 1 - switch(dist_i,
                                           LP3 = F.gamma(log(Qfail), params[1],params[2],params[3]),
                                           P3  = F.gamma(Qfail, params[1],params[2],params[3]),
                                           GEV = F.GEV(Qfail, params[1],params[2],params[3]) 
      )
      pExcMax[[ID]][[dist_i]]   <- 1 - switch(dist_i,
                                              LP3 = F.gamma(log(Qmax), params[1],params[2],params[3]),
                                              P3  = F.gamma(Qmax, params[1],params[2],params[3]),
                                              GEV = F.GEV(Qmax, params[1],params[2],params[3]) 
      )
      pExcMaxPreFail[[ID]][[dist_i]]   <- 1 - switch(dist_i,
                                                     LP3 = F.gamma(log(QmaxPreFail), params[1],params[2],params[3]),
                                                     P3  = F.gamma(QmaxPreFail, params[1],params[2],params[3]),
                                                     GEV = F.GEV(QmaxPreFail, params[1],params[2],params[3]) 
      )
    }
  }
  else{
    pExc[[ID]] <- sapply(names(paramMaxLikelihoodUSGSgagePeaks), function(dist_i) NA_real_,
                         USE.NAMES = TRUE,
                         simplify = FALSE)
    pExcMax[[ID]] <- sapply(names(paramMaxLikelihoodUSGSgagePeaks), function(dist_i) NA_real_,
                         USE.NAMES = TRUE,
                         simplify = FALSE)
    pExcMaxPreFail[[ID]] <- sapply(names(paramMaxLikelihoodUSGSgagePeaks), function(dist_i) NA_real_,
                            USE.NAMES = TRUE,
                            simplify = FALSE)
  }
}

# store in df.Fail.NBI.Gage
p_Exc_LP3 <- unlist(lapply(pExc,"[[",1))
p_Exc_GEV <- unlist(lapply(pExc,"[[",2))

df.Fail.NBI.Gage$T_FAIL_PEAK_LP3_USGS <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"T_FAIL_PEAK_LP3_USGS"] <- 1/p_Exc_LP3
df.Fail.NBI.Gage$T_FAIL_PEAK_GEV_USGS <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"T_FAIL_PEAK_GEV_USGS"] <- 1/p_Exc_GEV

p_Exc_LP3 <- unlist(lapply(pExcMax,"[[",1))
p_Exc_GEV <- unlist(lapply(pExcMax,"[[",2))

df.Fail.NBI.Gage$T_MAX_PEAK_LP3_USGS <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"T_MAX_PEAK_LP3_USGS"] <- 1/p_Exc_LP3
df.Fail.NBI.Gage$T_MAX_PEAK_GEV_USGS <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"T_MAX_PEAK_GEV_USGS"] <- 1/p_Exc_GEV

p_Exc_LP3 <- unlist(lapply(pExcMaxPreFail,"[[",1))
p_Exc_GEV <- unlist(lapply(pExcMaxPreFail,"[[",2))

df.Fail.NBI.Gage$T_MAX_PRE_FAIL_PEAK_LP3_USGS <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"T_MAX_PRE_FAIL_PEAK_LP3_USGS"] <- 1/p_Exc_LP3
df.Fail.NBI.Gage$T_MAX_PRE_FAIL_PEAK_GEV_USGS <- NA_real_
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView,"T_MAX_PRE_FAIL_PEAK_GEV_USGS"] <- 1/p_Exc_GEV

# SAVE
savefile <- paste(gsub("-","",Sys.Date()),
                  paste("df.Fail.NBI.Gage_With_T_FAIL_T_MAX_USGS_LP3_GEV_Updated.RData",sep=""),
                  sep="_")
save(df.Fail.NBI.Gage,IDsToView,rowsToView,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
