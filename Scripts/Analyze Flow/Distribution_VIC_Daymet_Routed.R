# 2015-08-07 Fit distributions to VIC-Daymet Routed Data
# 2015-08-13 modifying to perform statistical tests on data peaks
# (doesn't change parameters but wil affect test values (fewer samples))

library(nsRFA)
library(MASS)
source(file.path(dirsGit$ScriptsDir,"hydrologyDistrAssess.R"))
source(file.path(dirsGit$ScriptsDir,"A2_GOFlaio_MF.R"))
source(file.path(dirsGit$ScriptsDir,"ks_test_approx_MF.R"))

# use nsRFA package to test a variety of distributions using the Anderson Darling test
dists <- c("LN","GUMBEL","GEV","P3","LP3")

########### ANDERSON-DARLING TEST BRIDGES TO SELECT POTENTIAL DISTRIBUTIONS ############################
# test full dataset -------------------------------------------------------------------
load(file.path(dirs$DataDirFrequent,"20150807_BridgeRoutingVICDaymet.RData"))
ADtestBridges <- sapply(dists, 
                        function(dist_i) sapply(names(ls.BridgeRouted), 
                                                function(j) hydrologyDistrAssess(ls.BridgeRouted[[j]][["day"]]$RUNOFF_CFS, dist_i, name = j),
                                                simplify = FALSE,
                                                USE.NAMES = TRUE
                                                ),
                        simplify = FALSE,
                        USE.NAMES = TRUE
                        )

# analyze results of test (find minimum AD for each location, then look at distribution of selections)
minADdist <- sapply(names(ls.BridgeRouted), 
                    function(j) dists[which.min(c(ADtestBridges[[dists[1]]][[j]]["AD"],
                                                  ADtestBridges[[dists[2]]][[j]]["AD"],
                                                  ADtestBridges[[dists[3]]][[j]]["AD"],
                                                  ADtestBridges[[dists[4]]][[j]]["AD"],
                                                  ADtestBridges[[dists[5]]][[j]]["AD"]))
                                      ],
                    simplify = "array")
table(factor(minADdist))
# GEV GUMBEL     LN    LP3     P3 
# 88      4     17    224     54 
# looks like implementing LP3 and GEV for all data types makes sense.

# test yearly extremes  -------------------------------------------------------------------
# obtain annual maxes
ls.BridgeRoutedAnnualMax <- lapply(names(ls.BridgeRouted), 
                                   function(j) sapply(c(1980:2013), 
                                                      function(y) max(ls.BridgeRouted[[j]]$day$RUNOFF_CFS[ls.BridgeRouted[[j]]$day$YEAR == y], 
                                                                      na.rm=TRUE)
                                                                  )
                            )
names(ls.BridgeRoutedAnnualMax) <- names(ls.BridgeRouted)

savefile <- paste(gsub("-","",Sys.Date()),"BridgeRoutingVICDaymetAnnualMaxes.RData",sep="_")
save(ls.BridgeRoutedAnnualMax,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

# perform test
ADtestBridgesAnnualMax <- sapply(dists, 
                              function(dist_i) sapply(names(ls.BridgeRoutedAnnualMax), 
                                                      function(j) hydrologyDistrAssess(ls.BridgeRoutedAnnualMax[[j]], dist_i, name = j),
                                                      simplify = FALSE,
                                                      USE.NAMES = TRUE
                              ),
                              simplify = FALSE,
                              USE.NAMES = TRUE
)

# analyze results of test (find minimum AD for each location, then look at distribution of selections)
minADdistAnnualMax <- sapply(names(ls.BridgeRoutedAnnualMax), 
                             function(j) dists[which.min(c(ADtestBridgesAnnualMax[[dists[1]]][[j]]["AD"],
                                                           ADtestBridgesAnnualMax[[dists[2]]][[j]]["AD"],
                                                           ADtestBridgesAnnualMax[[dists[3]]][[j]]["AD"],
                                                           ADtestBridgesAnnualMax[[dists[4]]][[j]]["AD"],
                                                           ADtestBridgesAnnualMax[[dists[5]]][[j]]["AD"]))
                                               ],
                             simplify = "array")
table(factor(minADdistAnnualMax))
#  GEV GUMBEL     LN    LP3     P3 
#  140     50     18     71    108
# Interesting... So with annual max the preferred distributions change.


########### ANDERSON-DARLING TEST GAGES TO SELECT POTENTIAL DISTRIBUTIONS ############################
# test full dataset -----------
load(file.path(dirs$DataDirFrequent,"20150807_GageRoutingVICDaymet.RData"))
ADtestGages <- sapply(dists, 
                      function(dist_i) sapply(names(ls.GageRouted), 
                                              function(j) hydrologyDistrAssess(ls.GageRouted[[j]][["day"]]$RUNOFF_CFS, dist_i, name = j),
                                              simplify = FALSE,
                                              USE.NAMES = TRUE
                                              ),
                      simplify = FALSE,
                      USE.NAMES = TRUE
                      )

# analyze results of test (find minimum AD for each location, then look at distribution of selections)
minADdist <- sapply(names(ls.GageRouted), 
                    function(j) dists[which.min(c(ADtestGages[[dists[1]]][[j]]["AD"],
                                                  ADtestGages[[dists[2]]][[j]]["AD"],
                                                  ADtestGages[[dists[3]]][[j]]["AD"],
                                                  ADtestGages[[dists[4]]][[j]]["AD"],
                                                  ADtestGages[[dists[5]]][[j]]["AD"]))
                                      ],
                    simplify = "array")
table(factor(minADdist))
# GEV GUMBEL     LN    LP3     P3 
#  88      1     14    165     50 
# Still dominated by LP3, second by GEV. Seems like the best overall strategy is to use LP3 and GEV.
rm(ls.GageRouted,minADdist,dists,ADtestGages)

# test yearly extremes  -------------------------------------------------------------------
# obtain annual maxes
ls.GageRoutedAnnualMax <- lapply(names(ls.GageRouted), 
                                   function(j) sapply(c(1980:2013), 
                                                      function(y) max(ls.GageRouted[[j]]$day$RUNOFF_CFS[ls.GageRouted[[j]]$day$YEAR == y], 
                                                                      na.rm=TRUE)
                                   )
)
names(ls.GageRoutedAnnualMax) <- names(ls.GageRouted)

savefile <- paste(gsub("-","",Sys.Date()),"GageRoutingVICDaymetAnnualMaxes.RData",sep="_")
save(ls.GageRoutedAnnualMax,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

# perform test
ADtestGagesAnnualMax <- sapply(dists, 
                                 function(dist_i) sapply(names(ls.GageRoutedAnnualMax), 
                                                         function(j) hydrologyDistrAssess(ls.GageRoutedAnnualMax[[j]], dist_i, name = j),
                                                         simplify = FALSE,
                                                         USE.NAMES = TRUE
                                 ),
                                 simplify = FALSE,
                                 USE.NAMES = TRUE
)

# analyze results of test (find minimum AD for each location, then look at distribution of selections)
minADdistAnnualMax <- sapply(names(ls.GageRoutedAnnualMax), 
                             function(j) dists[which.min(c(ADtestGagesAnnualMax[[dists[1]]][[j]]["AD"],
                                                           ADtestGagesAnnualMax[[dists[2]]][[j]]["AD"],
                                                           ADtestGagesAnnualMax[[dists[3]]][[j]]["AD"],
                                                           ADtestGagesAnnualMax[[dists[4]]][[j]]["AD"],
                                                           ADtestGagesAnnualMax[[dists[5]]][[j]]["AD"]))
                                               ],
                             simplify = "array")
table(factor(minADdistAnnualMax))
#  GEV GUMBEL     LN    LP3     P3 
#  119     51     14     63     71
# GEV more popular, LP3 much less, P3 up

######## OBTAIN PARAMETERS FOR SELECTED DISTRIBUTIONS AND ANALYZE GOODNESS OF FIT USING A-D AND K-S ####

# BRIDGES ------------------------
# test full dataset -------------
load(file.path(dirs$DataDirFrequent,"20150807_BridgeRoutingVICDaymet.RData"))
n             <- nrow(ls.BridgeRouted[[1]][[1]])
dists <- c("LP3","GEV")
alphas        <- c(0.01,0.05,0.1)
paramMaxLikelihoodBridge <- sapply(dists, 
                                   function(dist_i) sapply(names(ls.BridgeRouted), 
                                                           function(j) hydrologyDistrAssess(ls.BridgeRouted[[j]][["day"]]$RUNOFF_CFS, dist_i, name = j, paramFlag = TRUE, ADpFlag = TRUE),
                                                           simplify = FALSE,
                                                           USE.NAMES = TRUE
                                                           ),
                                   simplify = FALSE,
                                   USE.NAMES = TRUE
                                   )
mlBridges <- sapply(dists, 
                    function(dist_i) sapply(names(paramMaxLikelihoodBridge[[dist_i]]), 
                                            function(j) paramMaxLikelihoodBridge[[dist_i]][[j]][["ml"]],
                                            simplify = FALSE,
                                            USE.NAMES = TRUE
                    ),
                    simplify = FALSE,
                    USE.NAMES = TRUE
)
savefile <- paste(gsub("-","",Sys.Date()),"Bridge_Daymet_VIC_LP3_GEV_Param.RData",sep="_")
save(paramMaxLikelihoodBridge,mlBridges,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)


ADbridges <- sapply(dists, 
                    function(dist_i) sapply(names(paramMaxLikelihoodBridge[[dist_i]]), 
                                            function(j) paramMaxLikelihoodBridge[[dist_i]][[j]][["AD"]][1]
                                           ),
                    simplify = FALSE,
                    USE.NAMES = TRUE
                    )
pADbridges <- sapply(dists, 
                    function(dist_i) sapply(names(paramMaxLikelihoodBridge[[dist_i]]), 
                                            function(j) paramMaxLikelihoodBridge[[dist_i]][[j]][["AD"]][2]
                                            ),
                    simplify = FALSE,
                    USE.NAMES = TRUE
                    )
nADbridgesPassed <- sapply(dists, function(dist_i) sapply(alphas,
                                                          function(alpha) sum(pADbridges[[dist_i]] <= 1-alpha, na.rm = TRUE)))
# In general, the AD values are very large (far above the ~1 or so they're allowed to be to accept the null hypothesis at the 1% level)
# After significant playing around with A2_GOFlaio, seems like want the p-values (and A values) to be as small as possible. So basically
# no distributions are passing.
RUNOFF_CFS <- sapply(names(ls.BridgeRouted), function(j) sort(ls.BridgeRouted[[j]]$day$RUNOFF_CFS),
                     simplify = FALSE,
                     USE.NAMES = TRUE
                      )
DnBridges     <- sapply(dists, 
                        function(dist_i) sapply(1:length(paramMaxLikelihoodBridge[[dist_i]]), 
                                                function(j) switch(dist_i,
                                                                   LP3 = max(abs(F.gamma(log(RUNOFF_CFS[[j]]), mlBridges[[dist_i]][[j]][1],mlBridges[[dist_i]][[j]][2],mlBridges[[dist_i]][[j]][3]) - 
                                                                               sapply(RUNOFF_CFS[[j]], function(i) which(RUNOFF_CFS[[j]]==i)[1]/length(RUNOFF_CFS[[j]])) )),
                                                                   GEV = max(abs(F.GEV(RUNOFF_CFS[[j]], mlBridges[[dist_i]][[j]][1],mlBridges[[dist_i]][[j]][2],mlBridges[[dist_i]][[j]][3]) - 
                                                                                   sapply(RUNOFF_CFS[[j]], function(i) which(RUNOFF_CFS[[j]]==i)[1]/length(RUNOFF_CFS[[j]])) )))
                                                ),
                        simplify = FALSE,
                        USE.NAMES = TRUE
                        )
KSbridges   <- sapply(dists,
                      function(dist_i) sapply(alphas, 
                                              function(alpha) sapply(1:length(DnBridges[[dist_i]]),
                                                                     function(j) ifelse(!is.null(DnBridges[[dist_i]][j]) & !is.na(DnBridges[[dist_i]][j]),
                                                                                        ks_test_approx_MF(DnBridges[[dist_i]][j],n,alpha),
                                                                                        NA),
                                                                     USE.NAMES = TRUE
                                                                      ),
                                              simplify = FALSE,
                                              USE.NAMES = TRUE
                                              ),
                      simplify = FALSE,
                      USE.NAMES = TRUE
                      )
nKSbridgesPassed <- sapply(dists, function(dist_i) sapply(1:length(alphas),
                                                          function(alpha) sum(KSbridges[[dist_i]][[alpha]], na.rm = TRUE)))
# only about 4.6% pass the KS test for LP3 at alpha=0.01, 1.3% for GEV. 3.3% and 0.8%, respectively, for alpha = 0.05.

# Check a few distributions by plotting to see what's going on
library(ggplot2)
j <- j+1
df <- data.frame(x    = sort(RUNOFF_CFS[[j]]),
                 FGEV = F.GEV(sort(RUNOFF_CFS[[j]]),mlBridges[["GEV"]][[j]][1],mlBridges[["GEV"]][[j]][2],mlBridges[["GEV"]][[j]][3]),
                 FLP3 = F.gamma(sort(log(RUNOFF_CFS[[j]])),mlBridges[["LP3"]][[j]][1],mlBridges[["LP3"]][[j]][2],mlBridges[["LP3"]][[j]][3])
                 )
ggplot(data=df) + stat_ecdf(aes(x=x)) + geom_line(aes(x=x,y=FGEV),color="red") + geom_line(aes(x=x,y=FLP3),color="blue")
# visual inspection shows that LP3 generally fits better, and GEV goes from being somewhat OK to very far off

# seems like also need to do something similar to partial duration--just straight up use the ECDF and find out % of time exceeded.
savefile <- paste(gsub("-","",Sys.Date()),"Bridge_Daymet_VIC_LP3_GEV_GoodnessFit_Bridge.RData",sep="_")
save(ADbridges,pADbridges,nADbridgesPassed,KSbridges,nKSbridgesPassed,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
rm(ADbridges,DnBridges,KSbridges,mlBridges,ls.BridgeRouted,pADbridges,paramMaxLikelihoodBridge,RUNOFF_CFS,nADbridgesPassed,nKSbridgesPassed)

# test annual peaks dataset -------------
load(file.path(dirs$DataDirAnalysis,"20150810_BridgeRoutingVICDaymetAnnualMaxes.RData"))
dists         <- c("GEV","P3","LP3")
alphas        <- c(0.01,0.05,0.1)
n             <- length(ls.BridgeRoutedAnnualMax[[1]])
paramMaxLikelihoodBridgeAnnualMax <- sapply(dists, 
                                   function(dist_i) sapply(names(ls.BridgeRoutedAnnualMax), 
                                                           function(j) hydrologyDistrAssess(ls.BridgeRoutedAnnualMax[[j]], dist_i, name = j, paramFlag = TRUE, ADpFlag = TRUE),
                                                           simplify = FALSE,
                                                           USE.NAMES = TRUE
                                   ),
                                   simplify = FALSE,
                                   USE.NAMES = TRUE
)
mlBridgesAnnualMax <- sapply(dists, 
                    function(dist_i) sapply(names(paramMaxLikelihoodBridgeAnnualMax[[dist_i]]), 
                                            function(j) paramMaxLikelihoodBridgeAnnualMax[[dist_i]][[j]][["ml"]],
                                            simplify = FALSE,
                                            USE.NAMES = TRUE
                    ),
                    simplify = FALSE,
                    USE.NAMES = TRUE
)
savefile <- paste(gsub("-","",Sys.Date()),"Bridge_Daymet_VIC_Annual_Max_P3_LP3_GEV_Param.RData",sep="_")
save(paramMaxLikelihoodBridgeAnnualMax,mlBridgesAnnualMax,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)


ADbridgesAnnualMax <- sapply(dists, 
                    function(dist_i) sapply(names(paramMaxLikelihoodBridgeAnnualMax[[dist_i]]), 
                                            function(j) paramMaxLikelihoodBridgeAnnualMax[[dist_i]][[j]][["AD"]][1]
                    ),
                    simplify = FALSE,
                    USE.NAMES = TRUE
)
pADbridgesAnnualMax <- sapply(dists, 
                     function(dist_i) sapply(names(paramMaxLikelihoodBridgeAnnualMax[[dist_i]]), 
                                             function(j) paramMaxLikelihoodBridgeAnnualMax[[dist_i]][[j]][["AD"]][2]
                     ),
                     simplify = FALSE,
                     USE.NAMES = TRUE
)
nADbridgesPassedAnnualMax <- sapply(dists, function(dist_i) sapply(alphas,
                                                          function(alpha) sum(pADbridgesAnnualMax[[dist_i]] <= 1 - alpha, na.rm = TRUE)))
# Once flipping to 1-alpha (because pAD is a probability of non-exceedence) we get that most of them pass
# GEV 366/338/308, P3 349/298/267, LP3 348/306/270
RUNOFF_CFS <- sapply(names(ls.BridgeRoutedAnnualMax), function(j) sort(ls.BridgeRoutedAnnualMax[[j]]),
                     simplify = FALSE,
                     USE.NAMES = TRUE
)
DnBridges     <- sapply(dists, 
                        function(dist_i) sapply(1:length(paramMaxLikelihoodBridgeAnnualMax[[dist_i]]), 
                                                function(j) switch(dist_i,
                                                                   LP3 = max(abs(F.gamma(log(RUNOFF_CFS[[j]]), mlBridgesAnnualMax[[dist_i]][[j]][1],mlBridgesAnnualMax[[dist_i]][[j]][2],mlBridgesAnnualMax[[dist_i]][[j]][3]) - 
                                                                                   sapply(RUNOFF_CFS[[j]], function(i) which(RUNOFF_CFS[[j]]==i)[1]/length(RUNOFF_CFS[[j]])) )),
                                                                   P3 = max(abs(F.gamma(RUNOFF_CFS[[j]], mlBridgesAnnualMax[[dist_i]][[j]][1],mlBridgesAnnualMax[[dist_i]][[j]][2],mlBridgesAnnualMax[[dist_i]][[j]][3]) - 
                                                                                   sapply(RUNOFF_CFS[[j]], function(i) which(RUNOFF_CFS[[j]]==i)[1]/length(RUNOFF_CFS[[j]])) )),
                                                                   GEV = max(abs(F.GEV(RUNOFF_CFS[[j]], mlBridgesAnnualMax[[dist_i]][[j]][1],mlBridgesAnnualMax[[dist_i]][[j]][2],mlBridgesAnnualMax[[dist_i]][[j]][3]) - 
                                                                                   sapply(RUNOFF_CFS[[j]], function(i) which(RUNOFF_CFS[[j]]==i)[1]/length(RUNOFF_CFS[[j]])) )))
                        ),
                        simplify = FALSE,
                        USE.NAMES = TRUE
)
KSbridgesAnnualMax   <- sapply(dists,
                      function(dist_i) sapply(alphas, 
                                              function(alpha) sapply(1:length(DnBridges[[dist_i]]),
                                                                     function(j) ifelse(!is.null(DnBridges[[dist_i]][j]) & !is.na(DnBridges[[dist_i]][j]),
                                                                                        ks_test_approx_MF(DnBridges[[dist_i]][j],n,alpha),
                                                                                        NA),
                                                                     USE.NAMES = TRUE
                                              ),
                                              simplify = FALSE,
                                              USE.NAMES = TRUE
                      ),
                      simplify = FALSE,
                      USE.NAMES = TRUE
) # returns TRUE or FALSE for given value of alpha
nKSbridgesPassedAnnualMax <- sapply(dists, function(dist_i) sapply(1:length(alphas),
                                                          function(alpha) sum(KSbridgesAnnualMax[[dist_i]][[alpha]], na.rm = TRUE)))
# All of them pass the KS test for GEV and LP3, 384/381/375 pass for P3

# Check a few distributions by plotting to see what's going on
library(ggplot2)
j <-0
j <- j+1
df <- data.frame(x    = sort(RUNOFF_CFS[[j]]),
                 FGEV = F.GEV(sort(RUNOFF_CFS[[j]]),mlBridgesAnnualMax[["GEV"]][[j]][1],mlBridgesAnnualMax[["GEV"]][[j]][2],mlBridgesAnnualMax[["GEV"]][[j]][3]),
                 FLP3 = F.gamma(sort(log(RUNOFF_CFS[[j]])),mlBridgesAnnualMax[["LP3"]][[j]][1],mlBridgesAnnualMax[["LP3"]][[j]][2],mlBridgesAnnualMax[["LP3"]][[j]][3]),
                 FP3  = F.gamma(sort(RUNOFF_CFS[[j]]),mlBridgesAnnualMax[["P3"]][[j]][1],mlBridgesAnnualMax[["P3"]][[j]][2],mlBridgesAnnualMax[["P3"]][[j]][3])
)
ggplot(data=df) + stat_ecdf(aes(x=x)) + geom_line(aes(x=x,y=FGEV),color="red") + geom_line(aes(x=x,y=FLP3),color="blue") + geom_line(aes(x=x,y=FP3),color="green")

# All fits are very similar.

# seems like also need to do something similar to partial duration--just straight up use the ECDF and find out % of time exceeded.
savefile <- paste(gsub("-","",Sys.Date()),"Bridge_Daymet_VIC_AnnualMax_P3_LP3_GEV_GoodnessFit_Bridge.RData",sep="_")
save(ADbridgesAnnualMax,pADbridgesAnnualMax,nADbridgesPassedAnnualMax,KSbridgesAnnualMax,nKSbridgesPassedAnnualMax,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
rm(ADbridgesAnnualMax,DnBridges,KSbridgesAnnualMax,mlBridgesAnnualMax,ls.BridgeRoutedAnnualMax,pADbridgesAnnualMax,paramMaxLikelihoodBridgeAnnualMax,RUNOFF_CFS,nADbridgesPassedAnnualMax,nKSbridgesPassedAnnualMax)

# GAGES ----------------------------
# test full dataset -----------
load(file.path(dirs$DataDirFrequent,"20150807_GageRoutingVICDaymet.RData"))
n             <- nrow(ls.GageRouted[[1]][[1]])
dists <- c("LP3","GEV")
paramMaxLikelihoodGage <- sapply(dists, 
                                   function(dist_i) sapply(names(ls.GageRouted), 
                                                           function(j) hydrologyDistrAssess(ls.GageRouted[[j]][["day"]]$RUNOFF_CFS, dist_i, name = j, paramFlag = TRUE, ADpFlag = TRUE),
                                                           simplify = FALSE,
                                                           USE.NAMES = TRUE
                                   ),
                                   simplify = FALSE,
                                   USE.NAMES = TRUE
)
mlGages <- sapply(dists, 
                    function(dist_i) sapply(names(paramMaxLikelihoodGage[[dist_i]]), 
                                            function(j) paramMaxLikelihoodGage[[dist_i]][[j]][["ml"]],
                                            simplify = FALSE,
                                            USE.NAMES = TRUE
                    ),
                    simplify = FALSE,
                    USE.NAMES = TRUE
)
savefile <- paste(gsub("-","",Sys.Date()),"Gage_Daymet_VIC_LP3_GEV_Param.RData",sep="_")
save(paramMaxLikelihoodGage,mlGages,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)


ADGages <- sapply(dists, 
                    function(dist_i) sapply(names(paramMaxLikelihoodGage[[dist_i]]), 
                                            function(j) paramMaxLikelihoodGage[[dist_i]][[j]][["AD"]][1]
                    ),
                    simplify = FALSE,
                    USE.NAMES = TRUE
)
pADGages <- sapply(dists, 
                     function(dist_i) sapply(names(paramMaxLikelihoodGage[[dist_i]]), 
                                             function(j) paramMaxLikelihoodGage[[dist_i]][[j]][["AD"]][2]
                     ),
                     simplify = FALSE,
                     USE.NAMES = TRUE
)
nADGagesPassed <- sapply(dists, function(dist_i) sapply(alphas,
                                                          function(alpha) sum(pADGages[[dist_i]] <= 1- alpha, na.rm = TRUE)))
# In general, the AD values are very large--mean is 62, so better than for bridges but not by much. None pass A2.

RUNOFF_CFS <- sapply(names(ls.GageRouted), function(j) sort(ls.GageRouted[[j]]$day$RUNOFF_CFS),
                     simplify = FALSE,
                     USE.NAMES = TRUE
)
# In order to get this to work, going to put in dummy ML values for my problem gauge 9512200
paramMaxLikelihoodGage[["LP3"]][["9512200"]]$ml <- c(1,1,1)
DnGages     <- sapply(dists, 
                        function(dist_i) sapply(names(paramMaxLikelihoodGage[[dist_i]]), 
                                                function(j) switch(dist_i,
                                                                   LP3 = max(abs(F.gamma(log(RUNOFF_CFS[[j]]), mlGages[[dist_i]][[j]][1],mlGages[[dist_i]][[j]][2],mlGages[[dist_i]][[j]][3]) - 
                                                                                   sapply(RUNOFF_CFS[[j]], function(i) which(RUNOFF_CFS[[j]]==i)[1]/length(RUNOFF_CFS[[j]])) )),
                                                                   GEV = max(abs(F.GEV(RUNOFF_CFS[[j]], mlGages[[dist_i]][[j]][1],mlGages[[dist_i]][[j]][2],mlGages[[dist_i]][[j]][3]) - 
                                                                                   sapply(RUNOFF_CFS[[j]], function(i) which(RUNOFF_CFS[[j]]==i)[1]/length(RUNOFF_CFS[[j]])) )))
                        ),
                        simplify = FALSE,
                        USE.NAMES = TRUE
)
KSGages   <- sapply(dists,
                      function(dist_i) sapply(alphas, 
                                              function(alpha) sapply(1:length(DnGages[[dist_i]]),
                                                                     function(j) ifelse(!is.null(DnGages[[dist_i]][j]) & !is.na(DnGages[[dist_i]][j]),
                                                                                        ks_test_approx_MF(DnGages[[dist_i]][j],n,alpha),
                                                                                        NA),
                                                                     USE.NAMES = TRUE
                                              ),
                                              simplify = FALSE,
                                              USE.NAMES = TRUE
                      ),
                      simplify = FALSE,
                      USE.NAMES = TRUE
)
nKSGagesPassed <- sapply(dists, function(dist_i) sapply(1:length(alphas),
                                                          function(alpha) sum(KSGages[[dist_i]][[alpha]], na.rm = TRUE)))
# only about 5% pass the KS test for LP3 at alpha=0.01, 1.2% for GEV. 3.1% and 0%, respectively, for alpha = 0.05.

# Check a few distributions by plotting to see what's going on
library(ggplot2)
j <- j+1
df <- data.frame(x    = sort(RUNOFF_CFS[[j]]),
                 FGEV = F.GEV(sort(RUNOFF_CFS[[j]]),mlGages[["GEV"]][[j]][1],mlGages[["GEV"]][[j]][2],mlGages[["GEV"]][[j]][3]),
                 FLP3 = F.gamma(sort(log(RUNOFF_CFS[[j]])),mlGages[["LP3"]][[j]][1],mlGages[["LP3"]][[j]][2],mlGages[["LP3"]][[j]][3])
)
ggplot(data=df) + stat_ecdf(aes(x=x)) + geom_line(aes(x=x,y=FGEV),color="red") + geom_line(aes(x=x,y=FLP3),color="blue")
# visual inspection shows that LP3 generally fits better, and GEV goes from being somewhat OK to very far off


savefile <- paste(gsub("-","",Sys.Date()),"Gage_Daymet_VIC_LP3_GEV_GoodnessFit.RData",sep="_")
save(ADGages,pADGages,nADGagesPassed,KSGages,nKSGagesPassed,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
rm(ADGages,DnGages,KSGages,mlGages,ls.GageRouted,pADGages,paramMaxLikelihoodGage,RUNOFF_CFS,nADGagesPassed,nKSGagesPassed,alphas,n,dists)
rm(A2_GOFlaio_MF,hydrologyDistrAssess,ks_test_approx_MF)

# test annual peaks dataset -----
load(file.path(dirs$DataDirAnalysis,"20150813_GageRoutingVICDaymetAnnualMaxes.RData"))
dists         <- c("GEV","P3","LP3")
alphas        <- c(0.01,0.05,0.1)
n             <- length(ls.GageRoutedAnnualMax[[1]])
paramMaxLikelihoodGageAnnualMax <- sapply(dists, 
                                            function(dist_i) sapply(names(ls.GageRoutedAnnualMax), 
                                                                    function(j) hydrologyDistrAssess(ls.GageRoutedAnnualMax[[j]], dist_i, name = j, paramFlag = TRUE, ADpFlag = TRUE),
                                                                    simplify = FALSE,
                                                                    USE.NAMES = TRUE
                                            ),
                                            simplify = FALSE,
                                            USE.NAMES = TRUE
)
mlGagesAnnualMax <- sapply(dists, 
                             function(dist_i) sapply(names(paramMaxLikelihoodGageAnnualMax[[dist_i]]), 
                                                     function(j) paramMaxLikelihoodGageAnnualMax[[dist_i]][[j]][["ml"]],
                                                     simplify = FALSE,
                                                     USE.NAMES = TRUE
                             ),
                             simplify = FALSE,
                             USE.NAMES = TRUE
)
savefile <- paste(gsub("-","",Sys.Date()),"Gage_Daymet_VIC_Annual_Max_P3_LP3_GEV_Param.RData",sep="_")
save(paramMaxLikelihoodGageAnnualMax,mlGagesAnnualMax,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)


ADGagesAnnualMax <- sapply(dists, 
                             function(dist_i) sapply(names(paramMaxLikelihoodGageAnnualMax[[dist_i]]), 
                                                     function(j) paramMaxLikelihoodGageAnnualMax[[dist_i]][[j]][["AD"]][1]
                             ),
                             simplify = FALSE,
                             USE.NAMES = TRUE
)
pADGagesAnnualMax <- sapply(dists, 
                              function(dist_i) sapply(names(paramMaxLikelihoodGageAnnualMax[[dist_i]]), 
                                                      function(j) paramMaxLikelihoodGageAnnualMax[[dist_i]][[j]][["AD"]][2]
                              ),
                              simplify = FALSE,
                              USE.NAMES = TRUE
)
nADGagesPassedAnnualMax <- sapply(dists, function(dist_i) sapply(alphas,
                                                                   function(alpha) sum(pADGagesAnnualMax[[dist_i]] <= 1-alpha, na.rm = TRUE)))
# AD values are again reasonably low, and several of the distributions are passing. According to the documentation, pA is the probability
# of non-exceedence. Like there's only a 5% chance of the statistic being that small if they were not a good match.
# when I flip alpha in that way, I get 220-300 of them passing, which is much better in line with K-S.
#      GEV  P3 LP3
# [1,] 301 288 287
# [2,] 280 240 252
# [3,] 257 217 223
RUNOFF_CFS <- sapply(names(ls.GageRoutedAnnualMax), function(j) sort(ls.GageRoutedAnnualMax[[j]]),
                     simplify = FALSE,
                     USE.NAMES = TRUE
)
mlGagesAnnualMax[["P3"]][["9512200"]] <- c(1,1,1)
DnGages     <- sapply(dists, 
                        function(dist_i) sapply(1:length(paramMaxLikelihoodGageAnnualMax[[dist_i]]), 
                                                function(j) switch(dist_i,
                                                                   LP3 = max(abs(F.gamma(log(RUNOFF_CFS[[j]]), mlGagesAnnualMax[[dist_i]][[j]][1],mlGagesAnnualMax[[dist_i]][[j]][2],mlGagesAnnualMax[[dist_i]][[j]][3]) - 
                                                                                   sapply(RUNOFF_CFS[[j]], function(i) which(RUNOFF_CFS[[j]]==i)[1]/length(RUNOFF_CFS[[j]])) )),
                                                                   P3 = max(abs(F.gamma(RUNOFF_CFS[[j]], mlGagesAnnualMax[[dist_i]][[j]][1],mlGagesAnnualMax[[dist_i]][[j]][2],mlGagesAnnualMax[[dist_i]][[j]][3]) - 
                                                                                  sapply(RUNOFF_CFS[[j]], function(i) which(RUNOFF_CFS[[j]]==i)[1]/length(RUNOFF_CFS[[j]])) )),
                                                                   GEV = max(abs(F.GEV(RUNOFF_CFS[[j]], mlGagesAnnualMax[[dist_i]][[j]][1],mlGagesAnnualMax[[dist_i]][[j]][2],mlGagesAnnualMax[[dist_i]][[j]][3]) - 
                                                                                   sapply(RUNOFF_CFS[[j]], function(i) which(RUNOFF_CFS[[j]]==i)[1]/length(RUNOFF_CFS[[j]])) )))
                        ),
                        simplify = FALSE,
                        USE.NAMES = TRUE
)
KSGagesAnnualMax   <- sapply(dists,
                               function(dist_i) sapply(alphas, 
                                                       function(alpha) sapply(1:length(DnGages[[dist_i]]),
                                                                              function(j) ifelse(!is.null(DnGages[[dist_i]][j]) & !is.na(DnGages[[dist_i]][j]),
                                                                                                 ks_test_approx_MF(DnGages[[dist_i]][j],n,alpha),
                                                                                                 NA),
                                                                              USE.NAMES = TRUE
                                                       ),
                                                       simplify = FALSE,
                                                       USE.NAMES = TRUE
                               ),
                               simplify = FALSE,
                               USE.NAMES = TRUE
)
nKSGagesPassedAnnualMax <- sapply(dists, function(dist_i) sapply(1:length(alphas),
                                                                           function(alpha) sum(KSGagesAnnualMax[[dist_i]][[alpha]], na.rm = TRUE)))
# All pass for GEV and LP3, 314/313/312 pass for P3 (and 1 not fitted for P3)

# Check a few distributions by plotting to see what's going on
library(ggplot2)
j <-0
j <- j+1
df <- data.frame(x    = sort(RUNOFF_CFS[[j]]),
                 FGEV = F.GEV(sort(RUNOFF_CFS[[j]]),mlGagesAnnualMax[["GEV"]][[j]][1],mlGagesAnnualMax[["GEV"]][[j]][2],mlGagesAnnualMax[["GEV"]][[j]][3]) ,
                 FLP3 = F.gamma(sort(log(RUNOFF_CFS[[j]])),mlGagesAnnualMax[["LP3"]][[j]][1],mlGagesAnnualMax[["LP3"]][[j]][2],mlGagesAnnualMax[["LP3"]][[j]][3]),
                 FP3  = F.gamma(sort(RUNOFF_CFS[[j]]),mlGagesAnnualMax[["P3"]][[j]][1],mlGagesAnnualMax[["P3"]][[j]][2],mlGagesAnnualMax[["P3"]][[j]][3])
)
ggplot(data=df) + stat_ecdf(aes(x=x)) + geom_line(aes(x=x,y=FGEV),color="red") + geom_line(aes(x=x,y=FLP3),color="blue") + geom_line(aes(x=x,y=FP3),color="green")

gammaplot(log(ls.BridgeRoutedAnnualMax[[j]]),shape=mlBridgesAnnualMax[["LP3"]][[j]][3])
gammaplot(ls.BridgeRoutedAnnualMax[[j]],shape=mlBridgesAnnualMax[["P3"]][[j]][3])
weibullplot(ls.BridgeRoutedAnnualMax[[j]]))
frechetplot(ls.BridgeRoutedAnnualMax[[j]]))
gumbelplot(ls.BridgeRoutedAnnualMax[[j]]))

# All fits are very similar.

savefile <- paste(gsub("-","",Sys.Date()),"Gage_Daymet_VIC_AnnualMax_P3_LP3_GEV_GoodnessFit_Gage.RData",sep="_")
save(ADGagesAnnualMax,pADGagesAnnualMax,nADGagesPassedAnnualMax,KSGagesAnnualMax,nKSGagesPassedAnnualMax,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
rm(ADGagesAnnualMax,DnGages,KSGagesAnnualMax,mlGagesAnnualMax,ls.GageRoutedAnnualMax,pADGagesAnnualMax,paramMaxLikelihoodGageAnnualMax,RUNOFF_CFS,nADGagesPassedAnnualMax,nKSGagesPassedAnnualMax)

###### PARTIAL DURATION ANALYSIS ##################################################
# Bridge
load(file.path(dirs$DataDirFrequent,"20151203_BridgeRoutingVICDaymet.RData"))
ecdfBridgeDaily <- list()
for (ID in names(ls.BridgeRouted)){
  ecdfBridgeDaily[[ID]] <- ecdf(ls.BridgeRouted[[ID]]$day$RUNOFF_CFS)
}
rm(ls.BridgeRouted)

# Gage
load(file.path(dirs$DataDirFrequent,"20151203_GageRoutingVICDaymet.RData"))
ecdfGageDaily <- list()
for (STAID in names(ls.GageRouted)){
  ecdfGageDaily[[STAID]] <- ecdf(ls.GageRouted[[STAID]]$day$RUNOFF_CFS)
}
rm(ls.GageRouted)

savefile <- paste(gsub("-","",Sys.Date()),"ecdfGagesDailyData.RData",sep="_")
save(ecdfGageDaily,file=file.path(dirs$DataDirAnalysis,savefile))
savefile <- paste(gsub("-","",Sys.Date()),"ecdfBrigesDailyData.RData",sep="_")
save(ecdfBridgeDaily,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

