# 2015-09-09 Fit distributions to NLDAS-VIC Gridded Data

require(nsRFA)
require(MASS)
source(file.path(dirsGit$ScriptsDir,"hydrologyDistrAssess.R"))
source(file.path(dirsGit$ScriptsDir,"A2_GOFlaio_MF.R"))
source(file.path(dirsGit$ScriptsDir,"ks_test_approx_MF.R"))
source(file.path(dirs$UtilityScriptsDir,"gcd_slc.R"))
source(file.path(dirs$UtilityScriptsDir,"deg2rad.R"))

# use nsRFA package to test a variety of distributions using the Anderson Darling test
dists <- c("LN","GUMBEL","GEV","P3","LP3")

########### ANDERSON-DARLING TEST NLDASS TO SELECT POTENTIAL DISTRIBUTIONS ############################
# test full dataset -------------------------------------------------------------------
load(file.path(dirs$DataDirFrequent,"20150604_NLDAS_Daily_Means_36_years.RData"))
rowsToAnalyzeNLDAS <- rowsToView[df.Fail.NBI.Gage[rowsToView,"YR_FAIL"] > as.Date("1979-01-01")]
IDsToAnalyzeNLDAS  <- as.character(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"ID"])
nNLDAS             <- length(rowsToAnalyzeNLDAS)

years         <- as.character(1979:2014)
Lats          <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"LATDD"]
Lons          <- df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"LONGDD"]
Dates         <- as.Date(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DATE_FAIL_EST_USGS"])
Years         <- format.Date(df.Fail.NBI.Gage[rowsToAnalyzeNLDAS,"DATE_FAIL_EST_USGS"],"%Y")

Lon_index      <- integer(nNLDAS)
Lat_index      <- integer(nNLDAS)
Day_index      <- integer(nNLDAS)
min_dist       <- numeric(nNLDAS)

for (i in 1:nNLDAS){
  ii            <- which(years==Years[i])
  Lon_index[i]  <- which(order(c(NLDAS_daily_all[[ii]]$lon,Lons[i]))==(NLDAS_daily_all[[ii]]$nlon+1))
  Lat_index[i]  <- which(order(c(NLDAS_daily_all[[ii]]$lat,Lats[i]))==(NLDAS_daily_all[[ii]]$nlat+1))
  Day_index[i]  <- which(NLDAS_daily_all[[ii]]$days==Dates[i])
  lon_indices   <- c(Lon_index[i]-1,Lon_index[i]-1,Lon_index[i],Lon_index[i])
  lat_indices   <- c(Lat_index[i]-1,Lat_index[i],Lat_index[i]-1,Lat_index[i])
  d             <- numeric(4)
  for (j in 1:4){ # 4 points bounding NLDAS location
    d[j]        <- gcd_slc(deg2rad(NLDAS_daily_all[[ii]]$lon[lon_indices[j]]),
                           deg2rad(NLDAS_daily_all[[ii]]$lat[lat_indices[j]]),
                           deg2rad(Lons[i]),
                           deg2rad(Lats[i]))
  }
  nearest          <- which.min(d)
  Lon_index[i]     <- lon_indices[nearest]
  Lat_index[i]     <- lat_indices[nearest]
  min_dist[i]      <- d[nearest]
}

rm(d,nearest,lon_indices,lat_indices)
SSRUN <- sapply(IDsToAnalyzeNLDAS,
                function(ID) sapply(years,
                                    function(y) NLDAS_daily_all[[y]]$SSRUN[Lon_index[IDsToAnalyzeNLDAS==ID],Lat_index[IDsToAnalyzeNLDAS==ID],]
                                    ),
                USE.NAMES = TRUE,
                simplify = FALSE)
SSRUNpeaks <- sapply(IDsToAnalyzeNLDAS,
                     function(ID) sapply(years,
                                         function(y) max(SSRUN[[ID]][[y]],na.rm=TRUE)
                     ),
                     USE.NAMES = TRUE,
                     simplify = FALSE)
ADtest <- sapply(dists, 
                        function(dist_i) sapply(names(SSRUNpeaks), 
                                                function(j) hydrologyDistrAssess(SSRUNpeaks[[j]], dist_i, name = j),
                                                simplify = FALSE,
                                                USE.NAMES = TRUE
                                                ),
                        simplify = FALSE,
                        USE.NAMES = TRUE
                        )

# analyze results of test (find minimum AD for each location, then look at distribution of selections)
minADdist <- sapply(names(SSRUNpeaks), 
                    function(j) dists[which.min(c(ADtest[[dists[1]]][[j]]["AD"],
                                                  ADtest[[dists[2]]][[j]]["AD"],
                                                  ADtest[[dists[3]]][[j]]["AD"],
                                                  ADtest[[dists[4]]][[j]]["AD"],
                                                  ADtest[[dists[5]]][[j]]["AD"]))
                                      ],
                    simplify = "array")
table(factor(minADdist))
# GEV  LN LP3  P3 
# 13   3   8   3 
# So, as with USGS, GEV and LP3 are the best... Will continue with those.
savefile <- paste(gsub("-","",Sys.Date()),"NLDAS_Daily_Peaks_At_Bridges.RData",sep="_")
save(SSRUN,SSRUNpeaks,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
rm(NLDAS_daily_all,SSRUN)

# PARAMETERS -----
dists <- c("LP3","GEV")
alphas        <- c(0.01,0.05,0.1)
paramMaxLikelihoodNLDAS <- sapply(dists, 
                                   function(dist_i) sapply(names(SSRUNpeaks), 
                                                           function(j) hydrologyDistrAssess(SSRUNpeaks[[j]], dist_i, name = j, paramFlag = TRUE, ADpFlag = TRUE),
                                                           simplify = FALSE,
                                                           USE.NAMES = TRUE
                                                           ),
                                   simplify = FALSE,
                                   USE.NAMES = TRUE
                                   )
mlNLDASs <- sapply(dists, 
                    function(dist_i) sapply(names(paramMaxLikelihoodNLDAS[[dist_i]]), 
                                            function(j) paramMaxLikelihoodNLDAS[[dist_i]][[j]][["ml"]],
                                            simplify = FALSE,
                                            USE.NAMES = TRUE
                    ),
                    simplify = FALSE,
                    USE.NAMES = TRUE
)
savefile <- paste(gsub("-","",Sys.Date()),"NLDAS_Daymet_VIC_Peaks_LP3_GEV_Param.RData",sep="_")
save(paramMaxLikelihoodNLDAS,mlNLDASs,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)


ADNLDASs <- sapply(dists, 
                    function(dist_i) sapply(names(paramMaxLikelihoodNLDAS[[dist_i]]), 
                                            function(j) paramMaxLikelihoodNLDAS[[dist_i]][[j]][["AD"]][1]
                                           ),
                    simplify = FALSE,
                    USE.NAMES = TRUE
                    )
pADNLDASs <- sapply(dists, 
                    function(dist_i) sapply(names(paramMaxLikelihoodNLDAS[[dist_i]]), 
                                            function(j) paramMaxLikelihoodNLDAS[[dist_i]][[j]][["AD"]][2]
                                            ),
                    simplify = FALSE,
                    USE.NAMES = TRUE
                    )
nADNLDASsPassed <- sapply(dists, function(dist_i) sapply(alphas,
                                                          function(alpha) sum(pADNLDASs[[dist_i]] <= 1-alpha, na.rm = TRUE)))
#       LP3 GEV
# [1,]  27  27
# [2,]  24  25
# [3,]  23  25
# So they both work pretty well.
n <- length(years)
DnNLDASs     <- sapply(dists, 
                        function(dist_i) sapply(1:length(paramMaxLikelihoodNLDAS[[dist_i]]), 
                                                function(j) switch(dist_i,
                                                                   LP3 = max(abs(F.gamma(log(SSRUNpeaks[[j]]), mlNLDASs[[dist_i]][[j]][1],mlNLDASs[[dist_i]][[j]][2],mlNLDASs[[dist_i]][[j]][3]) - 
                                                                               sapply(SSRUNpeaks[[j]], function(i) which(SSRUNpeaks[[j]]==i)[1]/length(SSRUNpeaks[[j]])) )),
                                                                   GEV = max(abs(F.GEV(SSRUNpeaks[[j]], mlNLDASs[[dist_i]][[j]][1],mlNLDASs[[dist_i]][[j]][2],mlNLDASs[[dist_i]][[j]][3]) - 
                                                                                   sapply(SSRUNpeaks[[j]], function(i) which(SSRUNpeaks[[j]]==i)[1]/length(SSRUNpeaks[[j]])) )))
                                                ),
                        simplify = FALSE,
                        USE.NAMES = TRUE
                        )
KSNLDASs   <- sapply(dists,
                      function(dist_i) sapply(alphas, 
                                              function(alpha) sapply(1:length(DnNLDASs[[dist_i]]),
                                                                     function(j) ifelse(!is.null(DnNLDASs[[dist_i]][j]) & !is.na(DnNLDASs[[dist_i]][j]),
                                                                                        ks_test_approx_MF(DnNLDASs[[dist_i]][j],n,alpha),
                                                                                        NA),
                                                                     USE.NAMES = TRUE
                                                                      ),
                                              simplify = FALSE,
                                              USE.NAMES = TRUE
                                              ),
                      simplify = FALSE,
                      USE.NAMES = TRUE
                      )
nKSNLDASsPassed <- sapply(dists, function(dist_i) sapply(1:length(alphas),
                                                          function(alpha) sum(KSNLDASs[[dist_i]][[alpha]], na.rm = TRUE)))
# Saying that we're failed, but the plots look great. Maybe calc'ing wrong?

# Check a few distributions by plotting to see what's going on
require(ggplot2)
j<-0
j <- j+1
df <- data.frame(x    = sort(SSRUNpeaks[[j]]),
                 FGEV = F.GEV(sort(SSRUNpeaks[[j]]),mlNLDASs[["GEV"]][[j]][1],mlNLDASs[["GEV"]][[j]][2],mlNLDASs[["GEV"]][[j]][3]),
                 FLP3 = F.gamma(sort(log(SSRUNpeaks[[j]])),mlNLDASs[["LP3"]][[j]][1],mlNLDASs[["LP3"]][[j]][2],mlNLDASs[["LP3"]][[j]][3])
                 )
ggplot(data=df) + stat_ecdf(aes(x=x)) + geom_line(aes(x=x,y=FGEV),color="red") + geom_line(aes(x=x,y=FLP3),color="blue")
# visual inspection shows that LP3 and GEV generally very similar

savefile <- paste(gsub("-","",Sys.Date()),"NLDAS_Daymet_VIC_Peaks_LP3_GEV_GoodnessFit.RData",sep="_")
save(ADNLDASs,pADNLDASs,nADNLDASsPassed,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
rm(ADNLDASs,DnNLDASs,KSNLDASs,mlNLDASs,pADNLDASs,paramMaxLikelihoodNLDAS,SSRUNpeaks,nADNLDASsPassed,nKSNLDASsPassed)


###### PARTIAL DURATION ANALYSIS ##################################################
load(file.path(dirs$DataDirAnalysis,"20151217_NLDASGageDailyMeans.RData"))
load(file.path(dirs$DataDirAnalysis,"20151217_NLDASBridgeDailyMeans.RData"))

ecdfNLDASg <- sapply(names(ls.NLDAS.Gage), function(i) ecdf(ls.NLDAS.Gage[[i]]$SSRUN),
                     simplify = FALSE,
                     USE.NAMES = TRUE)
ecdfNLDASb <- sapply(names(ls.NLDAS.Bridge), function(i) ecdf(ls.NLDAS.Bridge[[i]]$SSRUN),
                     simplify = FALSE,
                     USE.NAMES = TRUE)

savefile <- paste(gsub("-","",Sys.Date()),"ecdfNLDASDailyData.RData",sep="_")
save(ecdfNLDASg,ecdfNLDASb,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)
rm(ecdfNLDASDaily,ADtest,alphas,Dates,Day_index,i,ID,IDsToAnalyzeNLDAS,ii,Lat_index,Lon_index,Lats,Lons,
   min_dist,minADdist,n,nNLDAS,rowsToAnalyzeNLDAS,Years,years,A2_GOFlaio_MF,deg2rad,gcd_slc,hydrologyDistrAssess,
   ks_test_approx_MF,dists,j,df.Fail.NBI.Gage)

