# 2015-08-17 Estimate return period of failure event from Daymet-VIC distributions by
# performing bootstrapping and block-bootstrapping to obtain a range of MLE parameters

require(stats)
require(nsRFA)
require(ggplot2)
require(MASS)

# Load and source ------------------
source(file.path(dirsGit$ScriptsDir,"hydrologyDistrAssess.R"))

load(file.path(dirs$DataDirAnalysis,"20150909_Gage_Daymet_VIC_Annual_Max_P3_LP3_GEV_Param.RData"))
load(file.path(dirs$DataDirAnalysis,"20150813_Bridge_Daymet_VIC_Annual_Max_P3_LP3_GEV_Param.RData"))
load(file.path(dirs$DataDirAnalysis,"20150909_GageRoutingVICDaymet.RData"))
load(file.path(dirs$DataDirFrequent,"20150807_BridgeRoutingVICDaymet.RData"))
load(file.path(dirs$DataDirAnalysis,"20150810_BridgeRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirs$DataDirAnalysis,"20150909_GageRoutingVICDaymetAnnualMaxes.RData"))
load(file.path(dirs$DataDirFrequent, "20151112_HEC_PFA_VICG.RData"))
load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))

# ESTIMATE RETURN PERIOD OF FAILURE FLOW USING MLE PARAMETERS FROM DAYMET-VIC ANNUAL PEAKS -----------------------
rowsToAnalyzeVIC  <- rownames(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView & df.Fail.NBI.Gage$YR_FAIL_EST >= "1980-01-01",])
IDsToAnalyzeVIC   <- as.character(df.Fail.NBI.Gage[rowsToAnalyzeVIC,"ID"])

STAIDtoAnalyzeVIC <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToAnalyzeVIC,"STAID"]
nIDsVIC           <- length(IDsToAnalyzeVIC)

# ESTIMATE UNCERTAINTY IN RETURN PERIOD USING BOOTSTRAPPING (NON-BLOCK) ############################## -------------
# BRIDGES - using +/- (PM) 2 days from given failure date to get best estimate of event from VIC -------------
nYr             <- length(ls.GageRoutedAnnualMax[[1]])
sampleLength    <- 25
nSamples        <- 1000
pExcfailEstSamples <- list()
pExcfailEstParam   <- list()
for (ID in IDsToAnalyzeVIC[1:5]){
  print(ID)
#   Qfail  <- switch(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"T_BEST_SOURCE"],
#                    DAILY = df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAIL_D_USGS"],
#                    PEAK  = df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAIL_P_USGS"],
#                    INST  =df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAIL_I_USGS"]
#   )

  pExcfailEstParam[[ID]]   <- list()
  pExcfailEstSamples[[ID]] <- list()
  for (dist_i in names(paramMaxLikelihoodBridgeAnnualMax)){
    for (j in 1:nSamples){
      Q      <- ls.BridgeRoutedAnnualMax[[ID]][sample(1:nYr,sampleLength)]
      if (sum(is.na(Q))>2){
        print(paste("For ID",ID,"Issue with Q, more than 2 NAs"))
        next
      }
      params <- unlist(hydrologyDistrAssess(Q, dist_i, name = ID, ADFlag = FALSE, paramFlag = TRUE, ADpFlag = FALSE))
      if (any(is.na(params))){
        print(paste("For ID",ID,"NA params"))
        next
      }
      pExc   <- 1 - switch(dist_i,
                          LP3 = F.gamma(log(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAILPM2_DVICB"]), params[1],params[2],params[3]),
                          P3  = F.gamma(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAILPM2_DVICB"], params[1],params[2],params[3]),
                          GEV = F.GEV(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAILPM2_DVICB"], params[1],params[2],params[3]) 
                          )
      if(is.na(pExc)){
        print(paste("NA pEXc for ID",ID))
        next
      }
      pExcfailEstSamples[[ID]][[dist_i]][j] <- pExc
    }
    pExcfailEstParam[[ID]][[dist_i]]$mean <- mean(pExcfailEstSamples[[ID]][[dist_i]],na.rm=TRUE)
    pExcfailEstParam[[ID]][[dist_i]]$sd <- sd(pExcfailEstSamples[[ID]][[dist_i]],na.rm=TRUE)
  }
}

savefile <- paste(gsub("-","",Sys.Date()),
                  paste("pExc_Bridge_DaymetVIC_Annual_SampleLength",sampleLength,"_nSamples",nSamples,".RData",sep=""),
                  sep="_")
save(pExcfailEstParam,pExcfailEstSamples,file=file.path(dirs$DataDirAnalysis,"DaymetVICbootstrapping",savefile))
rm(savefile)

# after testing with 10,000, am confident that has converged sufficiently with 1000 bootstraps

# plot
j <- 0
j <- j+1
pExcMinMax <- c(min(c(pExcfailEstSamples[[j]][["LP3"]],pExcfailEstSamples[[j]][["P3"]],pExcfailEstSamples[[j]][["GEV"]]),na.rm=TRUE),
                max(c(pExcfailEstSamples[[j]][["LP3"]],pExcfailEstSamples[[j]][["P3"]],pExcfailEstSamples[[j]][["GEV"]]),na.rm=TRUE))
pExc       <- seq(pExcMinMax[1],pExcMinMax[2],length.out=nSamples)
df <- data.frame(pExc        <- pExc,
                 pExcSamplesLP3 <- pExcfailEstSamples[[j]][["LP3"]],
                 pExcSamplesP3  <- pExcfailEstSamples[[j]][["P3"]],
                 pExcSamplesGEV <- pExcfailEstSamples[[j]][["GEV"]],
                 FnormLP3    <- pnorm(pExc,pExcfailEstParam[[j]][["LP3"]]$mean,pExcfailEstParam[[j]][["LP3"]]$sd),
                 FnormP3     <- pnorm(pExc,pExcfailEstParam[[j]][["P3"]]$mean,pExcfailEstParam[[j]][["P3"]]$sd),
                 FnormGEV    <- pnorm(pExc,pExcfailEstParam[[j]][["GEV"]]$mean,pExcfailEstParam[[j]][["GEV"]]$sd)
                 )
ggplot(data=df) + stat_ecdf(aes(x=pExcSamplesLP3),color="red") + geom_line(aes(x=pExc,y=FnormLP3),color="red") + 
  stat_ecdf(aes(x=pExcSamplesP3),color="blue") + geom_line(aes(x=pExc,y=FnormP3),color="blue") +
  stat_ecdf(aes(x=pExcSamplesGEV),color="darkgreen") + geom_line(aes(x=pExc,y=FnormGEV),color="darkgreen")

# some look reasonably normal, some definitely do not
#bounds on P_exc can be very large, especially with large probability of p_exc
# which, in general, they are... 

# GAGES - using +/- (PM) 2 days from given failure date to get best estimate of event from VIC --------------------
pExcfailEstSamples <- list()
pExcfailEstParam   <- list()
for (ID in IDsToAnalyzeVIC){
  STAID <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"STAID"]
  print(STAID)
  name <- paste(STAID,ID,sep="-")
  pExcfailEstParam[[name]]   <- list()
  pExcfailEstSamples[[name]] <- list()
   for (dist_i in names(paramMaxLikelihoodGageAnnualMax)){
    for (j in 1:nSamples){
      Q      <- ls.GageRoutedAnnualMax[[STAID]][sample(1:nYr,sampleLength)]
      if (sum(is.na(Q))>2){
        print(paste("For STAID",STAID,"Issue with Q, more than 2 NAs"))
        next
      }
      params <- unlist(hydrologyDistrAssess(Q, dist_i, name = STAID, ADFlag = FALSE, paramFlag = TRUE, ADpFlag = FALSE))
      if (any(is.na(params))){
        print(paste("For STAID",STAID,"NA params"))
        next
      }
      pExc   <- 1 - switch(dist_i,
                           LP3 = F.gamma(log(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAILPM2_DVICG"]), params[1],params[2],params[3]),
                           P3  = F.gamma(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAILPM2_DVICG"], params[1],params[2],params[3]),
                           GEV = F.GEV(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAILPM2_DVICG"], params[1],params[2],params[3]) 
      )
      if(is.na(pExc)){
        print(paste("NA pEXc for STAID",STAID))
        next
      }
      pExcfailEstSamples[[name]][[dist_i]][j] <- pExc
    }
    pExcfailEstParam[[name]][[dist_i]]$mean <- mean(pExcfailEstSamples[[name]][[dist_i]],na.rm=TRUE)
    pExcfailEstParam[[name]][[dist_i]]$sd <- sd(pExcfailEstSamples[[name]][[dist_i]],na.rm=TRUE)
  }
}

savefile <- paste(gsub("-","",Sys.Date()),
                  paste("pExc_Gage_DaymetVIC_Annual_SampleLength",sampleLength,"_nSamples",nSamples,".RData",sep=""),
                  sep="_")
save(pExcfailEstParam,pExcfailEstSamples,file=file.path(dirs$DataDirAnalysis,"DaymetVICbootstrapping",savefile))
rm(savefile)

# plot
j <- 0
j <- j+1
pExcMinMax <- c(min(c(pExcfailEstSamples[[j]][["LP3"]],pExcfailEstSamples[[j]][["P3"]],pExcfailEstSamples[[j]][["GEV"]]),na.rm=TRUE),
                max(c(pExcfailEstSamples[[j]][["LP3"]],pExcfailEstSamples[[j]][["P3"]],pExcfailEstSamples[[j]][["GEV"]]),na.rm=TRUE))
pExc       <- seq(pExcMinMax[1],pExcMinMax[2],length.out=nSamples)
df <- data.frame(pExc        <- pExc,
                 pExcSamplesLP3 <- pExcfailEstSamples[[j]][["LP3"]],
                 pExcSamplesP3  <- pExcfailEstSamples[[j]][["P3"]],
                 pExcSamplesGEV <- pExcfailEstSamples[[j]][["GEV"]],
                 FnormLP3    <- pnorm(pExc,pExcfailEstParam[[j]][["LP3"]]$mean,pExcfailEstParam[[j]][["LP3"]]$sd),
                 FnormP3     <- pnorm(pExc,pExcfailEstParam[[j]][["P3"]]$mean,pExcfailEstParam[[j]][["P3"]]$sd),
                 FnormGEV    <- pnorm(pExc,pExcfailEstParam[[j]][["GEV"]]$mean,pExcfailEstParam[[j]][["GEV"]]$sd)
)
ggplot(data=df) + stat_ecdf(aes(x=pExcSamplesLP3),color="red") + geom_line(aes(x=pExc,y=FnormLP3),color="red") + 
  stat_ecdf(aes(x=pExcSamplesP3),color="blue") + geom_line(aes(x=pExc,y=FnormP3),color="blue") +
  stat_ecdf(aes(x=pExcSamplesGEV),color="darkgreen") + geom_line(aes(x=pExc,y=FnormGEV),color="darkgreen")

#   BLOCK BOOTSTRAPPING - DOES NOT PERFORM WELL DUE TO SMALL SAMPLE SIZE - DO NOT USE #################### ------
source(file.path(dirsGit$ScriptsDir,"blockbootstrap.R"))
nBlock  <- 7
# BRIDGES - using +/- 2 day to compute P_Exc
pExcfailEstSamples <- list()
pExcfailEstParam   <- list()
for (ID in IDsToAnalyzeVIC){
  print(ID)
  pExcfailEstParam[[ID]]   <- list()
  pExcfailEstSamples[[ID]] <- list()
  for (dist_i in names(paramMaxLikelihoodBridgeAnnualMax)){
    for (j in 1:nSamples){
      Q      <- blockbootstrap(ls.BridgeRoutedAnnualMax[[ID]],nBlock)
      if (sum(is.na(Q))>2){
        print(paste("For ID",ID,"Issue with Q, more than 2 NAs"))
        next
      }
      params <- unlist(hydrologyDistrAssess(Q, dist_i, name = ID, ADFlag = FALSE, paramFlag = TRUE, ADpFlag = FALSE))
      if (any(is.na(params))){
        print(paste("For ID",ID,"NA params"))
        next
      }
      pExc   <- 1 - switch(dist_i,
                           LP3 = F.gamma(log(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAILPM2_DVICB"]), params[1],params[2],params[3]),
                           P3  = F.gamma(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAILPM2_DVICB"], params[1],params[2],params[3]),
                           GEV = F.GEV(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAILPM2_DVICB"], params[1],params[2],params[3]) 
      )
      if(is.na(pExc)){
        print(paste("NA pEXc for ID",ID))
        next
      }
      pExcfailEstSamples[[ID]][[dist_i]][j] <- pExc
    }
    pExcfailEstParam[[ID]][[dist_i]]$mean <- mean(pExcfailEstSamples[[ID]][[dist_i]],na.rm=TRUE)
    pExcfailEstParam[[ID]][[dist_i]]$sd <- sd(pExcfailEstSamples[[ID]][[dist_i]],na.rm=TRUE)
  }
}

# OBTAIN 5 AND 95% CONFIDENCE INTERVALS FOR T_R FROM BOOTSTRAPPING ########## -------------------------------
# BRIDGE -------------------
savefile <- paste("20150925",
                  paste("pExc_Bridge_DaymetVIC_Annual_SampleLength",sampleLength,"_nSamples",nSamples,".RData",sep=""),
                  sep="_")
load(file.path(dirs$DataDirAnalysis,"DaymetVICbootstrapping",savefile))
rm(savefile)

pExcConfLimBridge <- list()
CLs <- c(0.01,0.05,0.95,0.99)
for (ID in IDsToAnalyzeVIC){
  for (dist_i in names(pExcfailEstSamples[[1]])){ 
    pExcConfLimBridge[[ID]][[dist_i]]     <- quantile(pExcfailEstSamples[[ID]][[dist_i]],CLs,na.rm=TRUE,type=1) # type 1 is inverse of ECDF
    }
}

savefile <- paste(gsub("-","",Sys.Date()),
                  paste("pExc_Conf_Lim_Bridge_DaymetVIC_Annual_From_SampleLength",sampleLength,"_nSamples",nSamples,".RData",sep=""),
                  sep="_")
save(pExcConfLimBridge,file=file.path(dirs$DataDirAnalysis,"DaymetVICbootstrapping",savefile))
rm(savefile)

# GAGE-----------------
savefile <- paste("20150925",
                  paste("pExc_Gage_DaymetVIC_Annual_SampleLength",sampleLength,"_nSamples",nSamples,".RData",sep=""),
                  sep="_")
load(file.path(dirs$DataDirAnalysis,"DaymetVICbootstrapping",savefile))
rm(savefile)

pExcConfLimGage <- list()
for (ID in IDsToAnalyzeVIC){
  STAID <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"STAID"]
  name <- paste(STAID,ID,sep="-")
  for (dist_i in names(pExcfailEstSamples[[name]])){ 
    pExcConfLimGage[[name]][[dist_i]]     <- quantile(pExcfailEstSamples[[name]][[dist_i]],CLs,na.rm=TRUE,type=1) # type 1 is inverse of ECDF
  }
}

savefile <- paste(gsub("-","",Sys.Date()),
                  paste("pExc_Conf_Lim_Gage_DaymetVIC_Annual_From_SampleLength",sampleLength,"_nSamples",nSamples,".RData",sep=""),
                  sep="_")
save(pExcConfLimGage,file=file.path(dirs$DataDirAnalysis,"DaymetVICbootstrapping",savefile))
rm(savefile)

# PUT CONFIDENCE LIMITS INTO df.Fail.NBI.Gage -------------------
# After re-doing calcs on 2015-09-25, no significant difference for bridge at limits, and many NAs in 2nd calc, so will skip those
BridgeCIcols <- sapply(c("GEV","LP3","P3"),function(dist_i) sapply(c("01","05","95","99"), function(CI) paste("T_FAIL_PM2",dist_i,"DAYMET_VIC_BRIDGE",CI, sep="_")))
BridgeCIcols <- as.vector(BridgeCIcols)
df.Fail.NBI.Gage[,BridgeCIcols] <- rep(NA_real_,nrow(df.Fail.NBI.Gage))
GageCIcols <- sapply(c("GEV","LP3","P3"),function(dist_i) sapply(c("01","05","95","99"), function(CI) paste("T_FAIL_PM2",dist_i,"DAYMET_VIC_GAGE",CI, sep="_")))
GageCIcols <- as.vector(GageCIcols)
df.Fail.NBI.Gage[,GageCIcols] <- rep(NA_real_,nrow(df.Fail.NBI.Gage))

load("~/Documents/Research/Climate_Scour_Local/R_data/Analysis_Results_In_Progress/DaymetVICbootstrapping/20150823_pExc_Conf_Lim_Bridge_DaymetVIC_Annual_From_SampleLength25_nSamples1000.RData")
load("~/Documents/Research/Climate_Scour_Local/R_data/Analysis_Results_In_Progress/DaymetVICbootstrapping/201508925_pExc_Conf_Lim_Gage_DaymetVIC_Annual_From_SampleLength25_nSamples1000.RData")

for (ID in IDsToAnalyzeVIC){
  for (dist_i in names(pExcConfLimBridge[[ID]])){ 
    df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,paste("T_FAIL_PM2",dist_i,"DAYMET_VIC_BRIDGE",sub("0.","",CLs),sep="_")] <- 1/pExcConfLimBridge[[ID]][[dist_i]]
  }
}
df.Fail.NBI.Gage[rowsToAnalyzeVIC,BridgeCIcols] <- sapply(BridgeCIcols, 
                                                        function(j) sapply(rowsToAnalyzeVIC, 
                                                                           function(i) ifelse(is.infinite(df.Fail.NBI.Gage[i,j]),
                                                                                              NA_real_,
                                                                                              df.Fail.NBI.Gage[i,j])
                                                        )
)

for (i in rowsToAnalyzeVIC){
  STAID <- df.Fail.NBI.Gage[i,"STAID"]
  ID    <- df.Fail.NBI.Gage[i,"ID"]
  name <- paste(STAID,ID,sep="-")
  for (dist_i in names(pExcConfLimGage[[name]])){ 
    df.Fail.NBI.Gage[i,paste("T_FAIL_PM2",dist_i,"DAYMET_VIC_GAGE",sub("0.","",CLs),sep="_")] <- 1/pExcConfLimGage[[name]][[dist_i]]
  }
}
df.Fail.NBI.Gage[rowsToAnalyzeVIC,GageCIcols] <- sapply(GageCIcols, 
                                                        function(j) sapply(rowsToAnalyzeVIC, 
                                                                           function(i) ifelse(is.infinite(df.Fail.NBI.Gage[i,j]),
                                                                                              NA_real_,
                                                                                              df.Fail.NBI.Gage[i,j])
                                                                          )
                                                        )

savefile <- paste(gsub("-","",Sys.Date()),
                  paste("df.Fail.NBI.Gage_With_T_Conf_Lim_DaymetVIC_Fixed.RData",sep=""),
                  sep="_")
save(df.Fail.NBI.Gage,rowsToAnalyzeVIC,IDsToAnalyzeVIC,STAIDtoAnalyzeVIC,file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)
save(df.Fail.NBI.Gage,rowsToView,IDsToView,file=file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))

# CONFIDENCE INTERVALS FOR HEC-SSP ############################# ------------------------------
FLOW.5Pct.Conf  <- lapply(HEC_PFA_VICG,"[[","FLOW.5Pct.Conf")
FLOW.95Pct.Conf <- lapply(HEC_PFA_VICG,"[[","FLOW.95Pct.Conf")
FREQ            <- lapply(HEC_PFA_VICG,"[[","FREQ")

ExceedProbFail <- list()
ExceedProbFailPM2 <- list()
for (i in rowsToAnalyzeVIC){
  ID <- as.character(df.Fail.NBI.Gage[i,"ID"])
  STAID <- df.Fail.NBI.Gage[i,"STAID"]
  ExceedProbFail[[ID]] <- list()
  for (CL in c("5%","95%")){
    log_flow <- switch(CL,
                       "5%"  = log(FLOW.5Pct.Conf[[STAID]]),
                       "95%" = log(FLOW.95Pct.Conf[[STAID]])
    )
    log_freq <- log(FREQ[[STAID]]/100)
    log_freq <- log_freq[!is.na(log_flow)]
    log_flow <- log_flow[!is.na(log_flow)]

    log_Fail <- log(df.Fail.NBI.Gage[i,"Q_FAILPM2_DVICG"])
    ExceedProbFailPM2[[ID]][CL] <- exp(approxExtrap(log_flow,log_freq,log_Fail)$y)
    log_Fail <- log(df.Fail.NBI.Gage[i,"Q_FAIL_DVICG"])
    ExceedProbFail[[ID]][CL] <- exp(approxExtrap(log_flow,log_freq,log_Fail)$y)
  }
}
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILPM2_95_HEC_DVICG"] <- 1/unlist(lapply(ExceedProbFailPM2,"[[","5%"))
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAILPM2_05_HEC_DVICG"] <- 1/unlist(lapply(ExceedProbFailPM2,"[[","95%"))
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_95_HEC_DVICG"] <- 1/unlist(lapply(ExceedProbFail,"[[","5%"))
df.Fail.NBI.Gage[rowsToAnalyzeVIC,"T_FAIL_05_HEC_DVICG"] <- 1/unlist(lapply(ExceedProbFail,"[[","95%"))

savefile <- paste(gsub("-","",Sys.Date()),
                  paste("df.Fail.NBI.Gage_With_T_Conf_Lim_DaymetVIC_USGS_HEC.RData",sep=""),
                  sep="_")
save(df.Fail.NBI.Gage,IDsToAnalyzeVIC,file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage, rowsToView,IDsToView,file = file.path(dirs$dfActive,savefile))
rm(savefile)



# FIND REPRESENTATIVE DISTRIBUTIONS OF BOOTSTRAPPING FOR PLOTTING -----------------------
# BRIDGES --------
savefile <- paste("20150823",
                  paste("pExc_Bridge_DaymetVIC_Annual_SampleLength",sampleLength,"_nSamples",nSamples,".RData",sep=""),
                  sep="_")
load(file.path(dirs$DataDirAnalysis,"DaymetVICbootstrapping",savefile))
rm(savefile)
savefile <- paste(gsub("-","",Sys.Date()),
                  paste("pExc_Conf_Lim_Bridge_DaymetVIC_Annual_From_SampleLength",sampleLength,"_nSamples",nSamples,".RData",sep=""),
                  sep="_")
load(file.path(dirs$DataDirAnalysis,"DaymetVICbootstrapping",savefile))
rm(savefile)

COVbridge <- list()
for (ID in IDsToAnalyzeVIC){
  name <- ID
  for (dist_i in names(pExcfailEstParam[[name]])){ 
    COVbridge[[name]][[dist_i]]   <- pExcfailEstParam[[name]][[dist_i]]$sd / pExcfailEstParam[[name]][[dist_i]]$mean
  }
}
maxCOVindex <- which.max(lapply(names(COVbridge), function(name) max(COVbridge[[name]],na.rm=TRUE)))
minCOVindex <- which.min(lapply(names(COVbridge), function(name) min(COVbridge[[name]],na.rm=TRUE)))
medianCOVval <- list(GEV = median(sapply(names(COVbridge), function(name) COVbridge[[name]][1])),
                     P3  = median(sapply(names(COVbridge), function(name) COVbridge[[name]][2])),
                     LP3  = median(sapply(names(COVbridge), function(name) COVbridge[[name]][3]))
)
medianCOVindex <- list(GEV = which.min(abs(sapply(names(COVbridge), function(name) COVbridge[[name]][1]) - medianCOVval$GEV)),
                       P3  = which.min(abs(sapply(names(COVbridge), function(name) COVbridge[[name]][2]) - medianCOVval$P3)),
                       LP3 = which.min(abs(sapply(names(COVbridge), function(name) COVbridge[[name]][3]) - medianCOVval$LP3))
)
# 22 is a match for GEV and LP3
medianCOVindex <- 22
namesMaxMedMin <- c(max    = names(COVbridge)[maxCOVindex],
                    median = names(COVbridge)[medianCOVindex],
                    min    = names(COVbridge)[minCOVindex],
                    gage.med = "1675",
                    USGS.med = "3610")

plotDataTrDistVICbridge <- list(max = list(ID      = namesMaxMedMin["max"],
                                           param   = pExcfailEstParam[[namesMaxMedMin["max"] ]],
                                           samples = pExcfailEstSamples[[namesMaxMedMin["max"] ]],
                                           COV     = COVbridge[[namesMaxMedMin["max"] ]],
                                           CL      = pExcConfLimBridge[[namesMaxMedMin["max"] ]]
),
median = list(ID   = namesMaxMedMin["median"],
              param   = pExcfailEstParam[[namesMaxMedMin["median"] ]],
              samples = pExcfailEstSamples[[namesMaxMedMin["median"] ]],
              COV     = COVbridge[[namesMaxMedMin["median"] ]],
              CL      = pExcConfLimBridge[[namesMaxMedMin["median"] ]]
),
min    =  list(ID   = namesMaxMedMin["min"],
               param   = pExcfailEstParam[[namesMaxMedMin["min"] ]],
               samples = pExcfailEstSamples[[namesMaxMedMin["min"] ]],
               COV     = COVbridge[[namesMaxMedMin["min"] ]],
               CL      = pExcConfLimBridge[[namesMaxMedMin["min"] ]]
),
gage.med = list(STAID   = namesMaxMedMin["gage.med"],
                  param   = pExcfailEstParam[[namesMaxMedMin["gage.med"] ]],
                  samples = pExcfailEstSamples[[namesMaxMedMin["gage.med"] ]],
                  COV     = COVgage[[namesMaxMedMin["gage.med"] ]],
                  CL      = pExcConfLimBridge[[namesMaxMedMin["gage.med"] ]]
),
USGS.med = list(STAID   = namesMaxMedMin["USGS.med"],
                param   = pExcfailEstParam[[namesMaxMedMin["USGS.med"] ]],
                samples = pExcfailEstSamples[[namesMaxMedMin["USGS.med"] ]],
                COV     = COVgage[[namesMaxMedMin["USGS.med"] ]],
                CL      = pExcConfLimBridge[[namesMaxMedMin["USGS.med"] ]]
)
)
# GAGES ---------
savefile <- paste("20150925",
                  paste("pExc_Gage_DaymetVIC_Annual_SampleLength",sampleLength,"_nSamples",nSamples,".RData",sep=""),
                  sep="_")
load(file.path(dirs$DataDirAnalysis,"DaymetVICbootstrapping",savefile))
rm(savefile)
savefile <- paste("20150925",
                  paste("pExc_Conf_Lim_Gage_DaymetVIC_Annual_From_SampleLength",sampleLength,"_nSamples",nSamples,".RData",sep=""),
                  sep="_")
load(file.path(dirs$DataDirAnalysis,"DaymetVICbootstrapping",savefile))
rm(savefile)

COVgage <- list()
for (ID in IDsToAnalyzeVIC){
  STAID <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"STAID"]
  name <- paste(STAID,ID,sep="-")
  for (dist_i in names(pExcfailEstParam[[name]])){ 
    COVgage[[name]][[dist_i]]   <- pExcfailEstParam[[name]][[dist_i]]$sd / pExcfailEstParam[[name]][[dist_i]]$mean
  }
}
maxCOVindex <- which.max(lapply(names(COVgage), function(name) max(COVgage[[name]],na.rm=TRUE)))
minCOVindex <- which.min(lapply(names(COVgage), function(name) min(COVgage[[name]],na.rm=TRUE)))
medianCOVval <- list(GEV = median(sapply(names(COVgage), function(name) COVgage[[name]][1])),
                       P3  = median(sapply(names(COVgage), function(name) COVgage[[name]][2])),
                       LP3  = median(sapply(names(COVgage), function(name) COVgage[[name]][3]))
                       )
medianCOVindex <- list(GEV = which.min(abs(sapply(names(COVgage), function(name) COVgage[[name]][1]) - medianCOVval$GEV)),
                       P3  = which.min(abs(sapply(names(COVgage), function(name) COVgage[[name]][2]) - medianCOVval$P3)),
                       LP3 = which.min(abs(sapply(names(COVgage), function(name) COVgage[[name]][3]) - medianCOVval$LP3))
                       )
# 19 is closest to median over all distributions (appears to be median for GEV and LP3)
medianCOVindex <- 19
namesMaxMedMin <- c(max    = names(COVgage)[maxCOVindex],
                    median = names(COVgage)[medianCOVindex],
                    min    = names(COVgage)[minCOVindex],
                    bridge.med = paste(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID == 3535,"STAID"],"3535",sep="-"),
                    USGS.med   = paste(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID == 3610,"STAID"],"3610",sep="-")
)

plotDataTrDistVICgage <- list(max = list(STAID   = namesMaxMedMin["max"],
                                         param   = pExcfailEstParam[[namesMaxMedMin["max"] ]],
                                         samples = pExcfailEstSamples[[namesMaxMedMin["max"] ]],
                                         COV     = COVgage[[namesMaxMedMin["max"] ]],
                                         CL      = pExcConfLimGage[[namesMaxMedMin["max"] ]]
                                         ),
                           median = list(STAID   = namesMaxMedMin["median"],
                                         param   = pExcfailEstParam[[namesMaxMedMin["median"] ]],
                                         samples = pExcfailEstSamples[[namesMaxMedMin["median"] ]],
                                         COV     = COVgage[[namesMaxMedMin["median"] ]],
                                         CL      = pExcConfLimGage[[namesMaxMedMin["median"] ]]
                                         ),
                           min    =  list(STAID   = namesMaxMedMin["min"],
                                          param   = pExcfailEstParam[[namesMaxMedMin["min"] ]],
                                          samples = pExcfailEstSamples[[namesMaxMedMin["min"] ]],
                                          COV     = COVgage[[namesMaxMedMin["min"] ]],
                                          CL      = pExcConfLimGage[[namesMaxMedMin["min"] ]]
                                          ),
                           bridge.med = list(STAID   = namesMaxMedMin["bridge.med"],
                                             param   = pExcfailEstParam[[namesMaxMedMin["bridge.med"] ]],
                                             samples = pExcfailEstSamples[[namesMaxMedMin["bridge.med"] ]],
                                             COV     = COVgage[[namesMaxMedMin["bridge.med"] ]],
                                             CL      = pExcConfLimGage[[namesMaxMedMin["bridge.med"] ]]
                           ),
                           USGS.med   = list(STAID   = namesMaxMedMin["USGS.med"],
                                             param   = pExcfailEstParam[[namesMaxMedMin["USGS.med"] ]],
                                             samples = pExcfailEstSamples[[namesMaxMedMin["USGS.med"] ]],
                                             COV     = COVgage[[namesMaxMedMin["USGS.med"] ]],
                                             CL      = pExcConfLimGage[[namesMaxMedMin["USGS.med"] ]]
                           )
                          )

# SAVE PLOTTING DATA --------
savefile <- paste(gsub("-","",Sys.Date()),
                  paste("Plotting.pExcDist_Daymet_VIC_Annual_SampleLength",sampleLength,"_nSamples",nSamples,".RData",sep=""),
                  sep="_")
save(plotDataTrDistVICgage,plotDataTrDistVICbridge, file = file.path(dirs$PlotsDirScripts,savefile))
rm(savefile)

rm(list = ls(pattern="^[pClmnQsijFBG]"))
rm(ID,STAID,dist_i,hydrologyDistrAssess,IDsToAnalyzeVIC,rowsToAnalyzeVIC,STAIDtoAnalyzeVIC)


