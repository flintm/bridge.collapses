# Analysis of confidence in USGS return periods, using:
#   (1) Bootstrapping for LP3,GEV,P3
#   (2) Confidence intervals for HEC-SSP
#   (3) Compare moments of LP3 with HEC-SSP
# written by Madeleine Flint on 2015-08-24

require(stats)
require(nsRFA)
require(ggplot2)
require(PearsonDS)

# Load and source ------------------
source(file.path(dirsGit$ScriptsDir,"hydrologyDistrAssess.R"))

load(file.path(dirs$DataDirAnalysis,"20150813_USGSgagePeaks_LP3_GEV_Param.RData"))
load(file.path(dirs$DataDirFrequent, "20150729_Discharge_39Gages.RData"))
load(file.path(dirs$DataDirFrequent, "20150605_HEC_PFA_41_Bridges.RData"))
load(file.path(dirs$DataDirFrequent,"20151209_HEC_PFA_AnMaxDayMean_36_Bridges.RData"))
load(file.path(dirs$dfActive, "df.Fail.NBI.Gage.Active.RData"))


# (1) ESTIMATE UNCERTAINTY IN RETURN PERIOD USING BOOTSTRAPPING (NON-BLOCK) ############################## -------------
# USGS -  -------------
sampleLengthRatio <- 0.8
pExcfailEstSamples <- list()
pExcfailEstParam   <- list()
IDsToAnalyzeUSGS   <- as.character(IDsToView)
for (ID in IDsToAnalyzeUSGS[3:36]){
  print(ID)
  STAID        <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID == ID, "STAID"]
  nYr          <- nrow(ls.Discharge.Peaks[[STAID]][!is.na(ls.Discharge.Peaks[[STAID]]$peak_va),])
  sampleLength <- ceiling(sampleLengthRatio*nYr)
  
  # Option (1) to obtain Qfail from best estimate
  #   Qfail  <- switch(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"T_BEST_SOURCE"],
  #                    DAILY = df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAIL_D_USGS"],
  #                    PEAK  = df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAIL_P_USGS"],
  #                    INST  =df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAIL_I_USGS"]
  #   )
  
  # Option (2) to obtain Qfail from DAILY data, as it's most similar to Daymet-VIC
  Qfail <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"Q_FAIL_D_USGS"]
  
  pExcfailEstParam[[ID]]   <- list()
  pExcfailEstSamples[[ID]] <- list()
  
  if (ID %in% c(195)) nSamples <- 500
  else nSamples <- 1000
  for (j in 1:nSamples){
    Q      <- ls.Discharge.Peaks[[STAID]][!is.na(ls.Discharge.Peaks[[STAID]]$peak_va),][sample(1:nYr,sampleLength),"peak_va"]
    if (sum(is.na(Q))>2){
      print(paste("For ID",ID,"Issue with Q, more than 2 NAs"))
      next
    }
    for (dist_i in names(paramMaxLikelihoodUSGSgagePeaks)){
      params <- unlist(hydrologyDistrAssess(Q, dist_i, name = ID, ADFlag = FALSE, paramFlag = TRUE, ADpFlag = FALSE))
      if (any(is.na(params))){
        print(paste("For ID",ID,"NA params"))
        next
      }
      pExc   <- 1 - switch(dist_i,
                           LP3 = F.gamma(log(Qfail), params[1],params[2],params[3]),
                           P3  = F.gamma(Qfail, params[1],params[2],params[3]),
                           GEV = F.GEV(Qfail, params[1],params[2],params[3]) 
      )
      if(is.na(pExc)){
        print(paste("NA pEXc for ID",ID))
        next
      }
      pExcfailEstSamples[[ID]][[dist_i]][j] <- pExc
      pExcfailEstParam[[ID]][[dist_i]]$mean <- mean(pExcfailEstSamples[[ID]][[dist_i]],na.rm=TRUE)
      pExcfailEstParam[[ID]][[dist_i]]$sd   <- sd(pExcfailEstSamples[[ID]][[dist_i]],na.rm=TRUE)
    }
  }
}
# for ID=195, can't get it to converge with 1000 samples, using 500

savefile <- paste(gsub("-","",Sys.Date()),
                  paste("pExc_USGS_Annual_FailQ_Daily_SampleLengthRatio",sampleLengthRatio,"_nSamples",nSamples,".RData",sep=""),
                  sep="_")
save(pExcfailEstParam,pExcfailEstSamples,file=file.path(dirs$DataDirAnalysis,"DaymetVICbootstrapping",savefile))
rm(savefile)

# OBTAIN 5 AND 95% CONFIDENCE INTERVALS FOR T_R FROM BOOTSTRAPPING -------------------------------
# USGS ---------
pExcConfLimUSGS <- list()
CLs <- c(0.01,0.05,0.95,0.99)
for (ID in IDsToAnalyzeUSGS){
  for (dist_i in names(pExcfailEstSamples[[1]])){ 
    pExcConfLimUSGS[[ID]][[dist_i]]     <- quantile(pExcfailEstSamples[[ID]][[dist_i]],CLs,na.rm=TRUE,type=1) # type 1 is inverse of ECDF
  }
}

savefile <- paste(gsub("-","",Sys.Date()),
                  paste("pExc_Conf_Lim_USGS_DaymetVIC_Annual_From_SampleLength",sampleLength,"_nSamples",nSamples,".RData",sep=""),
                  sep="_")
save(pExcConfLimUSGS,file=file.path(dirs$DataDirAnalysis,"DaymetVICbootstrapping",savefile))
rm(savefile)

# PUT CONFIDENCE INTERVALS INTO FAILURE DATA FRAME -------------
df.Fail.NBI.Gage$T_FAIL_GEV_USGS_01 <- numeric(nrow(df.Fail.NBI.Gage))
df.Fail.NBI.Gage$T_FAIL_GEV_USGS_05 <- numeric(nrow(df.Fail.NBI.Gage))
df.Fail.NBI.Gage$T_FAIL_GEV_USGS_95 <- numeric(nrow(df.Fail.NBI.Gage))
df.Fail.NBI.Gage$T_FAIL_GEV_USGS_99 <- numeric(nrow(df.Fail.NBI.Gage))
df.Fail.NBI.Gage[,c("T_FAIL_LP3_USGS_01","T_FAIL_LP3_USGS_05","T_FAIL_LP3_USGS_95","T_FAIL_LP3_USGS_99")] <-
            df.Fail.NBI.Gage[,c("T_FAIL_GEV_USGS_01","T_FAIL_GEV_USGS_05","T_FAIL_GEV_USGS_95","T_FAIL_GEV_USGS_99")]


for (ID in IDsToAnalyzeUSGS){
  for (dist_i in names(pExcfailEstSamples[[1]])){ 
    df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,paste("T_FAIL",dist_i,"USGS",sub("0.","",CLs),sep="_")] <- 1/pExcConfLimUSGS[[ID]][[dist_i]]
  }
}

savefile <- paste(gsub("-","",Sys.Date()),
                  paste("df.Fail.NBI.Gage_With_T_Conf_Lim_DaymetVIC_USGS.RData",sep=""),
                  sep="_")
save(df.Fail.NBI.Gage,IDsToAnalyzeUSGS,file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

# FIND REPRESENTATIVE DISTRIBUTIONS OF BOOTSTRAPPING FOR PLOTTING -----------------------
# USGS --------

COVusgs <- list()
for (ID in IDsToAnalyzeVIC){
  name <- ID
  for (dist_i in names(pExcfailEstParam[[name]])){ 
    COVusgs[[name]][[dist_i]]   <- pExcfailEstParam[[name]][[dist_i]]$sd / pExcfailEstParam[[name]][[dist_i]]$mean
  }
}
maxCOVindex <- which.max(lapply(names(COVusgs), function(name) max(COVusgs[[name]],na.rm=TRUE)))
minCOVindex <- which.min(lapply(names(COVusgs), function(name) min(COVusgs[[name]],na.rm=TRUE)))
medianCOVval <- list(GEV = median(sapply(names(COVusgs), function(name) COVusgs[[name]][1])),
                     LP3  = median(sapply(names(COVusgs), function(name) COVusgs[[name]][2]))
)
medianCOVindex <- list(GEV = which.min(abs(sapply(names(COVusgs), function(name) COVusgs[[name]][1]) - medianCOVval$GEV)),
                       LP3 = which.min(abs(sapply(names(COVusgs), function(name) COVusgs[[name]][2]) - medianCOVval$LP3))
)
# 3610 is the best over both distributions
medianCOVindex <- 23
namesMaxMedMin <- c(max    = names(COVusgs)[maxCOVindex],
                    median = names(COVusgs)[medianCOVindex],
                    min    = names(COVusgs)[minCOVindex],
                    gage.med = "1675",
                    bridge.med = "3535")

plotDataTrDistUSGS      <- list(max = list(ID      = namesMaxMedMin["max"],
                                           param   = pExcfailEstParam[[namesMaxMedMin["max"] ]],
                                           samples = pExcfailEstSamples[[namesMaxMedMin["max"] ]],
                                           COV     = COVusgs[[namesMaxMedMin["max"] ]],
                                           CL      = pExcConfLimUSGS[[namesMaxMedMin["max"] ]]
),
median = list(ID   = namesMaxMedMin["median"],
              param   = pExcfailEstParam[[namesMaxMedMin["median"] ]],
              samples = pExcfailEstSamples[[namesMaxMedMin["median"] ]],
              COV     = COVusgs[[namesMaxMedMin["median"] ]],
              CL      = pExcConfLimUSGS[[namesMaxMedMin["median"] ]]
),
min    =  list(ID   = namesMaxMedMin["min"],
               param   = pExcfailEstParam[[namesMaxMedMin["min"] ]],
               samples = pExcfailEstSamples[[namesMaxMedMin["min"] ]],
               COV     = COVusgs[[namesMaxMedMin["min"] ]],
               CL      = pExcConfLimUSGS[[namesMaxMedMin["min"] ]]
),
gage.med = list(STAID   = namesMaxMedMin["gage.med"],
                param   = pExcfailEstParam[[namesMaxMedMin["gage.med"] ]],
                samples = pExcfailEstSamples[[namesMaxMedMin["gage.med"] ]],
                COV     = COVusgs[[namesMaxMedMin["gage.med"] ]],
                CL      = pExcConfLimUSGS[[namesMaxMedMin["gage.med"] ]]
),
bridge.med = list(STAID   = namesMaxMedMin["bridge.med"],
                param   = pExcfailEstParam[[namesMaxMedMin["bridge.med"] ]],
                samples = pExcfailEstSamples[[namesMaxMedMin["bridge.med"] ]],
                COV     = COVusgs[[namesMaxMedMin["bridge.med"] ]],
                CL      = pExcConfLimUSGS[[namesMaxMedMin["bridge.med"] ]]
)
)

# Re-do actual bootstraps to use in plots
ID <- "3610"
IDboot <- ID
STAID        <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID == ID, "STAID"]
nYr          <- nrow(ls.Discharge.Peaks[[STAID]][!is.na(ls.Discharge.Peaks[[STAID]]$peak_va),])
sampleLength <- ceiling(sampleLengthRatio*nYr)
Qfail        <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==IDboot,"Q_FAIL_D_USGS"] 
nSamples <- 1000
paramsBoot <- list(GEV=list(),LP3=list())
pExc <- list(GEV=list(),LP3=list())
for (j in as.character(1:nSamples)){
#   Q      <- ls.Discharge.Peaks[[STAID]][!is.na(ls.Discharge.Peaks[[STAID]]$peak_va),][sample(1:nYr,sampleLength),"peak_va"]
#   if (sum(is.na(Q))>2){
#     print(paste("For ID",ID,"Issue with Q, more than 2 NAs"))
#     next
#   }
  for (dist_i in c("GEV","LP3")){
#     paramsBoot[[dist_i]][[j]] <- hydrologyDistrAssess(Q, dist_i, name = ID, ADFlag = FALSE, paramFlag = TRUE, ADpFlag = FALSE)
#     if (any(is.na(paramsBoot))){
#       print("NA params")
#       next
#     }
    pExc[[dist_i]][j] <- 1 - switch(dist_i,
                                    LP3 = F.gamma(log(Qfail), paramsBoot[[dist_i]][[j]]$ml[1],paramsBoot[[dist_i]][[j]]$ml[2],paramsBoot[[dist_i]][[j]]$ml[3]),
                                    GEV = F.GEV(Qfail,paramsBoot[[dist_i]][[j]]$ml[1],paramsBoot[[dist_i]][[j]]$ml[2],paramsBoot[[dist_i]][[j]]$ml[3])
    )
  }
}


savefile <- paste(gsub("-","",Sys.Date()),
                  paste("Plotting.pExcDist_USGS_Daily_Annual_SampleLength",sampleLength,"_nSamples",nSamples,".RData",sep=""),
                  sep="_")
save(plotDataTrDistUSGS,paramsBoot,IDboot,pExc, file = file.path(dirs$PlotsDirScripts,savefile))
rm(savefile)

rm(list = ls(pattern="\\<[pClmnQsij]"))
rm(ID,STAID,dist_i,hydrologyDistrAssess)

# (2) CONFIDENCE INTERVALS FOR HEC-SSP ############################# ------------------------------
# USGS AN/PEAK----
FLOW.5Pct.Conf  <- lapply(HEC_PFA,"[[","FLOW.5Pct.Conf")
FLOW.95Pct.Conf <- lapply(HEC_PFA,"[[","FLOW.95Pct.Conf")
FREQ            <- lapply(HEC_PFA,"[[","FREQ")
rowsWithPeak <- rownames(df.Fail.NBI.Gage[!is.na(df.Fail.NBI.Gage$Q_FAIL_IP_USGS),])
ExceedProbFail <- list()
for (i in rowsWithPeak){
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
 
  log_Fail <- log(df.Fail.NBI.Gage[i,"Q_FAIL_IP_USGS"])
  ExceedProbFail[[ID]][CL] <- exp(approxExtrap(log_flow,log_freq,log_Fail)$y)
  }
}
df.Fail.NBI.Gage[rowsWithPeak,"T_FAIL_P_95_HECP_USGS"] <- 1/unlist(lapply(ExceedProbFail,"[[","5%"))
df.Fail.NBI.Gage[rowsWithPeak,"T_FAIL_P_05_HECP_USGS"] <- 1/unlist(lapply(ExceedProbFail,"[[","95%"))

# USGS DAY----
FLOW.5Pct.Conf  <- lapply(HEC_PFA_AnMaxDayMean,"[[","FLOW.5Pct.Conf")
FLOW.95Pct.Conf <- lapply(HEC_PFA_AnMaxDayMean,"[[","FLOW.95Pct.Conf")
FREQ            <- lapply(HEC_PFA_AnMaxDayMean,"[[","FREQ")

ExceedProbFail <- list()
for (i in rowsToView){
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
    
    log_Fail <- log(df.Fail.NBI.Gage[i,"Q_FAIL_D_USGS"])
    ExceedProbFail[[ID]][CL] <- exp(approxExtrap(log_flow,log_freq,log_Fail)$y)
  }
}
df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_95_HECD_USGS"] <- 1/unlist(lapply(ExceedProbFail,"[[","5%"))
df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_05_HECD_USGS"] <- 1/unlist(lapply(ExceedProbFail,"[[","95%"))

savefile <- paste(gsub("-","",Sys.Date()),
paste("df.Fail.NBI.Gage_With_T_Conf_Lim_DaymetVIC_USGS_HEC_Dayk.RData",sep=""),
sep="_")
save(df.Fail.NBI.Gage,rowsToView,IDsToView,file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

savefile <- "df.Fail.NBI.Gage.Active.RData"
save(df.Fail.NBI.Gage, rowsToView, IDsToView, file=file.path(dirs$dfActive,savefile))
rm(savefile)

# (3) COMPUTE MOMENTS OF LP3-MLE FIT TO COMPARE TO MOMENTS FROM HEC-SSP  ###########---------------------
# get moments of LP3 fitted distribution
STAIDtoAnalyzeUSGS <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToAnalyzeUSGS,"STAID"]
pearsonDistNum <- 3
mleLP3mom <- list()
for(ID in IDsToAnalyzeUSGS){
  STAID <- df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==ID,"STAID"]
  #nsRFA gives xi = location, beta=scale, alpha=shape
  params <- list(pearsonDistNum, shape=mlUSGSgagesPeaks[["LP3"]][[STAID]][3], location=mlUSGSgagesPeaks[["LP3"]][[STAID]][1], scale= mlUSGSgagesPeaks[["LP3"]][[STAID]][2])
  mleLP3mom[[ID]] <- pearsonMoments(params)
}

# load moments reported by HEC-SSP Bulletin 17B Analysis Reports
HECmoments <- list()
HECreportDirs <- list.dirs(path = file.path(dirs$OrigDataDir,"HEC","AllReports"))
for (dir_i in HECreportDirs[2:length(HECreportDirs)]){
  HECfiles <- list.files(path = dir_i, pattern = ".rpt\\>")
  for (file_i in HECfiles){
    fileNameSplit <- unlist(strsplit(file_i,"[_-]"))
    STAID         <-  fileNameSplit[grepl("[[:digit:]]",fileNameSplit)]
    if(STAID %in% STAIDtoAnalyzeUSGS){
      IDs <- IDsToAnalyzeUSGS[df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToAnalyzeUSGS,"STAID"] == STAID]
      
      fileInput           <- readLines(con = file.path(dir_i,file_i))
      if (any(grepl("Statistics and frequency curve were modified",fileInput))){
        rowFlowMomentsBegin <- grep("<< Adjusted Statistics >>",fileInput)
        if (length(rowFlowMomentsBegin) == 0){
          rowFlowMomentsBegin <- grep("<< Synthetic Statistics >>",fileInput)
        }
      }
      else{
        if (any(grepl("Adopted skew equals station skew and preliminary",fileInput))){
          rowFlowMomentsBegin <- grep("<< Conditional Statistics >>",fileInput)
        }
        else{
          rowFlowMomentsBegin <- grep("[<]{2} Systematic Statistics [>]{2}",fileInput)
        }
      }
      fileInput           <- fileInput[rowFlowMomentsBegin:(rowFlowMomentsBegin + 13)]
      
      # mean (of logarithm of flow in CFS)
      pattern     <- "Mean[[:space:]]+[[:digit:]].[[:digit:]]{3}"
      row         <- grep(pattern,fileInput)
      index       <- regexpr("[[:digit:]].[[:digit:]]{3}",fileInput[row])
      meanLog     <- as.numeric(substr(fileInput[row],index,index + attr(index,"match.length")-1))
      
      # std dev
      pattern     <- "Standard Dev[[:space:]]+[[:digit:]].[[:digit:]]{3}"
      row         <- grep(pattern,fileInput)
      index       <- regexpr("[[:digit:]].[[:digit:]]{3}",fileInput[row])
      stdLog      <- as.numeric(substr(fileInput[row],index,index + attr(index,"match.length")-1))
      
      # skew
      pattern     <- "Adopted Skew[[:space:]]+[-]?[[:digit:]].[[:digit:]]{3}"
      row         <- grep(pattern,fileInput)
      index       <- regexpr("[-]?[[:digit:]].[[:digit:]]{3}",fileInput[row])
      skewLog     <- as.numeric(substr(fileInput[row],index,index + attr(index,"match.length")-1))
        for (ID in IDs){
          HECmoments[[ID]] <- c(mean = meanLog, variance = stdLog^2, skewness = skewLog)
      }
    }
  }
}

# Compare moments using ?
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

savefile <- paste(gsub("-","",Sys.Date()),"MomentsHECandMLE-fitLP3dists",sep="_")
save(mleLP3mom,HECmoments,file=file.path(dirs$DataDirAnalysis,savefile))
rm(savefile)

rm(list = ls(pattern="^[fHilmpSs]"))
rm(ID,IDs,dir_i,row,rowFlowMomentsBegin)
