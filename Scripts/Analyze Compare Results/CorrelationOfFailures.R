# Performs correlation of Q and T estimates related to failure
# Copyright Madeleine Flint, 2016
CorrelationOfFailures <- function(BridgesDataFrame = NULL,TYPES = c("FAILQ", "USGS", "USGS-VICG", "USGS-BEST-VICg", "USGS-PART-VICg-PART","VICg","VICg-VICb",
                                            "USGS-NLDAS", "USGS-BEST-NLDAS", "NLDASg-NLDASb", "VIC-NLDAS"),
                                  SAVE = TRUE, VERBOSE = FALSE){
if(is.null(BridgesDataFrame)) load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
else df.Fail.NBI.Gage <- BridgesDataFrame
source(file.path(dirsGit$Scripts,"Bases.R"))

require(stats)
require(Kendall)
require(SuppDists)
require(pspearman)
require(nsRFA)

rowsToAnalyzeVIC   <- rowsToView[df.Fail.NBI.Gage[rowsToView,"YR_FAIL"] > as.Date("1980-01-01")]
rowsToAnalyzeNLDAS <- rowsToView[df.Fail.NBI.Gage[rowsToView,"YR_FAIL"] > as.Date("1979-01-01")]

kgCfs          <- 0.035315*(1/3600)

ls.corrs.fail <- list()
for (type in TYPES){
  if (VERBOSE) print(type)
  ls.corrs.fail[[type]] <- list()
  ls.Bases       <- Bases(type)
  for (i in 1:length(ls.Bases$bases)){
    T1 <- df.Fail.NBI.Gage[rowsToView,ls.Bases$T1ls[i]]
    T2 <- df.Fail.NBI.Gage[rowsToView,ls.Bases$T2ls[i]]
    if (type == "FAILQ"){
      if (grepl("NLDAS_BRIDGE",ls.Bases$T1ls[i],ignore.case = TRUE))  T1 <- T1*kgCfs*df.Fail.NBI.Gage[rowsToView,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"]
      if (grepl("NLDAS_GAGE",ls.Bases$T1ls[i],ignore.case = TRUE))    T1 <- T1*kgCfs*df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]
      if (grepl("NLDAS_BRIDGE",ls.Bases$T2ls[i],ignore.case = TRUE))  T2 <- T2*kgCfs*df.Fail.NBI.Gage[rowsToView,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"]
      if (grepl("NLDAS_GAGE",ls.Bases$T2ls[i],ignore.case = TRUE))    T2 <- T2*kgCfs*df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]
    }
    ls.corrs.fail[[type]][[paste(ls.Bases$bases[i],"P",sep="")]]      <- cor.test(T1,T2,
                                                                                  method="pearson",
                                                                                  conf.level=0.95,
                                                                                  exact=TRUE)
    ls.corrs.fail[[type]][[paste(ls.Bases$bases[i],"K",sep="")]]       <- cor.test(T1,T2,
                                                                                   method="kendall",
                                                                                   conf.level=0.95,
                                                                                   exact=TRUE)
    ls.corrs.fail[[type]][[paste(ls.Bases$bases[i],"Papprox",sep="")]] <- cor.test(T1,T2,
                                                                                   method="pearson",
                                                                                   conf.level=0.95,
                                                                                   exact=FALSE)
    ls.corrs.fail[[type]][[paste(ls.Bases$bases[i],"Kapprox",sep="")]] <- cor.test(T1,T2,
                                                                                   method="kendall",
                                                                                   conf.level=0.95,
                                                                                   exact=FALSE)
    ls.corrs.fail[[type]][[paste(ls.Bases$bases[i],"LR",sep="")]]      <- lm(T2 ~ T1)
  }
}


## SAVE ############ ------
if (SAVE){
  savefile <- paste(gsub("-","",Sys.Date()),"CorrelationsOfFailures.RData", sep="_")
  save(ls.corrs.fail, file = file.path(dirs$DataDirAnalysis,savefile))
}
return(ls.corrs.fail)
}
