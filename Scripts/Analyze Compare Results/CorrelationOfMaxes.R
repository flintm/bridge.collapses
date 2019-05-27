# Performs correlation of Q & T for maximum flows
# Copyright Madeleine Flint, 2016
CorrelationOfMaxes <- function(TYPES = c("MAXQ", "USGS-TMAX", "USGS-VIC-TMAX", "USGS-NLDAS-TMAX", "VIC-NLDAS-TMAX"),
                                  SAVE = TRUE){

load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
source(file.path(dirsGit$Scripts,"Bases.R"))

require(stats)
require(Kendall)
require(SuppDists)
require(pspearman)
require(nsRFA)

rowsToAnalyzeVIC  <- rownames(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID %in% IDsToView & df.Fail.NBI.Gage$YR_FAIL_EST >= "1980-01-01",])
rowsToAnalyzeNLDAS <- rowsToView[df.Fail.NBI.Gage[rowsToView,"YR_FAIL"] > as.Date("1979-01-01")]

kgCfs          <- 0.035315*(1/3600)

ls.corrs.max <- list()
for (type in TYPES){
  ls.corrs.max[[type]] <- list()
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
    ls.corrs.max[[type]][[paste(ls.Bases$bases[i],"P",sep="")]]      <- cor.test(T1,T2,
                                                                                  method="pearson",
                                                                                  conf.level=0.95,
                                                                                  exact=TRUE)
    ls.corrs.max[[type]][[paste(ls.Bases$bases[i],"K",sep="")]]       <- cor.test(T1,T2,
                                                                                   method="kendall",
                                                                                   conf.level=0.95,
                                                                                   exact=TRUE)
    ls.corrs.max[[type]][[paste(ls.Bases$bases[i],"Papprox",sep="")]] <- cor.test(T1,T2,
                                                                                   method="pearson",
                                                                                   conf.level=0.95,
                                                                                   exact=FALSE)
    ls.corrs.max[[type]][[paste(ls.Bases$bases[i],"Kapprox",sep="")]] <- cor.test(T1,T2,
                                                                                   method="kendall",
                                                                                   conf.level=0.95,
                                                                                   exact=FALSE)
    ls.corrs.max[[type]][[paste(ls.Bases$bases[i],"LR",sep="")]]      <- lm(T2 ~ T1)
  }
}

## SAVE ############ ------
if (SAVE) {
  savefile <- paste(gsub("-","",Sys.Date()),"CorrelationsOfMaxes.RData", sep="_")
  save(ls.corrs.max, file = file.path(dirs$DataDirAnalysis,savefile))
}
return(ls.corrs.max)
}
