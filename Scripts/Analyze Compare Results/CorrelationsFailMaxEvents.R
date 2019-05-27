# Performs correlation of Q and T estimates related to failure and collapse
# Copyright Madeleine Flint, 2016
# modified to combine failure and collapse by Madeleine Flint, 2018-06-22

CorrelationsFailMaxEvents <- function(BridgesDataFrame = NULL,TYPES = c("FAILQ", "USGS","MAXQ", "USGS-TMAX"),
                                  SAVE = TRUE, VERBOSE = FALSE){
  
source(file.path(dirsGit$Scripts,"Helper","Bases.R"))
  
if(is.null(BridgesDataFrame)) load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))
else df.Fail.NBI.Gage <- BridgesDataFrame

require(stats)
require(Kendall)
require(SuppDists)
require(pspearman)
require(nsRFA)

rowsToAnalyzeVIC   <- rowsToView[df.Fail.NBI.Gage[rowsToView,"YR_FAIL"] > as.Date("1980-01-01")]
rowsToAnalyzeVIC   <- rowsToAnalyzeVIC[rowsToAnalyzeVIC!="132"] # missing from new routing model
rowsToAnalyzeNLDAS <- rowsToView[df.Fail.NBI.Gage[rowsToView,"YR_FAIL"] > as.Date("1979-01-01")]

kgCfs          <- 0.035315*(1/3600)

ls.corrs <- list()
for (type in TYPES){
  if (VERBOSE) print(type)
  ls.corrs[[type]] <- list()
  ls.Bases       <- Bases(type)
  for (i in 1:length(ls.Bases$bases)){
    T1 <- df.Fail.NBI.Gage[rowsToView,ls.Bases$T1ls[i]]
    T2 <- df.Fail.NBI.Gage[rowsToView,ls.Bases$T2ls[i]]
    if (grepl("Q",type)){
      if (grepl("NLDAS_BRIDGE",ls.Bases$T1ls[i],ignore.case = TRUE))  T1 <- T1*kgCfs*df.Fail.NBI.Gage[rowsToView,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"]
      if (grepl("NLDAS_GAGE",ls.Bases$T1ls[i],ignore.case = TRUE))    T1 <- T1*kgCfs*df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]
      if (grepl("NLDAS_BRIDGE",ls.Bases$T2ls[i],ignore.case = TRUE))  T2 <- T2*kgCfs*df.Fail.NBI.Gage[rowsToView,"DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM"]
      if (grepl("NLDAS_GAGE",ls.Bases$T2ls[i],ignore.case = TRUE))    T2 <- T2*kgCfs*df.Fail.NBI.Gage[rowsToView,"DRAIN_SQKM"]
    }
    ls.corrs[[type]][[paste(ls.Bases$bases[i],"P",sep="")]]      <- cor.test(T1,T2,
                                                                                  method="pearson",
                                                                                  conf.level=0.95,
                                                                                  exact=TRUE)
    ls.corrs[[type]][[paste(ls.Bases$bases[i],"K",sep="")]]       <- cor.test(T1,T2,
                                                                                   method="kendall",
                                                                                   conf.level=0.95,
                                                                                   exact=TRUE)
    ls.corrs[[type]][[paste(ls.Bases$bases[i],"Papprox",sep="")]] <- cor.test(T1,T2,
                                                                                   method="pearson",
                                                                                   conf.level=0.95,
                                                                                   exact=FALSE)
    ls.corrs[[type]][[paste(ls.Bases$bases[i],"Kapprox",sep="")]] <- cor.test(T1,T2,
                                                                                   method="kendall",
                                                                                   conf.level=0.95,
                                                                                   exact=FALSE)
    ls.corrs[[type]][[paste(ls.Bases$bases[i],"LR",sep="")]]      <- lm(T2 ~ T1)
  }
}


## SAVE ############ ------
if (SAVE){
  savefile <- paste(gsub("-","",Sys.Date()),"CorrelationsOfFailures.RData", sep="_")
  save(ls.corrs, file = file.path(dirs$DataDirAnalysis,savefile))
}
return(ls.corrs)
}
