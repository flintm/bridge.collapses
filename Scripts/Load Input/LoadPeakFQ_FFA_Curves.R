# Loads Flood-Frequency-Analysis (FFA) curves created using USGS PeakFQ Version 5.2
# software Bulletin 17 analysis
# Modified 2018-06-27 by Madeleine Flint to load USGS PeakFQ v7.2 Bulletin 17C (EMA)
# results

library(gdata)
library(stringr)

load(file.path(dirs$dfActive,"df.Fail.NBI.Gage.Active.RData"))

colNamesPFA17B <- c("ANN_EXC_PROB",  "BULL17B_EST",	"SYSTEM_RECORD",	"EXPECTED_PROB",	"5PCT_CONF",	"95PCT_CONF")
widths   <- c(12,13,13,13,13,13)
skip <- 83

## NLDAS ------------------------------------------------------------------------
## gauge data ------------------------------------------------------------------------
file.out <- file.path(dirs$OrigDataDir,"PeakFQ","WithMySkews","NLDASg")
filenames   <- list.files(path = file.out, pattern = ".PRT\\>")
gage_sites  <- substr(filenames,1,8)


PFA_NLDASg     <- lapply(1:length(filenames), function(i) read.fwf(file.path(file.out,filenames[i]),
                                                                  77,
                                                                  header=FALSE,
                                                                  sep = "\n",
                                                                  n = 15,
                                                                  skip=skip, #col.names = colNames,
                                                                  stringsAsFactors = FALSE))
names(PFA_NLDASg) <- gage_sites

PFA_NLDASg <- sapply(names(PFA_NLDASg), 
                      function(i) as.data.frame(t(sapply(1:nrow(PFA_NLDASg[[i]]), function(j) as.numeric(unlist(strsplit(PFA_NLDASg[[i]][j,], "[[:space:]]+")))[2:7] ))),
                      USE.NAMES = TRUE,
                      simplify = FALSE)
for (i in names(PFA_NLDASg)){
  colnames(PFA_NLDASg[[i]]) <- colNames
}                      

savefile <- paste(gsub("-","",Sys.Date()),"PeakFQ_PFA_NLDASg_UserRegSkews.RData",sep="_")
save(PFA_NLDASg, file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

## bridge data -------------------------------------------------------------------------------------
file.out <- file.path(dirs$OrigDataDir,"PeakFQ","WithMySkews","NLDASb")
filenames    <- list.files(path = file.out, pattern = ".PRT\\>")
bridge_sites <- gsub("[NLDASBPRT.]","",filenames)

PFA_NLDASb     <- lapply(1:length(filenames), function(i) read.fwf(file.path(file.out,filenames[i]),
                                                                   77,
                                                                   header=FALSE,
                                                                   sep = "\n",
                                                                   n = 15,
                                                                   skip=skip, #col.names = colNames,
                                                                   stringsAsFactors = FALSE))
names(PFA_NLDASb) <- bridge_sites

PFA_NLDASb <- sapply(names(PFA_NLDASb), 
                     function(i) as.data.frame(t(sapply(1:nrow(PFA_NLDASb[[i]]), function(j) as.numeric(unlist(strsplit(PFA_NLDASb[[i]][j,], "[[:space:]]+")))[2:7] ))),
                     USE.NAMES = TRUE,
                     simplify = FALSE)
for (i in names(PFA_NLDASb)){
  colnames(PFA_NLDASb[[i]]) <- colNames
}                      

savefile <- paste(gsub("-","",Sys.Date()),"PeakFQ_PFA_NLDASb_UserRegSkews.RData",sep="_")
save(PFA_NLDASb, file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

## DAYMET-VIC ------------------------------------------------------------------------
## gauge data ------------------------------------------------------------------------
file.out <- file.path(dirs$OrigDataDir,"PeakFQ","WithMySkews","VICg")
filenames   <- list.files(path = file.out, pattern = ".PRT\\>")
gage_sites  <- substr(filenames,1,8)

PFA_VICg     <- lapply(1:length(filenames), function(i) read.fwf(file.path(file.out,filenames[i]),
                                                                   77,
                                                                   header=FALSE,
                                                                   sep = "\n",
                                                                   n = 15,
                                                                   skip=skip, #col.names = colNames,
                                                                   stringsAsFactors = FALSE))
names(PFA_VICg) <- gage_sites

PFA_VICg <- sapply(names(PFA_VICg), 
                   function(i) as.data.frame(sapply(1:nrow(PFA_VICg[[i]]),
                                                    function(j) gsub("--","NA",PFA_VICg[[i]][j,])), stringsAsFactors = FALSE),
                   USE.NAMES = TRUE,
                   simplify = FALSE)

names(PFA_VICg) <- gage_sites

PFA_VICg <- sapply(names(PFA_VICg), 
                     function(i) as.data.frame(t(sapply(1:nrow(PFA_VICg[[i]]), function(j) as.numeric(unlist(strsplit(PFA_VICg[[i]][j,], "[[:space:]]+")))[2:7] ))),
                     USE.NAMES = TRUE,
                     simplify = FALSE)
for (i in names(PFA_VICg)){
  colnames(PFA_VICg[[i]]) <- colNames
}                      

savefile <- paste(gsub("-","",Sys.Date()),"PeakFQ_PFA_VICg_UserRegSkews.RData",sep="_")
save(PFA_VICg, file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

## gauge data B17C------------------------------------------------------------------------
file.out <- "/Users/MM/Downloads"
filenames   <- list.files(path = file.out, pattern = ".PRT\\>")
gage_sites  <- substr(filenames,1,8)
n_gage <- length(gage_sites)

colNames <- c("ANN_EXC_PROB",  "EMA_REG",	"EMA_STAT",	"LOG_VAR_REG",	"5PCT_CONF_REG",	"95PCT_CONF_REG")
widths   <- c(12,13,13,13,13,13)
skip <- 83

PFA_17C_VICRg     <- lapply(1:length(filenames), function(i) read.fwf(file.path(file.out,filenames[i]),
                                                                 77,
                                                                 header=FALSE,
                                                                 sep = "\n",
                                                                 stringsAsFactors = FALSE))
skips <- sapply(1:length(PFA_17C_VICRg), function(i) grep("TABLE 4",PFA_17C_VICRg[[i]]$V1)[1] + 5)
PFA_17C_VICRg <- sapply(1:length(filenames), 
                        function(i) read.fwf(file.path(file.out,filenames[i]),
                                                                  77,
                                                                  header=FALSE,
                                                                  sep = "\n",
                                                                  n = 15,
                                                                  skip=skips[i],
                                                                  stringsAsFactors = FALSE),
                                                         USE.NAMES = TRUE,
                                                         simplify = FALSE)
names(PFA_17C_VICRg) <- gage_sites
                        
PFA_17C_VICRg <- sapply(names(PFA_17C_VICRg), 
                                      function(i) as.data.frame(sapply(1:nrow(PFA_17C_VICRg[[i]]),
                                                                       function(j) gsub("--","NA",PFA_17C_VICRg[[i]][j,])), stringsAsFactors = FALSE),
                                      USE.NAMES = TRUE,
                                      simplify = FALSE)

names(PFA_17C_VICRg) <- gage_sites

PFA_17C_VICRg <- sapply(names(PFA_17C_VICRg),
                   function(i) as.data.frame(t(sapply(1:nrow(PFA_17C_VICRg[[i]]), function(j) as.numeric(unlist(strsplit(PFA_17C_VICRg[[i]][j,], "[[:space:]]+")))[2:7] ))),
                   USE.NAMES = TRUE,
                   simplify = FALSE)
for (i in names(PFA_17C_VICRg)){
  colnames(PFA_17C_VICRg[[i]]) <- colNames
}

savefile <- paste(gsub("-","",Sys.Date()),"PeakFQ_PFA_17C_VICRg_UserRegSkews.RData",sep="_")
save(PFA_17C_VICRg, file=file.path(dirsGit$Data,savefile))
rm(savefile)

## bridge data -------------------------------------------------------------------------------------
file.out <- file.path(dirs$OrigDataDir,"PeakFQ","WithMySkews","VICb")
filenames   <- list.files(path = file.out, pattern = ".PRT\\>")
bridge_sites <- gsub("[VICBPRT.]","",filenames)

PFA_VICb     <- lapply(1:length(filenames), function(i) read.fwf(file.path(file.out,filenames[i]),
                                                                   77,
                                                                   header=FALSE,
                                                                   sep = "\n",
                                                                   n = 15,
                                                                   skip=skip, #col.names = colNames,
                                                                   stringsAsFactors = FALSE))
names(PFA_VICb) <- bridge_sites

PFA_VICb <- sapply(names(PFA_VICb), 
                   function(i) as.data.frame(sapply(1:nrow(PFA_VICb[[i]]),
                                             function(j) gsub("--","NA",PFA_VICb[[i]][j,]))),
                   USE.NAMES = TRUE,
                   simplify = FALSE)

names(PFA_VICb) <- bridge_sites

PFA_VICb <- sapply(names(PFA_VICb), 
                     function(i) as.data.frame(t(sapply(1:nrow(PFA_VICb[[i]]), function(j) as.numeric(unlist(strsplit(as.character(PFA_VICb[[i]][j,]), "[[:space:]]+")))[2:7] ))),
                     USE.NAMES = TRUE,
                     simplify = FALSE)

for (i in names(PFA_VICb)){
  colnames(PFA_VICb[[i]]) <- colNames
}                      

savefile <- paste(gsub("-","",Sys.Date()),"PeakFQ_PFA_VICb_UserRegSkews.RData",sep="_")
save(PFA_VICb, file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

## USGS ------------------------------------------------------------------------
## daily data ------------------------------------------------------------------------
file.out <- file.path(dirs$OrigDataDir,"PeakFQ","WithMySkews","USGSd")
filenames   <- list.files(path = file.out, pattern = ".PRT\\>")
gage_sites  <- substr(filenames,1,8)

PFA_USGSd     <- lapply(1:length(filenames), function(i) read.fwf(file.path(file.out,filenames[i]),
                                                                 77,
                                                                 header=FALSE,
                                                                 sep = "\n",
                                                                 n = 15,
                                                                 skip=skip, #col.names = colNames,
                                                                 stringsAsFactors = FALSE))
names(PFA_USGSd) <- gage_sites

PFA_USGSd <- sapply(names(PFA_USGSd), 
                   function(i) as.data.frame(sapply(1:nrow(PFA_USGSd[[i]]),
                                                    function(j) gsub("--","NA",PFA_USGSd[[i]][j,])), stringsAsFactors = FALSE),
                   USE.NAMES = TRUE,
                   simplify = FALSE)

names(PFA_USGSd) <- gage_sites

PFA_USGSd <- sapply(names(PFA_USGSd), 
                   function(i) as.data.frame(t(sapply(1:nrow(PFA_USGSd[[i]]), function(j) as.numeric(unlist(strsplit(PFA_USGSd[[i]][j,], "[[:space:]]+")))[2:7] ))),
                   USE.NAMES = TRUE,
                   simplify = FALSE)
for (i in names(PFA_USGSd)){
  colnames(PFA_USGSd[[i]]) <- colNames
}                      

savefile <- paste(gsub("-","",Sys.Date()),"PeakFQ_PFA_USGSd_UserRegSkews.RData",sep="_")
save(PFA_USGSd, file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

## peaks data -------------------------------------------------------------------------------------
file.out <- file.path(dirs$OrigDataDir,"PeakFQ","WithMySkews","USGSp")
filenames   <- list.files(path = file.out, pattern = ".PRT\\>")
gage_sites  <- substr(filenames,1,8)

PFA_USGSp     <- lapply(1:length(filenames), function(i) read.fwf(file.path(file.out,filenames[i]),
                                                                  77,
                                                                  header=FALSE,
                                                                  sep = "\n",
                                                                  n = 15,
                                                                  skip=skip, #col.names = colNames,
                                                                  stringsAsFactors = FALSE))
names(PFA_USGSp) <- gage_sites

PFA_USGSp <- sapply(names(PFA_USGSp), 
                    function(i) as.data.frame(sapply(1:nrow(PFA_USGSp[[i]]),
                                                     function(j) gsub("--","NA",PFA_USGSp[[i]][j,])), stringsAsFactors = FALSE),
                    USE.NAMES = TRUE,
                    simplify = FALSE)

names(PFA_USGSp) <- gage_sites

PFA_USGSp <- sapply(names(PFA_USGSp), 
                    function(i) as.data.frame(t(sapply(1:nrow(PFA_USGSp[[i]]), function(j) as.numeric(unlist(strsplit(PFA_USGSp[[i]][j,], "[[:space:]]+")))[2:7] ))),
                    USE.NAMES = TRUE,
                    simplify = FALSE)
for (i in names(PFA_USGSp)){
  colnames(PFA_USGSp[[i]]) <- colNames
}                      

savefile <- paste(gsub("-","",Sys.Date()),"PeakFQ_PFA_USGSp_UserRegSkews.RData",sep="_")
save(PFA_USGSp, file=file.path(dirs$DataDirFrequent,savefile))
rm(savefile)

## skew data -------------------------------------------------------------------------------------
file.out <- file.path(dirs$OrigDataDir,"PeakFQ","WithMySkews","USGSp")
filenames   <- list.files(path = file.out, pattern = ".PRT\\>")
gage_sites  <- substr(filenames,1,8)

PFA_skews     <- sapply(1:length(filenames), function(i) read.fwf(file.path(file.out,filenames[i]),
                                                                  77,
                                                                  header=FALSE,
                                                                  sep = "\n",
                                                                  n = 1,
                                                                  skip=37, #col.names = colNames,
                                                                  stringsAsFactors = FALSE))
PFA_skews    <- sapply(1:length(filenames), function(i) ifelse(grepl("Generalized", PFA_skews[[i]]),
                                                               PFA_skews[[i]],
                                                               read.fwf(file.path(file.out,filenames[i]),
                                                                 77,
                                                                 header=FALSE,
                                                                 sep = "\n",
                                                                 n = 14,
                                                                 skip=37, #col.names = colNames,
                                                                 stringsAsFactors = FALSE)
                                                                )
                                                               )

PFA_skews    <- sapply(1:length(filenames), function(i) ifelse(grepl("Generalized", PFA_skews[[i]][1]),
                                                               PFA_skews[[i]],
                                                               PFA_skews[[i]][grepl("Generalized", PFA_skews[[i]])]
)
)

names(PFA_skews) <- gage_sites
PFA_skews        <- sapply(names(PFA_skews), function(i) as.numeric(substr(PFA_skews[[i]],58,63)),
                           USE.NAMES = TRUE,
                           simplify = TRUE)
