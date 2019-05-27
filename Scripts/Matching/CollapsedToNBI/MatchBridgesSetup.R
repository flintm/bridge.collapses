# setup for debugging of bridge matching
DataDir <- "~/Documents/Research/Climate_Scour_Local/R_data/csv"
load("~/Documents/Research/Climate_Scour_Local/R_data/Failed_Bridges_Hydraulic.Rdata") 
load("~/Documents/Research/Climate_Scour_Local/R_data/NBI_bridges_over_water (original).RData")

source('~/Documents/R/convert.df.classes.R')
source('~/Documents/R/clean.df.strings.R')

# --------- Data Frames ---------

FailDataFrame <- df.failures.hyd
nbiDataFrame <- df.USbridges.rivers[c(1:20),]

FailDataFrame <- df.failures.hyd[seq(1,nrow(df.failures.hyd), by = 50),]
nbiDataFrame <- df.USbridges.rivers[c(1:10),]

FailDataFrame <- df.failures.hyd[seq(1,nrow(df.failures.hyd), by = 20),]
nbiDataFrame <- df.USbridges.rivers[seq(1,nrow(df.USbridges.rivers), by = 20),]

# ------- Source & run ------

source('~/Documents/Research/Climate_Scour_Local/R_data/20141114_Match_Failed_Bridges.R')
test <- Match_Failed_Bridges(FailDataFrame,nbiDataFrame,DataDir)


View(test)