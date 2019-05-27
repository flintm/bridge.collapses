---
  title: "Evaluating Re-Routed VIC results"
output: html_notebook
---
# Evaluating re-routed VIC results (obtained November, 2018) for the 35 gauged sites
View(df.Fail.NBI.Gage[rowsToView[order(df.Fail.NBI.Gage[rowsToView,"STAID"])],])
# STAID and VIC-rerouted R2 value (>0.5 is good, 0.7 is very good)
# "01031500"  0.66
# "01049500" 0.55
# "01090800" N/A? # piscataquog river bl everett dam, nr e weare, nh
#"01349711" 0.43
#"01349810" 0.47
#"01365000" 0.65
#"01387400" 0.47
#"01387450" 0.49
#"01591000" 0.51
#"01596500" 0.54
#"01606500" 0.66
#"01611500" 0.56
#"01624800" 0.47
# "01643000" 0.55
#"01643500" 0.46
#"01648000" 0.56
#"01651000" 0.35
#"01662800" 0.44
#"01662800" 0.44
#"02021500" 0.59
#"02164000" 0.73 !s125 reedy*
#"02482550" 0.79 !s3480 pearl*
#"02487500" 0.63
#"04216500" 0.50
#"04234000" 0.49
#"04273800" 0.47
# "06480000" 0.55
#"06891500" 0.32
#"06927000" 0.47
#"07030050" 0.74 !s5970 #hatchie*
#"07069500" 0.64
#"07075000" 0.54
#"10312000" 0.72 !s3800 carson
#"12087000" 0.70 !f214 mashel
#"12358500" 0.74 !s2940 mf flathead

# look at hatchie, pearl, and reedy first
filen <- "USGS_VIC4_RAPID_daily_v1_20171118.csv"
foldn <- "/Volumes/GoogleDrive/My Drive/Research/Climate Scour/VIC Data/HTTP/Bridge_VIC4_RAPID_v1_20171118 (1)/Bridge_VIC4_RAPID_v1_20171118/data"
df.Rapid.VIC.Raw <- read.csv(file.path(foldn,filen), skip = 2)

Bridge.Gage.Mapping <- read.table(file.path(foldn,"VIC.Rapid.Col.Names.txt"),sep="\t",header=T,colClasses = "character")
colnames(df.Rapid.VIC.Raw) <- c("YEAR","MONTH","DAY",Bridge.Gage.Mapping)
save(df.Rapid.VIC.Raw,file = file.path(dirsGit$Data,"20180620_DVIC_RapidRouting_Raw.RData"))

# format into same way had VIC originally formatted
ls.GageRapidRouted <- list()
dates <- df.Rapid.VIC.Raw[,1:3]
# STAID <- c("02482550", "07030050", "02164000")
STAID <- Bridge.Gage.Mapping
for(i in STAID){
  ls.GageRapidRouted[[i]] <- dates
  ls.GageRapidRouted[[i]]$RUNOFF_CFS <- df.Rapid.VIC.Raw[,i]
}
savefile <- paste(gsub("-","",Sys.Date()),"ListReroutedFlows.RData",sep="_")
save(ls.GageRapidRouted,file=file.path(dirsGit$Data,savefile))
rm(savefile)

# get annual maxes
ls.GageRapidRoutedAnnualMax <- lapply(names(ls.GageRapidRouted), 
                                 function(j) sapply(c(1980:2015), 
                                                    function(y) max(ls.GageRapidRouted[[j]]$RUNOFF_CFS[ls.GageRapidRouted[[j]]$YEAR == y], 
                                                                    na.rm=TRUE)
                                 )
)
names(ls.GageRapidRoutedAnnualMax) <- names(ls.GageRapidRouted)

savefile <- paste(gsub("-","",Sys.Date()),"GageRapidRoutingVICDaymetAnnualMaxes.RData",sep="_")
save(ls.GageRapidRoutedAnnualMax,file=file.path(dirsGit$Data,savefile))
rm(savefile)
