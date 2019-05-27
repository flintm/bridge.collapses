# Figure 3 in "Historical analysis of hydraulic bridge collapses in the continental United States"
# Material and types stacked bar plot for NBI bridges and collapsed bridges
# Copyright Madeleine Flint, 2016

PlotMatTypesBarChart <- function(BridgesDataFrame,nbiDataFrame, PERCENT = TRUE, SAVE = FALSE, outputType = "PRINT", 
                                SIZE = c(3.5,3.5), TOP_STATES = FALSE, embedFonts = FALSE, gs_path = "/usr/local/bin/gs"){
require(plyr)
require(reshape2)
require(ggplot2)
require(gridExtra)
require(scales)
source(file.path(dirsGit$ScriptsPlot,"SetupEncoding.R"))

df.NBI  <- nbiDataFrame[,c("ITEM43A","ITEM43B","STFIPS")]
df.NBI$ITEM43A <- as.character(df.NBI$ITEM43A)
df.NBI[df.NBI$ITEM43A=="2" & !is.na(df.NBI$ITEM43A),"ITEM43A"] <- "1" # lump both types of concrete data
df.NBI[df.NBI$ITEM43A=="4" & !is.na(df.NBI$ITEM43A),"ITEM43A"] <- "3" # lump both types of steel data
df.NBI[df.NBI$ITEM43A=="6" & !is.na(df.NBI$ITEM43A),"ITEM43A"] <- "5" # lump both types of pc data
df.NBI$ITEM43A <- factor(df.NBI$ITEM43A, levels = levelsP$Mat, labels = labelsP$Mat)
df.NBI$ITEM43B <- as.character(df.NBI$ITEM43B)
df.NBI$ITEM43B <- factor(df.NBI$ITEM43B, levels = levelsP$Type, labels = labelsP$Type)
df.NBI <- df.NBI[!(df.NBI$ITEM43B %in% c("orthotropic", "truss deck","arch thru","suspension","stayed girder","lift","bascule","swing","approach","segmental box")),]
counts <- table(df.NBI$ITEM43B)
labelsSort <- labelsP$Type[order(counts,decreasing=TRUE)]
labelsSort <- labelsSort[1:12]
levelsSort <- levelsP$Type[order(counts,decreasing=TRUE)]
levelsSort <- levelsSort[1:12]

df.Fail <- BridgesDataFrame[,c("ITEM_43A_FAIL","ITEM_43B_FAIL","STATE")] 
colnames(df.Fail)[1:2] <- c("ITEM43A","ITEM43B")
df.Fail$ITEM43A <- as.character(df.Fail$ITEM43A)
df.Fail[df.Fail$ITEM43A=="2" & !is.na(df.Fail$ITEM43A),"ITEM43A"] <- "1" # lump both types of concrete data
df.Fail[df.Fail$ITEM43A=="4" & !is.na(df.Fail$ITEM43A),"ITEM43A"] <- "3" # lump both types of steel data
df.Fail[df.Fail$ITEM43A=="6" & !is.na(df.Fail$ITEM43A),"ITEM43A"] <- "5" # lump both types of pc data
df.Fail$ITEM43A <- factor(df.Fail$ITEM43A, levels = levelsP$Mat, labels = labelsP$Mat)
df.Fail$ITEM43B <- as.character(df.Fail$ITEM43B)
df.Fail$ITEM43B <- factor(df.Fail$ITEM43B, levels = levelsSort, labels = labelsSort)

if (TOP_STATES){
  mainStates <- c("VA", "MD", "NY")
  mainSTFIPS <- c(51, 36, 24)
  df.Fail <- df.Fail[df.Fail$STATE %in% mainStates,]
  df.NBI <- df.NBI[df.NBI$STFIPS %in% mainSTFIPS,]
}

labs <- c("US BRIDGES (504K)", "COLLAPSED (35)")
if(PERCENT) labs <- paste("%", labs)

# plots
p <- list()
p[[1]] <- ggplot(df.NBI, aes(x=ITEM43B, fill = ITEM43A)) 
if (PERCENT)  p[[1]] <- p[[1]] + geom_bar(aes(y = (..count..)/sum(..count..)))
else  p[[1]] <- p[[1]] + geom_bar()
p[[1]] <- p[[1]]  + scale_x_discrete(limits = labelsSort) +
  scale_fill_manual(values = colorsP$MatTypes, name = legendsP$Mat)  +
   labs(y = labs[1], x = NULL) + 
  guides(fill = guide_legend(title = NULL, nrow = 2, byrow = TRUE))
if(PERCENT) p[[1]] <- p[[1]] + scale_y_continuous(labels = percent)
else p[[1]] <- p[[1]] + scale_y_continuous(breaks = seq(0,200000,40000), limits = c(0,200000))
p[[1]] <- p[[1]] + theme(panel.margin = unit(c(0,0,2,0), "cm"))
  
p[[2]] <- ggplot(df.Fail, aes(x = ITEM43B, fill = ITEM43A))
  if(PERCENT)    p[[2]] <- p[[2]] +  geom_bar(aes(y = (..count..)/sum(..count..))) 
else p[[2]] <- p[[2]] + geom_bar()
p[[2]] <- p[[2]]  + scale_x_discrete(limits = labelsSort) +
  scale_fill_manual(values = colorsP$MatTypes, name = legendsP$Mat)  +
  labs(y = labs[2], x = NULL) + 
  guides(fill = FALSE) 
if(PERCENT) p[[2]] <- p[[2]] + scale_y_continuous(labels=percent)
else p[[2]] <- p[[2]] + scale_y_continuous(breaks = seq(0,12,2))

for (i in 1:2){
  p[[i]] <- p[[i]] + theme(panel.background   = element_rect(fill = "white"),
                          legend.key         = element_rect(fill = "white") ,
                          panel.grid.major.y = element_blank(),
                          panel.grid.major.x = element_blank(),
                          panel.grid.minor.y = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          axis.title.y = element_text(color = "black", size = textP$reg[outputType]),
                          axis.ticks.y = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.text.y  = element_text(color = "black", size = textP$reg[outputType]),
                          axis.title.x = element_text(color = "black", size = textP$reg[outputType]),
                          legend.text  = element_text(color = "black", size = textP$reg[outputType]),
                          legend.title = element_text(color = "black", size = textP$reg[outputType]),
                          legend.position = "bottom",
                          legend.margin     = unit(0, "cm"),
                          legend.key.size = unit(0.3, "cm"),
                          plot.margin = unit(c(0,0,0,0), "cm")
  )
  if (i==2){ 
    p[[i]] <- p[[i]] + theme(axis.text.x = element_blank())
  }
  else p[[i]] <- p[[i]] + theme(axis.text.x  = element_text(color = "black", size = textP$reg[outputType], angle = 90, hjust = 1, vjust = 0.5))
}

grp <- lapply(p, function(i) ggplot_gtable(ggplot_build(i)) )
grid.arrange(grp[[2]],grp[[1]],heights = c(0.4,1),nrow = 2)
if (SAVE == TRUE){
  pdf(file = file.path(dirsGit$Plots,"Fig3.pdf"),width = SIZE[1], height = SIZE[2],useDingbats = F)
  grid.arrange(grp[[2]],grp[[1]],heights = c(0.4,.9), nrow = 2)
  dev.off()
  
  if (embedFonts){
    require(extrafont)
    Sys.setenv(R_GSCMD=gs_path) 
    embed_fonts(file.path(dirsGit$Plots,"Fig3.pdf")) 
  }
}
return(p)
}
