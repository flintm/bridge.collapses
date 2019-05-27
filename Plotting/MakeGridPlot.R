# ARRANGE AND PRINT PLOTS -----------------------------------
MakeGridPlot <- function(grp, SIZE = c(10.5, 7.5), SCREEN = FALSE, SAVE = FALSE, SAVENAME = "gridPlot",
                         embedFonts = TRUE, gs_path = "/usr/local/bin/gs"){
  if (embedFonts){
    library(extrafont)
    Sys.setenv(R_GSCMD = gs_path)

  }
if(length(grp)==1){
  if(SCREEN) grp[[1]]
  if (SAVE){
#     grp[[1]]
#     pdf(file = file.path(dirsGit$Plots,paste(SAVENAME,"pdf",sep=".")), width = SIZE[1], height = SIZE[2],useDingbats = F)
#     print(grp[[1]])
#     dev.off()
    ggsave(grp[[1]],filename = file.path(dirsGit$Plots,paste(SAVENAME,"pdf",sep=".")), width = SIZE[1], height = SIZE[2])
  }
}
if(length(grp)==2){
  if(SCREEN) grid.arrange(grp[[1]],grp[[2]], ncol=2)
  if (SAVE){
    pdf(file = file.path(dirsGit$Plots,paste(SAVENAME,"pdf",sep=".")), width = SIZE[1], height = SIZE[2],useDingbats = F)
    grid.arrange(grp[[1]],grp[[2]], ncol=2)
    dev.off()
  }
}
if(length(grp)==3){
  if(SCREEN) grid.arrange(grp[[1]],grp[[2]], grp[[3]],ncol=3)
  if (SAVE){
    pdf(file = file.path(dirsGit$Plots,paste(SAVENAME,"pdf",sep=".")), width = SIZE[1], height = SIZE[2],useDingbats = F)
    grid.arrange(grp[[1]],grp[[2]],grp[[3]], ncol=3)
    dev.off()
  }
}
if(length(grp)==4){
  if(SCREEN) grid.arrange(grp[[1]],grp[[2]],grp[[3]],grp[[4]], as.table=TRUE)
  if (SAVE){
    pdf(file = file.path(dirsGit$Plots,paste(SAVENAME,"pdf",sep=".")), width = SIZE[1], height = SIZE[2],useDingbats = F)
    grid.arrange(grp[[1]],grp[[2]],grp[[3]],grp[[4]], as.table=TRUE)
    dev.off()
  }
}
if(length(grp)==5){
  if(SCREEN) grid.arrange(grp[[1]],grp[[2]],grp[[3]],grp[[4]],grp[[5]], as.table=TRUE)
  if (SAVE){
    pdf(file = file.path(dirsGit$Plots,paste(SAVENAME,"pdf",sep=".")), width = SIZE[1], height = SIZE[2],useDingbats = F)
    grid.arrange(grp[[1]],grp[[2]],grp[[3]],grp[[4]],grp[[5]], as.table=TRUE)
    dev.off()
  }
}
if(length(grp)==6){
  if(SCREEN) grid.arrange(grp[[1]],grp[[2]],grp[[3]],grp[[4]],grp[[5]],grp[[6]], ncol = 2, nrow = 3)
  if (SAVE){
    pdf(file = file.path(dirsGit$Plots,paste(SAVENAME,"pdf",sep=".")), width = SIZE[1], height = SIZE[2],useDingbats = F)
    grid.arrange(grp[[1]],grp[[2]],grp[[3]],grp[[4]],grp[[5]],grp[[6]], ncol = 2, nrow = 3)
    dev.off()
  }
}
if(length(grp)==7){
  if(SCREEN) grid.arrange(grp[[1]],grp[[4]],grp[[7]],grp[[2]],grp[[5]],grp[[3]],grp[[6]], ncol = 3, nrow = 3)
  if (SAVE){
    pdf(file = file.path(dirsGit$Plots,paste(SAVENAME,"pdf",sep=".")), width = SIZE[1], height = SIZE[2],useDingbats = F)
    grid.arrange(grp[[1]],grp[[4]],grp[[7]],grp[[2]],grp[[5]],grp[[3]],grp[[6]], as.table=TRUE)
    dev.off()
  }
}
if(length(grp)==8){
  if(SCREEN) grid.arrange(grp[[1]],grp[[4]],grp[[7]],grp[[2]],grp[[5]],grp[[8]],grp[[3]],grp[[6]], as.table=TRUE)
  if (SAVE){
    pdf(file = file.path(dirsGit$Plots,paste(SAVENAME,"pdf",sep=".")), width = SIZE[1], height = SIZE[2],useDingbats = F)
    grid.arrange(grp[[1]],grp[[4]],grp[[7]],grp[[2]],grp[[5]],grp[[8]],grp[[3]],grp[[6]], as.table=TRUE)
    dev.off()
  }
}
if(length(grp)==9){
  if(SCREEN) grid.arrange(grp[[1]],grp[[2]],grp[[3]],grp[[4]],grp[[5]],grp[[6]],grp[[7]],grp[[8]],grp[[9]], ncol = 3, nrow = 3)
  if (SAVE){
    pdf(file = file.path(dirsGit$Plots,paste(SAVENAME,"pdf",sep=".")), width = SIZE[1], height = SIZE[2],useDingbats = F)
    grid.arrange(grp[[1]],grp[[2]],grp[[3]],grp[[4]],grp[[5]],grp[[6]],grp[[7]],grp[[8]],grp[[9]], ncol = 3, nrow = 3)
    dev.off()
  }
}

if(length(grp)==10){
  if(SCREEN) grid.arrange(grp[[1]],grp[[4]],grp[[7]],grp[[2]],grp[[5]],grp[[8]],grp[[3]],grp[[6]],grp[[9]],grp[[10]], as.table=TRUE)
  if (SAVE){
    pdf(file = file.path(dirsGit$Plots,paste(SAVENAME,"pdf",sep=".")), width = SIZE[1], height = SIZE[2],useDingbats = F)
    grid.arrange(grp[[1]],grp[[4]],grp[[7]],grp[[2]],grp[[5]],grp[[8]],grp[[3]],grp[[6]],grp[[9]],grp[[10]], as.table=TRUE)
    dev.off()
  }
}
  if(embedFonts){
    embedFonts(file = file.path(dirsGit$Plots,paste(SAVENAME,"pdf",sep=".")))
  }
}
