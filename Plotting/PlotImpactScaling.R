# Lightweight plotting function for showing effect of scaling hazard curve
# Madeleine Flint, 2019

PlotImpactScaling <- function(Data){
  require(ggplot2)
  require(reshape2)
  if(!("getTheme" %in% ls(.GlobalEnv))) source(file.path("Plotting","getTheme.R"))
  
  Data.melt <-  melt(Data,id.vars = "Delta",value.name = "pf")
  Data.melt$variable <- factor(as.character(Data.melt$variable), levels = c("Kernel","Median","Nom"))
  pP <- ggplot(data = Data.melt, aes(x=Delta, y=pf, group=variable, color=variable,shape=variable)) + geom_point(size=3) +
     scale_shape_manual(values = c(Kernel=43, Median=1, Nom=42), name = "Data Source",
                        labels = c("Collapse Full Distribution","Median Collapse","Nominal Reliability")) + 
    scale_color_manual(values = c(Kernel="#003399",Median="#6699FF",Nom="black" ), name = "Data Source", 
                       labels = c("Collapse Full Distribution","Median Collapse","Nominal Reliability")) + 
    labs(x = expression(delta), y = expression(~(p[f]^{delta}~-~p[f])/p[f]), 
         title = "Relative Change in Failure Rate Produced by Hazard Scaling") +
    getTheme("NOTE",FALSE) + theme(plot.margin = unit(c(1,1,1,1), "cm"))
  return(pP)
}