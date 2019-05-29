# Lightweight plotting function for return period hazard curve
# Madeleine Flint, 2019

PlotHazardCurve <- function(T.delta, d.emph = NA, nCol = 9,
                            d.limits = c(-0.2,0.2),
                            x.limits = c(50,200), y.limits = c(0.005,0.5),
                            AXES = "LOG", 
                            outputType = "NOTE"){
  require(ggplot2)
  require(reshape2)
  require(RColorBrewer)
  if(!("getTheme" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","getTheme.R"))
  
  T.delta.melt <- melt(T.delta, id.vars = "T_0")
  colors <- brewer.pal(nCol, "RdBu")
  names(colors) <- as.character(seq(d.limits[1],d.limits[2],length.out = nCol))
  colors["0"]   <- "black"
  colors        <- colors[levels(T.delta.melt$variable)]
  T.delta.melt$alpha <- "back"
  T.delta.melt[T.delta.melt$variable %in% c("0", d.emph[!is.na(d.emph)]),"alpha"] <- "emph"
  pT <- ggplot(data=T.delta.melt, aes(x=T_0, y=value, group=variable, color=variable, alpha = alpha)) + 
    geom_line() +  labs(x=expression(T[R]), y = expression(lambda(T[R])), 
         title = "Generic Hazard Curve") + 
    scale_color_manual(values = colors, name = expression(delta)) + 
     scale_alpha_manual(values = alphasP$emph, guide = FALSE) +
    # guides(color = guide_legend(override.aes = list(values = sort(c(-0.2,0,as.numeric(d.emph/100),0.2)))))+
  getTheme(outputType = outputType, MAP = FALSE) + 
    theme( axis.ticks.x = element_blank(),
           axis.ticks.y = element_blank(),
           axis.text.x = element_text(color = "black", angle = 90, vjust = 0.5, hjust = 1),
           legend.position = "bottom") 
  if(AXES == "LOG") pT <- pT + 
    scale_x_log10(limits = x.limits, expand=c(0,0)) + scale_y_log10(limits = y.limits, expand = c(0,0)) 
  return(pT)
}
