# Lightweight plotting function for return period hazard curve
# Madeleine Flint, 2019

PlotHazardCurve <- function(T.delta, d.emph = NA, nCol = 9,
                            d.limits = c(-0.2,0.2),
                            x.limits = c(50,200), y.limits = c(0.005,0.5),
                            AXES = "LOG", 
                            STATIC = FALSE,
                            outputType = "NOTE"){
  require(ggplot2)
  require(reshape2)
  require(RColorBrewer)
  if(!("getTheme" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","getTheme.R"))
  T.lab <- 80
  d <- ifelse(!is.na(d.emph),
              ifelse(d.emph!="0",paste0(as.numeric(d.emph)*100,"%"),""),
              "")
  sgn <- ifelse(sign(as.numeric(d.emph))>0,"+","")
  d <- paste0(sgn,d)
  
  T.delta.melt <- melt(T.delta, id.vars = "T_0")
  colors <- brewer.pal(nCol, "RdBu")
  names(colors) <- as.character(seq(d.limits[1],d.limits[2],length.out = nCol))
  colors["0"]   <- "black"
  colors        <- colors[levels(T.delta.melt$variable)]
  T.delta.melt$alpha <- "back"
  T.delta.melt[T.delta.melt$variable %in% c("0", d.emph[!is.na(d.emph)]),"alpha"] <- "emph"
  T.delta.melt$line <- "back"
  T.delta.melt[T.delta.melt$variable %in% c("0", d.emph[!is.na(d.emph)]),"line"] <- "emph"
  
  df.text <- data.frame(x = c(rep(T.lab,2),120),
                        y = c(min(T.delta[which.min(abs(T.delta$T_0-T.lab)),1],y.limits[2]),
                              max(y.limits[1],T.delta[which.min(abs(T.delta$T_0-T.lab)),nCol-1]),
                              T.delta[which.min(abs(T.delta$T_0-120)),ifelse(!is.na(d.emph),d.emph,"0")]),
                        label = c("-20%\nflooding","+20% \nflooding",d),
                        hjust = c(1,0,ifelse(sgn=="+",0,1)),
                        vjust = c(1,0,ifelse(sgn=="+",0,1)),
                        variable = c("-0.2","0.2",ifelse(!is.na(d.emph),d.emph,"0")),
                        alpha = c("back","back","emph"))
  if(is.na(d.emph)) df.text <- df.text[1:2,]
  pT <- ggplot(data=T.delta.melt, aes(x=T_0, y=value)) + 
    geom_line(aes(group=variable, color=variable, alpha = alpha, size = line)) +  
    labs(x=expression(T[R]), y = expression(lambda(T[R])==1/T[R]), 
         title = expression("Hazard Curve for \nFlood Return Periods")) + 
   
    scale_color_manual(values = colors, name = expression(delta), guide = FALSE) + 
     scale_alpha_manual(values = alphasP$emph, guide = FALSE) +
    scale_size_manual(values = sizesP$emph, guide = FALSE) 
  
  if(!STATIC){
    pT <- pT +  geom_text(data = df.text, aes(x=x,y=y,label=label,hjust=hjust,vjust=vjust,
                                              color = variable, alpha = alpha), size = textP$annotate[outputType])
    if(is.na(d.emph) | d.emph=="0"){
      pT <- pT +  annotate("segment", x = 1.5*df.text[2,"x"], xend = 1.6*df.text[2,"x"], 
                           y = T.delta[which.min(abs(T.delta$T_0-1.5*df.text[2,"x"])),nCol-1], 
                           yend =  1.4*T.delta[which.min(abs(T.delta$T_0-1.5*df.text[2,"x"])),nCol-1], colour = colors["0.2"], size=1,
                           arrow=arrow(length = unit(0.3,"cm"), type = "closed"))+
        annotate("segment", x = 100, xend = 95, 
                 y = T.delta[which.min(abs(T.delta$T_0-100)),1], 
                 yend =  0.7*T.delta[which.min(abs(T.delta$T_0-100)),1], colour = colors["-0.2"], size=1,
                 arrow=arrow(length = unit(0.3,"cm"), type = "closed"))
    }
  }
 
  pT <- pT +

    # guides(color = guide_legend(override.aes = list(values = sort(c(-0.2,0,as.numeric(d.emph/100),0.2)))))+
  getTheme(outputType = outputType, MAP = FALSE) + 
    theme( axis.ticks.x = element_blank(),
           axis.ticks.y = element_blank(),
           axis.text.x = element_text(color = "black", angle = 90, vjust = 0.5, hjust = 1),
           legend.position = "bottom",
           plot.margin = unit(c(0.5,0.25,0.25,0.25),"cm")) 
  if(AXES == "LOG") pT <- pT + 
    scale_x_log10(limits = x.limits, expand=c(0.05,0.05)) + scale_y_log10(limits = y.limits, expand = c(0,0)) 
  return(pT)
}
