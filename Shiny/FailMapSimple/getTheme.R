# Sets up standard elements of ggplot2 "theme" to simplify plotting scripts

getTheme <- function(outputType="PRINT", MAP=FALSE){
  # ggplot2 standard theme elements
  if(!MAP){
    themeP <- theme(panel.background   = element_rect(fill = "white"),
                    legend.key         = element_rect(fill = "white") ,
                    panel.grid.major.y = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.spacing      = unit(c(0,0,0,0),"cm"),
                    axis.ticks.x = element_line(color = "black", size = 0.5),
                    axis.ticks.y = element_line(color = "black", size = 0.5),
                    axis.ticks.length=unit(-0.1, "cm"), 
                    axis.text.x = element_text(color = "black", size = textP$sub[outputType]),
                    axis.text.y = element_text(color = "black", size = textP$sub[outputType]),
                    axis.title.x = element_text(color = "black", size = textP$sub[outputType]),
                    axis.title.y = element_text(color = "black", size = textP$sub[outputType]),
                    legend.text  = element_text(color = "black", size = textP$sub[outputType]),
                    legend.title = element_text(color = "black", size = textP$sub[outputType], face = "bold"),
                    legend.key.height=unit(0.7,"line"),
                    plot.margin     = unit(c(0,0,0,0), "cm"),
                    plot.title   = element_text(color = "black", size = textP$head[outputType], hjust = 0.5))
  }
  else{
    themeP <- theme(panel.background = element_rect(fill = "white"),
                    legend.key = element_rect(fill = "white"),
                    panel.grid.major.y = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.spacing      = unit(c(0,0,0,0),"cm"),
                    axis.text.x  = element_blank(),
                    axis.title.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.text.y  = element_blank(),
                    axis.title.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    legend.text  = element_text(color = "black", size = textP$sub[outputType]),
                    legend.title = element_text(color = "black", size = textP$sub[outputType], face = "bold"),
                    plot.title    = element_text(color = "black", size = textP$head[outputType], hjust = 0.5),
                    plot.margin     = unit(c(0,0,0,0), "cm")
                    )
  }

  return(themeP)
  
}