PlaceAnnotation <- function(df,T1l,T2l,limits, breaks, ANNOTATE_LOC = "BR", AXES = "LOG",VERBOSE=FALSE,outputType = "PRINT"){

  if (AXES == "LOG"){ 
    if(ANNOTATE_LOC == "BR"){
      x <- ifelse(6*breaks[length(breaks)] < limits[2],6*breaks[length(breaks)], limits[2] - 0.0001*breaks[length(breaks)])
      if(VERBOSE==TRUE) print(paste("ratio of breaks:",breaks[length(breaks)]/breaks[1]))
      if (breaks[length(breaks)]/breaks[1]>=1e+07){
        if(VERBOSE==TRUE) print(">=7 orders of magnitude")
        ylims <- c(limits[1]+0.01*breaks[1],limits[1]+0.5*breaks[1]*(log10(breaks[2]-log10(breaks[1]))))
      }
      else{if (breaks[length(breaks)]/breaks[1]>=1e+04){
        if(VERBOSE==TRUE) print("4-7 orders of magnitude")
        ylims <- c(limits[1]+0.01*breaks[1],limits[1]+3.6*breaks[1]*(log10(breaks[2]-log10(breaks[1]))))
      }
        else{if(breaks[length(breaks)]/breaks[1]>=5e+02){
          if(VERBOSE==TRUE) print(">2 orders of magnitude, 500")
          if(grepl("T_MAX_PEAK_LP3",T2l)){
            if(VERBOSE==TRUE) print("in special LP3 peak")
            ylims <- c(limits[1]+0.01*breaks[1], limits[1]+0.7*breaks[1]*(log10(breaks[2]-log10(breaks[1]))))
          }
          else{
            ylims <- c(limits[1]+0.01*breaks[1], limits[1]+1.4*breaks[1]*(log10(breaks[2]-log10(breaks[1]))))
          }
        }
          else{
            if (breaks[length(breaks)]/breaks[1]>=3e+02){
              if(VERBOSE==TRUE) print(">2 orders of magnitude, 300")
              ylims <- c(limits[1]+0.01*breaks[1], limits[1]+1.5*breaks[1]*(log10(breaks[2]-log10(breaks[1]))))
            }
            else{
              if (breaks[length(breaks)]/breaks[1]>1e+02){
                if(VERBOSE==TRUE) print(">2 orders of magnitude, 100")
                if (grepl("HEC",T2l)){
                  if(VERBOSE==TRUE) print("in special HEC")
                  ylims <- c(limits[1]+0.01*breaks[1], limits[1]+0.3*breaks[1]*(log10(breaks[2]-log10(breaks[1]))))
                }
              }
              else{
                if (breaks[length(breaks)]/breaks[1]==1e+02){
                  if(VERBOSE==TRUE) print("2 orders of magnitude, 100")
                  ylims <- c(limits[1]+0.01*breaks[1], limits[1]+0.5*breaks[1]*(log10(breaks[2]-log10(breaks[1]))))
                }
                else{
                  if(VERBOSE==TRUE) print("<2 orders of magnitude")
                  ylims <- c(limits[1]+0.01*breaks[1],limits[1]+0.7*breaks[1]*(log10(breaks[2]-log10(breaks[1]))))
                }
              }}
          }
        }}
      if(textP$annotate[outputType] > 2){ 
        ylimFactor <- ifelse(limits[2]>100000, 1.1, 1.25)
        ylims[2] <- ylims[2]*ylimFactor
      }
      if(grepl("part",T2l,ignore.case=TRUE)) ylims[2] <- ylims[2]*1.5
      x <- c(rep(x,4),x-(limits[2]-breaks[length(breaks)]))
      y <- 10^seq(log10(ylims[1]),log10(ylims[2]),length.out=5)
      hjust <- 1
    }
    
    if(ANNOTATE_LOC == "UL"){
      x <- ifelse(0.01*breaks[1] > limits[1],0.01*breaks[1], limits[1] + 0.0000000001*breaks[length(breaks)])
      if (breaks[length(breaks)]/breaks[1]>=1e+05){
        if(VERBOSE==TRUE) print("5 orders of magnitude")
        yl1 <- ifelse(limits[2]-1.5*breaks[length(breaks)]*log10(breaks[length(breaks)-1])>0,
                      limits[2]-1.5*breaks[length(breaks)]*log10(breaks[length(breaks)-1]),
                      limits[2]-0.5*breaks[length(breaks)]*log10(breaks[length(breaks)-1]))
        if (yl1 < 0) yl1 <- limits[2]-0.1*breaks[length(breaks)]*log10(breaks[length(breaks)-1])
        ylims <- c(yl1,limits[2]-0.01*breaks[1])
        if(VERBOSE==TRUE) print(ylims)
      }
      else{
        if(VERBOSE==TRUE) print(paste("ratio of breaks:",breaks[length(breaks)]/breaks[1]))
        if(breaks[length(breaks)]/breaks[1]>=1e+04){
          if(VERBOSE==TRUE) print("4 orders of magnitude")
          yl1 <- ifelse(limits[2]-0.4*breaks[length(breaks)]*log10(breaks[length(breaks)-1])>0,
                        limits[2]-0.4*breaks[length(breaks)]*log10(breaks[length(breaks)-1]),
                        limits[2]-0.2*breaks[length(breaks)]*log10(breaks[length(breaks)-1]))
          ylims <- c(yl1,limits[2]-breaks[2])
          if(VERBOSE==TRUE) print(limits[2]-0.4*breaks[length(breaks)]*log10(breaks[length(breaks)-1])>0)
        }
        else{
          if(breaks[length(breaks)]/breaks[1]>=1e+03){
            if(VERBOSE==TRUE) print("3 orders of magnitude")
            yl1 <- ifelse(limits[2]-2.45*breaks[length(breaks)]*log10(breaks[length(breaks)-1])>0,
                          limits[2]-2.45*breaks[length(breaks)]*log10(breaks[length(breaks)-1]),
                          limits[2]-0.65*breaks[length(breaks)]*log10(breaks[length(breaks)-1]))
            if(yl1<0) yl1 <- limits[2]-0.45*breaks[length(breaks)]*log10(breaks[length(breaks)-1])
            if(yl1<0) yl1 <- limits[2]-0.25*breaks[length(breaks)]*log10(breaks[length(breaks)-1])
            ylims <- c(yl1,limits[2]-breaks[2])
          }
          else{
            if(breaks[length(breaks)]/breaks[1]>1e+02){
              if(VERBOSE==TRUE) print("2+-4 orders of magnitude")
              yl1 <- ifelse(limits[2]-1.9*breaks[length(breaks)]*log10(breaks[length(breaks)-1])>0,
                            limits[2]-1.9*breaks[length(breaks)]*log10(breaks[length(breaks)-1]),
                            limits[2]-0.2*breaks[length(breaks)]*log10(breaks[length(breaks)-1]))
              
              if (grepl("GEV",T2l)){
                yl1 <- yl1 + breaks[length(breaks)]*0.5*log10(breaks[length(breaks)-1])
              }
              ylims <- c(yl1,limits[2]-breaks[2])
            }
            else{
              if(breaks[length(breaks)]/breaks[1]==1e+02){
                if(VERBOSE==TRUE) print("2 orders of magnitude")
                if(VERBOSE==TRUE) print(max(df$T1,na.rm=TRUE))
                if (limits[2] - breaks[length(breaks)] > 100000){
                  if(VERBOSE==TRUE) print("In the limits dist >100,000 case")
                  yl1 <- ifelse(limits[2]-2.0*breaks[length(breaks)]*log10(breaks[length(breaks)-1])>0,
                                limits[2]-2.0*breaks[length(breaks)]*log10(breaks[length(breaks)-1]),
                                limits[2]-0.35*breaks[length(breaks)]*log10(breaks[length(breaks)-1]))
                }
                else{
                  if(limits[2] - breaks[length(breaks)] > 10000){
                    if(VERBOSE==TRUE) print("In the >10,000 case")
                    yl1 <- ifelse(limits[2]-1.0*breaks[length(breaks)]*log10(breaks[length(breaks)-1])>0,
                                  limits[2]-1.0*breaks[length(breaks)]*log10(breaks[length(breaks)-1]),
                                  limits[2]-0.15*breaks[length(breaks)]*log10(breaks[length(breaks)-1]))
                    if(VERBOSE==TRUE) print(yl1)
                  }
                  else{
                    if(floor(max(df$T1,na.rm=TRUE))==100){
                      if(VERBOSE==TRUE) print("Hit 100 floor")
                      yl1 <- ifelse(limits[2]-3.0*breaks[length(breaks)]*log10(breaks[length(breaks)-1])>0,
                                    limits[2]-3.0*breaks[length(breaks)]*log10(breaks[length(breaks)-1]),
                                    limits[2]-0.6*breaks[length(breaks)]*log10(breaks[length(breaks)-1]))
                    }
                    else{
                      yl1 <- ifelse(limits[2]-1.8*breaks[length(breaks)]*log10(breaks[length(breaks)-1])>0,
                                    limits[2]-1.8*breaks[length(breaks)]*log10(breaks[length(breaks)-1]),
                                    limits[2]-0.8*breaks[length(breaks)]*log10(breaks[length(breaks)-1]))
                      if(VERBOSE==TRUE) print("In else case")
                    }
                  }}
                ylims <- c(yl1,limits[2]-breaks[2])
              }
              else{
                if(breaks[length(breaks)]/breaks[1]>1e+02){
                  if(VERBOSE==TRUE) print(">1, <2 orders of magnitude")
                  yl1 <- limits[2]-0.01*breaks[length(breaks)]*log10(breaks[length(breaks)-1])
                  ylims <- c(yl1,limits[2]-breaks[2])
                }
                else{
                  if(VERBOSE==TRUE) print("<=1 order of magnitude")
                  if(floor(max(df$T1,na.rm=TRUE)) < 200){
                    if(VERBOSE==TRUE) print("hit 200 floor")
                    yl1 <- limits[2] - .05*breaks[length(breaks)]*log10(breaks[length(breaks)-1])
                    ylims <- c(yl1,limits[2]-0.5*breaks[2])
                    if(VERBOSE==TRUE) print(ylims)
                  }
                  else {yl1 <- limits[2]-0.055*breaks[length(breaks)]*log10(breaks[length(breaks)-1])
                  ylims <- c(yl1,limits[2]-breaks[2])}
                }
                
              }
            }}}
      }
      if(VERBOSE==TRUE) print(ylims)
      if (ylims[2] < ylims[1]){ 
        ylims <- c(ylims[2], ylims[1])
        if(VERBOSE==TRUE) print("switched lims")
        if(VERBOSE==TRUE) print(ylims)
      }
      hjust <- 0
      if(textP$annotate[outputType] > 2){ 
        if(VERBOSE==TRUE) print("*** adjusting ylim for UL")
        ylims[1] <- ylims[1]*0.85
      }
      y <- 10^seq(log10(ylims[1]),log10(ylims[2]),length.out=4)
    }
  }
  else{ # not in log-scale
    if(ANNOTATE_LOC == "BR"){
      x     <- limits[2] - 0.03*(limits[2]-limits[1])
      y     <- c(limits[1] + 0.25*(limits[2] - limits[1]), 
                 limits[1] + 0.2*(limits[2] - limits[1]),
                 limits[1] + 0.15*(limits[2] - limits[1]),
                 limits[1] + 0.1*(limits[2] - limits[1]),
                 limits[1] + 0.05*(limits[2] - limits[1]))
      hjust <- 1
    }
    if(ANNOTATE_LOC == "UL"){
      x     <- limits[1] + 0.03*(limits[2]-limits[1])
      y     <- c(limits[2] - 0.1*(limits[2] - limits[1]), 
                 limits[2] - 0.2*(limits[2] - limits[1]),
                 limits[2] - 0.05*(limits[2] - limits[1]),
                 limits[1] + 0.15*(limits[2] - limits[1]))
      hjust <- 0
    }
  }
  return(data.frame(x=x,y=y,hjust=hjust))
}
