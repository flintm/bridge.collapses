PlaceAnnotation <- function(min,max,nRows, ANNOTATE_LOC = "TL",SCALE = "LOG", INDENT = rep(FALSE,nRows)){
  if(!any(ANNOTATE_LOC == c("TL","BR","TR","BL"))) warning("ANNOTATE_LOC must be [T,B][L,R] (e.g., TL = top left)", immediate. = TRUE)
  if(!any(SCALE == c("LOG","LINEAR"))) warning("SCALE must be LOG or LINEAR", immediate. = TRUE)
  y <- switch(substr(ANNOTATE_LOC,1,1),
              T = seq(0.99, by = -0.05, length.out = nRows),
              B = seq(0.01, by = 0.05, length.out = nRows)[nRows:1])
  
  x <- switch(substr(ANNOTATE_LOC,2,2),
              L = rep(0.005,nRows),
              R = rep(0.995,nRows))
  x[INDENT] <- switch(substr(ANNOTATE_LOC,2,2),
                      L = x[INDENT] + 0.04,
                      R = x[INDENT] - 0.08)
  if (SCALE == "LOG"){
    x <- max^x/min^(x-1)
    y <- max^y/min^(y-1)
  }
  else{ # scale must be linear
    x <- min + x*(max-min)
    y <- min + y*(max-min)
  }
  
  hjust <- switch(substr(ANNOTATE_LOC,2,2),
                  L = 0,
                  R = 1)
  df.text <- data.frame(x = x, y = y, hjust = hjust)
  return(df.text)
}