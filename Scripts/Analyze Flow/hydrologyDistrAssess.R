# 2015-08-10 function to find Anderson-Darling test statistic for distributions commonly used in hydrology,
# including lognormal, Gumbel, Generalized Extreme Value, Pearson Type III, and log-Pearson III.
# can return either or both of parameters and the test statistic (returns a list if using both).
# if ADpFlag is set, an alternate function is used that calculates the p-statisic of the AD test.

hydrologyDistrAssess <- function(x, dist, name = NULL,paramFlag = FALSE, ADFlag = TRUE, ADpFlag = FALSE){
  if (!(dist %in% c("LN","GUMBEL","GEV","P3","LP3","EXP"))) return(warning("unsupported distribution type"))
  if (!paramFlag & !ADFlag & !ADpFlag) return(warning("Must specify output type using paramFlag, ADFlag, and ADpFlag"))

  require(MASS)
  require(nsRFA)
  
  # process input data
  x       <- x[!is.na(x)]
  x[x==0] <- min(x[x!=0])
  
  # find distribution parameters using maximum likelihood
  if (dist %in% c("LP3","LN")){ 
    if (dist == "LP3"){
      xlog <- log(x)
      xlog[xlog==-Inf] <- min(xlog[xlog!=-Inf])
      ml <- ML_estimation(xlog, dist = "P3")
    }
    if (dist=="LN"){
      ml <- fitdistr(x, "lognormal")
    }
  }
  else{
    if (dist == "EXP"){
      ll <- Lmoments(x)
      ml  <- par.exp(ll[1],ll[2])
    }
    else {
      ml <- ML_estimation(x, dist = dist)
      }
  }
  if (any(is.na(ml))){
    warning("NA param for dist ",dist," and data ",name)
    if (paramFlag & (ADFlag | ADpFlag)){
      out <- list(ml = NA, AD = NA)
    }
    else{
      if (paramFlag & !ADFlag & !ADpFlag){
        out <- list(ml = NA)
      }
      else{
        out <- list(AD = NA)
      }
    }
    name <- out
    return(name)
  }

  
  # obtain Anderson-Darling test statistic if flag is set
  if (ADFlag & !ADpFlag){
    F <- switch(dist,
                LN     = sort(plnorm(x, ml$estimate[1],ml$estimate[2])),
                GUMBEL = sort(F.gumb(x,ml[1],ml[2])),
                GEV    = sort(F.GEV(x, ml[1],ml[2],ml[3])),
                P3     = sort(F.gamma(x, ml[1],ml[2],ml[3])),
                LP3    = sort(F.gamma(xlog, ml[1],ml[2],ml[3])),
                EXP    = sort(F.exp(x, ml$xi, ml$alfa))
                )
    F[F==1] <- max(F[F!=1])
    F[F==0] <- min(F[F!=0])
    AD <- A2(F)
  }
  if (ADpFlag){
    AD <- A2_GOFlaio_MF(x, dist=dist)
  }
  
  # return desired output data
  if (paramFlag & (ADFlag | ADpFlag)){
    out <- list(ml = ml, AD = AD)
  }
  else{
    if (paramFlag & !ADFlag & !ADpFlag){
      out <- list(ml = ml)
    }
    else{
      out <- list(AD = AD)
    }
  }
  name <- out
  return(name)
}
