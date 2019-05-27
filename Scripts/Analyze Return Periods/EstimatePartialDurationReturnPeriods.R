# Function to estimae the return periods of maximum and other important events in a flow history
# using log-log interpolation of input results of a partial duration analysis OR a flow vector
# Written by Madeleine Flint, 2018-07-06

# Capable of returning a vector of values of different Q if names are used in Q to indicate types

EstimatePartialDurationReturnPeriod <- function(Q = c(FAIL = NA_real_, FAILPM = NA_real_,FAILYRMAX=NA_real_,
                                                      MAX = NA_real_,MAXPREFAIL=NA_real_, Q100 = NA_real_, Q500 = NA_real_),
                                                df.PartDur = NA, # Input df.PartDur should have columns "Flow" and "Freq"
                                                nPart   = NA, # required if using analysis results (number of values)
                                                df.Flow = NA, # gage record in place of partial duration results
                                                colSuff = NA){   # column name suffix (e.g., "D_PARTDUR_USGS")
  
  if(is.na(df.PartDur) & is.na(df.Flow)) error('Must input partial duration data or flow record')
  if(class(df.PartDur) == "data.frame" & class(df.Flow) == "data.frame") error('Must input only ONE of partial duration data or flow record')
  require(Hmisc)
  df.out <- data.frame()
  
  TYPES = names(Q)
  if(is.null(TYPES)){
    warning('Input a named vector for Q to return with column names')
    TYPES <- "GENERIC"
    names(Q) <- "GENERIC"
  }
  
  # if partial duration analysis has been completed, assuming from USGS format
  if(class(df.PartDur=="data.frame")){
    PartialFlow <- df.PartDur$Flow
    PartialExc  <- 1 - df.PartDur$Freq
    PartialExc  <- c(1 - 1e-05, PartialExc[2:8],  1/nPart)
    log_flow    <- log(PartialFlow)[!is.na(log(PartialFlow))]
    low_freq    <- log(PartialExc)[!is.na(log(PartialFlow))]
    for(type in TYPES){
      log_Val   <- log(Q[type])
      ExceedDur <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
      Tval      <- max(1/ExceedDur/365.25,1)
      df.out[,paste("T",type,colSuff,sep="_")] <- Tval
    }
  }
  
  # if raw flow data is available use empirical CDF
  if(class(df.Flow)=="data.frame"){
    dts       <- df.Flow$Date[2:nrow(df.Flow)] - df.Flow$Date[1:(nrow(df.Flow)-1)]
    if(!all(as.numeric(dts)==1)) error('Input flow values must be at uniform spacing of 1 day')
    ecdfFun   <- ecdf(df.Flow$Flow)
    for (type in TYPES){
      exceedVal <- 1 - ecdfFun(Q[type])
      if (exceedVal == 0) exceedVal <- 1/nrow(df.Flow)
      Tval <- max(1/exceedVal/365.25,1)
      df.out[,paste("T",type,colSuff,sep="_")] <- Tval
    }
  }
  return(df.out)
}
 
