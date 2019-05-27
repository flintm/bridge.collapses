FailureReliabilityAnalysis <- function()
{
require(Hmisc)
load(file.path(dirsGit$Data,"df.Fail.NBI.Gage.Active.RData"))

# analyze return period of failure events using mean and median data
TfMeanD <- mean(df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_HECD_USGS"])
TfMeanIP <- mean(df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"],na.rm=TRUE)
TfMedianD <- median(df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_HECD_USGS"])
TfMedianIP <- median(df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"],na.rm=TRUE)

# Final annual probability of failure with all mass at median
pMeanD   <- 1/TfMeanD
pMeanIP  <- 1/TfMeanIP
pMedianD   <- 1/TfMedianD
pMedianIP  <- 1/TfMedianIP

# lognormal distribution for return period of failure event
logTfD  <- log(df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_HECD_USGS"])
logTfIP <- df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"]
logTfIP <- logTfIP[!is.na(logTfIP)]
meanlogTfD <- mean(logTfD)
sdlogTfD   <- sd(logTfD)
meanlogTfIP <- mean(logTfIP)
sdlogTfIP   <- sd(logTfIP)

# to get probability of failure 
# convolution integral of probability that it fails given the return period is "t"
# times the probability of seeing an event with probability "t"
dt <- 1
t <- seq(dt,12000,by=dt)

pFlogD <- 0
pFlogIP <- 0
for (i in 1:length(t)){
  dP <- dnorm(log(t[i]),meanlogTfD,sdlogTfD)*1/t[i]^2
  pFlogD <- pFlogD + dP
  dP <- dnorm(log(t[i]),meanlogTfIP,sdlogTfIP)*1/t[i]^2
  pFlogIP <- pFlogIP + dP
}

# using kernel density for T and convolution approach
pTkernelD  <- density(df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_HECD_USGS"], bw = "nrd", adjust = 0.5, from=0, to=2000)
pTkernelIP <- density(df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"][!is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"])], bw = "nrd",adjust = 0.5, from=0, to=11000)
pTkernelDinterp <- approxExtrap(pTkernelD$x, pTkernelD$y, t)
pTkernelIPinterp <- approxExtrap(pTkernelIP$x, pTkernelIP$y, t)
pFkernelD  <- 0
pFkernelIP <- 0
for (i in 1:length(t)){
  dP <- pTkernelDinterp$y[i]*1/t[i]^2
  pFkernelD <- pFkernelD + dP
  dP <- pTkernelIPinterp$y[i]*1/t[i]^2
  pFkernelIP <- pFkernelIP + dP
}

# considering potential impact of climate change
pFkernelD10p  <- 0
pFkernelIP10p <- 0
for (i in 1:length(t)){
  dP <- pTkernelDinterp$y[i]*1/(t[i]*.9)^2
  pFkernelD10p <- pFkernelD10p + dP
  dP <- pTkernelIPinterp$y[i]*1/(t[i]*.9)^2
  pFkernelIP10p <- pFkernelIP10p + dP
}

pFkernelD10l  <- 0
pFkernelIP10l <- 0
for (i in 1:length(t)){
  dP <- pTkernelDinterp$y[i]*1/(t[i]*1.1)^2
  pFkernelD10l <- pFkernelD10l + dP
  dP <- pTkernelIPinterp$y[i]*1/(t[i]*1.1)^2
  pFkernelIP10l <- pFkernelIP10l + dP
}

pMeanD10p <- 1/(TfMeanD*.9)
pMeanIP10p <- 1/(TfMeanIP*.9)
pMeanD10l <- 1/(TfMeanD*1.1)
pMeanIP10l <- 1/(TfMeanIP*1.1)
pMedianD10p <- 1/(TfMedianD*.9)
pMedianIP10p <- 1/(TfMedianIP*.9)
pMedianD10l <- 1/(TfMedianD*1.1)
pMedianIP10l <- 1/(TfMedianIP*1.1)

pFkernelD20p  <- 0
pFkernelIP20p <- 0
for (i in 1:length(t)){
  dP <- pTkernelDinterp$y[i]*1/(t[i]*.8)^2
  pFkernelD20p <- pFkernelD20p + dP
  dP <- pTkernelIPinterp$y[i]*1/(t[i]*.8)^2
  pFkernelIP20p <- pFkernelIP20p + dP
}

# theoretical reliability
pF35 <- pnorm(-3.5)
pF50y175 <- (1/50)*pnorm(-1.75)
pF100y175 <- (1/100)*pnorm(-1.75)
pF50y175.10p <- (1/50/0.9)*pnorm(-1.75)
pF100y175.10p <- (1/100/0.9)*pnorm(-1.75)
pF50y175.10l <- (1/50/1.1)*pnorm(-1.75)
pF100y175.10l <- (1/100/1.1)*pnorm(-1.75)

# organize and save data
pTdata <- list(d   = c(mean = TfMeanD, median = TfMedianD),
               ip  = c(mean = TfMeanIP, median = TfMedianIP))
pFannualEstimates <- list(pFD  = c(mean = pMeanD, median = pMedianD, logn = pFlogD, kernel = pFkernelD),
                          pFIP = c(mean = pMeanIP, median = pMedianIP, logn = pFlogIP, kernel = pFkernelIP),
                          pFrel = c("3pt5" = pF35, "50y1pt75" = pF50y175, "100yr1pt75" = pF100y175),
                          pFD10p = c(mean = pMeanD10p, median = pMedianD10p, kernel = pFkernelD10p),
                          pFIP10p = c(mean = pMeanIP10p, median = pMedianIP10p, kernel = pFkernelIP10p),
                          pFrel10p = c("3pt5" = NA, "50y1pt75" = pF50y175.10p, "100yr1pt75" = pF100y175.10p),
                          pFD10l = c(mean = pMeanD10l, median = pMedianD10l, kernel = pFkernelD10l),
                          pFIP10l = c(mean = pMeanIP10l, median = pMedianIP10l, kernel = pFkernelIP10l),
                          pFrel10l = c("3pt5" = NA, "50y1pt75" = pF50y175.10l, "100yr1pt75" = pF100y175.10l))
save(pFannualEstimates, pTdata, file = file.path(dirsGit$Data, "FailureReliabilityComparison.RData"))

}