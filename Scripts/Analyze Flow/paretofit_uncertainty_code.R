dir = "/Users/Indian_monsoon_research/uttarakhand/Drafts/Reviewer_Edits/Uncertainty_Quantification/"
setwd(dir)

library(ncdf4)
library(nortest)
library(np)
library(fields)
library(forecast)
library(exactRankTests)
library(ggplot2)
library(plyr)
library(perm)
library(DAAG)
library(gregmisc)
library(evd)
library(vcd)
library(fitdistrplus)
library(VGAM)
library(MASS)

#### Adapated from MATZ's code ###############

# Import data #
# 3 vectors are:
# 1. observational: total_baseline
# 2. preindustrial: get_pi_pr_mod
# 3. historical: get_hist_pr_mod
load("DeeptisData.RData") 
preindustrial = get_pi_pr_mod
historical = get_hist_pr_mod
Z.observed = total_baseline
#Select a model
model = 1
Z.historical = c(get_hist_pr_mod[,,model])
Z.preindustrial = c(get_pi_pr_mod[,,model])
Z.historical = Z.historical[!is.na(Z.historical)]
Z.preindustrial = Z.preindustrial[!is.na(Z.preindustrial)]

#### Functions ####
loglikParetoiii = function(params, obs) {
	-sum(log(dparetoIII(obs,location=params[1],scale=params[2],inequality=params[3])))
}

plotZs = function(obs, params, ...) {
	hist(obs,breaks=10,col="lightgrey",prob=TRUE, ...)
	xmin=range(obs)[1];	xmax=range(obs)[2]
	u = seq(xmin,xmax,length=100)
	lines(u,dparetoIII(u, location =params[1], scale = 	params[2],inequality=params[3]),col="green",lwd=2)
}
fitPareto = function(z, lower,upper) {
	n=10	
	locv = seq(lower[1],upper[1],length=n)
	locu = seq(lower[2],upper[2],length=n)
	mins = matrix(0, n,n)
	for (i in 1:n) {
		for (j in 1:n) {
			mins[i,j] = optim(c(locv[i],locu[j],fit$est[3]),loglikParetoiii,obs=z,method="L-BFGS-B", lower=lower,upper=upper)$value
		}
	}
	optind = which(mins == min(mins), arr.ind=T)
	out = optim(c(locv[optind[1]],locu[optind[2]],fit$est[3]),
		loglikParetoiii,obs=z,method="L-BFGS-B", 		
		lower=lower,upper=upper, hessian=T)
}
################### observed ranges -  these are good good bounds
#fit = fitdistr(Z.observed,dparetoIII,start=list(location=11,scale=70,inequality=0.35))
fit = fitdistr(Z.observed,dparetoIII,start=list(location=-8,scale=90,inequality=0.25))
out = fitPareto(Z.observed,c(-100,50,0.01),c(15,200,0.5))

################### historical - these are good good bounds
fit = fitdistr(Z.historical,dparetoIII,start=list(location=-30,scale=130,inequality=0.2))
out = fitPareto(Z.historical,c(-100,50,0.01),c(0,200,0.5))
###################
################### prehistorical these are good good bounds
fit = fitdistr(Z.preindustrial,dparetoIII,start=list(location=-30,scale=130,inequality=0.2))
out = fitPareto(Z.preindustrial,c(-100,50,0.01),c(0,200,0.5))
#-15.480235 102.541364   0.229517 
###################
comp.hist(Z.preindustrial, Z.historical)


########### ########### ########### ########### 
#### get Information matrix and return period with confidence intervals #####
########### ########### ########### ########### ########### 
B = 1000
hessian = array(0,dim = c(B,3,3))
paramStar = matrix(0,B,3)
lower = c(-100,50,0.01)
upper = c(15,200,0.5)
nobs = length(Z.observed)
par1 = runif(B,lower[1],upper[1]);par2 = runif(B,lower[2],upper[2]);par3 = runif(B,lower[3],upper[3])
returnPeriod = rep(0,B)
max.dat.ncep = total_2013
par(ask=F)

for (i in 1:B) {
	obsStar = Z.observed[resample(1:nobs,nobs,replace=T)]
	out = optim(c(par1[i],par2[i],par3[i]),loglikParetoiii, obs=obsStar,
	 method="L-BFGS-B", lower=c(5000,50,0.01),upper=c(5500,250,0.30), hessian=T)
	returnPeriod[i] = signif(1/(1 - pparetoIII(max.dat.ncep,out$par[1],out$par[2],out$par[3])), 3)
	hessian[i,,] = out$hess #Hessian
	paramStar[i,] = signif(out$par,4)
	print(paste(i,"Return Period",returnPeriod[i]," Parameters:", paramStar[i,1], paramStar[i,2], paramStar[i,3], signif(out$val,3)))
	#plotZs(obsStar, paramStar[i,], xlim=range(Z.observed),
	#main=paste("Return Period=",returnPeriod[i]))
}
# max(dat.ncep) is the observed data point from 2013 that we want to compute a return period for.
#Return period = 1/(Tail probability of an event) >
# input whatever loc, scal, ineq params from a fit
EReturnPeriod  = median(returnPeriod) #median return period
sdReturnPeriod = sd(returnPeriod)
Ehessian       = apply(hessian,c(2,3),mean)
Epar           = colMeans(paramStar)
covPar         = cov(paramStar) #We note a large magnitude in the off-diagonal of entry 12
confidenceInterval = c(quantile(returnPeriod,0.975), quantile(returnPeriod,0.025)) #95th confidence interval
########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 
########### ########### ########### ########### ########### 


########### ########### ########### ########### ########### ########### ########### ########### 
######### Get return ratio between historical and preinducstrial using 
######### the observed as a benchmark 
output = function(Z.observational, Z.historical, Z.preindustrial, B=100) {
	
	###### 
	
	lower = c(-100,50,0.01) #bounds
	upper = c(15,200,0.5)
	lowerObs = c(-80,140,0.01)
	upperObs = c(-50,180,0.5)
	upper2 = c(min(c(Z.historical, Z.preindustrial))-0.5,200,0.5)
	nobs = length(Z.observed)
	nhist = length(Z.historical)
	npre = length(Z.preindustrial)
	par1 = runif(B,lowerObs[1],upperObs[1]);
	par2 = runif(B,lowerObs[2],upperObs[2]);
	par3 = runif(B,lowerObs[3],upperObs[3])
	
	parhist1 = runif(B,lower[1],upper2[1]);
	parhist2 = runif(B,lower[2],upper[2]);
	parhist3 = runif(B,lower[3],upper[3])
	
	parpre1 = runif(B,lower[1],upper2[1]);
	parpre2 = runif(B,lower[2],upper[2]);
	parpre3 = runif(B,runif(B,lower[3],upper[3]))
	paramsStar = matrix(0,B,9)
	
	returnPeriodObs = rep(0,B)
	histAtPobs 		= rep(0,B)
	returnPeriodPre = rep(0,B)
	ratioPreOverObs = rep(0,B)
	PpreAtHistAtPobs= rep(0,B)
	max.dat.ncep = total_2013
	plotit = F
	for (i in 1:B) {
		print(i)
		if (i==1) { #Point estimate
			obsStar = Z.observed
			histStar = Z.historical
			preStar = Z.preindustrial
		} else { # Bootstrap
			obsStar = Z.observed[resample(1:nobs,nobs,replace=T)]
			histStar = Z.historical[resample(1:nhist,nhist,replace=T)]
			preStar = Z.preindustrial[resample(1:npre,npre,replace=T)]
		}
		
		out = optim(c(par1[i],par2[i],par3[i]),loglikParetoiii, obs=obsStar,
		 method="L-BFGS-B", lower=lowerObs,upper=upperObs, hessian=F)
		 
		returnPeriodObs[i] = signif(1/(1 - pparetoIII(max.dat.ncep,out$par[1],out$par[2],out$par[3])), 3)
		pObs = pparetoIII(max.dat.ncep,out$par[1],out$par[2],out$par[3])
		outHist = optim(c(parhist1[i],parhist2[i],parhist3[i]),loglikParetoiii, obs=histStar,
		 method="L-BFGS-B", lower=lower,upper=upper2, hessian=F)
		# print(outHist$par)
		histAtPobs[i] = qparetoIII(pObs, outHist$par[1],outHist$par[2],outHist$par[3])
		# fit preindustrial
		outPre = optim(c(parpre1[i],parpre2[i],parpre3[i]),loglikParetoiii, obs=preStar,
		 method="L-BFGS-B", lower=lower,upper=upper2, hessian=F)
		 #print(outPre$par)
		#	plotZs(preStar, outPre$par, xlim=range(Z.preindustrial))
		# Get return period for preindustrial
		PpreAtHistAtPobs[i] = pparetoIII(histAtPobs[i],outPre$par[1],outPre$par[2],outPre$par[3])
		returnPeriodPre[i] = signif(1/(1 - PpreAtHistAtPobs[i]), 3)
		ratioPreOverObs[i] = returnPeriodPre[i]/returnPeriodObs[i]
		paramsStar[i,] = c(out$par, outHist$par, outPre$par)
		par(mfrow=c(1,3))
		if (plotit) {
			par(ask=T)
			plotZs(obsStar, out$par, xlim=range(Z.observed))
			plotZs(histStar, outHist$par, xlim=range(Z.historical))
			plotZs(preStar, outPre$par, xlim=range(Z.preindustrial))
			print(outPre$par)
			print(outPre$value)
		} else {
			par(ask=F)
		}
	}
	######
	ratioPreOverObs
}

B = 5000
nmodels = 11
ratios = matrix(0, B, nmodels)
for (i in 1:nmodels) {
	model = i
	Z.historical = c(get_hist_pr_mod[,,model])
	Z.preindustrial = c(get_pi_pr_mod[,,model])
	Z.historical = Z.historical[!is.na(Z.historical)]
	Z.preindustrial = Z.preindustrial[!is.na(Z.preindustrial)]
	
	ratios[,i] = output(Z.observational, Z.historical, Z.preindustrial, B)
}

save(ratios, file="ratios5000.Rdat")

###### TEST CODE ########
lower = c(-100,50,0.01) #bounds
upper = c(15,200,0.5)
lowerObs = c(-80,140,0.01)
upperObs = c(-50,180,0.5)
upper2 = c(min(c(Z.historical, Z.preindustrial))-0.5,200,0.5)
B = 5000
nobs = length(Z.observed)
nhist = length(Z.historical)
npre = length(Z.preindustrial)
par1 = runif(B,lowerObs[1],upperObs[1]);
par2 = runif(B,lowerObs[2],upperObs[2]);
par3 = runif(B,lowerObs[3],upperObs[3])

parhist1 = runif(B,lower[1],upper2[1]);
parhist2 = runif(B,lower[2],upper[2]);
parhist3 = runif(B,lower[3],upper[3])

parpre1 = runif(B,lower[1],upper2[1]);
parpre2 = runif(B,lower[2],upper[2]);
parpre3 = runif(B,runif(B,lower[3],upper[3]))
paramsStar = matrix(0,B,9)

returnPeriodObs = rep(0,B)
histAtPobs 		= rep(0,B)
returnPeriodPre = rep(0,B)
ratioPreOverObs = rep(0,B)
PpreAtHistAtPobs= rep(0,B)
max.dat.ncep = total_2013
plotit = F
for (i in 1:B) {
	print(i)
	if (i==1) { #Point estimate
		obsStar = Z.observed
		histStar = Z.historical
		preStar = Z.preindustrial
	} else { # Bootstrap
		obsStar = Z.observed[resample(1:nobs,nobs,replace=T)]
		histStar = Z.historical[resample(1:nhist,nhist,replace=T)]
		preStar = Z.preindustrial[resample(1:npre,npre,replace=T)]
	}
	
	out = optim(c(par1[i],par2[i],par3[i]),loglikParetoiii, obs=obsStar,
	 method="L-BFGS-B", lower=lowerObs,upper=upperObs, hessian=F)
	 
	returnPeriodObs[i] = signif(1/(1 - pparetoIII(max.dat.ncep,out$par[1],out$par[2],out$par[3])), 3)
	pObs = pparetoIII(max.dat.ncep,out$par[1],out$par[2],out$par[3])
	outHist = optim(c(parhist1[i],parhist2[i],parhist3[i]),loglikParetoiii, obs=histStar,
	 method="L-BFGS-B", lower=lower,upper=upper2, hessian=F)
	# print(outHist$par)
	histAtPobs[i] = qparetoIII(pObs, outHist$par[1],outHist$par[2],outHist$par[3])
	# fit preindustrial
	outPre = optim(c(parpre1[i],parpre2[i],parpre3[i]),loglikParetoiii, obs=preStar,
	 method="L-BFGS-B", lower=lower,upper=upper2, hessian=F)
	 #print(outPre$par)
	#	plotZs(preStar, outPre$par, xlim=range(Z.preindustrial))
	# Get return period for preindustrial
	PpreAtHistAtPobs[i] = pparetoIII(histAtPobs[i],outPre$par[1],outPre$par[2],outPre$par[3])
	returnPeriodPre[i] = signif(1/(1 - PpreAtHistAtPobs[i]), 3)
	ratioPreOverObs[i] = returnPeriodPre[i]/returnPeriodObs[i]
	paramsStar[i,] = c(out$par, outHist$par, outPre$par)
	par(mfrow=c(1,3))
	if (plotit) {
		par(ask=T)
		plotZs(obsStar, out$par, xlim=range(Z.observed))
		plotZs(histStar, outHist$par, xlim=range(Z.historical))
		plotZs(preStar, outPre$par, xlim=range(Z.preindustrial))
		print(outPre$par)
		print(outPre$value)
	} else {
		par(ask=F)
	}
}

###########
#Plotting
par(mfrow=c(2,3))

col.pal	= rainbow(n_models)

names     = c( "ACCESS1-0",    "ACCESS1-3",    "CNRM-CM5",     "HadGEM2-AO",   "HadGEM2-CC" ,  "HadGEM2-ES" , "IPSL-CM5B-LR", "MIROC4h",      "MPI-ESM-MR" ,  "MPI-ESM-P"   , "bcc-csm1-1")
ratioPreOverObs = ratios[,1]
plot(ecdf(ratioPreOverObs),xlim=c(0,8), xlab="likelihood ratio", ylab = "confidence",col=col.pal[1])
n_models	= length(names)
for (i in 2:n_models) {
	ratioPreOverObs = ratios[,i]	
	lines(ecdf(ratioPreOverObs),xlim=c(0,8),col=col.pal[i])
	
}

legend("bottomright",names,text.col=col.pal)
abline(h=0.5,lty=2)
abline(v=1,lty=2)

x	= matrix(0, nrow=5000,ncol=n_models)

for (i in 1:n_models){
	ratioPreOverObs = ecdf(ratios[,i])
	x[,i]			= ratios[,i]
}

y=boxplot(x, ylim =c(0,6),col= col.pal, labels=names, ylab="likelihood ratio", xaxt="n", horizontal=T)
text(x=seq_along(names),y=par("usr")[3]-0.2,srt=90, adj=1,labels=names,xpd=T,cex=0.8)
abline(v=1,lty=2)


hist(ratioPreOverObs[ratioPreOverObs<3], main="")
abline(v=ratioPreOverObs[1])
abline(v=median(ratioPreOverObs),col=2)
legend("topright", c("Point Estimate", "Median"),col=c(1,2), lty=1)
confidenceIntervalRatio = c(quantile(ratioPreOverObs,0.025), quantile(ratioPreOverObs,0.975))#95th confidence interval
title(paste("conf int:",signif(confidenceIntervalRatio[1],3),signif(confidenceIntervalRatio[2],3)))
plot(ecdf(ratioPreOverObs),xlim=c(0,2))
##############
par(mfrow=c(3,3)); for (i in 1:9) {hist(paramsStar[,i])}
