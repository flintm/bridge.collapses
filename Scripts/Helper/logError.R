# compute the relative error associated with exponentially distributed variables
# i.e., if x.obs ranges from 1 to 10,000, we want an error measure that gives 
# a reasonable interpretation of the difference between x.obs and x.pred across
# the entire range
logError <- function(x.obs,x.pred){
  if (length(x.obs)!=length(x.pred)) stop("observed and predicted values must have the same length")
  err <- log10(x.pred)-log10(x.obs)
  return(err)
}