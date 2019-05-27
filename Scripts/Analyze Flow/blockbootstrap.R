blockbootstrap <- function(vec,b){
  # Computes one bootstrap realization using moving block bootstrap
  # Inputs data matrix mat and block length b
  # Bootstraps across time
  n <- length(vec)
  X.boot <- numeric(ceiling(n/b)*b)
  for(k in 1:ceiling(n/b)){
    i <- sample(n-b+1,1)
    index <- i:(i+b-1)
    X.boot[((k-1)*b + 1):(k*b)] <- vec[index]
  }
  # If b is not a multiple of n, truncate series at 
  # length n (length of original series)
  X.boot[1:n]
}
