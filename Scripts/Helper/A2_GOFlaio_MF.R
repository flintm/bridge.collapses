A2_GOFlaio_MF <- function (x, dist = "NORM"){
  if (any(c("LN", "EV2", "LP3") == dist)) 
    x <- log(x)
  b <- sort(x)
  n <- length(x)
  eta0 = 0.851
  beta0 = 0.116
  csi0 = 0.0403
  eps1 = 1.2
  eps2 = 0.2
  if ((dist == "NORM") || (dist == "LN")) {
    T <- ML_estimation(b, dist = "NORM")
    F <- .Fx(b, T, dist = "NORM")
    eta1 = 1.147
    beta1 = 0.229
    csi1 = 0.167
    eta1corr <- eta1 * (1 + 0.5/n)
    beta1corr <- beta1 * (1 - 0.2/n)
    csi1corr <- csi1 * (1 + 0.3/n)
  }
  else if ((dist == "EV1") || (dist == "GUMBEL") || (dist == 
                                                     "EV2")) {
    T <- ML_estimation(b, dist = "EV1")
    F <- .Fx(b, T, dist = "EV1")
    eta1 = 1.141
    beta1 = 0.229
    csi1 = 0.169
    eta1corr <- eta1 * (1 + 0.5/n)
    beta1corr <- beta1 * (1 - 0.2/n)
    csi1corr <- csi1 * (1 + 0.1/n)
  }
  else if (dist == "GEV") {
    T <- ML_estimation(b, dist = "GEV")
    F <- .Fx(b, T, dist = "GEV")
    if (T[3] > 0.5) 
      T[3] = 0.5
    eta1 <- 1.186 * (1 - 0.04 * T[3] - 0.04 * T[3]^2 - 0.01 * 
                       T[3]^3)
    beta1 <- 0.189 * (1 + 0.2 * T[3] + 0.37 * T[3]^2 + 0.17 * 
                        T[3]^3)
    csi1 <- 0.147 * (1 + 0.13 * T[3] + 0.21 * T[3]^2 + 0.09 * 
                       T[3]^3)
    eta1corr <- eta1 * (1 - 0.7/n + 0.2/sqrt(n))
    beta1corr <- beta1 * (1 - 1.8/n)
    csi1corr <- csi1 * (1 + 0.9/n - 0.2/sqrt(n))
  }
  else if ((dist == "GAM") || (dist == "P3") || (dist == "LP3")) {
    T <- ML_estimation(b, dist = "GAM")
    F <- .Fx(b, T, dist = "GAM")
    F[F > 0.99999999] = 0.99999999
    F[F < 1e-08] = 1e-08
    if (T[3] < 2) 
      T[3] = 2
    eta1 <- 1.194 * (1 - 0.04 * T[3]^(-1) - 0.12 * T[3]^(-2))
    beta1 <- 0.186 * (1 + 0.34 * T[3]^(-1) + 0.3 * T[3]^(-2))
    csi1 <- 0.145 * (1 + 0.17 * T[3]^(-1) + 0.33 * T[3]^(-2))
    eta1corr <- eta1 * (1 - 1.8/n + 0.1/sqrt(n) + 0.5/(sqrt(n) * 
                                                         T[3]))
    beta1corr <- beta1 * (1 - 0.5/n - 0.3/sqrt(n) + 0.3/(sqrt(n) * 
                                                           T[3]))
    csi1corr <- csi1 * (1 + 2/n - 0.3/sqrt(n) - 0.4/(sqrt(n) * 
                                                       T[3]))
  }
 # else stop("A2_GOFlaio(x,T,dist,case): distribution unknown")
  # print(F)
  if (any(F<0 | F > 1 | is.na(F))){
    A <- NA
    pA <- NA
  }
  else{
    A <- A2(F)
    if (A <= eps1 * csi1corr) {
      w <- max((csi0 + beta0 * ((eps1 - 1) * csi1corr/beta1corr)^(eta1corr/eta0))/((eps1 - 
                                                                                      eps2) * csi1corr) * (A - eps2 * csi1corr), 1e-05)
      #     print("w from A <= eps1 * csi1corr is ")
      #     print(w)
    }
    else {
      w <- csi0 + beta0 * ((A - csi1corr)/beta1corr)^(eta1corr/eta0)
      #     print("w from A > eps1 * csi1corr is ")
      #     print(w)
    }
    pA <- ifelse(!is.na(w), fw2(w), NA)
  }

  output <- c(A, pA)
  names(output) <- c("A2", "p(A2)")
  return(output)
}
