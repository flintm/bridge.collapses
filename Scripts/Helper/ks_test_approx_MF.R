# Kolmogorov-Smirnov test critical values for large samples
# Used Benjamin and Cornell for alpha = 0.01,0.05,0.1, which agreed with alternate source for 0.15, 0.2
ks_test_approx_MF <- function(Dn, n, alpha, twoSample=FALSE, n2 = NA, VERBOSE=FALSE){
 if (twoSample == FALSE){
  if (n<=35 & !(n %in% c(30,11,22,17,35,34,10,18))){
    warning("Sample sizes<=35 are not currently supported except n={11,17,22,30,34,35")
    return(NA)
  }
  else{
    if(!any(alpha==c(0.01,0.05,0.1))){
      warning("Alpha value must be in {0.01,0.05,0.1,0.15,0.2}")
      return(NA)
    }
    else{
      if (n>35){
        DnCrit <- switch(as.character(alpha),
                         "0.01" = 1.628/sqrt(n),
                         "0.05" = 1.358/sqrt(n),
                         "0.1"  = 1.224/sqrt(n),
                         "0.15" = 1.14/sqrt(n),
                         "0.2"  = 1.07/sqrt(n)
        )
      }
      else{
        if (!(alpha %in% c(0.01,0.05,0.1))){
          warning("With n<35, only alpha of 0.01, 0.05, 0.1 supported")
          return(NA)
        }
        DnCrits <- data.frame(n         = c(10,    11,   17,     18,    22,    30,    34,    35),
                              alphaPt01 = c(0.490, 0.468, 0.381, 0.371, 0.337, 0.290, 0.273, 0.269),
                              alphaPt05 = c(0.410, 0.391, 0.318, 0.309, 0.281, 0.242, 0.227, 0.224),
                              alphaPt1  = c(0.368, 0.352, 0.286, 0.278, 0.253, 0.218, 0.205, 0.202)
                              )
        DnCrit  <- switch(as.character(alpha),
                          "0.01" = DnCrits[DnCrits$n ==  n,"alphaPt01"],
                          "0.05" = DnCrits[DnCrits$n ==  n,"alphaPt05"],
                          "0.1"  = DnCrits[DnCrits$n ==  n,"alphaPt1"]
                          )
      }
      if (Dn <= DnCrit){
        if (VERBOSE == TRUE) print(paste("Null hypothesis accepted: sample consistent with distribution with alpha =", as.character(alpha)))
        return(TRUE)
      }
      else{
        if (VERBOSE == TRUE) print(paste("Null hypothesis rejected: sample is NOT consistent with distribution with alpha =", as.character(alpha)))
        return(FALSE)
      }
      }
    }
 }
 else{ # two-sample test
   if (!any(alpha==c(0.01,0.05,0.1))){
     warning("Alpha value for two-sample test must be in {0.01,0.05, 0.1}")
     return(NA)
   }
   if (is.na(n2)){
     warning("n2 must be provided for two-sample test")
     return(NA)
   }
   else{
      if (n==10 & n2==10 & alpha!=0.1){
        DnCrit <- switch(as.character(alpha),
                         "0.01" = 0.8,
                         "0.05" = 07)
      }
     else{
       if (n>12 | n2>12){
         c_alpha <- data.frame(alpha = c(0.01, 0.05, 0.10),
                               c     = c(1.63, 1.36, 1.22))
         DnCrit <- c_alpha[c_alpha$alpha == alpha, "c"]*sqrt((n+n2)/(n*n2))
       }
       else{
         warning("For two-sample test, n1=n2=10 or n1 or n2 > 12")
         return(NA)
       }
     }
   }
   print(paste("n1 =",n))
   print(paste("n2 =",n2))
   print(paste("alpha =",alpha,"Dn=",Dn,"DnCrit = ",DnCrit))
   if (Dn <= DnCrit){
     if (VERBOSE == TRUE) print(paste("Null hypothesis accepted: samples consistent with alpha =", as.character(alpha)))
     return(TRUE)
   }
   else{
     if (VERBOSE == TRUE) print(paste("Null hypothesis rejected: samples are NOT consistent with alpha =", as.character(alpha)))
     return(FALSE)
   }
 }
}
