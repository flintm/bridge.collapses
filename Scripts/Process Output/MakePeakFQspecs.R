MakePeakFQspecs <- function(STAID, pkfFile, BegYear, EndYear, skew = NA, filePath = getwd()){

  if (is.na(skew)){
  Out <- c("Verbose",
           paste("I ASCI",pkfFile),
           paste("O File ", sub(".pkf","",pkfFile, ignore.case = TRUE),".PRT", sep = ""),
           paste("O Export ", sub(".pkf","",pkfFile, ignore.case = TRUE),".exp", sep = ""),
           paste("O Empirical ", sub(".pkf","",pkfFile, ignore.case = TRUE),".emp", sep = ""),
           paste("Station", STAID),
           "Analyze B17B",
           paste("BegYear", BegYear),
           paste("EndYear", EndYear),
           "'NO Historic Period in use",
           "     SkewOpt Generalized")
  }
  else{
    Out <- c("Verbose",
             paste("I ASCI",pkfFile),
             paste("O File ", sub(".pkf","",pkfFile, ignore.case = TRUE),".PRT", sep = ""),
             paste("O Export ", sub(".pkf","",pkfFile, ignore.case = TRUE),".exp", sep = ""),
             paste("O Empirical ", sub(".pkf","",pkfFile, ignore.case = TRUE),".emp", sep = ""),
             paste("Station", STAID),
             "Analyze B17B",
             paste("BegYear", BegYear),
             paste("EndYear", EndYear),
             "'NO Historic Period in use",
             "     SkewOpt Generalized",
             paste("     GenSkew",skew))
  }
  
  file <-  file.path(filePath,paste(sub(".pkf","",pkfFile, ignore.case = TRUE),".PSF", sep = ""))
  
  write.table(Out, file = file, quote = FALSE, sep = "", col.names = FALSE, row.names = FALSE)#), eol = "\r\n")
}
