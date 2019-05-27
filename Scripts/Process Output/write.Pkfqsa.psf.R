MakePeakFQspecs <- function(dat.file, 
                            STANAME,
                            LOTHRESH = 0,
                            SKEWOPT  = "WEIGHTED",
                            GENSKEW = NA,
                            SKEWSD,
                            BEGYEAR,
                            ENDYEAR,
                            GAGEBASE = 0,
                            CSV.FLAG = FALSE,
                            PCPT = NA,
                            file.out = "",
                            filePath = getwd()){

  if (is.na(GENSKEW)){
  out <- c("Verbose",
           paste0("I  ",dat.file),
           paste0("STATION  ", STANAME,", ",BEGYEAR,"-",ENDYEAR),
           paste0("LOTHRESH         ",LOTHRESH), # NEEDS TO BE XXX.0?
           paste0("SKEWOPT          ",SKEWOPT),
           paste0("GENSKEW          ",GENSKEW),
           paste0("SKEWSD           ",SKEWSD),
           paste0("BEGYEAR          ",BEGYEAR),
           paste0("ENDYEAR          ",ENDYEAR),
           paste0("GAGEBASE         ",GAGEBASE),
           paste0("CSV"),
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
