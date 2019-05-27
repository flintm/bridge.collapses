# Function to load flood frequency analysis results from output file
# Written by Madeleine Flint, 2018-06-27
# Supports HEC-SSP v2? Bulletin 17B analysis, PeakFQ v4.2 B17B, and PeakFQ v7.2 B17C
LoadFFA <- function(filename, TYPE = "PKFQ_17C",
                    colStandard = TRUE){ # standardize column output names

  ls.colNames <- list("PKFQ72_17C" = c("ANN_EXC_PROB",  "EMA_REG",	"EMA_STAT",	"LOG_VAR_REG",	"5PCT_CONF_REG",	"95PCT_CONF_REG"),
                      "HEC_17B"    = c("Ordinate",  "FREQ",	"FLOW Computed Curve",	"FLOW Expected Prob Curve",	"FLOW 5Pct Conf",	"FLOW 95Pct Conf",	
                                       "FLOW Observed (Median)",	"FLOW Historic Data",	"FLOW High Outlier",	"FLOW Low Outlier"),
                      "PKFQ42_17B" = c("ANN_EXC_PROB",  "BULL17B_EST",	"SYSTEM_RECORD",	"EXPECTED_PROB",	"5PCT_CONF",	"95PCT_CONF"),
                      "PARTDUR"    = c("Val","Exc"))
  colNames <- ls.colNames[[TYPE]]
  
  colsSwitch <- list(FlowCols   = c("EMA_REG", "FLOW Computed Curve", "BULL17B_EST","Val"),
                     FreqCols   = c("ANN_EXC_PROB","FREQ","Exc"),
                     Flow05Cols = c("5PCT_CONF_REG", "5PCT_CONF"),
                     Flow95Cols = c("95PCT_CONF_REG", "95PCT_CONF"))
  colsOut <- c(FlowCols   = "Flow",
               FreqCols   = "Freq",
               Flow05Cols = c("Flow05"),
               Flow95Cols = c("Flow95"))
  
  if(grepl("PeakFQ72_17C",TYPE)){ # 7.2 version, non-batch
    PFA <- read.fwf(file.path(folder.out,filename),
                    77,
                    header=FALSE,
                    sep = "\n",
                    stringsAsFactors = FALSE)
    skip <- grep("TABLE 4",PFA$V1)[1] + 5
    PFA <- read.fwf(file.path(folder.out,filename),
                    77,
                    header=FALSE,
                    sep = "\n",
                    n = 15,
                    skip=skip,
                    stringsAsFactors = FALSE)
    PFA <- as.data.frame(sapply(1:nrow(PFA_17C_VICRg[[i]]),
                                function(j) gsub("--","NA",PFA[j,])), stringsAsFactors = FALSE)
    PFA <- as.data.frame(t(sapply(1:nrow(PFA), function(j) as.numeric(unlist(strsplit(PFA[j,], "[[:space:]]+")))[2:7] )))
  }
  
  if(grepl("PeakFQ42_17B",TYPE)){ # older batch version
    skip <- 83
    PFA <- read.fwf(file.path(folder.out,filename),
                    77,
                    header=FALSE,
                    sep = "\n",
                    n = 15,
                    skip=skip,
                    stringsAsFactors = FALSE)
    PFA <- as.data.frame(t(sapply(1:nrow(PFA), function(j) as.numeric(unlist(strsplit(PFA[j,], "[[:space:]]+")))[2:7] )))
  }
  
  if(grepl("HEC_17B_P",TYPE)){ # HEC-SSP ANALYSIS USING PEAK DATA (TAB OUTPUT)
    skip <- 5
    PFA <- read.table(file.path(folder.out,filename),
                      header=FALSE,na.strings="NA",sep = "\t",
                      skip=skip, col.names = colNames,
                      stringsAsFactors = FALSE)
  }
  
  if(grepl("HEC_17B_D",TYPE)){ # HEC-SSP ANALYSIS USING DAILY MEAN DATA
    skip <- 3
    colClasses <- c()
    PFA    <- read.table(file.path(folder.out,filename),
                         header=FALSE,na.strings="NA",sep="\t",
                         skip=skip, col.names = colNames,
                         stringsAsFactors = FALSE)
    skip   <- skip + grep("TYPE",PFA[,1])[1] + 1
    PFA    <- read.table(file.path(folder.out,filename),
                         header=FALSE,na.strings="NA",sep="\t",
                         skip=skip, col.names = colNames,
                         colClasses = c("integer", rep("numeric",9)),
                         stringsAsFactors = FALSE)
  }
  if(grepl("PARTDUR",TYPE)){
    ExcVals <- c(0,0.05,0.10,0.25,0.5,0.75,0.90,0.95,1)
    PFA <- read.table(file.path(folder.out,filename),
                          sep = " ",
                          skip = 5,
                          stringsAsFactors = FALSE,
                          colClasses = c("character","numeric"),
                          col.names = colNames)
    PFA$ExcVal <- ExcVals
    nPart      <- read.table(file.path(folder.out,filename),
                             sep = " ",
                             skip = 2,
                             stringsAsFactors = FALSE,
                             nrows = 1,
                             colClasses = c("character","numeric"),
                             col.names = colNames)[2]
    PFA <- PFA[,2:3]
    attr(PFA,nPart = nPart)
  }
  
  # Now setup column names
  colnames(PFA) <- colNames
  colClasses    <- names(colsSwitch)
  if(colStandard){
    for(col in colClasses){
      colsIn <- unlist(colsSwitch[[col]])
      colnames(PFA)[sapply(colNames, function(c) any(grepl(c, colsIn)))] <- colsOut[col]
    }
    PFA <- PFA[,colnames(PFA) %in% colsOut]
  }
  return(PFA)
}