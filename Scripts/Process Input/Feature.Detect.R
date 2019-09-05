Feature.Detect <- function(Data,               # must contain col.in and cols.out
                           df.patterns,        # the main string to be matched
                           type,               # pattern type to choose prefix and suffix for patterns, NONE, WORD, or COMPOUND
                           col.in,             # field to be searched for pattern
                           cols.out,           # fields to put identified features
                           perl       = FALSE, # use perl in grepl
                           useBytes   = FALSE, #
                           n.dup.cols = 1,     # when multiple matches are expected, increase value
                           DELETE  = TRUE,     # whether to delete identified string
                           VERBOSE = FALSE){
  # print(paste("***incoming to Feature.Detect:",Data[,col.in]))
  # print(paste("cols out are", paste(cols.out, collapse = ", ")))
  # print(paste("pattern colnames are", paste(colnames(df.patterns), collapse = ", ")))
  ls.pattern.types <- list(NONE = c("","",""),
                           WORD = c("\\<","\\>",""),
                           COMPOUND = c("\\<","\\>","[[:punct:]]?[[:space:]]+\\<","\\>"))
  
  patterns   <- paste0(ls.pattern.types[[type]][1],
                       df.patterns[,"PATTERN1"],
                       ls.pattern.types[[type]][2],
                       df.patterns[,"PATTERN2"],
                       ls.pattern.types[[type]][3])

  key.index      <- sapply(patterns,grepl,Data[,col.in], perl = perl, useBytes = useBytes)
  dim(key.index) <- c(nrow(Data), length(patterns)) # force into matrix form if necessary
  match.keys     <- which(apply(key.index, MARGIN = 1, any))
  
  # loop over all rows in Data that have a match
  for(i in match.keys){
    # print("has match")
    for(col in colnames(df.patterns)[!grepl("PATTERN",colnames(df.patterns)) & !grepl("REGEX",colnames(df.patterns))]){
      cols <- cols.out[grepl(col,cols.out) & cols.out!=col.in] # in case of more than one column available
      if(length(cols) > 0){
        # print(paste("cols are:",paste(cols, collapse = ", ")))
        n.empty.cols <- sum(is.na(Data[i,cols])) # assumes filled in from left
        if(n.empty.cols==0 & !DELETE){
          stop("Insufficient empty fields.")
        }
        # print(paste("empty cols:", n.empty.cols))
        if(!df.patterns[1,"REGEX"]){
          matches <- tolower(df.patterns[key.index[i,],col])
        }
        else{
          matches <- sapply(df.patterns[key.index[i,],col], 
                            function(pat) 
                              regmatches(Data[i,col.in],
                                         regexpr(pat,Data[i,col.in],
                                                 ignore.case = TRUE,
                                                 perl = perl,
                                                 useBytes = useBytes)))
        }
        # print(paste("     matches:",paste(matches,collapse = ", ")))
        # print(length(matches))
        if(length(matches) > n.empty.cols){
          # print("excess matches")
          if(matches[1]==matches[2]) matches <- matches[1] # assumes all are duplicated
          else if(grepl(matches[2],matches[1]) | grepl(matches[1],matches[2])) matches <- matches[which.max(nchar(matches))] # assumes longest substring
          else if(length(matches) > n.empty.cols) stop("not enough fields provided to contain all matches")
        }
        else{ # don't put in extraneous information, regardless
          # print("non-excess matches")
          if(length(matches) == n.empty.cols & n.empty.cols > 1){
            if(matches[1]==matches[2]) matches <- matches[1] # assumes all are duplicated
            else if(grepl(matches[2],matches[1]) | grepl(matches[1],matches[2])) matches <- matches[which.max(nchar(matches))] # assumes longest substring
          }
        }
        matches <- c(matches, rep(NA_character_, n.empty.cols-length(matches)))
        if(n.empty.cols < n.dup.cols) matches <- c(Data[i,cols][!is.na(Data[i,cols]) & Data[i,cols]!=""], matches)
        # print(paste("     matches consolidation:",paste(matches,collapse = "-")))
        Data[i,cols] <- c(matches)
        # print(paste(Data[i,cols], collapse = "-"))
      }
      
      # delete from string
      if(DELETE){
        if(length(patterns[key.index[i,]])==1){ p <- df.patterns[key.index[i,],col]
        }
        else{
          p <- as.character(df.patterns[key.index[i,],col])
          if(grepl(p[1],p[2],fixed = TRUE)|grepl(p[2],p[1], fixed = TRUE)){
            p <- p[which.max(nchar(p))]
          }
          else{
            p <- paste0("(",paste0(p, collapse=")|("),")")
            }
        }
        Data[i,col.in] <- str_squish(gsub(p,"",Data[i,col.in],
                                          ignore.case = TRUE,
                                          perl = perl,
                                          useBytes = useBytes))
        # print(paste("     after deletion:",Data[i, col.in]))
      }
    }
    }
  if(col.in!=cols.out[1]){
    Data <- Data[,c(col.in,cols.out)]
  }
  else Data <- Data[,col.in]
  return(Data)
}