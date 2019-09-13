Feature.Detect <- function(Data,               # must contain col.in and cols.out
                           df.patterns,        # the main string to be matched
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
  
  patterns   <- df.patterns[,"PATTERN"]

  key.index      <- sapply(patterns,grepl,Data[,col.in], perl = perl, useBytes = useBytes)
  dim(key.index) <- c(nrow(Data), length(patterns)) # force into matrix form if necessary
  match.keys     <- which(apply(key.index, MARGIN = 1, any))
  
  # loop over all rows in Data that have a match
  for(i in match.keys){
    # print("has match")
    for(col in colnames(df.patterns)[!grepl("PATTERN",colnames(df.patterns)) & !grepl("REGEX",colnames(df.patterns))]){
      cols <- cols.out[grepl(col,cols.out) & cols.out!=col.in] # in case of more than one column available
      if(length(cols) > 0){
        n.empty.cols <- sum(is.na(Data[i,cols]))
        # print(paste("cols are:",paste(cols, collapse = ", ")))
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
        un <- unique(matches)
        # keep longest/fullest matches
        # e.g., will keep "north ambrose" over "ambrose" if both contained
        un <- un[!sapply(1:length(un), 
                         function(i) any(grepl(
                           paste0("([[:alpha:]] ",un[i],")|(",un[i]," [[:alpha:]])"),un[-i])))] 
        
        matches <- matches[matches %in% un]
        if(length(matches) > n.empty.cols){
          stop(paste0("Need ", length(matches) - n.empty.cols," additional fields to contain all matches."))
        }
        if(n.empty.cols < n.dup.cols) matches <- c(Data[i,cols][!is.na(Data[i,cols]) & Data[i,cols]!=""], matches)
        else matches <- c(matches, rep(NA_character_, n.empty.cols-length(matches)))

        Data[i,cols] <- matches
        # print(paste(Data[i,cols], collapse = "-"))
      }
      # delete from string
      if(DELETE){
        # print("In Deletion:")
        if(length(patterns[key.index[i,]])==1){ 
          # print("one pattern")
          p <- df.patterns[key.index[i,],col]
        }
        else{
          # print("multi-pattern")
          p <- as.character(df.patterns[key.index[i,],col]) # vectorize
          if(grepl(p[1],p[2],fixed = TRUE)|grepl(p[2],p[1], fixed = TRUE)){
            p <- p[which.max(nchar(p))]
          }
          else{
            p <- paste0("(",paste0(p, collapse=")|("),")")
          }
        }
        # print(Data[i,col.in])
        # print(p)
        Data[i,col.in] <- str_squish(gsub(p,"",Data[i,col.in],
                                          perl = perl,
                                          useBytes = useBytes))
        Data[i, col.in] <- sub("[,-]$","", Data[i, col.in])
        # print(paste("     after deletion:",Data[i, col.in]))
      }
    }
  }
  # Return correct set of fields
  return(Data[,unique(c(col.in,cols.out))])
}