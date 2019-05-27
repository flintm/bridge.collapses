Feature.Detect <- function(Data,             # must contain col.in and cols.out
                           df.patterns,      # the main string to be matched
                           type,             # pattern type to choose prefix and suffix for patterns, NONE, WORD, or COMPOUND
                           col.in,           # field to be searched for pattern
                           cols.out,         # fields to put identified features
                           n.dup.cols = 1,   # when multiple matches are expected, increase value
                           DELETE  = TRUE,   # whether to delete identified string
                           VERBOSE = FALSE){
  
  ls.pattern.types <- list(NONE = NA_character_,
                           WORD = c("\\<","\\>"),
                           COMPOUND = c("\\<","\\>","[[:punct:]]?[[:space:]]+\\<","\\>"))
  
  patterns <- paste0(ls.pattern.types[[type]][1],df.patterns$PATTERN,ls.pattern.types[[type]][2])
  key.index  <- sapply(patterns,grepl,Data[,col.in],ignore.case=TRUE)
  match.keys <- which(apply(key.index, MARGIN = 1, any))
  for(i in match.keys){
    for(col in colnames(df.patterns)[colnames(df.patterns)!="PATTERN"]){
      matches <- tolower(df.patterns[key.index[i,],col])
      matches <- c(matches, rep(NA_character_, n.dup.cols-length(matches)))
      Data[i,cols.out[grepl(col,cols.out)]] <- matches
    }
    if(DELETE){
      p <- ifelse(length(patterns[key.index[i,]])==1,
                  patterns[key.index[i,]],
                  paste0(patterns[key.index[i,]], collapse="|"))
      Data[i,col.in] <- str_squish(gsub(p,"",Data[i,col.in]))
    }
  }
  Data <- Data[,c(col.in,cols.out)]
  return(Data)
}