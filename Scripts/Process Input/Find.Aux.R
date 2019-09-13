# Code chunk (not a true subfunction) segmenting auxilliary phrase detection
Find.Aux <- function(Data,Feature,col.in,cols.out,n.dup.cols, DELETION = TRUE, VERBOSE = FALSE){
  Rows <- rownames(Data)[!is.na(Data[,col.in]) & Data[,col.in]!=""]
  
  # Keys
  cardKeys <- unlist(ls.CardKeys[1:8])
  distKeys <- unlist(ls.RelationalKeys[c("miles","kilometers")])
  
  # non state-specific auxilliary phrase checks ---------
  if(length(Rows)>0){
    if(VERBOSE) print("Auxilliary phrase checks.")
    
    # (1) statements with explicit distance, e.g., "3.5 miles from Stanford"---------
    rows <- Rows[grepl("[[:digit:]][[:space:]]?(m|k)",Data[Rows,col.in])]
    if(length(rows)>0){
      if(VERBOSE) print("Checking distance-relational auxilliary phrases")
      # note: assumes that the relational destination ("stanford") is 1 or 2 words in length
      # followed by end-of-line, comma, period, or dash.
      # pulls parentheses if present
      relKeys  <- unlist(ls.RelationalKeys[c("off","from",
                                            "by","of","near","at",
                                            "in_loc")])
      relationals <- sapply(distKeys,
                            function(dist)
                              sapply(c("",cardKeys),
                                     function(card)
                                       sapply(c("",relKeys),
                                              function(rel)
                                                paste0("([(]?)([[:digit:]]+[.]?[[:digit:]]?[[:space:]]?)(",
                                                       dist,
                                                       " ",
                                                       card,
                                                       " ",
                                                       rel,
                                                       ")( [[:alpha:]]+[[:space:]]?[[:alpha:]]{0,}[[:space:]]?[[:alpha:]]{0,})([,.-) ]|$)")
                                       )))
      relationals <- data.frame(PATTERN = as.vector(relationals), 
                                REGEX = TRUE, stringsAsFactors = FALSE)
      relationals$PATTERN <- str_squish(relationals$PATTERN)
      relationals$PATTERN <- gsub(" )",")",relationals$PATTERN,fixed = TRUE)
      # relationals$PATTERN <- sub("[[:space:]]?[[:alpha:]]+","\\> [[:alpha:]]+",relationals$PATTERN,fixed = TRUE)
      # relationals <- relationals[!duplicated(relationals$PATTERN),]
      relationals$AUX <- relationals$PATTERN
      relationals <- relationals[order(nchar(relationals$PATTERN),decreasing = TRUE),]
      
      Data[rows,c(col.in,cols.out)] <- Feature.Detect(Data[rows,],
                                                      relationals,
                                                      col.in,
                                                      cols.out,
                                                      n.dup.cols = n.dup.cols,
                                                      DELETE = TRUE)
    }
    
    # (2) non-explicit distance relationals -------
    relKeys  <- unlist(ls.RelationalKeys[c("off","from",
                                           "by","of","near","at",
                                           "in_loc")])
    rows        <- Rows[grepl(paste0("(\\<",relKeys,"\\>)",collapse="|"),
                              Data[Rows,col.in]) | 
                          grepl(paste0("(\\<",cardKeys,"\\>)", collapse = "|"),
                                Data[Rows,col.in])]
    if(length(rows)>0){
      if(VERBOSE) print("Checking relational auxilliary phrases")
      
      
      relationals <- as.vector(sapply(c("",cardKeys),
                                      function(card)
                                        sapply(c(relKeys,""),
                                               function(rel)
                                                 paste0("([\\(]?)(\\<",card,"\\>",
                                                        "[. ]{0,2}",
                                                        rel,
                                                        ")( [[:alpha:]]+[[:space:]]?[[:alpha:]]{0,}[[:space:]]?[[:alpha:]]{0,})([,.-\\)]|$)")
                                        )))
      relationals <- sub("(\\<\\>[. ]{0,2}", "(\\<",relationals, fixed = TRUE)
      # relationals <- sub("[. ]{0,2} ","[.]? ", relationals, fixed = TRUE)
      relationals <- relationals[order(nchar(relationals), decreasing = TRUE)]
      relationals <- relationals[relationals!="([\\(]?)(\\<)( [[:alpha:]]+[[:space:]]?[[:alpha:]]{0,}[[:space:]]?[[:alpha:]]{0,})([,.-\\)]|$)"]
      relationals <- relationals[relationals!="([\\(]?)(\\<of)( [[:alpha:]]+[[:space:]]?[[:alpha:]]{0,}[[:space:]]?[[:alpha:]]{0,})([,.-\\)]|$)"]
      relationals <- data.frame(PATTERN = relationals,  REGEX = TRUE, stringsAsFactors = FALSE)
      relationals$AUX <-relationals$PATTERN # sub(" [[:alpha:]]+","",relationals$PATTERN, fixed = TRUE)
      # relationals[!grepl("(\\?)$",relationals$AUX),
      # "AUX"] <- paste0(relationals[!grepl("(\\?)$",
      #                                                  relationals$AUX),"AUX"],
      #                                        "\\>")
      # print(n.dup.cols)
      # print(colnames(relationals))
      # print(c(col.in,cols.out))
      Data[rows,c(col.in,cols.out)] <- Feature.Detect(Data[rows,],
                                                      relationals,
                                                      col.in,
                                                      cols.out,
                                                      n.dup.cols = n.dup.cols,
                                                      DELETE = TRUE)
      Data[rows,col.in] <- str_squish(gsub("(","",gsub(")","",Data[rows,col.in], fixed = T), fixed = TRUE))
    }
    
    # parentheticals ------
    rows    <- Rows[grepl("(",Data[Rows,col.in], fixed = TRUE)]
    if(length(rows)>0){
      if(VERBOSE) print("Checking parenthetical auxilliary phrases")
      parenth <- data.frame(PATTERN = "[\\(][.,a-z0-9-]+[\\)]", 
                            REGEX = TRUE, stringsAsFactors = FALSE)
      parenth$AUX <- parenth$PATTERN
      Data[rows,c(col.in,cols.out)] <- Feature.Detect(Data[rows,],
                                                      parenth,
                                                      col.in,
                                                      cols.out,
                                                      n.dup.cols = n.dup.cols,
                                                      DELETE = TRUE)
      Data[,col.in] <- str_squish(gsub("(","",gsub(")","",Data[,col.in], fixed = T), fixed = TRUE))
      
    }
  }
  return(Data[,c(col.in,cols.out)])
}