Confirm.Matched.Bridges <- function(Fail, NBI, ls.Match){
  
  # set up for using quality codes to format table ------
  MatchQual <- c(absolute = "a",
                 exact   = "e",
                 full    = "f",
                 partial = "p",
                 fuzzy   = "z")
  MatchQuals <- paste0(MatchQual, collapse = "")
  qual1Rank  <- c(as.vector(sapply(1:2, function(i) paste0(i,MatchQual))),"")
  qual2Rank  <- apply(combn(qual1Rank,2), MARG = 2, paste0, collapse="")
  qual3Rank  <- apply(combn(qual1Rank,3), MARG = 2, paste0, collapse="")
  qualRank   <- c(qual3Rank, qual2Rank, qual1Rank)
  qualLabels <- round(1:length(qualRank)/length(qualRank),3)
  
  MatchSuffix <- c(bin    = "B",
                   road   = "R",
                   stream = "S",
                   route  = "T")
  MatchTypes <- toupper(names(MatchSuffix))
  
  Q_cols    <- paste0("Q_",MatchTypes)
  F_cols <- c(colnames(Fail)[1:11],"FIPS_1","FIPS_FROM_CITY_1")
  cols <- c("BIN","STRUCTURE_NUMBER_008","LOCATION","COUNTY_NBI","LOC",
            "ROAD","ROUTE_NUM",
            "FEAT_UND","STREAM_UNDER","FAIL_TYPE","YR_BLT","YR_FAIL","YEAR_BUILT_027","YEAR_RECONSTRUCTED_106",
            "MAT","MAT_NBI","TYPE","TYPE_NBI",Q_cols, "Q_COUNTY","Q_ALL")
  cols.out <- cols
  cols.out[c(2,5,6,7,9,13,14)] <- c("BIN_NBI","LOC_NBI","ROAD_NBI","ROUTE_NBI","FEAT_NBI","YR_BLT_NBI","YR_RBLT_NBI")

  # set up data.frame to hold collapsed bridge and potential matches ------
  df <- Fail[rep(1,length(ls.Match$MatchID)),F_cols]
  df$OBJECTID_new <- ls.Match$MatchID
  df$Qual         <- ls.Match$Qual
  for(m in tolower(MatchTypes)){
    df[,paste0("Q_",toupper(m))] <- sapply(row.names(df), 
                                            function(i) ifelse(grepl(MatchSuffix[m],df[i,"Qual"]),
                                                               substr(regmatches(df[i,"Qual"],
                                                                                 regexpr(paste0("[1-3][",MatchQuals,"]",MatchSuffix[m]),
                                                                                         df[i,"Qual"])),
                                                                      1,2),
                                                               ""),
                                            simplify = TRUE, USE.NAMES = FALSE)
  }
  df[,Q_cols] <- sapply(Q_cols, function(j) factor(df[,j], levels = qualRank, labels = qualLabels))
  df[,Q_cols] <- sapply(Q_cols, function(j) as.numeric(as.character(df[,j])))
  
  # Merge data frames and format columns ------
  df <- merge(df, NBI[,c("OBJECTID_new","STFIPS", "COUNTY_CODE_003",
                                       "STRUCTURE_NUMBER_008","LOC",
                                       "ROAD","ROUTE_NUM",
                                       "STREAM_UNDER","YEAR_BUILT_027","YEAR_RECONSTRUCTED_106","STRUCTURE_KIND_043A",
                                       "STRUCTURE_TYPE_043B")], 
                        by = "OBJECTID_new")
  df$MAT_NBI <- factor(as.character(df$STRUCTURE_KIND_043A),
                                 levels = names(ls.ITEM43A.Ont),
                                 labels = unlist(lapply(ls.ITEM43A.Ont,"[[",1)))
  df$TYPE_NBI <- factor(as.character(df$STRUCTURE_TYPE_043B),
                                  levels = names(ls.ITEM43B.Ont),
                                  labels = unlist(lapply(ls.ITEM43B.Ont,"[[",1)))
  df$COUNTY_NBI <- factor(as.character(df$COUNTY_CODE_003),
                                    levels = df.Counties$FIPS_C[df.Counties$FIPS_C %in% unique(df$COUNTY_CODE_003)],
                                    labels = df.Counties$COUNTY_NAME[df.Counties$FIPS_C %in% unique(df$COUNTY_CODE_003)])
  
  df[,grepl("(YR)|(YEAR)",colnames(df))] <- sapply(colnames(df)[grepl("(YR)|(YEAR)",colnames(df))],
                                                   function(j) as.integer(format.Date(df[,j],"%Y")))
  
  # sort based on overall match quality -------
  rows <- which(!is.na(df$FIPS_1))
  df$Q_COUNTY <- 1
  df[rows,"Q_COUNTY"] <- ifelse(df[rows,"COUNTY_CODE_003"]==df[rows,"FIPS_1"],0.2,1)
  rows <- which(!is.na(df$FIPS_FROM_CITY_1) & df$Q_COUNTY==1)
  df[rows,"Q_COUNTY"] <- ifelse(df[rows,"COUNTY_CODE_003"]==df[rows,"FIPS_FROM_CITY_1"],0.6,1)
  df$Q_ALL    <- df$Q_COUNTY*df$Q_BIN*df$Q_STREAM*df$Q_ROAD*df$Q_ROUTE # would be nice to make this clever
  df <- df[order(df$Q_ALL, df$Q_COUNTY, df$Q_BIN, df$Q_STREAM, df$Q_ROUTE, df$Q_ROAD),]
  df <- df[,cols]
  colnames(df) <- cols.out

  return(df)
}