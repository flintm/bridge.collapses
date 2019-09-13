str_clout <- function(str){str_squish(gsub("(\\A[[:punct:]])|(\\A[[:space:]])|([[:punct:]]\\Z)|([[:space:]]\\Z)","",str, perl = T))}
