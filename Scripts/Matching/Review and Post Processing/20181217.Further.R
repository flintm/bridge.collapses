# load notes on matches so that can concentrate on
# ones that I haven't looked at yet
temp <- readLines(con = "/Users/MM/Downloads/20181217.Addl.Matchig.Notes.txt")
temp <- temp[3:length(temp)]
# pattern <- " \\<[[:alpha:]]+\\> [[:print:]]?"
# IDs <- sapply(temp, function(line) gsub(pattern,"",line),
#               USE.NAMES = FALSE)
IDs <- substr(temp,1,4)
IDs <- gsub(" ","",IDs) 
IDs <- gsub("[[:alpha:]]","",IDs)  
IDs <- IDs[!grepl("[[:punct:]]",IDs)]
IDs <- IDs[IDs!=""] # 231 checked?
IDs <- IDs[IDs!="NA"]
IDs <- unique(IDs)
IDsAll <- c(as.numeric(IDs),df.Fail.NBI.Gage$ID)
# IDsAll <- IDsAll[!is.na(IDsAll)]

IDsToCheck  <- df.Fail$ID[!(df.Fail$ID %in% IDsAll)] # still good, so what problem?
# 592 haven't been looked at
# now 547, 514
# down to 279! Now 183. Checking previous scripts

# 2018-12-17 matches that I  haven't seen  yet
# PossibleMatchRowsNonConf <- PossibleMatchRowsSorted[!(names(PossibleMatchRowsSorted)%in% df.Fail.NBI.Gage$ID)]
# nMatchNC <- unlist(lapply(names(PossibleMatchRowsNonConf),
#                           function(n) ifelse(!is.na(PossibleMatchRowsNonConf[[n]][1]),
#                                              length(PossibleMatchRowsNonConf[[n]][1]),
#                                              NA_integer_)))
# PossibleMatchesNC <- PossibleMatchRowsNonConf[!is.na(nMatchNC)]
# nMatchNC <- unlist(lapply(PossibleMatchesNC,length))
# rm(PossibleMatchRowsNonConf,PossibleMatchRowsSorted)

PossibleMatchesNC <- PossibleMatchRowsSorted[as.character(IDsToCheck)]
nMatchNC <- unlist(lapply(PossibleMatchesNC,length))
PossibleMatchesNC <- PossibleMatchesNC[nMatchNC!=0] # down to 251, then 231
PossibleMatchesNC <- PossibleMatchesNC[!is.na(unlist(lapply(PossibleMatchesNC,"[[",1)))] # 
nMatchNC <- unlist(lapply(PossibleMatchesNC,length))

qualFirstNC <- unlist(lapply(names(PossibleMatchesNC),function(n) sub(".[[:digit:]]+\\>","",PossibleMatchesNC[[n]][1])))

BoolMatchFew <- nMatchNC >=45 # & nMatchNC < 45 # 
BoolQualMatch <- nchar(qualFirstNC) < 6 & substr(qualFirstNC,1,1)=="1"
# BoolExactFirst <- grepl("1f[[:alnum:]]{5,}", qualFirstNC) # no exact, 358 full, 187 multiple, 48 3 match
# BoolBIN <- grepl("fB.",qualFirstNC) # &  #277, 172 if full,33 if no others

# IDs <- names(PossibleMatchesNC)[BoolMatchFew & BoolQualMatch] #  & BoolQualMatch BoolMatchFew
# IDmanual <- c(121, 533, 525, 143, 1277, 402, 397, 1207, 1465, 1462, 1453,1445, 1443, 315, 1936, 3515, 3516, 3504, 3470, 1873)
IDmanual <- c(971)
IDs <- as.character(IDmanual)
ls.Rows <- lapply(IDs, function(n) sub("[[:alnum:]]+.","",PossibleMatchRowsSorted[[n]]))
names(ls.Rows) <- IDs
ls.df.Fail.Poss.NBI <- list()
for (n in IDs){
  nRow <- length(ls.Rows[[n]])
  df.Fail.Poss.NBI <- data.frame(ID = rep(n, nRow),
                                 NBI_ROW = ls.Rows[[n]])
  df.Fail.Poss.NBI <- merge(df.Fail.Poss.NBI,df.Fail,by="ID")
  df.PossNBI <- df.NBI[ls.Rows[[n]],]
  df.PossNBI$NBI_ROW <- row.names(df.PossNBI)
  df.Fail.Poss.NBI <- merge(df.Fail.Poss.NBI,df.PossNBI,by="NBI_ROW")
  ls.df.Fail.Poss.NBI[[n]] <- df.Fail.Poss.NBI
  
}
i<-0

i <- i +1
View(ls.df.Fail.Poss.NBI[[i]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)])

View(ls.df.Fail.Poss.NBI[["12"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("gravois cr",ls.df.Fail.Poss.NBI[["12"]]$ITEM6A) & grepl("tt",ls.df.Fail.Poss.NBI[["12"]]$ITEM7),])
View(ls.df.Fail.Poss.NBI[["170"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("35",ls.df.Fail.Poss.NBI[["170"]]$ITEM6A) & grepl("w",ls.df.Fail.Poss.NBI[["170"]]$ITEM7),])
View(ls.df.Fail.Poss.NBI[["1608"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("lake washington",ls.df.Fail.Poss.NBI[["1608"]]$ITEM6A) & grepl("0090",ls.df.Fail.Poss.NBI[["1608"]]$ROUTE_NO.y),])
View(ls.df.Fail.Poss.NBI[["36"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("bingamon",ls.df.Fail.Poss.NBI[["36"]]$ITEM6A),])# & grepl("0090",ls.df.Fail.Poss.NBI[["160836"]]$ROUTE_NO.y),])
View(ls.df.Fail.Poss.NBI[["1480"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("koontz",ls.df.Fail.Poss.NBI[["1480"]]$ITEM6A),])# & grepl("0090",ls.df.Fail.Poss.NBI[["160836"]]$ROUTE_NO.y),])
View(ls.df.Fail.Poss.NBI[["452"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("ston",ls.df.Fail.Poss.NBI[["452"]]$ITEM6A) & grepl("cold",ls.df.Fail.Poss.NBI[["452"]]$ITEM7),])
View(ls.df.Fail.Poss.NBI[["1479"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("north",ls.df.Fail.Poss.NBI[["1479"]]$ITEM6A) & grepl("047",ls.df.Fail.Poss.NBI[["1479"]]$ROUTE_NO.y),])
View(ls.df.Fail.Poss.NBI[["7"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("med",ls.df.Fail.Poss.NBI[["7"]]$ITEM6A) & grepl("036",ls.df.Fail.Poss.NBI[["7"]]$ROUTE_NO.y),])
View(ls.df.Fail.Poss.NBI[["883"]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)][grepl("chen",ls.df.Fail.Poss.NBI[["883"]]$ITEM6A) & grepl("16",ls.df.Fail.Poss.NBI[["883"]]$ROUTE_NO.y),])
View(df.NBI[grepl("salmon cr",df.NBI$ITEM6A) & df.NBI$STFIPS==53 & (grepl("99|005|11",df.NBI$ITEM5D)),])

# now ordering notes
temp <- readLines(con = "/Users/MM/Downloads/20181217.Addl.Matchig.Notes.txt")
temp <- temp[2:length(temp)]
IDs <- substr(temp,1,4)
IDs <- gsub(" ","",IDs) 
IDs <- gsub("[[:alpha:]]","",IDs) # 700 check lines as of 2018-12-19
IDsUniqueChecked <- unique(IDs) # 629 unique
Notes <- temp[order(as.integer(IDs))]
writeLines(Notes,"20181219.NotesOnMatches.Sorted.txt")

temp <- readLines(con = "20181219.NotesOnMatches.Sorted.Cleaned.txt")
IDs <- substr(temp,1,4)
IDs <- gsub(" ","",IDs) 
IDs <- gsub("[[:alpha:]]","",IDs) #
IDsUniqueChecked <- unique(IDs) # 630 for both X

# now figure out which to look at again: either "vl" or "cb" with *
IDs.vl <- IDs[grepl("^[[:digit:]]{1,4} vl ",temp)] # down to 22 to check
IDs.ast <- IDs[grepl("*",temp, fixed = TRUE)]
IDs.cb <- IDs[grepl("^[[:digit:]]{1,4} cb ",temp)]
IDs.cb.ast <- IDs[(IDs %in% IDs.ast) & (IDs %in% IDs.cb)] # 29, which is do-able
IDs.vl.ast <- IDs[(IDs %in% IDs.ast) & (IDs %in% IDs.vl)] # 3 only
IDs.has <- IDs[grepl("^[[:digit:]]{1,4} has ",temp)] # 26
IDs.has.ast <- IDs[(IDs %in% IDs.ast) & (IDs %in% IDs.has)] # 15

IDs.check <- IDs.vl.ast
ls.Rows <- lapply(IDs.check , function(n) sub("[[:alnum:]]+.","",PossibleMatchRowsSorted[[n]]))
names(ls.Rows) <- IDs.check 
ls.df.Fail.Poss.NBI <- list()
for (n in IDs.check ){
  nRow <- length(ls.Rows[[n]])
  df.Fail.Poss.NBI <- data.frame(ID = rep(n, nRow),
                                 NBI_ROW = ls.Rows[[n]])
  df.Fail.Poss.NBI <- merge(df.Fail.Poss.NBI,df.Fail,by="ID")
  df.PossNBI <- df.NBI[ls.Rows[[n]],]
  df.PossNBI$NBI_ROW <- row.names(df.PossNBI)
  df.Fail.Poss.NBI <- merge(df.Fail.Poss.NBI,df.PossNBI,by="NBI_ROW")
  ls.df.Fail.Poss.NBI[[n]] <- df.Fail.Poss.NBI
  
}
i<-0

i <- i +1
View(ls.df.Fail.Poss.NBI[[i]][,c(1,5,2,6,12,16,7,8,46,169,55:57,14,19,170)])

View(df.NBI[df.NBI$STFIPS==54 & grepl("\\<7\\>",df.NBI$ITEM3)& grepl("16",df.NBI$ITEM5D),]) #grepl("kan",df.NBI$ITEM6A) & 
View(df.NBI[df.NBI$STFIPS==53 & grepl("little boulder",df.NBI$ITEM6A),]) #grepl("kan",df.NBI$ITEM6A) & 
View(df.NBI[df.NBI$STFIPS==20 & grepl("bear",df.NBI$ITEM6A) & grepl("187",df.NBI$ITEM3),]) #grepl("kan",df.NBI$ITEM6A) & 

# for the rest of the asterisks, cb, has, check if gauge around
streams.check <- df.Fail[df.Fail$ID %in% IDs.ast, "STREAM_NAME"]
bool.stream.check <- sapply(streams.check, function(s) s!="" )
# only 8 non-blank
IDs.stream.check <- df.Fail[df.Fail$ID %in% IDs.ast,"ID"][bool.stream.check]
match.stream.STFIPS <- as.character(df.Fail[df.Fail$ID %in% IDs.stream.check,"STFIPS"])
match.streams.check <- df.Fail[df.Fail$ID %in% IDs.stream.check, "STREAM_NAME"]
IDs.stream.check  <- df.Fail[df.Fail$ID %in% IDs.stream.check, "ID"]
# bool.match.stream.check <- sapply(1:8, function(i) any(grepl(match.streams.check[i],as.character(df.USgauges$STANAME)) &
#                                                                          df.USgauges$STFIPS==match.stream.STFIPS[i]))

# "eel"    "5 mile" "hog"    "mill"   "owl"    "owl"    "dry"    "mill"  

i<-0

i<-i+1
df.USg.Adj <- df.USgauges[df.USgauges$STFIPS %in% ls.Adj.STFIPS[[match.stream.STFIPS[i]]],]
any(grepl(paste0("\\<",match.streams.check[i],"\\>"),as.character(df.USg.Adj$STANAME),ignore.case=TRUE))

View(df.USg.Adj[grepl(paste0("\\<",match.streams.check[i],"\\>"),as.character(df.USg.Adj$STANAME),ignore.case=TRUE),])
IDs.stream.check[i]
View(df.Fail[df.Fail$ID==IDs.stream.check[i],])

# how many "is"?
temp <- readLines(con = "20181220.NotesOnMatches.Sorted.Cleaned.txt")
IDs <- substr(temp,1,4)
IDs <- gsub(" ","",IDs) 
IDs <- gsub("[[:alpha:]]","",IDs) #
IDs.is <- IDs[grepl("^[[:digit:]]{1,4} is ",temp)] # 144 is's., 37 already confirmed
# so found 107 more bridges and very likely add'l?!
IDs.is.NBI_ROW <- temp[IDs %in% IDs.is]
IDs.is.NBI_ROW <- gsub("\\<[http][[:print:]]+\\>","",IDs.is.NBI_ROW)
IDs.is.NBI_ROW <- gsub("[[:alpha:]]","",IDs.is.NBI_ROW)
IDs.is.NBI_ROW <- gsub("[[:digit:]]{2,3}[.][[:digit:]]{2,6}","",IDs.is.NBI_ROW)
IDs.is.NBI_ROW <- gsub("[[:punct:]]","",IDs.is.NBI_ROW)
IDs.is.NBI_ROW <- gsub("\\<[[:digit:]]{1,4}\\>","",IDs.is.NBI_ROW)
IDs.is.NBI_ROW <- gsub("[[:space:]]","",IDs.is.NBI_ROW)

IDs.is[nchar(IDs.is.NBI_ROW)>6]
IDs.is.NBI_ROW[nchar(IDs.is.NBI_ROW)>6]
df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==250,"NBI_ROW"]
