load(file.path("Data","Input","Fail.Example.RData"))
row.names(df.Fail) <- as.character(df.Fail$ID)
# df.Fail <- df.Fail[df.Fail$ID %in% EXAMPLE_ID,]

# loaded to clean
df.Fail.l  <- df.Fail
source(file.path("Scripts","Process Input","Clean.Fields.R"))

FieldNames <- sapply(opts[["Fields"]][["Fail"]], function(i) ls.Fail.Ont[[i]][1],
                     USE.NAMES = TRUE)
df.Fail  <- Clean.Fields(df.Fail.l, NULL,
                         DATA_SET     = "Fail",
                         FieldNames   = FieldNames,
                         VERB      = VERB)

df.Fail.cl <- df.Fail

View(df.Fail[,c(16:20,22,30:31,33:34,42:51,56:59)])

# check pattern
df.patterns <- relationals
patterns   <- paste0(ls.pattern.types[[type]][1],
                     df.patterns[,"PATTERN1"],
                     ls.pattern.types[[type]][2],
                     df.patterns[,"PATTERN2"],
                     ls.pattern.types[[type]][3])
key.index      <- sapply(patterns,grepl,Data[2,"LOC"],ignore.case=TRUE)
dim(key.index) <- c(1,920)

sapply(df.patterns[key.index[i,],"LOC_AUX"], function(pat) regmatches(Data[2,"LOC"],regexpr(pat,Data[2,"LOC"])))
