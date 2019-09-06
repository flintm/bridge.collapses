# USGS GNIS ID piped file, download 2019-09-06, NationalFile_20190901.txt
# FEATURE_ID|FEATURE_NAME|FEATURE_CLASS|STATE_ALPHA|STATE_NUMERIC|COUNTY_NAME|COUNTY_NUMERIC|PRIMARY_LAT_DMS|PRIM_LONG_DMS|PRIM_LAT_DEC|PRIM_LONG_DEC|SOURCE_LAT_DMS|SOURCE_LONG_DMS|SOURCE_LAT_DEC|SOURCE_LONG_DEC|ELEV_IN_M|ELEV_IN_FT|MAP_NAME|DATE_CREATED|DATE_EDITED
cols <- readLines(con = file.path("Data","Input","Standard","NationalFile_20190901.txt"),
                  1)
cols <- as.vector(str_split(cols,"\\|", simplify = TRUE))

check <- read.table(file.path("Data","Input","Standard","NationalFile_20190901.txt"),
                    header = TRUE, sep = "|", col.names = cols, nrows = 10,
                    stringsAsFactors = FALSE)
colClasses <- c(rep("character", 3), "NULL", "integer","NULL","integer","NULL","NULL",
                "numeric", "numeric", rep("NULL", 9))
check <- read.table(file.path("Data","Input","Standard","NationalFile_20190901.txt"),
                    header = TRUE, sep = "|", col.names = cols, colClasses = colClasses,
                    skip = 635,
                    nrows = 10,
                    stringsAsFactors = FALSE)

df.GNIS <- read.table(file.path("Data","Input","Standard","NationalFile_20190901.txt"),
                      header = TRUE, sep = "|", col.names = cols, colClasses = colClasses,
                      # nrows = 471131,
                      stringsAsFactors = FALSE, quote = "", comment.char = "")
df.GNIS$STFIPS <- factor(sprintf("%02d",df.GNIS$STATE_NUMERIC))
df.GNIS$FIPS   <- factor(sprintf("%05d",1000*df.GNIS$STATE_NUMERIC + df.GNIS$COUNTY_NUMERIC))
df.GNIS <- df.GNIS[,c(1,2,3,9,8,6,7)]
df.GNIS$FEATURE_CLASS <- factor(df.GNIS$FEATURE_CLASS)
colnames(df.GNIS)[1] <- "GNIS_ID"
df.GNIS$name <- gsub("'","",tolower(df.GNIS$FEATURE_NAME))
df.GNIS$name <- gsub("/","-", df.GNIS$name, fixed = TRUE)
df.GNIS$name <- gsub(" - ","-",df.GNIS$name, fixed = TRUE)
df.GNIS$name <- str_squish(sub("[\\(][[:print:]]+[\\)]","", df.GNIS$name))
df.GNIS$name <- sub("(city of )|(( city)$)","", df.GNIS$name)
df.GNIS$name <- sub("(township of )|( (township)$)","", df.GNIS$name)
df.GNIS$name <- sub("(town of )|(( town)$)","", df.GNIS$name)
df.GNIS$name <- sub("(village of )|(( village)$)","", df.GNIS$name)
df.GNIS <- df.GNIS[!(grepl("county", df.GNIS$name) & 
                       df.GNIS$FEATURE_CLASS %in% c("Civil","Locale")),]
save(df.GNIS,file = file.path("Data","Input","Standard","df.GNIS.RData"))

