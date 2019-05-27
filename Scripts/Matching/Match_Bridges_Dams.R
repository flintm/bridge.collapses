Match_Bridges_Dams <- function(BridgeDataFrame,DamDataFrame){

 
 library(stringr)
 
 # find rivers with dams on the river
 # ( will need to do some sort of partial string matching - need to convert case, need to remove
 #   any extraneous information or characters in the site_name, also need to check for alternate name,
 #   then need to make sure that the dam is upstream by checking elevations. Which means I need to make
 #   sure they're on the same datum. It would be ideal if I also do some sort of spatial
 #   test on the lat/long to make sure it's reasonably close.)
 # Maybe can use the main use information?
 # [1] "Hydroelectricity" "Recreation"       "Water supply"     "Irrigation"      
 # [5] "Flood control"    "Fisheries"        "Other"            NA                
# df.ds.USdams[seq(1,1902,by=100),"ADMIN_UNIT"]
# [1] "Alaska"         "California"     "Washington"     "Idaho"         
# [5] "Colorado"       "California"     "Minnesota"      "Minnesota"     
# [9] "Colorado"       "Oklahoma"       "Arkansas"       "Louisiana"     
#[13] "Wisconsin"      "Ohio"           "Indiana"        "North Carolina"
#[17] "Mississippi"    "Maine"          "New Hampshire"  "Michigan"# [9] "Navigation"
# so can match the last two characters of the site_name (CA->California) as a first step
# then can delete all characters from site_name starting with "AT" "NR" "A" "BL" "AB" "ABOVE" "BELOW" "NEAR"
# alternately, can keep first part of string up to "C" "WASH" "TRIB" "R" "RIVER" "TR" "DRAIN" "CREEK" "CANYON"
# "TRIBUTARY" "RV" "CR" "RUN"? "BROOK"?
# in the dams data they are called "River" or "Creek" or "Run" or "Brook" or "Draw" or "Canal"
# Maybe don't even use the USGS as an intermediary? Just go straight from NBI to Dams?
# looks like 223 dams in the SW
# > BridgeDataFrame[seq(1,421,by=20),"FEATURES_DESC_006A"]
# [1] 'Pinal Creek             ' 'Esperanza Wash          ' 'Wash                    '
# [4] 'Gila River Bridge       ' 'Dry Wash                ' 'Little Colorado River   '
# [7] 'Pinal Creek             ' 'Mesa Drainage Channel   ' 'Verde River             '
#[10] 'Falls Spring Wash       ' '              CAVE CREEK' 'E. Maricopa Floodway    '
#[13] 'Santa Cruz River        ' 'Eastern Canal           ' '           ARIZONA CANAL'
#[16] 'Indian Bend Wash        ' 'Consolidated Canal      ' 'Indian Bend Wash        '
#[19] 'Pantano Wash            ' 'East Maricopa Fldwy     ' 'CAP Canal               '
#[22] 'BLACK   CREEK           '

# STEPS TO MATCH THE STRINGS
# Start by getting the NBI data better organized
# 1. Lower case
# 2. Expand all "E." or "E" to "east", etc.
# 3. Shorten "tributary" to "trib"
# 4. Move "trib" to front of string
# 5. Delete all remaining spaces and punctuation
# Then get the dams data organized
# 6. Lower case
# 7. Delete punctuation and spacing
# then make sure this covers the "middle fork" or "north fork"-type issues
# use grep to find strings in which "E" or "N" or "trib" is present
# use gsub("[[:punct:]]","","Trib. Cajalco Creek") to remove all punctuation
# use gsub(" ","","string too many spaces") to remove all spaces
#> any(grepl("Wash",df.ds.SWdams$RIVER))
#[1] TRUE
#> any(grepl("Creek",df.ds.SWdams$RIVER))
#[1] TRUE
#> any(grepl("Channel",df.ds.SWdams$RIVER))
#[1] FALSE
#> any(grepl("Canal",df.ds.SWdams$RIVER))
#[1] FALSE
# > any(grepl("Floodway",df.ds.USdams$RIVER))
# [1] FALSE
# so no dams on channels or floodways in the US


### PROCESS THE BRIDGE DATA TO MATCH
BridgeDataFrame$RIVERMATCH <- tolower(BridgeDataFrame$FEATURES_DESC_006A)
BridgeDataFrame$RIVERMATCH <- gsub("[[:punct:]]","",BridgeDataFrame$RIVERMATCH)
# expand north, south, east, west
BridgeDataFrame$RIVERMATCH <- gsub(" e ", " east ", BridgeDataFrame$RIVERMATCH)
BridgeDataFrame$RIVERMATCH <- gsub(" w ", " west ", BridgeDataFrame$RIVERMATCH)
BridgeDataFrame$RIVERMATCH <- gsub(" n ", " north ", BridgeDataFrame$RIVERMATCH)
BridgeDataFrame$RIVERMATCH <- gsub(" s ", " south ", BridgeDataFrame$RIVERMATCH)
BridgeDataFrame$RIVERMATCH <- gsub(" eastern ", " east ", BridgeDataFrame$RIVERMATCH)
BridgeDataFrame$RIVERMATCH <- gsub(" western ", " west ", BridgeDataFrame$RIVERMATCH)
BridgeDataFrame$RIVERMATCH <- gsub(" northern ", " north ", BridgeDataFrame$RIVERMATCH)
BridgeDataFrame$RIVERMATCH <- gsub(" southern ", " south ", BridgeDataFrame$RIVERMATCH)

# if tributary, put "trib" at front
i = grepl("tributary",BridgeDataFrame$RIVERMATCH)
BridgeDataFrame[i,"RIVERMATCH"] <- gsub("tributary","",BridgeDataFrame[i,"RIVERMATCH"])
j = grepl("trib",BridgeDataFrame$RIVERMATCH)
BridgeDataFrame[j,"RIVERMATCH"] <- gsub("trib","",BridgeDataFrame[j,"RIVERMATCH"])
BridgeDataFrame[i,"RIVERMATCH"] <- paste("trib",BridgeDataFrame[i,"RIVERMATCH"],sep=" ")
BridgeDataFrame[j,"RIVERMATCH"] <- paste("trib",BridgeDataFrame[j,"RIVERMATCH"],sep=" ")

# expand R, RV, etc.
BridgeDataFrame$RIVERMATCH <- gsub(" r ", " river ", BridgeDataFrame$RIVERMATCH)
BridgeDataFrame$RIVERMATCH <- gsub(" rv ", " river ", BridgeDataFrame$RIVERMATCH)
BridgeDataFrame$RIVERMATCH <- gsub(" riv ", " river ", BridgeDataFrame$RIVERMATCH)
BridgeDataFrame$RIVERMATCH <- gsub(" cnl ", " canal ", BridgeDataFrame$RIVERMATCH)

#trim
BridgeDataFrame$RIVERMATCH <- str_trim(BridgeDataFrame$RIVERMATCH)


### PROCESS THE DAM DATA TO MATCH
DamDataFrame$RIVERMATCH <- tolower(DamDataFrame$RIVER)
DamDataFrame$RIVERMATCH <- gsub("[[:punct:]]","",DamDataFrame$RIVERMATCH)
DamDataFrame$RIVERMATCH <- gsub(" eastern ", " east ", DamDataFrame$RIVERMATCH)
DamDataFrame$RIVERMATCH <- gsub(" western ", " west ", DamDataFrame$RIVERMATCH)
DamDataFrame$RIVERMATCH <- gsub(" northern ", " north ", DamDataFrame$RIVERMATCH)
DamDataFrame$RIVERMATCH <- gsub(" southern ", " south ", DamDataFrame$RIVERMATCH)

### NOW MATCH - SEPARATELY BY WORD IN THE BRIDGE DATA - ALL MUST MATCH SAME ROW OF DAM DATA
# first see if in same state - dams may span two states, so use both 
# (assumes that pass data from one state at a time)
MatchStateCode = BridgeDataFrame[1,"STATE_CODE_001"]
if (MatchStateCode==6) {MatchState=="California"
else
	if (MatchStateCode==4) {MatchState=="Arizona"
	else  if (MatchStateCode==35) {MatchState=="New Mexico" else return("Not coded for given state")   }
	}
}
PossibleMatchDams <-subset(DamDataFrame,ADMIN_UNIT==MatchState | SEC_ADMIN==MatchState)
for (i in 1:nrow(BridgeDataFrame)){
	PossibleMatch <- PossibleMatchDams
	ls.names <- strsplit(BridgeDataFrame[i,"RIVERMATCH"]," ")
	for (j in 1:length(ls.names)) {
		DamMatch <- grep(ls.names[1][j],PossibleMatch)
		if (DamMatch != NA) {
			PossibleMatch <- PossibleMatch[DamMatch,]
		else next
		}
	}
	# if went through all names in Bridge River and found one or more matches, then check elevation
	if (nrow(PossibleMatch)!=0) {
		#################### PROBABLY GOING TO NEED TOPO FILE	
		for (j in 1:nrow(PossibleMatch)){
			
		}
		
	}
	
}
	

### CHECK WHETHER OR NOT DAM IS UPSTREAM, AND DISTANCE

return(BridgeDataFrame)
}