Locate_Unmatched_Bridges <- function(BridgeDataFrame){
  # some NBI bridges have bad match data (according to US transporation atlas, about 110k)
  # if some of the failed bridges fall into that category, I will need to check the given location data
  # if there is no location data, I need a good head-start to locate these bridges by hand
  
# STEPS
# (1) sort bridges into bad-location classes: (a) accepted NBI data, (b) missing data
# (2) for class (a), check that bridge is also in stated county (not just state)
#     (2a) if in correct county, try to find intersection of road name and stream name
#          (i) if can find intersection, find minimum difference from given location to intersection, 
#               (I)  if small, accept lat/long
#               (II) if not small, label as hand-locate but give stream/road intersection as start point
#          (ii) if can't find intersection, or multiple, label and hand-locate and given county, road or stream if found
#     (2b) if not in correct county, go to correct county, try to find road or stream lines in county
#          (i)   if find road AND stream, get intersection, give as hand-locate data 
#          (ii)  if find road OR stream, give whatever data available
#          (iii) if find no data, label as hand-locate, problematic
# (3) for class (b), go to state, try to find road or stream lines
#     (3a) if find road AND stream, check for intersections
#          (i)   if one intersection, label as tentative location
#          (ii)  if multiple intersections, provide as hand-locate data
#          (iii) if no intersections, provide road and stream as hand-locate data
#     (3a) if find road OR stream, provide as hand-locate data
#     (3c) if find no data, label as hand-locate, problematic
}