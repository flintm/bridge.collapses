# makes a scale for a ggmap, based on directions by Andy Clifton at
# http://stackoverflow.com/questions/18136468/is-there-a-way-to-add-a-scale-bar-for-linear-distances-to-ggmap
# bb is obtained through attr(basemap,"bb")
createScale <- function(bb, ROUND = TRUE){

sbar <- data.frame(lon.start = c(bb$ll.lon + 0.1*(bb$ur.lon - bb$ll.lon)),
                   lon.end = c(bb$ll.lon + 0.25*(bb$ur.lon - bb$ll.lon)),
                   lat.start = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)),
                   lat.end = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)))

distHaversine <- function(long, lat){
  dlong = (long[2] - long[1])*pi/180
  dlat  = (lat[2] - lat[1])*pi/180
  
  # Haversine formula:
  R = 6371;
  a = sin(dlat/2)*sin(dlat/2) + cos(lat[1])*cos(lat[2])*sin(dlong/2)*sin(dlong/2)
  c = 2 * atan2( sqrt(a), sqrt(1-a) )
  d = R * c
  return(d) # in km
}
d<- distHaversine(long = c(sbar$lon.start,sbar$lon.end),
              lat = c(sbar$lat.start,sbar$lat.end))
if (ROUND){
  d.round <- 10^round(log10(d))
  sbar$lon.end <- c(bb$ll.lon + 0.25*(bb$ur.lon - bb$ll.lon)*d.round/d)
  sbar$distance = d.round
}
else{sbar$distance = d}
return(sbar)
}