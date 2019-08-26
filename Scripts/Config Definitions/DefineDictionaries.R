# Custom dictionaries used to match (hydraulic) bridge collapses from the 
# NYSDOT Bridge Failure Database to the NBI or other datasets
# Dictionaries are written to R lists and also as .json files, to support
# future use of GEOjson.
require(rjson)  

# Pull in state, county and city data if not already present
  munic <- c("States","Counties","Cities")
  for(m in munic[!(paste0("df.",munic) %in% ls())]){
    load(file.path("Data","Input","Standard",paste0("df.",m,".RData")))
  }
  rm(munic)

## ROADS, ROUTES, RAILS & BRIDGES --------------
  # Road: note that order is set so that the most complete match is found
  ls.RoadKeys <- list(countyRoad = c("county road", "co road", "co rd", "county rd", "c r", "cr"),
                      parkRoad   = c("national park road", "park road", "park rd"),
                      stateLands = c("state lands road", "state lands rd", "state lands"),
                      countyHighway = c("county highway", "co highway", "co hwy"),
                      connector  = c("connector", "conn"),
                      privateRoad=c("private road", "priv road", "prvt road", "priv rd", "priv road", "pr"),
                      stateRoute = c("state route", "state rte", "st rte", "st rt", "s r", "sr"),
                      tempRoute  = c("temporary route", "temp rte"),
                      stateHighway=c("state highway", "st hwy", "sh", "psh"), #psh = "primary state highway" in washington
                      stateSecondary=c("state secondary", "st secondary"),
                      cityRoute  = c("city route","city rt", "city rte"),
                      thruway    = c("thruway","thwy","th"),
                      usHighway  = c("us highway", "us route", "us rte", "us"),
                      avenue     = c("avenue", "ave"),
                      court      = c("court", "crt", "ct"),
                      crossing   = c("crossing", "crss", "crs", "cross", "xng"),
                      drive      = c("drive", "drv", "dr", "d"),
                      freeway    = c("freeway", "frwy", "fwy", "free", "f"),
                      highway    = c("highway", "hwy", "hw", "high","h"),
                      interstate = c("interstate", "int", "i"),
                      lane       = c("lane", "ln"),
                      nfs        = c("national forest service", "nfs road", "forest road", "forest track", "fire road", "fire track", "fire trail"),
                      parkway    = c("parkway","pkwy"),
                      pass       = c("pass", "pss", "pa", "p"),
                      road       = c("road", "rd"),
                      route      = c("route", "rte", "rt", "r"),
                      street     = c("street", "strt", "st"),
                      state      = c(tolower(df.States$STATE_CODE),"k"),
                      statefull  = tolower(df.States$STATE_FULL))
  save(ls.RoadKeys, file = file.path("Data","Input","Dictionaries","ls.RoadKeys.RData"))
  cat(toJSON(ls.RoadKeys), file = file.path("Data","Input","Dictionaries","RoadKeys.json"))
  
  # Route (see also ITEM5B)
  ls.RteKeys <- ls.RoadKeys[c("freeway","highway","interstate","countyRoad",
                                      "stateRoute","stateHighway","usHighway",
                              "countyHighway", "stateSecondary", "thruway","state", "statefull",
                              "route")]
  save(ls.RteKeys, file = file.path("Data","Input","Dictionaries","ls.RteKeys.RData"))
  cat(toJSON(ls.RteKeys), file = file.path("Data","Input","Dictionaries","RteKeys.json"))
  
  # Bridges
  ls.BridgeKeys <- list( pedBridge  = c("pedestrian bridge","poc", "pedestrian br","pedestrian overpass","pedestrian walkway", 
                                        "ped overpass","ped br","pedest overpass","pedestrain bridge","pedestrain"),
                         bridge     = c("bridge", "brdge", "brdg", "br"),
                         overcross  = c("overcrossing","oc")
  )
  ls.BridgeKeys$bridgeNo <- c(paste(ls.BridgeKeys$bridge,"no",sep=" "), "no")
  save(ls.BridgeKeys, file = file.path("Data","Input","Dictionaries","ls.BridgeKeys.RData"))
  cat(toJSON(ls.BridgeKeys), file = file.path("Data","Input","Dictionaries","BridgeKeys.json"))
  
  # Rail
  ls.RailKeys <- list(railway = c("railway", "railroad", "rlwy", "rail","rr"),
                      amtrack = c("amtrack","amtrak"))
  save(ls.RailKeys, file = file.path("Data","Input","Dictionaries","ls.RailKeys.RData"))
  cat(toJSON(ls.RailKeys), file = file.path("Data","Input","Dictionaries","RailKeys.json"))
  
## JURISDICTION, COUNTY, CITY, PLACE -----------------
  # Jurisdiction/Municipalities
  ls.JurisKeys <- list(city     = c("city", "cty"),
                       county   = c("county", "co", "cnty", "cnt"),
                       state    = c("state"),
                       town     = c("town", "twn"),
                       township = c("township", "twnshp","twnsp","twsp", "twp"),
                       village  = c("village", "villlage"))
  save(ls.JurisKeys, file = file.path("Data","Input","Dictionaries","ls.JurisKeys.RData"))
  cat(toJSON(ls.JurisKeys), file = file.path("Data","Input","Dictionaries","JurisKeys.json"))
  
  # Place abbreviations and misspellings
  ls.CountyKeys <- list(prince_georges = c("prince george s","prince george's", "prince georges", "p geor", "p geo", "prince george", "prince georg", "p. geor", "prin geor"),
                        prince_william = c("prince william", "pr. william", "pr william"),
                        prince      = c("prince", "prin"),
                        montgomery  = c("montgomery", "montgomry", "montgom", "mongomery","mont"),
                        baltimore   = c("baltimore", "balt"),
                        le_sueur    = c("le sueur", "lesueur"),
                        lewis       = c("lewis", "lewisco"),
                        hamp        = c("hampshire", "hamp"),
                        doddridge   = c("doddridge", "dodridge", "dodd"),
                        pendleton   = c("pendleton", "pend"),
                        pocahontas  = c("pocahontas", "poca"),
                        rand        = c("randolph", "rand", "ran"),
                        preston     = c("preston", "pres"),
                        monongalia  = c("monongalia", "monogalia"),
                        humboldt    = c("humboldt", "humbolt"),
                        audubon     = c("audubon", "audabon"),
                        atchison    = c("atchison", "atchinson"),
                        buena_vista = c("buena vista", "buean vista"),
                        buch        = c("buchanan", "buch"),
                        geary       = c("geary" , "greary"),
                        pottawatomie   = c("pottawatomie", "pottawotamie"),
                        sacramento     = c("sacramento", "sacremento"),
                        saint_clair    = c("saint clair", "st. clair", "st clair"),
                        saint_louis    = c("saint louis", "st louis", "st. louis"),
                        saint_charles  = c("saint charles", "st. charles", "st charles"),
                        burleigh       = c("burleigh", "bruliegh", "bruleigh"),
                        northumberland = c("northumberland", "no umberland"),
                        anasco         = c("añasco", "anasco"),
                        rappahannock   = c("rappahannock", "rappahannaok"),
                        albemarle      = c("albemarle", "abermarle", "abemarle"),
                        valencia       = c("valencia", "velencia"),
                        bedford        = c("bedford", "beford"),
                        louisa         = c("louisa", "lousia"),
                        mercer         = c("mercer", "mercert"))
  save(ls.CountyKeys, file = file.path("Data","Input","Dictionaries","ls.CountyKeys.RData"))
  cat(toJSON(ls.CountyKeys), file = file.path("Data","Input","Dictionaries","CountyKeys.json"))
  
  ls.PlaceKeys <- list(washington = c("washington", "wash."),
                       illinois   = c("illinois", "ill"),
                       american   = c("american", "amer","amer."),
                       savannah   = c("savannah", "savanah"),
                       montgomery  = c("montgomery", "montgomry", "montgom", "mongomery"),
                       mountain   = c("mountain", "mtn"),
                       school     = c("school", "schl","sch"),
                       hill       = c("hill", "hil"),
                       valley     = c("valley", "valey","vly"),
                       platte     = c("platte", "plat"),
                       plantation = c("plantation", "plnt","plt"),
                       saint_george   = c("saint george", "st. george", "st george"),
                       arrington      = c("arrington", "arrinton"),
                       bismarck       = c("bismarck", "bismark"),
                       saint_ann      = c("saint ann", "st. ann", "st ann"),
                       saint_john     = c("saint john", "st. john", "st john"),
                       saint_helen   = c("saint helens","st. helen's","saint helen", "st. helen", "st helen"),
                       nicolaus       = c("nicolaus", "nicholaus"),
                       denison        = c("denison", "denisen"))
  
  save(ls.PlaceKeys, file = file.path("Data","Input","Dictionaries","ls.PlaceKeys.RData"))
  cat(toJSON(ls.PlaceKeys), file = file.path("Data","Input","Dictionaries","PlaceKeys.json"))
 
  
## CARDINAL AND RELATIONAL -------------
  # Cardinal directions
  ls.CardKeys <- list(south = c("south", "southern", "so", "s"), 
                      north  = c("north", "northern", "no","n"), 
                      east   = c("east", "eastern", "e"), 
                      west   = c("west", "western", "w"),
                      nw     = c("northwest", "nw"), 
                      ne     = c("northeast", "ne"), 
                      sw     = c("southwest", "sw"), 
                      se     = c("southeast", "se"), 
                      sb     = c("southbound", "sb", "s"), 
                      nb     = c("northbound", "nb", "n"), 
                      eb     = c("eastbound", "eb", "e"), 
                      wb     = c("westbound", "wb", "w"))
  save(ls.CardKeys, file = file.path("Data","Input","Dictionaries","ls.CardKeys.RData"))
  cat(toJSON(ls.CardKeys), file = file.path("Data","Input","Dictionaries","CardKeys.json"))
  
  # Relations, geographic and size
  ls.RelationalKeys <- list(main   = c("main", "mn"),
                            middle = c("middle", "mid"),
                            left   = c("left", "lt", "l"),
                            right  = c("right", "rt", "r"),
                            near   = c("near", "nr"),
                            by     = c("by"),
                            of     = c("of"),
                            on     = c("on"),
                            from   = c("from"),
                            to     = c("to"),
                            over   = c("over", "ovr"),
                            intersection = c("intersection", "int", "ih"),
                            junction = c("junction", "junct", "jct"),
                            in_loc   = c("in"),
                            off      = c("off"),
                            at       = c("at"),
                            miles    = c("miles", "mile", "mi", "m"),
                            kilometers = c("kilometers", "kilometer", "km", "k"),
                            upper    = c("upper","upr"),
                            lower    = c("lower","low"),
                            little   = c("little", "litttle", "ltle"),
                            big      = c("big"))
  save(ls.RelationalKeys, file = file.path("Data","Input","Dictionaries","ls.RelationalKeys.RData"))
  cat(toJSON(ls.RelationalKeys), file = file.path("Data","Input","Dictionaries","RelationalKeys.json"))
  
  
## STREAMS AND TRIBUTARIES ---------
  # Streams
  ls.StreamKeys <- list(arroyo    = c("arroyo", "ary"),
                        bayou     = c("bayou", "byou", "byo","bay"),
                        brook     = c("brook", "brk", "bk", "bro", "b"),
                        branch    = c("branch","brnch", "brn", "bra", "br"),
                        creek     = c("creek", "crk", "ck", "cr"),
                        canal     = c("canal", "cnl", "can", "cn"),
                        canyon    = c("canyon", "cnyn"),
                        channel   = c("channel", "chnl", "chn", "cha", "ch"),
                        cove      = c("cove"),
                        dam       = c("dam"),
                        delta     = c("delta"),
                        ditch     = c("ditch", "dtch", "dt", "dit", "d"),
                        eddy      = c("eddy", "edd"),
                        falls     = c("falls", "fall", "fll", "fal", "f"),
                        floodway  = c("floodway", "fld"),
                        fork      = c("fork", "frk", "fk", "f"),
                        forks     = c("forks", "frks"),
                        gorge     = c("gorge"),
                        gully     = c("gully"),
                        hollow    = c("hollow", "hllw", "hlw", "hol", "h"),
                        inlet     = c("inlet"),
                        kill      = c("kill", "kll", "kl", "kil", "k"),
                        lakes     = c("lakes", "lake", "lks", "lk", "lak", "la", "l"),
                        lock      = c("lock"),
                        ponds     = c("ponds", "pond"),
                        river     = c("river", "rive", "rvr", "riv", "rv", "r"),
                        rio       = c("rio"),
                        run       = c("run", "rn"),
                        overflow  = c("overflow","overflo", "overf"),
                        slough    = c("slough", "slgh", "slo", "sl"),
                        spillway  = c("spillway", "spw"),
                        stream    = c("stream", "stre", "str"),
                        swamp     = c("swamp", "swmp", "swa", "sw"),
                        tributary = c("tributary", "trb", "trib", "tr", "t"),
                        wash      = c("wash", "wsh", "w"))
  save(ls.StreamKeys, file = file.path("Data","Input","Dictionaries","ls.StreamKeys.RData"))
  cat(toJSON(ls.StreamKeys), file = file.path("Data","Input","Dictionaries","StreamKeys.json"))
  
  # Tributaries (branches and forks)
  Cards        <- unlist(ls.CardKeys[!grepl("b",names(ls.CardKeys))])
  Relations    <- unlist(ls.RelationalKeys[c("main", "middle", "left","right","upper","lower","little", "big")])
  Tribs        <- unlist(ls.StreamKeys[c("branch", "fork", "tributary")])
  ls.TribKeys <- sapply(c("",Cards),
                        function(card)
                          sapply(c("",Relations),
                                   function(rel)
                                     sapply(Tribs,
                                            function(trib)
                                              paste0("\\<",card,
                                                     "\\> \\<",
                                                     rel,
                                                     "\\> \\<",
                                                     trib,"\\>"))
                                   
                          ))
  # ls.TribKeys  <- sapply(1:length(ls.TribKeys), 
  #                        function(i) gsub("[[:space:]][[:space:]]","[[:space:]]",ls.TribKeys[[i]],fixed = TRUE))
  # ls.TribKeys  <- sapply(1:length(ls.TribKeys), 
  #                        function(i) sub("^\\[\\[\\:space\\:\\]\\]","",ls.TribKeys[[i]]))
  ls.TribKeys  <- str_squish(gsub("\\<\\>","",ls.TribKeys,fixed = TRUE))
  ls.TribKeys  <- ls.TribKeys[grepl("\\<", sub("\\<","",ls.TribKeys, fixed = TRUE), fixed = TRUE)]
  ls.TribKeys  <- unlist(ls.TribKeys)
  ls.TribKeys  <- c(ls.TribKeys, paste0("\\<",c("n","s","e","w","m","r","l"),"f\\>"))
  # for (i in 1:length(Cards)){
  #   for (j in 1:length(Tribs)){
  #     ls.TribKeys[[paste0(names(Cards)[i],"_",names(Tribs)[j])]] <- paste(Cards[[i]],Tribs[[j]])
  #   }
  # }
  # for (i in 1:length(Relations)){
  #   for (j in 1:length(Tribs)){
  #     ls.TribKeys[[paste0(names(Relations)[i],"_",names(Tribs)[j])]] <- paste(Relations[[i]],Tribs[[j]])
  #   }
  # }
  rm(Cards,Relations,Tribs)
  save(ls.TribKeys, file = file.path("Data","Input","Dictionaries","ls.TribKeys.RData"))
  cat(toJSON(ls.TribKeys), file = file.path("Data","Input","Dictionaries","TribKeys.json"))
  
## BRIDGE STRUCTURE MATERIALS AND TYPES --------------
  ls.MatKeys   <- list(pc      = c("prestressed concrete", "prestress concrete", "pc concrete", 
                                          "pc concret", "concrete prestress", "prestressed conc", "prestress", 
                                          "pc", "p s concrete", "p s conc","p s", "ps","5","6"), 
                              conc    = c("concrete", "concret", "conc","1","2"), 
                              steel   = c("steel", "3", "4"), 
                              wood    = c("wood", "timber", "timb","log", "7"),
                              masonry = c("masonry", "stone","8"), 
                              metal   = c("aluminum", "cast iron", "wrought iron", "wrght iron", "iron", "alum", "cor metal", "metal", "iron","9"), 
                              other   = c("other", "0"))
  save(ls.MatKeys, file = file.path("Data","Input","Dictionaries","ls.MatKeys.RData"))
  cat(toJSON(ls.MatKeys), file = file.path("Data","Input","Dictionaries","MatKeys.json"))
  
  ls.SupKeys   <- list(simple = c("simply supported", "simple", "smp", "simp", "ss"), 
                         cont = c("continuous", "contin", "cont", "con", "cnt"))
  save(ls.SupKeys, file = file.path("Data","Input","Dictionaries","ls.SupKeys.RData"))
  cat(toJSON(ls.SupKeys), file = file.path("Data","Input","Dictionaries","SupKeys.json"))
  

  # TypeKeys need to be listed out of NBI order so that more specific definitions (e.g., "t beam") are 
  # listed earlier than more generic ones (e.g., "beam")
  ls.TypeKeys <- list(slab    = c("slab", "span","01"), 
                      girdFB  = c("girder floorbeam", "girder fb", "03"),
                      tBeam   = c("t beam", "t-beam", "tee beam", "tbeam", "04"), 
                      frame   = c("frame", "rigid frame", "rigid","07"), 
                      ortho   = c("orthotropic","08"), 
                      trussD  = c("truss deck", "deck truss", "09"),
                      trussT  = c("truss thru", "truss through", "thru truss", "through truss", "truss beam", "truss prat",
                                  "truss pony", "pony truss", "truss grdr", "high truss", "bailey", "covered","cov","10"),
                      truss   = c("truss"),
                      archD   = c("arch deck", "deck arch", "filled arch", "jack arch","11"), 
                      archT   = c("arch thru", "arch through", "thru arch", "through arch", "tied arch","12"),
                      arch    = c("arch"),
                      susp    = c("suspension","13"), 
                      stayGird= c("stayed girder","14"), 
                      mvLift  = c("movable lift", "lift","15"),
                      mvBasc  = c("movable bascule", "bascule", "bascu","16"), 
                      mvSwing  = c("movable swing", "swing","17"), 
                      tunnel  = c("tunnel","18"),
                      culvert = c("box culvert","culvert","pipe arch", "corrugated pipe",  "pipe","19"),
                      mixed   = c("mixed approach", "mixed","20"), 
                      seg     = c("segmental box girder","21"),
                      chnlBm  = c("channel beam","22"), 
                      other   = c("other", "trestle", "deck girder","00","0"),
                      boxMult = c("box beam multiple", "box girder multiple", "multiple box beam", "multiple box girder","05"), 
                      boxSing = c("box beam single", "box beam spread", "box girder single", "box girder spread","06"),
                      box     = c("box beam", "box"),
                      string  = c("stringer", "multi beam", "girder", "mult beam", "thru girder", "702", "multi gird", "beam",
                                  "rolled beam", "flatcar", "i beam", "w beam", "two girder","rolled bm","02"))
  
  save(ls.TypeKeys, file = file.path("Data","Input","Dictionaries","ls.TypeKeys.RData"))
  cat(toJSON(ls.TypeKeys), file = file.path("Data","Input","Dictionaries","TypeKeys.json"))
  
## DATABASE-SPECIFIC MISSPELLINGS AND CORRECTIONS
  ls.Fail.Keys <- list("671"  = c("richmond","richland","LOC"),
                       "1345" = c("wash.", "washington ","LOC"),
                       "212"  = c("amer.", "american ", "LOC"),
                       "1304" = c("s4\\&5","s4\\-5","LOC"),
                       "1061" = c("razrhon","razor hone","LOC", "STREAM"),
                       "1557" = c("\\(","","LOC"),
                       "218"  = c("sf-oak", "san francisco oakland","LOC"),
                       "240"  = c("sf-oak", "san francisco oakland","LOC"),
                       "1331" = c("rcb", "reinforced concrete box culvert", "MAT"),
                       "1191" = c("pend.","pendleton", "LOC"),
                       "1269" = c("balt co.", "baltimore county ", "LOC"))
  save(ls.Fail.Keys, file = file.path("Data","Input","Dictionaries","ls.Fail.Keys.RData"))
  cat(toJSON(ls.Fail.Keys), file = file.path("Data","Input","Dictionaries","Fail.Keys.json"))
  
  ls.NBI.Keys <- list()
  save(ls.NBI.Keys, file = file.path("Data","Input","Dictionaries","ls.NBI.Keys.RData"))
  cat(toJSON(ls.NBI.Keys), file = file.path("Data","Input","Dictionaries","NBI.Keys.json"))
  

  