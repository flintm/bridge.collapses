# Custom ontologies used to match (hydraulic) bridge collapses from the 
# NYSDOT Bridge Failure Database to the NBI or other datasets
require(rjson)

## Failure data ------
# FEATURE MAPPING, FIELD NAMES, AND FIELD FORMATS
ls.Fail.Ont <- list(MAT     = c("MAT", "TYPE"),
                   TYPE     = c("TYPE", "MAT"),
                   LOCATION = c("LOCATION"),
                   COUNTY   = "LOCATION",
                   CITY     = "LOCATION",
                   ROAD     = "LOCATION",
                   ROUTE    = "LOCATION",
                   BRIDGE   = c("LOCATION", "LOCATION"),
                   STREAM   = c("FEAT_UND", "LOCATION"),
                   BIN      = "BIN",
                   ID       = "ID")

save(ls.Fail.Ont, file = file.path("Data","Input","Ontologies","ls.Fail.Ont.RData"))
cat(toJSON(ls.Fail.Ont), file = file.path("Data","Input","Ontologies","Fail.Ont.json"))

ls.Fail.Fields <-  list( colNames = c("ID", "BIN", "LOCATION", "FEAT_UND", "STATE_CODE", "TYPE", "MAT", "YR_BLT", "YR_FAIL", 
                                     "DATE_FAIL", "FAIL_TYPE", "FAIL_CAUS", "FATAL", "INJURY", "SOURCE", "COMMENTS"),
                        colClasses = c(ID         = 'c',
                                       BIN        = 'C',
                                       LOCATION   = 'c',
                                       FEAT_UND   = 'c',
                                       STATE_CODE = 'f',
                                       TYPE       = 'c',
                                       MAT        = 'c',
                                       YR_BLT     = 'y',
                                       YR_FAIL    = 'y',
                                       DATE_FAIL  = 'D',
                                       FAIL_TYPE  = 'f',
                                       FAIL_CAUS  = 'c',
                                       FATAL      = 'i',
                                       INJURY     = 'i',
                                       SOURCE     = 'cnp',
                                       COMMENTS   = 'c'))
save(ls.Fail.Fields, file = file.path("Data","Input","Ontologies","ls.Fail.Fields.RData"))
cat(toJSON(ls.Fail.Fields), file = file.path("Data","Input","Ontologies","Fail.Fields.json"))

# Bridge number naming conventions in Fail Data
ls.DOT.Keys <-list("29" = list(ROUTE    = c(PATTERN = "(\\<rt[e.]{0,2} [[:alpha:]]{1,2}\\>(-[[:alnum:]]{1,3})?)|(\\<co[.]? rd[.?] [[:alpha:]]{1}-[[:alnum:]]{1,2}\\>)",
                                            SUBTRN  = "(\\<rt[e.]{0,2} [[:alpha:]]{1,2}\\>(-[[:alnum:]]{1,3})?)|(\\<co[.]? rd[.?] [[:alpha:]]{1}-[[:alnum:]]{1,2}\\>)",
                                            SUB     = "",
                                            MOVE    = "ROUTE_NAME_1",
                                            MVPTRN  = "(\\<[[:alpha:]]{1,2}\\>(-[[:alnum:]]{1,3})?,)|(\\<[[:alpha:]]{1,2}\\>(-[[:alnum:]]{1,3})?$)",
                                            ADDTO   = "ROUTE_TYPE_1",
                                            ADD     = "route")), # Missouri, supplemental rountes
                   # can be A-F,H,J,K,M-P,T,U,Y,Z or AA-FF,etc., AA-AZ, BA, R(A-Z mins GILQSX). No bypass but Spur Routes
                   # they are state routes but sometimes called county road
                   "39" = list(LOCATION = c(PATTERN = "\\<[[:alnum:]]{1,3}-[[:alnum:]]{1,5}[[:punct:]]?[[:alnum:]]{0,3}[[:punct:]]?[[:alnum:]]{0,3}\\>",
                                            SUBPTRN = "\\<[[:alnum:]]{1,3}-[[:alnum:]]{1,5}[[:punct:]]?[[:alnum:]]{0,3}[[:punct:]]?[[:alnum:]]{0,3}\\>",
                                            SUB     = "",
                                            MOVE    = "BRIDGE_NAME",
                                            MVPTRN  = "\\<[[:alnum:]]{1,3}-[[:alnum:]]{1,5}[[:punct:]]?[[:alnum:]]{0,3}[[:punct:]]?[[:alnum:]]{0,3}\\>",
                                            ADDTO   = NA,
                                            ADD     = NA)), # Ohio
                   "47" = list(LOCATION = c(PATTERN = "\\<[[:alnum:]]{1,3}-[[:alnum:]]{1,5}[[:punct:]]?[[:alnum:]]{0,3}[[:punct:]]?[[:alnum:]]{0,3}\\>",
                                            SUBPTRN = "\\<[[:alnum:]]{1,3}-[[:alnum:]]{1,5}[[:punct:]]?[[:alnum:]]{0,3}[[:punct:]]?[[:alnum:]]{0,3}\\>",
                                            SUB     = "",
                                            MOVE    = "BRIDGE_NAME",
                                            MVPTRN  = "\\<[[:alnum:]]{1,3}-[[:alnum:]]{1,5}[[:punct:]]?[[:alnum:]]{0,3}[[:punct:]]?[[:alnum:]]{0,3}\\>",
                                            ADDTO   = NA,
                                            ADD     = NA),
                               BRIDGE   = c(PATTERN = "\\<fau[[:space:]]?[[:alpha:]]{0,3}[[:space:]]?[[:digit:]]{1,5}\\>",
                                            SUBPTRN = "\\<fau[[:space:]]?[[:alpha:]]{0,3}[[:space:]]?[[:digit:]]{1,5}\\>",
                                            SUB     = "",
                                            MOVE    = "BRIDGE_NAME",
                                            MVPTRN  = "\\<fau[[:space:]]?[[:alpha:]]{0,3}[[:space:]]?[[:digit:]]{1,5}\\>",
                                            ADDTO   = NA,
                                            ADD     = NA)), # Tenessee
                   "05" = list(LOCATION = c(PATTERN = "[[:alpha:]]{1,2}[[:space:]]?[[:digit:]]{1,4}-[[:graph:]]{1,}( [0-9\\(\\)]{3,4})?",
                                            SUBPTRN = "-[[:graph:]]{1,}( [0-9\\(\\)]{3,4})?",
                                            SUB     = "",
                                            MOVE    = "BRIDGE_NAME",
                                            MVPTRN  = "[[:alpha:]]{1,2}[[:space:]]?[[:digit:]]{1,4}-[[:graph:]]{1,}( [0-9\\(\\)]{3,4})?",
                                            ADDTO   = NA,
                                            ADD     = "")),# Arkansas
                   # ROUTE    = c(PATTERN = "[[:alpha:]]{1,2}[[:space:]]?[[:digit:]]{1,4}",
                   #              SUBPTRN = NA,
                   #              SUB     = NA,
                   #              MOVE    = NA,
                   #              ADDTO   = "ROUTE_TYPE_1",
                   #              ADD     = "county highway")), # Arkansas
                   "19" = list(LOCATION = c(PATTERN = "[[:alpha:]]{1,2}[[:space:]]?[[:digit:]]{1,4}-[[:graph:]]{1,}",
                                            SUBPTRN = "-[[:graph:]]{1,}",
                                            SUB     = "",
                                            MOVE    = "BRIDGE_NAME",
                                            MVPTRN  = "[[:alpha:]]{1,2}[[:space:]]?[[:digit:]]{1,4}-[[:graph:]]{1,}",
                                            ADDTO   = NA,
                                            ADD     = NA))) # Iowa
save(ls.DOT.Keys, file = file.path("Data","Input","Ontologies","ls.DOT.Keys.RData"))
cat(toJSON(ls.DOT.Keys), file = file.path("Data","Input","Ontologies","DOT.Keys.json"))

## NBI Data --------
ls.NBI.Ont <- list(STFIPS   = c("STFIPS", "STATE_CODE_001", "ITEM1", "STATE_CODE_001", "ITEM98A", "OTHER_STATE_CODE_98A"),
                   MAT      = c("STRUCTURE_KIND_043A","ITEM43A"),
                   TYPE     = c( "STRUCTURE_TYPE_043B","ITEM43B"),
                   LOCATION = c( "LOCATION_009","ITEM9"),
                   COUNTY   = c("COUNTY_CODE_003","ITEM3" ),
                   CITY     = c( "LOCATION_009","ITEM9"),
                   ROAD     = c('FACILITY_CARRIED_007', 'ROUTE_NUMBER_005D',"ITEM7", "ITEM5B" ),
                   ROUTE    = c('ROUTE_NUMBER_005D',  'ROUTE_PREFIX_005B',"ITEM5D", "ITEM5B"),
                   BRIDGE   = c('FACILITY_CARRIED_007',"ITEM7" ),
                   STREAM   = c( 'FEATURES_DESC_006A', 'NAVIGATION_038',"ITEM6A",'ITEM38'),
                   BIN      = c( 'STRUCTURE_NUMBER_008',"ITEM8"),
                   ID       = c("OBJECTID_new","ObjectID_new","ITEM8", 'STRUCTURE_NUMBER_008'))
save(ls.NBI.Ont, file = file.path("Data","Input","Ontologies","ls.NBI.Ont.RData"))
cat(toJSON(ls.NBI.Ont), file = file.path("Data","Input","Ontologies","NBI.Ont.json"))

ls.NBI.Fields = list(colNamesI = c('STFIPS',   'REGION',   'ITEM8',    'ITEM5A',  'ITEM5B',  'ITEM5C',  'ITEM5D',    'ITEM5E',  'ITEM2',  #1
                                   'ITEM3',    'ITEM4',    'ITEM6A',   'ITEM6B',  'ITEM7',   'ITEM9',   'ITEM10',    'ITEM11',  'ITEM12', #2
                                   'ITEM13A',  'ITEM13B',  'ITEM16',   'ITEM17',  'ITEM19',  'ITEM20',  'ITEM21',    'ITEM22',  'ITEM26', #3
                                   'ITEM27',   'ITEM28A',  'ITEM28B',  'ITEM29',  'ITEM30',  'ITEM31',  'ITEM32',    'ITEM33',  'ITEM34', #4
                                   'ITEM35',   'ITEM36A',  'ITEM36B',  'ITEM36C', 'ITEM36D', 'ITEM37',  'ITEM38',    'ITEM39',  'ITEM40', #5
                                   'ITEM41',   'ITEM42A',  'ITEM42B',  'ITEM43A', 'ITEM43B', 'ITEM44A', 'ITEM44B',   'ITEM45',  'ITEM46', #6
                                   'ITEM47',   'ITEM48',   'ITEM49',   'ITEM50A', 'ITEM50B', 'ITEM51',  'ITEM52',    'ITEM53',  'ITEM54A',#7
                                   'ITEM54B',  'ITEM55A',  'ITEM55B',  'ITEM56',  'ITEM58',  'ITEM59',  'ITEM60',    'ITEM61',  'ITEM62', #8
                                   'ITEM63',   'ITEM64',   'ITEM65',   'ITEM66',  'ITEM67',  'ITEM68',  'ITEM69',    'ITEM70',  'ITEM71', #9
                                   'ITEM72',   'ITEM75A',  'ITEM75B',  'ITEM76',  'ITEM90',  'ITEM91',  'ITEM92A',   'ITEM92B', 'ITEM92C',#10
                                   'ITEM93A',  'ITEM93B',  'ITEM93C',  'ITEM94',  'ITEM95',  'ITEM96',  'ITEM97',    'ITEM98A', 'ITEM98B',#11
                                   'ITEM99',   'ITEM100',  'ITEM101',  'ITEM102', 'ITEM103', 'ITEM104', 'ITEM105',   'ITEM106', 'ITEM107',#12
                                   'ITEM108A', 'ITEM108B', 'ITEM108C', 'ITEM109', 'ITEM110', 'ITEM111', 'ITEM112',   'ITEM113', 'ITEM114',#13
                                   'ITEM115',  'ITEM116',  'FUNDED',   'FEDERAL', 'WO',      'DT',      'WO_2',      'STAT',    'SR1',    #14
                                   'SR2',      'EXTRA',    'STATUS',   'DATE',    'LONGDD',  'LATDD',   'coords.x1', 'coords.x2','X',     #15
                                   'Y',        'OBJECTID_new','ITEM1', 'ITEM2'),
                     colnamesN  = c('001',   '002',   '008',    '005A',  '005B',  '005C',  '005D',    '005E',  '002',  #1
                                    '003',   '004',   '006A',   '006B',  '007',   '009',   '010',    '011',  '012', #2
                                    '013A',  '013B',  '016',   '017',  '019',  '020',  '021',    '022',  '026', #3
                                    '027',   '028A',  '028B',  '029',  '030',  '031',  '032',    '033',  '034', #4
                                    '035',   '036A',  '036B',  '036C', '036D', '037',  '038',    '039',  '040', #5
                                    '041',   '042A',  '042B',  '043A', '043B', '044A', '044B',   '045',  '046', #6
                                    '047',   '048',   '049',   '050A', '050B', '051',  '052',    '053',  '054A',#7
                                    '054B',  '055A',  '055B',  '056',  '058',  '059',  '060',    '061',  '062', #8
                                    '063',   '064',   '065',   '066',  '067',  '068',  '069',    '070',  '071', #9
                                    '072',   '075A',  '075B',  '076',  '090',  '091',  '092A',   '092B', '092C',#10
                                    '093A',  '093B',  '093C',  '094',  '095',  '096',  '097',    '098A', '098B',#11
                                    '099',   '100',  '101',  '102', '103', '104', '105',   '106', '107',#12
                                    '108A', '108B', '108C', '109', '110', '111', '112',   '113', '114',#13
                                    '115',  '116',  'FUNDED',   'FEDERAL', 'WO',      'DT',      'WO_2',      'STAT',    'SR1',    #14
                                    'SR2',      'EXTRA',    'STATUS',   'DATE',    'LONGDD',  'LATDD',   'coords.x1', 'coords.x2','X',     #15
                                    'Y',        'OBJECTID_new','001', '002'),
                     colNamesF  = c('STATE_CODE_001', 'REGION', 'STRUCTURE_NUMBER_008', 'RECORD_TYPE_005A', 'ROUTE_PREFIX_005B', 'SERVICE_LEVEL_005C', 'ROUTE_NUMBER_005D', 'DIRECTION_005E', 'HIGHWAY_DISTRICT_002', 
                                    'COUNTY_CODE_003', 'PLACE_CODE_004', 'FEATURES_DESC_006A', 'CRITICAL_FACILITY_006B', 'FACILITY_CARRIED_007', 'LOCATION_009', 'MIN_VERT_CLR_010', 'KILOPOINT_011', 'BASE_HWY_NETWORK_012', 
                                    'LRS_INV_ROUTE_013A', 'SUBROUTE_NO_013B', 'LAT_016', 'LONG_017', 'DETOUR_KILOS_019', 'TOLL_020', 'MAINTENANCE_021', 'OWNER_022', 'FUNCTIONAL_CLASS_026', 
                                    'YEAR_BUILT_027', 'TRAFFIC_LANES_ON_028A', 'TRAFFIC_LANES_UND_028B', 'ADT_029', 'YEAR_ADT_030', 'DESIGN_LOAD_031', 'APPR_WIDTH_MT_032', 'MEDIAN_CODE_033', 'DEGREES_SKEW_034', 
                                    'STRUCTURE_FLARED_035', 'RAILINGS_036A', 'TRANSITIONS_036B', 'APPR_RAIL_036C', 'APPR_RAIL_END_036D', 'HISTORY_037', 'NAVIGATION_038', 'NAV_VERT_CLR_MT_039', 'NAV_HORR_CLR_MT_040', 
                                    'OPEN_CLOSED_POSTED_041', 'SERVICE_ON_042A', 'SERVICE_UND_042B', 'STRUCTURE_KIND_043A', 'STRUCTURE_TYPE_043B', 'APPR_KIND_044A', 'APPR_TYPE_044B', 'MAIN_UNIT_SPANS_045', 'APPR_SPANS_046', 
                                    'HORR_CLR_MT_047', 'MAX_SPAN_LEN_MT_048', 'STRUCTURE_LEN_MT_049', 'LEFT_CURB_MT_050A', 'RIGHT_CURB_MT_050B', 'ROADWAY_WIDTH_MT_051', 'DECK_WIDTH_MT_052', 'VERT_CLR_OVER_MT_053', 'VERT_CLR_UND_REF_054A', 
                                    'VERT_CLR_UND_054B', 'LAT_UND_REF_055A', 'LAT_UND_MT_055B', 'LEFT_LAT_UND_MT_056', 'DECK_COND_058', 'SUPERSTRUCTURE_COND_059', 'SUBSTRUCTURE_COND_060', 'CHANNEL_COND_061', 'CULVERT_COND_062', 
                                    'OPR_RATING_METH_063', 'OPERATING_RATING_064', 'INV_RATING_METH_065', 'INVENTORY_RATING_066', 'STRUCTURAL_EVAL_067', 'DECK_GEOMETRY_EVAL_068', 'UNDCLRENCE_EVAL_069', 'POSTING_EVAL_070', 'WATERWAY_EVAL_071', 
                                    'APPR_ROAD_EVAL_072', 'WORK_PROPOSED_075A', 'WORK_DONE_BY_075B', 'IMP_LEN_MT_076', 'DATE_OF_INSPECT_090', 'INSPECT_FREQ_MONTHS_091', 'FRACTURE_092A', 'UNDWATER_LOOK_SEE_092B', 'SPEC_INSPECT_092C', 
                                    'FRACTURE_LAST_DATE_093A', 'UNDWATER_LAST_DATE_093B', 'SPEC_LAST_DATE_093C', 'BRIDGE_IMP_COST_094', 'ROADWAY_IMP_COST_095', 'TOTAL_IMP_COST_096', 'YEAR_OF_IMP_097', 'OTHER_STATE_CODE_098A', 'OTHER_STATE_PCNT_098B', 
                                    'OTHR_STATE_STRUC_NO_099', 'STRAHNET_HIGHWAY_100', 'PARALLEL_STRUCTURE_101', 'TRAFFIC_DIRECTION_102', 'TEMP_STRUCTURE_103', 'HIGHWAY_SYSTEM_104', 'FEDERAL_LANDS_105', 'YEAR_RECONSTRUCTED_106', 'DECK_STRUCTURE_TYPE_107', 
                                    'SURFACE_TYPE_108A', 'MEMBRANE_TYPE_108B', 'DECK_PROTECTION_108C', 'PERCENT_ADT_TRUCK_109', 'NATIONAL_NETWORK_110', 'PIER_PROTECTION_111', 'BRIDGE_LEN_IND_112', 'SCOUR_CRITICAL_113', 'FUTURE_ADT_114', 
                                    'YEAR_OF_FUTURE_ADT_115', 'MIN_NAV_CLR_MT_116', rep(NA_character_, 7),
                                    rep(NA_character_, 4), 'LONGDD', 'LATDD',rep(NA_character_,2),'X', 
                                    'Y', 'OBJECTID_new', rep(NA_character_,2)),                      
                     colClasses = c('stfips', 'f',        'c',        'f',       'f',       'f',       'c',          'f',       'f',     #1
                                    'fips',   'f',        'c',        NA,        'c',       'c',       'n100',       'n1000',   'l',     #2
                                    'c',      'c',        'n',        'n',       'i',       'f',       'f',          'f',       'f',     #3
                                    'y',      'i',        'i',        'i',       'y',       'f',       'n',          'f',       'i',     #4
                                    'l',      'l',        'l',        'l',       'l',       'f',       'l',          'n10',     'n10',   #5
                                    'f',      'f',        'f',        'fi',      'fi',      'f',       'f',          'i',       'i',     #6
                                    'n10',    'n10',      'n10',      'n10',     'n10',     'n10',     'n10',        'n100',    'f',     #7
                                    'n',      'f',        'n10',      'n10',     'f',       'f',       'f',          'f',       'f',     #8
                                    'f',      'n10',      'f',        'n10',     'f',       'f',       'f',          'f',       'f',     #9
                                    'f',      'f',        'f',        'n10',     'mmyy',    'i',       'c',          'c',       'c',     #10
                                    'mmyy',   'mmyy',     'mmyy',     'i',       'i',       'i',       'y',          'stfips',  'n',     #11
                                    'c',      'f',        'f',        'f',       'f',       'l',       'f',          'y',       'f',     #12
                                    'f',      'f',        'f',        'n100',    'l',       'f',       'yn',         'f',       'n',     #13
                                    'y',      'n10',      'yn',       'yn',      'f',       'f',       'f',          'f',       'f',     #14
                                    'f',      'f',        'f',        'mmyy',    'n',       'n',       NA,           NA,        'n',     #15
                                    'n',      'c',        'stfips',   'f')) 
names(ls.NBI.Fields[["colClasses"]]) <- ls.NBI.Fields[["colNames"]]
save(ls.NBI.Fields, file = file.path("Data","Input","Ontologies","ls.NBI.Fields.RData"))
cat(toJSON(ls.NBI.Fields), file = file.path("Data","Input","Ontologies","NBI.Fields.json"))

## MAPPING GENERAL BRIDGE TERMS TO NBI CLASSIFICATIONS
# ITEM5B ROUTE SIGNING PREFIX -------------------
#    1 INTERSTATE HIGHWAY
#    2 US NUMBERED HIGHWAY
#    3 STATE HIGHWAY
#    4 COUNTY HIGHWAY
#    5 CITY STREET
#    6 FEDERAL LANDS ROAD
#    7 STATE LANDS ROAD
#    8 OTHER
ls.ITEM5B.Ont <- list("1" = "interstate",
                      "2" = "usHighway",
                      "3" = c("stateHighway", "stateRoute", "stateSecondary", "route", "freeway", "highway", "thruway", "state"),
                      "4" = c("countyRoad", "countyHighway"),
                      "5" = c("avenue", "crossing", "court", "drive", "lane", "road", "street","parkway","connector","cityRoute"),
                      "6" = c("nfs", "parkRoad"),
                      "7" = c("stateLands", "pass"),
                      "8" = c("privateRoad", "tempRoute"))
load(file.path("Data","Input","Dictionaries","ls.RoadKeys.RData"))

# will also work for RouteKeys, which is a subset of RoadKeys
ls.ITEM5B.OntRev <- sapply(names(ls.RoadKeys), function(Ont) substr(names(unlist(ls.ITEM5B.Ont))[unlist(ls.ITEM5B.Ont)==Ont],1,1))

save(ls.ITEM5B.Ont, file = file.path("Data","Input","Ontologies","ls.ITEM5B.Ont.RData"))
cat(toJSON(ls.ITEM5B.Ont), file = file.path("Data","Input","Ontologies","ITEM5B.Ont.json"))
save(ls.ITEM5B.OntRev, file = file.path("Data","Input","Ontologies","ls.ITEM5B.OntRev.RData"))
cat(toJSON(ls.ITEM5B.OntRev), file = file.path("Data","Input","Ontologies","ITEM5B.OntRev.json"))

# ITEM 43A BRIDGE MATERIALS -----------------
# The first digit indicates the kind of material and/or design and shall
# be coded using one of the following codes:
# Code             Description
# 1                Concrete
# 2                Concrete continuous
# 3                Steel
# 4                Steel continuous
# 5                Prestressed concrete *
# 6                Prestressed concrete continuous *
# 7                Wood or Timber
# 8                Masonry
# 9                Aluminum, Wrought Iron, or Cast Iron
# 0                Other
# * Post-tensioned concrete should be coded as prestressed concrete.
ls.ITEM43A.Ont <- list("1"   = c("conc", "simple"), 
                       "2"   = c("conc", "cont"),
                       "1_2" = "conc",
                       "3"   = c("steel", "simple"),
                       "4"   = c("steel", "cont"),
                       "3_4" = "steel",
                       "5"   = c("pc", "simple"),
                       "6"   = c("pc", "cont"),
                       "6_7" = "pc",
                       "7"   = "wood",
                       "8"   = "masonry",
                       "9"   = "metal",
                       "10"  = "other")

save(ls.ITEM43A.Ont, file = file.path("Data","Input","Ontologies","ls.ITEM43A.Ont.RData"))
cat(toJSON(ls.ITEM43A.Ont), file = file.path("Data","Input","Ontologies","ITEM43A.Ont.json"))

# Item 43B - BRIDGE STRUCTURE TYPE, MAIN SPAN(S) -----------------
# The second and third digits indicate the predominant type of design
# and/or type of construction and shall be coded using one of the
# following codes:
#
#  Code    Description
#  01      Slab
#  02      Stringer/Multi-beam or Girder
#  03      Girder and Floorbeam System
#  04      Tee Beam
#  05      Box Beam or Girders - Multiple
#  06      Box Beam or Girders - Single or Spread
#  07      Frame (except frame culverts)
#  08      Orthotropic
#  09      Truss - Deck
#  10      Truss - Thru
#  11      Arch - Deck
#  12      Arch - Thru 
#  13      Suspension
#  14      Stayed Girder (cable-stayed)
#  15      Movable - Lift
#  16      Movable - Bascule
#  17      Movable - Swing
#  18      Tunnel
#  19      Culvert (includes frame culverts)
#  20 *    Mixed types
#  21      Segmental Box Girder
#  22      Channel Beam
#  00      Other
#    * Applicable only to approach spans - Item 44     
# Entries are listed out of NBI order for consistency with TypeKeys
ls.ITEM43B.Ont <- list("1"     = "slab", 
                       "3"     = "girdFB",
                       "4"     = "tBeam", 
                       "7"     = "frame", 
                       "8"     = "ortho", 
                       "9"     = "trussD",
                       "10"    = "trussT",
                       "9_10"  = "truss",
                       "11"    = "archD", 
                       "12"    = "archT",
                       "11_12" = "arch",
                       "13"    = "susp", 
                       "14"    = "stayGird", 
                       "15"    = "mvLift",
                       "16"    = "mvBasc", 
                       "17"    = "mvSwing", 
                       "18"    = "tunnel",
                       "19"    = "culvert", 
                       "20"    = "mixed", 
                       "21"    = "seg",
                       "22"    = "chnlBm", 
                       "0"     = "other",
                       "5"     = "boxMult", # "box beam multiple", "box girder multiple", "multiple box beam", "multiple box girder", 
                       "6"     = "boxSing", # "box beam single", "box beam spread", "box girder single", "box girder spread",
                       "5_6"   = "box",
                       "2"     = "string") # "stringer", "multi beam", "girder"

load(file.path("Data","Input","Dictionaries","ls.TypeKeys.RData"))
ls.ITEM43B.OntRev <- sapply(names(ls.TypeKeys), function(Ont) names(unlist(ls.ITEM43B.Ont))[unlist(ls.ITEM43B.Ont)==Ont])

save(ls.ITEM43B.Ont, file = file.path("Data","Input","Ontologies","ls.ITEM43B.Ont.RData"))
cat(toJSON(ls.ITEM43B.Ont), file = file.path("Data","Input","Ontologies","ITEM43B.Ont.json"))
save(ls.ITEM43B.OntRev, file = file.path("Data","Input","Ontologies","ls.ITEM43B.OntRev.RData"))
cat(toJSON(ls.ITEM43B.OntRev), file = file.path("Data","Input","Ontologies","ITEM43B.OntRev.json"))
