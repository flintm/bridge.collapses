library(shiny)
# source("PlotFailedBridgesMapShiny.R")
source("SetupEncoding.R")
source("getTheme.R")
source("PlaceAnnotation.R")
load("df.Fail.NBI.Gage.RData")
BridgesDataFrame <- df.Fail.NBI.Gage
BridgesDataFrame[12,"DATE_P_BEGIN_USGS"] <- "1964-04-04"
BridgesDataFrame[BridgesDataFrame$COMMENT_LINKED_HURRICANE=="","COMMENT_LINKED_HURRICANE"] <- "none"
BridgesDataFrame[BridgesDataFrame$FAIL_TYPE=="","FAIL_TYPE"] <- "U"
BridgesDataFrame$FAIL_TYPE <- factor(BridgesDataFrame$FAIL_TYPE, levels = c("PC","TC","U"),labels = c("partial","total","unknown"))
BridgesDataFrame$SCALE <- BridgesDataFrame$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM
pal <- colorFactor(colorsP$Fail, names(colorsP$Fail))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  filteredBridges <- reactive({
    subset(BridgesDataFrame,  DRAIN_SQKM >= input$drainArea[1] &  DRAIN_SQKM <= input$drainArea[2] &
             FAIL_CAUS_CODE %in% input$FailCause)
  })
  
  bridgesInBounds <- reactive({
    if (is.null(input$map_bounds)) return(BridgesDataFrame[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(filteredBridges(),
           LATDD >= latRng[1] & LATDD <= latRng[2] &
             LONGDD >= lngRng[1] & LONGDD <= lngRng[2])
  })
  
  
  output$map <- renderLeaflet({
    leaflet(BridgesDataFrame) %>%
      addTiles(group = "OSM (default)") %>%
     
      setView(lng = -93.85, lat = 37.45, zoom = 4) #s%>%
  })
  
  observe({
    leafletProxy("map", data = filteredBridges()) %>%
      clearShapes() %>% clearControls()
      # gages
    leafletProxy("map", data = filteredBridges()) %>%
      addCircles(lat = ~LATDD, lng = ~LONGDD, radius = ~sqrt(DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM)*1000, color = ~pal(FAIL_CAUS_CODE), 
                 fill = TRUE, opacity = 1, weight = 1, fillOpacity = 0.2, stroke = FALSE,
                 group = "Bridges", layerId = ~ID) %>%
      addCircleMarkers(lat = ~LATDD, lng = ~LONGDD, radius = 1, color = ~pal(FAIL_CAUS_CODE), fill = TRUE, opacity = 1,
                       weight = 1, group = "Bridges", layerId = ~ID) %>%
      addLegend("bottomleft", pal = pal, values = ~FAIL_CAUS_CODE,
                title = "Collapse Cause",
                opacity = 1
      )
  })
  
  output$scatter <- renderPlot({
    PlotT1T2corrRegress(filteredBridges(),"Collapse Return Period","Max Recorded Return Period",NA, TEXT = NA, 
                                    ALPHA_VAR = NA, REG = TRUE,
                                    SCALE = "LINEAR", LEGEND = FALSE, outputType = "PRINT", ANNOTATE_LOC = "BR", dataType = "T",
                                    SCALE_CORRECTION = 1, JITTER = FALSE, VERBOSE = FALSE, SHAPE_VAR = NA)},
    # ggplot(bridgesInBounds(),aes(x=T_FAIL_D_HECD_USGS,y=T_MAX_D_HECD_USGS))+geom_point(colour='red')},
    height = 200,width = 500)
    # If no zipcodes are in view, don't plot
    # if (nrow(bridgesInBounds()) == 0) return(NULL)
    # print(head(bridgesInBounds()$T_FAIL_D_HECD_USGS))
    # hist(bridgesInBounds()$T_FAIL_D_HECD_USGS,
         # breaks = centileBreaks,
         # main = "Return periods of failure flows",
         # xlab = "T (years)" ,
         # xlim = range(BridgesDataFrame$T_FAIL_D_HECD_USGS)#,
         # col = '#00DD00',
         # border = 'white'
    # )
  # })
  
})