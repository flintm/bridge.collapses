library(shiny)
library(Cairo)
library(reshape2)
options(shiny.usecairo=T)
source("SetupEncoding.R")
source("getTheme.R")
source("PlotHazardCurve.R")
source("my.kernel.interp.R")
source("plotBar.R")
load("BridgesDataFrame.RData")

# SETUP AND OPTIONS------------------------------------------------------------
  # Shiny/display: color palettes, limits----------------------------------------
  hazChangeLims <- c(-25,20)
  step          <- 5
  nStops        <- (hazChangeLims[2]-hazChangeLims[1])/step+1
  deltas        <- seq(from       = as.numeric(hazChangeLims[1]/100),
                     to         = as.numeric(hazChangeLims[2]/100),
                     length.out = nStops)
  names(deltas) <- as.character(deltas)

  pal           <- colorFactor(colorsP$Fail, names(colorsP$Fail), ordered = TRUE)
  colors        <- brewer.pal(nStops, "RdBu")
  names(colors) <- as.character(deltas)
  colors["0"]   <- "black"

  # Analysis-------------------------------------------------------------------
  dt       <- 1 # discretization of time in years for integrating hazard
  t        <- seq(1,3000,by=dt) # integration points
  Ts       <- 10^seq(from = 0.1, to = 3, length.out = 50) # collapse return periods
  T.shift  <- 100  # return period value fixed when scaling/shifting hazard curve
  T.nom    <- 100  # nominal return period of collapse-causing flood
  beta.nom <- 1.75 # nominal reliability given a "100-year" flood



# SHINY SERVER ----------------------------------------------------------------
shinyServer(function(input, output){
  # REACTIVES -----------------------------------------------------------------
  # Filtered/displayed collapsed bridges
  filteredBridges <- reactive({
    subset(BridgesDataFrame,  DRAIN_SQKM >= input$drainArea[1]*1000 & 
             DRAIN_SQKM <= input$drainArea[2]*1000 &
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
  
  # limits of bridges in bounds and return periods
  T.lims <- reactive({
    T.lims <- list(x = range(bridgesInBounds()$T_FAIL_D_HECD_USGS, na.rm = T),
                   y = nrow(bridgesInBounds()))
    if(any(is.na(T.lims$x) | is.infinite(T.lims$x))){
      T.lims$x <- range(t)
    }
    T.lims
  })
  
  # Statistics of filtered/displayed bridges
  median.stat <- reactive({
    median.stat <- list(val = median(bridgesInBounds()$T_FAIL_D_HECD_USGS))
    medCol <- apply(col2rgb(colorsP$Fail[input$FailCause]),MARGIN = 1, mean)
    median.stat$col <- rgb(medCol[1]/255,medCol[2]/255,medCol[3]/255)
    median.stat
  })
  
  # Statistics of kernel-smoothed distribution of filtered/displayed bridges
  kernel.stat <- reactive({
    t.int <- t[t<2*T.lims()$x[2]]
    T.kernel.interp <- my.kernel.interp(bridgesInBounds()$T_FAIL_D_HECD_USGS, 
                                        to = 2*T.lims()$x[2], t.interp = t.int)
    kernel.plot <- as.data.frame(x=T.kernel.interp)
    integral    <- sum(kernel.plot$y*dt)
    kernel.plot <- kernel.plot[kernel.plot$x<T.lims()$x[2],]
    kernel.plot$y <- kernel.plot$y/integral*T.lims()$y*100
    kernel.plot$c <- seq(0,1,length.out = nrow(kernel.plot))
    kernel.plot
  })
  
  # TAB 1: MAP OF COLLAPSED BRIDGES ---------------------------------------------
  output$map <- renderLeaflet({
    leaflet(BridgesDataFrame) %>%
      addTiles(group = "OSM (default)") %>%
      setView(lng = -95.85, lat = 37.45, zoom = 3) 
  })
  
  observe({
    leafletProxy("map", data = bridgesInBounds()) %>%
      clearShapes() %>% clearControls()
    leafletProxy("map", data = bridgesInBounds()) %>%
      addCircles(lat = ~LATDD, lng = ~LONGDD, 
                 radius = ~sqrt(DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM)*1000, 
                 color = ~pal(FAIL_CAUS_CODE), 
                 fill = TRUE, opacity = 1, weight = 1, fillOpacity = 0.2, 
                 stroke = FALSE,
                 group = "Bridges", layerId = ~ID) %>%
      addCircleMarkers(lat = ~LATDD, lng = ~LONGDD, 
                       radius = 1, 
                       color = ~pal(FAIL_CAUS_CODE), 
                       fill = TRUE, opacity = 1,
                       weight = 1, 
                       group = "Bridges", layerId = ~ID) %>%
      addLegend("bottomleft", pal = pal, values = ~FAIL_CAUS_CODE,
                title = "Collapse Cause",
                opacity = 1
      )
  })
  
  showBridgePopup <- function(ID, lat, lng) {
    selectedBridge <- BridgesDataFrame[BridgesDataFrame$ID == ID,]
    content <- as.character(tagList(
      tags$h4("Failed bridge ID:",selectedBridge$ID),
      tags$strong(HTML(sprintf("%s, %s - %s",
                               selectedBridge$STATE, 
                               format(selectedBridge$YR_BLT_EST,"%Y"), 
                               format(selectedBridge$YR_FAIL_EST,"%Y")
      ))), 
      tags$br(),
      sprintf("Location: %s", selectedBridge$LOCATION), tags$br(),
      sprintf("Feature under: %s", selectedBridge$FEAT_UND), tags$br(),
      sprintf("Drainage area: %s sq.km", 
              round(selectedBridge$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM)), tags$br(),
      sprintf("Bridge material: %s", selectedBridge$MAT), tags$br(),
      sprintf("Bridge type: %s", selectedBridge$TYPE), tags$br(),
      sprintf("Failure cause: %s", selectedBridge$FAIL_CAUS), tags$br(),
      sprintf("Fail date: %s", tolower(selectedBridge$BOOL_KNOWN_FAIL_DATE)), tags$br(),
      sprintf("Link to tropical cyclone: %s", 
              selectedBridge$COMMENT_LINKED_HURRICANE), tags$br(),
      sprintf("Collapse (total or partial): %s", selectedBridge$FAIL_TYPE), tags$br(),
      sprintf("Comment: %s", selectedBridge$COMMENTS)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = ID)
  }
  
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event) ) return()
    isolate({
      showBridgePopup(event$id, event$lat, event$lng)
    })
  })
  
  # TAB 2: CALCULATING COLLAPSE PROBABILITIES--------------------------------------
  # Histograms of failure return period
  output$histTfail <- renderPlot({
    if (nrow(bridgesInBounds()) < 3) return(NULL)
    # Labels
    data.text <- data.frame(x = c(T.nom+25, 
                                  median.stat()$val + (T.lims()$x[2]-T.lims()$x[1])/10,
                                  0.6*T.lims()$x[2]), 
                            y = c(T.lims()$y/1.5, T.lims()$y/2.5,T.lims()$y/10),
                            label = c(paste("nominal =", T.nom ,"yr design flood"),
                                      paste("collapse median =\n", round(median.stat()$val),"years"),
                                      "collapse kernel-smoothed\ndistribution"),
                            group = labelsP$Dist,
                            hjust = c(0,0,0.5),
                            row.names = c("Nominal","Median","Kernel"))[input$FailData,]
    
    # Histogram plot with nominal, median, and/or kernel-smoothed statistics
    pHist <- ggplot(bridgesInBounds(), aes(x=T_FAIL_D_HECD_USGS)) + 
      geom_histogram(aes(fill=FAIL_CAUS_CODE))+
      scale_fill_manual(values = colorsP$Fail) + guides(fill=FALSE) + 
      labs(title = "Return Periods of Floods\nCausing Bridge Collapse", 
           x = expression(paste(T["R"]," [yr]")), 
           y = "") 
    
    if("Nominal" %in% input$FailData){
      pHist <- pHist + geom_vline(xintercept = T.nom, 
                                  linetype = linesP$Dist["nominal"], 
                                  color = colorsP$Dist["nominal"], 
                                  size = 1) +
        geom_text(data = data.text["Nominal",],aes(x=x,y=y,
                                                   label = label, 
                                                   hjust = hjust), 
                  size = textP$annotate["SHINY"], 
                  color = colorsP$Dist["nominal"])
    }
    if("Median" %in% input$FailData){
      pHist <- pHist + geom_vline(xintercept = median.stat()$val, 
                                  linetype = linesP$Dist["median"], 
                                  color = median.stat()$col, 
                                  size = 1) +
        geom_text(data = data.text["Median",],
                  aes(x=x,y=y,label = label, hjust = hjust), 
                  size = textP$annotate["SHINY"], 
                  color = median.stat()$col)
      
    }
    if("Kernel" %in% input$FailData){
      pHist <- pHist + geom_line(data = kernel.stat(), aes(x=x,y=y, color = c), size = 1.5) + 
        geom_text(data = data.text["Kernel",],
                  aes(x=x,y=y,label = label, hjust = hjust),
                  size = textP$annotate["SHINY"], 
                  color = colorsP$Dist["kernel"]) +
        scale_color_gradientn(colours=as.vector(colorsP$Fail[input$FailCause]), guide=FALSE)
    }
    pHist + getTheme(outputType = "SHINY") +
      theme(axis.text.x   = element_text(color = "black", 
                                         size = textP$reg["SHINY"], 
                                         angle = 90, vjust = 0.5, hjust = 1, 
                                         margin  = margin(0.16,0,0,0, "cm")),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank())
  })
  
  # Absolute value of collapse probabilities
  output$pFail <- renderPlot({
    if(nrow(bridgesInBounds())<3) return()
    d <-  as.character(input$hazChange/100)
    plotBar(bridgesInBounds(), FailData = input$FailData, FailCause = input$FailCause,
            delta = input$hazChange/100, hazCol = colors[d],
            hazSelect = input$hazSelect)
  })
  
  # TAB 3: CHANGE IN COLLAPSE RISK WITH CHANGE IN FLOOD HAZARD---------------------------
  # Reactive text to slider for hazard change
  output$delta <- renderText({
    sgn <- as.character(sign(input$hazChange))
    clr <- colors[as.character(input$hazChange/100)]
    txt <- "Floods of a given return period occur are assumed to occur with "
    txt[2] <- switch(sgn,
                  "0" =  "unchanged frequency.",
                  "1" = paste0('<span style=\"color:', clr,'\">'), 
                  "-1" = paste0('<span style=\"color:', clr,'\">'))
    txt[3] <- switch(sgn,
                     "0" =  "",
                     "1" = paste0("+",abs(input$hazChange),"% more frequency.",'</span>'),
                     "-1" = paste0("-",abs(input$hazChange),"% less frequency.",'</span>'))
    paste0(txt,collapse = "")

  })

  #  hazard curve shifting and scaling 
  output$hazStatic <- renderPlot({
    T.delta <- data.frame(T_0 = Ts,"delta"=1/Ts)
    colnames(T.delta)[2] <- "0"
    PlotHazardCurve(T.delta, STATIC = TRUE,x.limits = c(1,1000),y.limits = c(0.0005,1.01),
                    AXES = "LOG", outputType = "SHINY")
  })
    
  output$haz <- renderPlot({
    d.emph <- as.character(input$hazChange/100)
    T.delta <- switch(as.character(input$hazSelect),
                      "1" = as.data.frame(sapply(deltas, function(d) 1/(Ts*(1-d)), USE.NAMES = TRUE)),
                      "2" = as.data.frame(sapply(deltas, function(d) 1/(Ts-T.shift*d)), USE.NAMES = TRUE))
    T.delta$T_0 <- Ts 
    PlotHazardCurve(T.delta, d.emph = d.emph, nCol = nStops,
                    d.limits = range(deltas),
                    x.limits = c(50,200), y.limits = c(0.002,0.05),
                    AXES = "LOG", outputType = "SHINY")
  })
  
  # change in annual probability of collapse, delta 
  output$anFail <- renderPlot({
    if(nrow(bridgesInBounds())<3) return()
    d <-  as.character(input$hazChange/100)
    plotBar(bridgesInBounds(), FailData = input$FailData, FailCause = input$FailCause,
            DELTA = TRUE, delta = input$hazChange/100, hazCol = colors[d],
            hazSelect = input$hazSelect)
  })
})