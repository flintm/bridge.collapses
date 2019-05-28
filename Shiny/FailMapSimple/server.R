library(shiny)
library(Cairo)
options(shiny.usecairo=T)
source("../../Plotting/SetupEncoding.R")
source("../../Plotting/getTheme.R")
source("../../Plotting/PlaceAnnotation.R")
source("../../Plotting/PlotT1T2corrRegress.R")
source("../../Plotting/PlotHazardCurve.R")
source("my.kernel.interp.R")
load("../../Data/df.Fail.NBI.Gage.Active.RData")

# Augment data frame as necessary--------
BridgesDataFrame <- df.Fail.NBI.Gage[rowsToView,]
BridgesDataFrame[12,"DATE_P_BEGIN_USGS"] <- "1964-04-04"
BridgesDataFrame[BridgesDataFrame$COMMENT_LINKED_HURRICANE=="","COMMENT_LINKED_HURRICANE"] <- "none"
BridgesDataFrame[BridgesDataFrame$FAIL_TYPE=="","FAIL_TYPE"] <- "U"
BridgesDataFrame$FAIL_TYPE <- factor(BridgesDataFrame$FAIL_TYPE, levels = c("PC","TC","U"),labels = c("partial","total","unknown"))
BridgesDataFrame$SCALE <- BridgesDataFrame$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM
BridgesDataFrame$DRAIN_SQKM <- BridgesDataFrame$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM
BridgesDataFrame$ALPHA <- TRUE
BridgesDataFrame$T1    <- BridgesDataFrame$T_FAIL_D_HECD_USGS
BridgesDataFrame$T2    <- BridgesDataFrame$T_MAX_D_HECD_USGS
BridgesDataFrame$BOOL_REGULATION      <- factor(BridgesDataFrame$BOOL_REGULATION, 
                                                levels = c(TRUE,FALSE, NA), labels = labelsP$Reg, exclude = NULL)

dt    <- 1
t <- seq(1,3000,by=dt)
Ts    <- 10^seq(from = 0.1, to = 3, length.out = 50)
Tshift <- 100
t.F   <- 100  # return period of collapse-causing flood
beta  <- 1.75 # nominal reliability given a "100-year" flood

pal <- colorFactor(colorsP$Fail, names(colorsP$Fail), ordered = TRUE)
colors <- brewer.pal(9, "RdBu")
names(colors) <- as.character(seq(-0.2,0.2,length.out = 9))

# server setup------------
shinyServer(function(input, output) {
  # Define server logic to make plots and map consistent--------
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
    REACT <- TRUE
    subset(filteredBridges(),
           LATDD >= latRng[1] & LATDD <= latRng[2] &
             LONGDD >= lngRng[1] & LONGDD <= lngRng[2])
  })
  
  # Map-------
  output$map <- renderLeaflet({
    leaflet(BridgesDataFrame) %>%
      addTiles(group = "OSM (default)") %>%
      setView(lng = -95.85, lat = 37.45, zoom = 3) 
  })
  observe({
    leafletProxy("map", data = bridgesInBounds()) %>%
      clearShapes() %>% clearControls()
    leafletProxy("map", data = bridgesInBounds()) %>%
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
  
  showBridgePopup <- function(ID, lat, lng) {
    selectedBridge <- BridgesDataFrame[BridgesDataFrame$ID == ID,]
    content <- as.character(tagList(
      tags$h4("Failed bridge ID:",selectedBridge$ID),
      tags$strong(HTML(sprintf("%s, %s - %s",
                               selectedBridge$STATE, format(selectedBridge$YR_BLT_EST,"%Y"), format(selectedBridge$YR_FAIL_EST,"%Y")
      ))), 
      tags$br(),
      sprintf("Location: %s", selectedBridge$LOCATION), tags$br(),
      sprintf("Feature under: %s", selectedBridge$FEAT_UND), tags$br(),
      sprintf("Drainage area: %s sq.km", round(selectedBridge$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM)), tags$br(),
      sprintf("Bridge material: %s", selectedBridge$MAT), tags$br(),
      sprintf("Bridge type: %s", selectedBridge$TYPE), tags$br(),
      sprintf("Failure cause: %s", selectedBridge$FAIL_CAUS), tags$br(),
      sprintf("Fail date: %s", tolower(selectedBridge$BOOL_KNOWN_FAIL_DATE)), tags$br(),
      sprintf("Link to tropical cyclone: %s", selectedBridge$COMMENT_LINKED_HURRICANE), tags$br(),
      sprintf("Collapse (total or partial): %s", selectedBridge$FAIL_TYPE), tags$br(),
      sprintf("Comment: %s", selectedBridge$COMMENTS)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = ID)
  }
  

  #  hazard curve shifting and scaling -----------
  output$haz <- renderPlot({
    deltas <- sort(c(0, input$hazChange/100))
    T.delta <- switch(as.character(input$hazSelect),
                      "1" = as.data.frame(sapply(deltas, function(d) 1/(Ts*(1-d)))),
                      "2" = as.data.frame(sapply(deltas, function(d) 1/(Ts-Tshift*d))))
    colnames(T.delta) <- as.character(deltas)
    T.delta$T_0 <- Ts 
    PlotHazardCurve(T.delta, AXES = "LOG")
  })
  
  # Histograms of failure return period--------
  output$histTfail <- renderPlot({
    if (nrow(bridgesInBounds()) < 3) return(NULL)
    med <- median(bridgesInBounds()$T_FAIL_D_HECD_USGS)
    lims <- range(bridgesInBounds()$T_FAIL_D_HECD_USGS, na.rm = T)
    if(any(is.na(lims) | is.infinite(lims))){
      lims <- c(0,2000)
      warning('Problem in histogram')
    }
    h = nrow(bridgesInBounds())
    t.int <- t[t<2*lims[2]]
    T.kernel.interp <- my.kernel.interp(bridgesInBounds()$T_FAIL_D_HECD_USGS, 
                                        to = 2*lims[2], t.interp = t.int)
    kernel.plot <- as.data.frame(T.kernel.interp)
    integral    <- sum(kernel.plot$y*dt)
    kernel.plot <- kernel.plot[kernel.plot$x<lims[2],]
    kernel.plot$y <- kernel.plot$y/integral*h*100
    
    data.text <- data.frame(x = c(t.F+25,med + (lims[2]-lims[1])/10,0.6*lims[2]), 
                             y = c(h/1.5, h/2.5,h/10),
                            label = c("nominal = 100 yrs",
                                      paste("collapse median =", round(med),"years"),
                                      "collapse kernel"),
                            group = labelsP$Dist)
    pTheme <- getTheme("SHINY", FALSE) +
      theme(axis.text.x   = element_text(color = "black", size = textP$reg["SHINY"], 
                                         angle = 90, vjust = 0.5, hjust = 1, margin  = margin(0.16,0,0,0, "cm")),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank())
    ggplot(bridgesInBounds(), aes(x=T_FAIL_D_HECD_USGS)) + 
      geom_histogram(aes(fill=FAIL_CAUS_CODE))+
      scale_fill_manual(values = colorsP$Fail) + guides(fill=FALSE) + pTheme + 
      labs(title = "Collapse Return Periods", x = expression(paste(T["R"]," [yr]")), y = "")+
      geom_vline(xintercept = t.F, linetype = linesP$Dist["nominal"], color = colorsP$Dist["nominal"], size = 1) +
      geom_vline(xintercept = med, linetype = linesP$Dist["median"], color = colorsP$Dist["median"], size = 1) + 
      geom_line(data = kernel.plot, aes(x=x,y=y), linetype = linesP$Dist["kernel"], color = colorsP$Dist["kernel"], size = 1) +
      geom_text(data = data.text,aes(x=x,y=y,label = label, color = group),hjust=0) + scale_color_manual(values = colorsP$Dist, guide=FALSE)
      
  })
 
  # no hazard change collapse probabilities ----
  output$pFail <- renderPlot({
    if(nrow(bridgesInBounds())<3) return()
    lims <- range(bridgesInBounds()$T_FAIL_D_HECD_USGS, na.rm = T)
    if(any(is.na(lims) | is.infinite(lims))){
      lims <- c(0,2000)
    }
    t.int <- t[t<2*lims[2]]
    T.kernel.interp <- my.kernel.interp(bridgesInBounds()$T_FAIL_D_HECD_USGS, 
                                        to = 2*lims[2], 
                                        t.interp = t.int)
    T.kernel.interp$y <- T.kernel.interp$y/sum(T.kernel.interp$y*dt)
    pF0 <- data.frame(x = labelsP$Dist,
                     group = labelsP$Dist,
                     "T" = c(t.F,
                             median(bridgesInBounds()$T_FAIL_D_HECD_USGS),
                             NA),
                     ymin = rep(0,3),
                     row.names = labelsP$Dist)
    pF0$ymax <- c(1/t.F*pnorm(-beta), 
                 1/pF0["median","T"], 
                 NA)
    pF0["kernel","ymax"] <- sum(T.kernel.interp$y/(T.kernel.interp$x)^2*dt)
    pF0$label <- signif(pF0$ymax,2)
    ggplot(data = pF0, aes(x=x,ymin=ymin,ymax=ymax, group = group)) + 
      geom_linerange(aes(linetype = group, color = group), size = 1) + scale_linetype_manual(values = linesP$Dist, guide = FALSE) + 
      scale_color_manual(values = colorsP$Dist, guide = FALSE) + ggtitle("Annual probability of collapse") +
      # ylim(c(0,0.1))+
      geom_text(aes(y = ymax, label = label), hjust=0) + 
      coord_flip()+ getTheme("SHINY") +
      theme(axis.text.x   = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank())
  })
  # annual probability of collapse  WITH hazard change, delta---------
  output$anFail <- renderPlot({
    if(nrow(bridgesInBounds())<3) return()
    d <-  as.character(input$hazChange/100)
    lims <- range(bridgesInBounds()$T_FAIL_D_HECD_USGS, na.rm = T)
    if(any(is.na(lims) | is.infinite(lims))){
      lims <- c(0,2000)
    }
    
    t.int <- t[t<2*lims[2]]
    T.kernel.interp <- my.kernel.interp(bridgesInBounds()$T_FAIL_D_HECD_USGS, 
                                        to = 2*lims[2], 
                                        t.interp = t.int)
    T.kernel.interp$y <- T.kernel.interp$y/sum(T.kernel.interp$y*dt)

    pF <- data.frame(x = labelsP$Dist,
                     group = labelsP$Dist,
                     "T" = c(t.F,
                             median(bridgesInBounds()$T_FAIL_D_HECD_USGS),
                             NA),
                     ymin = rep(0,3),
                     row.names = labelsP$Dist)
    pF$none <- c(1/t.F*pnorm(-beta), 
                    1/pF["median","T"], 
                    sum(T.kernel.interp$y/(t.int)^2*dt))
    pF[,d] <- c(switch(as.character(input$hazSelect),
                       "1" = 1/(t.F*(1-input$hazChange/100))*pnorm(-beta),
                       "2" = 1/(t.F-Tshift*input$hazChange/100)*pnorm(-beta)),
                switch(as.character(input$hazSelect),
                       "1" = 1/(pF["median","T"]*(1-input$hazChange/100)),
                       "2" = 1/(pF["median","T"]-Tshift*input$hazChange/100)),
                switch(as.character(input$hazSelect),
                       "1" = sum(T.kernel.interp$y/(T.kernel.interp$x*(1-input$hazChange/100))^2*dt),
                       "2" = sum(T.kernel.interp$y/(T.kernel.interp$x-Tshift*input$hazChange/100)^2*dt)))
    pF$change <- (pF[,d] - pF$none)/pF$none*100
    pF$label <- signif(pF$change,2)

    p <- ggplot(data = pF,aes(x=x,y=change)) + geom_col() + geom_hline(yintercept  = 0) + ylim(c(-60,60))+
       getTheme() + geom_text(aes(y = change, label = label), hjust=0) + 
      ggtitle('Change in annual collapse probability given hazard change [%]')+
      theme(#axis.text.x   = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank())
    if(input$hazChange!=0){
      p + geom_hline(yintercept = input$hazChange, color = colors[d]) + coord_flip() 
    }
    else p + coord_flip()
  })#, 
  # height = 200,
  # width = 500)
  
  # scatter plot (not used)
  output$scatter <- renderPlot({
    if (nrow(bridgesInBounds()) <3) return(NULL)
    PlotT1T2corrRegress(bridgesInBounds(),
                        "Collapse Return Period [yr]","Max Recorded Return Period [yr]",
                        NA, TEXT = character(0), 
                        ALPHA_VAR = "ALPHA", REG = TRUE, FIT = FALSE,
                        AXES = "LINEAR", LEGEND = FALSE, outputType = "SHINY",  dataType = "T",
                        SHAPE_VAR = NA)},
    res = 128,
    height = 500,width = 500)
  
  # popups and highlighting of scatter plot point ------------
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event) ) return()
    isolate({
      showBridgePopup(event$id, event$lat, event$lng)
    })
    output$scatter <- renderPlot({
      PlotT1T2corrRegress(bridgesInBounds(),"Collapse Return Period [yr]","Max Recorded Return Period [yr]",NA, TEXT = character(0), 
                          ALPHA_VAR = "ALPHA", REG = TRUE, FIT = FALSE,
                          AXES = "LINEAR", LEGEND = FALSE, outputType = "SHINY", dataType = "T",
                          SHAPE_VAR = NA) + geom_point(data = BridgesDataFrame[BridgesDataFrame$ID==event$id,],
                                                       color = "black", size = sizesP$FailCorrMax["SHINY"])},
      res = 128,
      height = 500,width = 500)
  })
})