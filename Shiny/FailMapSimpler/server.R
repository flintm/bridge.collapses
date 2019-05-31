library(shiny)
library(Cairo)
options(shiny.usecairo=T)
source("../../Plotting/SetupEncoding.R")
source("../../Plotting/getTheme.R")
source("../../Plotting/PlaceAnnotation.R")
source("../../Plotting/PlotT1T2corrRegress.R")
source("../../Plotting/PlotHazardCurve.R")
source("my.kernel.interp.R")
source("calc.pF.R")
load("../../Data/df.Fail.NBI.Gage.Active.RData")
# Controls on scaling limits
hazChangeLims <- c(-20,20)
step  <- 5
nStops <- length(seq(hazChangeLims[1],hazChangeLims[2],by=step))

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

# colors and such
deltas <- seq(as.numeric(hazChangeLims[1]/100),as.numeric(hazChangeLims[2]/100),length.out = nStops)
names(deltas) <- as.character(deltas)
pal <- colorFactor(colorsP$Fail, names(colorsP$Fail), ordered = TRUE)
colors <- brewer.pal(nStops, "RdBu")
names(colors) <- as.character(deltas)
colors["0"] <- "black"

# server setup------------
shinyServer(function(input, output) {
  output$delta <- renderText({
    sgn <- as.character(sign(input$hazChange))
    clr <- colors[as.character(input$hazChange/100)]
    txt <- "Floods of a given return period occur are assumed to occur with "
    txt[2] <- switch(sgn,
                  "0" =  "unchanged frequency.",
                  "1" = paste0('<span style=\"color:', clr,'\">'), #'; font-size: 24px\">'
                  "-1" = paste0('<span style=\"color:', clr,'\">'))
    txt[3] <- switch(sgn,
                     "0" =  "",
                     "1" = paste0("+",abs(input$hazChange),"% more frequency.",'</span>'),
                     "-1" = paste0("-",abs(input$hazChange),"% less frequency.",'</span>'))
    paste0(txt,collapse = "")
    # paste0('<span style=\"color:', clr,'; font-size: 28px\">',
    #       # '\">',
    #       txt,'</span>')#span("dashboard", 
    #      style = "color: red; font-size: 28px"))
  })
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
    d.emph <- as.character(input$hazChange/100)
    T.delta <- switch(as.character(input$hazSelect),
                      "1" = as.data.frame(sapply(deltas, function(d) 1/(Ts*(1-d)), USE.NAMES = TRUE)),
                      "2" = as.data.frame(sapply(deltas, function(d) 1/(Ts-Tshift*d)), USE.NAMES = TRUE))
    T.delta$T_0 <- Ts 
    PlotHazardCurve(T.delta, d.emph = d.emph, nCol = nStops,
                    d.limits = range(deltas),
                    x.limits = c(50,200), y.limits = c(0.002,0.05),
                    AXES = "LOG", outputType = "SHINY")
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
    kernel.plot$c <- seq(0,1,length.out = nrow(kernel.plot))
    
    data.text <- data.frame(x = c(t.F+25,med + (lims[2]-lims[1])/10,0.6*lims[2]), 
                             y = c(h/1.5, h/2.5,h/10),
                            label = c("nominal = 100 yr design flood",
                                      paste("collapse median =\n", round(med),"years"),
                                      "collapse kernel-smoothed\ndistribution"),
                            group = labelsP$Dist,
                            hjust = c(0,0,0.5),
                            row.names = c("Nominal","Median","Kernel"))
    data.text <- data.text[input$FailData,]
    pTheme <- getTheme(outputType = "SHINY") +
      theme(axis.text.x   = element_text(color = "black", size = textP$reg["SHINY"], 
                                         angle = 90, vjust = 0.5, hjust = 1, margin  = margin(0.16,0,0,0, "cm")),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank())
    pHist <- ggplot(bridgesInBounds(), aes(x=T_FAIL_D_HECD_USGS)) + 
      geom_histogram(aes(fill=FAIL_CAUS_CODE))+
      scale_fill_manual(values = colorsP$Fail) + guides(fill=FALSE) + pTheme + 
      labs(title = "Return Periods of Floods\nCausing Bridge Collapse", x = expression(paste(T["R"]," [yr]")), y = "") + 
      geom_text(data = data.text,aes(x=x,y=y,label = label, hjust = hjust), size = textP$annotate["SHINY"])
      
    if("Nominal" %in% input$FailData){
      pHist <- pHist + geom_vline(xintercept = t.F, linetype = linesP$Dist["nominal"], color = colorsP$Dist["nominal"], size = 1)
    }
    if("Median" %in% input$FailData){
      medCol <- apply(col2rgb(colorsP$Fail[input$FailCause]),MARGIN = 1, mean)
      medCol <- rgb(medCol[1]/255,medCol[2]/255,medCol[3]/255)
      pHist <- pHist + geom_vline(xintercept = med, linetype = linesP$Dist["median"], color = medCol, size = 1) #
    }
    if("Kernel" %in% input$FailData){
      pHist <- pHist + geom_line(data = kernel.plot, aes(x=x,y=y, color = c), size = 1.5) + #, linetype = linesP$Dist["kernel"],
        scale_color_gradientn(colours=as.vector(colorsP$Fail[input$FailCause]), guide=FALSE)
    }
    pHist
  })
 
  # absolute value of collapse probabilities ----
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
                     group = tolower(labelsP$Dist),
                     "T" = c(t.F,
                             median(bridgesInBounds()$T_FAIL_D_HECD_USGS),
                             NA),
                     ymin = rep(0,3),
                     row.names = labelsP$Dist)
    pF0$ymax <- c(1/t.F*pnorm(-beta), 
                 1/pF0["Median","T"], 
                 NA)
    pF0["Kernel","ymax"] <-sum(T.kernel.interp$y/(T.kernel.interp$x)^2*dt)
    pF0$label <- signif(pF0$ymax,2)

    pF0.melt2 <- melt(pF0[,colnames(pF0)!="T"], id.vars = c("x","group","label"), value.name = "y")
    pF0.melt2 <- pF0.melt2[order(pF0.melt2$x,decreasing = T ),]
    row.names(pF0.melt2) <- seq(1,nrow(pF0.melt2))
    grad.out <- 30
    pF0.melt2[grep("Kernel",pF0.melt2$x)[1]:(grep("Kernel",pF0.melt2$x)[1]+grad.out-1),] <- pF0.melt2[grep("Kernel",pF0.melt2$x)[2],]
    pF0.melt2[(grep("Kernel",pF0.melt2$x)[1]):(grep("Kernel",pF0.melt2$x)[1]+grad.out-1),"y"] <- seq(0,pF0.melt2[grep("Kernel",pF0.melt2$x)[1],"y"],length.out = grad.out) 
    pF0.melt2[pF0.melt2$x=="Kernel","variable"] <- "ymin"
    pF0.melt2[nrow(pF0.melt2),"variable"] <- "ymax"
    pF0.melt2[pF0.melt2$x=="nominal","col"] <- 1
    pF0.melt2[pF0.melt2$x=="median","col"] <- 0.5
    pF0.melt2[pF0.melt2$x=="Kernel","col"] <- seq(0,0.1*length(input$FailCause)-0.1,length.out = grad.out)
    pF0.melt2[pF0.melt2$variable=="ymin","label"] <- NA
    
    medCol <- apply(col2rgb(colorsP$Fail[input$FailCause]),MARGIN = 1, mean)
    medCol <- rgb(medCol[1]/255,medCol[2]/255,medCol[3]/255)
    colValues <- c(seq(0,0.1*length(input$FailCause)-0.1, by = 0.1),0.5,1)
    colGrad   <- c(colorsP$Fail[input$FailCause], medCol, "black")
    
    pF0.melt2 <- pF0.melt2[pF0.melt2$x %in% tolower(input$FailData),]
    
    ggplot(data = pF0.melt2, aes(x=x,y=y, color = col)) + 
      geom_path(size = 4) +# scale_linetype_manual(values = linesP$Dist, guide = FALSE) + 
      scale_color_gradientn(colours = colGrad, values = colValues, guide = FALSE) + ggtitle("Probability of a Bridge\nCollapsing Over One Year") +
      ylim(c(0,0.04))+
      geom_text(aes(y = y, label = label), hjust=0, size = textP$annotate["SHINY"]) +
      getTheme(outputType = "SHINY") + coord_flip()+
      theme(axis.text.y = element_text(size = textP$sub["SHINY"]),#14),#
            axis.text.x   = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank())
  })
  # change in annual probability of collapse, delta ---------
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

    pF <- data.frame(x = c("Hazard",labelsP$Dist),
                     group = tolower(c("Hazard",labelsP$Dist)),
                     "T" = c(NA,
                             t.F,
                             median(bridgesInBounds()$T_FAIL_D_HECD_USGS),
                             NA),
                     ymin = rep(0,4),
                     row.names = c("Hazard",labelsP$Dist))
    pF$none <- c(0,1/t.F*pnorm(-beta), 
                    1/pF["Median","T"], 
                    sum(T.kernel.interp$y/(T.kernel.interp$x)^2*dt))
    pF[,d] <- c(NA,
                switch(as.character(input$hazSelect),
                       "1" = 1/(t.F*(1-input$hazChange/100))*pnorm(-beta),
                       "2" = 1/(t.F-Tshift*input$hazChange/100)*pnorm(-beta)),
                switch(as.character(input$hazSelect),
                       "1" = 1/(pF["Median","T"]*(1-input$hazChange/100)),
                       "2" = 1/(pF["Median","T"]-Tshift*input$hazChange/100)),
                switch(as.character(input$hazSelect),
                       "1" = sum(T.kernel.interp$y/(T.kernel.interp$x*(1-input$hazChange/100))^2*dt),
                       "2" = NA))
    if(is.na(pF["Kernel",d])){
      T.kernel.interp$y <- T.kernel.interp$y[T.kernel.interp$x-Tshift*input$hazChange/100 > 0]
      T.kernel.interp$x <- T.kernel.interp$x[T.kernel.interp$x-Tshift*input$hazChange/100 > 0]
      T.kernel.interp$y <- T.kernel.interp$y/sum(T.kernel.interp$y*dt)
      pF["Kernel",d] <- sum(T.kernel.interp$y/(T.kernel.interp$x-Tshift*input$hazChange/100)^2*dt)
  }
    pF$change <- (pF[,d] - pF$none)/pF$none*100
    pF["Hazard","Change"] <- input$hazChange
    pF$label <- paste0(signif(pF$change,2),"%")
    pF$hjust <- ifelse(input$hazChange>=0,0,1)
    
    medCol <- apply(col2rgb(colorsP$Fail[input$FailCause]),MARGIN = 1, mean)
    medCol <- rgb(medCol[1]/255,medCol[2]/255,medCol[3]/255)
    colValues <- c(seq(0,0.1*length(input$FailCause)-0.1, by = 0.1),0.5,1)
    colGrad   <- c(colorsP$Fail[input$FailCause], medCol, "black")
    
    labels <- rev(c("Hazard",labelsP$Dist[labelsP$Dist %in% input$FailData]))
    pF <- pF[pF$x %in% labels,]
    pF$x <- factor(pF$x, levels = labels, labels = labels, ordered = T)
    
    # make necessary changes if beyond axis limits
    pF$label[abs(pF$change)>60] <- paste0("(",pF$label[abs(pF$change)>60],")")
    pF$hjust[abs(pF$change)>60] <- abs(pF$hjust[abs(pF$change)>60]-1)
    pF$change[abs(pF$change)>60] <- sign(pF$change[abs(pF$change)>60])*60

    p <- ggplot(data = pF,aes(x=x,y=change)) + geom_col(aes(fill=group)) + 
      scale_fill_manual(values = c(hazard = colors[d],colorsP$Dist), guide = FALSE)+
    geom_hline(yintercept  = 0) + 
      ylim(c(-60,60))+
       geom_text(aes(y = change, label = label, hjust = hjust),
                 size =  textP$annotate["SHINY"]) + 
      labs(title='Change in annual collapse\nprobability given hazard change [%]',
           x=NULL,
           y=NULL)
    if(input$hazChange!=0){
      p <- p + geom_hline(yintercept = input$hazChange, color = colors[d]) 
    }
    p  + getTheme(outputType = "SHINY") + 
      theme(axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 14)) + coord_flip()
      # theme(axis.text.x = element_text(size = textP$sub["SHINY"]),
            # axis.ticks.x = element_blank(),
            # axis.ticks.y = element_blank(),
            # axis.title.x = element_blank(),
            # axis.title.y = element_blank(),
            # )
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