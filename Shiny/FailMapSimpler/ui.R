library(shiny)
library(shinythemes)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(Hmisc)

# Controls on scaling limits
hazChangeLims <- c(-20,20)
step  <- 5
nStops <- length(seq(hazChangeLims[1],hazChangeLims[2],by=step))

# UI page
shinyUI(fluidPage(
  
  # Application title
  title="Effect of changing flood hazard on bridge collapse",
  h2("Effect of changing flood hazard on bridge collapse"),
  includeText("intro.txt"),
  fluidRow(column(4,
                  h3("\\Delta Hazard"),
                  h4(htmlOutput("delta")),
                  helpText("Use the slider to create a hypothetical change in the rate of flooding. 
                           The change will be uniformly applied across all levels (return periods) of flooding.
                           To BLAH BLAH, select the Scale option"),
                  sliderInput("hazChange",
                              label = NULL,
                              # label = "Select Uniform hazard change [%].",
                              post = "%",
                              min = hazChangeLims[1],
                              max = hazChangeLims[2],
                              value = 0,
                              round = TRUE,
                              step = step),
                  radioButtons("hazSelect",
                               label = NULL,
                              # label = "Hazard change method",
                              choices = list("Scale" = 1, "Shift" = 2), selected = 1)),
           column(4,
                  h3("x Consequences"),
                  h4("Model A: nominal bridge designed for 100-year flood"),
                  h4("Model B: data from actual collapsed bridges")
                  ),

           column(4,
                  h3("= \\Delta Risk"),
                  plotOutput("anFail", height = 150)
           )),

 fluidRow(
column(4,
       plotOutput("haz", height = 300)
       
),
column(4,
       plotOutput("histTfail", height = 300)
       
),
column(4,
       plotOutput("pFail", height = 300)
       
))#,
                # failure

# fluidRow(
#   column(4,
#          h4("Historical collapse explorer"),
#          checkboxGroupInput("FailCause",
#                             label = "Collapse causes",
#                             choices = list("FLOOD" = "FLOOD",
#                                            "SCOUR" = "SCOUR",
#                                            "HURRICANE" = "HURRICANE",
#                                            "OTHER" = "OTHER"),
#                             selected = c("FLOOD","SCOUR","HURRICANE","OTHER")),
#          sliderInput("drainArea",
#                      label = "Drainage Area (1000 sq.km)",
#                      min = 0,
#                      max = 8.5,
#                      value = c(.01,8.2),
#                      round = TRUE,
#                      step = 0.5)
#   ),
#  column(8,
#                   leafletOutput("map") )
# )
))