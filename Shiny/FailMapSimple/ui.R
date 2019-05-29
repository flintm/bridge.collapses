library(shiny)
library(shinythemes)
library(leaflet)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  title="Effect of changing flood hazard on bridge collapse",
  h2("Effect of changing flood hazard on bridge collapse"),
  includeText("intro.txt"),
  
  # Sidebar with a slider input for the number of bins
 fluidRow(
column(4,
       plotOutput("haz", height = 300)
       
),
column(4,
       plotOutput("histTfail", height = 300)
       
),
column(4,
       plotOutput("pFail", height = 300)
       
)),
                # failure
fluidRow(column(4,
                h3("Uniform hazard change"),
                sliderInput("hazChange",
                            label = "Uniform hazard change [%]",
                            min = -20,
                            max = 20,
                            value = 0,
                            round = TRUE,
                            step = 5),
                selectInput("hazSelect",
                            label = "Hazard change method",
                            choices = list("Scale" = 1, "Shift" = 2), selected = 1)),
                
column(8,
  #p("Visualization created by Madeleine Flint, Virginia Tech, 2016."),
  plotOutput("anFail", height = 150),
  leafletOutput("map") 
  # plotOutput("scatter"),
  
),
fluidRow(
  column(3, offset = 4,
         h4("Historical collapse explorer"),
         checkboxGroupInput("FailCause",
                            label = "Collapse causes",
                            choices = list("FLOOD" = "FLOOD",
                                           "SCOUR" = "SCOUR",
                                           "HURRICANE" = "HURRICANE",
                                           "OTHER" = "OTHER"),
                            selected = c("FLOOD","SCOUR","HURRICANE","OTHER"))
  ),
  column(4, offset = 1,
         sliderInput("drainArea",
                     label = "Drainage Area (1000 sq.km)",
                     min = 0,
                     max = 8.5,
                     value = c(.01,8.2),
                     round = TRUE,
                     step = 0.5)
         # selectInput('x', 'X', names(dataset)),
         # selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
         # selectInput('color', 'Color', c('None', names(dataset)))
  )#,
  # column(4,
  #        selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
  #        selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
  # )
)
)))