library(shiny)
library(shinythemes)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hydraulic Bridge Collapses in the Continental US"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(sidebarPanel(
                  sliderInput("drainArea",
                              "Drainage Area (km2):",
                              min = 0,
                              max = 8500,
                              value = c(10,8200),
                              round = TRUE,
                              step = 500),

                  checkboxGroupInput("FailCause",
                                     label = "Failure causes",
                                     choices = list("FLOOD" = "FLOOD",
                                                    "SCOUR" = "SCOUR",
                                                    "HURRICANE" = "HURRICANE",
                                                    "OTHER" = "OTHER"),
                                     selected = c("FLOOD","SCOUR","HURRICANE","OTHER"))
),
                # Show map
                mainPanel(
                  # p("Visualization created by Madeleine Flint, Virginia Tech, 2016."),
leafletOutput("map") ,
plotOutput("scatter")
                )
)))