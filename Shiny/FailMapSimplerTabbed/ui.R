library(shiny)
library(shinythemes)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(Hmisc)

# SETUP AND OPTIONS------------------------------------------------------------
# Controls on scaling limits
hazChangeLims <- c(-20,20)
step  <- 5
nStops <- length(seq(hazChangeLims[1],hazChangeLims[2],by=step))

# UI PAGE----------------------------------------------------------------------
shinyUI(fluidPage(
  # Header --------------------------------------------------------------------
  title="Effect of changing flood hazard on bridge collapse",
  h2("Effect of changing flood hazard on bridge collapse"),
  includeText("intro.txt"),
  tabsetPanel(type = "tabs",
              # Tab 1: collapsed bridges -------------------------------------
              tabPanel("Collapsed Bridges",fluidRow(
                column(5,
                       h4("Empirical collapse data"),
                       p("35 historical bridges collapses identified by the 
                         New York State Department of Transportation were 
                         analyzed by Flint et al. (2016) to study the severity
                         of flooding required to cause a bridge to collapse.
                         Bridge collapses were identified as being caused by
                         floods, scour (erosion of the stream bed supporting the 
                         bridge foundation), hurricanes, or other hydraulic
                         phenomenon."),
                       helpText("Select causes of bridge collapse and range of stream size (proportional to drainage area) to consider:"),
                       checkboxGroupInput("FailCause",
                                          label = "Collapse causes",
                                          choices = list("FLOOD" = "FLOOD",
                                                         "SCOUR" = "SCOUR",
                                                         "HURRICANE" = "HURRICANE",
                                                         "OTHER" = "OTHER"),
                                          selected = c("FLOOD","SCOUR","HURRICANE","OTHER")),
                       sliderInput("drainArea",
                                   label = "Drainage Area (1000 sq.km)",
                                   min = 0,
                                   max = 8.5,
                                   value = c(.01,8.2),
                                   round = TRUE,
                                   step = 0.5)),
                column(7,
                       helpText("Zoom in to focus on a region or click on an individual point to learn
                                more about the collapsed bridge:"),
                       leafletOutput("map")))),
              # Tab 2: collapsed risk ----------------------------------------
              tabPanel("Collapse Risk",
                       fluidRow(
                         column(3,
                                h3("Hazard"),
                                p("Analysis of an individual bridge collapse would use the 'hazard curve'
                                  relating flood severity to likelihood of occuring for a particular stream
                                  and site. Since bridges across multiple sites are studied it is necessary
                                  to use a generic form of the hazard curve, in which BLAH."),
                                plotOutput("hazStatic", height = 300)
                                
                         ),
                         column(6,
                                h3("x Consequences"),
                                p("The likelihood of a bridge collapsing given that a flood of a certain
                                  severity has occurred can be calculated using either nominal design values
                                  or using empirical data from past collapses."),
                                tags$div(
                                  tags$ul(
                                    tags$li("The nominal approach is  more frequently used in climate change 
                                            impact analyses, as it BLAH. As modern
                                            bridges are designed to withstand a 100-year flood with low risk of collapse,
                                            this point estimate BLAH."),
                                    tags$li("A point estimate obtained using the empirical data,
                                  such as the median, can also be BLAH. "),
                                    tags$li("A more robust description of collapse
                                  risk would BLAH.")
                                  )
                                ),
                                plotOutput("histTfail", height = 300),
                                helpText("Select the consequence (bridge collapse) models to display:")
                         ),
                         column(3,
                                h3("= Risk"),
                                p("Conditional probabilities are used to compute the collapse risk for point estimates.
                                  Risk = Prob(collapse | collapse flood)*Prob(collapse flood. If a full distribution
                                  of collapse is used (as in the kernel estimate), an integral and the total probability
                                  theorem must be used BLAH."),
                                plotOutput("pFail", height = 300)
                                
                         )),
                       fluidRow(
                         column(3, offset = 3,
                       checkboxGroupInput("FailData",
                                          label = "Collapse data",
                                          choices = list("Nominal" = "Nominal",
                                                         "Median" = "Median",
                                                         "Kernel" = "Kernel"),
                                          selected = c("Nominal","Kernel")))
                       )),
              # Tab 3: hazard and risk change -------------------------------------
              tabPanel("Hazard and Risk Change",
                       fluidRow(column(7,
                                h3(HTML("Change in Hazard: &Delta;&lambda;<sub>T</sub>"))
                                ),
                                column(5,
                                       h3(HTML("&rArr; Change in Risk: &Delta;P<sub>F</sub>"))
                                       )),
                       fluidRow(
                                  column(4,
                                       p("Whereas climate change is expected to BLAH, a general sensitivity
                                         of collapse risk to BLAH can be estimated by BLAH. The flood hazard
                                         curve can be altered to uniformly BLAH."),
                                       # tags$h3(HTML("&Delta;")), # add back in color
                                       helpText("Use the slider to create a hypothetical change in the rate of flooding. 
                                                The change will be uniformly applied across all levels (return periods) of flooding.
                                                To BLAH BLAH, select the Scale option"),
                                       sliderInput("hazChange",
                                                   label = "Select uniform flood hazard change:",
                                                   post = "%",
                                                   min = hazChangeLims[1],
                                                   max = hazChangeLims[2],
                                                   value = 0,
                                                   round = TRUE,
                                                   step = step),
                                       radioButtons("hazSelect",
                                                    label = NULL,
                                                    choices = list("Scale" = 1, "Shift" = 2), selected = 1)),
                                column(4,
                                       # add a render text related to how hazard is shifted
                                       plotOutput("haz", height = 300)),
                                
                                column(4,
                                       p("In a linear BLAH, a 10% increase in flood hazard would result
                                         in a 10% increase in BLAH. Due to BLAH, BLAH."),
                                       plotOutput("anFail", height = 150)
                                ))),
              tabPanel("Conclusions"))
 
))