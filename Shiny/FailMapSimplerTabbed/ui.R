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
  includeHTML("intro.html"),
  tabsetPanel(type = "tabs",
              # Tab 1: collapsed bridges -------------------------------------
              tabPanel("Collapsed Bridges",fluidRow(
                column(5,
                       h4("Empirical collapse data"),
                       p("Historical bridges collapses identified by the 
                         New York State Department of Transportation were 
                         analyzed to study the severity
                         of flooding required to cause a bridge to collapse.
                         35 bridges were selected where a stream gauge was
                         nearby to record the flow volume on the day of collapse.
                         Collapses were identified as being caused by
                         floods, scour (erosion of the stream bed supporting the 
                         bridge foundation), hurricanes, or other hydraulic
                         phenomenon."),
                       helpText("Select causes of bridge collapse and range of drainage area 
                                (proportional to stream size)."),
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
                                more about the collapsed bridge."),
                       leafletOutput("map")))),
              # Tab 2: collapsed risk ----------------------------------------
              tabPanel("Collapse Risk",
                       fluidRow(
                         column(4,
                                h4("Hazard"),
                                p("Analysis of an individual bridge collapse would use the 'hazard curve'
                                  relating flood severity to likelihood of occuring for a particular stream
                                  and site.")
                         ),
                         column(5,
                                h4("x Consequences"),
                                p("The likelihood of a bridge collapsing given that a flood of a certain
                                  severity has occurred can be calculated using either nominal design values
                                  or using empirical data from past collapses, shown below.")
                         ),
                         column(3,
                                h4("= Risk"),
                                withMathJax(),
                                p('$$P_f=P(C|T_R^C)P(T_R^C)$$'),
                                tags$div("where",tags$em("C"),"denotes collapse."))),
                       fluidRow(
                         column(4, 
                                plotOutput("hazStatic", height = 300),
                                p("As bridges across multiple sites are studied it is necessary
                                to use a generic form of the hazard curve, based on the collapse flow
                                return periods rather than the flow values themselves.")),
                         column(5,
                                plotOutput("histTfail", height = 300),
                                tags$div(
                                  tags$ul(
                                    tags$li(tags$em("Nominal:"),"straighforward and frequently used in climate change 
                                            impact analyses. Usually assumes collapse is linked to a 100-year flood
                                            (1% annual chance of occurring) based on modern designs."),
                                    tags$li(tags$em("Median:"),"a point estimate obtained using the empirical data. "),
                                    tags$li(tags$em("Kernel:"),"uses the full distribution of collapse return periods 
                                            obtained from the historical analysis. With sufficient data, more
                                            robust than point estimates.")
                                  )
                                )),
                         column(3,
                                plotOutput("pFail", height = 300),
                                p("Conditional probabilities are used to compute the collapse risk for point estimates.
                                  If a full conditional distribution
                                  of collapse is used (as in the kernel estimate), an integral and the total probability
                                  theorem must be used."))),
                       fluidRow(
                         column(5, offset = 4,
                         helpText("Select the consequence (bridge collapse) models to display."),
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
                                h4(HTML("Change in Hazard: &Delta;&lambda;<sub>T</sub>")),
                                p("Whereas climate change is expected to have varied and complex effects
                                  on future flood risk, a uniform shifting or scaling of the flood
                                  hazard curve can be used in a preliminary study.")
                                ),
                                column(5,
                                       h4(HTML("&rArr; Change in Risk: &Delta;P<sub>F</sub>")),
                                    p("Accurate climate impact assessment requires understanding the sensitivity
                                      of bridge collapse risk to changing flood hazard.")
                                       )
                                ),
                       fluidRow(column(4,
                                       helpText("Use the slider to create a hypothetical change in the rate of flooding. 
                                                The change will be uniformly applied across all levels (return periods) of flooding."),
                                       sliderInput("hazChange",
                                                   label = "Uniform flood hazard change",
                                                   post = "%",
                                                   min = hazChangeLims[1],
                                                   max = hazChangeLims[2],
                                                   value = 0,
                                                   round = TRUE,
                                                   step = step),
                                       radioButtons("hazSelect",
                                                    label = NULL,
                                                    choices = list("Scale" = 1, "Shift" = 2), selected = 1),
                                       helpText("The change can be applied multiplicatively (scaling) or through translation (shifting).")),
                                column(4,
                                       plotOutput("haz", height = 300)
                                       ),
                                
                                column(4,
                                       plotOutput("anFail", height = 150)
                                )),
                       HTML("<br><br><br><br>")),
              tabPanel("Conclusions",
                       includeHTML("conclusions.html"))
              ),
  tags$footer("Copyright Madeleine Flint, 2019")
    )
)