#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library("shiny")
library("leaflet")


navbarPage("Fire Weather Explorer", id = "tabs",
           tabPanel("Station Selection",
                    sidebarLayout(
                      sidebarPanel(
                        includeMarkdown("includedText/mainPageText.md"),
                        selectInput('State',
                                    label = "Select State",
                                    choices = sort(as.character(unique(AllRAWS$STATION$STATE)))),
                        uiOutput('County'),
                        uiOutput('station'),
                        uiOutput('POR'),
                        actionButton("pickStations", "Submit")
                        ),
                      mainPanel(
                        fluidRow(htmlOutput("metadataTitle")),
                        fluidRow(leafletOutput("station_location")),
                        fluidRow(htmlOutput("metadata"))
                        )
                        
                      )
           ),
           tabPanel(title = "Data Quality Plots", value = "Diagnostic",
                    sidebarLayout(
                      #
                      # This section adds a series of radio buttons that define
                      # which plots to show in the data quality check plots.
                      #
                      sidebarPanel(
                        radioButtons(inputId = "diagnosticType",
                                     label = "Choose plot type:",
                                     choiceNames = c("Relative Humidity", "Temperature", "Wind Speed"),
                                     choiceValues = c("RH","Temp","Wind_Speed"),
                                     selected = "RH"),
                        #
                        # Include diagnostic plot verbiage by importing a markdown file.
                        #
                        includeMarkdown("includedText/diagnosticPlots.md")
                                ),
                      mainPanel(
                        #
                        # This section adds a series of panels that are responsive to
                        # radio buttons (above). They also check to see if the wx data are
                        # available for plotting.
                        #
                        tags$div(class="header", checked = NA,
                                 tags$h1("Data Quality Plots")),
                        conditionalPanel("input.diagnosticType == 'RH'",
                                         plotOutput("rh_ts_plot")),
                        conditionalPanel("input.diagnosticType == 'Temp'",
                                         plotOutput("temp_ts_plot")),
                        conditionalPanel("input.diagnosticType == 'Wind_Speed'",
                                         plotOutput("wind_ts_plot"))
                                )
                              )
                    ),
           
           tabPanel(title = "Station Summary Data", value = "Summary",
                    sidebarLayout(
                      #
                      # This section adds a series of radio buttons that define
                      # which plots to show in the station data check plots.
                      #
                      sidebarPanel(
                        radioButtons(inputId = "monthlyType",
                                     label = "Choose monthly plot type:",
                                     choiceNames = c("Relative Humidity", "Temperature", "Wind Speed"),
                                     choiceValues = c("RH","Temp","Wind_Speed"),
                                     selected = "RH"),
                        radioButtons(inputId = "hourlyType",
                                     label = "Choose hourly plot type:",
                                     choiceNames = c("Relative Humidity", "Temperature", "Wind Speed"),
                                     choiceValues = c("RH","Temp","Wind_Speed"),
                                     selected = "RH")#,
                        #
                        # Include diagnostic plot verbiage by importing a markdown file.
                        #
                        #includeMarkdown("includedText/diagnosticPlots.md")
                      ),
                      mainPanel(
                        #
                        # This section adds a series of panels that are responsive to
                        # radio buttons (above). They also check to see if the wx data are
                        # available for plotting.
                        #
                        
                        tags$div(class="header", checked = NA,
                                 tags$h1("Station Summary Plots")),
                        tabsetPanel(
                          tabPanel("Monthly Plots",
                                   conditionalPanel("input.monthlyType == 'RH'",
                                                    plotOutput("month_rh")),
                                   conditionalPanel("input.monthlyType == 'Temp'",
                                                    plotOutput("month_temp")),
                                   conditionalPanel("input.monthlyType == 'Wind_Speed'",
                                                    plotOutput("month_wind"))), 
                          tabPanel("Hourly Plots",
                                   conditionalPanel("input.hourlyType == 'RH'",
                                                    plotOutput("hour_rh")),
                                   conditionalPanel("input.hourlyType == 'Temp'",
                                                    plotOutput("hour_temp")),
                                   conditionalPanel("input.hourlyType == 'Wind_Speed'",
                                                    plotOutput("hour_wind"))
                                   )
                          )
                        )
                    )
           ), 
           tabPanel("Prescription Plots", id = "Subset",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons(inputId = "rxPlotType",
                                     label = "Choose plot to display:",
                                     choiceNames = c("Hours in Prescription", "Percent of Months in Prescription"),
                                     choiceValues = c("hoursRx","histMonthRx"),
                                     selected = "hoursRx"),
                        
                        includeMarkdown("includedText/subsetPlots.md"),
                        
                        # Month Sliders
                        
                        sliderInput("months",
                         "Months to use:",
                         min = 1,
                         max = 12, value = c(9,12)),
                        
                        # Hour Sliders
                        
                        sliderInput("hours",
                         "Hours to use:",
                         min = 1,
                         max = 24, value = c(8,18)),
                        
                        # RH Sliders
                        
                        sliderInput("rh",
                         "Relative Humidity:",
                         min = 1,
                         max = 100,
                         value = c(15,35)),
                        
                        # Temp Sliders
                        
                        sliderInput("temp",
                                    "Temperature (F):",
                                    min = -15,
                                    max = 110,
                                    value = c(15,75)),
                        
                        # Wind Sliders
                        
                        sliderInput("wind",
                         "Wind speeds (mph, 20 ft):",
                         min = 0,
                         max = 50, value = c(8,25)),
                        
                        # Wind Direction Check Boxes
                        
                        checkboxGroupInput("wind_directions", "Wind Directions:",
                                c("N",
                                  "NNE",
                                  "NE",
                                  "ENE",
                                  "E",
                                  "ESE",
                                  "SE",
                                  "SSE",
                                  "S",
                                  "SSW",
                                  "SW",
                                  "WSW",
                                  "W",
                                  "WNW",
                                  "NW",
                                  "NNW"),
                                selected = "E")),
                      
                      # Main Panel Ouput
                      
                      mainPanel(tags$div(class="header", checked = NA,
                                         tags$h1("Prescription Plots")),
                                conditionalPanel("input.rxPlotType == 'hoursRx'",
                                                 plotOutput("rh_ts_sub_plot")),
                                conditionalPanel("input.rxPlotType == 'histMonthRx'",
                                                 plotOutput("rhplot"))
                                )
                      )
                    ),
           tabPanel("About", id = "About",
                    mainPanel(
                      includeMarkdown("includedText/about.md")
                      )
                    )
)




