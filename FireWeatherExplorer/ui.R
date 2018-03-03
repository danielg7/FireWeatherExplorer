#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


navbarPage("Fire Weather Explorer",
          

# Station Selection ------------------------------------------------
           tabPanel("Station Selection",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput('station',
                                    label = "Select Station",
                                    choices = Larimer$STATION$NAME),
                        dateRangeInput('dateRange',
                                       label = 'Date range input: yyyy-mm-dd',
                                       start = Sys.Date() - 365*3, end = Sys.Date()),
                        actionButton("pickStations", "Submit"),
                        htmlOutput("metadata")
                        ),
                      mainPanel(
                        plotOutput("temp_ts_plot"),
                        plotOutput("rh_ts_plot"),
                        plotOutput("wind_ts_plot")
           ))),

# Subset Plots ------------------------------------------------

           tabPanel("Subset Plots",
                      sidebarLayout(
                        sidebarPanel(
                          
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
                          
                          # TO DO: Wind Sliders
                          
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
                                             selected = "E"),
                          actionButton("subsetData", "Submit")
                        ),
                        
                        # Output plots for subset
                        
                        mainPanel(plotOutput("rh_ts_sub_plot"),
                                  plotOutput("rhplot")))
                    )
)




