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
           tabPanel("Station Selection",
                    sidebarLayout(
                      sidebarPanel(
                        dateRangeInput('dateRange',
                                       label = 'Date range input: yyyy-mm-dd',
                                       start = Sys.Date() - 2, end = Sys.Date() + 2),
                        sliderInput("months",
                                    "Months to use:",
                                    min = 1,
                                    max = 12, value = c(9,12))),
                      mainPanel(
                        plotOutput("temp_ts_plot"))
           )),
           tabPanel("Station Diagnostics",
                    sidebarLayout(
                      sidebarPanel(),mainPanel())
                    ),
           tabPanel("Subset Plots",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("rh",
                                      "Relative Humidity:",
                                      min = 1,
                                      max = 100,
                                      value = c(15,35)),
                          checkboxGroupInput("wind_directions", "Wind Directions:",
                                             levels(wx_df$Wind_Direction))
                        ),
                        mainPanel(plotOutput("rhplot")))
                    )
)




