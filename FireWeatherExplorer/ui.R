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
                        leafletOutput("station_location")
           )))
)




