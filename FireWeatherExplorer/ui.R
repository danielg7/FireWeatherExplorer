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
                        selectInput('State',
                                    label = "Select State",
                                    choices = sort(as.character(unique(AllRAWS$STATION$STATE)))),
                        uiOutput('County'),
                        uiOutput('station'),
                        uiOutput('POR'),
                        actionButton("pickStations", "Submit"),
                        htmlOutput("metadata")
                        ),
                      mainPanel(
                        leafletOutput("station_location")
           )))
)




