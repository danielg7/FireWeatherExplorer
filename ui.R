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

# TODO: update token section

# Read in API Key for Mesowest --------------------------------------------

# if(!file.exists("~/.mesowesttoken")){
#   print("Token not found.", quote = F)
#   
#   fileName <- 'api_key.txt'
#   api_key <- readChar(fileName, file.info(fileName)$size)
#   
#   mesowest::requestToken(apikey = api_key)
# }

# tweaks, a list object to set up multicols for checkboxGroupInput
tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                 height: 150px;
                                 -webkit-column-count: 4; /* Chrome, Safari, Opera */ 
                                 -moz-column-count: 4;    /* Firefox */ 
                                 column-count: 4; 
                                 -moz-column-fill: balanced;
                                 -column-fill: balanced;
                                 -margin-top: 0px !important;
                                 -webkit-margin-after: 0px !important; 
                                 }
                                .checkbox{
                                  margin-top: 0px !important;
                                 -margin-left: 0px;
                                 -webkit-margin-after: 0px !important; 
                                }
                                .checkbox-inline {margin-left: 0px;
                                                  -margin-top: 0px !important;
                                                  -margin: 0 !important;
                                                  -webkit-margin-after: 0px !important; }
                                 ")) 
  ))

controls <-
  list(strong("Wind Direction(s):"),
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'wind_directions', 
                                   label    = "", 
                                   choices  = windDirList,
                                   selected = "E",
                                   inline   = FALSE))) 

navbarPage("Fire Weather Explorer", id = "tabs",
           
           tabPanel("Station Selection",
                    sidebarLayout(
                      sidebarPanel(
                        includeMarkdown("includedText/mainPageText.md"),
                        selectInput('State',
                                    label = "Select State",
                                    choices = State_List,
                                    selected = "CO"),
                        uiOutput('County'),
                        uiOutput('station'),
                        uiOutput('POR'),
                        actionButton("pickStations", "Download Station Data")
                        ),
                      mainPanel(
                      #  fluidRow(htmlOutput("metadataTitle")),
                        leafletOutput("station_location", height = "600")
                       # fluidRow(htmlOutput("metadata"))
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
                                     choiceNames = c("Relative Humidity", "Temperature", "Wind Speed", "1 Hr Fuel Moisture","10 hr Fuel Moisture", "Hourly Precipitation","Growing Season Index"),
                                     choiceValues = c("RH","Temp","Wind_Speed","FMC1","FMC10","HourlyPrecip","GSI"),
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
                        tabsetPanel(
                        tabPanel("Data Quality Plots",
                          conditionalPanel("input.diagnosticType == 'RH'",
                                           plotOutput("rh_ts_plot")),
                          conditionalPanel("input.diagnosticType == 'Temp'",
                                           plotOutput("temp_ts_plot")),
                          conditionalPanel("input.diagnosticType == 'Wind_Speed'",
                                           plotOutput("wind_ts_plot")),
                          conditionalPanel("input.diagnosticType == 'FMC1'",
                                           plotOutput("fmc1_ts_plot")),
                          conditionalPanel("input.diagnosticType == 'FMC10'",
                                           plotOutput("fmc10_ts_plot")),
                          conditionalPanel("input.diagnosticType == 'HourlyPrecip'",
                                           plotOutput("precip_ts_plot")),
                          conditionalPanel("input.diagnosticType == 'GSI'",
                                           plotOutput("gsi_ts_plot"))
                        ),
                        tabPanel("Data",
                                 div(DT::dataTableOutput("totalPlot"), style = "font-size: 75%; width: 500px"),
                                 downloadButton(outputId = "downloadWxData", "Download CSV of Data"))
                        )
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
                        radioButtons(inputId = "summaryType",
                                     label = "Choose plot type:",
                                     choiceNames = c("Relative Humidity", "Temperature", "Wind Speed"),
                                     choiceValues = c("RH","Temp","Wind_Speed"),
                                     selected = "RH")#,
                      #  radioButtons(inputId = "hourlyType",
                       #              label = "Choose hourly plot type:",
                        #             choiceNames = c("Relative Humidity", "Temperature", "Wind Speed"),
                         #            choiceValues = c("RH","Temp","Wind_Speed"),
                          #           selected = "RH")#,
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
                        
                     #   tags$div(class="header", checked = NA,
                      #           tags$h1("Station Summary Plots")),
                        tabsetPanel(
                          tabPanel("Monthly Plots",
                                   conditionalPanel("input.summaryType == 'RH'",
                                                    plotOutput("month_rh")),
                                   conditionalPanel("input.summaryType == 'Temp'",
                                                    plotOutput("month_temp")),
                                   conditionalPanel("input.summaryType == 'Wind_Speed'",
                                                    plotOutput("month_wind"))), 
                          tabPanel("Hourly Plots",
                                   conditionalPanel("input.summaryType == 'RH'",
                                                    plotOutput("hour_rh")),
                                   conditionalPanel("input.summaryType == 'Temp'",
                                                    plotOutput("hour_temp")),
                                   conditionalPanel("input.summaryType == 'Wind_Speed'",
                                                    plotOutput("hour_wind"))
                                   )
                          )
                        )
                    )
           ), 
           tabPanel("Prescription Plots", id = "Subset",
                    sidebarLayout(
                      sidebarPanel(
                        includeMarkdown("includedText/subsetPlots.md"),
                        tabsetPanel(
                          tabPanel(title = "Time",
                                   # Month Sliders
                                   
                                   sliderInput(inputId = "months",
                                               label = "Months to use:",
                                               min = 1,
                                               max = 12, value = c(9,12)),
                                   
                                   # Hour Sliders
                                   
                                   sliderInput(inputId = "hours",
                                               label = "Hours to use:",
                                               min = 1,
                                               max = 24, value = c(8,18))
                                   ),
                          tabPanel(title = "Fuel Moisture",
                                   # RH Sliders
                                   
                                   sliderInput(inputId = "rh",
                                               label = "Relative Humidity:",
                                               min = 1,
                                               max = 100,
                                               value = c(15,35)),
                                   
                                   # Temp Sliders
                                   
                                   sliderInput(inputId = "temp",
                                               label = "Temperature (F):",
                                               min = -15,
                                               max = 110,
                                               value = c(15,75)),
                                   
                                   # FM1 Slides
                                   
                                   sliderInput(inputId = "FMC1",
                                               label = "1 Hr Fuel Moisture (%):",
                                               min = 0,
                                               max = 30,
                                               value = c(0,30)),
                                   
                                   # FM10 Slides
                                   
                                   sliderInput(inputId = "FMC10",
                                               label = "10 Hr Fuel Moisture (%)",
                                               min = 0,
                                               max = 60,
                                               value = c(0,60)),
                                   
                                   # FM10 Slides
                                   
                                   sliderInput(inputId = "FMC100",
                                               label = "100 Hr Fuel Moisture (%)",
                                               min = 0,
                                               max = 40,
                                               value = c(0,40))
                                   
                                   ),
                          tabPanel(title = "Wind",tweaks,
                                   # Wind Sliders
                                   
                                   sliderInput("wind",
                                               "Wind speeds (mph, 20 ft):",
                                               min = 0,
                                               max = 50, value = c(8,25)),
                                   
                                   # Wind Direction Check Boxes
                                   controls,
                                  # checkboxGroupInput("wind_directions", "Wind Directions:",windDirList,
                                   #                   selected = "E"),
                                   actionLink("selectall","Select All") 
                                   )
                          )
                        ),
                        
                      # Main Panel Ouput
                      
                      mainPanel(#tags$div(class="header", checked = NA,
                                      #   tags$h1("Prescription Plots")),
                                tabsetPanel(
                                  tabPanel("Hours in Prescription",
                                           plotOutput("rh_ts_sub_plot", height = "500px")),
                                  tabPanel("Months in Prescription",
                                           plotOutput("rhplot")),
                                  tabPanel("Calendar",
                                           plotOutput("calendarPlot")),
                                  tabPanel("Table",
                                           div(DT::dataTableOutput("prescriptionTable"), style = "font-size: 75%; width: 500px"))
                                )
                                )
                      )
                    ),
           tabPanel(title = "Generate Reports", value = "Reports",
                    sidebarLayout(
                    sidebarPanel(includeMarkdown("includedText/reportInfo.md")),
                    mainPanel(textInput(inputId = "ProjectTitle",
                                        label = "Enter Project Title"),
                              downloadButton(outputId = "downloadReport",
                                             label = "Download Report")
                    )
                    )
           ),
           navbarMenu("About",
                      tabPanel("Fire Weather Explorer", id = "About",
                               mainPanel(
                                 includeMarkdown("includedText/about.md")
                                 )
                               ),
                      tabPanel("Help", id = "Help",
                               mainPanel(
                                 includeMarkdown("includedText/help.md")
                               )
                      )
           )
)



