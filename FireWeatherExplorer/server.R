#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library("shiny")
library("ggplot2")
library("dplyr")
library("DT")
library("leaflet")



server <- function(input, output, session) {

 
  
  observeEvent(input$State, {
    output$County <- renderUI({
      selectInput("County",
                  label = "Select County",
                  choices = sort(as.character(unique(AllRAWS$STATION$COUNTY[which(AllRAWS$STATION$STATE == input$State)]))),
                  selected = "CO")
      })
  })
  
  observeEvent(input$County, {
    output$station <- renderUI({
      selectInput(label = "Select Stations",
                  inputId = "station",
                  choices = unique(AllRAWS$STATION$NAME[which(AllRAWS$STATION$STATE == input$State & AllRAWS$STATION$COUNTY == input$County)]),
                  selected = "Larimer")
    })
  })
  
  observeEvent(input$pickStations, {
 

    isolate({
   # StartDate_unformatted <- as.Date(input$dateRange[1], origin = "1970-01-01")
    StartDate_formatted <- paste(input$Year[1],"01","01","0001",sep = "")
    
    #EndDate_unformatted <- as.Date(input$dateRange[2], origin = "1970-01-01")
    EndDate_formatted <- paste(input$Year[2],"12","12","2300",sep = "")
    
    showNotification(paste("Searching for:\n",StartDate_formatted," - ",EndDate_formatted,sep=""))
    
    
    wx_dl <<- readInWeather(StationID = StationID,
                           Start = StartDate_formatted,
                           End = EndDate_formatted)
    
    
    wx_df <<- fxn_weatherCleaner(wx_dl)
    })
    

# Add new sections when button is pressed ---------------------------------

    appendTab(inputId = "tabs",
              select = TRUE, # Select the panel
              tabPanel("Diagnostic Plots", id = "Diagnostic",
                       mainPanel(
                         plotOutput("temp_ts_plot"),
                         plotOutput("rh_ts_plot"),
                         plotOutput("wind_ts_plot")
                       )))
    
    appendTab(inputId = "tabs",
              tabPanel("Subset Plots", id = "Subset",
                       tabPanel("Subset Plots", id = "Subset",
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
                                    
                                    # Wind Sliders
                                    
                                    sliderInput("wind",
                                                "Wind speeds:",
                                                min = 0,
                                                max = 50, value = c(8,12)),
                                    
                                    
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
                                                       selected = "E")
                                  ),
                                  
                                  # Output plots for subset
                                  
                                  mainPanel(plotOutput("rh_ts_sub_plot"),
                                            plotOutput("rhplot")))
                       )))
    
    
    # Plots for diagnostics
    
    output$temp_ts_plot <- renderPlot({
      tempPlot <- ggplot(data = wx_df, aes(x = DayOfYear, y = Temp))+
        geom_point(alpha = 0.25, size = 0.25)+
        scale_x_date("Day of the Year", labels = function(x) format(x, "%d-%b"))+
        scale_y_continuous("Temperature (F)")+
        labs(title = "Temperature Records",
             subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
        facet_grid(facets = Year ~ .)+
        theme_minimal()
      tempPlot
    })
    output$rh_ts_plot <- renderPlot({
      rhPlot <- ggplot(data = wx_df, aes(x = DayOfYear, y = RH/100))+
        geom_point(alpha = 0.25, size = 0.25)+
        scale_x_date("Day of the Year",
                     labels = function(x) format(x, "%d-%b"))+
        scale_y_continuous("Relative Humidity (%)", labels = scales::percent,limits = c(0,1))+
        labs(title = "Relative Humidity Records",
             subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
        facet_grid(facets = Year ~ .)+
        theme_minimal()
      rhPlot
    })
    
    output$wind_ts_plot <- renderPlot({
      wsPlot <- ggplot(data = wx_df, aes(x = DayOfYear, y = Wind_Speed))+
        geom_point(alpha = 0.25, size = 0.25)+
        scale_x_date("Day of the Year",
                     labels = function(x) format(x, "%d-%b"))+
        scale_y_continuous("Wind Speed (mph)")+
        labs(title = "Wind Speed Records",
             subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
        facet_grid(facets = Year ~ .)+
        theme_minimal()
      wsPlot
    })
    
    

  })
  
  # Subset Data Plots -----------------------------------------------
  
  wxSubsetByConditions <- reactive({
    wx_Context <- wx_df %>%
      filter(Month >= input$months[1]) %>%
      filter(Month <= input$months[2]) %>%
      mutate(Conditions = "Window")
    
    wx_Rx <- wx_Context %>%
      filter(RH >= input$rh[1] & RH <= input$rh[2]) %>%
      filter(Wind_Direction %in% input$wind_directions) %>%
      filter(Hour >= input$hours[1] & Hour <= input$hours[2]) %>%
      filter(Wind_Speed >= input$wind[1] & Wind_Speed <= input$wind[2]) %>%
      mutate(Conditions = "In Prescription")
    
    wx_Context <- wx_Context[,c("DateTime","Conditions")]
    wx_Rx <- wx_Rx[,c("DateTime","Conditions")]
    wx_Both <- rbind(wx_Rx,wx_Context)
    wx_Both$Conditions <- as.character(wx_Both$Conditions)
    
    
    combinedWx <<- merge(wx_df,wx_Both, by = "DateTime", all.x = TRUE)
    
    
    combinedWx$Conditions[which(!combinedWx$Conditions %in% c("In Prescription","Window"))] <<- "Not Matching"
    
    combinedWx$Conditions <<- as.factor(combinedWx$Conditions)
    
    combinedWx$DayOfYear <<- as.Date(paste("2000-",format(combinedWx$DateTime, "%j")), "%Y-%j")
    combinedWx$Year <<- year(combinedWx$DateTime)
    
    wx_sub_countHours <<- combinedWx %>%
      count(Month, Conditions) %>%
      group_by(Month) %>%
      mutate(Percent = n / sum(n))  
    
  })
  
    output$rh_ts_sub_plot <- renderPlot({
      
      wxSubsetByConditions()

      rh_sub_Plot <- ggplot(data = filter(combinedWx, Conditions %in% c("Window","Not Matching")), aes(x = DayOfYear, y = RH/100, size = Conditions, color = Conditions))+
        geom_point(alpha = .5)+
        geom_point(data = filter(combinedWx, Conditions %in% "In Prescription"), aes(x = DayOfYear, y = RH/100),
                   color = "red",
                   size = 2,
                   show.legend = FALSE)+
        scale_x_date("Day of the Year",
                     labels = function(x) format(x, "%d-%b"))+
        scale_y_continuous("Relative Humidity (%)",
                           labels = scales::percent,
                           limits = c(0,1))+
        scale_size_manual(values = c("Window" = 1,
                                     "Not Matching" = 1))+
        scale_color_manual(values = c("In Prescription" = "red",
                                      "Window" = "black",
                                      "Not Matching" = "gray"
                                      ))+
        labs(title = "Relative Humidity Records",
             subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
        facet_grid(facets = Year ~ .,
                   scales = "free_x")+
        theme_minimal()
      rh_sub_Plot})
    
    output$rhplot <- renderPlot({
      
        wxSubsetByConditions()
      
        histPlot <- ggplot(data = filter(wx_sub_countHours, Conditions == "In Prescription"),
                           aes(x = Month, y = Percent, fill = Conditions))+
          scale_y_continuous("Percent of Hours Matching Conditions (by month)",
                             labels = scales::percent)+
        geom_bar(color = "black", width = 1, stat = "identity", position="dodge")+
        scale_x_continuous(breaks = seq(1,12,1),limits = c(1,12))+
        theme_minimal()+
        theme(legend.position="none")
      histPlot
    })
    
  

  
# Change Based on Station Selection ---------------------------------------------------------

  observeEvent(input$station, {

    # Fetch metadata
    
    StationID <<- AllRAWS$STATION$STID[which(AllRAWS$STATION$NAME == input$station)]
    
   # StationID <<- Larimer$STATION$STID[which(Larimer$STATION$NAME == input$station)]
    stationMetadata <<- wxStationMetadata(StationID = StationID)
    
    # Draw the map
    
    output$station_location <- renderLeaflet({
      leaflet(width = 100, height = 100) %>%
        addProviderTiles(providers$OpenTopoMap,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(label = stationMetadata$STATION$NAME,
                   lat = as.numeric(stationMetadata$STATION$LATITUDE),
                   lng = as.numeric(stationMetadata$STATION$LONGITUDE))})
    
    # Draw the metadata
    
    output$metadata <- renderUI({
    meta_StationName <- paste("Station Name: ", stationMetadata$STATION$NAME, " (",stationMetadata$STATION$STID,")", sep = "")
    meta_type <- paste("Station Type:", stationMetadata$STATION$SHORTNAME)
    meta_GACC <- paste("GACC:", stationMetadata$STATION$GACC)
    meta_FireWxZone <- paste("NWS Fire Weather Zone:", stationMetadata$STATION$NWSFIREZONE)
    meta_Range <- paste("Period of Record: ",
                        min(year(ymd_hms(stationMetadata$STATION$PERIOD_OF_RECORD))),
                        " - ",
                        max(year(ymd_hms(stationMetadata$STATION$PERIOD_OF_RECORD))),
                        sep = "")
    meta_LatLong <- paste("Lat / Long:",as.character(stationMetadata$STATION$LATITUDE),",",as.character(stationMetadata$STATION$LONGITUDE))
    HTML(paste('<br/>',meta_StationName, meta_type, meta_Range, meta_LatLong, meta_GACC, meta_FireWxZone, sep = '<br/>'))
    })
    
    # Change the period of record indicator
    
    output$POR <- renderUI({
      sliderInput(label = 'Select Period of Record',
                  inputId = "Year",
                  min = min(year(ymd_hms(stationMetadata$STATION$PERIOD_OF_RECORD))),
                  max = max(year(ymd_hms(stationMetadata$STATION$PERIOD_OF_RECORD))),
                  value = c(min(year(ymd_hms(stationMetadata$STATION$PERIOD_OF_RECORD)))+1,
                            max(year(ymd_hms(stationMetadata$STATION$PERIOD_OF_RECORD)))),
                  sep = "")
      })
    
})
}
  
 





