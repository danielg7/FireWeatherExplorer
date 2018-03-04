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
library("hms")


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
    StartDate_formatted <- paste(input$Year[1],"01","01","1200",sep = "")
    EndDate_formatted <- paste(input$Year[2],"12","12","2300",sep = "")
    
    showNotification(paste("Searching for:\n",StartDate_formatted," - ",EndDate_formatted,sep=""))
    
    
    wx_dl <<- readInWeather(StationID = StationID,
                           Start = StartDate_formatted,
                           End = EndDate_formatted)
    
    
    wx_df <<- fxn_weatherCleaner(wx_dl)
    })
    
    
    # Plots for diagnostics
    
 if(is.null(wx_df)){}
    else{
    output$temp_ts_plot <- renderPlot({
      tempPlot <- ggplot(data = wx_df, aes(x = DayOfYear, y = Temp))+
        geom_point(alpha = 0.25, size = 0.25)+
        scale_x_date("Day of the Year", labels = function(x) format(x, "%d-%b"))+
        scale_y_continuous("Temperature (F)")+
        labs(title = "Temperature Records",
             subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
        facet_grid(facets = Year ~ .)+
        theme_bw(base_size=15, base_family="Avenir")
      tempPlot
    }, height = 100 * length(unique(wx_df$Year)), units = "px")
    
    output$rh_ts_plot <- renderPlot({
      rhPlot <- ggplot(data = wx_df, aes(x = DayOfYear, y = RH/100))+
        geom_point(alpha = 0.25, size = 0.25)+
        scale_x_date("Day of the Year",
                     labels = function(x) format(x, "%b"),date_breaks = "1 month")+
        scale_y_continuous("Relative Humidity (%)", labels = scales::percent,limits = c(0,1))+
        labs(title = "Relative Humidity Records",
             subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
        facet_grid(facets = Year ~ .)+
        theme_bw(base_size=15, base_family="Avenir")
      rhPlot
    }, height = 100 * length(unique(wx_df$Year)), units = "px")
    
    output$wind_ts_plot <- renderPlot({
      wsPlot <- ggplot(data = wx_df, aes(x = DayOfYear, y = Wind_Speed))+
        geom_point(alpha = 0.25, size = 0.25)+
        scale_x_date("Day of the Year",
                     labels = function(x) format(x, "%d-%b"))+
        scale_y_continuous("Wind Speed (mph)")+
        labs(title = "Wind Speed Records",
             subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
        facet_grid(facets = Year ~ .)+
        theme_bw(base_size=15, base_family="Avenir")
      wsPlot
    }, height = 100 * length(unique(wx_df$Year)), units = "px")
    
    }

    
    # Plots for diagnostics
    
    updateTabsetPanel(session = session, inputId =  "tabs",
                      selected = "Diagnostic")
    
  })
  
  # Subset Data Plots -----------------------------------------------
  
  wxSubsetByConditions <- reactive({
    wx_Context <- wx_df %>%
      filter(Month >= input$months[1]) %>%
      filter(Month <= input$months[2]) %>%
      filter(Hour >= input$hours[1] & Hour <= input$hours[2]) %>%
      mutate(Conditions = "Window")
    
    wx_Rx <- wx_Context %>%
      filter(RH >= input$rh[1] & RH <= input$rh[2]) %>%
      filter(Wind_Direction %in% input$wind_directions) %>%
      filter(Wind_Speed >= input$wind[1] & Wind_Speed <= input$wind[2]) %>%
      filter(Temp >= input$temp[1] & Temp <= input$temp[2]) %>%
      mutate(Conditions = "In Prescription")
    
    wx_Context <- wx_Context[,c("DateTime","Conditions")]
    wx_Rx <- wx_Rx[,c("DateTime","Conditions")]
    wx_Both <- rbind(wx_Rx,wx_Context)
    wx_Both$Conditions <- as.character(wx_Both$Conditions)
    
    
    combinedWx <<- merge(wx_df,wx_Both, by = "DateTime", all.x = TRUE)
    
    
    combinedWx$Conditions[which(!combinedWx$Conditions %in% c("In Prescription","Window"))] <<- "Not Matching"
    
    combinedWx$Conditions <<- as.factor(combinedWx$Conditions)
    
    combinedWx$DayOfYear <<- as.Date(paste("2000-",format(combinedWx$DateTime, "%j")), "%Y-%j")
    
    combinedWx$hms <<- as.POSIXct(format(combinedWx$DateTime, format = "%H:%M:%S"), format = "%H:%M:%S")
    
    combinedWx$Year <<- year(combinedWx$DateTime)
    
    #save(combinedWx,file="combinedWx.Rda")
    
    wx_sub_countHours <<- combinedWx %>%
      count(Month, Conditions) %>%
      group_by(Month) %>%
      mutate(Percent = n / sum(n))  
    
  })
 
  if(is.null(wx_df)){}
  else{ 
    output$rh_ts_sub_plot <- renderPlot({
      
      wxSubsetByConditions()
      
     lims_dt <- as.POSIXct(strptime(c(min(filter(combinedWx, Conditions %in% "Not Matching")$hms),
                                          max(filter(combinedWx, Conditions %in% "Not Matching")$hms)),
                                    format = "%Y-%m-%d %H:%M"))
     lims_d <- as.Date(c(min(wx_df$DayOfYear),
                         max(wx_df$DayOfYear)))
      

      rh_sub_Plot <- ggplot(data = filter(combinedWx, Conditions %in% "In Prescription"), aes(x = DayOfYear, y = hms))+
        annotate("rect",
                 xmin = min(filter(combinedWx, Conditions %in% "Window")$DayOfYear),
                 xmax = max(filter(combinedWx, Conditions %in% "Window")$DayOfYear),
                 ymin = min(filter(combinedWx, Conditions %in% "Window")$hms),
                 ymax = max(filter(combinedWx, Conditions %in% "Window")$hms),
                 alpha = .2, color = "green")+
        stat_density_2d(aes(fill = ..level..),
                        geom = "polygon", alpha = 0.1)+
        scale_fill_continuous("Probability Density")+
        geom_count(fill = "red", alpha = .5, pch=21)+
        scale_size_area(name = "Number of Hours In Prescription",
                        max_size = 5,
                        breaks = c(seq(1,10,1)))+
        scale_x_date("Day of the Year",
                     labels = function(x) format(x, "%b"),
                     date_breaks = "1 month",
                     limits = as.Date(c(min(wx_df$DayOfYear),
                                        max(wx_df$DayOfYear))))+
        scale_y_datetime("Hours", date_breaks = "2 hours",
                         labels = function(x) format(x, "%H%M"),
                         limits = lims_dt)+
        theme_bw(base_size=15, base_family="Avenir")+
        labs(title = "Hours That Match Prescription Parameters", subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))
      rh_sub_Plot
      })
    
    output$rhplot <- renderPlot({
      
        wxSubsetByConditions()
      
        histPlot <- ggplot(data = filter(wx_sub_countHours, Conditions == "In Prescription"),
                           aes(x = Month, y = Percent, fill = Conditions))+
          scale_y_continuous("Percent of Hours Matching Conditions (by month)",
                             labels = scales::percent)+
        geom_bar(color = "black", width = 1, stat = "identity", position="dodge")+
        scale_x_continuous(breaks = seq(1,12,1),limits = c(1,12))+
        theme_bw(base_size=15, base_family="Avenir")+
        theme(legend.position="none")
      histPlot
    })
  }   
  

  
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
    meta_LatLong <- paste("Lat / Long :",as.character(stationMetadata$STATION$LATITUDE),", ",as.character(stationMetadata$STATION$LONGITUDE),
                          sep = "")
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
  
 





