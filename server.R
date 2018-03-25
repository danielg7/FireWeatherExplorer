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
library("testthat")


server <- function(input, output, session) {

# React to ‘State’ changing -----------------------------------------------
  
  observeEvent(input$State, {
    output$County <- renderUI({
      
      #
      # Once the state input changes, add counties to the list by pulling them from the station list
      #
      
      County_List <- sort(as.character(unique(AllRAWS$STATION$COUNTY[which(AllRAWS$STATION$STATE == input$State)])))

      selectInput(inputId = "County",
                  label = "Select County",
                  choices = County_List,
                  selected = County_List[1])
      
      })
    output$station <- renderUI({
      
      #
      # Once the 'state' input changes, add stations to the list by pulling them from the station list relative to the first county
      #
      County_List <- sort(as.character(unique(AllRAWS$STATION$COUNTY[which(AllRAWS$STATION$STATE == input$State)])))
      
      
      Station_List <- unique(AllRAWS$STATION$NAME[which(AllRAWS$STATION$STATE == input$State & AllRAWS$STATION$COUNTY == County_List[1])])
      
      selectInput(label = "Select Stations",
                  inputId = "station",
                  choices = Station_List,
                  selected = Station_List[1])
    })
  })
  

# React to ‘County’ changing ----------------------------------------------

  
  observeEvent(input$County, {
    output$station <- renderUI({
      
      #
      # Once the 'County' input changes, add stations to the list by pulling them from the station list
      #
      
      Station_List <- unique(AllRAWS$STATION$NAME[which(AllRAWS$STATION$STATE == input$State & AllRAWS$STATION$COUNTY == input$County)])

      selectInput(label = "Select Stations",
                  inputId = "station",
                  choices = Station_List,
                  selected = Station_List[1])
    })
  })
  

# React to ‘Submit’ button pressed for picking stations -------------------
  
  observeEvent(input$pickStations, {
    
    #
    # Add a progress bar to tell you that the data are downloading
    #
 
    withProgress(message = 'Downloading station data...', value = 0, {
      
      #
      # 'Isolate' the station data so that it only gets called when the buton is pressed
      #
      
      isolate({
        
        
        #
        # Increment the progress bar a little
        #
        
        incProgress(amount = .5)
        
        #
        # Pass stationID and parsed dates to the readInWeather function (see RawsDL.R for more)
        #
        
        wx_dl <<- readInWeather(StationID = StationID,
                           Start = input$Year[1],
                           End = input$Year[2])
        
        #
        # Increment the progress bar a little
        #
        
        incProgress(amount = .25,
                message = "Cleaning data...")
        
        #
        # Pass the output data from the reader function to a cleaner function.
        #
        
        wx_df <<- fxn_weatherCleaner(wx_dl)
        
        #
        # Final increment...let them know it's done!
        #
        
        incProgress(amount = .25,
                message = "Done!")
        
        })
    })
    
    # Data Quality Plots ------------------------------------------------------
    
      #
      # Temperature Timeseries Plot
      #
      
      output$temp_ts_plot <- renderPlot({
        validate(
          need(input$pickStations, 'Please select a station!')
        )
        
        tempPlot <- ggplot(data = wx_df,
                           aes(x = DayOfYear,
                               y = Temp))+
          geom_point(alpha = 0.25, size = 0.25)+
          scale_x_date("Day of the Year", labels = function(x){format(x, "%b")})+
          scale_y_continuous("Temperature (F)")+
          labs(title = "Temperature Records",
               subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
          facet_grid(facets = Year ~ .)+
          theme_bw(base_size=15,
                   base_family="Avenir")
        tempPlot
        }, height = 100 * length(unique(wx_df$Year)), units = "px") # Scale the size of the plots with the number of years to plot
    
      #
      # Temperature Timeseries Plot
      #
      
      output$precip_ts_plot <- renderPlot({
        validate(
          need(input$pickStations, 'Please select a station!')
        )
        
        precipPlot <- ggplot(data = wx_df,
                           aes(x = DayOfYear,
                               y = HourlyRainfall))+
          geom_point(alpha = 0.25, size = 0.25)+
          scale_x_date("Day of the Year", labels = function(x){format(x, "%b")})+
          scale_y_continuous("Hourly Precipitation (hr)")+
          labs(title = "Precipitation",
               subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
          facet_grid(facets = Year ~ .)+
          theme_bw(base_size=15,
                   base_family="Avenir")
        precipPlot
      }, height = 100 * length(unique(wx_df$Year)), units = "px") # Scale the size of the plots with the number of years to plot
      
      
      #
      # RH Time Series Pots
      #
      
      output$rh_ts_plot <- renderPlot({
        validate(
          need(input$pickStations, 'Please select a station!')
        )
        
        rhPlot <- ggplot(data = wx_df,
                         aes(x = DayOfYear,
                             y = RH/100))+
          geom_point(alpha = 0.25, size = 0.25)+
          scale_x_date("Day of the Year",
                       labels = function(x) format(x, "%b"),
                       date_breaks = "1 month")+
          scale_y_continuous("Relative Humidity (%)",
                             labels = scales::percent,limits = c(0,1))+
          labs(title = "Relative Humidity Records",
               subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
          facet_grid(facets = Year ~ .)+
          theme_bw(base_size=15,
                   base_family="Avenir")
        rhPlot
        }, height = 100 * length(unique(wx_df$Year)), units = "px") # Dynamically scale size of plot
      
      #
      # 1 Hr Fuel Moisture Time Series Pots
      #
      
      output$fmc1_ts_plot <- renderPlot({
        validate(
          need(input$pickStations, 'Please select a station!')
        )
        
        FMC1_Plot <- ggplot(data = wx_df,
                         aes(x = DayOfYear,
                             y = FuelMoisture_1hr/100))+
          geom_point(alpha = 0.25, size = 0.25)+
          scale_x_date("Day of the Year",
                       labels = function(x) format(x, "%b"),
                       date_breaks = "1 month")+
          scale_y_continuous("1 Hour Fuel Moisture (%)",
                             labels = scales::percent)+
          labs(title = "1 Hr Fuel Moistures (Calculated)",
               subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
          facet_grid(facets = Year ~ .)+
          theme_bw(base_size=15,
                   base_family="Avenir")
        FMC1_Plot
      }, height = 100 * length(unique(wx_df$Year)), units = "px") # Dynamically scale size of plot
      
      #
      # 10 Hr Fuel Moisture Time Series Pots
      #
      
      output$fmc10_ts_plot <- renderPlot({
        validate(
          need(input$pickStations, 'Please select a station!')
        )
        
        if(FMCMissing == TRUE)
          FMStatement <- "Calculated"
        if(FMCMissing == FALSE)
          FMStatement <- "Instrumented"
        
        FMC10_Plot <- ggplot(data = wx_df,
                            aes(x = DayOfYear,
                                y = FuelMoisture_10hr/100))+
          geom_point(alpha = 0.25, size = 0.25)+
          scale_x_date("Day of the Year",
                       labels = function(x) format(x, "%b"),
                       date_breaks = "1 month")+
          scale_y_continuous("10 Hour Fuel Moisture (%)",
                             labels = scales::percent)+
          labs(title = paste("10 Hr Fuel Moistures (",FMStatement,")", sep = ""),
               subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
          facet_grid(facets = Year ~ .)+
          theme_bw(base_size=15,
                   base_family="Avenir")
        FMC10_Plot
      }, height = 100 * length(unique(wx_df$Year)), units = "px") # Dynamically scale size of plot
      
      
      #
      # Wind speed time series plot
      #
      
      output$wind_ts_plot <- renderPlot({
        validate(
          need(input$pickStations, 'Please select a station!')
        )
        
        wsPlot <- ggplot(data = wx_df,
                         aes(x = DayOfYear,
                             y = Wind_Speed,
                             color = as.factor(Wind_Direction)))+
          geom_point(#alpha = 0.25,
                     size = 0.25)+
          scale_color_discrete("Wind Direction")+
          scale_x_date("Day of the Year",
                       labels = function(x) format(x, "%b"),
                       date_breaks = "1 month")+
          scale_y_continuous("Wind Speed (mph)")+
          labs(title = "Wind Speed Records",
               subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
          facet_grid(facets = Year ~ .)+
          theme_bw(base_size=15,
                   base_family="Avenir")+
          guides(size = guide_legend(ncol = 8), colour = guide_legend(override.aes = list(size = 3)))+
          theme(legend.position="bottom",
                legend.direction = "horizontal")
        wsPlot
      }, height = 150 * length(unique(wx_df$Year)), units = "px")
    
      output$totalPlot = DT::renderDataTable({
                                               validate(
                                                 need(input$pickStations, 'Please select a station!')
                                               )
                                               
                                               wx_df_formatted <- wx_df %>%
                                                 select("DateTime","Temp","RH","Wind_Speed","Wind_Direction","FuelMoisture_1hr","FuelMoisture_10hr","HourlyRainfall")
                                               
                                               datatable(wx_df_formatted, width = '500px') %>% formatDate(1, method = "toLocaleString", params = list("en-US", "hourCycle: '24h'"))
                                             })
      
# Station Summary Plots ---------------------------------------------------
      
      #
      # Monthly temperature histogram
      #
      
      output$month_temp <- renderPlot({
        validate(
          need(input$pickStations, 'Please select a station!')
        )
        
        month_temp_plot <- ggplot(data = wx_df,
                                  aes(x = Month,
                                      y = Temp,
                                      group = Month))+
          geom_boxplot(position = "identity")+
          scale_x_continuous(breaks = seq(1,12,1),
                             labels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
                             limits = c(0.5,12.5))+
          scale_y_continuous("Temperature (F)")+
          labs(title = "Temperature Records by Month",
               subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
          theme_bw(base_size=15,
                   base_family="Avenir")
        month_temp_plot
      })
      
      #
      # Monthly RH plot
      #
      
      output$month_rh <- renderPlot({
        validate(
          need(input$pickStations, 'Please select a station!')
        )
        
        
        month_rh_plot <- ggplot(data = wx_df, aes(x = Month,
                                                  y = RH/100,
                                                  group = Month))+
          geom_boxplot()+
          scale_x_continuous(breaks = seq(1,12,1),
                              labels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
                              limits = c(0.5,12.5))+
          scale_y_continuous("Relative Humidity (%)",
                             labels = scales::percent,limits = c(0,1))+
          labs(title = "Relative Humidity Records by Month",
               subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
          theme_bw(base_size=15,
                   base_family="Avenir")
        month_rh_plot
      })
      
      #
      # Monthly wind speed plots
      #
      
        output$month_wind <- renderPlot({
          validate(
            need(input$pickStations, 'Please select a station!')
          )
          
          month_wind_plot <- ggplot(data = wx_df, aes(x = Month,
                                                      y = Wind_Speed,
                                                      group = Month))+
            geom_boxplot()+
            scale_x_continuous(breaks = seq(1,12,1),
                               labels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
                               limits = c(0.5,12.5))+
            scale_y_continuous("Wind Speed (mph)")+
            labs(title = "Wind Speed Records by Month",
                 subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
            theme_bw(base_size=15,
                     base_family="Avenir")
          month_wind_plot
      })
        
        #
        # Hourly Temperature Plots
        #
        
        output$hour_temp <- renderPlot({
          validate(
            need(input$pickStations, 'Please select a station!')
          )
          
          hour_temp_plot <- ggplot(data = wx_df, aes(x = Hour,
                                                     y = Temp,
                                                     group = Hour))+
            geom_boxplot()+
            scale_x_continuous("Hour",
                               breaks = seq(0,23,1),
                               limits = c(-.45,23.45),
                               labels = c("0000","0100","0200","0300","0400","0500","0600","0700","0800","0900","1000","1100",
                                          "1200","1300","1400","1500","1600","1700","1800","1900","2000","2100","2200",
                                          "2300"))+
            scale_y_continuous("Temperature (F)")+
            labs(title = "Temperature Records by Hour",
                 subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
            theme_bw(base_size=15,
                     base_family="Avenir")
          hour_temp_plot
        })
        
        #
        # Hourly RH Plots
        #
        
        output$hour_rh <- renderPlot({
          validate(
            need(input$pickStations, 'Please select a station!')
          )
          
          hour_rh_plot <- ggplot(data = wx_df, aes(x = Hour,
                                                   y = RH/100,
                                                   group = Hour))+
            geom_boxplot()+
            scale_x_continuous("Hour",
                               breaks = seq(0,23,1),
                               limits = c(-.45,23.45),
                               labels = c("0000","0100","0200","0300","0400","0500","0600","0700","0800","0900","1000","1100",
                                          "1200","1300","1400","1500","1600","1700","1800","1900","2000","2100","2200",
                                          "2300"))+
            scale_y_continuous("Relative Humidity (%)",
                               labels = scales::percent,limits = c(0,1))+
            labs(title = "Relative Humidity Records by Hour",
                 subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
            theme_bw(base_size=15,
                     base_family="Avenir")+
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
          hour_rh_plot
        })
        
        #
        # Hourly Windspeed Plots
        #
        
        output$hour_wind <- renderPlot({
          validate(
            need(input$pickStations, 'Please select a station!')
          )
          
          hour_wind_plot <- ggplot(data = wx_df, aes(x = Hour,
                                                     y = Wind_Speed,
                                                     group = Hour))+
            geom_boxplot()+
            scale_x_continuous("Hour",
                               breaks = seq(0,23,1),
                               limits = c(-.45,23.45),
                               labels = c("0000","0100","0200","0300","0400","0500","0600","0700","0800","0900","1000","1100",
                                          "1200","1300","1400","1500","1600","1700","1800","1900","2000","2100","2200",
                                          "2300"))+
            scale_y_continuous("Wind Speed (mph)")+
            labs(title = "Wind Speed Records by Hour",
                 subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
            theme_bw(base_size=15,
                     base_family="Avenir")
          hour_wind_plot
        })
      
    
    
    #
    # Shift focus after drawing station
    #
    
    updateTabsetPanel(session = session, inputId =  "tabs",
                      selected = "Diagnostic")
    
    wxSubsetByConditions()
    
  })
  
  # Subset Data Plots -----------------------------------------------
  
  #
  # Clean Data Based on Prescription
  #
  
  wxSubsetByConditions <- reactive({
    
    if(input$pickStations){
      
     
      
    #
    # Filter based on month and hour to make the "prescribed burn window" (wx_Context)
    #
    
    wx_Context <- wx_df %>%
      filter(Month >= input$months[1]) %>%
      filter(Month <= input$months[2]) %>%
      filter(Hour >= input$hours[1] & Hour <= input$hours[2]) %>%
      mutate(Conditions = "Window")
    
    #
    # Filter the "prescribed burn window" (wx_Context) based on fire weather variables
    # This will determine prescription areas.
    #
    
    wx_Rx <- wx_Context %>%
      filter(RH >= input$rh[1] & RH <= input$rh[2]) %>%
      filter(Wind_Direction %in% input$wind_directions) %>%
      filter(Wind_Speed >= input$wind[1] & Wind_Speed <= input$wind[2]) %>%
      filter(Temp >= input$temp[1] & Temp <= input$temp[2]) %>%
      filter(FuelMoisture_1hr >= input$FMC1[1] & FuelMoisture_1hr <= input$FMC1[2]) %>%
      filter(FuelMoisture_10hr >= input$FMC10[1] & FuelMoisture_10hr <= input$FMC10[2]) %>%
      mutate(Conditions = "In Prescription")
    
    #
    # Combine dataframe of values in the 
    # This will determine prescription areas.
    #
    
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
    
    wx_sub_countHours <<- combinedWx %>%
      count(Month, Conditions) %>%
      group_by(Month) %>%
      mutate(Percent = n / sum(n))
    
    
    
    } 
    else{
      return(NULL)
    }
 })
 
    output$rh_ts_sub_plot <- renderPlot({
      validate(
        need(input$pickStations, 'Please select a station!')
      )
      
      
      wxSubsetByConditions()
      
      
      
     lims_dt <- as.POSIXct(strptime(c(min(filter(combinedWx, Conditions %in% "Not Matching")$hms),
                                          max(filter(combinedWx, Conditions %in% "Not Matching")$hms)),
                                    format = "%Y-%m-%d %H:%M"))
     lims_d <- as.Date(c(min(wx_df$DayOfYear),
                         max(wx_df$DayOfYear)))
      
     countRange <- combinedWx %>%
       filter(Conditions %in% "In Prescription") %>%
       group_by(Month, Hour) %>%
       summarise(Count = n())

      rh_sub_Plot <- ggplot(data = filter(combinedWx, Conditions %in% "In Prescription"), aes(x = Month, y = Hour))+
        annotate("rect",
                 xmin = min(filter(combinedWx, Conditions %in% "Window")$Month),
                 xmax = max(filter(combinedWx, Conditions %in% "Window")$Month),
                 ymin = min(filter(combinedWx, Conditions %in% "Window")$Hour),
                 ymax = max(filter(combinedWx, Conditions %in% "Window")$Hour),
                 alpha = .2, color = "green")+
        stat_density_2d(aes(fill = ..level..),
                        geom = "polygon", alpha = 0.1)+
        scale_fill_continuous("Probability Density", guide = FALSE)+
        geom_count(fill = "red", alpha = .5, pch=21)+
        scale_size_area(name = "Number of Hours In Prescription\nAcross Period of Record",
                        max_size = 10,
                        breaks = seq(1,max(countRange$Count,na.rm = TRUE),1))+
        scale_x_continuous("Months",breaks = seq(1,12,1),
                           labels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
                           limits = c(0.5,12.5))+
        scale_y_continuous("Hour of the Day", breaks = seq(0,23,1),
                           limits = c(0,23),
                           labels = c("0000","0100","0200","0300","0400","0500","0600","0700","0800","0900","1000","1100",
                                      "1200","1300","1400","1500","1600","1700","1800","1900","2000","2100","2200",
                                      "2300"))+
        guides(size = guide_legend(ncol = 8))+
        theme_bw(base_size=15, base_family="Avenir")+
        theme(legend.position="bottom",
              legend.direction = "vertical")+
        labs(title = "Hours That Match Prescription Parameters", subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))
      rh_sub_Plot
      })
    
    output$rhplot <- renderPlot({
      validate(
        need(input$pickStations, 'Please select a station!')
      )
      
       wxSubsetByConditions()
      
        histPlot <- ggplot(data = filter(wx_sub_countHours, Conditions == "In Prescription"),
                           aes(x = Month, y = Percent, fill = Conditions))+
          scale_y_continuous("Percent of Hours Matching Conditions (by month)",
                             labels = scales::percent)+
        geom_bar(color = "black", width = 1, stat = "identity", position="dodge")+
          scale_x_continuous(breaks = seq(1,12,1),
                             labels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
                             limits = c(0.5,12.5))+
        theme_bw(base_size=15, base_family="Avenir")+
          labs(title = "Percent of Hours That Match Prescription Parameters Per Month", subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
        theme(legend.position="none")
      histPlot
    })
  
    output$prescriptionTable = DT::renderDataTable({
      validate(
        need(input$pickStations, 'Please select a station!')
      )
      
      wxSubsetByConditions()
      
      output <- combinedWx %>%
        filter(Conditions %in% "In Prescription") %>%
        select(-one_of(c("DayOfYear","Conditions","hms","Hour","Month","Year"))) %>%
        select(DateTime, Temp, RH, Wind_Speed, Wind_Direction, FuelMoisture_10hr, FuelMoisture_1hr) %>%
        rename(FM_10hr = FuelMoisture_10hr) %>%
        rename(FM_1hr = FuelMoisture_1hr) %>%
        mutate(FM_1hr = round(FM_1hr, digits = 2))
      
      datatable(output) %>% formatDate(1, method = "toLocaleString", params = list("en-US", "hourCycle: '24h'"))
    }, width = '500px')
    
  
# Change Based on Station Selection ---------------------------------------------------------

  observeEvent(input$station, {

    # Fetch metadata
    
    StationID <<- AllRAWS$STATION$STID[which(AllRAWS$STATION$NAME == input$station & AllRAWS$STATION$COUNTY == input$County)]

    
    stationMetadata <<- wxStationMetadata(StationID = StationID)
    
    # Draw the map

    output$station_location <- renderLeaflet({
      map <<- leaflet() %>%
        addProviderTiles(providers$OpenTopoMap,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(label = stationMetadata$STATION$NAME,
                   lat = as.numeric(stationMetadata$STATION$LATITUDE),
                   lng = as.numeric(stationMetadata$STATION$LONGITUDE),
                   labelOptions = labelOptions(noHide = TRUE)) %>%
        setView(lat = as.numeric(stationMetadata$STATION$LATITUDE),
                lng = as.numeric(stationMetadata$STATION$LONGITUDE),
                zoom = 13) %>%
        addMiniMap(
          tiles = providers$Esri.WorldStreetMap,
          toggleDisplay = TRUE)
      
      map
                
  })
    
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
      HTML(paste('<br/>',meta_type, meta_Range, meta_LatLong, meta_GACC, meta_FireWxZone, sep = '<br/>'))
    })
    
    # Draw the metadata title
    
    output$metadataTitle <- renderUI({
      meta_StationName <- paste(stationMetadata$STATION$NAME, " (",stationMetadata$STATION$STID,")", sep = "")
      
      HTML(paste('<h1>',meta_StationName,'</h1>',sep = ""))
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
    
    observeEvent(input$station_location_marker_click, {
      click <- input$station_location_marker_click
      
      Station_Clicked <- AllLocations[which(AllLocations$Lat == click$lat & AllLocations$Long == click$lng), ]$StationName
      State_Clicked <- AllLocations[which(AllLocations$Lat == click$lat & AllLocations$Long == click$lng), ]$State
      County_Clicked <- AllLocations[which(AllLocations$Lat == click$lat & AllLocations$Long == click$lng), ]$County
      
      #print(paste(Station_Clicked,County_Clicked,State_Clicked))
      
      #
      # Update station input
      #
      
      Station_List <- unique(AllRAWS$STATION$NAME[which(AllRAWS$STATION$STATE == State_Clicked & AllRAWS$STATION$COUNTY == County_Clicked)])
      
      updateSelectInput(session = session,
                        inputId = "station",
                        label = "Select Stations",
                        choices = Station_List, 
                        selected = Station_Clicked)
      
      #
      # Update county input
      #
      
      County_List <- sort(as.character(unique(AllRAWS$STATION$COUNTY[which(AllRAWS$STATION$STATE == State_Clicked)])))
      
      updateSelectInput(session = session,
                        inputId = "county",
                        label = "Select County",
                        choices = County_List, 
                        selected = County_Clicked)
      
      #
      # Update state input
      #
      
      updateSelectInput(session = session,
                        inputId = 'State',
                        label = "Select State",
                        choices = State_List,
                        selected = State_Clicked)
      
    })
    
    observe({
      if(input$selectall == 0) return(NULL) 
      else if (input$selectall%%2 == 0)
      {
        updateCheckboxGroupInput(session,"wind_directions","",
                                 choices = windDirList)
      }
      else
      {
        updateCheckboxGroupInput(session,"wind_directions","",
                                 choices = windDirList,
                                 selected = windDirList)
      }
    })
    

# Generate Report ---------------------------------------------------------
    
    output$downloadReport <- downloadHandler(validate(
      need(input$pickStations, 'Please select a station!')),
      filename = "Report.docx",
      content = function(file) {
        
        
        stationRxValues$Wind.Low <<- input$wind[1]
        stationRxValues$Wind.High <<- input$wind[2]
        stationRxValues$WindDirections <<- input$wind_directions
        stationRxValues$FM1.Low <<- input$FMC1[1]
        stationRxValues$FM1.High <<- input$FMC1[2]
        stationRxValues$FM10.Low <<- input$FMC10[1]
        stationRxValues$FM10.High <<- input$FMC10[2]     
        stationRxValues$Temp.Low <<- input$temp[1]
        stationRxValues$Temp.High <<- input$temp[2]
        stationRxValues$RH.Low <<- input$rh[1]
        stationRxValues$RH.High <<- input$rh[2]
        stationRxValues$Month.Low <<- input$months[1]
        stationRxValues$Month.High <<- input$months[2]
        stationRxValues$Hour.Low <<- input$hours[1]
        stationRxValues$Hour.High <<- input$hours[2]
        
        src <- normalizePath('reportWireframe.Rmd')
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        file.copy(src, 'reportWireframe.Rmd', overwrite = TRUE)
        
        library(rmarkdown)
        
        out <- rmarkdown::render("reportWireframe.Rmd",
                                 params = list(
                                   projectName = input$ProjectTitle,
                                   stationMetadata = stationMetadata,
                                   weatherDataFrame = wx_df,
                                   prescriptionRangesDataFrame = stationRxValues,
                                   prescriptionDataFrame = combinedWx))
        
        
        file.rename(out, file)
      }
    )
    

# Download CSV ------------------------------------------------------------
    
    # Downloadable csv of selected dataset ----
    output$downloadWxData <- downloadHandler(validate(
      need(input$pickStations, 'Please select a station!')),
      filename = "OutputData.csv",
      content = function(file) {
        
        wx_df_formatted <- wx_df %>%
          select("DateTime","Temp","RH","Wind_Speed","Wind_Direction","FuelMoisture_1hr","FuelMoisture_10hr","HourlyRainfall")
        
        meta_StationName <- paste("# Station Name: ", stationMetadata$STATION$NAME, " (",stationMetadata$STATION$STID,")", sep = "")
        meta_type <- paste("# Station Type:", stationMetadata$STATION$SHORTNAME)
        meta_GACC <- paste("# GACC:", stationMetadata$STATION$GACC)
        meta_FireWxZone <- paste("# NWS Fire Weather Zone:", stationMetadata$STATION$NWSFIREZONE)
        meta_Range <- paste("# Period of Record: ",
                            min(year(ymd_hms(stationMetadata$STATION$PERIOD_OF_RECORD))),
                            " - ",
                            max(year(ymd_hms(stationMetadata$STATION$PERIOD_OF_RECORD))),
                            sep = "")
        
        
        header <- paste(meta_StationName,
                        meta_type,
                        meta_GACC,
                        meta_FireWxZone,
                        meta_Range, sep = "\r\n")
        
        src <- normalizePath('OutputData.csv')
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        
        
        
        write.table(x = header,
                    file = src,
                    append = FALSE,
                    row.names = FALSE,
                    sep = ',',
                    quote = FALSE)
        
        
        write.table(x = wx_df_formatted,
                    file = src,
                    append = TRUE,
                    row.names = FALSE,
                    sep = ',',
                    quote = FALSE)
        
        file.copy(from = src, to = file, overwrite = TRUE)
        
       # file.rename(out, file)
        
        #writeWeatherData(WeatherData = wx_df_formatted, stationMetadata = stationMetadata, file = file)
      }
    )
    
}







