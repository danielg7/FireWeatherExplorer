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
#library("highcharter")



source("./global.R")


server <- function(input, output, session) {
  #
  # Create reactives
  #
  
  wxSubsetByConditions <- reactive({
    
    # if(input$pickStations){
    
    
    
    #
    # Filter based on month and hour to make the "prescribed burn window" (wx_Context)
    #
    
    print("Subsetting data based on filters.", quote = FALSE)
    
    
    wx_Context <- wx_df %>%
      filter(Month >= input$months[1]) %>%
      filter(Month <= input$months[2]) %>%
      filter(Hour >= input$hours[1] & Hour <= input$hours[2]) %>%
      mutate(Conditions = "Window")
    
    print("Done.", quote = FALSE)
    
    
    #
    # Filter the "prescribed burn window" (wx_Context) based on fire weather variables
    # This will determine prescription areas.
    #
    
    print("Filtering further based on the window...", quote = FALSE)
    
    
    wx_Rx <- wx_Context %>%
      filter(RH >= input$rh[1] & RH <= input$rh[2]) %>%
      filter(Wind_Direction %in% input$wind_directions) %>%
      filter(Wind_Speed >= input$wind[1] & Wind_Speed <= input$wind[2]) %>%
      filter(Temp >= input$temp[1] & Temp <= input$temp[2]) %>%
      filter(FuelMoisture_1hr >= input$FMC1[1] & FuelMoisture_1hr <= input$FMC1[2]) %>%
      filter(FuelMoisture_10hr >= input$FMC10[1] & FuelMoisture_10hr <= input$FMC10[2]) %>%
      filter(FuelMoisture_100hr >= input$FMC100[1] & FuelMoisture_100hr <= input$FMC100[2]) %>%
      mutate(Conditions = "In Prescription")
    
    print("Done.", quote = FALSE)
    
    
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
    
    
    combinedWx
  })
  
  wx_sub_test <- reactive({
    wxSubsetByConditions() %>%
      mutate(Month = as.factor(Month)) %>%
      #mutate(Month = factor(Month,levels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))) %>%
      mutate(Yday = yday(DateTime)) %>%
      mutate(weekday = wday(DateTime)) %>%
      mutate(weekdayf = weekdays(DateTime, abbreviate = TRUE)) %>%
      mutate(week = as.numeric(format(DateTime,"%W"))) %>%
      group_by(Month) %>%
      mutate(monthweek = 1 + week - min(week)) %>%
      ungroup() %>%
      count(Day, Month, Conditions) %>%
      group_by(Day) %>%
      mutate(Percent = n / sum(n)) %>%
      filter(Conditions %in% c("In Prescription","Window")) %>%
      ungroup()
  })
  
  # Draw the map
  
  output$station_location <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = providers$OpenTopoMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMiniMap(
        tiles = providers$OpenTopoMap,
        toggleDisplay = TRUE)
    
  })
  
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
                  selected = County_List[2])
      
    })
    output$station <- renderUI({
      
      #
      # Once the 'state' input changes, add stations to the list by pulling them from the station list relative to the first county
      #
      
      County_List <- sort(as.character(unique(AllRAWS$STATION$COUNTY[which(AllRAWS$STATION$STATE == input$State)])))
      
      Station_ID <- AllRAWS$STATION$STID[which(AllRAWS$STATION$STATE == input$State & AllRAWS$STATION$COUNTY == County_List[1])]
      Station_Names <- AllRAWS$STATION$NAME[which(AllRAWS$STATION$STATE == input$State & AllRAWS$STATION$COUNTY == County_List[1])]
      Station_Types <- AllRAWS$STATION$SHORTNAME[which(AllRAWS$STATION$STATE == input$State & AllRAWS$STATION$COUNTY == County_List[1])]
      Station_NewNames <- paste(Station_Names,Station_Types,sep = " ")
      Station_List <<- setNames(Station_ID,Station_NewNames)
      
      if(is.null(Stations_In_County)){
        Stations_In_County <<- data.frame("Station_ID" = as.character(Station_ID),
                                          "Station_NewNames" = as.character(Station_NewNames),
                                          "Station_Lat"= as.numeric(as.character(AllRAWS$STATION$LATITUDE[which(AllRAWS$STATION$STATE == input$State & AllRAWS$STATION$COUNTY == input$County)])),
                                          "Station_Long" = as.numeric(as.character(AllRAWS$STATION$LONGITUDE[which(AllRAWS$STATION$STATE == input$State & AllRAWS$STATION$COUNTY == input$County)])),
                                          "Color" = "Red")
      }
      
      if(is.null(Stations_In_County)){
        warning("No stations found in county.")
      }
      
      if(nrow(Stations_In_County) > 1){
        maxLong <- max(Stations_In_County$Station_Long)
        maxLat <- max(Stations_In_County$Station_Lat)
        minLong <- min(Stations_In_County$Station_Long)
        minLat <- min(Stations_In_County$Station_Lat)
        
        print("Multiple stations.", quote = FALSE)
      }
      
      if(nrow(Stations_In_County) == 1){
        maxLong <- max(Stations_In_County$Station_Long)+0.25
        maxLat <- max(Stations_In_County$Station_Lat)+.25
        minLong <- min(Stations_In_County$Station_Long)-0.25
        minLat <- min(Stations_In_County$Station_Lat)-0.25
        
        print("Single station.", quote = FALSE)
      }
      
      leafletProxy(mapId = "station_location") %>%
        addMarkers(label = as.character(Stations_In_County$Station_NewNames),
                   lat = as.numeric(as.character(Stations_In_County$Station_Lat)),
                   lng = as.numeric(as.character(Stations_In_County$Station_Long)),
                  # color = Stations_In_County$Color,
                   labelOptions = labelOptions(noHide = TRUE, direction = "bottom")) %>%
        fitBounds(minLong,minLat,maxLong,maxLat) 
      
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
      
      #Station_List <- unique(AllRAWS$STATION$NAME[which(AllRAWS$STATION$STATE == input$State & AllRAWS$STATION$COUNTY == input$County)])
      Station_ID <- AllRAWS$STATION$STID[which(AllRAWS$STATION$STATE == input$State & AllRAWS$STATION$COUNTY == input$County)]
      Station_Names <- AllRAWS$STATION$NAME[which(AllRAWS$STATION$STATE == input$State & AllRAWS$STATION$COUNTY == input$County)]
      Station_Types <- AllRAWS$STATION$SHORTNAME[which(AllRAWS$STATION$STATE == input$State & AllRAWS$STATION$COUNTY == input$County)]
      Station_NewNames <- paste(Station_Names,Station_Types,sep = " ")
      Station_List <<- setNames(Station_ID,Station_NewNames)
      
      Stations_In_County <<- data.frame("Station_ID" = as.character(Station_ID),
                                        "Station_NewNames" = as.character(Station_NewNames),
                                        "Station_Lat"= as.numeric(as.character(AllRAWS$STATION$LATITUDE[which(AllRAWS$STATION$STATE == input$State & AllRAWS$STATION$COUNTY == input$County)])),
                                        "Station_Long" = as.numeric(as.character(AllRAWS$STATION$LONGITUDE[which(AllRAWS$STATION$STATE == input$State & AllRAWS$STATION$COUNTY == input$County)])),
                                        "Color" = "red")
      
      #Stations_In_County$Color <- "Blue"
      #Stations_In_County[which(Stations_In_County$Station_ID == Station_ID),]$Color <- "Red"
      
      if(is.null(Stations_In_County)){
        warning("No stations found in county.")
      }
      
      if(nrow(Stations_In_County) > 1){
        maxLong <- max(Stations_In_County$Station_Long)
        maxLat <- max(Stations_In_County$Station_Lat)
        minLong <- min(Stations_In_County$Station_Long)
        minLat <- min(Stations_In_County$Station_Lat)
        
        print("Multiple stations.", quote = FALSE)
      }
      
      if(nrow(Stations_In_County) == 1){
        maxLong <- max(Stations_In_County$Station_Long)+0.25
        maxLat <- max(Stations_In_County$Station_Lat)+.25
        minLong <- min(Stations_In_County$Station_Long)-0.25
        minLat <- min(Stations_In_County$Station_Lat)-0.25
        
        print("Single station.", quote = FALSE)
      }
      
      leafletProxy(mapId = "station_location") %>%
        addMarkers(label = as.character(Stations_In_County$Station_NewNames),
                   #color = Stations_In_County$Color,
                   lat = as.numeric(as.character(Stations_In_County$Station_Lat)),
                   lng = as.numeric(as.character(Stations_In_County$Station_Long)),
                   labelOptions = labelOptions(noHide = TRUE, direction = "bottom")) %>%
        fitBounds(minLong,minLat,maxLong,maxLat) 
      
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
        
        # TODO: Update call from here
        
        wx_dl <<- readInWeather(token = token,
                                StationID = StationID,
                                State = input$State,
                                County = input$County,
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
        # Pass the output data from the reader function to a cleaner function.
        #
        
        incProgress(amount = .10,
                    message = "Calculating GSI...")
        
        
        GSIOutput <<- calcGSI(DateTime = wx_df$DateTime,
                              Temp = wx_df$Temp,
                              RH = wx_df$RH,
                              Latitude = as.numeric(stationMetadata$STATION$LATITUDE))
        
        seasonDF <<- findGreenupDates(Year = GSIOutput$Year,
                                      Yday = GSIOutput$Yday,
                                      GSIOutput$rollGSI)
        
        #
        # Final increment...let them know it's done!
        #
        
        incProgress(amount = .15,
                    message = "Done!")
        
        #wxSubsetByConditions()
        
        source("Figures/timeseriesPlots.R")
        source("Figures/monthlyPlots.R")
        source("Figures/hourlyPlots.R")
        #source("Figures/prescriptionPlots.R")
        
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
      
      # Output the plot as defined in Figures/timeseriesPlots.R
      
      plot_timeseries_temperature
      
    }, height = 100 * length(unique(wx_df$Year)), units = "px") # Scale the size of the plots with the number of years to plot
    
    #
    # Rainfall Timeseries Plot
    #
    
    output$precip_ts_plot <- renderPlot({
      validate(
        need(input$pickStations, 'Please select a station!')
      )
      
      # Output the plot as defined in Figures/timeseriesPlots.R
      
      plot_timeseries_precipitation
      
    }, height = 100 * length(unique(wx_df$Year)), units = "px") # Scale the size of the plots with the number of years to plot
    
    
    #
    # RH Time Series Pots
    #
    
    output$rh_ts_plot <- renderPlot({
      validate(
        need(input$pickStations, 'Please select a station!')
      )
      
      # Output the plot as defined in Figures/timeseriesPlots.R
      
      plot_timeseries_rh
      
    }, height = 100 * length(unique(wx_df$Year)), units = "px") # Dynamically scale size of plot
    
    #
    # 1 Hr Fuel Moisture Time Series Plots
    #
    
    output$fmc1_ts_plot <- renderPlot({
      validate(
        need(input$pickStations, 'Please select a station!')
      )
      
      # Output the plot as defined in Figures/timeseriesPlots.R
      
      plot_timeseries_fmc1
      
    }, height = 100 * length(unique(wx_df$Year)), units = "px") # Dynamically scale size of plot
    
    #
    # 10 Hr Fuel Moisture Time Series Pots
    #
    
    output$fmc10_ts_plot <- renderPlot({
      validate(
        need(input$pickStations, 'Please select a station!')
      )
      
      plot_timeseries_fmc10
      
    }, height = 100 * length(unique(wx_df$Year)), units = "px") # Dynamically scale size of plot
    
    
    #
    # Wind speed time series plot
    #
    
    output$wind_ts_plot <- renderPlot({
      validate(
        need(input$pickStations, 'Please select a station!')
      )
      
      plot_timeseries_windspeed
      
    }, height = 150 * length(unique(wx_df$Year)), units = "px")
    
    #
    # Wind speed time series plot
    #
    
    output$gsi_ts_plot <- renderPlot({
      validate(
        need(input$pickStations, 'Please select a station!')
      )
      
      plot_timeseries_GSI
      
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
      
      # Output the plot as defined in Figures/monthlyPlots.R
      
      month_temp_plot
      
    })
    
    #
    # Monthly RH plot
    #
    
    output$month_rh <- renderPlot({
      validate(
        need(input$pickStations, 'Please select a station!')
      )
      
      # Output the plot as defined in Figures/monthlyPlots.R
      
      month_rh_plot
      
    })
    
    #
    # Monthly wind speed plots
    #
    
    output$month_wind <- renderPlot({
      validate(
        need(input$pickStations, 'Please select a station!')
      )
      
      # Output the plot as defined in Figures/monthlyPlots.R
      
      month_wind_plot
      
    })
    
    #
    # Hourly Temperature Plots
    #
    
    output$hour_temp <- renderPlot({
      validate(
        need(input$pickStations, 'Please select a station!')
      )
      
      # Output the plot as defined in Figures/hourlyPlots.R
      
      hour_temp_plot
      
    })
    
    #
    # Hourly RH Plots
    #
    
    output$hour_rh <- renderPlot({
      validate(
        need(input$pickStations, 'Please select a station!')
      )
      
      # Output the plot as defined in Figures/hourlyPlots.R
      
      hour_rh_plot
      
    })
    
    #
    # Hourly Windspeed Plots
    #
    
    output$hour_wind <- renderPlot({
      validate(
        need(input$pickStations, 'Please select a station!')
      )
      
      # Output the plot as defined in Figures/hourlyPlots.R
      
      hour_wind_plot
      
    })
    
    
    
    #
    # Shift focus after drawing station
    #
    
    updateTabsetPanel(session = session, inputId =  "tabs",
                      selected = "Diagnostic")
    
    #wxSubsetByConditions()
    
  })
  
  # Subset Data Plots -----------------------------------------------
  
  #
  # Clean Data Based on Prescription
  #
  
  
  
  output$rh_ts_sub_plot <- renderPlot({
    validate(
      need(input$pickStations, 'Please select a station!')
    )
    
    dataToUse <- wxSubsetByConditions()
    
    #print(head(dataToUse))
    
    countRange <- dataToUse %>%
      filter(Conditions %in% "In Prescription") %>%
      group_by(Month, Hour) %>%
      summarise(Count = n())
    
    if(is.finite(max(countRange$Count,na.rm = TRUE)))
      maxRange <- max(countRange$Count,na.rm = TRUE)
    else{
      maxRange <- 2
    }
    
    # Output the plot as defined in Figures/prescriptionPlots.R
    
   # plot_rx_month_by_hour
    
    print("Plotting rx month by hour...", quote = FALSE)
    
    #print(head(combinedWx), quote = TRUE)
    

    
    plot_rx_month_by_hour <- ggplot(data = filter(dataToUse, Conditions %in% "In Prescription"), aes(x = Month, y = Hour))+
      annotate("rect",
               xmin = min(filter(dataToUse, Conditions %in% "Window")$Month),
               xmax = max(filter(dataToUse, Conditions %in% "Window")$Month),
               ymin = min(filter(dataToUse, Conditions %in% "Window")$Hour),
               ymax = max(filter(dataToUse, Conditions %in% "Window")$Hour),
               alpha = .2, color = "green")+
      stat_density_2d(aes(fill = ..level..),
                      geom = "polygon", alpha = 0.1)+
      scale_fill_continuous("Probability Density", guide = FALSE)+
      geom_count(fill = "red", alpha = .5, pch=21)+
      scale_size_area(name = "Number of Hours In Prescription\nAcross Period of Record",
                      max_size = 10)+
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
    
    plot_rx_month_by_hour
  })
  
  output$rhplot <- renderPlot({
    validate(
      need(input$pickStations, 'Please select a station!')
    )
    
    #wxSubsetByConditions()
    
    # Output the plot as defined in Figures/prescriptionPlots.R
    
    wx_sub_countHours <<- wxSubsetByConditions() %>%
      count(Month, Conditions) %>%
      group_by(Month) %>%
      mutate(Percent = n / sum(n))
    
    plot_rx_histogram_by_month <- ggplot(data = filter(wx_sub_countHours, Conditions == "In Prescription"),
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
    
    plot_rx_histogram_by_month
  })
  

  
  output$calendarPlot <- renderPlot({
    validate(
      need(input$pickStations, 'Please select a station!')
    )
    
    #wxSubsetByConditions()
    
    #print("Calculating ")
    
   
    
    #print(levels(wx_sub_test$Month))
    #levels(wx_sub_test$Month) <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
    #wx_sub_test$weekdayf <- as.factor(wx_sub_test$weekdayf)
    #wx_sub_test$weekdayf <- factor(x = wx_sub_test$weekdayf, levels = rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    

    
    # Output the plot as defined in Figures/prescriptionPlots.R
    
    windowCount <- wx_sub_test() %>%
      filter(Conditions == "Window")
    
    prescriptionCount <- wx_sub_test() %>%
      filter(Conditions == "In Prescription")
    
    plot_rx_percent_hours_by_month <- ggplot(prescriptionCount, aes(x = Month, y = Day, fill = Percent*100)) + 
      geom_tile(fill = "gray", data = windowCount, aes(Month, Day), size = 0.25, colour = "white")+
      geom_tile(colour="white", size = 0.25) + 
      # facet_grid(. ~ Month)+
 #     coord_fixed()+
      coord_flip()+
      scale_fill_viridis_c("Percent of Hours\nin Prescription",direction = 1)+
      xlab("Month") + ylab("Day of Month")+
      scale_y_continuous(breaks =  seq(1,31,1))+
      scale_x_discrete(limits = rev(levels(prescriptionCount$Month)))+
      guides("Percent of Hours in Prescription",
             size = guide_legend(ncol = 8),
             fill = guide_colourbar(title.position="top",
                                    title.hjust = 0.5,
                                    barwidth = 10))+
      labs(title = "Percent of Hours That Match Prescription Parameters Per Day of the Year", subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
      theme_bw(base_size=15, base_family="Avenir")+
      theme(legend.position="bottom",
            legend.direction = "horizontal",
            plot.background = element_blank(),
            panel.grid = element_blank())#,
    #panel.border = element_blank()
    plot_rx_percent_hours_by_month
  })
  
  output$prescriptionTable = DT::renderDataTable({
    validate(
      need(input$pickStations, 'Please select a station!')
    )
    
    #wxSubsetByConditions()
    
    output <- wxSubsetByConditions() %>%
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
    
    print("Selected station:",quote = FALSE)
    
    
    print(input$station)
    
    StationID <<- AllRAWS$STATION$STID[which(AllRAWS$STATION$STID == input$station & AllRAWS$STATION$COUNTY == input$County)]
    
    
    print("Fetching station metadata...",quote = FALSE)
    
    stationMetadata <<- wxStationMetadata(token = token, StationID = StationID,state = input$State)
    
    print("Returned data:")
    print(stationMetadata)
    print(str(stationMetadata))
    print(stationMetadata$STATION$PERIOD_OF_RECORD)
    
    print("Done.",quote = FALSE)
    
    
    print("Drawing the leaflet map...", quote = FALSE)
    
    
    
    print("Checking station count in county to determine zoom box...", quote = FALSE)
    
    if(is.null(Stations_In_County)){
      warning("No stations found in county.")
    }
    
    if(nrow(Stations_In_County) > 1){
      maxLong <- max(Stations_In_County$Station_Long)
      maxLat <- max(Stations_In_County$Station_Lat)
      minLong <- min(Stations_In_County$Station_Long)
      minLat <- min(Stations_In_County$Station_Lat)
      
      print("Multiple stations.", quote = FALSE)
    }
    
    if(nrow(Stations_In_County) == 1){
      maxLong <- max(Stations_In_County$Station_Long)+0.25
      maxLat <- max(Stations_In_County$Station_Lat)+.25
      minLong <- min(Stations_In_County$Station_Long)-0.25
      minLat <- min(Stations_In_County$Station_Lat)-0.25
      
      print("Single station.", quote = FALSE)
    }
    
    leafletProxy(mapId = "station_location") %>%
      addMarkers(label = as.character(Stations_In_County$Station_NewNames),
                 lat = as.numeric(as.character(Stations_In_County$Station_Lat)),
                 lng = as.numeric(as.character(Stations_In_County$Station_Long)),
               #  color = Stations_In_County$Color,
                 labelOptions = labelOptions(noHide = TRUE, direction = "bottom")) %>%
      fitBounds(minLong,minLat,maxLong,maxLat) 
    
    
    
    print("Done.", quote = FALSE)
    
    
    # Change the period of record indicator
    
    output$POR <- renderUI({
      sliderInput(label = 'Select Period of Record',
                  inputId = "Year",
                  min = min(stationMetadata$STATION$PERIOD_OF_RECORD),
                  max = max(stationMetadata$STATION$PERIOD_OF_RECORD),
                  value = c(min(stationMetadata$STATION$PERIOD_OF_RECORD)+1,
                            max(stationMetadata$STATION$PERIOD_OF_RECORD)),
                  sep = "")
    })
    
    
    
    
  })
  
  observeEvent(input$station_location_marker_click, {
    click <- input$station_location_marker_click
    click$lat <- round(as.numeric(click$lat), 6)
    click$lng <- round(as.numeric(click$lng), 6)
    
    print(paste("Clicked: ",click$lng,", ",click$lat), sep = "")
    # print(str(click))
    
    #  print(Stations_In_County)
    # print(str(Stations_In_County))
    
    #ClickedStationID <- as.character(Stations_In_County$Station_ID[which(Stations_In_County$Station_Lat == click$lat & Stations_In_County$Station_Long == click$lng)])
    ClickedStationID <- AllRAWS$STATION$STID[which(as.numeric(as.character(AllRAWS$STATION$LATITUDE)) == click$lat & as.numeric(as.character(AllRAWS$STATION$LONGITUDE)) == click$lng)]
    ClickedStationState <- AllRAWS$STATION$STATE[which(AllRAWS$STATION$STID == ClickedStationID)]
    ClickedStationCounty <- AllRAWS$STATION$COUNTY[which(AllRAWS$STATION$STID == ClickedStationID)]
    
    print(paste(ClickedStationID,ClickedStationState,ClickedStationCounty))
    
    StationID <<- AllRAWS$STATION$STID[which(AllRAWS$STATION$STID == ClickedStationID & AllRAWS$STATION$COUNTY == ClickedStationCounty)]
    
    print(paste("Redefined stationID from click:",StationID,sep = " "))
    
    stationMetadata <<- wxStationMetadata(token = token, StationID = StationID, state=input$State)
    
    County_List <- sort(as.character(unique(AllRAWS$STATION$COUNTY[which(AllRAWS$STATION$STATE == ClickedStationState)])))
    
    
    #Station_Names <- AllRAWS$STATION$NAME[which(AllRAWS$STATION$STATE == ClickedStationState & AllRAWS$STATION$COUNTY == ClickedStationCounty)]
    #Station_Types <- AllRAWS$STATION$SHORTNAME[which(AllRAWS$STATION$STATE == ClickedStationState & AllRAWS$STATION$COUNTY == ClickedStationCounty)]
    #Station_NewNames <- paste(Station_Names,Station_Types,sep = " ")
    #Station_List <- setNames(object = Station_ID,Station_NewNames)
    
    updateSelectInput(session = session,
                      inputId = "station",
                      label = "Select Stations",
                      choices = Station_List, 
                      selected = StationID)
    
    updateSliderInput(session = session,
                      label = 'Select Period of Record',
                      inputId = "Year",
                      min = min(stationMetadata$STATION$PERIOD_OF_RECORD),
                      max = max(stationMetadata$STATION$PERIOD_OF_RECORD),
                      value = c(min(stationMetadata$STATION$PERIOD_OF_RECORD)+1,
                                max(stationMetadata$STATION$PERIOD_OF_RECORD)))

    
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








