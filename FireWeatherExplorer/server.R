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



server <- function(input, output, session) {
  
  # Station Diagnostics Plots -----------------------------------------------  
  observeEvent(input$pickStations, {
    
    StationID <- Larimer$STATION$STID[which(Larimer$STATION$NAME == input$station)]
    
    StartDate_unformatted <- as.Date(input$dateRange[1], origin = "1970-01-01")
    StartDate_formatted <- paste(format(StartDate_unformatted, "%Y%m%d"),"0001",sep = "")
    
    EndDate_unformatted <- as.Date(input$dateRange[2], origin = "1970-01-01")
    EndDate_formatted <- paste(format(EndDate_unformatted, "%Y%m%d"),"2300",sep = "")
    
    showNotification(paste("Searching for:\n",StartDate_formatted," - ",EndDate_formatted,sep=""))
    
    
    wx_dl <<- readInWeather(StationID = StationID,
                           Start = StartDate_formatted,
                           End = EndDate_formatted)
    
    stationMetadata <<- wxStationMetadata(StationID = StationID)
    
    if(wx_dl$SUMMARY$NUMBER_OF_OBJECTS == 0){
      showNotification(paste("No data in this station for this period of record!",StartDate_formatted,"-",EndDate_formatted,sep=""))

      wx_df <<- data.frame("DateTime" = as.Date(NA),
                          "FuelMoisture" = NA,
                          "Wind_Direction" = NA,
                          "Wind_Speed" = NA,
                          "Temp" = NA,
                          "RH" = NA,
                          "Year" = NA,
                          "DayOfYear" = NA)
    }

    if(wx_dl$SUMMARY$NUMBER_OF_OBJECTS > 0){
    wx_df <<- data.frame("dt" = wx_dl$STATION$OBSERVATIONS$date_time,
                       "fuel_moisture" = wx_dl$STATION$OBSERVATIONS$fuel_moisture_set_1,
                        "wind_direction" = wx_dl$STATION$OBSERVATIONS$wind_cardinal_direction_set_1d,
                        "wind_speed" = wx_dl$STATION$OBSERVATIONS$wind_speed_set_1,
                        "temp" = wx_dl$STATION$OBSERVATIONS$air_temp_set_1,
                        "rh" = wx_dl$STATION$OBSERVATIONS$relative_humidity_set_1)
    
    #wx_df[nrow(wx_df)+1,1:6] <- NA
    
    names(wx_df) <<- c("DateTime","FuelMoisture","Wind_Direction","Wind_Speed","Temp","RH")
    wx_df$RH <<- as.numeric(as.character(wx_df$RH))
    wx_df$DateTime <<- ymd_hms(wx_df$DateTime,tz = "UTC")
    attributes(wx_df$DateTime)$tzone <<- "America/Denver"  
    wx_df$Hour <<- hour(wx_df$DateTime)
    wx_df$Month <<- month(wx_df$DateTime)
    wx_df$Year <<- year(wx_df$DateTime)
    
    wx_df$DayOfYear <<- as.Date(paste("2000-",format(wx_df$Date, "%j")), "%Y-%j")
    
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
    }
    

  })
  
  # Subset Data Plots -----------------------------------------------
  observeEvent(input$subsetData, {
    wx_Context <- wx_df %>%
      filter(Month >= input$months[1]) %>%
      filter(Month <= input$months[2]) %>%
      mutate(Conditions = "Window")
    
    wx_Rx <- wx_Context %>%
      filter(RH >= input$rh[1] & RH <= input$rh[2]) %>%
      filter(Wind_Direction %in% input$wind_directions) %>%
      filter(Hour >= input$hours[1] & Hour <= input$hours[2]) %>%
      mutate(Conditions = "In Prescription")
    
    wx_Context <- wx_Context[,c("DateTime","Conditions")]
    wx_Rx <- wx_Rx[,c("DateTime","Conditions")]
    wx_Both <- rbind(wx_Rx,wx_Context)
    wx_Both$Conditions <- as.character(wx_Both$Conditions)
    
       
    combinedWx <- merge(wx_df,wx_Both, by = "DateTime", all.x = TRUE)
    
    
    
    combinedWx$Conditions[which(!combinedWx$Conditions %in% c("In Prescription","Window"))] <- "Not Matching"
    
    combinedWx$Conditions <- as.factor(combinedWx$Conditions)
  
    combinedWx$DayOfYear <- as.Date(paste("2000-",format(combinedWx$DateTime, "%j")), "%Y-%j")
    combinedWx$Year <- year(combinedWx$DateTime)
    
    write.csv(combinedWx,"test.txt",row.names = FALSE)
    
    
    output$rh_ts_sub_plot <- renderPlot({

      rh_sub_Plot <- ggplot(data = combinedWx, aes(x = DayOfYear, y = RH/100, size = Conditions, color = Conditions))+
        geom_point(alpha = .5, size = .5)+
        scale_x_date("Day of the Year",
                     labels = function(x) format(x, "%d-%b"))+
        scale_y_continuous("Relative Humidity (%)",
                           labels = scales::percent,
                           limits = c(0,1))+
        scale_size_manual(values = c("In Prescription" = 2,
                                     "Window" = 1,
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
      wx_sub_countHours <- combinedWx %>%
        count(Month, Conditions) %>%
        group_by(Month) %>%
        mutate(Percent = n / sum(n))
        
      
        histPlot <- ggplot(data = filter(wx_sub_countHours, Conditions == "In Prescription"),
                           aes(x = Month, y = Percent, fill = Conditions))+
          scale_y_continuous("Percent of Hours Matching Condtions (by month)",
                             labels = scales::percent)+
        geom_bar(color = "black", width = 1, stat = "identity", position="dodge")+
        scale_x_continuous(breaks = seq(1,12,1),limits = c(1,12))+
        theme_minimal()+
        theme(legend.position="none")
      histPlot
    })
    
  })
}
  
 





