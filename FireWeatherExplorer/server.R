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


source("RawsDL.R")

server <- function(input, output, session) {
  
  observeEvent(input$pickStations, {
    
    StationID <- Larimer$STATION$STID[which(Larimer$STATION$NAME == input$station)]
    
    wx_dl <- readInWeather(StationID = StationID,
                           Start = "201101011200",
                           End = "201712312359")
    
    stationMetadata <- wxStationMetadata(StationID = StationID)
    
    if(wx_dl$SUMMARY$NUMBER_OF_OBJECTS == 0){
      showNotification("No data in this station for this period of record!")

      wx_df <- data.frame("DateTime" = as.Date(NA),
                          "FuelMoisture" = NA,
                          "Wind_Direction" = NA,
                          "Wind_Speed" = NA,
                          "Temp" = NA,
                          "RH" = NA,
                          "Year" = NA,
                          "DayOfYear" = NA)
    }

    if(wx_dl$SUMMARY$NUMBER_OF_OBJECTS > 0){
    wx_df <- data.frame("dt" = wx_dl$STATION$OBSERVATIONS$date_time,
                       "fuel_moisture" = wx_dl$STATION$OBSERVATIONS$fuel_moisture_set_1,
                        "wind_direction" = wx_dl$STATION$OBSERVATIONS$wind_cardinal_direction_set_1d,
                        "wind_speed" = wx_dl$STATION$OBSERVATIONS$wind_speed_set_1,
                        "temp" = wx_dl$STATION$OBSERVATIONS$air_temp_set_1,
                        "rh" = wx_dl$STATION$OBSERVATIONS$relative_humidity_set_1)
    
    #wx_df[nrow(wx_df)+1,1:6] <- NA
    
    names(wx_df) <- c("DateTime","FuelMoisture","Wind_Direction","Wind_Speed","Temp","RH")
    wx_df$DateTime <- ymd_hms(wx_df$DateTime,tz = "UTC")
    attributes(wx_df$DateTime)$tzone <- "America/Denver"  
    wx_df$Week <- week(wx_df$DateTime)
    wx_df$Month <- month(wx_df$DateTime)
    wx_df$Year <- year(wx_df$DateTime)
    
    wx_df$DayOfYear <- as.Date(paste("2000-",format(wx_df$Date, "%j")), "%Y-%j")
    }
    
    output$temp_ts_plot <- renderPlot({
      tempPlot <- ggplot(data = wx_df, aes(x = DayOfYear, y = Temp))+
        geom_point(alpha = 0.25)+
        scale_x_date("Day of the Year", labels = function(x) format(x, "%d-%b"))+
        scale_y_continuous("Temperature (F)")+
        labs(title = "Temperature Records",
             subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
        facet_grid(facets = Year ~ .)+
        theme_minimal()
      tempPlot
    })
  })
  
  
  
  output$rhplot <- renderPlot({
    # Subset based on months
    wx_sub <- wx_df %>%
      filter(Month >= input$months[1]) %>%
      filter(Month <= input$months[2]) %>%
      filter(RH >= input$rh[1] & RH <= input$rh[2])
    
    # draw the histogram with the specified number of bins
    histPlot <- ggplot(data = wx_sub, aes(x = Month, y = RH))+
      geom_bar(color = "black", fill = "gray", width = 1, aes(y = (..count..)/sum(..count..)))+
      scale_x_discrete(limits = c(1,12))+
      theme_minimal()
    histPlot
    
  })
}

