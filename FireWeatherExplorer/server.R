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

server <- function(input, output) {
  wx_df$DayOfYear <- as.Date(paste("2000-",format(wx_df$Date, "%j")), "%Y-%j")
  
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

