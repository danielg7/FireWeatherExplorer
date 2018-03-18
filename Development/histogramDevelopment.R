graphHistIQR <- function(Values, Years, StationID, ValueName){
  nonZeroValues <- Values[Values > 0]
  
  TukeyHigh <- mean(nonZeroValues, na.rm = TRUE) + 1.5 * IQR(nonZeroValues, na.rm = TRUE)
  TukeyLow <- mean(nonZeroValues, na.rm = TRUE) - 1.5 * IQR(nonZeroValues, na.rm = TRUE)
  
  print(mean(nonZeroValues, na.rm = TRUE))
  print(TukeyHigh)
  print(TukeyLow)
  
  plotDF <- data.frame("Values" = nonZeroValues)
  
  histplot <- ggplot(data = plotDF, aes(x = Values))+
    geom_histogram(binwidth = 0.01)+
    geom_vline(xintercept = TukeyHigh, color = "red")+
    geom_vline(xintercept = TukeyLow, color = "red")
  
  return(histplot)
}

graphHistIQR(test$HourlyRainfall)
