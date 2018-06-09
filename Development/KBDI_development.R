goodWx <- readInWeather(StationID = "K28J",
                        County = "Putnam",
                        Start = 2015,
                        End = 2018)

goodWxOut <- fxn_weatherCleaner(goodWx)

testInputs <- goodWxOut %>%
  group_by(Daily = format(DateTime, "%Y-%m-%d")) %>%
  summarise(DailyMaxTemperature = max(Temp, na.rm = TRUE),
            DailyRain = sum(HourlyRainfall,na.rm = TRUE))

MAP <- goodWxOut %>%
  group_by(Year) %>%
  summarise(YearlyRain = sum(HourlyRainfall, na.rm = TRUE))
MAP_given <- mean(MAP$YearlyRain)
#SD <- "201501012300"
#ED <- "201701012300"





calcKBDI <- function(DateTime,
                     Temp,
                     HourlyRainfall,startValue = 0){
  
  # First, munge data into format that works
  
  InputDF <- data.frame(DateTime, Temp, HourlyRainfall)
  
  DailyInputs <- InputDF %>%
    group_by(Daily = format(DateTime, "%Y-%m-%d")) %>%
    summarise(DailyMaxTemperature = max(Temp, na.rm = TRUE),
              DailyRain = sum(HourlyRainfall,na.rm = TRUE))
  
  MAP <- InputDF %>%
    group_by(Yearly = format(DateTime, "%Y")) %>%
    summarise(YearlyRain = sum(HourlyRainfall, na.rm = TRUE))
  MAP_given <- mean(MAP$YearlyRain)
  
  
  
  DailyInputs$kbdi <- c(startValue,rep(NA, length(DailyInputs$DailyRain)-1))

  
  #Q <- c(0, rep(NA, (length(DailyRain) - 1)))
  dQ <- rep(NA, (length(DailyInputs$DailyRain))) 
  Ep <- rep(NA, (length(DailyInputs$DailyRain))) 
  
  for(i in 2:length(DailyInputs$DailyRain)){
    
      if (DailyInputs$DailyRain[i] > 0) {
        Net <- DailyInputs$DailyRain[i] - 0.2
        
        if(Net > 0)
          DailyInputs$kbdi[i] <- DailyInputs$kbdi[i-1] - Net*100
        else{
          DailyInputs$kbdi[i] <- DailyInputs$kbdi[i-1]
        }
      } else {
        DailyInputs$kbdi[i] <- DailyInputs$kbdi[i-1]
      }  
      
    #kbdi[i] <-  (800 - kbdi[i]) * ((0.968*exp(0.0486 * DailyMaxTemperature[i]) - 8.3) * 10^-3)/(1 + 10.88 * exp(-0.0441 * MeanAnnualPrecipitation))
    Ep[i] <- ((.968 * exp(.0486 * DailyInputs$DailyMaxTemperature[i]) - 8.30) * .001) / (1 + (10.88 * exp(-.0441 * MAP_given))) 
    dQ[i] <- (800 - DailyInputs$kbdi[i]) * Ep[i]
    DailyInputs$kbdi[i] <- DailyInputs$kbdi[i] + dQ[i]
  }
  DailyInputs$kbdi[which(DailyInputs$kbdi < 0)] <- 0
  #kbdi <- (kbdi / 100) / .0393700787402
  

  return(DailyInputs)
}


output <- calcKBDI(DateTime = goodWxOut$DateTime,
                   Temp = goodWxOut$Temp,
                   HourlyRainfall = goodWxOut$HourlyRainfall, startValue = 0)

output$Daily <- ymd(output$Daily)

#df <- data.frame(x = seq(1,length(output),1), y = output)

  
ggplot(output, aes(x = Daily, y = kbdi))+geom_line()




FOLOWO <- read.table(file = "Development/FOLOWO_2005-2006_KBDI.txt",
           skip = 29,
           sep = "")
FOLOWO <- FOLOWO[c("V1","V2","V5")]
names(FOLOWO) <- c("Date","Time","KBDI")
FOLOWO$Date <- mdy(as.character(FOLOWO$Date))

ggplot(output, aes(x = Daily, y = kbdi))+geom_line()+
  geom_line(color = "red", data = FOLOWO, aes(x = Date, y = KBDI))
