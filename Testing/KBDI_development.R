testInputs <- goodWxOut %>%
  group_by(Daily = format(DateTime, "%Y-%m-%d")) %>%
  summarise(MaxTemp = max(Temp, na.rm = TRUE),
            DailyRain = sum(PrecipAccumulation,na.rm = TRUE))

MAP <- goodWxOut %>%
  group_by(Yearly = format(DateTime, "%Y")) %>%
  summarise(YearlyRain = sum(PrecipAccumulation, na.rm = TRUE))
MAP
#SD <- "201501012300"
#ED <- "201701012300"

testStats <- mesowest::mw(service = 'statistics',
             stid = 'RSOC2',
             start = SD,
             end = ED,
             type = 'All',
             units="ENGLISH")




calcKBDI <- function(DailyRain,
                     DailyMaxTemperature,
                     MeanAnnualPrecipitation){
  
  kbdi[2:length(DailyRain)] <- (((800 - YesterdayKBDI - DailyRain) * (0.968 exp(0.0486 * DailyMaxTemperature)-8.3))/(1 + 10.88 * exp(-0.001736 * MeanAnnualPrecipitation))) * 10^-3
  
  return(kbdi)
}

calcKBDI(DailyRain = testInputs$DailyRain,
         DailyMaxTemperature = testInputs$MaxTemp,
         MeanAnnualPrecipitation = 13.85)