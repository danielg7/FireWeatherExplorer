# Compare two datasets
SD <- "201101012300"
ED <- "201701012300"
goodWx <- readInWeather(StationID = "KOMN",
                        County = "Volusia",
                        Start = 2015,
                        End = 2017)

goodWxOut <- fxn_weatherCleaner(goodWx)


badWx <- readInWeather(StationID = "TS582",
                        Start = SD,
                        End = ED)



fxn_weatherCleaner <- function(weatherDB){
  # Quick tests of completeness
  print("Testing for completeness...", quote = FALSE)

   
  if(weatherDB$SUMMARY$NUMBER_OF_OBJECTS == 0){
    #showNotification("No data in this station for this period of record!")
  }  
  
  # Populate dataframe
  print("Check if fuel moisture is included as a variable...", quote = FALSE)
  
  if("fuel_moisture_set_1" %in% names(weatherDB$STATION$OBSERVATIONS)){
    print("Future moisture is present.", quote = FALSE)
  newWxDF <- data.frame("Date_Time" = weatherDB$STATION$OBSERVATIONS$date_time[[1]],
                       "Fuel_Moisture" = weatherDB$STATION$OBSERVATIONS$fuel_moisture_set_1,
                       "Wind_Direction" = weatherDB$STATION$OBSERVATIONS$wind_cardinal_direction_set_1d,
                       "Wind_Speed" = weatherDB$STATION$OBSERVATIONS$wind_speed_set_1,
                       "Temp" = weatherDB$STATION$OBSERVATIONS$air_temp_set_1,
                       "RH" = weatherDB$STATION$OBSERVATIONS$relative_humidity_set_1,
                       "SolarRad" = weatherDB$STATION$OBSERVATIONS$solar_radiation_set_1[[1]],
                       "PrecipAccumulation" = weatherDB$STATION$OBSERVATIONS$precip_accum_set_1[[1]]
                       )}

  
  if(!"fuel_moisture_set_1" %in% names(weatherDB$STATION$OBSERVATIONS)){
    # showNotification("No fuel moisture data available!")
    print("Future moisture is NOT present.", quote = FALSE)
    
    newWxDF <- data.frame("Date_Time" = weatherDB$STATION$OBSERVATIONS$date_time[[1]],
                           "Fuel_Moisture" = NA,
                           "Wind_Direction" = weatherDB$STATION$OBSERVATIONS$wind_cardinal_direction_set_1d,
                           "Wind_Speed" = weatherDB$STATION$OBSERVATIONS$wind_speed_set_1,
                           "Temp" = weatherDB$STATION$OBSERVATIONS$air_temp_set_1,
                           "RH" = weatherDB$STATION$OBSERVATIONS$relative_humidity_set_1,
                           "SolarRad" = weatherDB$STATION$OBSERVATIONS$solar_radiation_set_1[[1]],
                           "PrecipAccumulation" = weatherDB$STATION$OBSERVATIONS$precip_accum_set_1[[1]]
                          )
    }
  
  # Clean Data
  
  print("Cleaning data...", quote = FALSE)
  
  names(newWxDF) <- c("DateTime","FuelMoisture","Wind_Direction","Wind_Speed","Temp","RH","SolarRad","PrecipAccumulation")
  newWxDF$RH <- as.numeric(as.character(newWxDF$RH))
  
  print("Adjusting Time...", quote = FALSE)
  
  newWxDF$DateTime <- ymd_hms(newWxDF$DateTime, tz = "UTC")
 
  attributes(newWxDF$DateTime)$tzone <-  weatherDB$STATION$TIMEZONE  
  
  newWxDF$Hour <- lubridate::hour(newWxDF$DateTime)
  newWxDF$Month <- lubridate::month(newWxDF$DateTime)
  newWxDF$Year <- lubridate::year(newWxDF$DateTime)
  
  newWxDF$DayOfYear <- as.Date(paste("2000-",format(newWxDF$DateTime, "%j")), "%Y-%j")
  
  return(newWxDF)
  
}


goodWxOut <-fxn_weatherCleaner(goodWx)
badWxOut <- fxn_weatherCleaner(badWx)
