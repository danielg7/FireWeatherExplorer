# Install mesowest package
# library("devtools")
# install_github(repo = "fickse/mesowest")


# Packages ----------------------------------------------------------------

library("mesowest")
library("lubridate")


# Read in API Key for Mesowest --------------------------------------------

fileName <- 'api_key.txt'
api_key <- readChar(fileName, file.info(fileName)$size)

mesowest::requestToken(apikey = api_key)

# Read In Station Data ----------------------------------------------------

readInWeather <- function(StationID, Start, End)
{
downloadedWeather <- mesowest::mw(service = 'timeseries',
            stid=StationID,
            start = Start,
            end = End,
            units="ENGLISH")

return(downloadedWeather)
}

wxStationMetadata <- function(StationID){
  metadata <- mw(service = 'metadata', stid = StationID, complete = 1, network = 2)
  return(metadata)
}

fxn_weatherCleaner <- function(weatherDB){
  # Quick tests of completeness
  print("Testing for completeness...", quote = FALSE)
  
  
  if(length(weatherDB$STATION$OBSERVATIONS$date_time) == 0){
    showNotification(paste("No data in this station for this period of record!",StartDate_formatted,"-",EndDate_formatted,sep=""))
  }
  
  if(weatherDB$SUMMARY$NUMBER_OF_OBJECTS == 0){
    showNotification("No data in this station for this period of record!")
  }  
  
  # Populate dataframe
  
  if("fuel_moisture_set_1" %in% names(weatherDB$STATION$OBSERVATIONS)){
    newWxDF <- data.frame("Date_Time" = weatherDB$STATION$OBSERVATIONS$date_time[[1]],
                          "Fuel_Moisture" = weatherDB$STATION$OBSERVATIONS$fuel_moisture_set_1,
                          "Wind_Direction" = weatherDB$STATION$OBSERVATIONS$wind_cardinal_direction_set_1d,
                          "Wind_Speed" = weatherDB$STATION$OBSERVATIONS$wind_speed_set_1,
                          "Temp" = weatherDB$STATION$OBSERVATIONS$air_temp_set_1,
                          "RH" = weatherDB$STATION$OBSERVATIONS$relative_humidity_set_1)}
  
  
  if(!"fuel_moisture_set_1" %in% names(weatherDB$STATION$OBSERVATIONS)){
    # showNotification("No fuel moisture data available!")
    
    newWxDF <- data.frame("Date_Time" = weatherDB$STATION$OBSERVATIONS$date_time[[1]],
                          "Fuel_Moisture" = NA,
                          "Wind_Direction" = weatherDB$STATION$OBSERVATIONS$wind_cardinal_direction_set_1d,
                          "Wind_Speed" = weatherDB$STATION$OBSERVATIONS$wind_speed_set_1,
                          "Temp" = weatherDB$STATION$OBSERVATIONS$air_temp_set_1,
                          "RH" = weatherDB$STATION$OBSERVATIONS$relative_humidity_set_1)}
  
  # Clean Data
  
  print("Cleaning data...", quote = FALSE)
  
  names(newWxDF) <- c("DateTime","FuelMoisture","Wind_Direction","Wind_Speed","Temp","RH")
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
