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

readInWeather <- function(StationID, County, Start, End)
{
  downloadedWeather <- mesowest::mw(service = 'timeseries',
                                    stid = StationID,
                                    start = Start,
                                    end = End,
                                    units="ENGLISH")
  
  return(downloadedWeather)
}

wxStationMetadata <- function(StationID){
  metadata <- mw(service = 'metadata',
                 stid = StationID,
                 complete = 1,
                 network = 2)
  return(metadata)
}

fuelMoistureCalc <- function(RH, Temp){
  
  workingDF <- data.frame("RH" = RH,"Temp" = Temp)
  
  EMC <- function(RH,Temp){
    EMC_returned <- NULL
    
    if(is.na(RH))
      EMC_returned <- NA
    if(RH < 10 & !is.na(RH))
    {EMC_returned <- 0.03229 + 0.281073 * RH - 0.00578 * Temp * RH}
    if(RH >= 10 & RH < 50  & !is.na(RH))
      EMC_returned <- 2.22749 + 0.160107 * RH - 0.014784 * Temp
    if(RH >= 50  & !is.na(RH))
      EMC_returned <- 21.0606 + 0.005565 * RH^2 - 0.00035 * RH * Temp - 0.483199 * RH
    
    return(EMC_returned)
  }
  
  EMC_Calc <- mapply(FUN = EMC,
                     RH = workingDF$RH,
                     Temp = workingDF$Temp,
                     SIMPLIFY = TRUE)
  
  EMC_Calc <- unlist(EMC_Calc)
  
  MC1 <- 1.03 * EMC_Calc
  MC10 <- MC1 * 5 - (4 * EMC_Calc)
  
  #
  # Correct for negative 1-hr fuel moisture
  #
  
  MC1[MC1 <= 0] <- 1
  
  FuelMoistures <- data.frame("MC1" = MC1,"MC10" = MC10)
  
  return(FuelMoistures)
  
}


fxn_weatherCleaner <- function(weatherDB){
  FMCMissing <<- FALSE
  
  #
  # Quick tests of completeness
  #
  
  print("Testing for completeness...", quote = FALSE)
  
  
  if(length(weatherDB$STATION$OBSERVATIONS$date_time) == 0){
    showNotification(paste("No data in this station for this period of record!",
                           StartDate_formatted,"-",
                           EndDate_formatted,sep=""))
  }
  
  if(weatherDB$SUMMARY$NUMBER_OF_OBJECTS == 0){
    showNotification("No data in this station for this period of record!")
  }  
  
  # Populate dataframe
  
  if("fuel_moisture_set_1" %in% names(weatherDB$STATION$OBSERVATIONS)){
    newWxDF <- data.frame("Date_Time" = weatherDB$STATION$OBSERVATIONS$date_time[[1]],
                          "FuelMoisture_10hr" = weatherDB$STATION$OBSERVATIONS$fuel_moisture_set_1,
                          "Wind_Direction" = weatherDB$STATION$OBSERVATIONS$wind_cardinal_direction_set_1d,
                          "Wind_Speed" = weatherDB$STATION$OBSERVATIONS$wind_speed_set_1,
                          "Temp" = weatherDB$STATION$OBSERVATIONS$air_temp_set_1,
                          "RH" = weatherDB$STATION$OBSERVATIONS$relative_humidity_set_1)}
  
  
  if(!"fuel_moisture_set_1" %in% names(weatherDB$STATION$OBSERVATIONS)){
    print("No fuel moisture in this dataset!", quote = FALSE)
    FMCMissing <<- TRUE
    
    newWxDF <- data.frame("Date_Time" = weatherDB$STATION$OBSERVATIONS$date_time[[1]],
                          "FuelMoisture_10hr" = NA,
                          "Wind_Direction" = weatherDB$STATION$OBSERVATIONS$wind_cardinal_direction_set_1d,
                          "Wind_Speed" = weatherDB$STATION$OBSERVATIONS$wind_speed_set_1,
                          "Temp" = weatherDB$STATION$OBSERVATIONS$air_temp_set_1,
                          "RH" = weatherDB$STATION$OBSERVATIONS$relative_humidity_set_1)}
  

  
  # Clean Data
  
  print("Cleaning data...", quote = FALSE)
  
  names(newWxDF) <- c("DateTime","FuelMoisture_10hr","Wind_Direction","Wind_Speed","Temp","RH")
  newWxDF$RH <- as.numeric(as.character(newWxDF$RH))
  
  #
  # Calculating 1hr fuel moistures
  #
  
  FMC_calc <- fuelMoistureCalc(RH = newWxDF$RH,
                               Temp = newWxDF$Temp)
  
  newWxDF$FuelMoisture_1hr <- FMC_calc$MC1
  
  if(FMCMissing == TRUE){
    print("Adding calculate 10-hr fuel moisture...", quote = FALSE)
    newWxDF$FuelMoisture_10h <- FMC_calc$MC10
  }
  
  print("Adjusting Time...", quote = FALSE)
  
  newWxDF$DateTime <- ymd_hms(newWxDF$DateTime, tz = "UTC")
  
  attributes(newWxDF$DateTime)$tzone <-  weatherDB$STATION$TIMEZONE  
  
  newWxDF$Hour <- lubridate::hour(newWxDF$DateTime)
  newWxDF$Month <- lubridate::month(newWxDF$DateTime)
  newWxDF$Year <- lubridate::year(newWxDF$DateTime)
  
  newWxDF$DayOfYear <- as.Date(paste("2000-",format(newWxDF$DateTime, "%j")), "%Y-%j")
  
  print(head(newWxDF))
  
  return(newWxDF)
  
}
