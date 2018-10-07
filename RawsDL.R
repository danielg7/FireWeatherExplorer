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
  #
  # Error catching. Check to make sure start and end are useful.
  #
  
  if(!is.numeric(Start)){
    stop("Start year must be a numeric value. Example: as.numeric(1997)")
  }
  
  if(!is.numeric(End)){
    stop("End year must be a numeric value. Example: as.numeric(1997)")
  }
  
  if(Start > End)
    stop("Start year must be less than end year.")
  
  if(!StationID %in% AllRAWS$STATION$STID)
    stop("StationID not in list!")
  
  #
  # Formatting start and end date strings per what the Mesowest API requires.
  #
  
  StartDate_formatted <<- paste(Start,"01","01","1200",sep = "")
  EndDate_formatted <<- paste(End,"12","12","2300",sep = "")
  
  print(paste("Retrieving data for: ", StationID," (",StartDate_formatted,"-",EndDate_formatted, ")", sep = ""), quote = FALSE)
  
  #
  # Pass variables to mesowest app
  #
  
  downloadedWeather <- mesowest::mw(service = 'timeseries',
                                    stid = StationID,
                                    start = StartDate_formatted,
                                    end = EndDate_formatted,
                                    units="ENGLISH")
  return(downloadedWeather)
}

wxStationMetadata <- function(StationID, Network){
  #
  # Read in station metadata
  #
  
  metadata <- mw(service = 'metadata',
                 stid = StationID,
                 complete = 1,
                 network = c(1,2))
  return(metadata)
}

fuelMoistureCalc <- function(RH, Temp){
  
  workingDF <- data.frame("RH" = RH, "Temp" = ((Temp-32) * 5/9))
  
  returnFM <- ffm(method = 'anderson',
              rh = workingDF$RH,
              temp = workingDF$Temp)
  
  return(returnFM)
  
}

fxn_weatherCleaner <- function(weatherDB){
  FMCMissing <<- FALSE
  
  #
  # Quick tests of completeness
  #
  
  print("Testing for completeness...", quote = FALSE)
  
  
  if(length(weatherDB$STATION$OBSERVATIONS$date_time[[1]]) == 0){
    stop(paste("No data in this station for this period of record!",
               StartDate_formatted,"-",
               EndDate_formatted,sep=""))
  }
  
  
  if(weatherDB$SUMMARY$NUMBER_OF_OBJECTS == 0){
    stop("No data in this station for this period of record!")
  }
  
  if(!"precip_accum_set_1" %in% names(weatherDB$STATION$OBSERVATIONS)){
    print("No rainfall in station data. Checking for other options...", quote = FALSE)
    if(!"precip_accum_one_hour_set_1" %in% names(weatherDB$STATION$OBSERVATIONS)){
      print("No other hourly rainfall data found. Printing NAs.", quote = FALSE)
      rainfall <- rep(NA, length(weatherDB$STATION$OBSERVATIONS$date_time[[1]]))
    }
    if("precip_accum_one_hour_set_1" %in% names(weatherDB$STATION$OBSERVATIONS)){
      print("Alternative found. Using 'precip_accum_one_hour_set_1'", quote = FALSE)
      rainfall <- weatherDB$STATION$OBSERVATIONS$precip_accum_one_hour_set_1
    }
  }
  
  if("precip_accum_set_1" %in% names(weatherDB$STATION$OBSERVATIONS)){
    rainfall <- weatherDB$STATION$OBSERVATIONS$precip_accum_set_1[[1]]
  }
  
  print("Passed.", quote = FALSE)
  
  # Populate dataframe
  
  if("fuel_moisture_set_1" %in% names(weatherDB$STATION$OBSERVATIONS)){
    newWxDF <- data.frame("Date_Time" = weatherDB$STATION$OBSERVATIONS$date_time[[1]],
                          "FuelMoisture_10hr" = weatherDB$STATION$OBSERVATIONS$fuel_moisture_set_1,
                          "Wind_Direction" = weatherDB$STATION$OBSERVATIONS$wind_cardinal_direction_set_1d,
                          "Wind_Speed" = weatherDB$STATION$OBSERVATIONS$wind_speed_set_1,
                          "Temp" = weatherDB$STATION$OBSERVATIONS$air_temp_set_1,
                          "RH" = weatherDB$STATION$OBSERVATIONS$relative_humidity_set_1,
                          "SolarRad" = weatherDB$STATION$OBSERVATIONS$solar_radiation_set_1[[1]],
                          "PrecipAccumulation" = rainfall)
    names(newWxDF) <- c("DateTime","FuelMoisture_10hr","Wind_Direction","Wind_Speed","Temp","RH","SolarRad","PrecipAccumulation")
  }
  
  
  if(!"fuel_moisture_set_1" %in% names(weatherDB$STATION$OBSERVATIONS) & "solar_radiation_set_1" %in% names(weatherDB$STATION$OBSERVATIONS)){
    print("No fuel moisture in this dataset!", quote = FALSE)
    FMCMissing <<- TRUE
    
    newWxDF <- data.frame("Date_Time" = weatherDB$STATION$OBSERVATIONS$date_time[[1]],
                          "FuelMoisture_10hr" = NA,
                          "Wind_Direction" = weatherDB$STATION$OBSERVATIONS$wind_cardinal_direction_set_1d,
                          "Wind_Speed" = weatherDB$STATION$OBSERVATIONS$wind_speed_set_1,
                          "Temp" = weatherDB$STATION$OBSERVATIONS$air_temp_set_1,
                          "RH" = weatherDB$STATION$OBSERVATIONS$relative_humidity_set_1,
                          "SolarRad" = weatherDB$STATION$OBSERVATIONS$solar_radiation_set_1[[1]],
                          "PrecipAccumulation" = rainfall)
    names(newWxDF) <- c("DateTime","FuelMoisture_10hr","Wind_Direction","Wind_Speed","Temp","RH","SolarRad","PrecipAccumulation")
  }
  
  if(!"fuel_moisture_set_1" %in% names(weatherDB$STATION$OBSERVATIONS) & !"solar_radiation_set_1" %in% names(weatherDB$STATION$OBSERVATIONS)){
    
    print("No fuel moisture in this dataset!")
    FMCMissing <<- TRUE
    
    newWxDF <- data.frame("Date_Time" = weatherDB$STATION$OBSERVATIONS$date_time[[1]],
                          "FuelMoisture_10hr" = NA,
                          "Wind_Direction" = weatherDB$STATION$OBSERVATIONS$wind_cardinal_direction_set_1d,
                          "Wind_Speed" = weatherDB$STATION$OBSERVATIONS$wind_speed_set_1,
                          "Temp" = weatherDB$STATION$OBSERVATIONS$air_temp_set_1,
                          "RH" = weatherDB$STATION$OBSERVATIONS$relative_humidity_set_1,
                          "SolarRad" = NA,
                          "HourlyRainfall" = rainfall)
    names(newWxDF) <- c("DateTime","FuelMoisture_10hr","Wind_Direction","Wind_Speed","Temp","RH","SolarRad","HourlyRainfall")
  }
  
  
  
  # Clean Data
  
  print("Cleaning data...", quote = FALSE)
  
  newWxDF$RH <- as.numeric(as.character(newWxDF$RH))
  
  #
  # Calculating 1hr fuel moistures
  #
  
  FMC_calc <- fuelMoistureCalc(RH = newWxDF$RH,
                               Temp = newWxDF$Temp)
  
  newWxDF$FuelMoisture_1hr <- FMC_calc$fm1hr
  newWxDF$FuelMoisture_litter <- FMC_calc$fmLitter
  newWxDF$FuelMoisture_100hr <- FMC_calc$fm100hr
  
  if(FMCMissing == TRUE){
    print("Adding calculate 10-hr fuel moisture...", quote = FALSE)
    newWxDF$FuelMoisture_10hr <- FMC_calc$fm10hr
  }
  
  print("Adjusting Time...", quote = FALSE)
  
  newWxDF$DateTime <- ymd_hms(newWxDF$DateTime, tz = "UTC")
  
  attributes(newWxDF$DateTime)$tzone <-  weatherDB$STATION$TIMEZONE  
  
  newWxDF$Hour <- lubridate::hour(newWxDF$DateTime)
  newWxDF$Month <- lubridate::month(newWxDF$DateTime)
  newWxDF$Year <- lubridate::year(newWxDF$DateTime)
  newWxDF$Day <- lubridate::day(newWxDF$DateTime)
  
  newWxDF$DayOfYear <- as.Date(paste("2000-",format(newWxDF$DateTime, "%j")), "%Y-%j")
  
  #
  # Unborking rainfall so that it's hourly and not
  # 
  
  print("Cleaning Rainfall...", quote = FALSE)
  
  if(!"HourlyRainfall" %in% names(newWxDF)){
    newWxDF <- newWxDF %>%
      group_by(Year) %>%
      arrange(DateTime) %>%
      mutate(HourlyRainfall = PrecipAccumulation - lag(PrecipAccumulation, default=first(PrecipAccumulation))) %>%
      ungroup()
    
    # newWxDF[newWxDF$HourlyRainfall < 0,]$HourlyRainfall <- NA
    newWxDF$HourlyRainfall <- ifelse(!is.na(newWxDF$HourlyRainfall) & newWxDF$HourlyRainfall < 0, NA, newWxDF$HourlyRainfall)
    
    newWxDF <- newWxDF %>%
      group_by(Year, Month, Day) %>%
      mutate(DailyRainfall = sum(HourlyRainfall, na.rm = TRUE)) %>%
      ungroup()
  }
  
  print("Done.", quote = FALSE)
  return(newWxDF)
  
}

# Calculate GSI

calcGSI <- function(DateTime, Temp, RH, Latitude)
{
  originalDF <- data.frame(DateTime,Temp,RH)
  
  workingDF <- data.frame(DateTime,Temp,RH)
  workingDF$Year <- lubridate::year(DateTime)
  workingDF$Yday <- lubridate::yday(x = workingDF$DateTime)
  workingDF$DayOfYear <- as.Date(paste("2000-",format(workingDF$DateTime, "%j")), "%Y-%j")
  workingDF$DayLength <- geosphere::daylength(lat = Latitude,
                                              doy = workingDF$Yday)
  
  workingDF <- filter(workingDF, hour(DateTime) == 13)
  
  
  get.es <- function(temp){
    TempC <- (temp - 32) / 1.8
    
    es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + TempC)))
    return(es)
  }
  
  get.vpd <- function(rh, temp){
    ## calculate saturation vapor pressure
    es <- get.es(temp)
    ## calculate vapor pressure deficit
    vpd <- ((100 - rh) / 100) * es * 100
    return(vpd)
  }
  
  workingDF$VPD <- get.vpd(rh = workingDF$RH,
                           temp = workingDF$Temp)
  
  iGSI_hourly <- function(MinimumTemp, VPD, Photoperiod){
    
    
    # Calculate ITmin
    
    if(MinimumTemp >= 41 & !is.na(MinimumTemp))
      iTmin <- 1
    if(MinimumTemp < 41 & MinimumTemp > 28 & !is.na(MinimumTemp))
      iTmin <- (MinimumTemp - 28)/(41 - 28)
    if(MinimumTemp <= 28 & !is.na(MinimumTemp))
      iTmin <- 0
    if(is.na(MinimumTemp))
      iTmin <- NA
    
    # Calculate iVPD
    
    if(VPD >= 4100 & !is.na(VPD))
      iVPD <- 0
    if(VPD > 900 & VPD < 4100 & !is.na(VPD))
      iVPD <- 1 - (VPD - 900)/(4100 - 900)
    if(VPD <= 900 & !is.na(VPD))
      iVPD <- 1
    if(is.na(VPD))
      iVPD <- NA
    
    # Calculate iPhoto
    
    if(Photoperiod <= 10 & !is.na(Photoperiod))
      iPhoto <- 0
    if(Photoperiod > 10 & Photoperiod < 11 & !is.na(Photoperiod))
      iPhoto <- (Photoperiod - 10)/(11 - 10)
    if(Photoperiod >= 11 & !is.na(Photoperiod))
      iPhoto <- 1
    if(is.na(Photoperiod))
      iPhoto <- NA
    
    iGSI <- iTmin  * iVPD  * iPhoto
    
    return(iGSI)
  }
  
  workingDF_daily <- workingDF %>%
    group_by(Year,Yday) %>%
    summarise(Tmin = min(Temp, na.rm = TRUE), VPD = max(VPD, na.rm = TRUE), DayLength = max(DayLength), na.rm = TRUE) %>%
    ungroup()
  
  workingDF_daily$GSI <- mapply(iGSI_hourly, workingDF_daily$Tmin, workingDF_daily$VPD, workingDF_daily$DayLength)
  
  workingDF_daily <- workingDF_daily %>%
    group_by(Year,Yday) %>%
    mutate(maxGSI = max(GSI, na.rm = TRUE))# %>% ungroup()
  
  workingDF_daily$rollGSI <- rollapply(data = workingDF_daily$maxGSI,  # original series
                                       width = 21,  # width of the rolling window
                                       FUN = mean, na.rm = T,  # Any arbitrary function
                                       fill = NA)
  
  workingDF_daily$Yday <- as.Date(workingDF_daily$Yday, format = "%j", origin=paste0("1.1.",workingDF_daily$Year))
  return(workingDF_daily)
  
}

findGreenupDates <- function(Year, Yday,rollGSI){
  workingDF <- data.frame(Year, Yday,rollGSI)
  
  greenup <- workingDF %>%
    group_by(Year) %>%
    filter(rollGSI >= 0.5) %>%
    arrange(Yday) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(GreenUpDate = min(Yday))
  
  senesence <- workingDF %>%
    group_by(Year) %>%
    filter(rollGSI >= 0.5) %>%
    arrange(Yday) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(SenesenceDate = max(Yday))
  
  growingSeason <- merge(greenup,senesence,by = "Year")
  
  growingSeason$GreenUpDate <- as.Date(growingSeason$GreenUpDate, format = "%j", origin=paste0("1.1.",growingSeason$Year))
  growingSeason$SenesenceDate <- as.Date(growingSeason$SenesenceDate, format = "%j", origin=paste0("1.1.",growingSeason$Year))
  
  return(growingSeason)
}
