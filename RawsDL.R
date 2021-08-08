# Install mesowest package
# library("devtools")
# install_github(repo = "fickse/mesowest")


# Packages ----------------------------------------------------------------

library("mesowest")
library("lubridate")

# Create logging function

logger <- function(textToLog, logFile){
  
  if(!file.exists(logFile)){
    warning(paste(logFile," does not exist. Creating...", sep = ""))
    file(logFile)
    }
  
  print(textToLog, quote = FALSE)
  
  cat(textToLog, file = logFile, append = TRUE)
  
}



# Read In Station Data ----------------------------------------------------

readInWeather <- function(token, StationID, State, County, Start, End)
{
  print(Start)
  print(End)
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
  # Read data
  #
  
  tsCall <- function(token,StationID, State, County, Start, End){
    #StartDate_formatted <<- paste(Start,"01","01","1200",sep = "")
    #EndDate_formatted <<- paste(End,"12","12","2300",sep = "")
    
    url <- "https://api.synopticdata.com/v2/stations/timeseries?"
    request <- paste(url,stringr::str_c(paste("stid","=",StationID,sep = ""),
                                        paste("state","=",State,sep = ""),
                                        paste("county","=",County,sep = ""),
                                        paste("start","=",StartDate_formatted,sep = ""),
                                        paste("end","=",EndDate_formatted,sep = ""),
                                        # separate by slash (like web URL)
                                        "obtimezone=local",
                                        "units=english,speed|mph",
                                        "precip=1",
                                        "output=json",
                                        paste("token","=",token,sep = ""),
                                        sep = "&"),sep = "")
    
    print(paste("Request: ",request))
    
    
    output <- httr::GET(url = request)
    
    output <- jsonlite::fromJSON(request,simplifyVector = TRUE)
    
    
    #if(output$status_code != 200)
    # warning(paste("Error in GET request:",output$status_code))
    
    return(output)
  }  
  
  downloadedWeather <- tsCall(token = token,
                              StationID = StationID,
                              State = State,
                              County = County,
                              Start = StartDate_formatted,
                              End = EndDate_formatted
                              )
    
  
  return(downloadedWeather)
}

wxStationMetadata <- function(StationID, token,state){
  #
  # Read in station metadata
  #
  
  metadataCall <- function(token,StationID_given,State_given){
    url <- "https://api.synopticdata.com/v2/stations/metadata?"
    request <- paste(url,stringr::str_c(paste("token","=",token,sep = ""),
                                        # separate by slash (like web URL)
                                        paste("stid",StationID_given,sep="="),
                                        paste("state",State_given,sep=""),
                                        "output=json",
                                        sep = "&"),sep = "")
    
    output <- httr::GET(url = request)
    
    if(output$status_code != 200)
      warning(paste("Error in GET request:",output$status_code))
    
    return(output)
  }
  
  parseMetadataCall <- function(metadatOutput){
    cleanedOutput <- jsonlite::fromJSON(txt = rawToChar(metadatOutput$content))
    return(cleanedOutput)
  }
  
  metadata_output <- metadataCall(token,StationID,state)
  metadata_cleaned <- parseMetadataCall(metadata_output)
  metadata_cleaned$STATION$PERIOD_OF_RECORD[1] <- year(ymd_hms(metadata_cleaned$STATION$PERIOD_OF_RECORD[1]))
  metadata_cleaned$STATION$PERIOD_OF_RECORD[2] <- year(ymd_hms(metadata_cleaned$STATION$PERIOD_OF_RECORD[2]))
  
  #metadata_cleaned$STATION$StartYear <- year(ymd_hms(metadata_cleaned$STATION$PERIOD_OF_RECORD[1]))
  #metadata_cleaned$STATION$EndYear <- year(ymd_hms(metadata_cleaned$STATION$PERIOD_OF_RECORD[2]))
  
  
  print(metadata_cleaned)
  
  if(metadata_cleaned$SUMMARY$RESPONSE_MESSAGE == "No stations found for this request."){
    warning("No stations found for this request. Returning null.")
    return(NULL)}
  else
    return(metadata_cleaned)
}

fuelMoistureCalc <- function(RH, Temp){
  
  workingDF <- data.frame("RH" = RH, "Temp" = ((Temp-32) * 5/9))
  
  returnFM <- ffm(method = 'anderson',
              rh = workingDF$RH,
              temp = workingDF$Temp)
  
  return(returnFM)
  
}

fxn_weatherCleaner <- function(dirtyData){
  
  FMCMissing <<- FALSE
  
  #
  # First we need to check to make sure there's even data to clean.
  #
  
  print("Testing for completeness...", quote = FALSE)
  
  if(length(dirtyData$STATION$OBSERVATIONS$date_time[[1]]) == 0){
    stop("No data in this station for this period of record!")
  }
  
  
  if(dirtyData$SUMMARY$NUMBER_OF_OBJECTS == 0){
    stop("No data in this station for this period of record!")
  }
  
  #
  # Build a working dataframe
  #
  
  print("Getting dimensions...",quote = F)
  obslength <- length(unlist(dirtyData$STATION$OBSERVATIONS$date_time))
  collength <- length(names(dirtyData$STATION$OBSERVATIONS))
  
  print(paste(obslength,"x",collength),quote = F)
  
  print("Done. Creating new df...",quote = F)
  
  cleanData <- data.frame(matrix(nrow = obslength, ncol = collength))
  
  colnames(cleanData) <- names(dirtyData$STATION$OBSERVATIONS)
  
  print("Done. Writing new data...",quote = F)
  
  for(i in 1:collength){
    print(paste("Writing:",colnames(cleanData[i])))
    if(length(unlist(dirtyData$STATION$OBSERVATIONS[1:obslength,i])) > obslength)
      cleanData[1:obslength,i] <- rep(NA,obslength)
    else
      cleanData[1:obslength,i] <- unlist(dirtyData$STATION$OBSERVATIONS[1:obslength,i])
  }
  
  
  print("Done. Determining which rainfall data to use...",quote = F)
  
  #
  # Test for which rainfall data to use
  #
  
  if(!"precip_accum_set_1" %in% names(cleanData)){
    print("No rainfall in station data. Checking for other options...", quote = FALSE)
    if(!"precip_accum_one_hour_set_1" %in% names(cleanData)){
      print("No other hourly rainfall data found. Printing NAs.", quote = FALSE)
      rainfall <- rep(NA, obslength)
    }
    if("precip_accum_one_hour_set_1" %in% names(cleanData)){
      print("Alternative found. Using 'precip_accum_one_hour_set_1'", quote = FALSE)
      rainfall <- cleanData$precip_accum_one_hour_set_1
    }
    if("precip_accum_set_1" %in% names(cleanData)){
      print("Alternative found. Using 'precip_accum_set_1'", quote = FALSE)
      rainfall <- cleanData$precip_accum_set_1
    }
    if("precip_accumulated_set_1d" %in% names(cleanData)){
      print("Alternative found. Using 'precip_accumulated_set_1d'", quote = FALSE)
      rainfall <- cleanData$precip_accumulated_set_1d
    }
  }
  
  
  #
  # Populate a standardized dataframe
  #
  
  
  
  if("fuel_moisture_set_1" %in% names(cleanData)){
    newWxDF <- data.frame("Date_Time" = cleanData$date_time,
                          "FuelMoisture_10hr" = cleanData$fuel_moisture_set_1,
                          "Wind_Direction" = cleanData$wind_cardinal_direction_set_1d,
                          "Wind_Speed" = cleanData$wind_speed_set_1,
                          "Temp" = cleanData$air_temp_set_1,
                          "RH" = cleanData$relative_humidity_set_1,
                          "SolarRad" = cleanData$solar_radiation_set_1[[1]],
                          "PrecipAccumulation" = rainfall)
    names(newWxDF) <- c("DateTime","FuelMoisture_10hr","Wind_Direction","Wind_Speed","Temp","RH","SolarRad","PrecipAccumulation")
  }
  
  
  if(!"fuel_moisture_set_1" %in% names(cleanData) & "solar_radiation_set_1" %in% names(cleanData)){
    print("No fuel moisture in this dataset!", quote = FALSE)
    FMCMissing <<- TRUE
    
    newWxDF <- data.frame("Date_Time" = cleanData$date_time,
                          "FuelMoisture_10hr" = NA,
                          "Wind_Direction" = cleanData$wind_cardinal_direction_set_1d,
                          "Wind_Speed" = cleanData$wind_speed_set_1,
                          "Temp" = cleanData$air_temp_set_1,
                          "RH" = cleanData$relative_humidity_set_1,
                          "SolarRad" = cleanData$solar_radiation_set_1[[1]],
                          "PrecipAccumulation" = rainfall)
    names(newWxDF) <- c("DateTime","FuelMoisture_10hr","Wind_Direction","Wind_Speed","Temp","RH","SolarRad","PrecipAccumulation")
  }
  
  if(!"fuel_moisture_set_1" %in% names(cleanData) & !"solar_radiation_set_1" %in% names(cleanData)){
    
    print("No fuel moisture in this dataset!", quote = FALSE)
    FMCMissing <<- TRUE
    
    newWxDF <- data.frame("Date_Time" = cleanData$date_time,
                          "FuelMoisture_10hr" = NA,
                          "Wind_Direction" = cleanData$wind_cardinal_direction_set_1d,
                          "Wind_Speed" = cleanData$wind_speed_set_1,
                          "Temp" = cleanData$air_temp_set_1,
                          "RH" = cleanData$relative_humidity_set_1,
                          "SolarRad" = NA,
                          "HourlyRainfall" = rainfall)
    names(newWxDF) <- c("DateTime","FuelMoisture_10hr","Wind_Direction","Wind_Speed","Temp","RH","SolarRad","HourlyRainfall")
  }
  
  # Clean Data
  
  print("Cleaning data...", quote = FALSE)
  
  print("Setting RH to numeric...", quote = FALSE)
  
  newWxDF$RH <- as.numeric(as.character(newWxDF$RH))
  
  print("Done. Calculating fuel moistures...", quote = FALSE)
  
  
  #
  # Calculating fuel moistures fuel moistures
  #
  
  FMC_calc <- fuelMoistureCalc(RH = newWxDF$RH,
                               Temp = newWxDF$Temp)
  
  newWxDF$FuelMoisture_1hr <- FMC_calc$fm1hr
  newWxDF$FuelMoisture_litter <- FMC_calc$fmLitter
  newWxDF$FuelMoisture_100hr <- as.numeric(as.character(FMC_calc$fm100hr))
  
  if(FMCMissing == TRUE){
    print("Adding calculated 10-hr fuel moisture...", quote = FALSE)
    newWxDF$FuelMoisture_10hr <- FMC_calc$fm10hr
  }
  
  print("Adjusting Time...", quote = FALSE)
  
  newWxDF$DateTime <- ymd_hms(newWxDF$DateTime, tz = "UTC")
  
  #attributes(newWxDF$DateTime)$tzone <-  weatherDB$STATION$TIMEZONE  
  
  newWxDF$DateTime <- lubridate::with_tz(newWxDF$DateTime, dirtyData$STATION$TIMEZONE)
  
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
    
    print("Done.", quote = FALSE)
    
    
    # newWxDF[newWxDF$HourlyRainfall < 0,]$HourlyRainfall <- NA
    newWxDF$HourlyRainfall <- ifelse(!is.na(newWxDF$HourlyRainfall) & newWxDF$HourlyRainfall < 0, NA, newWxDF$HourlyRainfall)
    
    newWxDF <- newWxDF %>%
      group_by(Year, Month, Day) %>%
      mutate(DailyRainfall = sum(HourlyRainfall, na.rm = TRUE)) %>%
      ungroup()
  }
  
  print("Done cleaning weather.", quote = FALSE)
  return(newWxDF)
  
}

# Calculate GSI

calcGSI <- function(DateTime, Temp, RH, Latitude)
{
  
  print("Starting GSI calculation...", quote = FALSE)
  
  print("Adjusting dataframe...", quote = FALSE)
  
  
  originalDF <- data.frame(DateTime,Temp,RH)
  
  workingDF <- data.frame(DateTime,Temp,RH)
  

  
  
  
  workingDF$Year <- lubridate::year(DateTime)
  workingDF$Yday <- lubridate::yday(x = workingDF$DateTime)
  workingDF$DayOfYear <- as.Date(paste("2000-",format(workingDF$DateTime, "%j")), "%Y-%j")
  
  print("Done.", quote = FALSE)
  
  print("Calculating day length...", quote = FALSE)
  
  workingDF$DayLength <- geosphere::daylength(lat = Latitude,
                                              doy = workingDF$Yday)
  
  print("Done.", quote = FALSE)
  
  print("Subsetting by only 1300 daily weather reading...", quote = FALSE)
  
  workingDF <- filter(workingDF, hour(DateTime) == 13)
  
  print("Done.", quote = FALSE)
  
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
  
  print("Calculating VPD...", quote = FALSE)
  
  workingDF$VPD <- get.vpd(rh = workingDF$RH,
                           temp = workingDF$Temp)
  
  print("Done.", quote = FALSE)
  
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
  
  print("Calculating daily temp minima, VPD max, and daylength...", quote = FALSE)
  
  workingDF_daily <- workingDF %>%
    group_by(Year,Yday) %>%
    summarise(Tmin = min(Temp, na.rm = TRUE), VPD = max(VPD, na.rm = TRUE), DayLength = max(DayLength), na.rm = TRUE) %>%
    ungroup()
  
  print("Done.", quote = FALSE)
  
  print("Calculating GSI...", quote = FALSE)
  
  workingDF_daily$GSI <- mapply(iGSI_hourly, workingDF_daily$Tmin, workingDF_daily$VPD, workingDF_daily$DayLength)
  
  print("Done.", quote = FALSE)
  
  print("Identifying GSI maxima...", quote = FALSE)
  
  workingDF_daily <- workingDF_daily %>%
    group_by(Year,Yday) %>%
    mutate(maxGSI = max(GSI, na.rm = TRUE))# %>% ungroup()
  
  print("Done.", quote = FALSE)
  
  print("Calculating rolling GSI...", quote = FALSE)
  
  workingDF_daily$rollGSI <- rollapply(data = workingDF_daily$maxGSI,  # original series
                                       width = 21,  # width of the rolling window
                                       FUN = mean, na.rm = T,  # Any arbitrary function
                                       fill = NA)
  
  print("Done.", quote = FALSE)
  
  print("Munging data and return...", quote = FALSE)
  
  workingDF_daily$Yday <- as.Date(workingDF_daily$Yday, format = "%j", origin=paste0("1.1.",workingDF_daily$Year))
  
  print("GSI calculations complete.", quote = FALSE)
  
  return(workingDF_daily)
  
  
}

findGreenupDates <- function(Year, Yday,rollGSI){
  print("Starting green up date calculations...", quote = FALSE)
  
  print("Making working dataframe...", quote = FALSE)
  
  workingDF <- data.frame(Year, Yday, rollGSI)
  
  print(head(workingDF), quote = FALSE)
  
  print("Done.", quote = FALSE)
  
  print("Calculating greenup...", quote = FALSE)
  
  
  greenup <- workingDF %>%
    group_by(Year) %>%
    filter(rollGSI >= 0.5) %>%
    arrange(Yday) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(GreenUpDate = min(Yday))
  
  print("Done.", quote = FALSE)
  
  print("Calculating senesence...", quote = FALSE)
  
  
  senesence <- workingDF %>%
    group_by(Year) %>%
    filter(rollGSI >= 0.5) %>%
    arrange(Yday) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(SenesenceDate = max(Yday))
  
  print("Done.", quote = FALSE)
  
  print("Merging growing season datasets...", quote = FALSE)
  
  growingSeason <- merge(greenup,senesence,by = "Year")
  
  print("Done.", quote = FALSE)
  
  print("Forcing date calculations...", quote = FALSE)
  
  
  growingSeason$GreenUpDate <- as.Date(growingSeason$GreenUpDate, format = "%j", origin=paste0("1.1.",growingSeason$Year))
  growingSeason$SenesenceDate <- as.Date(growingSeason$SenesenceDate, format = "%j", origin=paste0("1.1.",growingSeason$Year))
  
  print("Done. Green up dates returning...", quote = FALSE)
  
  
  return(growingSeason)
}
