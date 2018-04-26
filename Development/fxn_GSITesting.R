library("geosphere")
library("zoo")
library("dplyr")
library("lubridate")

testWx <- readInWeather(StationID = "KS25",
                        County = "McKenzie",
                        Start = 2014,
                        End = 2014)

testWx_cleaned <- fxn_weatherCleaner(testWx)



testWx_cleaned$DayLength <- geosphere::daylength(lat = 47.663722,
                     doy = yday(x = testWx_cleaned$DateTime))

ggplot(data = testWx_cleaned, aes(x = DayOfYear, y = DayLength))+
  geom_point()+
  facet_wrap(~Year)

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
  
  return(workingDF_daily)
  
}

testGSIOutput <- calcGSI(DateTime = testWx_cleaned$DateTime,
                         Temp = testWx_cleaned$Temp,
                         RH = testWx_cleaned$RH,
                         Latitude = as.numeric(stationMetadata$STATION$LATITUDE))


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
  
  
    return(growingSeason)
}

seasonDF <- findGreenupDates(Year = testGSIOutput$Year,
                             Yday = testGSIOutput$Yday,
                             testGSIOutput$rollGSI)


ggplot(data = testGSIOutput, aes(x = Yday, y = rollGSI))+
  geom_line(color = "gray50", alpha = .5, aes(x = Yday, y = GSI))+
  geom_line(color = "black")+
  geom_vline(data = seasonDF, color = "green", aes(xintercept = GreenUpDate))+
  geom_vline(data = seasonDF, color = "red", aes(xintercept = SenesenceDate))+
  facet_wrap(~Year)



liveFuelMoistures <- function(DateTime, GreenupDates, rollGSI){
  workingDF <- data.frame(DateTime,Year = year(DateTime),rollGSI)
  workingDF$diffGSI <- c(0,diff(workingDF$rollGSI))
  
  workingDF$WoodyLiveFM <- NA
  workingDF$HerbaceousLiveFM <- NA
  
  
  cumsumHours <- function(Minimum, diffGSI, Peak){
    returnValues <- NA
    
    if((300 * diffGSI + Minimum) < Peak & !is.na(diffGSI))
      returnValues <- 300 * diffGSI + Minimum 
    if((300 * diffGSI + Minimum) >= Peak & !is.na(diffGSI))
      returnValues <- Peak
    if(!is.na(diffGSI))
      returnValues <- NA
    if((300 * diffGSI + Minimum) < Minimum & !is.na(diffGSI))
      returnValues <- Minimum
    
    return(returnValues)
  }
  
   minWoody <- 50
   maxWoody <- 200
   
   minHerb <- 30
   maxHerb <- 250
  
  for(i in unique(GreenupDates$Year)){
    print(i)
    
    minDateDF <- filter(workingDF, Year == i) %>% summarise(minDate = min(DateTime, na.rm = T))
    maxDateDF <- filter(workingDF, Year == i) %>% summarise(maxDate = max(DateTime, na.rm = T))
    
    minDate <- minDateDF[1,1]
    maxDate <- maxDateDF[1,1]
    
    GUDate <- GreenupDates$GreenUpDate[GreenupDates$Year == i]
    SDate <- GreenupDates$SenesenceDate[GreenupDates$Year == i]
   
    workingDF[which(workingDF$DateTime >= minDate & workingDF$DateTime <= GUDate & workingDF$DateTime <= SDate & workingDF$Year == i ),]$WoodyLiveFM <- minWoody
    workingDF[which(workingDF$DateTime >= SDate  & workingDF$Year == i),]$WoodyLiveFM <- minWoody
    
    #workingDF[which(workingDF$DateTime >= GUDate & workingDF$DateTime <= SDate),]$WoodyLiveFM <- mapply(cumsumHours,
    #                                                                                                    minWoody,
    #                                                                                                    workingDF[which(workingDF$DateTime >= GUDate & workingDF$DateTime <= SDate),]$diffGSI,
    #                                                                                                    maxWoody)
    workingDF[which(workingDF$DateTime >= GUDate & workingDF$DateTime <= SDate& workingDF$Year == i),]$WoodyLiveFM <- 300 * cumsum(workingDF[which(workingDF$DateTime >= GUDate & workingDF$DateTime <= SDate  & workingDF$Year == i),]$diffGSI ) + minWoody
    
    
    workingDF[which(workingDF$DateTime < GUDate  & workingDF$Year == i),]$HerbaceousLiveFM <- 30
    workingDF[which(workingDF$DateTime > SDate  & workingDF$Year == i),]$HerbaceousLiveFM <- 30
    
    GUDate <- NA
    SDate <- NA
  }
  
  
  return(workingDF)
}

testOutput <- liveFuelMoistures(DateTime = test$DateTime,
                                GreenupDates = seasonDF,
                                rollGSI = test$rollGSI)

testOutput

ggplot(data = testOutput, aes(x = yday(DateTime), y = WoodyLiveFM))+
  geom_line()+
  geom_vline(data = seasonDF, color = "green", aes(xintercept = yday(seasonDF$GreenUpDate)))+
  geom_vline(data = seasonDF, color = "red", aes(xintercept = yday(seasonDF$SenesenceDate)))+
  facet_wrap(~Year)



