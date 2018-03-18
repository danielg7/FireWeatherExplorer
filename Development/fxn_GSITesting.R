library("geosphere")
library("zoo")

testWx_cleaned$DayLength <- geosphere::daylength(lat = as.numeric(stationMetadata$STATION$LATITUDE),
                     doy = yday(x = testWx_cleaned$DateTime))

ggplot(data = testWx_cleaned, aes(x = DayOfYear, y = DayLength))+
  geom_point()+
  facet_wrap(~Year)


# Calculate Vapor Pressure Deficit ----------------------------------------

# T is temp in Celcius
TempC <- (testWx_cleaned$Temp - 32) / 1.8

es <- 0.6108 * exp(17.27 * TempC / (TempC + 237.3))

# Actual vapor pressure:

ea <- testWx_cleaned$RH / 100 * es

# Vapor Pressure Deficit

testWx_cleaned$VPD <- ea - es

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

testWx_cleaned$VPD <- get.vpd(rh = testWx_cleaned$RH,
                              temp = testWx_cleaned$Temp)

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



test <- testWx_cleaned %>%
  group_by(Year,yday(DateTime)) %>%
  mutate(Tmin = min(Temp, na.rm = TRUE)) %>%
  ungroup()

View(test)

test$GSI <- mapply(iGSI_hourly, test$Tmin, test$VPD, test$DayLength)

test <- test %>%
  group_by(Year,yday(DateTime)) %>%
  mutate(maxGSI = max(GSI, na.rm = TRUE)) %>% ungroup()


#test$rollGSI <- zoo::rollmean(x = test$maxGSI, k = 24*21, fill = list(NA,NULL,NA))
test$rollGSI <- rollapply(data = test$maxGSI,  # original series
                          width = 24 * 21,  # width of the rolling window
                          FUN = mean, na.rm = T,  # Any arbitrary function
                          fill = NA)
  
ggplot(data = test, aes(x = yday(DateTime), y = rollGSI))+
  geom_point()+
  facet_wrap(~Year)

findGreenupDates <- function(DateTime, rollGSI){
  workingDF <- data.frame(DateTime, rollGSI)
  workingDF$Year <- year(DateTime)
 
  greenup <- workingDF %>%
    group_by(Year) %>%
    filter(rollGSI >= 0.5) %>%
    arrange(DateTime) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(GreenUpDate = min(DateTime))
  
  senesence <- workingDF %>%
    group_by(Year) %>%
    filter(rollGSI >= 0.5) %>%
    arrange(DateTime) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(SenesenceDate = max(DateTime))
  
  growingSeason <- merge(greenup,senesence,by = "Year")
  
  
    return(growingSeason)
}

seasonDF <- findGreenupDates(test$DateTime,test$rollGSI)
#seasonDF$GreenUpDate <- 


ggplot(data = test, aes(x = yday(DateTime), y = rollGSI))+
  geom_line(color = "black")+
  geom_vline(data = seasonDF, color = "green", aes(xintercept = yday(seasonDF$GreenUpDate)))+
  geom_vline(data = seasonDF, color = "red", aes(xintercept = yday(seasonDF$SenesenceDate)))+
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



