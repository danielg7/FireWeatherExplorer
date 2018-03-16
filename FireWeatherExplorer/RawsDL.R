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
    stop("Start year must be a numeric value. Example: as.numeric(1997)")
  }
  
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
  
  #
  # Correct for negative 10-hr fuel moisture
  #
  
  MC10[MC10 <= 0] <- 1
  
  FuelMoistures <- data.frame("MC1" = MC1,"MC10" = MC10)
  
  return(FuelMoistures)
  
}
