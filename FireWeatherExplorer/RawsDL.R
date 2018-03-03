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





