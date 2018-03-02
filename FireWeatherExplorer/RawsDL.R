# Install mesowest package
# library("devtools")
# install_github(repo = "fickse/mesowest")


# Packages ----------------------------------------------------------------

library("mesowest")
library("lubridate")


# Read in API Key for Mesowest --------------------------------------------

fileName <- 'api_key.txt'
api_key <- readChar(fileName, file.info(fileName)$size)

requestToken(apikey = api_key)

# Read in Stations --------------------------------------------------------

Larimer <- mw('metadata',county="Larimer", state="CO")

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
  metadata <- mw(service = 'metadata', stid = StationID, complete = 1)
  return(metadata)
}

stationMetadata <- wxStationMetadata(StationID)



