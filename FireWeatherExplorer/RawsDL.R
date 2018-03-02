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

StationName <- "TS582"


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

stationMetadata <- wxStationMetadata("TS582")

wx_dl <- readInWeather(StationID = StationName,
              Start = "201101011200",
              End = "201712312359")


wx_df <- data.frame("dt" = wx_dl$STATION$OBSERVATIONS$date_time,
           "fuel_moisture" = wx_dl$STATION$OBSERVATIONS$fuel_moisture_set_1,
           "wind_direction" = wx_dl$STATION$OBSERVATIONS$wind_cardinal_direction_set_1d,
           "wind_speed" = wx_dl$STATION$OBSERVATIONS$wind_speed_set_1,
           "temp" = wx_dl$STATION$OBSERVATIONS$air_temp_set_1,
           "rh" = wx_dl$STATION$OBSERVATIONS$relative_humidity_set_1)

names(wx_df) <- c("DateTime","FuelMoisture","Wind_Direction","Wind_Speed","Temp","RH")
wx_df$DateTime <- ymd_hms(wx_df$DateTime,tz = "UTC")
attributes(wx_df$DateTime)$tzone <- "America/Denver"  
wx_df$Week <- week(wx_df$DateTime)
wx_df$Month <- month(wx_df$DateTime)
wx_df$Year <- year(wx_df$DateTime)

