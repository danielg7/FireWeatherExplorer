library("curl")
library("zoo")
library("firebehavioR")

# Read in API Key for Mesowest --------------------------------------------
token <<- "d3180529bc8f41a6a359d61a37592fa7"
# if(!file.exists("~/.mesowesttoken")){
#   print("Token not found.", quote = F)
#   
#   fileName <- 'api_key.txt'
#   api_key <- readChar(fileName, file.info(fileName)$size)
#   
#   mesowest::requestToken(apikey = api_key)
# }

source("RawsDL.R")


# Read in Stations --------------------------------------------------------

#AllRAWS <- mesowest::mw('metadata',
 #                       network = c(1,2),
  #                      status='ACTIVE', complete = TRUE)

load(file="AllRAWS.Rda")

AllLocations <- data.frame("StationName" = AllRAWS$STATION$NAME,
                           "StationID" = AllRAWS$STATION$STID,
                           "Lat" = as.numeric(AllRAWS$STATION$LATITUDE),
                           "Long" = as.numeric(AllRAWS$STATION$LONGITUDE),
                           "State" = AllRAWS$STATION$STATE,
                           "County" = AllRAWS$STATION$COUNTY)

AllLocations$DisplayName <- paste(AllLocations$StationName," (",AllLocations$StationID,")", sep = "")


# Read in States --------------------------------------------------------

State_List <- sort(as.character(unique(AllRAWS$STATION$STATE)))
State_List <- State_List[!State_List %in% "AS"]



stationRxValues <- data.frame(Wind.Low = NA,
                              Wind.High = NA,
                              FM1.Low = NA,
                              FM1.High = NA,
                              FM10.Low = NA,
                              FM10.High = NA,
                              RH.Low = NA,
                              RH.High = NA,
                              Temp.Low = NA,
                              Temp.High = NA,
                              Month.Low = NA,
                              Month.High = NA,
                              Hour.Low = NA,
                              Hour.High = NA,
                              WindDirections = NA)
map <- NULL
wx_df <- NULL
wx_dl <- NULL
GSI_output <- NULL
seasonDF <- NULL
stationMetadata <- NULL
combinedWx <- NULL
wx_sub_countHours <- NULL
StartDate_formatted <- NULL
EndDate_formatted <- NULL
FMCMissing <- NULL
Station_List <- NULL
Stations_In_County <- NULL
windDirList <- 
  c("N",
    "NNE",
    "NE",
    "ENE",
    "E",
    "ESE",
    "SE",
    "SSE",
    "S",
    "SSW",
    "SW",
    "WSW",
    "W",
    "WNW",
    "NW",
    "NNW")
