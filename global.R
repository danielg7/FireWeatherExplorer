library("curl")
library("zoo")

source("RawsDL.R")


# Read in Stations --------------------------------------------------------

AllRAWS <- mesowest::mw('metadata',
                        network = c(1,2),
                        status='ACTIVE', complete = TRUE)

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
