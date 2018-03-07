library("curl")


source("./RawsDL.R")

# Read in Stations --------------------------------------------------------

AllRAWS <- mesowest::mw('metadata',
                        network=2,
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


wx_df <- NULL
wx_dl <- NULL
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
