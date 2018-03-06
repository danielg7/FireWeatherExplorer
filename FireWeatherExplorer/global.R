library("curl")


source("./RawsDL.R")

# Read in Stations --------------------------------------------------------

AllRAWS <- mesowest::mw('metadata',
                        network=2,
                        status='ACTIVE', complete = TRUE)

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
