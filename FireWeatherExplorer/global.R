library("curl")


source("./RawsDL.R")

# Read in Stations --------------------------------------------------------

Larimer <- mesowest::mw('metadata',
                        county="Larimer",
                        state="CO",
                        network=2,
                        status='ACTIVE')


wx_df <- NULL
wx_dl <- NULL
stationMetadata <- NULL
combinedWx <- NULL