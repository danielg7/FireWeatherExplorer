
AllNWS <- mesowest::mw('metadata',
                       network = 1,
                       status = 'Active',
                       complete = TRUE)

AllNWS$STATION

SD <- 2008
ED <- 2017

testWx <- readInWeather(StationID = "KFNL",
                        Start = SD,
                        End = ED)

testWx_cleaned <- fxn_weatherCleaner(testWx)

head(testWx_cleaned)

View(testWx_cleaned)
