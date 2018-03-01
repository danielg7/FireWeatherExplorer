#library("devtools")
#install_github(repo = "fickse/mesowest")

library("mesowest")
library("lubridate")


requestToken(apikey = "KzUeoUZBhy7Sk3m4mBJaHHKRdkpMv1py2zc")

StationName <- "TS582"

wx_dl <- mw(service = 'timeseries', stid=StationName, start = "201101011200", end = "201712312359", units="ENGLISH")



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

