
#
# Temperature -------------------------------------------------------------
#

plot_timeseries_temperature <- ggplot(data = wx_df,
                                      aes(x = DayOfYear,
                                          y = Temp))+
  geom_point(alpha = 0.25, size = 0.25)+
  scale_x_date("Month", labels = function(x){format(x, "%b")},
               date_breaks = "1 month")+
  scale_y_continuous("Temperature (F)")+
  labs(title = "Temperature Records",
       subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
  facet_grid(facets = Year ~ .)+
  theme_bw(base_size=15, base_family="Avenir")

#
# Precipitation -------------------------------------------------------------
#

plot_timeseries_precipitation <- ggplot(data = wx_df,
                                        aes(x = DayOfYear,
                                            y = as.numeric(HourlyRainfall)))+
  geom_point(alpha = 0.25, size = 0.25)+
  scale_x_date("Month", labels = function(x){format(x, "%b")},
               date_breaks = "1 month")+
  scale_y_continuous("Hourly Precipitation (hr)")+
  labs(title = "Precipitation",
       subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
  facet_grid(facets = Year ~ .)+
  theme_bw(base_size=15,
           base_family="Avenir")

#
# RH -------------------------------------------------------------
#

plot_timeseries_rh <- ggplot(data = wx_df,
                             aes(x = DayOfYear,
                                 y = RH/100))+
  geom_point(alpha = 0.25, size = 0.25)+
  scale_x_date("Month",
               labels = function(x) format(x, "%b"),
               date_breaks = "1 month")+
  scale_y_continuous("Relative Humidity (%)",
                     labels = scales::percent,limits = c(0,1))+
  labs(title = "Relative Humidity Records",
       subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
  facet_grid(facets = Year ~ .)+
  theme_bw(base_size=15,
           base_family="Avenir")

# 1 hr Fuel Moisture -------------------------------------------------------------

plot_timeseries_fmc1 <- ggplot(data = wx_df,
                               aes(x = DayOfYear,
                                   y = FuelMoisture_1hr/100))+
  geom_point(alpha = 0.25, size = 0.25)+
  scale_x_date("Day of the Year",
               labels = function(x) format(x, "%b"),
               date_breaks = "1 month")+
  scale_y_continuous("1 Hour Fuel Moisture (%)",
                     labels = scales::percent)+
  labs(title = "1 Hr Fuel Moistures (Calculated)",
       subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
  facet_grid(facets = Year ~ .)+
  theme_bw(base_size=15,
           base_family="Avenir")


# 10 hr Fuel Moisture -----------------------------------------------------

if(FMCMissing == TRUE)
  FMStatement <- "Calculated"
if(FMCMissing == FALSE)
  FMStatement <- "Instrumented"

plot_timeseries_fmc10 <- ggplot(data = wx_df,
                                aes(x = DayOfYear,
                                    y = FuelMoisture_10hr/100))+
  geom_point(alpha = 0.25, size = 0.25)+
  scale_x_date("Day of the Year",
               labels = function(x) format(x, "%b"),
               date_breaks = "1 month")+
  scale_y_continuous("10 Hour Fuel Moisture (%)",
                     labels = scales::percent)+
  labs(title = paste("10 Hr Fuel Moistures (",FMStatement,")", sep = ""),
       subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
  facet_grid(facets = Year ~ .)+
  theme_bw(base_size=15,
           base_family="Avenir")


# Wind Speed --------------------------------------------------------------

plot_timeseries_windspeed <- ggplot(data = wx_df,
                                    aes(x = DayOfYear,
                                        y = Wind_Speed,
                                        color = as.factor(Wind_Direction)))+
  geom_point(#alpha = 0.25,
    size = 0.25)+
  scale_color_discrete("Wind Direction")+
  scale_x_date("Day of the Year",
               labels = function(x) format(x, "%b"),
               date_breaks = "1 month")+
  scale_y_continuous("Wind Speed (mph)")+
  labs(title = "Wind Speed Records",
       subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
  facet_grid(facets = Year ~ .)+
  theme_bw(base_size=15,
           base_family="Avenir")+
  guides(size = guide_legend(ncol = 8), colour = guide_legend(override.aes = list(size = 3)))+
  theme(legend.position="bottom",
        legend.direction = "horizontal")

# Wind Speed --------------------------------------------------------------

plot_timeseries_GSI <- ggplot(data = GSIOutput,
                              aes(x = Yday,
                                  y = rollGSI))+
  geom_line(color = "gray50", alpha = .5, aes(x = Yday, y = GSI))+
  geom_line(color = "black")+
  geom_vline(data = seasonDF, color = "green", aes(xintercept = GreenUpDate))+
  geom_vline(data = seasonDF, color = "red", aes(xintercept = SenesenceDate))+
  scale_x_date("Month", labels = function(x){format(x, "%b")},
               date_breaks = "1 month")+
  scale_y_continuous("Growing Season Index")+
  labs(title = "Growing Season Index and Green Up / Senesence Dates",
       subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
  facet_grid(facets = Year ~ .)+
  theme_bw(base_size=15, base_family="Avenir")
plot_timeseries_GSI