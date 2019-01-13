
# Monthly Temperature Plot ------------------------------------------------

month_temp_plot <- ggplot(data = wx_df,
                          aes(x = Month,
                              y = Temp,
                              group = Month))+
  geom_boxplot(position = "identity")+
  scale_x_continuous(breaks = seq(1,12,1),
                     labels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
                     limits = c(0.5,12.5))+
  scale_y_continuous("Temperature (F)")+
  labs(title = "Temperature Records by Month",
       subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
  theme_bw(base_size=15,
           base_family="Avenir")


# Monthly RH Plot ---------------------------------------------------------


month_rh_plot <- ggplot(data = wx_df, aes(x = Month,
                                          y = RH/100,
                                          group = Month))+
  geom_boxplot()+
  scale_x_continuous(breaks = seq(1,12,1),
                     labels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
                     limits = c(0.5,12.5))+
  scale_y_continuous("Relative Humidity (%)",
                     labels = scales::percent,limits = c(0,1))+
  labs(title = "Relative Humidity Records by Month",
       subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
  theme_bw(base_size=15,
           base_family="Avenir")


# Monthly Wind Speed Plots ------------------------------------------------

month_wind_plot <- ggplot(data = wx_df, aes(x = Month,
                                            y = Wind_Speed,
                                            group = Month))+
  geom_boxplot()+
  scale_x_continuous(breaks = seq(1,12,1),
                     labels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
                     limits = c(0.5,12.5))+
  scale_y_continuous("Wind Speed (mph)")+
  labs(title = "Wind Speed Records by Month",
       subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
  theme_bw(base_size=15,
           base_family="Avenir")


