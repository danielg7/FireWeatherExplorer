
# Hourly Temperature Plots ------------------------------------------------

hour_temp_plot <- ggplot(data = wx_df, aes(x = Hour,
                                           y = Temp,
                                           group = Hour))+
  geom_boxplot()+
  scale_x_continuous("Hour",
                     breaks = seq(0,23,1),
                     limits = c(-.45,23.45),
                     labels = c("0000","0100","0200","0300","0400","0500","0600","0700","0800","0900","1000","1100",
                                "1200","1300","1400","1500","1600","1700","1800","1900","2000","2100","2200",
                                "2300"))+
  scale_y_continuous("Temperature (F)")+
  labs(title = "Temperature Records by Hour",
       subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
  theme_bw(base_size=15,
           base_family="Avenir")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# Hourly RH  Plots --------------------------------------------------------

hour_rh_plot <- ggplot(data = wx_df, aes(x = Hour,
                                         y = RH/100,
                                         group = Hour))+
  geom_boxplot()+
  scale_x_continuous("Hour",
                     breaks = seq(0,23,1),
                     limits = c(-.45,23.45),
                     labels = c("0000","0100","0200","0300","0400","0500","0600","0700","0800","0900","1000","1100",
                                "1200","1300","1400","1500","1600","1700","1800","1900","2000","2100","2200",
                                "2300"))+
  scale_y_continuous("Relative Humidity (%)",
                     labels = scales::percent,limits = c(0,1))+
  labs(title = "Relative Humidity Records by Hour",
       subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
  theme_bw(base_size=15,
           base_family="Avenir")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# Hourly Wind Plots -------------------------------------------------------


hour_wind_plot <- ggplot(data = wx_df, aes(x = Hour,
                                           y = Wind_Speed,
                                           group = Hour))+
  geom_boxplot()+
  scale_x_continuous("Hour",
                     breaks = seq(0,23,1),
                     limits = c(-.45,23.45),
                     labels = c("0000","0100","0200","0300","0400","0500","0600","0700","0800","0900","1000","1100",
                                "1200","1300","1400","1500","1600","1700","1800","1900","2000","2100","2200",
                                "2300"))+
  scale_y_continuous("Wind Speed (mph)")+
  labs(title = "Wind Speed Records by Hour",
       subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
  theme_bw(base_size=15,
           base_family="Avenir")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

