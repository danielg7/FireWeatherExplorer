# Prescription Plot - Month by Hour ---------------------------------------

print("Plotting rx month by hour...", quote = FALSE)

#print(head(combinedWx), quote = TRUE)

#print(head(wxSubsetByConditions()))

plot_rx_month_by_hour <- ggplot(data = filter(wxSubsetByConditions(), Conditions %in% "In Prescription"), aes(x = Month, y = Hour))+
  annotate("rect",
           xmin = min(filter(wxSubsetByConditions(), Conditions %in% "Window")$Month),
           xmax = max(filter(wxSubsetByConditions(), Conditions %in% "Window")$Month),
           ymin = min(filter(wxSubsetByConditions(), Conditions %in% "Window")$Hour),
           ymax = max(filter(wxSubsetByConditions(), Conditions %in% "Window")$Hour),
           alpha = .2, color = "green")+
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon", alpha = 0.1)+
  scale_fill_continuous("Probability Density", guide = FALSE)+
  geom_count(fill = "red", alpha = .5, pch=21)+
  scale_size_area(name = "Number of Hours In Prescription\nAcross Period of Record",
                  max_size = 10)+#,
  #breaks = seq(1,maxRange,1))+
  scale_x_continuous("Months",breaks = seq(1,12,1),
                     labels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
                     limits = c(0.5,12.5))+
  scale_y_continuous("Hour of the Day", breaks = seq(0,23,1),
                     limits = c(0,23),
                     labels = c("0000","0100","0200","0300","0400","0500","0600","0700","0800","0900","1000","1100",
                                "1200","1300","1400","1500","1600","1700","1800","1900","2000","2100","2200",
                                "2300"))+
  guides(size = guide_legend(ncol = 8))+
  theme_bw(base_size=15, base_family="Avenir")+
  theme(legend.position="bottom",
        legend.direction = "vertical")+
  labs(title = "Hours That Match Prescription Parameters", subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))

print("Done.", quote = TRUE)


# Prescription Plog - Histogram by Month ----------------------------------

wx_sub_countHours <<- wxSubsetByConditions() %>%
  count(Month, Conditions) %>%
  group_by(Month) %>%
  mutate(Percent = n / sum(n))

plot_rx_histogram_by_month <- ggplot(data = filter(wx_sub_countHours, Conditions == "In Prescription"),
                   aes(x = Month, y = Percent, fill = Conditions))+
  scale_y_continuous("Percent of Hours Matching Conditions (by month)",
                     labels = scales::percent)+
  geom_bar(color = "black", width = 1, stat = "identity", position="dodge")+
  scale_x_continuous(breaks = seq(1,12,1),
                     labels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
                     limits = c(0.5,12.5))+
  theme_bw(base_size=15, base_family="Avenir")+
  labs(title = "Percent of Hours That Match Prescription Parameters Per Month", subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
  theme(legend.position="none")


# Prescription Plot - Percent of Hours by Month / Day ---------------------

windowCount <- wx_sub_test() %>%
  filter(Conditions == "Window")

prescriptionCount <- wx_sub_test() %>%
  filter(Conditions == "In Prescription")

plot_rx_percent_hours_by_month <- ggplot(prescriptionCount, aes(x = Month, y = Day, fill = Percent*100)) + 
  geom_tile(fill = "gray", data = windowCount, aes(Month, Day), size = 0.25, colour = "white")+
  geom_tile(colour="white",size=0.25) + 
  # facet_grid(. ~ Month)+
  coord_fixed()+
  coord_flip()+
  #scale_x_continuous(breaks = seq(1,6,1),
  #                  labels = seq(1,6,1))+
  scale_fill_viridis_c("Percent of Hours\nin Prescription",direction = 1)+
  
  #  scale_fill_distiller("Percent of Hours\nin Prescription", type = "seq",direction = -1,palette = "YlGn")+
  xlab("Month") + ylab("Day of Month")+
  scale_y_continuous(breaks =  seq(1,31,1))+
  scale_x_discrete(limits = rev(levels(prescriptionCount$Month)))+
  guides("Percent of Hours in Prescription",
         size = guide_legend(ncol = 8),
         fill = guide_colourbar(title.position="top",
                                title.hjust = 0.5,
                                barwidth = 10))+
  labs(title = "Percent of Hours That Match Prescription Parameters Per Day of the Year", subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
  theme_bw(base_size=15, base_family="Avenir")+
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        plot.background = element_blank(),
        panel.grid = element_blank())#,
        #panel.border = element_blank()
