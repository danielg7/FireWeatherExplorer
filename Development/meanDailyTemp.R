install.packages("ggridges")
install.packages("viridis")

library("ggridges")
library("viridis")

monthlyMeanTemp <- wx_df %>%
  group_by(DayOfYear, Month) %>%
  summarise(MeanTemp = mean(Temp, na.rm = TRUE))

monthlyMeanTemp$Month <- as.factor(monthlyMeanTemp$Month)
levels(monthlyMeanTemp$Month) <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

ggplot(monthlyMeanTemp, aes(x = MeanTemp, y = as.factor(Month), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1) +
  scale_x_continuous("Mean Daily Temperature (F)", expand = c(0.01, 0))+
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  theme_ridges(font_size = 13, grid = TRUE)+
  labs(title = "Temperature Records by Month",
       subtitle = paste(stationMetadata$STATION$NAME,": ",min(wx_df$Year)," - ",max(wx_df$Year),sep = ""))+
    theme_bw(base_size=15, base_family="Avenir")+
    theme(axis.title.y = element_blank())
