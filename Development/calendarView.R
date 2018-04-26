library("ggplot2")
library("dplyr")
library("lubridate")

wx_sub_test <- combinedWx %>%
  mutate(Yday = yday(DateTime)) %>%
  mutate(weekday = wday(DateTime)) %>%
  mutate(weekdayf = weekdays(DateTime, abbreviate = TRUE)) %>%
  mutate(week = as.numeric(format(DateTime,"%W"))) %>%
  group_by(Month) %>%
  mutate(monthweek = 1 + week - min(week)) %>%
  ungroup() %>%
  count(Yday, Month, weekdayf, monthweek, Conditions) %>%
  group_by(Yday) %>%
  mutate(Percent = n / sum(n)) %>%
  filter(Conditions %in% c("In Prescription","Window")) %>%
  ungroup()

wx_sub_test$Month <- as.factor(wx_sub_test$Month)
levels(wx_sub_test$Month) <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
wx_sub_test$weekdayf <- as.factor(wx_sub_test$weekdayf)
wx_sub_test$weekdayf <- factor(x = wx_sub_test$weekdayf, levels = rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))

windowCount <- wx_sub_test %>%
  filter(Conditions == "Window")
prescriptionCount <- wx_sub_test %>%
  filter(Conditions == "In Prescription")

ggplot(prescriptionCount, aes(monthweek, weekdayf, fill = Percent*100)) + 
  geom_tile(fill = "gray", data = windowCount, aes(monthweek, weekdayf), size = 0.25, colour = "white")+
  geom_tile(colour="white",size=0.25) + 
  facet_grid(. ~ Month)+
  coord_fixed()+
  scale_x_continuous(breaks = seq(1,5,1),
                     labels = seq(1,5,1))+
  scale_fill_gradient(low="red", high="green")+
  xlab("\n\nWeek of Month") + ylab("")+
  guides("Percent of Hours in Prescription", size = guide_legend(ncol = 8))+
  theme_bw(base_size=15, base_family="Avenir")+
  theme(legend.position="bottom",
        legend.direction = "vertical",
        plot.background = element_blank(),
        panel.border = element_blank())
  
  