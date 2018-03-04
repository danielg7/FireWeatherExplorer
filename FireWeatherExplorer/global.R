library("curl")


source("./RawsDL.R")

# Read in Stations --------------------------------------------------------

#Larimer <- mesowest::mw('metadata',
 #                       county="Larimer",
  #                      state="CO",
   #                     network=2,
    #                    status='ACTIVE')

AllRAWS <- mesowest::mw('metadata',
                        network=2,
                        status='ACTIVE', complete = TRUE)


wx_df <- NULL
wx_dl <- NULL
stationMetadata <- NULL
combinedWx <- NULL
wx_sub_countHours <- NULL
buttonClicker <- 0


universalTheme <- function(){ 
  theme_light(base_size=12, base_family="Avenir")
}