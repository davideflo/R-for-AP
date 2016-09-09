############# useful functions  for corr_meteo_pun
library(lubridate)
library(plyr)

##################################################
source("R_code/functions_for_PUN_server.R")
##################################################
find_weekly_seasonality <- function(st, flag = TRUE)
{
  days.of.week <- c("lun", "mar", "mer", "gio", "ven", "sab", "dom")
  season <- rep(0, 7)
  for(d in days.of.week)
  {
    converted <- maply(1:nrow(st), function(n) convert_day(lubridate::wday(st[n,"Date"],label = TRUE)))
    dayg <- which(converted == d)
    season[which(days.of.week == d)] <- mean(st[dayg,2])
  }
  if(flag)  return((season - mean(season))/sd(season))
  else return(season - mean(season))
}