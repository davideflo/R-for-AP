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
###################################################
find_weekly_seasonality_DB <- function(pp, flag = TRUE)
{
  days.of.week <- c("lun", "mar", "mer", "gio", "ven", "sab", "dom")
  season <- rep(0, 7)
  for(d in days.of.week)
  {
    diem <- which(tolower(unlist(pp[,"Week Day"])) == d) 
    season[which(days.of.week == d)] <- mean(unlist(pp[diem,13]), na.rm = TRUE)
  }
  if(flag)  return((season - mean(season))/sd(season))
  else return(season - mean(season))
}
###################################################
find_daily_sd_DB <- function(pp)
{
  days.of.week <- c("lun", "mar", "mer", "gio", "ven", "sab", "dom")
  season <- rep(0, 7)
  for(d in days.of.week)
  {
    diem <- which(tolower(unlist(pp[,"Week Day"])) == d) 
    ndays <- length(diem)/24
    loc <- rep(0, ndays)
    for(i in 1:ndays)
    {
      loc[i] <- mean(unlist(pp[diem[24*(i-1)+1]:diem[24*i],13]), na.rm=TRUE)
      season[which(days.of.week == d)] <- sd(loc)
    }
  }
  return(season)
}
###################################################
## add computation of skewness and kurtosis
