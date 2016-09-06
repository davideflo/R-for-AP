##### glm model for prediction #####

library(plyr)
library(readxl)
library(lubridate)
library(h2o)
library(dplyr)

source("R_code//functions_for_PUN_server.R")
source("C://Users//utente//Documents//R_code//functions_for_PPIA_server.R")

###############################################################################################################################################
dates2 <- function(vd)
{
  asdd <- c()
  for(n in 1:length(vd))
  {
    d <- unlist(strsplit(vd[n],"/"))
    ##asdd <- maply(1:length(vd), function(n) as.Date(paste0(unlist(strsplit(vd[n],"/"))[3],"-",unlist(strsplit(vd[n],"/"))[2],"-",unlist(strsplit(vd[n],"/"))[1])) )
    asdd <- c(asdd,as.character(as.Date(paste0(d[3],"-",d[2],"-",d[1]))) )
  }
  return(asdd)
}
###############################################################################################################################################
create_glm_dataset <- function(pun, varn, regressorh, targeth, meteo, day_ahead)
{
  Names <- c(varn,"aust","cors","fran","grec",
             "slov","sviz","angleday","holiday",
             "tmin","tmax", "tmed", "rain", "vento",
             "y","target_day", "target_holiday","target_tmin","target_tmax","target_tmed","target_pioggia","target_vento")
  
  if(day_ahead == 0 & regressorh == targeth)
  {
    print("ERROR: doing meaningless regression (x ~ x)")
    return(0)
  }
  
  variables <- which(tolower(colnames(pun)) %in% c(varn,"aust","cors","fran","grec","slov","sviz"))
  
  regh <- which(pun[,2] == regressorh)
  tarh <- which(pun[,2] == targeth)
  start_ix <- 1
  
  reg_val <- pun[regh[1:(length(regh)-day_ahead)], variables]
  if(day_ahead > 0) {y <- pun[tarh[day_ahead:length(tarh)],varn]; start_ix <- day_ahead}
  else {y <- pun[tarh,varn]}
  
  dts <- dates(pun[,1])
  hol <- add_holidays(dts)
  
  asdts <- dates2(dts)
  day <- maply(1:length(asdts), function(n) convert_day_to_angle(convert_day(as.character(lubridate::wday(asdts[n],label=TRUE)))))
  
  tmin <- maply(1:(length(dts)-day_ahead), associate_meteo_ora(dts[n], meteo, "Tmin"))
  tmax <- maply(1:(length(dts)-day_ahead), associate_meteo_ora(dts[n], meteo, "Tmax"))
  tmed <- maply(1:(length(dts)-day_ahead), associate_meteo_ora(dts[n], meteo, "Tmedia"))
  rain <- maply(1:(length(dts)-day_ahead), associate_meteo_ora(dts[n], meteo, "Pioggia"))
  vm <- maply(1:(length(dts)-day_ahead), associate_meteo_ora(dts[n], meteo, "Vento_media"))
  
  ttmin <- maply(1:length(y), associate_meteo_ora(dts[n+day_ahead], meteo, "Tmin"))
  ttmax <- maply(1:length(y), associate_meteo_ora(dts[n+day_ahead], meteo, "Tmax"))
  ttmed <- maply(1:length(y), associate_meteo_ora(dts[n+day_ahead], meteo, "Tmedia"))
  train <- maply(1:length(y), associate_meteo_ora(dts[n+day_ahead], meteo, "Pioggia"))
  tvm <- maply(1:length(y), associate_meteo_ora(dts[n+day_ahead], meteo, "Vento_media"))
  
  df <- data.frame(reg_val, day[1:(length(regh)-day_ahead)], hol[1:(length(regh)-day_ahead)], tmin, tmax, tmed, rain, vm, y,
                   day[start_ix:(length(y)+day_ahead)], hol[start_ix:(length(y)+day_ahead)], ttmin, ttmax, ttmed, train, tvm, stringsAsFactors = FALSE)
  
  colnames(df) <- Names
  return(df)
}
#####################################################################################################################################
from_dates_to_char <- function(dt)
{
  return(paste0(unlist(strsplit(as.character(dt),"-"))[3],"/",unlist(strsplit(as.character(dt),"-"))[2],"/",unlist(strsplit(as.character(dt),"-"))[1]))
}
#####################################################################################################################################a
get_meteo <- function(met)
{
  cols <- which(tolower(colnames(met)) %in% c("tmin","tmax","tmedia","pioggia","ventomedia"))
  dts <- unlist(met[,2])
  
  nums <- grep("/", dts)
  notn <- (nums[length(nums)]+1):nrow(met)
  
  dts2 <- c(dates2(dts[nums]), maply(1:length(notn), function(n) as.character(as.Date(as.numeric(dts[notn[n]]),origin = '1899-12-30'))))
  
  dts3 <- maply(1:length(dts2), function(n) from_dates_to_char(dts2[n]))
  
  met2 <- data.frame(dts3,met[,cols])
  colnames(met2) <- c("Data", "Tmin","Tmax","Tmedia","Vento_media","Pioggia")
  return(met2)
}