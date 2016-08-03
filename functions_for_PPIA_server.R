### better functions for PUN PREDICTION IN ACTION (PPIA)

library(readxl)
library(lubridate)

source("R_code/functions_for_PUN_server.R")


###########################################
create_dataset_day_ahead <- function(pun, first_day, varn, meteo, step, day_ahead, hb)
{
  ## step starts from 0, in which case the model predicts the hour after the predictors provided 
  ## and goes to 24, which is the same hour the day after
  ## hb (=hours_back) is how many hours I'm going back to build the training set
  ## day_ahead is how many ahead I'm going to predict
  d_f <- data_frame()
  Names <- c(paste0(varn,"-",hb:1), paste0("aust-",hb:1), paste0("cors-",hb:1), paste0("fran-",hb:1), paste0("grec-",hb:1),
             paste0("slov-",hb:1), paste0("sviz-",hb:1), paste0("angleday-",hb:1), paste0("holiday-",hb:1),
             paste0("angleora-",hb:1),
             paste0("tmin-",hb:1), paste0("tmax-",hb:1), paste0("tmed-",hb:1), paste0("rain-",hb:1), paste0("vento-",hb:1),
             "y","target_ora", "target_day", "target_holiday","target_tmin","target_tmax","target_tmed","target_pioggia","target_vento",
             paste0("day-",hb:1))
  
  hbb <- hb - 1
  da <- 24*day_ahead
  for(i in 1:(nrow(pun)-(hb+step+da)))
  {
    #print(da+i+hb+step)
    y <- p <- aus <- cors <- fran <- grec <- slov <- sviz <- ora <- dat <- c()
    for(j in i:(i+hbb))
    {
      p <- c(p, pun[j,varn]); aus <- c(aus, pun[j,"AUST"]); cors <- c(cors, pun[j,"CORS"])
      fran <- c(fran, pun[j,"FRAN"]); grec <- c(grec, pun[j,"GREC"]); slov <- c(slov, pun[j,"SLOV"])
      sviz <- c(sviz, pun[j,"SVIZ"]); ora <- c(ora, pun[j,2]); dat <- c(dat, pun[j,1]) 
    }
    
    ds <- dates(dat)
    #dt <- as.Date(ds[length(ds)])
    
    #target values and dates
    y <- c(y, pun[(da+i+hb+step),varn])
    new_hour <- pun[(da+i+hb+step),2]
    new_date <- dates(pun[(da+i+hb+step),1])
    thol <- add_holidays(new_date)
    dd3 <- unlist(strsplit(new_date,"/"))
    #print(paste0(dd3[3],"-",dd3[2],"-",dd3[1]))
    asdd <- as.Date(paste0(dd3[3],"-",dd3[2],"-",dd3[1]))
    tday <- convert_day_to_angle(convert_day(as.character(lubridate::wday(as.Date(asdd),label=TRUE))))
    thour <- convert_hour_to_angle(new_hour)
    
    ## target meteo
    ttmin <- associate_meteo_ora(new_date, meteo, "Tmin")
    ttmax <- associate_meteo_ora(new_date, meteo, "Tmax")
    ttmed <- associate_meteo_ora(new_date, meteo, "Tmedia")
    train <- associate_meteo_ora(new_date, meteo, "Pioggia")
    tvm <- associate_meteo_ora(new_date, meteo, "Vento_media")
    
    day <- unlist(ifelse(nrow(d_f) > 0, d_f[nrow(d_f),ncol(d_f)], first_day))
    #print(day)
    
    hol <- add_holidays(ds)
    vdays <- associate_days(ora, day)
    vdays2 <- maply(1:length(vdays), function(n) as.character(vdays[n]))
    aday <- maply(1:length(vdays2), function(n) convert_day_to_angle(vdays2[n]))
    ahour <- convert_hour_to_angle(ora)
    
    ## togli vdays e metti variabili meteo
    tmin <- associate_meteo_ora(ds, meteo, "Tmin")
    tmax <- associate_meteo_ora(ds, meteo, "Tmax")
    tmed <- associate_meteo_ora(ds, meteo, "Tmedia")
    rain <- associate_meteo_ora(ds, meteo, "Pioggia")
    vm <- associate_meteo_ora(ds, meteo, "Vento_media")
    
    ### transpose the vectors as they are column vectors in R
    adf <- data.frame(t(p), t(aus), t(cors), t(fran), t(grec), t(slov), t(sviz), t(aday[1:23]), t(hol[1:23]), t(ahour[1:23]),
                      t(tmin[1:hb]), t(tmax[1:hb]), t(tmed[1:hb]), t(rain[1:hb]), t(vm[1:hb]), y, thour, tday, thol, ttmin, ttmax, ttmed, train, tvm,
                      t(vdays[1:hb]), stringsAsFactors = FALSE)
    colnames(adf) <- Names
    
    d_f <- bind_rows(d_f, adf)
  }
  colnames(d_f) <- Names
  return(d_f[,1:354])
}
#######################################################
create_dataset_days_ahead <- function(pun, first_day, varn, meteo, step, day_ahead, hb=24)
{
  d_f <- data_frame()
  
  Names <- c(paste0(varn,"-",hb:1), paste0("aust-",hb:1), paste0("cors-",hb:1), paste0("fran-",hb:1), paste0("grec-",hb:1),
             paste0("slov-",hb:1), paste0("sviz-",hb:1), "angleday", "holiday",
             paste0("angleora-",hb:1),
             "tmin","tmax","tmed","pioggia","vento",
             "y","target_ora", "target_day", "target_holiday","target_tmin","target_tmax","target_tmed","target_pioggia","target_vento",
             "day")
  
  corr <- step + day_ahead*24
  
  well_dates <- dates(pun[,1])
  dat <- c()
  
  day <- first_day
  
  for(i in 1:(nrow(pun)-corr))
  {
    print(paste("i: ",i))
    y <- p <- aus <- cors <- fran <- grec <- slov <- sviz <- ora <- hol <- c()
    tmin <- tmax <- tmed <- rain <- vm <- c()
    ttmin <- ttmax <- ttmed <- train <- tvm <- thol <- tday <- c()
    
    dd <- pun[i,1]
    dd2 <- dates(dd)
    at_date <- pun[which(pun[,1] == dd),]
    
    if(!(dd2 %in% dat))
    {
      print("preso")
      dat <- c(dat, dd2) 
      p <- at_date[varn] 
      aus <- at_date["AUST"] 
      cors <- at_date["CORS"]
      fran <- at_date["FRAN"] 
      grec <- at_date["GREC"] 
      slov <- at_date["SLOV"]
      sviz <- at_date["SVIZ"] 
      ora <- at_date[,2]
      
      #day <- unlist(ifelse(nrow(d_f) > 0, d_f[nrow(d_f),ncol(d_f)], first_day))
      print(paste("day qui:", day))
      #vdays <- associate_days(ora, day)
      #vdays2 <- maply(1:length(vdays), function(n) as.character(vdays[n]))
      #aday <- maply(1:length(vdays2), function(n) convert_day_to_angle(vdays2[n]))
      #aday <- convert_day_to_angle(as.character(vdays[hb]))
      ahour <- convert_hour_to_angle(ora)
      
      if(i > 1) day <- subsequent_day(day)
      
      aday <- convert_day_to_angle(as.character(day))
      hol <- add_holidays(dd2)
      
      tmin <- associate_meteo_ora(dd2, meteo, "Tmin")
      tmax <- associate_meteo_ora(dd2, meteo, "Tmax")
      tmed <- associate_meteo_ora(dd2, meteo, "Tmedia")
      rain <- associate_meteo_ora(dd2, meteo, "Pioggia")
      vm <- associate_meteo_ora(dd2, meteo, "Vento_media")
      
      dd3 <- unlist(strsplit(dd2,"/"))
      asdd <- as.Date(paste0(dd3[3],"/",dd3[2],"/",dd3[1]))
      target_da <- asdd + day_ahead
      tda <- unlist(strsplit(as.character(target_da),"-"))
      target_data <- paste0(tda[3],"/",tda[2],"/",tda[1])
      
      tr <- which(well_dates %in% target_data)
      print(tr)
      target_pun <- pun[tr,]
      #print(target_pun[,1])
      tdts <- dates(target_pun[,1])
      #print(target_data)
      if(all(tdts==target_data) & length(tr) == 24)
      {
        y <- target_pun[which(target_pun[,2] == hb+step+1),varn]
        ttmin <- associate_meteo_ora(target_data, meteo, "Tmin")
        ttmax <- associate_meteo_ora(target_data, meteo, "Tmax")
        ttmed <- associate_meteo_ora(target_data, meteo, "Tmedia")
        train <- associate_meteo_ora(target_data, meteo, "Pioggia")
        tvm <- associate_meteo_ora(target_data, meteo, "Vento_media")
        thol <- add_holidays(target_data)
        print(day)
        tday <- convert_day_to_angle(compute_day_at(day, day_ahead))
      }
      
      else
      {
        print("ERROR: target dates not found")
        break
      }
      df <- data.frame(t(p),t(aus),t(cors),t(fran),t(grec),t(slov),t(sviz),aday,hol,t(ahour),tmin,tmax,tmed,rain,vm,
                       y,convert_hour_to_angle(step+1),tday,thol,ttmin,ttmax,ttmed,train,tvm,as.character(day),stringsAsFactors = FALSE)

      colnames(df) <- Names
      
      ll <- list(d_f,df)
      d_f <- rbindlist(ll,use.names = TRUE)
    }
    
  }
  return(d_f[,1:244])
}


# tp2 <- read_excel("C:/Users/utente/Documents/PUN/test_pun.xlsx")
# > tp2[1,1]
# [1] "2016-07-31 23:59:59 UTC"
# > as.POSIXct(tp2[,1], format="%d/%m/%Y %H:%M:%S", tz="ECT")
# [1] "2016-07-31 23:59:59 UTC" "2016-08-01 00:59:59 UTC" "2016-08-01 01:59:59 UTC" "2016-08-01 02:59:59 UTC" "2016-08-01 03:59:59 UTC" "2016-08-01 04:59:59 UTC"
# [7] "2016-08-01 06:00:00 UTC" "2016-08-01 06:59:59 UTC" "2016-08-01 07:59:59 UTC" "2016-08-01 08:59:59 UTC" "2016-08-01 09:59:59 UTC" "2016-08-01 10:59:59 UTC"
# [13] "2016-08-01 11:59:59 UTC" "2016-08-01 12:59:59 UTC" "2016-08-01 13:59:59 UTC" "2016-08-01 14:59:59 UTC" "2016-08-01 15:59:59 UTC" "2016-08-01 16:59:59 UTC"
# [19] "2016-08-01 17:59:59 UTC" "2016-08-01 18:59:59 UTC" "2016-08-01 19:59:59 UTC" "2016-08-01 20:59:59 UTC" "2016-08-01 21:59:59 UTC" "2016-08-01 22:59:59 UTC"
# [25] "2016-08-01 23:59:59 UTC" "2016-08-02 00:59:59 UTC" "2016-08-02 01:59:59 UTC" "2016-08-02 02:59:59 UTC" "2016-08-02 03:59:59 UTC" "2016-08-02 04:59:59 UTC"
# [31] "2016-08-02 05:59:59 UTC" "2016-08-02 06:59:59 UTC" "2016-08-02 07:59:59 UTC" "2016-08-02 08:59:59 UTC" "2016-08-02 09:59:59 UTC" "2016-08-02 10:59:59 UTC"
# [37] "2016-08-02 11:59:59 UTC" "2016-08-02 12:59:59 UTC" "2016-08-02 13:59:59 UTC" "2016-08-02 14:59:59 UTC" "2016-08-02 15:59:59 UTC" "2016-08-02 16:59:59 UTC"
# [43] "2016-08-02 17:59:59 UTC" "2016-08-02 18:59:59 UTC" "2016-08-02 19:59:59 UTC" "2016-08-02 20:59:59 UTC" "2016-08-02 21:59:59 UTC" "2016-08-02 22:59:59 UTC"
# > uct <- as.POSIXct(tp2[,1], format="%d/%m/%Y %H:%M:%S", tz="ECT")
# > uct[1]
# [1] "2016-07-31 23:59:59 UTC"
# > typeof(uct[1])
# [1] "double"
# > as.character(uct[1])
# [1] "2016-07-31 23:59:59"
# > ad <- as.character(uct[1])
# > add <- unlist(strsplit(ad, " "))
# > add
# [1] "2016-07-31" "23:59:59"  
# > add[1]
# [1] "2016-07-31"
# > 


# d <- as.Date('2004-01-01')
# month(d) <- month(d) + 1
# day(d) <- days_in_month(d)
# d
# [1] "2004-02-29"
