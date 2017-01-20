##############
### @BRIEF: functions for "similar days" model
###         forecast hourly consumption based on the same day of the week before
#############

###### INFO: functions in alphabetical order and only for this model

source("R_code/functions_for_POD_orari.R")

#########################################################################################
### @PARAM: data from hourly measurement
get_Table_similar_days <- function(df)
{
  aggdf <- Aggregator(df)
  d_f <- data_frame()
  for(i in 1:nrow(aggdf))
  {
    d <- as.Date(aggdf$date[i])
    numd <- lubridate::yday(d)
    numw <- lubridate::week(d)
    wd <- lubridate::wday(d)
    hol <- add_holidays_Date(d)
    cd <- daylight_saving(d)
    df2 <- data.frame(as.Date(d), numd, numw, wd, hol, cd, t(unlist(aggdf[i,2:25])))
    d_f <- bind_rows(d_f, df2)
  }
  colnames(d_f) <- c("date", "num_day", "num_week", "weekday", "holiday", "change_date", as.character(1:24))
  return(d_f)
}
##########################################################################################
### @PARAM: data from TERNA's report
###         variable gives the choice of which measurement can be taken
get_Table_similar_days2 <- function(df, zona, variable)
{
  tdf <- df[which(df$`CODICE RUC` == paste0("UC_DP1608_", zona)),]
  tdf$`DATA RIFERIMENTO CORRISPETTIVO` <- strptime(tdf$`DATA RIFERIMENTO CORRISPETTIVO`, '%d/%m/%Y %H:%M')
  days <- unique(as.Date(tdf$`DATA RIFERIMENTO CORRISPETTIVO`))
  d_f <- data_frame()
  for(i in 1:length(days))
  {
    vec <- rep(0, 24)
    d <- days[i]
    numd <- lubridate::yday(d)
    numw <- lubridate::week(d)
    wd <- lubridate::wday(d)
    hol <- add_holidays_Date(d)
    cd <- daylight_saving(d)
    atdf <- tdf[which(as.Date(tdf$`DATA RIFERIMENTO CORRISPETTIVO`) == d),]
    
    for(h in 0:23)
    {
      j <- which(lubridate::hour(atdf$`DATA RIFERIMENTO CORRISPETTIVO`) == h)
      
      if(length(j) == 1) vec[h+1] <- unlist(atdf[j,variable])
      else if(length(j) > 1) vec[h+1] <- sum(unlist(atdf[j,variable]), na.rm = TRUE)
      else next
    }
    
    df2 <- data.frame(as.Date(d), numd, numw, wd, hol, cd, t(vec))
    d_f <- bind_rows(d_f, df2)
  }
  colnames(d_f) <- c("date", "num_day", "num_week", "weekday", "holiday", "change_date", as.character(1:24))
  return(d_f)
}
###########################################################################################
### @PARAM: data from hourly measurement
make_dataset_similar_day <- function(df, meteo, final_date, weekday)
{
  d_f <- data_frame()
  aggdf <- get_Table_similar_days(df)
  meteo2 <- get_meteo2(meteo, final_date)
  
  onlydf <- aggdf[which(aggdf$weekday == weekday),]
  
  for(i in 1:nrow(onlydf))
  {
    d <- as.Date(onlydf$date[i])
    print(d)
    numd <- lubridate::yday(d)
    numw <- lubridate::week(d)
    wd <- lubridate::wday(d)
    hol <- add_holidays_Date(d)
    cd <- daylight_saving(d)
    tmedia <- associate_meteo_data(d, meteo2, "Tmedia")
    vento <- associate_meteo_data(d, meteo2, "VENTOMEDIA")
    #rain <- get_rain(meteo, d)
    rain <- get_rain_num(meteo, d)
    regr <- unlist(onlydf[i,7:30])
    
    next_week <- d + 7
    print(next_week)
    if(next_week %in% onlydf$date)
    {
      index <- which(onlydf$date == next_week)
      tnumd <- lubridate::yday(next_week)
      tnumw <- lubridate::week(next_week)
      twd <- lubridate::wday(next_week)
      thol <- add_holidays_Date(next_week)
      tcd <- daylight_saving(next_week)
      ttmedia <- associate_meteo_data(next_week, meteo2, "Tmedia")
      #train <- get_rain(meteo, d)
      train <- get_rain_num(meteo, next_week)
      tvento <- associate_meteo_data(next_week, meteo2, "VENTOMEDIA")
      y <- unlist(onlydf[index,7:30])
      df2 <- data.frame(numd, numw, wd, hol, cd, tmedia, vento, rain, t(regr),
                        tnumd, tnumw, twd, thol, tcd, ttmedia, tvento, train, t(y))
      colnames(df2) <- c("num_day", "num_week", "weekday", "holiday", "change_date", "Tmedia", "vento", "pioggia", paste0("regr",as.character(1:24)),
                         "tnum_day", "tnum_week", "tweekday", "tholiday", "tchange_date", "tTmedia", "tvento", "tpioggia", paste0("y",as.character(1:24)))
      l <- list(d_f, df2)
      d_f <- rbindlist(l)
    }
    
    else{
      break
    }
    
  }
  return(d_f)
  
}
#########################################################################################
