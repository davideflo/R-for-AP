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
    numd <- lubridate::day(d)
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
    numd <- lubridate::day(d)
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
make_dataset_similar_day <- function(df, meteo, weekday)
{
  d_f <- data_frame()
  
}
#########################################################################################
