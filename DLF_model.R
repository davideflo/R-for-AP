###### Deep Learning Model for forecasting

library(h2o)

######### The dataset is the same as for the GAMM model:
source("R_code/GAMM_model.R")

##########################################################################
#### @brief: model only for weekdays and non-holiday days
#### @param: hourly data
Get_DLF_model <- function(df, meteo, final_date, H)
{
  dfg <- make_dataset_GAMM_model(df, meteo, final_date, H)
  dfg <- as.data.frame(dfg)[which(dfg$tweekday %in% 2:6 & dfg$tholiday == 0),]
  
  dfg2 <- dfg[sample(nrow(dfg)),]
  
  response <- "y"
  regressors <- setdiff(colnames(dfg2), response)
  regressors <- setdiff(regressors, c("tholiday", "tchange_date"))
  
  dlmodel <- h2o.deeplearning(x = regressors, y = response, training_frame = as.h2o(dfg2),
                              standardize = TRUE, activation = "Rectifier", 
                              hidden = c(8760, 365, 52, 12, 7, 24), epochs = 100, max_w2 = 100, l1=1e-5)
  print("R2:")
  print(h2o.r2(dlmodel))
  
  return(dlmodel)
}
##########################################################################
#### @brief: model only for weekdays and non-holiday days
#### @param: hourly data
Get_DLF_model_hol <- function(df, meteo, final_date, H)
{
  dfg <- make_dataset_GAMM_model(df, meteo, final_date, H)
  dfg <- as.data.frame(dfg)[which(dfg$tweekday %in% c(1,7) | dfg$tholiday == 1),]
  
  dfg2 <- dfg[sample(nrow(dfg)),]
  
  response <- "y"
  regressors <- setdiff(colnames(dfg2), c("tholiday",response))
  
  dlmodel <- h2o.deeplearning(x = regressors, y = response, training_frame = as.h2o(dfg2),
                              standardize = TRUE, activation = "Rectifier", 
                              hidden = c(8760, 365, 52, 12, 7, 24), epochs = 100, max_w2 = 100, l1=1e-5)
  print("R2:")
  print(h2o.r2(dlmodel))
  
  return(dlmodel)
}
##########################################################################
#### @brief: model to predict hourly consumption as a time series
#### @param: hourly data
Get_DLF_model_TS <- function(df)
{
  dfg2 <- df[sample(nrow(df)),]
  
  response <- "y"
  regressors <- setdiff(colnames(dfg2), c(response, "num_week", "tnum_week")) 
  
  dlmodel <- h2o.deeplearning(x = regressors, y = response, training_frame = as.h2o(dfg2),
                              standardize = TRUE, activation = "Rectifier", 
                              hidden = 24*c(365, 52, 12, 7, 1), epochs = 100, max_w2 = 100, l1=1e-4, l2=1e-4, elastic_averaging = TRUE,
                              elastic_averaging_regularization = 0.01)
  print("R2:")
  print(h2o.r2(dlmodel))
  
  return(dlmodel)
}
##########################################################################
#### @brief: model to predict hourly consumption as a time series
#### @param: hourly data
Get_DLF_model_TS2 <- function(df)
{
  dfg2 <- df[sample(nrow(df)),]
  
  response <- "y"
  regressors <- setdiff(colnames(dfg2), response)
  
  dlmodel <- h2o.deeplearning(x = regressors, y = response, training_frame = as.h2o(dfg2),
                              standardize = TRUE, activation = "Rectifier", 
                              hidden = c(8760, 365, 52, 12, 7, 24), epochs = 100, max_w2 = 100, l1=1e-5, l2=1e-5, elastic_averaging = TRUE,
                              elastic_averaging_regularization = 0.01)
  print("R2:")
  print(h2o.r2(dlmodel))
  
  return(dlmodel)
}
####################################################################################
#### @brief: function to use DL to predict the consumption as a time series
make_dataset_DLTS <- function(df, meteo, final_date)
{
  d_f <- data_frame()
  meteo2 <- get_meteo2(meteo, final_date)
  aggdf <- Aggregator(df)
  
  for(i in 1:nrow(aggdf))
  {
    d <- as.Date(aggdf$date[i])
    next_day <- d + 2
    
    numd <- lubridate::yday(d)
    numw <- lubridate::week(d)
    wd <- lubridate::wday(d)
    hol <- add_holidays_Date(d)
    cd <- daylight_saving(d)
    tmedia <- associate_meteo_data(d, meteo2, "Tmedia")
    vento <- associate_meteo_data(d, meteo2, "VENTOMEDIA")
    rain <- get_rain_num(meteo, d)
    regr <- unlist(aggdf[i,2:25])
    
    
    if(next_day %in% aggdf$date)
    {
      index <- which(aggdf$date == next_day)
      tnumd <- lubridate::yday(next_day)
      tnumw <- lubridate::week(next_day)
      twd <- lubridate::wday(next_day)
      thol <- add_holidays_Date(next_day)
      tcd <- daylight_saving(next_day)
      ttmedia <- associate_meteo_data(next_day, meteo2, "Tmedia")
      train <- get_rain_num(meteo, next_day)
      tvento <- associate_meteo_data(next_day, meteo2, "VENTOMEDIA")
      
      for(h in 1:24)
      {
        y <- unlist(aggdf[index,which(colnames(aggdf) == as.character(h))])

        df2 <- data.frame(numd, numw, wd, hol, cd, tmedia, vento, rain, t(regr),
                          tnumd, tnumw, twd, thol, tcd, ttmedia, tvento, train, y)

        colnames(df2) <- c("num_day", "num_week", "weekday", "holiday", "change_date", "Tmedia", "vento", "pioggia", paste0("regr",as.character(1:24)),
                           "tnum_day", "tnum_week", "tweekday", "tholiday", "tchange_date", "tTmedia", "tvento", "tpioggia","y")
        
        l <- list(d_f, df2)
        d_f <- rbindlist(l)
      }
      
      
    }
    
    else{
      break
    }
    
  }
  return(d_f)
  
}
####################################################################################
#### @brief: function to use DL to predict the consumption as a time series
make_dataset_DLTS2 <- function(df, meteo, final_date)
{
  d_f <- data_frame()
  meteo2 <- get_meteo2(meteo, final_date)
  aggdf <- Aggregator(df)
  
  for(i in 1:nrow(aggdf))
  {
    d <- as.Date(aggdf$date[i])
    next_day <- d + 2
    
    numd <- lubridate::yday(d)
    numw <- lubridate::week(d)
    wd <- lubridate::wday(d)
    hol <- add_holidays_Date(d)
    cd <- daylight_saving(d)
    tmedia <- associate_meteo_data(d, meteo2, "Tmedia")
    vento <- associate_meteo_data(d, meteo2, "VENTOMEDIA")
    rain <- get_rain_num(meteo, d)
    
    
    
    if(next_day %in% aggdf$date)
    {
      index <- which(aggdf$date == next_day)
      tnumd <- lubridate::yday(next_day)
      tnumw <- lubridate::week(next_day)
      twd <- lubridate::wday(next_day)
      thol <- add_holidays_Date(next_day)
      tcd <- daylight_saving(next_day)
      ttmedia <- associate_meteo_data(next_day, meteo2, "Tmedia")
      train <- get_rain_num(meteo, next_day)
      tvento <- associate_meteo_data(next_day, meteo2, "VENTOMEDIA")
      
      for(h in 1:24)
      {
        regr <- unlist(aggdf[i,h+1])
        y <- unlist(aggdf[index,which(colnames(aggdf) == as.character(h))])
        
        df2 <- data.frame(numd, numw, wd, hol, cd, tmedia, vento, rain, regr,
                          tnumd, tnumw, twd, thol, tcd, ttmedia, tvento, train, y)
        
        colnames(df2) <- c("num_day", "num_week", "weekday", "holiday", "change_date", "Tmedia", "vento", "pioggia","regr",
                           "tnum_day", "tnum_week", "tweekday", "tholiday", "tchange_date", "tTmedia", "tvento", "tpioggia","y")
        
        l <- list(d_f, df2)
        d_f <- rbindlist(l)
      }
      
      
    }
    
    else{
      break
    }
    
  }
  return(d_f)
  
}
################################################################################
#### @brief: function to estimate the best function with a given shape that adapts to the DL-estimated hourly values
Shapificator <- function(fhat, phi)
{
  MSL <- function(fhat, phi, ab)
  {
    return(sum(fhat - ab[1:24]*phi - ab[25])^2 + sum(diff(ab[1:24]*phi,lag=1)^2))
  }
  
  res <- optim(par = rep(1,25), fhat = fhat, phi = phi, MSL)
  
  return(res$par)
  
}
#################################################################################
