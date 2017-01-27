#### GAMM model for forecasting
#### It uses the hourly data of two days before.
#### For the non-hourly, it will be used in conjunction with a "shape-fitting" algorithm.
library(mgcv)
library(ggplot2)

source("R_code/functions_for_POD_orari.R")

#######################################################################
fitted_vs_residuals <- function(fitted, residuals)
{  
  ggplot(data = data.table(fitted = fitted, residuals = residuals),
       aes(fitted, residuals)) +
  geom_point(size = 1.5) +
  geom_smooth() +
  labs(title = "fitted vs residuals")
}
#######################################################################
###
get_GAMM_model <- function(df, meteo, final_date, H)
{
  dfg <- make_dataset_GAMM_model(df, meteo, final_date, H)
  
  ctrl <- list(niterEM = 100, msVerbose = TRUE, optimMethod="L-BFGS-B")
  
  
  m3 <- tryCatch(
    {
      eval(substitute(gamm(y ~ regr1 + regr2 + regr3 + regr4 + regr5 + regr6 +
                                 regr7 + regr8 + regr9 + regr10 + regr11 + regr12 +
                                 regr13 + regr14 + regr15 + regr16 + regr17 + regr18 +
                                 regr19 + regr20 + regr21 + regr22 + regr23 + regr24 +
                                 holiday +
                                 pioggia +
                                 vento + 
                                 s(Tmedia, bs = "cc") +
                                 tholiday +
                                 tpioggia +
                                 tvento + 
                                 s(tTmedia, bs = "cc") +
                                 s(num_day, bs = "cc") + s(num_week, bs = "cc", k = max(num_week)) + s(weekday, bs = "cc", k = 7) +
                                 s(tnum_day, bs = "cc") + s(tnum_week, bs = "cc", k =  max(tnum_week)) + s(tweekday, bs = "cc", k = 7), data = dfg,
                                 correlation = corARMA(form = ~ 1|num_day, p = 2), control = ctrl, niterPQL = 500)), dfg)
    }, error = function(cond)
    {
      print("switch to simpler model")
      m4 <- eval(substitute(gamm(y ~ regr1 + regr2 + regr3 + regr4 + regr5 + regr6 +
                                   regr7 + regr8 + regr9 + regr10 + regr11 + regr12 +
                                   regr13 + regr14 + regr15 + regr16 + regr17 + regr18 +
                                   regr19 + regr20 + regr21 + regr22 + regr23 + regr24 +
                                   holiday +
                                   pioggia +
                                   vento + 
                                   s(Tmedia, bs = "cc") +
                                   tholiday +
                                   tpioggia +
                                   tvento + 
                                   s(tTmedia, bs = "cc") +
                                   s(num_day, bs = "cc") + s(num_week, bs = "cc", k = max(num_week)) + s(weekday, bs = "cc", k = 7) +
                                   s(tnum_day, bs = "cc") + s(tnum_week, bs = "cc", k =  max(tnum_week)) + s(tweekday, bs = "cc", k = 7),
                                   data = dfg, niterPQL = 500)), dfg)
      return(m4)
    }
  )
  
  return(m3)
  
  
}
#######################################################################
#### @ param: df is the dataset of the hourly measurement of a given zone
####          H is the target hour --> there will be a model for every hour for every zone
make_dataset_GAMM_model <- function(df, meteo, final_date, H)
{
  d_f <- data_frame()
  meteo2 <- get_meteo2(meteo, final_date)
  aggdf <- Aggregator(df)
  
  for(i in 1:nrow(aggdf))
  {
    d <- as.Date(aggdf$date[i])
    print(d)
    numd <- lubridate::yday(d)
    numw <- lubridate::week(d)
    wd <- lubridate::wday(d)
    hol <- add_holidays_Date(d)
    cd <- daylight_saving(d)
    tmedia <- associate_meteo_data(d, meteo2, "Tmedia")
    vento <- associate_meteo_data(d, meteo2, "VENTOMEDIA")
    rain <- get_rain_num(meteo, d)
    regr <- unlist(aggdf[i,2:25])
    
    next_day <- d + 2
    print(next_day)
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
      y <- unlist(aggdf[index,which(colnames(aggdf) == as.character(H))])
      df2 <- data.frame(numd, numw, wd, hol, cd, tmedia, vento, rain, t(regr),
                        tnumd, tnumw, twd, thol, tcd, ttmedia, tvento, train, y)
      
      colnames(df2) <- c("num_day", "num_week", "weekday", "holiday", "change_date", "Tmedia", "vento", "pioggia", paste0("regr",as.character(1:24)),
                         "tnum_day", "tnum_week", "tweekday", "tholiday", "tchange_date", "tTmedia", "tvento", "tpioggia","y")
      
      l <- list(d_f, df2)
      d_f <- rbindlist(l)
    }
    
    else{
      break
    }
    
  }
  return(d_f)

}