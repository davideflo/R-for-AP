##### Ricalendarizzatore automatico #####

library(plyr)
library(dplyr)
library(readxl)
library(feather)
library(lubridate)
library(data.table)


################################################################################################################
Is_holiday_Date <- function(vd)
{
  ##### codifica numerica delle vacanze
  ## 1 Gennaio = 1, Epifania = 2
  ## Pasqua = 3, Pasquetta = 4
  ## 25 Aprile = 5, 1 Maggio = 6, 2 Giugno = 7,
  ## Ferragosto = 8, 1 Novembre = 9
  ## 8 Dicembre = 10, Natale = 11, S.Stefano = 12, S.Silvestro = 13
  holidays <- 0
  pasqua <- as.Date(c("2010-04-04", "2011-04-24", "2012-04-08", "2013-03-31", "2014-04-20", "2015-04-05", "2016-03-27","2017-04-16", "2018-04-01"))
  pasquetta <- as.Date(c("2010-04-05", "2011-04-25", "2012-04-09", "2013-04-01", "2014-04-21", "2015-04-06", "2016-03-28","2017-04-17", "2018-04-02"))
  
  if(lubridate::month(vd) == 1 & lubridate::day(vd) == 1) holidays <- 1
  if(lubridate::month(vd)  == 1 & lubridate::day(vd) == 6) holidays <- 1
  if(lubridate::month(vd)  == 4 & lubridate::day(vd) == 25) holidays <- 1
  if(lubridate::month(vd)  == 5 & lubridate::day(vd) == 1) holidays <- 1
  if(lubridate::month(vd)  == 6 & lubridate::day(vd) == 2) holidays <- 1
  if(lubridate::month(vd)  == 8 & lubridate::day(vd) == 15) holidays <- 1
  if(lubridate::month(vd)  == 11 & lubridate::day(vd) == 1) holidays <- 1
  if(lubridate::month(vd)  == 12 & lubridate::day(vd) == 8) holidays <- 1
  if(lubridate::month(vd)  == 12 & lubridate::day(vd) == 25) holidays <- 1
  if(lubridate::month(vd)  == 12 & lubridate::day(vd) == 26) holidays <- 1
  if(lubridate::month(vd)  == 12 & lubridate::day(vd) == 31) holidays <- 1
  if(vd %in% pasqua) holidays <- 1
  if(vd %in% pasquetta) holidays <- 1
  
  return(holidays)
}
##################################################################################################################
findClosestUsefulDay <- function(y5, lag, lab, pun, h)
{
  ny5 <- y5 + lag
  period <- pun$PUN[which(lubridate::month(as.Date(pun$Date, origin = '1899-12-30'))  == lubridate::month(ny5) & 
                        lubridate::day(as.Date(pun$Date, origin = '1899-12-30')) == lubridate::month(ny5) & 
                        as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == lab &
                        pun$Lavorativo/Festivo == "Lavorativo" & pun$Hour == h)]
  
  return(period)
}
##################################################################################################################
AutomaticRicalendarizator <- function(year2, pun)
{
  endays <- c("Sunday", "Monday",  "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  df <- data_frame()
  for( yy in year2)
  {
    y <- as.Date(y, origin = '1899-12-30')
    y5 <- y - years(1)
    h <- lubridate::hour(yy)
    if(Is_holiday_Date(y) == 1 & Is_holiday_Date(y5) == 1) ### if it's a holiday --> corresponding holiday
    {
      y1 <- pun$PUN[which(lubridate::month(as.Date(pun$Date, origin = '1899-12-30'))  == lubridate::month(y) & 
                        lubridate::day(as.Date(pun$Date, origin = '1899-12-30')) == lubridate::month(y) & pun$Hour == h)]
      d.f <- data.frame(date = yy, pun = y1)
      l <- list(df, d.f)
      df <- rbindlist(l)
    }
    else
    {
      if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Monday" & Is_holiday_Date(y + 1) == 1)
      {
        y1 <- pun$PUN[which(lubridate::month(as.Date(pun$Date, origin = '1899-12-30'))  == lubridate::month(y - 2) & 
                              lubridate::day(as.Date(pun$Date, origin = '1899-12-30')) == lubridate::month(y - 2) & pun$Hour == h)]
        d.f <- data.frame(date = yy, pun = y1)
        l <- list(df, d.f)
        df <- rbindlist(l)
      } 
      else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Friday" & Is_holiday_Date(y - 1) == 1)
      {
        y1 <- pun$PUN[which(lubridate::month(as.Date(pun$Date, origin = '1899-12-30'))  == lubridate::month(y + 1) & 
                              lubridate::day(as.Date(pun$Date, origin = '1899-12-30')) == lubridate::month(y + 1) & pun$Hour == h)]
        d.f <- data.frame(date = yy, pun = y1)
        l <- list(df, d.f)
        df <- rbindlist(l)
      }
      else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) %in% c("Tuesday", "Wednesday", "Thursday") &
              as.character(lubridate::wday(y5, label = TRUE, abbr = FALSE)) %in% c("Tuesday", "Wednesday", "Thursday"))
      {
        y1 <- pun$PUN[which(lubridate::month(as.Date(pun$Date, origin = '1899-12-30'))  == lubridate::month(y) & 
                              lubridate::day(as.Date(pun$Date, origin = '1899-12-30')) == lubridate::month(y) & pun$Hour == h)]
        d.f <- data.frame(date = yy, pun = y1)
        l <- list(df, d.f)
        df <- rbindlist(l)
      }
      else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Monday" & Is_holiday_Date(y + 1) == 0)
      {
        if(as.character(lubridate::wday(y5, label = TRUE, abbr = FALSE)) == "Monday")
        {
          #### every time subset all the Mondays not-holidays of the month and of the following one --> take the closest one
          #### same thing for the other days
          y1 <- findClosestUsefulDay(y5, 0, "Monday", pun, h)
          d.f <- data.frame(date = yy, pun = y1)
          l <- list(df, d.f)
          df <- rbindlist(l)
        }
        else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Tuesday")
        {
          y1 <- findClosestUsefulDay(y5, -1, "Monday", pun, h)
          d.f <- data.frame(date = yy, pun = y1)
          l <- list(df, d.f)
          df <- rbindlist(l)
        }
        else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Wednesday")
        {
          y1 <- findClosestUsefulDay(y5, -2, "Monday", pun, h)
          d.f <- data.frame(date = yy, pun = y1)
          l <- list(df, d.f)
          df <- rbindlist(l)
        }
        else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Thursday")
        {
          y1 <- findClosestUsefulDay(y5, 4, "Monday", pun, h)
          d.f <- data.frame(date = yy, pun = y1)
          l <- list(df, d.f)
          df <- rbindlist(l)
        }
        else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Friday")
        {
          y1 <- findClosestUsefulDay(y5, 3, "Monday", pun, h)
          d.f <- data.frame(date = yy, pun = y1)
          l <- list(df, d.f)
          df <- rbindlist(l)
        }
        else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Saturday")
        {
          y1 <- findClosestUsefulDay(y5, 2, "Monday", pun, h)
          d.f <- data.frame(date = yy, pun = y1)
          l <- list(df, d.f)
          df <- rbindlist(l)
        }
        ##else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Sunday")
        else
        {
          y1 <- findClosestUsefulDay(y5, 1, "Monday", pun, h)
          d.f <- data.frame(date = yy, pun = y1)
          l <- list(df, d.f)
          df <- rbindlist(l)
        }
        
      }
      ### same as above for Friday
      else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Friday" & Is_holiday_Date(y - 1) == 0)
      {
        if(as.character(lubridate::wday(y5, label = TRUE, abbr = FALSE)) == "Monday")
        {
          #### every time subset all the Mondays not-holidays of the month and of the following one --> take the closest one
          #### same thing for the other days
          y1 <- findClosestUsefulDay(y5, 4, "Friday", pun, h)
          d.f <- data.frame(date = yy, pun = y1)
          l <- list(df, d.f)
          df <- rbindlist(l)
        }
        else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Tuesday")
        {
          y1 <- findClosestUsefulDay(y5, 3, "Friday", pun, h)
          d.f <- data.frame(date = yy, pun = y1)
          l <- list(df, d.f)
          df <- rbindlist(l)
        }
        else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Wednesday")
        {
          y1 <- findClosestUsefulDay(y5, 2, "Friday", pun, h)
          d.f <- data.frame(date = yy, pun = y1)
          l <- list(df, d.f)
          df <- rbindlist(l)
        }
        else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Thursday")
        {
          y1 <- findClosestUsefulDay(y5, 1, "Friday", pun, h)
          d.f <- data.frame(date = yy, pun = y1)
          l <- list(df, d.f)
          df <- rbindlist(l)
        }
        else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Friday")
        {
          y1 <- findClosestUsefulDay(y5, 0, "Friday", pun, h)
          d.f <- data.frame(date = yy, pun = y1)
          l <- list(df, d.f)
          df <- rbindlist(l)
        }
        else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Saturday")
        {
          y1 <- findClosestUsefulDay(y5, -1, "Friday", pun, h)
          d.f <- data.frame(date = yy, pun = y1)
          l <- list(df, d.f)
          df <- rbindlist(l)
        }
        ##else if(as.character(lubridate::wday(y, label = TRUE, abbr = FALSE)) == "Sunday")
        else
        {
          y1 <- findClosestUsefulDay(y5, -2, "Friday", pun, h)
          d.f <- data.frame(date = yy, pun = y1)
          l <- list(df, d.f)
          df <- rbindlist(l)
        }
        
      }
    }
  }
  return(df)
}