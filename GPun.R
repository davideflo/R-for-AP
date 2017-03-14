#### GAMM models for PUN ###
library(readxl)
library(mgcv)
library(lubridate)
library(plyr)
library(dplyr)
library(data.table)


### file di K2E per forward pun: 
### H:\Energy Management\04. WHOLESALE\07. ANALISI MERCATO\01. Indici Mercato\Indici_MERCATO_2016

##############################################################################################################
################################################################################################################
add_holidays_Date <- function(vd)
{
  ##### codifica numerica delle vacanze
  ## 1 Gennaio = 1, Epifania = 2
  ## Pasqua = 3, Pasquetta = 4
  ## 25 Aprile = 5, 1 Maggio = 6, 2 Giugno = 7,
  ## Ferragosto = 8, 1 Novembre = 9
  ## 8 Dicembre = 10, Natale = 11, S.Stefano = 12, S.Silvestro = 13
  holidays <- 0
  pasqua <- as.Date(c("2010-04-04", "2011-04-24", "2012-04-08", "2013-03-31", "2014-04-20", "2015-04-05", "2016-03-27","2017-04-16"))
  pasquetta <- as.Date(c("2010-04-05", "2011-04-25", "2012-04-09", "2013-04-01", "2014-04-21", "2015-04-06", "2016-03-28","2017-04-17"))
  
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
################################################################################################################
make_DLdataset_pun_forward2 <- function(data)
{
  ### @BRIEF: better function: should work automatically, without expliciting the dates
  d_f <- data_frame()

  for(i in 1:nrow(data))
  {
    #print(i)
    y <- 0
    hs <-  lubridate::hour(as.POSIXct(unlist(data[i,'date']), origin = '1970-01-01'))
    wds <- lubridate::wday(as.POSIXct(unlist(data[i,'date']), origin = '1970-01-01'))
    wdys <- lubridate::yday(as.POSIXct(unlist(data[i,'date']), origin = '1970-01-01'))
    wks <-  lubridate::week(as.POSIXct(unlist(data[i,'date']), origin = '1970-01-01'))
    hol <-  add_holidays_Date(as.POSIXct(unlist(data[i,'date']), origin = '1970-01-01'))
    mon <- lubridate::month(as.POSIXct(unlist(data[i,'date']), origin = '1970-01-01'))
    
    new_date <- lubridate::ymd(as.Date(data$date[i])) + lubridate::years(1)
    new_date2 <- lubridate::ymd(as.Date(data$date[i]) + 1) + lubridate::years(1)
    
    if(!is.na(new_date))
    {
      ypun <- data[which(as.Date(data$date) == new_date),]
      
      
      ypun <- ypun[which(lubridate::hour(as.POSIXct(ypun$date, origin = '1970-01-01')) == hs),]
      
      PK <- 0
      OP <- 0
      F1 <- 0
      F2 <- 0
      F3 <- 0
      
      
      nr <- nrow(ypun)
      
      if(nr == 0)
      {
        y <- 0
      }
      else if(nr == 2)
      {
        y <- sum(ypun$pun, na.rm = FALSE)
        
      }
      else
      {
        y <- ypun$pun
        
        if(unlist(ypun[,3]) == 'OP')
        {
          OP <- 1
        }
        else
        {
          PK <- 1
        }  
        
        if(unlist(ypun[,4]) == 'F1')
        {
          F1 <- 1
        }
        else if(unlist(ypun[,4]) == 'F2')
        {
          F2 <- 1
        }
        else
        {
          F3 <- 1
        }
        
      }
      
      twds <- lubridate::wday(new_date)
      twdys <- lubridate::yday(new_date)
      twks <-  lubridate::week(new_date)
      tmonth <- lubridate::month(new_date)
      thol <-  add_holidays_Date(new_date)
      
      df2 <- data.frame(data$pun[i], hs, wds, wdys, wks, hol,mon, y, hs, twds, twdys, twks, thol, OP, PK, F1, F2, F3, tmonth)
      colnames(df2) <- c("lpun","hour","weekday","day","week","holiday", "month","ypun","thour","tweekday","tday","tweek","tholiday", "OP", "PK", "F1", "F2", "F3","tmonth")
      #d_f <- bind_rows(d_f, df2)
      l <- list(data.frame(d_f), df2)
      d_f <- rbindlist(l)
    }
    
    else if(is.na(new_date) & new_date2 %in% data$date)
    {
      next
    }
    
    else
    {
      break
    }
    
  }
  colnames(d_f) <- c("lpun","hour","weekday","day","week","holiday", "month","ypun","thour","tweekday","tday","tweek","tholiday", "OP", "PK", "F1", "F2", "F3","tmonth")
  return(d_f)
}
#####################################################################
prediction_pun_forward2 <- function(df, start_date)
{
  d_f <- data_frame()
  
  days_left <- 365 - (as.Date(start_date) - as.Date("2017-01-01"))
  seq17 <- seq.POSIXt(as.POSIXct(start_date), as.POSIXct('2018-01-02'), by = 'hour')[1:((days_left*24)-1)]
  
  pun16 <- df[which(lubridate::year(df$date) == 2016),]
  
  for(i in 1:nrow(pun16))
  {
    #print(i)
    hs <-  lubridate::hour(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    wds <- lubridate::wday(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    wdys <- lubridate::yday(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    wks <-  lubridate::week(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    hol <-  add_holidays_Date(as.Date(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01')))
    
    new_date <- lubridate::ymd(as.Date(pun16$date[i])) + lubridate::years(1)
    new_date2 <- lubridate::ymd(as.Date(pun16$date[i]) + 1) + lubridate::years(1)
    
    if(!is.na(new_date))
    {
      
      twds <- lubridate::wday(new_date)
      twdys <- lubridate::yday(new_date)
      twks <-  lubridate::week(new_date)
      thol <-  add_holidays_Date(new_date)
      
      df2 <- data.frame(pun16$pun[i], hs, wds, wdys, wks, hol, hs, twds, twdys, twks, thol)
      #colnames(df2) <- c("lpun","hour","weekday","day","week","holiday","ypun","thour","tweekday","tday","tweek","tholiday")
      #d_f <- bind_rows(d_f, df2)
      l <- list(data.frame(d_f), df2)
      d_f <- rbindlist(l)
    }
    
    else
    {
      next
    }
    
  }
  colnames(d_f) <- c("lpun","hour","weekday","day","week","holiday","thour","tweekday","tday","tweek","tholiday")
  return(d_f)
}
#############################################################################################################
CleanDataset <- function(dt)
{
  il <- c()
  for(i in seq(nrow(dt),1, length.out = nrow(dt)))
  {
    lil <- length(il)
    if(dt$ypun[i] == 0) il <- c(il, i)
    if(length(il) == lil)
    {
      break
    }
  }
  return(dt[-il,])
}
#############################################################################################################

library(feather)

data2 <- read_excel("dati_2014-2017.xlsx") #### da python
colnames(data2) <- c('date', 'pun')
data2$date <- seq.POSIXt(as.POSIXct('2014-01-01'), as.POSIXct('2017-02-21'), by = 'hour')[1:nrow(data2)]

DLD2 <- make_DLdataset_pun_forward2(data2)
DLD2 <- CleanDataset(DLD2) ## last number += difference in days from last update * 24)
write_feather(DLD2, "dati_punForward")

pred17 <- prediction_pun_forward2(data2, "2017-03-11") ### 2 days ahead from last date of PUN

CleanDataset(dt)

for(m in 1:12)
{
  mDLD2 <- DLD2[which(DLD2$tmonth == m & DLD2$PK == 1),]
  print(paste("PK/OP average spread in month",m," = ", mean(unlist(DLD2[which(DLD2$tmonth == m & DLD2$PK == 1),"lpun"])) - mean(unlist(DLD2[which(DLD2$tmonth == m & DLD2$PK == 0),"lpun"])) ))
}

# library(h2o)
# #### C:\Program Files\R\R-3.3.2\library\h2o\java
# h2o.init(nthreads = -1, max_mem_size = '20g')
# 
# response <- "ypun"
# regressors <- setdiff(colnames(DLD2),response)
# 
# modeldl <- h2o.deeplearning(x = regressors, y = response, training_frame = as.h2o(DLD2), standardize = TRUE, activation = "Rectifier", 
#                             hidden = c(8760, 365, 52, 12, 7, 24), epochs = 100)
# 
# 
# 
# plot(modeldl)
# summary(modeldl)
# h2o.r2(modeldl)

library(neuralnet)

maxs <- apply(DLD2, 2, max) 
mins <- apply(DLD2, 2, min)

scaled <- as.data.frame(scale(DLD2, center = mins, scale = maxs - mins))
n <- names(DLD2)
f <- as.formula(paste("ypun ~", paste(n[!n %in% "ypun"], collapse = " + ")))
nn <- neuralnet(f,data=scaled,hidden=c(100,100,100),linear.output=T)




