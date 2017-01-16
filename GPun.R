#### GAMM models for PUN ###
library(readxl)
library(mgcv)
library(lubridate)
library(plyr)
library(dplyr)
library(data.table)

data <- read_excel('dati_2014-2016.xlsx')
colnames(data)
dim(data)

ConvertDate <- function(df)
{
  df <- as.data.frame(df)
  dfc <- as.POSIXct(unlist(df[,1]), origin = "1970-01-01")
  df$Giorno <- dfc
  return(df[,c(3,2)])
}

cdata <- ConvertDate(data)

plot(cdata, type = 'l', lwd = 2)

Date <- seq.POSIXt(from=as.POSIXct('2014-01-01'), to = as.POSIXct('2016-12-14'), by='hour')
DM <- data.frame(Date[1:(length(Date)-1)], unlist(data[,2]), c(1:nrow(data)))
colnames(DM) <- c("Date", "daily_mean", "day.of.year")

fit <- gamm(daily_mean ~ s(day.of.year, bs = "cc"), data = DM)
summary(fit)

plot(scale(unlist(cdata[,2])), type = 'l', lwd = 2)
lines(scale(fit$gam$fitted.values), type = 'l', lwd = 2, col = 'blue')

DT <- as.data.table(cdata)
DT[,sd(pun),by=floor_date(Date[1:(length(Date)-1)],"day")]
plot(DT[,sd(pun),by=floor_date(Date[1:(length(Date)-1)],"day")], type = 'l', lwd = 2)

DT2 <- DT[,sd(pun),by=floor_date(Date[1:(length(Date)-1)],"day")]
colnames(DT2) <- c("dd", "sd_pun")
DT2 <- data.frame(DT2)
DT2['dd'] <- c(1:nrow(DT2))

fit2 <- gamm(sd_pun ~ s(dd, bs = "cc"), data = DT2)
summary(fit2)

plot(scale(unlist(DT2[,2])), type = 'l', lwd = 2)
lines(scale(fit2$gam$fitted.values), type = 'l', lwd = 2, col = 'blue')
abline(v = 365, col = 'blue', lwd = 2)
abline(v = 730, col = 'red', lwd = 2)

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
    
    new_date <- lubridate::ymd(as.Date(data$date[i])) + lubridate::years(1)
    new_date2 <- lubridate::ymd(as.Date(data$date[i]) + 1) + lubridate::years(1)
    
    if(!is.na(new_date))
    {
      ypun <- data[which(as.Date(data$date) == new_date),]
      
      
      ypun <- ypun[which(lubridate::hour(as.POSIXct(ypun$date, origin = '1970-01-01')) == hs),]
      
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
      }
      
      twds <- lubridate::wday(new_date)
      twdys <- lubridate::yday(new_date)
      twks <-  lubridate::week(new_date)
      thol <-  add_holidays_Date(new_date)
      
      df2 <- data.frame(data$pun[i], hs, wds, wdys, wks, hol, y, hs, twds, twdys, twks, thol)
      colnames(df2) <- c("lpun","hour","weekday","day","week","holiday","ypun","thour","tweekday","tday","tweek","tholiday")
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
  colnames(d_f) <- c("lpun","hour","weekday","day","week","holiday","ypun","thour","tweekday","tday","tweek","tholiday")
  return(d_f)
}
#####################################################################

data2 <- read_excel("dati_2014-2017.xlsx")
colnames(data2) <- c('date', 'pun')
data2$date <- seq.POSIXt(as.POSIXct('2014-01-01'), as.POSIXct('2017-01-18'), by = 'hour')[1:nrow(data2)]

DLD2 <- make_DLdataset_pun_forward2(data2)
DLD2 <- DLD2[1:17904]

library(h2o)
#### C:\Program Files\R\R-3.3.2\library\h2o\java
h2o.init(nthreads = -1, max_mem_size = '20g')

response <- "ypun"
regressors <- setdiff(colnames(DLD2),response)

modeldl <- h2o.deeplearning(x = regressors, y = response, training_frame = as.h2o(DLD2), standardize = TRUE, activation = "Rectifier", 
                            hidden = c(8760, 365, 52, 12, 7, 24), epochs = 100)

plot(modeldl)
summary(modeldl)
h2o.r2(modeldl)