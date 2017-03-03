##### Automatic & executable Pun Forward

###### PUN FORWARD LONG TERM #######
library(plyr)
library(dplyr)
library(readxl)
library(feather)
library(lubridate)
library(data.table)
library(h2o)


source("C://Users//utente//Documents//R_code//functions_for_corr_meteo_pun.R")
source("C://Users//utente//Documents//R_code//functions_for_POD_orari.R")
source("C://Users//utente//Documents//glm_dataset.R")


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
##############################################################################################################################################
make_DLdataset_pun_forward2 <- function(data)
{
  ### @BRIEF: better function: should work automatically, without expliciting the dates
  ### http://www.noamross.net/blog/2013/4/25/faster-talk.html
  d_f <- data_frame()
  
  # feb29 <- which(as.Date(data$date) == as.Date("2016-02-29")) 
  # Data <- data[-feb29,]
  # pun14 <- Data[which(lubridate::year(Data$date) == 2014),]
  # pun15 <- Data[which(lubridate::year(Data$date) == 2015),]
  # pun16 <- Data[which(lubridate::year(Data$date) == 2016),]
  
  for(i in 1:nrow(data))
  {
    print(i)
    
    hs <-  lubridate::hour(as.POSIXct(unlist(data[i,'date']), origin = '1970-01-01'))
    wds <- lubridate::wday(as.POSIXct(unlist(data[i,'date']), origin = '1970-01-01'))
    wdys <- lubridate::yday(as.POSIXct(unlist(data[i,'date']), origin = '1970-01-01'))
    wks <-  lubridate::week(as.POSIXct(unlist(data[i,'date']), origin = '1970-01-01'))
    hol <-  add_holidays_Date(as.Date(unlist(data[i,'date'])))
    
    new_date <- lubridate::ymd(as.Date(data$date[i])) + lubridate::years(1)
    new_date2 <- lubridate::ymd(as.Date(data$date[i]) + 1) + lubridate::years(1)
    
    if(!is.na(new_date))
    {
      ypun <- data[which(as.Date(data$date) == new_date),]
      #& lubridate::hour(as.POSIXct(unlist(data$date), origin = '1970-01-01')) == hs),]
      
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
#############################################################################################################
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
#################################################################################
Assembler <- function(real, ph)
{
  rows <- which(unlist(is.na(real[,13])))
  assembled <- data.frame(ph$date, c(unlist(real[1:(rows[1]-1),13]), unlist(ph[rows[1]:8760,2])), c(rep(1, (8760-length(rows))),rep(0,length(rows))))
  colnames(assembled) <- c("date", "pun", "real")
  return(assembled)
}
#################################################################################
Redimensioner <- function(ph, mh, from, to)
{
  d_f <- data_frame()
  from <- as.Date(from)
  to <- as.Date(to)
  period <- ph[which(as.Date(ph$date) >= from & as.Date(ph$date) <= to),]
  M <- nrow(period)
  phb <- (1/M)*sum(period$pun[period$real == 0])
  pb <- ifelse(length(period$pun[period$real == 1]) > 0, (1/M)*sum(period$pun[period$real == 1]), 0)
  pihat <- (mh - pb)/phb
  period$pun[period$real == 0] <- pihat * period$pun[period$real == 0]
  
  d_f <- bind_rows(d_f, ph[which(as.Date(ph$date) < from),])
  d_f <- bind_rows(d_f, period)
  d_f <- bind_rows(d_f, ph[which(as.Date(ph$date) > to),])
  
  return(d_f)
}
#################################################################################
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


######## automatic dataset #### USA QUESTO PER PUN FORWARD

system('python DataAggregator.py')


library(h2o)
h2o.init(nthreads = -1, max_mem_size = '20g')


data2 <- read_excel("C:/Users/utente/Documents/misure/dati_2014-2017.xlsx")
colnames(data2) <- c('date', 'pun')

last_date <- as.Date(unlist(data2[nrow(data2),1]), origin = "1899-12-30")

data2$date <- seq.POSIXt(as.POSIXct('2014-01-01'), as.POSIXct(as.character(last_date + 2)), by = 'hour')[1:nrow(data2)]

DLD2 <- make_DLdataset_pun_forward2(data2)
DLD2 <- CleanDataset(DLD2)


response <- "ypun"
regressors <- setdiff(colnames(DLD2),response)

modeldl2 <- h2o.deeplearning(x = regressors, y = response, training_frame = as.h2o(DLD2),
                             standardize = TRUE, activation = "Rectifier", 
                             hidden = c(8760, 365, 52, 12, 7, 24), epochs = 100, elastic_averaging = TRUE,
                             elastic_averaging_regularization = 0.01)


plot(modeldl2)
print(summary(modeldl2))
print(h2o.r2(modeldl2))

pred17 <- prediction_pun_forward2(data2, as.character(last_date + 2)) ### 2 days ahead from last date of PUN

yhat17 <- h2o.predict(modeldl2, newdata = as.h2o(pred17))
yhat17 <- unlist(as.matrix(as.numeric(yhat17$predict)))

#plot(yhat17, type = 'l', col = 'orange')

real <- read_excel("DB_Borse_Elettriche_PER MI_17_conMacro - Copy.xlsm", sheet = 2)

### paste existing 2017 pun
sequence_dates <- seq.POSIXt(as.POSIXct('2017-01-01'), as.POSIXct('2018-01-01'), by = 'hour')
ph <- data.frame(sequence_dates[1:8760], yhat17)
colnames(ph) <- c("date", "pun")

PH <- Assembler(real, ph)

mean(PH$pun)
PC <- read_excel('Power Curves.xlsx', skip = 3)

RPH <- Redimensioner(PH, unlist(PC[2,2]), "2017-01-01", "2017-01-31")
RPH <- Redimensioner(RPH, unlist(PC[3,2]), "2017-02-01", "2017-02-28")
RPH <- Redimensioner(RPH, unlist(PC[4,2]), "2017-03-01", "2017-03-31")
RPH <- Redimensioner(RPH, unlist(PC[5,2]), "2017-04-01", "2017-04-30")
RPH <- Redimensioner(RPH, unlist(PC[6,2]), "2017-05-01", "2017-05-31")
RPH <- Redimensioner(RPH, unlist(PC[7,2]), "2017-06-01", "2017-06-30")
RPH <- Redimensioner(RPH, unlist(PC[8,2]), "2017-07-01", "2017-07-31")
RPH <- Redimensioner(RPH, unlist(PC[9,2]), "2017-08-01", "2017-08-31")
RPH <- Redimensioner(RPH, unlist(PC[10,2]), "2017-09-01", "2017-09-30")
RPH <- Redimensioner(RPH, unlist(PC[11,2]), "2017-10-01", "2017-10-31")
RPH <- Redimensioner(RPH, unlist(PC[12,2]), "2017-11-01", "2017-11-30")
RPH <- Redimensioner(RPH, unlist(PC[13,2]), "2017-12-01", "2017-12-31")

# plot(RPH$pun, type = 'l', col = "blue3")
# lines(RPH$pun, type = 'l', col = "blue3")
# 
# 
# mean(RPH$pun[as.Date(RPH$date) <= as.Date("2017-01-31")])
# mean(PH$pun[as.Date(PH$date) <= as.Date("2017-01-31")])
# mean(RPH$pun)

library(xlsx)
write.xlsx(RPH, "longterm_pun.xlsx")

#### send email 
