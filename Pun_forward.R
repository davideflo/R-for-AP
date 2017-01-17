###### PUN FORWARD LONG TERM #######
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(zoo)
library(gamair)
library(mgcv)
library(feather)
library(lubridate)
library(data.table)
library(fda)
library(forecast)
library(Interpol.T)

source("C://Users//utente//Documents//R_code//functions_for_corr_meteo_pun.R")
source("C://Users//utente//Documents//R_code//functions_for_POD_orari.R")
source("C://Users//utente//Documents//glm_dataset.R")


data <- read_excel("C:/Users/utente/Documents/misure/dati_2014-2016.xlsx")
colnames(data) <- c('date', 'pun')
data$date <- seq.POSIXt(as.POSIXct('2014-01-01'), as.POSIXct('2017-01-02'), by = 'hour')[1:nrow(data)]

hs <- maply(1:nrow(data), function(n) lubridate::hour(as.POSIXct(unlist(data[n,'date']), origin = '1970-01-01')))
wds <- maply(1:nrow(data), function(n) lubridate::wday(as.POSIXct(unlist(data[n,'date']), origin = '1970-01-01')))
wdys <- maply(1:nrow(data), function(n) lubridate::yday(as.POSIXct(unlist(data[n,'date']), origin = '1970-01-01')))
wks <- maply(1:nrow(data), function(n) lubridate::week(as.POSIXct(unlist(data[n,'date']), origin = '1970-01-01')))
hol <- maply(1:nrow(data), function(n) add_holidays_Date(as.Date(as.POSIXct(unlist(data[n,'date']), origin = '1970-01-01'))))

### 2016 is a leap year ---> remove February 29
feb29 <- which(as.Date(data$date) == as.Date("2016-02-29")) 
Data <- data[-feb29,]

### divide in different years:
pun14 <- Data$pun[which(lubridate::year(Data$date) == 2014)]
pun15 <- Data$pun[which(lubridate::year(Data$date) == 2015)]
pun16 <- Data$pun[which(lubridate::year(Data$date) == 2016)]

### get "general behaviour" of pun
pun_general <- (1/3)*(pun14 + pun15 + pun16)

plot(pun14, type = 'l', col = "blue")
lines(pun15, type = 'l', col = "orange")
lines(pun16, type = 'l', col = "red")
lines(pun_general, type = 'l', col = "black", lwd = 2)

#########################################################################################################################
#### meteo 
mi6 <- read_excel("C:/Users/utente/Documents/PUN/Milano 2016.xlsx", sheet= 1)
ro6 <- read_excel("C:/Users/utente/Documents/PUN/Roma 2016.xlsx", sheet= 1)
fi6 <- read_excel("C:/Users/utente/Documents/PUN/Firenze 2016.xlsx", sheet= 1)
pa6 <- read_excel("C:/Users/utente/Documents/PUN/Palermo 2016.xlsx", sheet= 1)
ca6 <- read_excel("C:/Users/utente/Documents/PUN/Cagliari 2016.xlsx", sheet= 1)
ba6 <- read_excel("C:/Users/utente/Documents/PUN/Bari 2016.xlsx", sheet= 1)

mi6 <- get_meteo2(mi6, "2016-12-31")
ro6 <- get_meteo2(ro6, "2016-12-31")
fi6 <- get_meteo2(fi6, "2016-12-31")
pa6 <- get_meteo2(pa6, "2016-12-31")
ca6 <- get_meteo2(ca6, "2016-12-31")
ba6 <- get_meteo2(ba6, "2016-12-31")

meteoav16 <- mediate_meteos2(mi6, ro6, fi6, pa6, ca6, ba6)
meteoav16[is.na(meteoav16)] <- 0

pairs(meteoav16)

Tmi <- TminTable(meteoav16)
Tma <- TmaxTable(meteoav16)
Tmh <- Th_interp(Tmi, Tma, day = 1:366, tab_calibr = )

data(Trentino_hourly_T)
load("Trentino_hourly_T.rda")
###########################################################################################################################

ctrl <- list(niterEM = 10, msVerbose = TRUE, optimMethod="L-BFGS-B")

fitp <-  gamm(data$pun ~ meteoav16$Tmedia + s(hs, bs = "cc", k = 24) + s(wds, bs = "cc", k = 7) + s(wdys, bs = "cc", k = 365) + ### needs intercept but the result is TOO smooth
                s(wks, bs = "cc") + hol, data = data, correlation = corARMA(form = ~ 1|wks, p = 2),control = ctrl)

plot(fitp$gam)

plot(data$pun, type = 'l', lwd = 2)
lines(fitp$gam$fitted.values, type = 'l', lwd = 2, col = "skyblue")

###################################################################################
library(h2o)

h2o.init(nthreads = -1, max_mem_size = '20g')
df <- data.frame(y = data$pun, hs = hs, wds = wds, wdys = wdys, wks = wks, hol = hol)

dl <- h2o.deeplearning(x = c("hs", "wds", "wdys", "wks", "hol"), y = "y", training_frame = as.h2o(df), standardize = TRUE, activation = "Rectifier", 
                       hidden = c(1000, 365, 52, 12, 7, 24), epochs = 100)

plot(dl)
summary(dl)
h2o.r2(dl)
yhat <- h2o.predict(dl, newdata = as.h2o(df[,2:6]))
yhat <- as.matrix(as.numeric(yhat$predict))
yhat <- unlist(yhat)

plot(df$y, type = "l")
lines(yhat, type = "l", col = "red")

#### simple DL model done in the following way: 
#### year ~ (year - 1) + hour + day + week + holiday + ...
make_DLdataset_pun_forward <- function(data)
{
  d_f <- data_frame()
  
  feb29 <- which(as.Date(data$date) == as.Date("2016-02-29")) 
  Data <- data[-feb29,]
  pun14 <- Data[which(lubridate::year(Data$date) == 2014),]
  pun15 <- Data[which(lubridate::year(Data$date) == 2015),]
  pun16 <- Data[which(lubridate::year(Data$date) == 2016),]
  
  for(i in 1:nrow(pun14))
  {
    hs <-  lubridate::hour(as.POSIXct(unlist(pun14[i,'date']), origin = '1970-01-01'))
    wds <- lubridate::wday(as.POSIXct(unlist(pun14[i,'date']), origin = '1970-01-01'))
    wdys <- lubridate::yday(as.POSIXct(unlist(pun14[i,'date']), origin = '1970-01-01'))
    wks <-  lubridate::week(as.POSIXct(unlist(pun14[i,'date']), origin = '1970-01-01'))
    hol <-  add_holidays_Date(as.Date(as.POSIXct(unlist(pun14[i,'date']), origin = '1970-01-01')))
    
    ths <-  lubridate::hour(as.POSIXct(unlist(pun15[i,'date']), origin = '1970-01-01'))
    twds <- lubridate::wday(as.POSIXct(unlist(pun15[i,'date']), origin = '1970-01-01'))
    twdys <- lubridate::yday(as.POSIXct(unlist(pun15[i,'date']), origin = '1970-01-01'))
    twks <-  lubridate::week(as.POSIXct(unlist(pun15[i,'date']), origin = '1970-01-01'))
    thol <-  add_holidays_Date(as.Date(as.POSIXct(unlist(pun15[i,'date']), origin = '1970-01-01')))
    
    df2 <- data.frame(pun14$pun[i], hs, wds, wdys, wks, hol, pun15$pun[i], ths, twds, twdys, twks, thol)
    colnames(df2) <- c("lpun","hour","weekday","day","week","holiday","ypun","thour","tweekday","tday","tweek","tholiday")
    d_f <- bind_rows(d_f, df2)
  }
  
  for(i in 1:nrow(pun15))
  {
    hs <-  lubridate::hour(as.POSIXct(unlist(pun15[i,'date']), origin = '1970-01-01'))
    wds <- lubridate::wday(as.POSIXct(unlist(pun15[i,'date']), origin = '1970-01-01'))
    wdys <- lubridate::yday(as.POSIXct(unlist(pun15[i,'date']), origin = '1970-01-01'))
    wks <-  lubridate::week(as.POSIXct(unlist(pun15[i,'date']), origin = '1970-01-01'))
    hol <-  add_holidays_Date(as.Date(as.POSIXct(unlist(pun15[i,'date']), origin = '1970-01-01')))
    
    ths <-  lubridate::hour(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    twds <- lubridate::wday(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    twdys <- lubridate::yday(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    twks <-  lubridate::week(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    thol <-  add_holidays_Date(as.Date(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01')))
    
    df2 <- data.frame(pun15$pun[i], hs, wds, wdys, wks, hol, pun16$pun[i], ths, twds, twdys, twks, thol)
    colnames(df2) <- c("lpun","hour","weekday","day","week","holiday","ypun","thour","tweekday","tday","tweek","tholiday")
    d_f <- bind_rows(d_f, df2)
  }
  colnames(d_f) <- c("lpun","hour","weekday","day","week","holiday","ypun","thour","tweekday","tday","tweek","tholiday")
  return(d_f)
}
##############################################################################################################################################
make_DLdataset_pun_forward2 <- function(data)
{
  ### @BRIEF: better function: should work automatically, without expliciting the dates
  d_f <- data_frame()
  
  # feb29 <- which(as.Date(data$date) == as.Date("2016-02-29")) 
  # Data <- data[-feb29,]
  # pun14 <- Data[which(lubridate::year(Data$date) == 2014),]
  # pun15 <- Data[which(lubridate::year(Data$date) == 2015),]
  # pun16 <- Data[which(lubridate::year(Data$date) == 2016),]
  
  for(i in 1:nrow(data))
  {
    print(i)
    y <- 0
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
      #colnames(df2) <- c("lpun","hour","weekday","day","week","holiday","ypun","thour","tweekday","tday","tweek","tholiday")
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
####################################################################################################################################
prediction_pun_forward <- function(df, start_date)
{
  d_f <- data_frame()
  
  feb29 <- which(as.Date(data$date) == as.Date("2016-02-29")) 
  Data <- data[-feb29,]
  pun14 <- Data[which(lubridate::year(Data$date) == 2014),]
  pun15 <- Data[which(lubridate::year(Data$date) == 2015),]
  pun16 <- Data[which(lubridate::year(Data$date) == 2016),]
  
  seq17 <- seq.POSIXt(as.POSIXct(start_date), as.POSIXct('2018-01-02'), by = 'hour')[1:8760]
  
  for(i in 1:nrow(pun16))
  {
    
    hs <-  lubridate::hour(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    wds <- lubridate::wday(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    wdys <- lubridate::yday(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    wks <-  lubridate::week(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    hol <-  add_holidays_Date(as.Date(unlist(pun16[i,'date'])))
    
    ths <-  lubridate::hour(as.POSIXct(unlist(seq17[i]), origin = '1970-01-01'))
    twds <- lubridate::wday(as.POSIXct(unlist(seq17[i]), origin = '1970-01-01'))
    twdys <- lubridate::yday(as.POSIXct(unlist(seq17[i]), origin = '1970-01-01'))
    twks <-  lubridate::week(as.POSIXct(unlist(seq17[i]), origin = '1970-01-01'))
    thol <-  add_holidays_Date(as.Date(as.POSIXct(unlist(seq17[i]), origin = '1970-01-01')))
    
    df2 <- data.frame(pun16$pun[i], hs, wds, wdys, wks, hol, ths, twds, twdys, twks, thol)
    colnames(df2) <- c("lpun","hour","weekday","day","week","holiday","thour","tweekday","tday","tweek","tholiday")
    d_f <- bind_rows(d_f, df2)
  }
  colnames(d_f) <- c("lpun","hour","weekday","day","week","holiday","thour","tweekday","tday","tweek","tholiday")
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
DLD <- make_DLdataset_pun_forward(data)

response <- "ypun"
regressors <- setdiff(colnames(DLD),response)

modeldl <- h2o.deeplearning(x = regressors, y = response, training_frame = as.h2o(DLD), standardize = TRUE, activation = "Rectifier", 
                       hidden = c(8760, 365, 52, 12, 7, 24), epochs = 100)

plot(modeldl)
summary(modeldl)
h2o.r2(modeldl)

newd <- prediction_pun_forward(data, "2017-01-01")

yhat <- h2o.predict(modeldl, newdata = as.h2o(newd))
yhat <- as.matrix(as.numeric(yhat$predict))
yhat <- unlist(yhat)

plot(yhat, type = "l")
lines(yhat, type = "l", col = "red")

######## automatic dataset
data2 <- read_excel("C:/Users/utente/Documents/misure/dati_2014-2017.xlsx")
colnames(data2) <- c('date', 'pun')
data2$date <- seq.POSIXt(as.POSIXct('2014-01-01'), as.POSIXct('2017-01-18'), by = 'hour')[1:nrow(data2)]

DLD2 <- make_DLdataset_pun_forward2(data2)

DLD2 <- as.data.frame(read_feather("C:\\Users\\utente\\Documents\\misure\\dati_punForward"))

response <- "ypun"
regressors <- setdiff(colnames(DLD2),response)

modeldl2 <- h2o.deeplearning(x = regressors, y = response, training_frame = as.h2o(DLD2),
                            standardize = TRUE, activation = "Rectifier", 
                            hidden = c(8760, 365, 52, 12, 7, 24), epochs = 100)

plot(modeldl2)
summary(modeldl2)
h2o.r2(modeldl2)

pred17 <- prediction_pun_forward2(data2, "2017-01-16")

yhat17 <- h2o.predict(modeldl2, newdata = as.h2o(pred17))
yhat17 <- unlist(as.matrix(as.numeric(yhat17$predict)))

plot(yhat17, type = 'l', col = 'orange')

### paste existing 2017 pun
library(xlsx)
write.xlsx(yhat17, "longterm_pun.xlsx")
####################################################################
###### fit the hourly process with a spline ########
HourlyProcess <- function(df, finaldate) ### finaldate = today + 2 days ahead if the pun file is updated
{
  d_f <- data_frame()  

  seqdates <- seq.Date(as.Date('2014-01-01'), as.Date(finaldate), by = 'day')
  
  for( i in 2:(length(seqdates)-1))
  {
    loc <- rep(0, 25)
    dft1 <- df[which(as.Date(df$date) == seqdates[i-1]),]
    dft2 <- df[which(as.Date(df$date) == seqdates[i]),]
    obday <- mode(lubridate::day(dft2$date))
    dft <- bind_rows(dft1[which(lubridate::day(dft1$date) == obday),], dft2[which(lubridate::day(dft2$date) == obday),])
    
    for(j in 1:nrow(dft))    
    {
      h <- lubridate::hour(dft$date[j])
      if(exists('h'))
      {
        loc[h+1] <- loc[h+1] + dft$pun[j]
        rm(h)
      }
    }
    df2 <- data.frame(seqdates[i], t(loc))
    d_f <- bind_rows(d_f, df2)
    
    
  }
  colnames(d_f) <- c('date', as.character(1:25))
  return(d_f)
}
es <- HourlyProcess(data, '2016-12-24')
colSums(es[,2:26])

matplot(t(es[,2:25]), type = 'l')

cov(es[,2:25])
cor(es[,2:25])

fitarima <- arima(data$pun, order = c(24,1,24), include.mean = TRUE)
summary(fitarima)

qqnorm(residuals(fitarima)) ; qqline(residuals(fitarima))
hist(residuals(fitarima))

fitarima$arma
fitarima$series
fitarima$model


#### functional regression experiment
yy <- data$pun

freg <- fRegress(yy ~ hs + wds + wdys + wks, hol)


data.frame(data[which(as.Date(data$date) == as.Date('2016-10-29')),])
gg <- data.frame(data[which(as.Date(data$date) == as.Date('2016-10-30')),])
