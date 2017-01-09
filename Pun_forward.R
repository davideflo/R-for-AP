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
