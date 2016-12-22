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

source("C://Users//utente//Documents//R_code//functions_for_corr_meteo_pun.R")
source("C://Users//utente//Documents//R_code//functions_for_POD_orari.R")
source("C://Users//utente//Documents//glm_dataset.R")


data <- read_excel("C:/Users/utente/Documents/misure/dati_2014-2016.xlsx")
colnames(data) <- c('date', 'pun')
data$date <- seq.POSIXt(as.POSIXct('2014-01-01'), as.POSIXct('2016-12-23'), by = 'hour')[1:nrow(data)]



hs <- maply(1:nrow(data), function(n) lubridate::hour(as.POSIXct(unlist(data[n,'date']), origin = '1970-01-01')))
wds <- maply(1:nrow(data), function(n) lubridate::wday(as.POSIXct(unlist(data[n,'date']), origin = '1970-01-01')))
wdys <- maply(1:nrow(data), function(n) lubridate::yday(as.POSIXct(unlist(data[n,'date']), origin = '1970-01-01')))
wks <- maply(1:nrow(data), function(n) lubridate::week(as.POSIXct(unlist(data[n,'date']), origin = '1970-01-01')))
hol <- maply(1:nrow(data), function(n) add_holidays_Date(as.Date(as.POSIXct(unlist(data[n,'date']), origin = '1970-01-01'))))

ctrl <- list(niterEM = 10, msVerbose = TRUE, optimMethod="L-BFGS-B")

fitp <-  gamm(data$pun ~ 0 + s(hs, bs = "cc", k = 24) + s(wds, bs = "cc", k = 7) + s(wdys, bs = "cc", k = 365) + 
                s(wks, bs = "cc") + hol, data = data, correlation = corARMA(form = ~ 1|wks, p = 2),control = ctrl)
