library(XML)
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(zoo)
library(gamair)
library(mgcv)

source("C://Users//utente//Documents//R_code//functions_for_corr_meteo_pun.R")

data <- read_excel("C:/Users/utente/Documents/misure/AggregPDO_orari_2016.xlsx")
zona <- read_excel("C:/Users/utente/Documents/misure/mappa-pod.xlsx")

pod_nord <- unlist(zona[which(zona['AREA'] == 'NORD'),'POD'])
datan <- data[unlist(which(unlist(data['Pod']) %in% pod_nord)),]

matplot(t(datan[which(unlist(datan['Mese']) == 1),4:99]), type = "l")
