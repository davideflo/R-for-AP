library(XML)
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(zoo)
library(gamair)
library(mgcv)
library(feather)
library(lubridate)

source("C://Users//utente//Documents//R_code//functions_for_corr_meteo_pun.R")
source("C://Users//utente//Documents//R_code//functions_for_POD_orari.R")

data <- read_excel("C:/Users/utente/Documents/misure/AggregPDO_orari_2016.xlsx")
zona <- read_excel("C:/Users/utente/Documents/misure/mappa-pod.xlsx")

pod_nord <- unlist(zona[which(zona['AREA'] == 'NORD'),'POD'])
pod_cnord <- unlist(zona[which(zona['AREA'] == 'CNOR'),'POD'])
pod_csud <- unlist(zona[which(zona['AREA'] == 'CSUD'),'POD'])
pod_sud <- unlist(zona[which(zona['AREA'] == 'SUD'),'POD'])
pod_sici <- unlist(zona[which(zona['AREA'] == 'SICI'),'POD'])
pod_sard <- unlist(zona[which(zona['AREA'] == 'SARD'),'POD'])

datan <- data[unlist(which(unlist(data['Pod']) %in% pod_nord)),]
write_feather(datan, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_nord")
datacn <- data[unlist(which(unlist(data['Pod']) %in% pod_cnord)),]
write_feather(datacn, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_cnord")
datacs <- data[unlist(which(unlist(data['Pod']) %in% pod_csud)),]
write_feather(datacs, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_csud")
datas <- data[unlist(which(unlist(data['Pod']) %in% pod_sud)),]
write_feather(datas, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_sud")
datasi <- data[unlist(which(unlist(data['Pod']) %in% pod_sici)),]
write_feather(datasi, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_sici")
datasa <- data[unlist(which(unlist(data['Pod']) %in% pod_sard)),]
write_feather(datasa, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_sard")

datan <- ConvertDate(datan)
AggN <- HourAggregator(datan)
write_feather(AggN, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_nord")

matplot(t(datan[which(as.Date(as.POSIXct(unlist(datan['Giorno']), origin = "1970-01-01")) == as.Date('2016-01-01')),4:99]), type = "l")
