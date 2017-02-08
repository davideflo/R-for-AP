library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(gplots)
library(zoo)
library(gamair)
library(mgcv)
library(feather)
library(lubridate)
library(fda)
library(data.table)
library(fda.usc)
library(dlm)
library(KFAS)

source("C://Users//utente//Documents//R_code//functions_for_corr_meteo_pun.R")
source("C://Users//utente//Documents//R_code//functions_for_POD_orari.R")
source("C://Users//utente//Documents//glm_dataset.R")
source("R_code/similar_days_model.R")
source("R_code/GAMM_model.R")
source("R_code/DLF_model.R")


databd <- read_excel("C:/Users/utente/Documents/misure/DB_2016.xlsm", sheet = "DB_SI_perd")
data.table(databd)

datan <- databd[unlist(which(unlist(databd['Area']) ==  "NORD")),]
write_feather(datan, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_nord")
datacn <- databd[unlist(which(unlist(databd['Area']) ==  "CNOR")),]
write_feather(datacn, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_cnord")
datacs <- databd[unlist(which(unlist(databd['Area']) ==  "CSUD")),]
write_feather(datacs, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_csud")
datas <- databd[unlist(which(unlist(databd['Area']) ==  "SUD")),]
write_feather(datas, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_sud")
datasi <- databd[unlist(which(unlist(databd['Area']) ==  "SICI")),]
write_feather(datasi, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_sici")
datasa <- databd[unlist(which(unlist(databd['Area']) ==  "SARD")),]
write_feather(datasa, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_sard")


AggN <- HourlyAggregator2(datan)
write_feather(AggN, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_nord")
AggCN <- HourlyAggregator2(datacn)
write_feather(AggCN, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_cnord")
AggCS <- HourlyAggregator2(datacs)
write_feather(AggCS, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_csud")
AggS <- HourlyAggregator2(datas)
write_feather(AggS, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_sud")
AggSI <- HourlyAggregator2(datasi)
write_feather(AggSI, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_sici")
AggSA <- HourlyAggregator2(datasa)
write_feather(AggSA, "C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_sard")

