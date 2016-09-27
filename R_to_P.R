#### R TO PYTHON
library(openxlsx)
library(plyr)
library(reshape)
library(stringi)
library(xlsx)
library(h2o)
library(dplyr)
library(ggplot2)
library(readxl)

source("C://Users//utente//Documents//R_code//functions_for_PPIA_server.R")

prices10 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2010.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices12 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2012.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices11 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2011.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices13 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2013.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices14 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2014.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices15 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2015.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices16 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2016_06.xlsx", sheet="Prezzi-Prices", colNames=TRUE)

meteonord <- read.csv2("C:/Users/utente/Documents/PUN/storico_milano_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteocsud <- read.csv2("C:/Users/utente/Documents/PUN/storico_roma.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteocnord <- read.csv2("C:/Users/utente/Documents/PUN/storico_firenze_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteosud <- read.csv2("C:/Users/utente/Documents/PUN/storico_reggiocalabria_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteosici <- read.csv2("C:/Users/utente/Documents/PUN/storico_palermo_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteosard <- read.csv2("C:/Users/utente/Documents/PUN/storico_cagliari_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)

mi6 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Milano 2016.xlsx", sheet= 1, colNames=TRUE)
ro6 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Roma 2016.xlsx", sheet= 1, colNames=TRUE)
fi6 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Firenze 2016.xlsx", sheet= 1, colNames=TRUE)
pa6 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Palermo 2016.xlsx", sheet= 1, colNames=TRUE)
ca6 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Cagliari 2016.xlsx", sheet= 1, colNames=TRUE)
rc6 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Reggio Calabria 2016.xlsx", sheet= 1, colNames=TRUE)

variables <- colnames(prices10)[c(1:12,14:21)]
prices <- rbind(prices10[c(1:12,14:21)], prices11[,which(colnames(prices11) %in% variables)], prices12[,which(colnames(prices12) %in% variables)], 
                prices13[,which(colnames(prices13) %in% variables)], prices14[,which(colnames(prices14) %in% variables)],
                prices15[,which(colnames(prices15) %in% variables)])

## average weather variables 

meteonord[,2:ncol(meteonord)] <- data.matrix(meteonord[,2:ncol(meteonord)])
meteocsud[,2:ncol(meteocsud)] <- data.matrix(meteocsud[,2:ncol(meteocsud)])
meteocnord[,2:ncol(meteocnord)] <- data.matrix(meteocnord[,2:ncol(meteocnord)])
meteosici[,2:ncol(meteosici)] <- data.matrix(meteosici[,2:ncol(meteosici)])
meteosard[,2:ncol(meteosard)] <- data.matrix(meteosard[,2:ncol(meteosard)])
meteosud[,2:ncol(meteosud)] <- data.matrix(meteosud[,2:ncol(meteosud)])

meteoav <- mediate_meteos(meteonord, meteocsud, meteocnord, meteosici, meteosard, meteosud, FALSE)
meteoav16 <- mediate_meteos(mi6, ro6, fi6, pa6, ca6, rc6, TRUE)

library(h2o)

generate_fixed_dataset(prices, prices16, meteoav, meteoav16)

#############################################
prices15 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2015.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices16 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2016_08.xlsx", sheet="Prezzi-Prices", colNames=TRUE)

meteonord <- read.csv2("C:/Users/utente/Documents/PUN/storico_milano_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteocsud <- read.csv2("C:/Users/utente/Documents/PUN/storico_roma.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteocnord <- read.csv2("C:/Users/utente/Documents/PUN/storico_firenze_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteosud <- read.csv2("C:/Users/utente/Documents/PUN/storico_reggiocalabria_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteosici <- read.csv2("C:/Users/utente/Documents/PUN/storico_palermo_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteosard <- read.csv2("C:/Users/utente/Documents/PUN/storico_cagliari_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)

## average weather variables 

meteonord[,2:ncol(meteonord)] <- data.matrix(meteonord[,2:ncol(meteonord)])
meteocsud[,2:ncol(meteocsud)] <- data.matrix(meteocsud[,2:ncol(meteocsud)])
meteocnord[,2:ncol(meteocnord)] <- data.matrix(meteocnord[,2:ncol(meteocnord)])
meteosici[,2:ncol(meteosici)] <- data.matrix(meteosici[,2:ncol(meteosici)])
meteosard[,2:ncol(meteosard)] <- data.matrix(meteosard[,2:ncol(meteosard)])
meteosud[,2:ncol(meteosud)] <- data.matrix(meteosud[,2:ncol(meteosud)])

meteoav <- mediate_meteos(meteonord, meteocsud, meteocnord, meteosici, meteosard, meteosud, FALSE)

mi6 <- read_excel("C:/Users/utente/Documents/PUN/Milano 2016.xlsx", sheet= 1)
ro6 <- read_excel("C:/Users/utente/Documents/PUN/Roma 2016.xlsx", sheet= 1)
fi6 <- read_excel("C:/Users/utente/Documents/PUN/Firenze 2016.xlsx", sheet= 1)
pa6 <- read_excel("C:/Users/utente/Documents/PUN/Palermo 2016.xlsx", sheet= 1)
ca6 <- read_excel("C:/Users/utente/Documents/PUN/Cagliari 2016.xlsx", sheet= 1)
rc6 <- read_excel("C:/Users/utente/Documents/PUN/Reggio Calabria 2016.xlsx", sheet= 1)

mi6 <- get_meteo(mi6)
ro6 <- get_meteo(ro6)
fi6 <- get_meteo(fi6)
pa6 <- get_meteo(pa6)
ca6 <- get_meteo(ca6)
rc6 <- get_meteo(rc6)

meteoav16 <- mediate_meteos(mi6, ro6, fi6, pa6, ca6, rc6, FALSE)

start <- Sys.time()
test <- create_fixed_dataset_average(prices16, "ven","PUN",meteoav16,1)
Sys.time() - start

generate_fixed_dataset_average(prices15, prices16, meteoav, meteoav16)

