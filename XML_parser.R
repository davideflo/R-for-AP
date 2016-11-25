library(XML)
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(zoo)
library(gamair)
library(mgcv)

source("C://Users//utente//Documents//R_code//functions_for_corr_meteo_pun.R")


data <- xmlParse("C:/Users/utente/Documents/misure/12883450152_03728900964_201609_PDO_20161004072732_1DP1608.xml")

xml_data <- xmlToList(data)
xml_df <- xmlToDataFrame(xml_data)

length(xml_data$DatiPod$Curva)
xml_data$DatiPod$Curva$Ea

pod <- xml_data$DatiPod$Pod

df <- data_frame()

for( i in 10:30)
{
  txt <- as.character(i)
  xml_data$DatiPod$Curva$Ea$text == txt
}

data <- read_excel("C:/Users/utente/Documents/misure/a davide.xlsx")


pod <- unique(unlist(data$POD))
color <- c()
for( i in 1:nrow(data))
{
  for(j in 1:length(pod))
  {
    if(data[i,"POD"] == pod[j])
    {
      color <- c(color, j)
    }
  }
}

matplot(t(data[,6:29]), type = "l", lwd = 2, col = color)

data300 <- data[which(apply(data[,6:29],1,max) <= 350),6:29]

matplot(t(data300), type = "l", lwd = 2, col = color[which(apply(data[,6:29],1,max) <= 350)])

for( p in pod)
{
  matplot(t(data[which(data["POD"] == p),6:29]), type="l", lwd = 2, main = p)
}

for( p in pod)
{
  wp <- which(data["POD"] == p)
  sp <- c()
  for(j in 1:length(wp))
  {
    sp <- c(sp, unlist(data[wp[j],6:29]))
  }
  plot(sp, type="l", lwd = 2, main = p)
}


#################################################
#### GAMM Models ################################


dm <- read_excel('misure/Cartel2.xlsx')

nord <- dm[which(dm['Area'] == 'NORD'),]

Date <- seq.Date(from=as.Date('2016-11-01'), to = as.Date('2016-11-20'), by='day')
DM <- c()

untime <- maply(1:nrow(dm), function(n) unlist(dm[n,1]))
utc <- as.POSIXct(untime, origin = "1970-01-01")

for(i in 1:length(Date))
{
  DM <- c(DM, apply(dm[which(as.Date(utc) == Date[i]),6:29], 2, sum))
}

plot(DM, type = 'l', lwd = 2)

day.of.month <- 1:(20*24)

fit <- gamm(DM ~ s(day.of.month, bs = "cc"))

summary(fit)
summary(fit$gam)
summary(fit$lme)

plot(DM, type = 'l', lwd = 2)
lines(fit$gam$fitted.values, lwd = 2, col = 'blue')

############ reduced trial ###########
fit2 <- gamm(DM[1:200] ~ s(day.of.month[1:200], bs = "cc"))

summary(fit2)
summary(fit2$gam)
summary(fit2$lme)

plot(DM[1:200], type = 'l', lwd = 2, col = 'green')
lines(fit2$gam$fitted.values, lwd = 2, col = 'red')


pp <- read_excel("C:\\Users\\utente\\Documents\\PUN\\DB_Borse_Elettriche_PER MI.xlsx", sheet = "DB_Dati")

pp_last <- get_last_month(pp, Sys.Date())
seasonDB <- find_weekly_seasonality_DB(pp_last)
plot(seasonDB, type="l", col="red")
points(seasonDB,pch=16,col="black")

sdDB <- find_daily_sd_DB(pp_last)





