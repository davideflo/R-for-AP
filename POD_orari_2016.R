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

datan <- as.data.frame(read_feather("C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_nord"))

FilterByDate(datan, "2016-01-01")

matplot(t(FilterByDateAndValue(datan, "2016-01-01", 0.1, "less")), type = "l")
plot(colSums(FilterByDate(datan, "2016-01-01")[,3:26]), type = "l", lwd = 2)

dfn <- FilterByDate(datan, "2016-01-14")[,3:26]

chs <- CHStatistics(dfn, 20, 0.8)
plot(chs, type = 'l', lwd = 2, col = 'orange')
wk <- FKMSparseClustering(dfn, 1:24, 6, 0.8, 'kmea', maxiter = 100)
plot(wk$W, type = 'l', lwd = 2, col = 'gold')
plotGroupMeans(dfn, wk$CLUSTER)

GV <- GetGroupVariance(dfn, wk$CLUSTER)
matplot(sqrt(t(GV)), type = 'l', lwd = 2)


polyfit <- function(i) x <- AIC(lm(y~poly(x,i))) 
as.integer(optimize(polyfit,interval = c(1,length(x)-1))$minimum)

fit <- lm(y ~ poly(x, 7, raw=TRUE))
BestFittingPolynomial(y, x, 23)

plot(x, y, type = 'l', lwd = 2)
lines(x, fit$fitted.values, type = 'l', lwd = 2, col = 2)


argvals = seq(0,1,len=length(y))
nbasis = 20
basisobj = create.bspline.basis(c(0,1),nbasis)
Ys = smooth.basis(argvals=argvals, y=y, fdParobj=basisobj,returnMatrix=TRUE)
Dxhat = eval.fd(argvals, Ys$fd, Lfd=1)
plot(argvals, Dxhat,type="l", lwd = 2, col = "red")
lines(Ys)
lines(argvals, y,type="l", col = "blue")

der <- FitDerivatives(dfn)
chs <- CHStatistics(der, 20, 0.8)
plot(chs, type = 'l', lwd = 2, col = 'orange')
wk <- FKMSparseClustering(der, 1:24, 2, 0.8, 'kmea', maxiter = 100)
plot(wk$W, type = 'l', lwd = 2, col = 'gold')
plotGroupMeans(dfn, wk$CLUSTER)
GV <- GetGroupVariance(der, wk$CLUSTER)
matplot(sqrt(t(GV)), type = 'l', lwd = 2)
matplot(t(der), type = 'l', lwd = 2)

length(which(wk$CLUSTER == 1))
length(which(wk$CLUSTER == 2))
length(which(wk$CLUSTER == 3))
length(which(wk$CLUSTER == 4))
length(which(wk$CLUSTER == 5))
length(which(wk$CLUSTER == 6))

plot(colMeans(dfn[which(wk$CLUSTER == 5),]), type = "l") 
