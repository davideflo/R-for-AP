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
library(fdakma)
library(data.table)

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
wk <- FKMSparseClustering(der, 1:24, 3, 0.8, 'kmea', maxiter = 100)
plot(wk$W, type = 'l', lwd = 2, col = 'gold')
plotGroupMeans(dfn, wk$CLUSTER)
GV <- GetGroupVariance(der, wk$CLUSTER)
matplot(sqrt(t(GV)), type = 'l', lwd = 2)
matplot(t(der), type = 'l', lwd = 2)

par(mfrow = c(2,1))
plot(apply(dfn[which(wk$CLUSTER == 2),], 2, sd), type = 'l', lwd = 2, col = 'navy')
plot(apply(dfn[which(wk$CLUSTER == 1),], 2, sd), type = 'l', lwd = 2, col = 'red')
layout(1)

kma_ex <- kma(x=1:24, y0=scale(dfn, center = TRUE, scale = TRUE), y1=scale(FitDerivatives(dfn, FALSE),center = TRUE, scale = TRUE), 
              n.clust = 2,warping.method = 'affine',similarity.method = 'd1.pearson',center.method = 'k-means')
kma.show.results(kma_ex)

length(which(wk$CLUSTER == 1))
length(which(wk$CLUSTER == 2))
length(which(wk$CLUSTER == 3))
length(which(wk$CLUSTER == 4))
length(which(wk$CLUSTER == 5))
length(which(wk$CLUSTER == 6))

plot(colMeans(dfn[which(wk$CLUSTER == 5),]), type = "l") 

TreatData("cnord")
TreatData("csud")
TreatData("sud")
TreatData("sici")
TreatData("sard")


datasa <- as.data.frame(read_feather("C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_sard"))

dim(datasa)
es <- FilterByDate(datasa, "2016-01-14")[,3:26]
layout(1)
matplot(t(es), type = 'l', lwd = 2)


tssard <- TSAggregator(datasa)

plot(tssard, type = 'l', lwd = 2, col = 'blue')


DT <- data.table(sard = tssard, hour = 1:length(tssard))

sardsd <- GetSD(tssard, '2016-01-01', '2016-11-01')
plot(sardsd, type = 'l', lwd = 2)
lag.plot(sardsd[,2])
lag.plot(tssard)
acf(tssard)
acf(sardsd[,2])

msd <- c()
for(m in 1:10)
{
  print(m)
  atm <- as.data.frame(sardsd[which(lubridate::month(as.POSIXct(unlist(sardsd[,1]), origin = '1970-01-01')) == m),])
  msd <- c(msd, sd(atm[,2]))
}
plot(msd, type = 'l')


plot(GetHourlySD(datasa), type = 'l', lwd = 2, col = 'red')


######### does the load correlate with meteo? 

source("C://Users//utente//Documents//glm_dataset.R")

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

### DT2 da MLReg.R ###

DT8 <- DT2[which(lubridate::month(as.Date(unlist(DT2[,1]))) <= 8),]
data.table(DT8)

ca6[98, 'Tmedia'] <- 17
ca6[99, 'Tmedia'] <- 15
ca6[98, 'Vento_media'] <- 14
ca6[99, 'Vento_media'] <- 18


corrs <- rep(0, 24)
for(i in 1:24)
{
  print(paste("correlation between ", i,"th hour and mean Temp:", cor(unlist(DT8[as.character(i)]), ca6$Tmedia, na.rm = TRUE)))
  corrs[i] <- cor(unlist(DT8[as.character(i)]), ca6$Tmedia, na.rm = TRUE)
}

barplot(corrs)

corrv <- rep(0, 24)
for(i in 1:24)
{
  print(paste("correlation between ", i,"th hour and wind:", cor(unlist(DT8[as.character(i)]), ca6$Vento_media, na.rm = TRUE)))
  corrv[i] <- cor(unlist(DT8[as.character(i)]), ca6$Vento_media, na.rm = TRUE)
}

barplot(corrv, col = 'skyblue2')

sbil <- read_excel("C:/Users/utente/Documents/misure/aggregato_sbilanciamento.xlsx")

### same on CNORD

### DT2 da MLReg.R ###

DT8 <- DT2[which(lubridate::month(as.Date(unlist(DT2[,1]))) <= 8),]
data.table(DT8)

fi6[101,'Tmedia'] <- 13 
fi6[93,'Tmedia'] <- 16
  
  
corrs <- rep(0, 24)
for(i in 1:24)
{
  print(paste("correlation between ", i,"th hour and mean Temp:", cor(unlist(DT8[as.character(i)]), fi6$Tmedia, na.rm = TRUE)))
  corrs[i] <- cor(unlist(DT8[as.character(i)]), fi6$Tmedia, na.rm = TRUE)
}

barplot(corrs, col = 'skyblue3')

sbilcn <- sbil[which(sbil$`CODICE RUC` == 'UC_DP1608_CNOR'),]
sbilcn$`DATA RIFERIMENTO CORRISPETTIVO` <- seq.POSIXt(as.POSIXct('2015-01-01'), as.POSIXct('2016-10-01'), by = 'hour')[1:15335]
sbilcn16 <- sbilcn[which(lubridate::year(as.Date(unlist(sbilcn$`DATA RIFERIMENTO CORRISPETTIVO`))) == 2016),]
sbilcn168 <- sbilcn16[which(lubridate::month(as.Date(unlist(sbilcn16$`DATA RIFERIMENTO CORRISPETTIVO`))) <= 8),]
sbilcn168 <- sbilcn168[1:(nrow(sbilcn168)-1),]
data.table(sbilcn168)


corrsb <- rep(0, 24)
for(i in 1:24)
{
  print(i)
  if(i == 2) next
  else if(i == 24)
  {
    corrsb[i] <- cor(unlist(sbilcn168[which(lubridate::hour(unlist(sbilcn168$`DATA RIFERIMENTO CORRISPETTIVO`)) == 0),'SBILANCIAMENTO FISICO [MWh]']), fi6$Tmedia, na.rm = TRUE)
  }
  else
  {
    print(paste("correlation between ", i,"th hour and mean Temp:", cor(unlist(sbilcn168[which(lubridate::hour(unlist(sbilcn168$`DATA RIFERIMENTO CORRISPETTIVO`)) == i),'SBILANCIAMENTO FISICO [MWh]']), fi6$Tmedia, na.rm = TRUE)))
    corrsb[i] <- cor(unlist(sbilcn168[which(lubridate::hour(unlist(sbilcn168$`DATA RIFERIMENTO CORRISPETTIVO`)) == i),'SBILANCIAMENTO FISICO [MWh]']), fi6$Tmedia, na.rm = TRUE)
  }
}

barplot(corrsb, col = 'salmon2')
