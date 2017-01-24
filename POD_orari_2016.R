library(XML)
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

meteoav16 <- data.frame(mediate_meteos(mi6, ro6, fi6, pa6, ca6, rc6, FALSE))

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

#################################################################################
sbilcn <- sbil[which(sbil$`CODICE RUC` == 'UC_DP1608_CNOR'),]

plot(sbilcn$`FABBISOGNO REALE`, type = 'l')
#################################################################################


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

#####################################################################
### make dataset
meteoav16$Data <- seq.Date(as.Date('2016-01-01'), as.Date('2016-08-31'), by = 'day')

dataset <- MakeDatasetMLR(DT8, meteoav16, 6)
data.table(dataset)

test <- sample.int(240, 50)
testset <- dataset[test,]
trainset <- dataset[setdiff(1:240,test), ]

fit <- lm(y ~ 0 + ., data = trainset)
summary(fit)

plot(fit)

plot(trainset$y, type = 'l', lwd = 2)
lines(fit$fitted.values, type = 'l', lwd = 2, col = 'pink')

max(fit$residuals)

ynh <- predict(fit, testset[,1:27])
testy <- testset$y

plot(testy, type = 'l', lwd = 2)
lines(ynh, type = 'l', lwd = 2, col = 'purple')

mape(trainset$y, fit$fitted.values)
mape(testy, ynh)
vectorMape(trainset$y, fit$fitted.values)
max(vectorMape(trainset$y, fit$fitted.values))
vectorMape(testy, ynh)
max(vectorMape(testy, ynh))

###################################################

cnord <- read_excel("C:/Users/utente/Documents/misure/cnord.xlsx")

colnames(cnord)[1] <- "date"
cnord$date <- seq.Date(as.Date('2015-01-01'), as.Date('2016-10-31'), by = 'day')

cnord8 <- cnord[which(unlist(cnord$date) <= as.Date("2016-08-31")),]

meteocnord <- read.csv2("C:/Users/utente/Documents/PUN/storico_firenze_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
fi6 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Firenze 2016.xlsx", sheet= 1, colNames=TRUE)
fi6 <- get_meteo(fi6)

meteocnord[,2:ncol(meteocnord)] <- data.matrix(meteocnord[,2:ncol(meteocnord)])

meteocnord$Data <- seq.Date(as.Date('2010-01-01'), as.Date('2015-12-31'), by = 'day')
fi6$Data <- seq.Date(as.Date('2016-01-01'), as.Date('2016-08-31'), by = 'day')

meteoU <- rbind(meteocnord, fi6)

dtcn <- MakeDatasetMLR_2(cnord8, meteoU,6)
data.table(dtcn)
data.frame(dtcn)

trainset <- dtcn[1:484,]
testset <- dtcn[485:605,]

fitn <- lm(y ~ 0 + ., data = trainset)
summary(fitn)

plot(fitn)

mean(fitn$residuals)
sd(fitn$residuals)
max(fitn$residuals)
median(fitn$residuals)

plot(trainset$y, type = 'l', lwd = 2)
lines(fitn$fitted.values, type = 'l', lwd = 2, col = 'skyblue3')

hist(fitn$residuals, col = 'blue2')
skewness(fitn$residuals)
kurtosis(fitn$residuals)

yhats <- predict(fitn, testset[,1:29])

reshat <- testset$y - yhats

mean(reshat)
sd(reshat)
median(reshat)
max(reshat)
skewness(reshat)
kurtosis(reshat)

plot(testset$y, type = 'l', lwd = 2)
lines(yhats, type = 'l', lwd = 2, col = 'pink2')

mape(trainset$y, fitn$fitted.values)
vectorMape(trainset$y, fitn$fitted.values)
max(vectorMape(trainset$y, fitn$fitted.values))
mape(testset$y, yhats)
vectorMape(testset$y, yhats)
max(vectorMape(testset$y, yhats))


qqnorm(reshat); qqline(reshat, col = 2)
#qqplot(y)

ggplot(data = data.table(Fitted_values = yhats, Residuals = reshat),
       aes(Fitted_values, Residuals)) +
  geom_point(size = 1.5) +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red", size = 0.8) +
  labs(title = "Fitted vs Residuals")

fitr <- lm(reshat ~ I(yhats) + I(yhats^2) + I(yhats^3))
summary(fitr)
cor(reshat, yhats)

rcauchy(length(reshat))
ks.test(rcauchy(length(reshat)), reshat, alternative = "two.sided")
#ks.test(rt(length(yhats), df = 2), yhats, alternative = "two.sided")

hist(reshat, breaks = 20, col = 'grey')

plot(density(reshat), type = 'l', col = "green", lwd = 2)
points(reshat, rep(0,length(reshat)), col = "blue3", pch = 16)

shapiro.test(reshat)

#### only days and temp
fitn2 <- lm(y ~ 0 + target_day + target_week + target_T + change_date + holiday + target_day:target_week, data = trainset)
summary(fitn2)

plot(fitn2)

plot(trainset$y, type = 'l', lwd = 2)
lines(fitn2$fitted.values, type = 'l', lwd = 2, col = 'skyblue3')

mean(fitn2$residuals)
sd(fitn2$residuals)
max(fitn2$residuals)
median(fitn2$residuals)

mape(trainset$y, fitn2$fitted.values)
vectorMape(trainset$y, fitn2$fitted.values)
max(vectorMape(trainset$y, fitn2$fitted.values))

ggplot(data = data.table(Fitted_values = fitn2$fitted.values, Residuals = fitn2$residuals),
       aes(Fitted_values, Residuals)) +
  geom_point(size = 1.5) +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red", size = 0.8) +
  labs(title = "Fitted vs Residuals")


##### influsso dei non orari 

aggcn <- AggregateMLR(DT)
aggcn <- aggcn[which(aggcn$date <= as.Date('2016-08-31')),]
dtcn2 <- cnord8[which(cnord$date >= as.Date('2016-01-01') & cnord$date <= as.Date('2016-08-31')),]

data.table(aggcn)
data.table(dtcn2)

#dtcn2[1,2:25] - aggcn[1,2:25]

nhi = NHI(dtcn2, aggcn) 

data.table(nhi)
matplot(t(nhi), type="l")
matplot(t(aggcn), type="l")
matplot(t(dtcn2[,2:25]), type="l")

colMeans(nhi[,2:25]/(nhi[,2:25] + aggcn[,2:25]))
colMeans(aggcn[,2:25]/(nhi[,2:25] + aggcn[,2:25]))


wds <- maply(1:nrow(nhi), function(n) lubridate::wday(as.Date(unlist(nhi[n,1]))))
wks <- maply(1:nrow(nhi), function(n) lubridate::week(as.Date(unlist(nhi[n,1]))))

DTNH <- bind_cols(nhi, data.frame(wds), data.frame(wks))

head(data.table(DTNH))

fitgam <- gamm(DTNH$`20` ~ s(wds, bs = "cc", k = 7) + s(wks, bs = "cc", k = 35), data = DTNH)

plot(fitgam$gam,scale = 0)
summary(fitgam$gam)

### with temperature
DTNHT <- bind_cols(nhi, data.frame(wds), data.frame(wks), data.frame(fi6$Tmedia))

fitgamt <- gamm(DTNH$`20` ~ s(wds, bs = "cc", k = 7) + s(wks, bs = "cc", k = 35) + s(fi6$Tmedia, bs = "cc"), data = DTNHT)

plot(fitgamt$gam,scale = 0)
summary(fitgamt$gam)

plot(DTNH$`20`, type = "l", lwd = 2, col = 'blue3')
lines(fitgamt$gam$fitted.values, type = "l", lwd = 2, col = 'red')

resgam <- fitgamt$gam$residuals

mean(resgam)
median(resgam)
sd(resgam)
kurtosis(resgam)
skewness(resgam)
max(resgam)

qqnorm(resgam); qqline(resgam, col = 2)
qqnorm(rcauchy(length(resgam))); qqline(rcauchy(length(resgam)), col = 2)


ggplot(data = data.table(Fitted_values = fitgamt$gam$fitted.values, Residuals = resgam),
       aes(Fitted_values, Residuals)) +
  geom_point(size = 1.5) +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red", size = 0.8) +
  labs(title = "Fitted vs Residuals")

shapiro.test(resgam)

#########
nord <- read_excel("C:/Users/utente/Documents/misure/nord.xlsx")

colnames(nord)[1] <- "date"
nord$date <- seq.Date(as.Date('2015-01-01'), as.Date('2016-10-31'), by = 'day')

nord8 <- nord[which(unlist(nord$date) <= as.Date("2016-08-31")),]

meteonord <- read.csv2("C:/Users/utente/Documents/PUN/storico_milano_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
mi6 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Milano 2016.xlsx", sheet= 1, colNames=TRUE)
mi6 <- get_meteo(mi6)

meteonord[,2:ncol(meteonord)] <- data.matrix(meteonord[,2:ncol(meteonord)])

meteonord$Data <- seq.Date(as.Date('2010-01-01'), as.Date('2015-12-31'), by = 'day')
mi6$Data <- seq.Date(as.Date('2016-01-01'), as.Date('2016-08-31'), by = 'day')

meteoU <- rbind(meteonord, mi6)

dtcn <- MakeDatasetMLR_2(nord8, meteoU,20)
data.table(dtcn)
data.frame(dtcn)

trainset <- dtcn[1:484,]
testset <- dtcn[485:605,]

fitnord <- lm(y ~ 0 + ., data = trainset)
summary(fitnord)

plot(fitnord)

mean(fitnord$residuals)
sd(fitnord$residuals)
max(fitnord$residuals)
median(fitnord$residuals)

plot(trainset$y, type = 'l', lwd = 2)
lines(fitnord$fitted.values, type = 'l', lwd = 2, col = 'skyblue3')

hist(fitnord$residuals, col = 'blue2')
skewness(fitnord$residuals)
kurtosis(fitnord$residuals)

yhats <- predict(fitnord, testset[,1:29])

reshat <- testset$y - yhats

mean(reshat)
sd(reshat)
median(reshat)
max(reshat)
skewness(reshat)
kurtosis(reshat)

plot(testset$y, type = 'l', lwd = 2)
lines(yhats, type = 'l', lwd = 2, col = 'pink2')

mape(trainset$y, fitnord$fitted.values)
vectorMape(trainset$y, fitnord$fitted.values)
max(vectorMape(trainset$y, fitnord$fitted.values))
mape(testset$y, yhats)
vectorMape(testset$y, yhats)
max(vectorMape(testset$y, yhats))


qqnorm(reshat); qqline(reshat, col = 2)


ggplot(data = data.table(Fitted_values = yhats, Residuals = reshat),
       aes(Fitted_values, Residuals)) +
  geom_point(size = 1.5) +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red", size = 0.8) +
  labs(title = "Fitted vs Residuals")
#### ### ### ### ###
fitnord2 <- lm(y ~ 0 + . + target_day:target_week, data = trainset)
summary(fitnord2)

plot(fitnord2)

mean(fitnord2$residuals)
sd(fitnord2$residuals)
max(fitnord2$residuals)
median(fitnord2$residuals)

plot(trainset$y, type = 'l', lwd = 2)
lines(fitnord2$fitted.values, type = 'l', lwd = 2, col = 'skyblue3')

hist(fitnord2$residuals, col = 'blue2')
skewness(fitnord2$residuals)
kurtosis(fitnord2$residuals)

yhats <- predict(fitnord2, testset[,1:29])

reshat <- testset$y - yhats

mean(reshat)
sd(reshat)
median(reshat)
max(reshat)
skewness(reshat)
kurtosis(reshat)

plot(testset$y, type = 'l', lwd = 2)
lines(yhats, type = 'l', lwd = 2, col = 'pink2')

mape(trainset$y, fitnord$fitted.values)
vectorMape(trainset$y, fitnord$fitted.values)
max(vectorMape(trainset$y, fitnord$fitted.values))
mape(testset$y, yhats)
vectorMape(testset$y, yhats)
max(vectorMape(testset$y, yhats))


qqnorm(reshat); qqline(reshat, col = 2)


ggplot(data = data.table(Fitted_values = yhats, Residuals = reshat),
       aes(Fitted_values, Residuals)) +
  geom_point(size = 1.5) +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red", size = 0.8) +
  labs(title = "Fitted vs Residuals")


DT[, date_time := ymd(DT[["Giorno"]])]
DT[, date := as.Date(DT[["date"]], "%Y-%m-%d")]
DT[, ':='(timestamp = NULL, estimated = NULL, anomaly = NULL)]
str(DT)



nord16 <- nord[which(unlist(nord['date']) >= as.Date('2016-01-01') & unlist(nord['date']) <= as.Date('2016-08-31')),1:25]

wds <- maply(1:nrow(nord16), function(n) lubridate::wday(as.Date(unlist(nord16[n,1]))))
wks <- maply(1:nrow(nord16), function(n) lubridate::week(as.Date(unlist(nord16[n,1]))))


DT <- bind_cols(nord16, data.frame(wds), data.frame(wks), data.frame(mi6$Tmedia))
fitgamn <- gamm(DT$`20` ~ s(wds, bs = "cc", k = 7) + s(wks, bs = "cc", k = 35) + s(mi6$Tmedia, bs = "cc"), data = DT)

plot(fitgamn$gam,scale = 0)
summary(fitgamn$gam)

plot(DT$`20`, type = "l", lwd = 2, col = 'blue3')
lines(fitgamn$gam$fitted.values, type = "l", lwd = 2, col = 'red')

acf(fitgamn$gam$residuals)
pacf(fitgamn$gam$residuals)

ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")
m1 <- gamm(DT$`20` ~ s(wds, bs = "cc", k = 7) + s(wks, bs = "cc", k = 35) + s(mi6$Tmedia, bs = "cc"), data = DT, correlation = corARMA(form = ~ 1|wks, p = 1),control = ctrl)

summary(m1)

plot(DT$`20`, type = "l", lwd = 2, col = 'blue3')
lines(m1$gam$fitted.values, type = "l", lwd = 2, col = 'red')

m2 <- gamm(DT$`20` ~ s(wds, bs = "cc", k = 7) + s(wks, bs = "cc", k = 35) + s(mi6$Tmedia, bs = "cc"), data = DT, correlation = corARMA(form = ~ 1|wks, p = 2),control = ctrl)
m3 <- gamm(DT$`20` ~ s(wds, bs = "cc", k = 7) + s(wks, bs = "cc", k = 35) + s(mi6$Tmedia, bs = "cc"), data = DT, correlation = corARMA(form = ~ 1|wks, p = 3),control = ctrl)

anova(fitgamn$lme, m1$lme, m2$lme, m3$lme) ### wks gives better models than wds. but only slightly better

plot(m1$gam, scale = 0)

qqnorm(m1$gam$residuals); qqline(m1$gam$residuals, col = 2)
hist(m1$gam$residuals, breaks = 20)

ggplot(data = data.table(Fitted_values = m1$gam$fitted.values, Residuals = m1$gam$residuals),
       aes(Fitted_values, Residuals)) +
  geom_point(size = 1.5) +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red", size = 0.8) +
  labs(title = "Fitted vs Residuals")


m1 <- gamm(DT$`20` ~ s(wds, bs = "cc", k = 7) + s(wks, bs = "cc", k = 35) + s(wks:wds, bs = "cc") + s(mi6$Tmedia, bs = "cc"), data = DT, correlation = corARMA(form = ~ 1|wks, p = 1),control = ctrl)

#######################################################################
######### HOURLY NORD LOAD CURVES #########################

dtn <- AggregateMLR(datan)
data.table(datan)

dtn8 <- dtn[which(unlist(dtn$date) <= as.Date("2016-08-31")),]

dtn8 <- cbind(dtn8, CD = rep(0,nrow(dtn8)))

meteonord <- read.csv2("C:/Users/utente/Documents/PUN/storico_milano_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
mi6 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Milano 2016.xlsx", sheet= 1, colNames=TRUE)
mi6 <- get_meteo(mi6)

meteonord[,2:ncol(meteonord)] <- data.matrix(meteonord[,2:ncol(meteonord)])

meteonord$Data <- seq.Date(as.Date('2010-01-01'), as.Date('2015-12-31'), by = 'day')
mi6$Data <- seq.Date(as.Date('2016-01-01'), as.Date('2016-08-31'), by = 'day')

meteoU <- rbind(meteonord, mi6)

DTN <- MakeDatasetMLR_2(dtn8, meteoU,20, '2016-01-01')
data.table(DTN)

fit1 <- lm(y ~ 0 + . , data = DTN)
summary(fit1)

plot(fit1)

plot(DTN$y, type = 'l', lwd = 2)
lines(fit1$fitted.values, type = 'l', lwd = 2, col = 'skyblue')

hist(fit1$residuals)


wds <- maply(1:nrow(DTN), function(n) lubridate::wday(unlist(DTN[n,1])))
wks <- maply(1:nrow(DTN), function(n) lubridate::week(unlist(DTN[n,1])))


fitgamn <- gamm(DTN$y ~ s(DTN$`H-24`, bs = "cc") + s(DTN$`H-23`, bs = "cc") + s(DTN$`H-22`, bs = "cc") + s(DTN$`H-21`, bs = "cc") + s(DTN$`H-20`, bs = "cc") +
                  s(DTN$`H-19`, bs = "cc") +s(DTN$`H-18`, bs = "cc") +s(DTN$`H-17`, bs = "cc") +s(DTN$`H-16`, bs = "cc") +s(DTN$`H-15`, bs = "cc") +
                  s(DTN$`H-14`, bs = "cc") +s(DTN$`H-13`, bs = "cc") +s(DTN$`H-12`, bs = "cc") +s(DTN$`H-11`, bs = "cc") +s(DTN$`H-10`, bs = "cc") +
                  s(DTN$`H-9`, bs = "cc") +s(DTN$`H-8`, bs = "cc") +s(DTN$`H-7`, bs = "cc") +s(DTN$`H-6`, bs = "cc") +s(DTN$`H-5`, bs = "cc") +
                  s(DTN$`H-4`, bs = "cc") +s(DTN$`H-3`, bs = "cc") +s(DTN$`H-2`, bs = "cc") +s(DTN$`H-1`, bs = "cc") +
                  s(target_day, bs = "cc", k = 7) + s(target_week, bs = "cc", k = 35) + s(target_T, bs = "cc") + holiday, data = DTN)

plot(fitgamn$gam,scale = 0)
summary(fitgamn$gam)

plot(DTN$y, type = "l", lwd = 2, col = 'blue3')
lines(fitgamn$gam$fitted.values, type = "l", lwd = 2, col = 'red')

ggplot(data = data.table(Fitted_values = fitgamn$gam$fitted.values, Residuals = fitgamn$gam$residuals),
       aes(Fitted_values, Residuals)) +
  geom_point(size = 1.5) +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red", size = 0.8) +
  labs(title = "Fitted vs Residuals")

cov(fitgamn$gam$fitted.values, fitgamn$gam$residuals)/(sd(fitgamn$gam$fitted.values)*sd(fitgamn$gam$residuals))

qqnorm(fitgamn$gam$residuals) ; qqline(fitgamn$gam$residuals)

mape(DTN$y, fitgamn$gam$fitted.values)
vectorMape(DTN$y, fitgamn$gam$fitted.values)
max(vectorMape(DTN$y, fitgamn$gam$fitted.values))

acf(fitgamn$gam$residuals, lag.max = 60)
pacf(fitgamn$gam$residuals)

ctrl <- list(niterEM = 10, msVerbose = TRUE, optimMethod="L-BFGS-B")
m2 <- gamm(DTN$y ~ s(DTN$`H-24`, bs = "cc") + s(DTN$`H-23`, bs = "cc") + s(DTN$`H-22`, bs = "cc") + s(DTN$`H-21`, bs = "cc") + s(DTN$`H-20`, bs = "cc") +
                  s(DTN$`H-19`, bs = "cc") +s(DTN$`H-18`, bs = "cc") +s(DTN$`H-17`, bs = "cc") +s(DTN$`H-16`, bs = "cc") +s(DTN$`H-15`, bs = "cc") +
                  s(DTN$`H-14`, bs = "cc") +s(DTN$`H-13`, bs = "cc") +s(DTN$`H-12`, bs = "cc") +s(DTN$`H-11`, bs = "cc") +s(DTN$`H-10`, bs = "cc") +
                  s(DTN$`H-9`, bs = "cc") +s(DTN$`H-8`, bs = "cc") +s(DTN$`H-7`, bs = "cc") +s(DTN$`H-6`, bs = "cc") +s(DTN$`H-5`, bs = "cc") +
                  s(DTN$`H-4`, bs = "cc") +s(DTN$`H-3`, bs = "cc") +s(DTN$`H-2`, bs = "cc") +s(DTN$`H-1`, bs = "cc") +
                  s(target_day, bs = "cc", k = 7) + s(target_week, bs = "cc", k = 35) + s(target_T, bs = "cc") + holiday, data = DTN,
                  correlation = corARMA(form = ~ 1|wks, p = 2),control = ctrl)

summary(m2$gam) 
plot(m2$gam, scale = 0)

plot(DTN$y, type = "l", lwd = 2, col = 'blue3')
lines(m2$gam$fitted.values, type = "l", lwd = 2, col = 'red')

mape(DTN$y, m2$gam$fitted.values)
vectorMape(DTN$y, m2$gam$fitted.values)
max(vectorMape(DTN$y, m2$gam$fitted.values))


m3 <- gamm(DTN$y ~ 0 +DTN$`H-24` + DTN$`H-23` + DTN$`H-22` + DTN$`H-21` + DTN$`H-20` +
             DTN$`H-19` + DTN$`H-18` + DTN$`H-17` + DTN$`H-16` + DTN$`H-15` +
             DTN$`H-14` + DTN$`H-13` + DTN$`H-12` + DTN$`H-11` + DTN$`H-10` +
             DTN$`H-9` + DTN$`H-8` + DTN$`H-7` + DTN$`H-6` + DTN$`H-5` +
             DTN$`H-4` + DTN$`H-3` + DTN$`H-2` + DTN$`H-1` +
             s(target_day, bs = "cc", k = 7) + s(target_week, bs = "cc", k = 35) + s(target_T, bs = "cc") + holiday, data = DTN,
           correlation = corARMA(form = ~ 1|wks, p = 2),control = ctrl)

summary(m3$gam) I
plot(m3$gam, scale = 0)

plot(DTN$y, type = "l", lwd = 2, col = 'blue3')
lines(m3$gam$fitted.values, type = "l", lwd = 2, col = 'red')

mape(DTN$y, m3$gam$fitted.values)
vectorMape(DTN$y, m3$gam$fitted.values)
max(vectorMape(DTN$y, m3$gam$fitted.values))


m4 <- lm(y ~ 0 + . + target_day:target_week, data = DTN)
summary(m4)

plot(DTN$y, type = "l", lwd = 2, col = 'blue3')
lines(m4$fitted.values, type = "l", lwd = 2, col = 'red')

anova(fitgamn$mle, m2$mle, m3$mle) ### wks gives better models than wds. but only slightly better


#########################################################################################################
#########################################################################################################
#################################### GAMM models other zones ############################################
#########################################################################################################
#########################################################################################################
datacn <- as.data.frame(read_feather("C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_cnord"))
#colnames(datacn)[2] <- 'date'

df <- AggregateMLR(datacn)

meteocnord <- read.csv2("C:/Users/utente/Documents/PUN/storico_firenze_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
fi6 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Firenze 2016.xlsx", sheet= 1, colNames=TRUE)
fi6 <- get_meteo(fi6)

meteocnord[,2:ncol(meteocnord)] <- data.matrix(meteocnord[,2:ncol(meteocnord)])

meteocnord$Data <- seq.Date(as.Date('2010-01-01'), as.Date('2015-12-31'), by = 'day')
fi6$Data <- seq.Date(as.Date('2016-01-01'), as.Date('2016-08-31'), by = 'day')

meteoU <- rbind(meteocnord, fi6)

fitcn <- GetModel(df, meteoU, 20, '2016-01-01')

######################################################################################
######################################################################################
datas <- as.data.frame(read_feather("C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_cnord"))
#colnames(datacn)[2] <- 'date'

df <- AggregateMLR(datas)

meteocnord <- read.csv2("C:/Users/utente/Documents/PUN/storico_roma.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
fi6 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Firenze 2016.xlsx", sheet= 1, colNames=TRUE)
fi6 <- get_meteo(fi6)

meteocnord[,2:ncol(meteocnord)] <- data.matrix(meteocnord[,2:ncol(meteocnord)])

meteocnord$Data <- seq.Date(as.Date('2010-01-01'), as.Date('2015-12-31'), by = 'day')
fi6$Data <- seq.Date(as.Date('2016-01-01'), as.Date('2016-08-31'), by = 'day')

meteoU <- rbind(meteocnord, ro6)

fits <- GetModel(df, fi6, 20, '2016-01-01')

######################################################################
df8 <- df[which(unlist(df$date) <= as.Date("2016-08-31")),]
df8 <- cbind(df8, CD = rep(0,nrow(df8)))
DTN <- MakeDatasetMLR_2(df8, meteo, H, data_inizio) 
######################################################################

predict.gam(fits$gam, df[,-c(28,30)])

yy <- df$y[-c(89,97)]

plot(yy, type = 'l', lwd = 2, col = 'blue')
lines(fits$gam$fitted.values, type = 'l', lwd = 2, col = 'red')


cnordMO <- read_excel("C:\\Users\\utente\\Documents\\misure\\MOcnord2.xlsx")

cnordMO[,1] <- seq.Date(as.Date('2015-01-01'), as.Date('2016-10-31'), by = 'day')
colnames(cnordMO)[1] <- 'date'

cnordMO8 <- cnordMO[which(cnordMO$date >= as.Date('2016-01-01') & cnordMO$date <= as.Date('2016-08-31')),] 

cnmo20 <- cnordMO8$`20`[5:244]
cnmo20 <- cnmo20[-c(89,97)]

##################################################################################
##### check on why model$gam$fitted.values misses some values -----> there are NAs in the temperature -------
df8 <- df[which(unlist(df$date) <= as.Date("2016-08-31")),]
df8 <- cbind(df8, CD = rep(0,nrow(df8)))

data.frame(DT[c(89,97),])

DT <- MakeDatasetMLR_2(df8, fi6, 20, '2016-01-01')

dtp <- predict.gam(fits$gam, DT[,1:29])
dtp[is.na(dtp)] <- 0

plot(DT$y, type = 'l', lwd = 2, col = 'black')
lines(dtp, type = 'l', lwd = 2, col = 'red')
#################################################################################


plot(cnmo20, type = 'l', lwd = 2, col = 'black')
lines(DT$y, type = 'l', lwd = 2, col = 'red')

### differenze con i dati a consuntivo di Terna

mean(DT$y - cnmo20)
sd(DT$y - cnmo20)
median(DT$y - cnmo20)

abs(mean(DT$y - cnmo20))/(mean(DT$y - cnmo20))
############################################################################################################
##### CSUD linear model

datas <- as.data.frame(read_feather("C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_csud"))
#colnames(datacn)[2] <- 'date'

df <- AggregateMLR(datas)

meteocnord <- read.csv2("C:/Users/utente/Documents/PUN/storico_roma.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
fi6 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Roma 2016.xlsx", sheet= 1, colNames=TRUE)
fi6 <- get_meteo(fi6)

meteocnord[,2:ncol(meteocnord)] <- data.matrix(meteocnord[,2:ncol(meteocnord)])

meteocnord$Data <- seq.Date(as.Date('2010-01-01'), as.Date('2015-12-31'), by = 'day')
fi6$Data <- seq.Date(as.Date('2016-01-01'), as.Date('2016-08-31'), by = 'day')

#meteoU <- rbind(meteocnord, ro6)

fits <- GetLinModel(df, fi6, 20, '2016-01-01')

hist(fits$residuals, probability = TRUE, breaks = 10)
shapiro.test(fits$residuals)

###############################################################################################################
####### SUD linear model

datas <- as.data.frame(read_feather("C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_sud"))
#colnames(datacn)[2] <- 'date'

df <- AggregateMLR(datas)

fi6 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Reggio Calabria 2016.xlsx", sheet= 1, colNames=TRUE)
fi6 <- get_meteo(fi6)

fi6$Data <- seq.Date(as.Date('2016-01-01'), as.Date('2016-08-31'), by = 'day')


fits <- GetLinModel(df, fi6, 20, '2016-01-01')
fits <- GetModel(df, fi6, 20, '2016-01-01')

hist(fits$residuals, probability = TRUE, breaks = 10)
shapiro.test(fits$residuals)

#################################################################################################################
##################################################################################################################
#### experiments with similar days

daya <- get_Table_similar_days(datasa)

daya1 <- daya[which(daya$weekday == 1),]

matplot(t(daya1[,7:30]), type = 'l', lwd = 2, col = daya1$num_week)
pairs(daya1[,c(2,3,26)])

ggplot(data = data.table(week_number = daya1$num_week, H20 = daya1$`20`),
       aes(week_number, H20)) +
  geom_point(size = 1.5) +
  geom_smooth() +
  labs(title = "week number vs H20")

fit3 <- lm(daya1$`20` ~ I(daya1$num_week) + I(daya1$num_week^2) + I(daya1$num_week^3))
summary(fit3)
plot(fit3)

mean(fit3$residuals)
sd(fit3$residuals)
var(fit3$residuals)
median(fit3$residuals)
max(abs(fit3$residuals))

heatmap.2(cor(daya1[,7:30]), Rowv = FALSE, Colv = FALSE, dendrogram = 'none')

#### TERNA's data

terna <- read_excel("C:\\Users\\utente\\Documents\\misure\\aggregato_sbilanciamento.xlsx")

agg <- get_Table_similar_days2(terna, "CNOR", "FABBISOGNO REALE")
mo <- get_Table_similar_days2(terna, "CNOR", "MO [MWh]")
mno <- data.frame(agg[,1:6], agg[,7:30] + mo[,7:30])

color = ifelse(lubridate::year(agg$date) == 2015, "blue", "red")
matplot(t(agg[,7:30]), type = "l", col = color, main = 'fabbisogno totale')
matplot(t(mno[,7:30]), type = "l", col = color, main = 'consumo non orari')
matplot(t(-mo[,7:30]), type = "l", col = color, main = 'consumo orari')


agg2 <- agg[which(agg$weekday == 4),]

color = ifelse(lubridate::year(agg2$date) == 2015, "green", "purple")
matplot(t(agg2[,7:30]), type = "l", col = color)

mno5 <- mno[which(lubridate::year(mno$date) == 2015),]
mo5 <- mo[which(lubridate::year(mo$date) == 2015),]

color2 <- ifelse(mno5$weekday %in% c(1,7), "gold", "grey")
plot(mno5$num_day,mno5$X19, col = color2, pch = 16)
plot(mno5$num_week,mno5$X19, col = color2, pch = 16)
color3 <- ifelse(mo5$holiday > 0, "green", "black")
plot(mo5$num_day,-mo5$`19`, col = color3, pch = 16)
plot(mo5$num_week,-mo5$`19`, col = color3, pch = 16)


matplot(t(agg2[,7:30]), type = 'l', lwd = 2, col = agg2$num_week)
pairs(agg1[,c(2,3,26)])

ggplot(data = data.table(week_number = agg2$num_week, H20 = agg2$`20`),
       aes(week_number, H20)) +
  geom_point(size = 1.5) +
  geom_smooth() +
  labs(title = "week number vs H20")

fit3_terna <- lm(agg2$`20` ~ I(agg2$num_week) + I(agg2$num_week^2) + I(agg2$num_week^3) + holiday, data = agg2)
summary(fit3_terna)
plot(fit3_terna)


resG1 <- fit3_terna$residuals[which(fit3_terna$fitted.values >= 8.5)]
fitG1 <- fit3_terna$fitted.values[which(fit3_terna$fitted.values >= 8.5)]

ggplot(data = data.table(fittedG1 = fitG1, residualsG1 = resG1),
       aes(fittedG1, residualsG1)) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, color = "red", size = 0.8) +
  geom_smooth() 

##############################################################################################################
#### functional model for similar days

source("R_code/similar_days_model.R")

datacn <- as.data.frame(read_feather("C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_cnord"))

fi6 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Firenze 2016.xlsx", sheet= 1, colNames=TRUE)

# source("R_code/structSSM.R")
# 
# kalmanFilter = function( x )
# {
#   t = x
#   if (class(t) != "ts") {
#     t = ts(t)
#   }
#   ssModel = structSSM( y = t, distribution="Gaussian", transform = "none")
#   ssFit = fitSSM(inits=c(0.5*log(var(t)), 0.5*log(var(t))), model = ssModel )
#   kfs = KFS( ssFit$model, smoothing="state", nsim=length(t))
#   vals = kfs$a
#   lastVal = vals[ length(vals)]
#   return(lastVal)
# }
# 
# kalmanFilter(fi6$Tmedia)


dfr <- make_dataset_similar_day(datacn, fi6, "2016-12-31", 2)
sum(dfr$pioggia)

write.xlsx(dfr, "dfr.xlsx")
########### simple regression #############

fit1 <- lm(dfr$y7 ~ dfr$regr7 + dfr$num_day + dfr$num_week + dfr$holiday + I(dfr$Tmedia) + I(dfr$Tmedia^2) + I(dfr$Tmedia^3) + 
             dfr$vento + dfr$pioggia + dfr$tnum_day + dfr$tholiday + dfr$tTmedia + 
             dfr$tvento + dfr$tpioggia)
summary(fit1)
layout(1)
plot(fit1)

ctrl <- list(niterEM = 10, msVerbose = TRUE, optimMethod="L-BFGS-B")
fit2 <- gamm(dfr$y7 ~ dfr$regr7 + s(dfr$num_day, bs = "cc") + s(dfr$num_week, bs = "cc") + dfr$holiday + s(dfr$Tmedia, bs = "cc") +# I(dfr$Tmedia^2) + I(dfr$Tmedia^3) + 
             dfr$vento + dfr$pioggia + dfr$tnum_day + dfr$tholiday + dfr$tTmedia + 
             dfr$tvento + dfr$tpioggia, control = ctrl)

plot(fit2$gam, scale = 0)

R2gamm <- 1 - (sum(fit2$gam$residuals^2))/(sum((fit2$gam$fitted.values - mean(dfr$y7))^2))


ggplot(data = data.table(y7 = dfr$y7, fitted = fit2$gam$fitted.values),
       aes(y7, fitted)) +
  geom_point(size = 1.5) +
  geom_smooth() +
  labs(title = "y7 vs Tmedia")

ggplot(data = data.table(fitted = fit2$gam$fitted.values, residuals = fit2$gam$residuals),
       aes(fitted, residuals)) +
  geom_point(size = 1.5) +
  geom_smooth() +
  labs(title = "fitted vs residuals")

mean(fit2$gam$residuals)
median(fit2$gam$residuals)
hist(mean(fit2$gam$residuals))

plot(dfr$Tmedia, dfr$y7)
cor(dfr$tTmedia, dfr$y7)

ggplot(data = data.table(Tmedia = dfr$tTmedia, y7 = dfr$y7),
       aes(Tmedia, y7)) +
  geom_point(size = 1.5) +
  geom_smooth() +
  labs(title = "y7 vs Tmedia")


pairs(data.frame(dfr$y7,dfr$regr7,dfr$num_day,dfr$num_week,dfr$holiday,dfr$Tmedia, dfr$vento,dfr$pioggia,dfr$tnum_day,dfr$tholiday,dfr$tTmedia, dfr$tvento,dfr$tpioggia))

Xreg <- dfr[,9:32]
Y <- dfr[,41:64]
disc <- setdiff(colnames(dfr), colnames(dfr)[c(3,5,35,37, 9:32, 41:64)])
discv <- which(colnames(dfr) %in% disc)

Fbasis <-  create.fourier.basis(c(1,24), nbasis=23)
Sbasis <-  create.bspline.basis(c(1,24), nbasis=23, norder = 5)

FXreg <- smooth.basis(1:24, t(Xreg), Fbasis)$fd
YF <- smooth.basis(1:24, t(Y), Fbasis)$fd

h <- 23 ##### 23 is correct!!!!
BASIS <- matrix(0, nrow = 23, ncol = 24)
BASIS[1,] <- (1/sqrt(23))*rep(1, 24)
BASIS[2,] <- (1/sqrt(23/2))*sin((2*pi/h)*1:24)
BASIS[3,] <- (1/sqrt(23/2))*cos((2*pi/h)*1:24)
BASIS[4,] <- (1/sqrt(23/2))*sin((2*pi/h)*2*(1:24))
BASIS[5,] <- (1/sqrt(23/2))*cos((2*pi/h)*2*(1:24))
BASIS[6,] <- (1/sqrt(23/2))*sin((2*pi/h)*3*(1:24))
BASIS[7,] <- (1/sqrt(23/2))*cos((2*pi/h)*3*(1:24))
BASIS[8,] <- (1/sqrt(23/2))*sin((2*pi/h)*4*(1:24))
BASIS[9,] <- (1/sqrt(23/2))*cos((2*pi/h)*4*(1:24))
BASIS[10,] <- (1/sqrt(23/2))*sin((2*pi/h)*5*(1:24))
BASIS[11,] <- (1/sqrt(23/2))*cos((2*pi/h)*5*(1:24))
BASIS[12,] <- (1/sqrt(23/2))*sin((2*pi/h)*6*(1:24))
BASIS[13,] <- (1/sqrt(23/2))*cos((2*pi/h)*6*(1:24))
BASIS[14,] <- (1/sqrt(23/2))*sin((2*pi/h)*7*(1:24))
BASIS[15,] <- (1/sqrt(23/2))*cos((2*pi/h)*7*(1:24))
BASIS[16,] <- (1/sqrt(23/2))*sin((2*pi/h)*8*(1:24))
BASIS[17,] <- (1/sqrt(23/2))*cos((2*pi/h)*8*(1:24))
BASIS[18,] <- (1/sqrt(23/2))*sin((2*pi/h)*9*(1:24))
BASIS[19,] <- (1/sqrt(23/2))*cos((2*pi/h)*9*(1:24))
BASIS[20,] <- (1/sqrt(23/2))*sin((2*pi/h)*10*(1:24))
BASIS[21,] <- (1/sqrt(23/2))*cos((2*pi/h)*10*(1:24))
BASIS[22,] <- (1/sqrt(23/2))*sin((2*pi/h)*11*(1:24))
BASIS[23,] <- (1/sqrt(23/2))*cos((2*pi/h)*11*(1:24))

par(mfrow= c(2,1))
matplot(t(t(FXreg$coefs)%*%BASIS), type = 'l')
matplot(t(Xreg), type = 'l')


te <- t(FXreg$coefs)%*%BASIS
head(te)
head(Xreg)

layout(1)
matplot(t(te-Xreg), type = 'l')

res <- LeastSquareOptimizer(dfr, penalization = "beta", lambda = 5)
Bs <- vect_to_mat(res$Bstar)
betas <- res$betastar
heatmap.2(Bs, Rowv = FALSE, Colv = FALSE, dendrogram = 'none')
library(plot3D)
layout(1)
M <- mesh(seq(1, 23, length.out = 23),seq(1, 23, length.out = 23))
surf3D(M$x, M$y, Bs)#, colvar = Bs, colkey = FALSE, facets = FALSE)


disc <- setdiff(colnames(dfr), colnames(dfr)[c(3,5,35,37, 9:32, 41:64)])
discv <- which(colnames(dfr) %in% disc)
Z <- as.data.frame(dfr)[,discv]
Yhat <- predict_SimilarDays(Xreg, Z, Bs, rep(0,12))

matplot(t(Yhat%*%t(get_Basis())), type = 'l')

Bh <- solve(t(C)%*%C)%*%t(C)%*%D

Yhat2 <- C%*%Bh%*%get_Basis()
matplot(t(Yhat2), type = "l")

fdiff <- Y - Yhat2
matplot(t(fdiff),type="l")

library(MASS)
#inverse_MP <- svd(t(as.matrix(Z)) %*% as.matrix(Z))$v %*% diag(svd(t(as.matrix(Z)) %*% as.matrix(Z))$d)^(-2) %*% svd(t(as.matrix(Z)) %*% as.matrix(Z))$v
inverse_MP <- ginv(t(as.matrix(Z)) %*% as.matrix(Z))
rhat <- inverse_MP%*%t(as.matrix(Z))%*%as.matrix(fdiff)
RH <- as.matrix(Z)%*%rhat

matplot(t(rhat), type = "l", main = "beta_hat for the discrete variables", col = 1:12)
matplot(t(RH), type = "l")


YYH <- Yhat2 + RH
matplot(t(YYH), type = "l")


Epsilon <- as.matrix(Y) - YYH 
matplot(t(Epsilon), type = "l")

par(mfrow = c(1,1))
plot(YYH[12,], type = "l", col = "blue")
lines(unlist(Y[12,]), type = "l", col = "red")

abs(Epsilon)/as.matrix(Y)
rowMeans(abs(Epsilon)/as.matrix(Y))
colMeans(abs(Epsilon)/as.matrix(Y))
max(rowMeans(abs(Epsilon)/as.matrix(Y)))
max(colMeans(abs(Epsilon)/as.matrix(Y)))
max(abs(Epsilon)/as.matrix(Y))

# betalist <- vector("list", 2)
# betabasis1 <- create.constant.basis(c(1, 24))
# betafd1 <- fd(0, betabasis1)
# betafdPar1 <- fdPar(betafd1)
# betalist[[1]] <- betafdPar1
# nbetabasis <- 23
# betabasis2 <- create.fourier.basis(c(1, 24), nbetabasis)
# betafd2 <- fd(matrix(0,nbetabasis,1), betabasis2)
# 
# model_lin <- linmod(FXreg, YF, betalist)

###########


fitF <- fRegress(YF ~ FXreg, returnMatrix = TRUE)#dfr[,discv])
summary(fitF)
fitF$betaestlist
fitF$betalist

yf <- fdata(YF)
xf <- fdata(FXreg)

plot(yf)
plot(xf)

f = yf ~ dfr[,discv] + xf
basis.x1 = list(xf = create.fdata.basis(xf, l = c(1, 24), type.basis = "fourier"))
fregre.lm(f, dfr, basis.x = basis.x1)


rtt<-c(1, 24)
basis.alpha <- create.constant.basis(rtt)
basisx <- create.bspline.basis(rtt,21)
basisy <- create.bspline.basis(rtt,21)
basiss <- create.bspline.basis(rtt,21)
basist <- create.bspline.basis(rtt,21)
summary(fit1 <-fregre.basis.fr(xf,yf,basis.s=basiss,basis.t=basist))

matplot(t(fit1$residuals$data), type = 'l')

par(mfrow=c(2,1))
matplot(t(fit1$fitted.values$data), type = 'l', col = 'blue')
matplot(t(Y), type = 'l', col = 'red')


Yhat <- predict(fitF)$y
matplot(Yhat, type = 'l')

matplot(Yhat$y, type = 'l')
matplot(t(Y), type = 'l')







mean(with(fitF, (yhatfdobj-yfdPar)^2))
fitF$yhatfdobj - fitF$yfdPar

SXreg <- smooth.basis(1:24, t(Xreg), Sbasis)$fd
YS <- smooth.basis(1:24, t(Y), Sbasis)$fd

fitS <- fRegress(YS ~ SXreg)#dfr[,discv])
summary(fitF)

argvals = seq(0,24,len = ncol(Y))
nbasis = 23
basisobj = create.bspline.basis(c(0,24),nbasis)
Ys = smooth.basis(argvals=argvals, y=t(Y), fdParobj=basisobj,returnMatrix=TRUE)$fd
Xs = smooth.basis(argvals=argvals, y=t(Xreg), fdParobj=basisobj,returnMatrix=TRUE)$fd

plot(Ys)
plot(Xs)

fitS <- fRegress(YS ~ Xs)#dfr[,discv])
summary(fitS)


