#### Meteo model for forecast
#### Analysis of weekly correlations

library(plyr)
library(dplyr)
library(readxl)
library(feather)
library(lubridate)
library(data.table)
library(randomForest)
library(RFinfer)
library(fda)
library(roahd)
library(plot3D)
library(mgcv)
library(gplots)


##############################################################
DailyTimeCorrelation <- function(df, lm)
{
  dt <- data_frame()
  for(i in 2:52)
  {
    cm <- as.numeric(unlist(acf(df$Mon[1:i], lag.max = lm, plot = FALSE))[2])
    ctu <- as.numeric(unlist(acf(df$Tue[1:i], lag.max = lm, plot = FALSE))[2])
    cw <- as.numeric(unlist(acf(df$Wed[1:i], lag.max = lm, plot = FALSE))[2])
    cth <- as.numeric(unlist(acf(df$Thu[1:i], lag.max = lm, plot = FALSE))[2])
    cf <- as.numeric(unlist(acf(df$Fri[1:i], lag.max = lm, plot = FALSE))[2])
    csa <- as.numeric(unlist(acf(df$Sat[1:i], lag.max = lm, plot = FALSE))[2])
    csu <- as.numeric(unlist(acf(df$Sun[1:i], lag.max = lm, plot = FALSE))[2])
    dt <- bind_rows(dt, data.frame(cm, ctu, cw, cth, cf, csa, csu))
  }
  return(dt)
}
##############################################################

### it's better to model the weather dependency with a GAMM model
### DWT is absolute mean; DTW has the mean compared to a "basal consumption"

DWT = data.table(read_excel('C:\\Users\\utente\\Documents\\Sbilanciamento\\DWT.xlsx'))

DWT[, Month := cos(c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),
                  rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))*(pi/12))]

DWT[,c('X__1','pday','pioggia','ponte','dls','edls'):= NULL]
plot(DWT$y, type = 'l')

heatmap.2(cor(DWT), dendrogram = 'none')

DWTrand <- DWT[sample(nrow(DWT)),]


ctrl <- list(niterEM = 500, msVerbose = TRUE, optimMethod="L-BFGS-B")

gmodel <- eval(substitute(gamm(y ~ Lun + Mar + Mer + Gio + Ven + Sab +
                              Dom + Gen + Feb + March + Apr + Mag +
                              Giu + Lug + Ago + Set + Ott + Nov +
                              Dic + hol +
                              s(tmax, bs = "cc") +
                              s(vento, bs = "cc"), data = DWT,
                              control = ctrl, niterPQL = 500)), DWT)

##################################
DWTrand2 <- DWTrand[, c("Dom", "Dic") := NULL]
fit <- lm(y ~ . + I(tmax^2), data = DWTrand2)

summary(fit)
##################################

### this one works and it's the best model so far
gmodel_trial <- eval(substitute(gamm(y ~ s(tmax, bs = "cc") + s(vento) + hol +
                                       Gen + Feb + March + Apr + Mag + Giu + Lug + Ago + Set + Ott + Nov + 
                                       Lun + Mar + Mer + Gio + Ven + Sab, data = DWTrand,
                               control = ctrl, niterPQL = 500)), DWTrand)

DWTest <- DWT[,'y' := NULL]

summary(gmodel_trial$gam)
plot((gmodel_trial$gam))
acf(resid(gmodel_trial$lme))
pacf(resid(gmodel_trial$lme))

yhat = predict(gmodel_trial$gam,  newdata = DWTest, type = "terms", se.fit = TRUE)

chat <- rowSums(yhat$fit) + 12.53449
shat <- rowSums(yhat$se.fit) + 12.53449

plot(chat, type = 'b', col = 'black')
lines(DWT$y, type = 'b', pch = '16', col = 'red')


errors = DWT$y - chat
plot(errors, type = 'b', col = 'blue')
hist(errors, breaks = 20, col = 'skyblue3')


mean(errors)
var(errors)
sd(errors)
max(errors)
min(errors)
median(errors)

sbil = errors/chat
plot(sbil, type = 'b', col = 'grey')
abline(h = 0.15)
abline(h = -0.15)


mean(sbil)
var(sbil)
sd(sbil)
max(sbil)
min(sbil)
median(sbil)

(length(which(sbil >= 0.15)) + length(which(sbil <= -0.15)))/length(sbil)
plot(density(sbil))


mean(errors)
var(errors)
max(errors)
min(errors)
median(errors)

sbil = errors/chat
plot(sbil, type = 'b', col = 'grey')
abline(h = 0.15)
abline(h = -0.15)



mean(sbil)
var(sbil)
sd(sbil)
max(sbil)
min(sbil)
median(sbil)

(length(which(sbil >= 0.15)) + length(which(sbil <= -0.15)))/length(sbil)
plot(density(sbil))

more = DWT[which(sbil >= 0.15),]
less = DWT[which(sbil <= -0.15),]

nrow(more[which(more$hol == 1)])/nrow(more)
nrow(less[which(less$hol == 1)])/nrow(less)
length(which(DWT$hol == 1))/length(DWT$hol)

dwt = data.table(read_excel('C:\\Users\\utente\\Documents\\Sbilanciamento\\prev_meteo.xlsx'))
dwt[,c('X__1','pday','pioggia','ponte','dls','edls'):= NULL]
phat <- predict(gmodel_trial$gam,  newdata = dwt, type = "terms", se.fit = TRUE)

Chat <- sum(phat$fit) + 12.53449

dwt2 <- dwt[, tmax2 := tmax^2]

predict(fit, newdata = dwt)
##################################### WEEKLY CORRELATIONS ##################################################


WC = data.table(read_excel('C:\\Users\\utente\\Documents\\Sbilanciamento\\weekly_correlations.xlsx'))
WC = WC[,2:8]

boxplot(WC)

matplot(t(WC), type = 'l')

Fbasis <-  create.bspline.basis(c(1,7), nbasis=7)
fWC <- smooth.basis(1:7, t(WC), Fbasis)

plot(fWC)

fdwc <- fData(seq(1, 7, 1), WC)

ccf = cov_fun(fdwc)
plot(ccf)

cor(WC)

PCA <- pca.fd(fWC$fd, nharm = 3)

matplot(PCA$harmonics$coefs, type = 'l')

PCAv <- prcomp(WC)

PCAv$sdev
plot(PCAv$sdev/sum(PCAv$sdev), type = 'b', col = 'red')
plot(cumsum(PCAv$sdev/sum(PCAv$sdev)), type = 'b', col = 'blue', lwd = 2)


dtc <- DailyTimeCorrelation(WC, 1)

x <- seq(1, 7, by = 1)
y <- seq(2, 52,by = 1)
M <- mesh(x, y)

alpha <- M$x
beta <- M$y

z <- matrix(0, nrow = length(x), ncol = length(y))
for(i in 1:7)
{
  for(j in 1:50)
  {
    z[i,j] <- unlist(dtc[j,i])
  }
}

surf3D(x = M$x,y = M$y, z = z, colkey=TRUE,bty="b2",main="DTC")

boxplot(dtc)
acf(dtc$cm, lag.max = 50)
pca_dtc <- prcomp(dtc)
plot(pca_dtc$sdev/sum(pca_dtc$sdev), type = 'b', col = 'green', lwd = 2)
plot(cumsum(pca_dtc$sdev/sum(pca_dtc$sdev)), type = 'b', col = 'salmon', lwd = 2)

for(i in 1:7)
{
  for(k in 1:7)
  {
    rho_ik <- (pca_dtc$rotation[k,i]* sqrt(pca_dtc$sdev[i]))/sqrt(var(unlist(dtc[,k])))
    print(paste("rho", i, k, rho_ik))
  }
}
