########## forecast PUN orario ###
library(openxlsx)
library(dplyr)
library(reshape)
library(stringi)
library(xlsx)
library(vioplot)
library(fda)
library(h2o)
library(TSA)
library(tseries)
library(rnn)
library(TDA)
library(forecast)
library(rugarch)


##### forecast h2o - time series? #######
## https://0xdata.atlassian.net/browse/PUBDEV-2590
#########################################

library(ggplot2)
source("functions_for_PUN.R")

prices10 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2010.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices11 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2011.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices12 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2012.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices13 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2013.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices14 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2014.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices15 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2015.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices16 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2016_06.xlsx", sheet="Prezzi-Prices", colNames=TRUE)

meteonord <- read.csv2("C:/Users/d_floriello/Documents/PUN/storico_milano.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteocsud <- read.csv2("C:/Users/d_floriello/Documents/PUN/storico_roma.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)

# mi16 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Milano 2016.xlsx", sheet=1, colNames=TRUE)
# format(Sys.Date(), "%d/%m/%y")
# ad <- as.Date(unlist(mi16[,2]),origin = "1899-12-30")

# sysda <- as.character(Sys.Date())
# sysda
# "2016-07-18"
# strsplit(sysda, "-")

elabora_meteo_2016("Milano")

tmed <- as.numeric(unlist(meteocsud$Tmedia))
plot(tm <- stl(ts(tmed,frequency=365),s.window="periodic"))
### https://cran.r-project.org/web/views/TimeSeries.html
plot(1:length(unlist(prices10["PUN"])), unlist(prices10["PUN"]), type = "l", lwd=2, col="blue")
acf(unlist(prices10["CSUD"]), lag.max = 100)
acf(as.numeric(unlist(meteocsud["Tmedia"])), lag.max = 365)
#plot(stl(ts(unlist(prices10["PUN"]),frequency=365),s.window=7))
plot(se <- stl(ts(unlist(prices16["CSUD"]),frequency=24),s.window="periodic")) ## questo mi pare piu corretto dalla descrizione dell'help in R
plot(stl(ts(unlist(prices10["PUN"]),frequency=24),s.window="periodic"))
plot(stl(ts(as.numeric(unlist(meteocsud["Tmedia"])),frequency=365),s.window="periodic"))
#plot(stl(ts(unlist(prices10["PUN"]),frequency=12),s.window=7))

p10 <- prices10[,3:21]
cor(p10)

dts <- unique(dates(prices15[,1]))
tdm <- trova_date_mancanti(dts, meteo = meteocsud)
tdm

test <- create_dataset(prices10, "ven")
test <- test[1:8736,]
test23 <- create_dataset23(prices10, "ven", "CSUD", meteocsud)
csud11 <- create_dataset23(prices11, "sab", "CSUD", meteocsud)


library(h2o)
##### NOTA BENE: se h2o non parte, vai in C:\Users\d_floriello\Documents\R\R-3.3.1\library\h2o\java e fai partire il JAR file!!!!!
## http://localhost:54321/flow/index.html ## flow
h2o.init(nthreads = -1)

train <- as.h2o(test23[1:7000,])
val <- as.h2o(test23[7001:8737,])
dl <- h2o.deeplearning(names(train), "y", training_frame = train, validation_frame = val, activation = "Tanh",
                       hidden = c(365, 52, 12, 4), epochs = 100)
pred <- h2o.predict(dl, val)

plot(dl) 

a <- as.numeric(pred$predict) ### <- estrae i valori da pred (H2OFrame Class) 
a <- as.matrix(a)

plot(a, type="l",col="blue")
lines(unlist(test23[,208]), type="o",col="red")

yy <- unlist(test23[7001:8737,"y"])
diff <- yy - a
mean(diff)
sd(diff)

hist(diff,freq = FALSE)
lines(density(diff))

apdiff <- abs(diff)/unlist(test23[7001:8737,"y"])

for(p in c(1:10)/10) print(percentage_greater_than(apdiff,p))

std_diff <- (diff - mean(diff))/sd(diff)

hist(std_diff,freq = FALSE)
lines(density(std_diff))

shapiro.test(std_diff)
qqnorm(diff)
lines(seq(-3,3,0.001),seq(-3,3,0.001),type="l",col="red")

cor(yy,diff) ### <- explained by regression to the mean
cor(yy,apdiff)
par(mfrow = c(2,1))
plot(yy, type="o",col="red")
plot(diff, type="o")
########### distribution free qqplot ################################
qq <- qt(p = seq(0, 1, length.out = length(diff)), df = length(diff)-1 )
plot(qq,sort(diff,decreasing=FALSE))
lines(seq(-3,3,0.001),seq(-3,3,0.001),type="l",col="red")
#####################################################################

pred_tot <- predict(dl, as.h2o(test23))

cts <- as.numeric(pred_tot$predict) ### <- estrae i valori da pred (H2OFrame Class) 
cts <- unlist(as.matrix(cts)[,1])
plot(dlts <- stl(ts(cts,frequency=24),s.window="periodic"))

dlts$time.series
min_season <- dlts$time.series[1:24,1]
min_season_orig <- se$time.series[1:24,1]
par(mfrow = c(2,1))
plot(min_season, type="l", col="blue")
plot(min_season_orig, type= "o", col="red")

dl.trend <- unlist(dlts$time.series[,2])
se.trend <- unlist(se$time.series[,2])

plot(dl.trend, type="l", col="blue")
plot(se.trend, type= "l", col="red")

sqrt(mean((dl.trend - se.trend)^2))
RMSE(cts - test23$y)
#############################################################################
###### test: 2010 on 2011 ###################################################
train10 <- as.h2o(test23)
val11 <- as.h2o(csud11)

dl11 <- h2o.deeplearning(names(train10), "y", training_frame = train10, validation_frame = val11, activation = "Tanh",
                       hidden = c(365, 52, 12, 4), epochs = 100)
plot(dl11)

pred11 <- predict(dl11, val11)

p11 <- as.numeric(pred11$predict) 
p11 <- as.matrix(p11)

plot(p11, type="l",col="blue", xlab="time", ylab="euro/MWh", main="CSUD11 calcolato vs vero")
lines(unlist(csud11$y), type="o",col="red")

yy11 <- unlist(csud11$y)
diff11 <- yy11 - unlist(p11)
mean(diff11)
sd(diff11)
median(diff11)

plot(density(diff11),main="distribuzione degli errori")
hist(diff11,freq = FALSE, add=TRUE)

apdiff11 <- abs(diff11)/yy11

for(p in c(1:10)/10) print(percentage_greater_than(apdiff11,p))

std_diff11 <- (diff11 - mean(diff11))/sd(diff11)

hist(std_diff11,freq = FALSE)
lines(density(std_diff11))

shapiro.test(std_diff11)
qqnorm(diff11)
lines(seq(-20,20,0.0001),seq(-20,20,0.0001),type="l",col="red")

cor(yy11,diff11) 
cor(yy11,apdiff11) ## <- almost independent

plot(dlts11 <- stl(ts(unlist(p11[,1]),frequency=24),s.window="periodic"),main="serie stimata")
plot(se11 <- stl(ts(unlist(csud11$y),frequency=24),s.window="periodic"),main="serie vera")
#dlts11$time.series

min_season11 <- dlts11$time.series[1:24,1]
min_season_orig11 <- se11$time.series[1:24,1]
par(mfrow = c(2,1))
plot(min_season11, type="l", col="blue")
plot(min_season_orig11, type= "o", col="red")

dl11.trend <- unlist(dlts11$time.series[,2])
se11.trend <- unlist(se11$time.series[,2])

plot(dl11.trend, type="l", col="blue")
plot(se11.trend, type= "l", col="red")

RMSE(dl11.trend - se11.trend)
RMSE(unlist(p11[,1]) - csud11$y)

par(mfrow=c(1,1))
plot(ud11 <- unlist(diff11[,1]), type="l",xlab="time",ylab="euro/MWh", main = "error")
plot(pud11 <- unlist(apdiff11[,1]), type="l",xlab="time",ylab="%", main = "errore percentuale")
############################################################################
################# test: csud 2010 - 2014 on 2015 ###########################
############################################################################
variables <- colnames(prices10)[c(1:12,14:21)]
prices <- rbind(prices10[c(1:12,14:21)], prices11[,which(colnames(prices11) %in% variables)], prices12[,which(colnames(prices12) %in% variables)], 
                prices13[,which(colnames(prices13) %in% variables)], prices14[,which(colnames(prices14) %in% variables)])

test04 <- create_dataset23(prices, "ven", "CSUD",meteocsud)
val15 <- create_dataset23(prices15, "gio", "CSUD",meteocsud)

train_tot <- as.h2o(test04)
val <- as.h2o(val15)

dltot <- h2o.deeplearning(names(train_tot), "y", training_frame = train_tot, validation_frame = val, activation = "Tanh",
                         hidden = c(365, 52, 12, 4), epochs = 100)
plot(dltot)

predtot <- predict(dltot, val)

pt <- as.numeric(predtot$predict) 
pt <- as.matrix(pt)

plot(pt, type="l",col="blue", xlab="time", ylab="euro/MWh", main="CSUD15 calcolato vs vero")
lines(unlist(val15$y), type="o",col="red")

yy15 <- unlist(val15$y)
diff <- yy15 - unlist(pt)
mean(diff)
sd(diff)
median(diff)

plot(density(diff),main="distribuzione degli errori")
hist(diff,freq = FALSE, add=TRUE)

apdiff <- abs(diff)/yy15

for(p in c(1:10)/10) print(percentage_greater_than(apdiff,p))

std_diff <- (diff - mean(diff))/sd(diff)

#shapiro.test(std_diff)
qqnorm(diff)
lines(seq(-20,20,0.0001),seq(-20,20,0.0001),type="l",col="red")

cor(yy15,diff) 
cor(yy15,apdiff) ## <- almost independent

plot(vera <- stl(ts(unlist(prices["CSUD"]),frequency=24),s.window="periodic"),col="blue",main="serie vera 10 - 14")
plot(dlts <- stl(ts(unlist(pt[,1]),frequency=24),s.window="periodic"),col="blue",main="serie stimata")
plot(se15 <- stl(ts(unlist(val15$y),frequency=24),s.window="periodic"),col="red",main="serie vera")
#dlts11$time.series

min_season <- dlts$time.series[1:24,1]
min_season_orig15 <- se15$time.series[1:24,1]
par(mfrow = c(2,1))
plot(min_season, type="l", col="blue")
plot(min_season_orig15, type= "o", col="red")

dl.trend <- unlist(dlts$time.series[,2])
se15.trend <- unlist(se15$time.series[,2])

plot(dl.trend, type="l", col="blue")
plot(se15.trend, type= "l", col="red")

RMSE(dl.trend - se15.trend)
RMSE(unlist(pt[,1]) - val15$y)

##############################################################################

plot(1:1736,diff[1:1736,1],type="p",lwd=2,col="red")
abline(h =  colMeans(diff), lwd=2, col="black")

sp_test <- sign_process(unlist(test[7001:8736,"y"]))
at <- sign_process(a)

plot(sp_test, type= "o")

pt <- sp_test * at

length(which(pt <= 0))/length(pt)

#### test with forecast
fit_ts <- arfima(test$y)

Yt <- sign_process(test$y)

fr <- forecast(fit_ts, h = 24)

testy <- test[,c(1:216)]

fit_ari <- auto.arima(test$y, xreg = testy)
summary(fit_ari)

## test with autoregressive neural networks

f <- data.frame(coef = fft(unlist(test["y"])), freqindex = c(1:nrow(test)))
mc <- Mod(f$coef)[2:length(f$freqindex)]
qplot(freqindex, Mod(coef), data = f[3000:5000,] ,geom = "line")

f[Mod(f$coef) > 1500 & f$freqindex < 5000, "freqindex"] - 1

peaks <- Mod(f$coef) > 1500
ffilt <- f
ffilt[!peaks, "coef"] <- 0
ffilt <- data.frame(index=ffilt$freqindex[1:5000], value=Re(fft(ffilt$coef[1:5000], inverse=TRUE))/5000, type=rep("filtered", times=5000))
ffilt2 <- data.frame(index=1:5000, value=test[1:5000,"y"], type=rep("original", times=5000))
names(ffilt2) <- names(ffilt)
ffilt <- rbind(ffilt, ffilt2)

qplot(index, value, data = ffilt, colour = as.factor(type), geom = "line")

midindex <- ceiling((length(f$coef)-1)/ 2) + 1
peakind <- f[abs(f$coef) > 1000 & f$freqindex > 1 & f$freqindex < midindex,]
lindex <- length(f$coef)

lowerind <- 1

subsignals <- lapply(c(peakind$freqindex, midindex+1), function(x){
  upperind <- x
  fsub <- f
  notnullind <- ((fsub$freqindex >= lowerind
                  & fsub$freqindex < upperind)
                 |
                   (fsub$freqindex >  (lindex - upperind + 2)
                    & fsub$freqindex <= (lindex - lowerind + 2)))
  fsub[!notnullind,"coef"] <- 0
  lowerind <<- upperind
  Re(fft(fsub$coef, inverse=TRUE)/length(fsub$coef))
})

library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,2)))

vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)

psig <- function(x, y, z){
  h <- data.frame(index = c(1:length(subsignals[[x]])),
                  value = subsignals[[x]])
  lab <- paste("Subseries ", as.character(x), sep="")
  print(qplot(index, value, data = h, geom = "line", main=lab), vp = vplayout(y,z))
  TRUE
}

psig(1,1,1); psig(2,1,2); psig(3,2,1); psig(4,2,2); psig(5,3,1); psig(6,3,2); psig(7,4,1); psig(8,4,2)

library(nnet)
nn.sizes <- c(2,2,3,4,5,6,7,8)

numofsubs <- length(subsignals)
twindow <- 4

offsettedsubdfs <- lapply(1:numofsubs, function(x){
  singleoffsets <- lapply(0:(twindow-1), function(y){
    subsignals[[x]][(twindow-y):(length(subsignals[[x]])-y-1)]
  })
  a <- Reduce(cbind, singleoffsets)
  names <- lapply(1:twindow, function(y){paste("TS", as.character(x), "_", as.character(y), sep = "")})
  b <- as.data.frame(a)
  colnames(b) <- names
  b
})

sample.number <- length(offsettedsubdfs[[1]][,1])

nns <- lapply(1:length(offsettedsubdfs), function(i)
  
{
  
  nn <- nnet(offsettedsubdfs[[i]][1:(sample.number),], #the training samples
             
             subsignals[[i]][(twindow+1):(length(subsignals[[i]]))], #the output
             
             #corresponding to the training samples
             
             size=nn.sizes[i], #number of neurons
             
             maxit = 1000, #number of maximum iteration
             
             linout = TRUE) #the neuron in the output layer should be linear
  
  #the result of the trained networks should be plotted
  
  plot(subsignals[[i]][(twindow+1):(length(subsignals[[i]]))], type="l")
  
  lines(nn$fitted.values,type="l",col="red")
  
  nn
  
})

number.of.predict <- 14

#long term prediction

long.predictions <- lapply(1:length(offsettedsubdfs), function(i)
  
{
  
  prediction <- vector(length=number.of.predict, mode="numeric")
  
  #initial input
  
  input <- offsettedsubdfs[[i]][sample.number,]
  
  for (j in 1 : number.of.predict)
    
  {
    
    prediction[j] <- predict(nns[[i]], input)
    
    input <- c(prediction[j],input[1:(length(input)-1)])
    
  }
  
  #we want to plot the prediction
  
  plot(c(nns[[i]]$fitted.values,prediction), type="l",col="red")
  
  lines(subsignals[[i]][(twindow+1):length(subsignals[[i]])])
  
  prediction
  
})

gc()
nn <- nnet(x= test[,1:216], y = test[,217], size=c(52,24,12,4),data=test,entropy=TRUE, maxit = 1000, MaxNWts = 100000,linout = FALSE)
~
test2 <- test[,1:217]
nn <- nnet(test2$y ~ ., data=test2, size=100, rang=0.5, maxit = 1000, MaxNWts = 100000,linout = TRUE)

  
library(tsDyn)
modar <- aar(test$y, m=1,d=24,steps=24)
plot(modar)

plot(modar$fitted.values, col="blue", type="l", lwd=2)
lines(test$y, col="red", type="l", lwd=1.5)

diff2 <- test$y - modar$fitted.values

library(forecast)

nnf <- nnetar(test2$y, P = 24, size = 1, xreg = test2[,1:216], h = 1)

library(arfima)
arf <- arfima::arfima(prices10$PUN, xreg= prices10[,c(4,8,11,12,19,21)])















