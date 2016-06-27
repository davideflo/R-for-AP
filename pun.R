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

source("functions_for_PUN.R")

prices10 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2010.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices11 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2011.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices12 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2012.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices13 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2013.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices14 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2014.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices15 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2015.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices16 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2016_04.xlsx", sheet="Prezzi-Prices", colNames=TRUE)

plot(1:length(unlist(prices10["PUN"])), unlist(prices10["PUN"]), type = "l", lwd=2, col="blue")
acf(unlist(prices10["PUN"]), lag.max = 48)
#plot(stl(ts(unlist(prices10["PUN"]),frequency=365),s.window=7))
plot(stl(ts(unlist(prices10["PUN"]),frequency=24),s.window=7)) ## questo mi pare piu corretto dalla descrizione dell'help in R
#plot(stl(ts(unlist(prices10["PUN"]),frequency=8760),s.window=7))

test <- create_dataset(prices10, "ven")

library(h2o)
##### NOTA BENE: se h2o non parte, vai in C:\Users\d_floriello\Downloads\h2o-3.8.2.3\R\h2o\inst\java e fai partire il JAR file!!!!!
## http://localhost:54321/flow/index.html ## flow
h2o.init(nthreads = -1)

train <- as.h2o(test[1:7000,1:217])
val <- as.h2o(test[7001:8737,1:217])
dl <- h2o.deeplearning(names(train)[1:216], "y", training_frame = train, validation_frame = val, activation = "Tanh",
                       hidden = c(8760, 365, 52, 12, 4), epochs = 100)
pred <- h2o.predict(dl, val)

plot(a, type="l",col="blue")
lines(test[,217], type="l",col="red")
plot.H2OModel(train) 

a<-as.numeric(pred$predict) ### <- estrae i valori da pred (H2OFrame Class) 
a <- as.matrix(a)

diff <- test[7001:8737,"y"] - a
plot(1:1736,diff[1:1736,1],type="l",lwd=2,col="red")
abline(h =  1.481189, lwd=2, col="black")

#### test with forecast
fit_ts <- arfima(test$y)

Yt <- sign_process(test$y)

fr <- forecast(fit_ts, h = 24)

testy <- test[,c(1:216)]

fit_ari <- auto.arima(test$y, xreg = testy)
summary(fit_ari)


