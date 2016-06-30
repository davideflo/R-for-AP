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

library(ggplot2)
source("functions_for_PUN.R")

prices10 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2010.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices11 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2011.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices12 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2012.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices13 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2013.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices14 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2014.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices15 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2015.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices16 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/PUN/Anno 2016_04.xlsx", sheet="Prezzi-Prices", colNames=TRUE)

### https://cran.r-project.org/web/views/TimeSeries.html
plot(1:length(unlist(prices10["PUN"])), unlist(prices10["PUN"]), type = "l", lwd=2, col="blue")
acf(unlist(prices10["PUN"]), lag.max = 48)
#plot(stl(ts(unlist(prices10["PUN"]),frequency=365),s.window=7))
plot(se <- stl(ts(unlist(prices10["PUN"]),frequency=24),s.window=7)) ## questo mi pare piu corretto dalla descrizione dell'help in R
plot(stl(ts(unlist(prices10["PUN"]),frequency=24),s.window="periodic"))
#plot(stl(ts(unlist(prices10["PUN"]),frequency=12),s.window=7))

p10 <- prices10[,3:21]
cor(p10)

test <- create_dataset(prices10, "ven")
test <- test[1:8736,]
test23 <- create_dataset23(prices10, "ven")


library(h2o)
##### NOTA BENE: se h2o non parte, vai in C:\Users\d_floriello\Downloads\h2o-3.8.2.3\R\h2o\inst\java e fai partire il JAR file!!!!!
## http://localhost:54321/flow/index.html ## flow
h2o.init(nthreads = -1)

train <- as.h2o(test[1:7000,1:217])
val <- as.h2o(test[7001:8736,1:217])
dl <- h2o.deeplearning(names(train)[1:216], "y", training_frame = train, validation_frame = val, activation = "Tanh",
                       hidden = c(8736, 365, 52, 12, 4), epochs = 100)
pred <- h2o.predict(dl, val)

plot(dl) 

a <- as.numeric(pred$predict) ### <- estrae i valori da pred (H2OFrame Class) 
a <- as.matrix(a)

plot(a, type="l",col="blue")
lines(test[,217], type="l",col="red")


diff <- test[7001:8736,"y"] - a
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
