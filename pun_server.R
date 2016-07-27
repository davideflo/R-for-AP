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

library(TDA)
library(forecast)
library(rugarch)

library(ggplot2)
source("R_code/functions_for_PUN_server.R")

### time series prediction with NN-models for more than one step ahead
## https://www.cs.cmu.edu/afs/cs/academic/class/15782-f06/slides/timeseries.pdf

prices10 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2010.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices11 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2011.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices12 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2012.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices13 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2013.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices14 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2014.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices15 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2015.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices16 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2016_06.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
### H:\Energy Management\04. WHOLESALE\02. REPORT PORTAFOGLIO\2016\06. MI \ DB_Borse_Elettriche_PER MI --> file aggiornato giornalmente con PUN
meteonord <- read.csv2("C:/Users/utente/Documents/PUN/storico_milano_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteocsud <- read.csv2("C:/Users/utente/Documents/PUN/storico_roma.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteocnord <- read.csv2("C:/Users/utente/Documents/PUN/storico_firenze_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteosud <- read.csv2("C:/Users/utente/Documents/PUN/storico_reggiocalabria_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteosici <- read.csv2("C:/Users/utente/Documents/PUN/storico_palermo_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteosard <- read.csv2("C:/Users/utente/Documents/PUN/storico_cagliari_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)

firenze <- read.delim2("C:/Users/utente/Documents/PUN/Firenze.csv", header=FALSE, row.names=NULL,sep=",",colClasses = "character", stringsAsFactors = FALSE)

start.time <- Sys.time()
prepare_meteo(meteocnord,"Firenze")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

### https://cran.r-project.org/web/views/TimeSeries.html
plot(1:length(unlist(prices10["PUN"])), unlist(prices10["PUN"]), type = "l", lwd=2, col="blue")
acf(unlist(prices10["CSUD"]), lag.max = 100)
acf(as.numeric(unlist(meteocsud["Tmedia"])), lag.max = 365)
#plot(stl(ts(unlist(prices10["PUN"]),frequency=365),s.window=7))
plot(se <- stl(ts(unlist(prices10["CSUD"]),frequency=24),s.window="periodic")) ## questo mi pare piu corretto dalla descrizione dell'help in R
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
h2o.init(nthreads = -1, max_mem_size = "20g")

####################################################################
############ test some h2o features ################################
trainset <- h2o.importFile("dataset_totale10_14.csv")
test_set <- h2o.importFile("dataset_15.csv")

dltot <- h2o.deeplearning(names(trainset), "y", training_frame = trainset, validation_frame = test_set, activation = "Tanh",
                          hidden = c(365,52,12,4), epochs = 100)

dltot
summary(dltot)
df1 <- h2o.deepfeatures(dltot,trainset,layer=1)
df2 <- h2o.deepfeatures(dltot,trainset,layer=2)
df3 <- h2o.deepfeatures(dltot,trainset,layer=3)
df4 <- h2o.deepfeatures(dltot,trainset,layer=4)
show(dltot)

dltot2 <- h2o.deeplearning(names(trainset), "y", training_frame = trainset, validation_frame = test_set, activation = "Tanh",
                          hidden = c(4, 12, 52, 365), epochs = 100)
dltot2  ### first one better ### 

##### heavy tuning
act <- c( "Tanh", "TanhWithDropout","Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout")
hide <- list(c(365,52,12,4), c(365,52,12,6,4), c(365,52,12,6,4)*5)
stdize <- c(TRUE,FALSE)
ids <- generate_ids(act,hide,stdize)
ht <- brute_force_tuning(trainset,test_set,act,hide,stdize)
lapply(ht, write, "DL_tuning_results.txt", append=TRUE, ncolumns=1000 )
####################################################################
###################### test new datasets ###########################
step <- 1
train10 <- augmented_dataset(prices10, prices11, step)
test11 <- augmented_dataset(prices11, prices12, step)

start.time <- Sys.time()
trainset <- create_dataset23(train10, "ven", "CSUD", meteocsud, step)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
### ### ### ### ### ### ### ### ### ### ### ### ### 
start.time <- Sys.time()
generate_stepped_datasets(prices10, prices11, prices12,meteocsud)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

act <- c( "Tanh")
act2 <- c("Rectifier")
hide <- list(c(365,52,12,4), c(365,52,12,6,4), c(365,52,12,6,4)*5)
stdize <- c(TRUE,FALSE)

ids <- generate_ids(act,hide,stdize)

start.time <- Sys.time()
#ht <- tuning_with_grid(act, hide, stdize)
ht <- brute_force_tuning_with_steps(act,hide,stdize)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
print("finished Tanh")

start.time <- Sys.time()

ht2 <- brute_force_tuning_with_steps(act2,hide,stdize)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

print("finished all")
####################################################################


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
test23 <- create_dataset23(prices10, "ven", "CSUD", meteocsud)
csud11 <- create_dataset23(prices11, "sab", "CSUD", meteocsud)

train10 <- as.h2o(test23)
val11 <- as.h2o(csud11)

start.time <- Sys.time()
dl11 <- h2o.deeplearning(names(train10), "y", training_frame = train10, validation_frame = val11, activation = "Tanh",
                       hidden = c(8760,365,52,12,4), epochs = 100)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

plot(dl11)

pred11 <- predict(dl11, val11)

p11 <- as.numeric(pred11$predict) 
p11 <- as.matrix(p11)
p11 <- unlist(p11[,1])

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

plot(dlts11 <- stl(ts(p11,frequency=24),s.window="periodic"),main="serie stimata")
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
RMSE(p11 - csud11$y)

par(mfrow=c(1,1))
plot(ud11 <- unlist(diff11[,1]), type="l",xlab="time",ylab="euro/MWh", main = "error")
plot(pud11 <- unlist(apdiff11[,1]), type="l",xlab="time",ylab="%", main = "errore percentuale")
############################################################################
################# test: csud 2010 - 2014 on 2015 ###########################
############################################################################
## try only with weather variables 
variables <- colnames(prices10)[c(1:12,14:21)]
prices <- rbind(prices10[c(1:12,14:21)], prices11[,which(colnames(prices11) %in% variables)], prices12[,which(colnames(prices12) %in% variables)], 
                prices13[,which(colnames(prices13) %in% variables)], prices14[,which(colnames(prices14) %in% variables)])

start.time <- Sys.time()
test04 <- create_dataset23(prices, "ven", "CSUD",meteocsud)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

val15 <- create_dataset23(prices15, "gio", "CSUD",meteocsud)

train_tot <- as.h2o(test04)
val <- as.h2o(val15)

h2o.exportFile(train_tot, "dataset_totale10_14.csv")
h2o.exportFile(val, "dataset_15.csv")

dltot <- h2o.deeplearning(names(train_tot)[c(1:22,162:346)], "y", training_frame = train_tot, validation_frame = val, activation = "Tanh",
                         hidden = c(365,52,12,4), epochs = 100)

dltot
summary(dltot)

visualise_results(dltot, val15, val)

dltotm <- h2o.deeplearning(names(train_tot), "y", training_frame = train_tot, validation_frame = val, activation = "Tanh",
                          hidden = c(365,52,12,4), epochs = 100)

dltotm
summary(dltotm)

visualise_results(dltotm, val15, val)


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

################# try nnet  and nnetar ###################
test23 <- create_dataset23(prices10, "ven", "CSUD", meteocsud)
csud11 <- create_dataset23(prices11, "sab", "CSUD", meteocsud)

response <- "y"
predictors <- setdiff(names(test23), response)
cols <- which(colnames(test23) %in% predictors)

gc()
library(forecast)
library(nnet)

nn <- nnet(x= test23[,cols], y = test23$y, size=c(8760),data=test23,entropy=TRUE, maxit = 1000, MaxNWts = 100000,linout = FALSE)

test2 <- test[,1:217]
nn <- nnet(test2$y ~ ., data=test2, size=100, rang=0.5, maxit = 1000, MaxNWts = 100000,linout = TRUE)

library(tsDyn)
modar <- aar(test23$y, m=1,d=24,steps=24)
plot(modar)

plot(modar$fitted.values, col="blue", type="l", lwd=2)
lines(test$y, col="red", type="l", lwd=1.5)

diff2 <- test$y - modar$fitted.values


nnf <- nnetar(test23$y, P = 24, size = 1, xreg = test2[,1:216], h = 1)

library(arfima)
arf <- arfima::arfima(prices10$PUN, xreg= prices10[,c(4,8,11,12,19,21)])

#############################################

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















