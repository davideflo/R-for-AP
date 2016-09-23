##### predict week in advance #####

library(readxl)
library(lubridate)
library(plyr)
library(dplyr)

source("C://Users//utente//Documents//R_code//functions_for_PPIA_server.R")
source("C://Users//utente//Documents//R_code//functions_for_PUN_server.R")
source("C://Users//utente//Documents//glm_dataset.R")


prices10 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2010.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices12 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2012.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices11 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2011.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices13 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2013.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices14 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2014.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices15 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2015.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices16 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2016_08.xlsx", sheet="Prezzi-Prices", colNames=TRUE)

meteonord <- read.csv2("C:/Users/utente/Documents/PUN/storico_milano_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteocsud <- read.csv2("C:/Users/utente/Documents/PUN/storico_roma.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteocnord <- read.csv2("C:/Users/utente/Documents/PUN/storico_firenze_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteosud <- read.csv2("C:/Users/utente/Documents/PUN/storico_reggiocalabria_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteosici <- read.csv2("C:/Users/utente/Documents/PUN/storico_palermo_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
meteosard <- read.csv2("C:/Users/utente/Documents/PUN/storico_cagliari_aggiornato.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)

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

variables <- colnames(prices10)[c(1:12,14:21)]
prices <- rbind(prices10[c(1:12,14:21)], prices11[,which(colnames(prices11) %in% variables)], prices12[,which(colnames(prices12) %in% variables)], 
                prices13[,which(colnames(prices13) %in% variables)], prices14[,which(colnames(prices14) %in% variables)],
                prices15[,which(colnames(prices15) %in% variables)],prices16[,which(colnames(prices16) %in% variables)])

meteonord[,2:ncol(meteonord)] <- data.matrix(meteonord[,2:ncol(meteonord)])
meteocsud[,2:ncol(meteocsud)] <- data.matrix(meteocsud[,2:ncol(meteocsud)])
meteocnord[,2:ncol(meteocnord)] <- data.matrix(meteocnord[,2:ncol(meteocnord)])
meteosici[,2:ncol(meteosici)] <- data.matrix(meteosici[,2:ncol(meteosici)])
meteosard[,2:ncol(meteosard)] <- data.matrix(meteosard[,2:ncol(meteosard)])
meteosud[,2:ncol(meteosud)] <- data.matrix(meteosud[,2:ncol(meteosud)])

meteoav <- mediate_meteos(meteonord, meteocsud, meteocnord, meteosici, meteosard, meteosud, FALSE)

meteo <- bind_rows(meteoav, meteoav16)

test <- create_fixed_dataset_week(prices, "PUN", meteo)
test <- data.frame(test)
head(test)
tail(test)

dim(test)
test <- test[1:(nrow(test)-3),]

strain <- sample.int(nrow(test), ceiling(0.8*nrow(test))) 
stest <- setdiff(1:nrow(test), strain)
stest <- sample(stest)

trainset <- test[strain,]
testset <- test[stest,]

xlsx::write.xlsx(trainset,"C:\\Users\\utente\\Documents\\PUN\\week\\trainset_week.xlsx", row.names = FALSE, col.names = TRUE)
xlsx::write.xlsx(testset,"C:\\Users\\utente\\Documents\\PUN\\week\\testset_week.xlsx", row.names = FALSE, col.names = TRUE)


####### only 2016 ########
test <- create_fixed_dataset_week(prices16, "PUN", meteoav16)
test <- data.frame(test)
head(test)
tail(test)

dim(test)
test <- test[1:(nrow(test)-3),]

strain <- sample.int(nrow(test), ceiling(0.8*nrow(test))) 
stest <- setdiff(1:nrow(test), strain)
stest <- sample(stest)

trainset <- test[strain,]
testset <- test[stest,]

xlsx::write.xlsx(trainset,"C:\\Users\\utente\\Documents\\PUN\\week\\trainset_week_2016.xlsx", row.names = FALSE, col.names = TRUE)
xlsx::write.xlsx(testset,"C:\\Users\\utente\\Documents\\PUN\\week\\testset_week_2016.xlsx", row.names = FALSE, col.names = TRUE)


####################################################################################################################
######################################## build the models ############################################

response <- "y"

Y <- as.numeric(unlist(trainset["y"]))
yy <- as.numeric(unlist(testset["y"]))

trainseth2o <- as.h2o(trainset)
testseth2o <- as.h2o(testset)

predictors <- setdiff(names(trainseth2o), response)


model_DL <- h2o.deeplearning(x = predictors, y = response, training_frame = trainseth2o, model_id = "model_DL", validation_frame = testseth2o, standardize = TRUE,
                    activation = "Rectifier", hidden = c(2415*4,365,52,12,6), epochs = 100, max_w2 = 100, l1=1e-5)

plot(model_DL)
  
h2o.r2(model_DL, train = TRUE, valid = TRUE)
h2o.mse(model_DL, train = TRUE, valid = TRUE)


model_glm <- h2o.glm(x = predictors, y = response, training_frame = trainseth2o, model_id = "model_glm", validation_frame = testseth2o, standardize = TRUE,
                    family = "gaussian", alpha = 0, lambda = 0.01, lambda_search = TRUE, intercept = TRUE)

full <- h2o.getGLMFullRegularizationPath(model_glm)
sort(full$coefficients_std)
h2o.coef(model_glm)

plot(model_glm)  
  
h2o.r2(model_glm, train = TRUE, valid = TRUE)


h2o.saveModel(model_DL, "C:\\Users\\utente\\Documents\\PUN\\week", force = TRUE)
h2o.saveModel(model_glm, "C:\\Users\\utente\\Documents\\PUN\\week", force = TRUE)


### ERROR DISTRIBUTION  - MODEL_GLM - ##########
pred <- predict(model_glm, testseth2o)

pt <- as.numeric(pred$predict) 
pt <- as.matrix(pt)
pt <- unlist(pt[,1])

diff <- yy - pt

sum((diff-mean(diff))/sd(diff) > sd(diff))/length(diff)

for(v in 1:10)
{
  print(paste("error", v))
  print(paste("normalized percentage",sum((diff-mean(diff))/sd(diff) > v)/length(diff)))
  print(paste("error percentage",sum(abs(diff) > v)/length(diff)))
}

hist(diff)
###############################################
### ERROR DISTRIBUTION - MODEL_DL - ##########
pred2 <- predict(model2, testseth2o)

pt2 <- as.numeric(pred2$predict) 
pt2 <- as.matrix(pt2)
pt2 <- unlist(pt2[,1])

diff2 <- Y - pt2


h2o.rm(trainseth2o); h2o.rm(testseth2o)
h2o.rm(model_DL); h2o.rm(model_glm)

#######################################################################################################################
#######################################################################################################################
######################################## build the models - 2016 - ############################################

response <- "y"

Y <- as.numeric(unlist(trainset["y"]))
yy <- as.numeric(unlist(testset["y"]))

trainseth2o <- as.h2o(trainset)
testseth2o <- as.h2o(testset)

predictors <- setdiff(names(trainseth2o), response)


model_DL <- h2o.deeplearning(x = predictors, y = response, training_frame = trainseth2o, model_id = "model_DL_2016", validation_frame = testseth2o, standardize = TRUE,
                             activation = "Rectifier", hidden = c(2415*4,365,52,12,6), epochs = 100, max_w2 = 100, l1=1e-5)

plot(model_DL)

h2o.r2(model_DL, train = TRUE, valid = TRUE)
h2o.mse(model_DL, train = TRUE, valid = TRUE)


model_glm <- h2o.glm(x = predictors, y = response, training_frame = trainseth2o, model_id = "model_glm_2016", validation_frame = testseth2o, standardize = TRUE,
                     family = "gaussian", alpha = 1/2, lambda = 0.01, lambda_search = TRUE, intercept = TRUE)

full <- h2o.getGLMFullRegularizationPath(model_glm)
sort(full$coefficients_std)
h2o.coef(model_glm)

plot(model_glm)  

h2o.r2(model_glm, train = TRUE, valid = TRUE)


h2o.saveModel(model_DL, "C:\\Users\\utente\\Documents\\PUN\\week", force = TRUE)
h2o.saveModel(model_glm, "C:\\Users\\utente\\Documents\\PUN\\week", force = TRUE)


### ERROR DISTRIBUTION  - MODEL_GLM - ##########
pred <- predict(model_glm, testseth2o)

pt <- as.numeric(pred$predict) 
pt <- as.matrix(pt)
pt <- unlist(pt[,1])

diff <- yy - pt

sum((diff-mean(diff))/sd(diff) > sd(diff))/length(diff)

for(v in 1:10)
{
  print(paste("error", v))
  print(paste("normalized percentage",sum((diff-mean(diff))/sd(diff) > v)/length(diff)))
  print(paste("error percentage",sum(abs(diff) > v)/length(diff)))
}

hist(diff)
###############################################
### ERROR DISTRIBUTION - MODEL_DL - ##########
pred2 <- predict(model2, testseth2o)

pt2 <- as.numeric(pred2$predict) 
pt2 <- as.matrix(pt2)
pt2 <- unlist(pt2[,1])

diff2 <- Y - pt2


h2o.rm(trainseth2o); h2o.rm(testseth2o)
h2o.rm(model_DL); h2o.rm(model_glm)

