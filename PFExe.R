##### Automatic & executable Pun Forward

###### PUN FORWARD LONG TERM #######
library(plyr)
library(dplyr)
library(readxl)
library(feather)
library(lubridate)
library(data.table)
library(h2o)
library(xlsx)


source("C://Users//utente//Documents//R_code//functions_for_corr_meteo_pun.R")
source("C://Users//utente//Documents//R_code//functions_for_POD_orari.R")
source("C://Users//utente//Documents//glm_dataset.R")


################################################################################################################
add_holidays_Date <- function(vd)
{
  ##### codifica numerica delle vacanze
  ## 1 Gennaio = 1, Epifania = 2
  ## Pasqua = 3, Pasquetta = 4
  ## 25 Aprile = 5, 1 Maggio = 6, 2 Giugno = 7,
  ## Ferragosto = 8, 1 Novembre = 9
  ## 8 Dicembre = 10, Natale = 11, S.Stefano = 12, S.Silvestro = 13
  holidays <- 0
  pasqua <- as.Date(c("2010-04-04", "2011-04-24", "2012-04-08", "2013-03-31", "2014-04-20", "2015-04-05", "2016-03-27","2017-04-16", "2018-04-01"))
  pasquetta <- as.Date(c("2010-04-05", "2011-04-25", "2012-04-09", "2013-04-01", "2014-04-21", "2015-04-06", "2016-03-28","2017-04-17", "2018-04-02"))
  
  if(lubridate::month(vd) == 1 & lubridate::day(vd) == 1) holidays <- 1
  if(lubridate::month(vd)  == 1 & lubridate::day(vd) == 6) holidays <- 1
  if(lubridate::month(vd)  == 4 & lubridate::day(vd) == 25) holidays <- 1
  if(lubridate::month(vd)  == 5 & lubridate::day(vd) == 1) holidays <- 1
  if(lubridate::month(vd)  == 6 & lubridate::day(vd) == 2) holidays <- 1
  if(lubridate::month(vd)  == 8 & lubridate::day(vd) == 15) holidays <- 1
  if(lubridate::month(vd)  == 11 & lubridate::day(vd) == 1) holidays <- 1
  if(lubridate::month(vd)  == 12 & lubridate::day(vd) == 8) holidays <- 1
  if(lubridate::month(vd)  == 12 & lubridate::day(vd) == 25) holidays <- 1
  if(lubridate::month(vd)  == 12 & lubridate::day(vd) == 26) holidays <- 1
  if(lubridate::month(vd)  == 12 & lubridate::day(vd) == 31) holidays <- 1
  if(vd %in% pasqua) holidays <- 1
  if(vd %in% pasquetta) holidays <- 1
  
  return(holidays)
}
##############################################################################################################################################
make_DLdataset_pun_forward2 <- function(data)
{
  ### @BRIEF: better function: should work automatically, without expliciting the dates
  ### http://www.noamross.net/blog/2013/4/25/faster-talk.html
  d_f <- data_frame()
  
  # feb29 <- which(as.Date(data$date) == as.Date("2016-02-29")) 
  # Data <- data[-feb29,]
  # pun14 <- Data[which(lubridate::year(Data$date) == 2014),]
  # pun15 <- Data[which(lubridate::year(Data$date) == 2015),]
  # pun16 <- Data[which(lubridate::year(Data$date) == 2016),]
  
  for(i in 1:nrow(data))
  {
    
    
    hs <-  lubridate::hour(as.POSIXct(unlist(data[i,'date']), origin = '1970-01-01'))
    # wds <- lubridate::wday(as.POSIXct(unlist(data[i,'date']), origin = '1970-01-01'))
    # wdys <- lubridate::yday(as.POSIXct(unlist(data[i,'date']), origin = '1970-01-01'))
    # wks <-  lubridate::week(as.POSIXct(unlist(data[i,'date']), origin = '1970-01-01'))
    hol <-  add_holidays_Date(as.Date(as.POSIXct(unlist(data[i,'date']), origin = '1970-01-01')))
    
    new_date <- lubridate::ymd(as.Date(data$date[i])) + lubridate::years(1)
    new_date2 <- lubridate::ymd(as.Date(data$date[i]) + 1) + lubridate::years(1)
    
    if(!is.na(new_date))
    {
      ypun <- data[which(as.Date(data$date) == new_date),]
      #& lubridate::hour(as.POSIXct(unlist(data$date), origin = '1970-01-01')) == hs),]
      
      ypun <- ypun[which(lubridate::hour(as.POSIXct(ypun$date, origin = '1970-01-01')) == hs),]
      
      PK <- 0
      OP <- 0
      F1 <- 0
      F2 <- 0
      F3 <- 0
      
      
      nr <- nrow(ypun)
      
      if(nr == 0)
      {
        y <- 0
      }
      else if(nr == 2)
      {
        y <- sum(ypun$pun, na.rm = FALSE)
      
      }
      else
      {
        y <- ypun$pun
        
        if(unlist(ypun[,3]) == 'OP')
        {
          OP <- 1
        }
        else
        {
          PK <- 1
        }  
        
        if(unlist(ypun[,4]) == 'F1')
        {
          F1 <- 1
        }
        else if(unlist(ypun[,4]) == 'F2')
        {
          F2 <- 1
        }
        else
        {
          F3 <- 1
        }
        
      }
      mon <- tues <- wed <- thur <- fri <- sat <- sun <- 0
      twds <- as.character(lubridate::wday(new_date, label = TRUE, abbr = FALSE))
      
      if(twds == "Monday") mon <- 1
      if(twds == "Tuesday") tues <- 1
      if(twds == "Wednesday") wed <- 1
      if(twds == "Thursday") thur <- 1
      if(twds == "Friday") fri <- 1
      if(twds == "Saturday") sat <- 1
      if(twds == "Sunday") sun <- 1
      
      twdys <- lubridate::yday(new_date)
      twks <-  lubridate::week(new_date)
      tmonth <- lubridate::month(new_date)
      thol <-  add_holidays_Date(new_date)
      
      df2 <- data.frame(data$pun[i], hol, y, hs, mon, tues, wed, thur, fri, sat, sun, twdys, twks, thol, OP, PK, F1, F2, F3, tmonth)
      l <- list(data.frame(d_f), df2)
      d_f <- rbindlist(l)
    }
    
    else if(is.na(new_date) & new_date2 %in% data$date)
    {
      next
    }
    
    else
    {
      break
    }
    
  }
  colnames(d_f) <- c("lpun","holiday","ypun","thour","monday","tuesday","wednesday","thursday","friday","satday","sunday","tday","tweek","tholiday", "OP", "PK", "F1", "F2", "F3","tmonth")
  return(d_f)
}
#############################################################################################################
prediction_pun_forward2 <- function(df, start_date, list_ore)
{
  d_f <- data_frame()
  
  days_left <- 365 - (as.Date(start_date) - as.Date("2017-01-01"))
  seq17 <- seq.POSIXt(as.POSIXct(start_date), as.POSIXct('2018-01-02'), by = 'hour')[1:((days_left*24)-1)]
  #list_ore$Date <- seq17
  
  pun16 <- df[which(lubridate::year(as.Date(df$date, origin = '1899-12-30')) == 2016),]
  
  for(i in 1:nrow(pun16))
  {
    #print(i)
    hs <-  lubridate::hour(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    # wds <- lubridate::wday(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    # wdys <- lubridate::yday(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    # wks <-  lubridate::week(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01'))
    hol <-  add_holidays_Date(as.Date(as.POSIXct(unlist(pun16[i,'date']), origin = '1970-01-01')))
    
    new_date <- lubridate::ymd(as.Date(pun16$date[i], origin = '1899-12-30')) + lubridate::years(1)
    new_date2 <- lubridate::ymd(as.Date(pun16$date[i], origin = '1899-12-30') + 1) + lubridate::years(1)
    
    if(!is.na(new_date))
    {
      
      mon <- tues <- wed <- thur <- fri <- sat <- sun <- 0
      twds <- as.character(lubridate::wday(new_date, label = TRUE, abbr = FALSE))
      
      if(twds == "Monday") mon <- 1
      if(twds == "Tuesday") tues <- 1
      if(twds == "Wednesday") wed <- 1
      if(twds == "Thursday") thur <- 1
      if(twds == "Friday") fri <- 1
      if(twds == "Saturday") sat <- 1
      if(twds == "Sunday") sun <- 1
      
      twdys <- lubridate::yday(new_date)
      twks <-  lubridate::week(new_date)
      tmonth <- lubridate::month(new_date)
      thol <-  add_holidays_Date(new_date)
      tmday <- lubridate::mday(new_date)
      
      PK <- 0
      OP <- 0
      F1 <- 0
      F2 <- 0
      F3 <- 0
      
      lo <- list_ore[which(as.Date(list_ore$Date) == as.Date(new_date, origin = '1899-12-30') & 
                              lubridate::hour(as.POSIXct(list_ore$Date, origin = '1970-01-01')) == hs),]
      
      if(nrow(lo) == 1)
      {
        if(unlist(lo[,7]) == 'OP')
        {
          OP <- 1
        }
        else
        {
          PK <- 1
        }  
        
        if(unlist(lo[,8]) == 'F1')
        {
          F1 <- 1
        }
        else if(unlist(lo[,8]) == 'F2')
        {
          F2 <- 1
        }
        else
        {
          F3 <- 1
        }
      }
      
      df2 <- data.frame(df$pun[i], hol, hs, mon, tues, wed, thur, fri, sat, sun, twdys, twks, thol, OP, PK, F1, F2, F3, tmonth)
      #colnames(df2) <- c("lpun","hour","weekday","day","week","holiday","ypun","thour","tweekday","tday","tweek","tholiday")
      #d_f <- bind_rows(d_f, df2)
      l <- list(data.frame(d_f), df2)
      d_f <- rbindlist(l)
    }
    
    else
    {
      next
    }
    
  }
  colnames(d_f) <- c("lpun","holiday","thour","monday","tuesday","wednesday","thursday","friday","satday","sunday","tday","tweek","tholiday", "OP", "PK", "F1", "F2", "F3","tmonth")
  
  return(d_f)
}
#############################################################################################################
#################################################################################
CleanDataset <- function(dt)
{
  il <- c()
  for(i in seq(nrow(dt),1, length.out = nrow(dt)))
  {
    lil <- length(il)
    if(dt$ypun[i] == 0) il <- c(il, i)
    if(length(il) == lil)
    {
      break
    }
  }
  return(dt[-il,])
}
#############################################################################################################


######## automatic dataset #### USA QUESTO PER PUN FORWARD

#system('python DataAggregator.py')


library(h2o)
h2o.init(nthreads = -1, max_mem_size = '20g')


data2 <- read_excel("C:/Users/utente/Documents/misure/redati_2014-2017.xlsx")
colnames(data2) <- c('date', 'pun', "PK/OP", "Fasce")
list_ore <- read_excel("C:/Users/utente/Documents/misure/dati_2014-2017.xlsx", sheet = "Ore")

last_date <- as.Date(as.POSIXct(unlist(data2[nrow(data2),1]), origin = '1970-01-01'), origin = "1970-01-01")

data2$date <- seq.POSIXt(as.POSIXct('2014-01-01'), as.POSIXct(as.character(last_date + 2)), by = 'hour')[1:nrow(data2)]

data2 <- data2[which(lubridate::year(data2$date) >= 2015),]


DLD2 <- make_DLdataset_pun_forward2(data2)
DLD2 <- CleanDataset(DLD2)
DLD2 <- as.data.frame(read_feather("C:/Users/utente/Documents/misure/dati_punForward"))

response <- "ypun"
regressors <- setdiff(colnames(DLD2),response)
regressors2 <- setdiff(colnames(DLD2),c(response, "hour","weekday","day", "week","holiday"))

modeldl2 <- h2o.deeplearning(x = regressors, y = response, training_frame = as.h2o(DLD2),
                             standardize = TRUE, activation = "Rectifier", 
                             hidden = c(1000, 1000, 1000), epochs = 100, elastic_averaging = TRUE,
                             elastic_averaging_regularization = 0.01)

modelgbm <- h2o.gbm(x = regressors, y = response, training_frame = as.h2o(DLD2),
                    ntrees = 5000, max_depth = 8000)


modelglm <- h2o.glm(x = regressors2, y = response, training_frame = as.h2o(DLD2), family = "gaussian")


plot(modeldl2)
print(summary(modeldl2))
print(h2o.r2(modeldl2))
plot(modelgbm)
print(summary(modelgbm))
print(h2o.r2(modelgbm))
plot(modelglm)
print(summary(modelglm))
print(h2o.r2(modelglm))


yhat <- h2o.predict(modeldl2, newdata = as.h2o(DLD2))
yhat <- unlist(as.matrix(as.numeric(yhat$predict)))
plot(yhat, type = "o")
lines(DLD2$ypun, type = 'l', col = 'blue')

yg <- h2o.predict(modelgbm, newdata = as.h2o(DLD2))
yg <- unlist(as.matrix(as.numeric(yg$predict)))
plot(yg, type = "o")
lines(DLD2$ypun, type = 'l', col = 'blue')

ygl <- h2o.predict(modelglm, newdata = as.h2o(DLD2))
ygl <- unlist(as.matrix(as.numeric(ygl$predict)))
plot(ygl, type = "o")
lines(DLD2$ypun, type = 'l', col = 'blue')


pred17 <- prediction_pun_forward2(data2, as.character(last_date + 2), list_ore) ### 2 days ahead from last date of PUN

yhat17 <- h2o.predict(modeldl2, newdata = as.h2o(pred17))
yhat17 <- unlist(as.matrix(as.numeric(yhat17$predict)))
plot(yhat17, type = 'l', col = 'brown')

yg17 <- h2o.predict(modelgbm, newdata = as.h2o(pred17))
yg17 <- unlist(as.matrix(as.numeric(yg17$predict)))
plot(yg17, type = 'l', col = 'orange')
plot(diff(yg17), type = 'l', col = 'green')
abline(v = 3000)

plot((yhat17 + yg17)/2, type = 'l', col = 'red')

ygl17 <- h2o.predict(modelglm, newdata = as.h2o(pred17))
ygl17 <- unlist(as.matrix(as.numeric(ygl17$predict)))
plot(ygl17, type = 'l', col = 'purple')

sd(ygl17)
plot(8*ygl17/sd(ygl17), type = 'l', col = 'magenta')




library(xlsx)
write.xlsx(RPH, "longterm_pun.xlsx")

#### send email 

######### MONTHWISE MODELS ##########
h2o.init(nthreads = -1, max_mem_size = '20g')
response <- "ypun"
regressors <- setdiff(colnames(DLD2),c(response, "tmonth"))

list_ore <- bind_cols(list_ore, pun = data.frame(rep(0, nrow(list_ore) ))) 

pred17 <- prediction_pun_forward2(data2, as.character(last_date + 2), list_ore) ### 2 days ahead from last date of PUN

PFP <- c("PK", "OP")
fasce <- c("F1", "F2", "F3")

prediction <- c()
for(m in 1:12)
{
  for(pfp in PFP)
  {
    for(f in fasce)
    {
    
    model_name <- paste0("gbm_",m,"_",pfp,"_",f)
    
    if(pfp == "PK") 
    {
      if(f == "F1")
      {
        mDLD2 <- DLD2[which(DLD2$tmonth == m & DLD2$PK == 1 & DLD2$F1 == 1),] 
        mpred17 <- pred17[which(pred17$tmonth == m & pred17$PK ==1 & pred17$F1 == 1),]
        if(nrow(mDLD2) == 0)
        {
         next
        }
      }
      else if(f == "F2")
      {
        mDLD2 <- DLD2[which(DLD2$tmonth == m & DLD2$PK == 1 & DLD2$F2 == 1),] 
        mpred17 <- pred17[which(pred17$tmonth == m & pred17$PK ==1 & pred17$F2 == 1),]
        if(nrow(mDLD2) == 0)
        {
          next
        }
      }
      else
      {
        mDLD2 <- DLD2[which(DLD2$tmonth == m & DLD2$PK == 1 & DLD2$F3 == 1),] 
        mpred17 <- pred17[which(pred17$tmonth == m & pred17$PK ==1 & pred17$F3 == 1),]
        if(nrow(mDLD2) == 0)
        {
          next
        }
      }
      
    }
    else 
    {
      if(f == "F1")
      {
        mDLD2 <- DLD2[which(DLD2$tmonth == m & DLD2$OP == 1 & DLD2$F1 == 1),] 
        mpred17 <- pred17[which(pred17$tmonth == m & pred17$OP ==1 & pred17$F1 == 1),]
        if(nrow(mDLD2) == 0)
        {
          next
        }
      }
      else if(f == "F2")
      {
        mDLD2 <- DLD2[which(DLD2$tmonth == m & DLD2$OP == 1 & DLD2$F2 == 1),] 
        mpred17 <- pred17[which(pred17$tmonth == m & pred17$OP ==1 & pred17$F2 == 1),]
        if(nrow(mDLD2) == 0)
        {
          next
        }
      }
      else
      {
        mDLD2 <- DLD2[which(DLD2$tmonth == m & DLD2$OP == 1 & DLD2$F3 == 1),] 
        mpred17 <- pred17[which(pred17$tmonth == m & pred17$OP ==1 & pred17$F3 == 1),]
        if(nrow(mDLD2) == 0)
        {
          next
        }
      }
      
    }
    
    
    mDLD2 <- mDLD2[sample(nrow(mDLD2)),]
    
    
    
    # modelgbm <- h2o.deeplearning(x = regressors, y = response, training_frame = as.h2o(DLD2),
    #                              standardize = TRUE, activation = "Rectifier", 
    #                              hidden = c(1000, 1000, 1000), epochs = 100, elastic_averaging = TRUE,
    #                              elastic_averaging_regularization = 0.01)
    # 
    
    modelgbm <- h2o.gbm(x = regressors, y = response, training_frame = as.h2o(mDLD2), model_id = model_name,
                         ntrees = 5000, max_depth = 24)

    h2o.saveModel(modelgbm, paste0("C://Users/utente/Documents/pun_forward_models/",model_name), force = TRUE)
    print(paste("R2 for", model_name, ":", h2o.r2(modelgbm)))
    
    #modelgbm <- h2o.loadModel(path = paste0("C:/Users/utente/Documents/pun_forward_models/",model_name, "/", model_name))
    
    
    yhat17 <- h2o.predict(modelgbm, newdata = as.h2o(mpred17))
    yhat17 <- unlist(as.matrix(as.numeric(yhat17$predict)))
    
    for(i in 1:nrow(mpred17))
    {
      mlo <- which(list_ore$Month == m & list_ore$Day == mpred17$day_month[i] & list_ore$Hour == (mpred17$thour[i] + 1))
      # mlo <- list_ore[which(list_ore$Month == m),]
      # dmlo <- mlo[which(mlo$Day == mpred17$day_month[i]),]
      # hdmlo <- dmlo[which(dmlo$Hour == mpred17$thour[i])]
      list_ore[mlo,9] <- yhat17[i]
    }
    
    
    prediction <- c(prediction, yhat17)
    }
  }
  #print(paste("mean PK/OP spread forecasted = ", mean(unlist(list_ore[which(list_ore$Month == m & list_ore$`PK-OP` == "PK"), 9])) - mean(unlist(list_ore[which(list_ore$Month == m & list_ore$`PK-OP` == "OP"), 9])) ))
}

plot(unlist(list_ore[,9]), type = 'l', col = 'blue')
write.xlsx(prediction, "longterm_pun.xlsx")
write.xlsx(list_ore, "listore.xlsx")

min(unlist(list_ore[,9]))
prediction <- unlist(list_ore[,9])
length(which(prediction == 0))
plot(prediction, type = "l")

colnames(list_ore)[9] <- "pun"

#################################################################################################################################
#################################################################################################################################
######### MONTHWISE MODELS WITH GAMM##########


list_ore <- bind_cols(list_ore, pun = data.frame(rep(0, nrow(list_ore) ))) 

pred17 <- prediction_pun_forward2(data2, as.character(last_date + 2), list_ore) ### 2 days ahead from last date of PUN

PFP <- c("PK", "OP")

prediction <- c()
for(m in 1:12)
{
  for(pfp in PFP)
  {
    
    
    model_name <- paste0("gbm_",m,"_",pfp)
    
    if(pfp == "PK") {mDLD2 <- DLD2[which(DLD2$tmonth == m & DLD2$PK == 1),]; mpred17 <- pred17[which(pred17$tmonth == m & pred17$PK ==1),];
    print(paste("PK/OP average spread in month",m," = ", mean(unlist(DLD2[which(DLD2$tmonth == m & DLD2$PK == 1),"lpun"])) - mean(unlist(DLD2[which(DLD2$tmonth == m & DLD2$PK == 0),"lpun"])) ))}
    else {mDLD2 <- DLD2[which(DLD2$tmonth == m & DLD2$OP == 1),]; mpred17 <- pred17[which(pred17$tmonth == m & pred17$OP ==1),]}
    
    mDLD2 <- mDLD2[sample(nrow(mDLD2)),]
    
    ctrl <- list(niterEM = 100, msVerbose = TRUE, optimMethod="L-BFGS-B")
    
    m3 <- eval(substitute(gamm(ypun ~ lpun + holiday + s(thour, bs = "cc") + s(tday, bs = "cc") + monday + tuesday + wednesday +
                               thursday + friday + satday + sunday + tholiday + OP + PK +
                               F1 + F2 + F3 , data = mDLD2,
                                control = ctrl, niterPQL = 500)), mDLD2)
    
    
    
    #modelgbm <- h2o.loadModel(path = paste0("C:/Users/utente/Documents/pun_forward_models/",model_name, "/", model_name))
    
    
    yhat17 <- predict(modelgbm, newdata = mpred17)
    yhat17 <- unlist(as.matrix(as.numeric(yhat17$predict)))
    
    for(i in 1:nrow(mpred17))
    {
      mlo <- which(list_ore$Month == m & list_ore$Day == mpred17$day_month[i] & list_ore$Hour == (mpred17$thour[i] + 1))
      # mlo <- list_ore[which(list_ore$Month == m),]
      # dmlo <- mlo[which(mlo$Day == mpred17$day_month[i]),]
      # hdmlo <- dmlo[which(dmlo$Hour == mpred17$thour[i])]
      list_ore[mlo,9] <- yhat17[i]
    }
    
    
    prediction <- c(prediction, yhat17)
  }
  #print(paste("mean PK/OP spread forecasted = ", mean(unlist(list_ore[which(list_ore$Month == m & list_ore$`PK-OP` == "PK"), 9])) - mean(unlist(list_ore[which(list_ore$Month == m & list_ore$`PK-OP` == "OP"), 9])) ))
}

plot(unlist(list_ore[,9]), type = 'l', col = 'blue')
write.xlsx(prediction, "longterm_pun.xlsx")
write.xlsx(list_ore, "listore.xlsx")

#################################################################################
Assembler2 <- function(real, ph)
{
  rows <- which(unlist(!is.na(real[,13])))
  real <- real[rows,]
  ### comparison step
  last_date <- as.Date(ph$date[max(which(ph$real == 1))])
  mld <- max(which(ph$real == 1))
  errors <- unlist(real[(mld+1):nrow(real),13]) - ph$pun[(mld+1):nrow(real)]
  r <- (mld+1):nrow(real)
  #write.xlsx(data.frame(ph[r,1:(ncol(ph)-2)],Errors = errors), "C:/Users/utente/Documents/forward_pun_model_error/errors.xlsx", row.names = FALSE, append = TRUE)
  ### assembling step
  re <- rep(0, nrow(ph))
  
  for(i in 1:length(rows))
  {
    ph[i, "pun"] <- unlist(real[rows[i],13])
    re[i] <- 1
  }
  ph <- data.frame(ph, real = re)
  return(ph)
}
#################################################################################
Assemblerk2e <- function(real, ph)
{
  rows <- which(unlist(!is.na(real[,13])))
  real <- real[rows,]
  ### comparison step
  last_date <- as.Date(ph$date[max(which(ph$real == 1))])
  mld <- max(which(ph$real == 1))
  errors <- unlist(real[(mld+1):nrow(real),13]) - ph$ITALIA[(mld+1):nrow(real)]
  r <- (mld+1):nrow(real)
  write.xlsx(data.frame(ph[r,1:(ncol(ph)-2)],Errors = errors), "C:/Users/utente/Documents/forward_pun_model_error/k2e_errors.xlsx", row.names = FALSE, append = TRUE)
  ### assembling step
  re <- rep(0, nrow(ph))
  
  for(i in 1:length(rows))
  {
    ph[i, "ITALIA"] <- unlist(real[rows[i],13])
    re[i] <- 1
  }
  ph <- data.frame(ph, real = re)
  return(ph)
}
#################################################################################
Redimensioner_pkop <- function(ph, mh, mw, from, to, what)
{
  #### @BRIEF: if what == PK => mw is referring to PK
  d_f <- data_frame()
  from <- as.Date(from)
  to <- as.Date(to)
  nOP <- nrow(ph[which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK.OP` == "OP"),])
  nPK <- nrow(ph[which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK.OP` == "PK"),])
  rOP <- which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK.OP` == "OP")
  rPK <- which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & (ph$`PK.OP` == "PK" | ph$`PK.OP` == "P"))
  M <- nOP + nPK
  
  periodpk <- ph[rPK,]
  periodop <- ph[rOP,]
  
  nPKr <- length(which(periodpk$real == 1))
  nOPr <- length(which(periodop$real == 1))
  
  if(what == "PK")  
  {
    opm <- (1/nOP)*((mh*M) - (mw*nPK))
    
    
    pbpk <- ifelse(length(periodpk$pun[periodpk$real == 1]) > 0, (1/nPK)*sum(periodpk$pun[periodpk$real == 1]), 0)
    pbop <- ifelse(length(periodop$pun[periodop$real == 1]) > 0, (1/nOP)*sum(periodop$pun[periodop$real == 1]), 0)
    pihatpk <- (mw - pbpk)/((1/nPK)*sum(periodpk$pun[periodpk$real == 0]))
    pihatop <- (opm - pbop)/((1/nOP)*sum(periodop$pun[periodop$real == 0]))
    for(i in 1:length(rPK))
    {
      if(ph[rPK[i], "real"] == 0) ph[rPK[i], "pun"] <- pihatpk * unlist(ph[rPK[i], "pun"])
    }
    for(i in 1:length(rOP))
    {
      if(ph[rOP[i], "real"] == 0) ph[rOP[i], "pun"] <- pihatop * unlist(ph[rOP[i], "pun"])
    }
  }
  else
  {
    pkm <- (1/nPK)*((mh*M) - (mw*nOP))
    
    pbpk <- ifelse(length(periodpk$pun[periodpk$real == 1]) > 0, (1/nPK)*sum(periodpk$pun[periodpk$real == 1]), 0)
    pbop <- ifelse(length(periodop$pun[periodop$real == 1]) > 0, (1/nOP)*sum(periodop$pun[periodop$real == 1]), 0)
    pihatpk <- (pkm - pbpk)/((1/nPK)*sum(periodpk$pun[periodpk$real == 0]))
    pihatop <- (mw - pbop)/((1/nOP)*sum(periodop$pun[periodop$real == 0]))
    
    for(i in 1:length(rPK))
    {
      ph[rPK[i], "pun"] <- pihatpk * unlist(ph[rPK[i], "pun"])
    }
    for(i in 1:length(rOP))
    {
      ph[rOP[i], "pun"] <- pihatop * unlist(ph[rOP[i], "pun"])
    }
  }
  
  return(ph)
}
#################################################################################
Redimensioner_pkop_Fs <- function(ph, mh, mw, from, to, what)
{
  #### @BRIEF: if what == PK => mw is referring to PK
  d_f <- data_frame()
  from <- as.Date(from)
  to <- as.Date(to)
  nOP <- nrow(ph[which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK.OP` == "OP"),])
  nPK <- nrow(ph[which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK.OP` == "PK"),])
  rOP <- which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK.OP` == "OP")
  rPK <- which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & (ph$`PK.OP` == "PK" | ph$`PK.OP` == "P"))
  M <- nOP + nPK
  
  periodpk <- ph[rPK,]
  periodop <- ph[rOP,]
  
  nPKr <- length(which(periodpk$real == 1))
  nOPr <- length(which(periodop$real == 1))
  
  
    opm <- (1/nOP)*((mh*M) - (mw*nPK))
    
    
    pbpk <- ifelse(length(periodpk$pun[periodpk$real == 1]) > 0, (1/nPK)*sum(periodpk$pun[periodpk$real == 1]), 0)
    pbop <- ifelse(length(periodop$pun[periodop$real == 1]) > 0, (1/nOP)*sum(periodop$pun[periodop$real == 1]), 0)
    pihatpk <- (mw - pbpk)/((1/nPK)*sum(periodpk$pun[periodpk$real == 0]))
    pihatop <- (opm - pbop)/((1/nOP)*sum(periodop$pun[periodop$real == 0]))
    
    rpkf1 <- which(periodpk$AEEG.181.06 == "F1")
    rpkf2 <- which(periodpk$AEEG.181.06 == "F2")
    rpkf3 <- which(periodpk$AEEG.181.06 == "F3")
    ropf2 <- which(periodop$AEEG.181.06 == "F2")
    ropf3 <- which(periodop$AEEG.181.06 == "F3")
        
    for(i in 1:length(rPK))
    {
      #print(i)
      if(ph[rPK[i], "real"] == 0 & ph[rPK[i], "AEEG.181.06"] == "F1") ph[rPK[i], "pun"] <- (length(rpkf1)/(length(rpkf1) + length(rpkf2) + length(rpkf3))) * pihatpk * unlist(ph[rPK[i], "pun"])
      else if(ph[rPK[i], "real"] == 0 & ph[rPK[i], "AEEG.181.06"] == "F2") ph[rPK[i], "pun"] <- (length(rpkf2)/(length(rpkf1) + length(rpkf2) + length(rpkf3))) * pihatpk * unlist(ph[rPK[i], "pun"])
      else if(ph[rPK[i], "real"] == 0 & ph[rPK[i], "AEEG.181.06"] == "F3") ph[rPK[i], "pun"] <- (length(rpkf3)/(length(rpkf1) + length(rpkf2) + length(rpkf3))) * pihatpk * unlist(ph[rPK[i], "pun"])
      else next
    }
    for(i in 1:length(rOP))
    {
      #print(i)
      if(ph[rOP[i], "real"] == 0 & ph[rOP[i], "AEEG.181.06"] == "F3") ph[rOP[i], "pun"] <- (length(ropf3)/(length(ropf3) + length(ropf2))) * pihatop * unlist(ph[rOP[i], "pun"])
      else if(ph[rOP[i], "real"] == 0 & ph[rOP[i], "AEEG.181.06"] == "F2") ph[rOP[i], "pun"] <- (length(ropf2)/(length(ropf3) + length(ropf2))) * pihatop * unlist(ph[rOP[i], "pun"])
      else next
    }
  
  return(ph)
}
#################################################################################
Redimensioner_K2E <- function(ph, mh, mw, from, to, what)
{
  #### @BRIEF: function to agjust K2E's forecast
  #### @BRIEF: if what == PK => mw is referring to PK
  d_f <- data_frame()
  from <- as.Date(from)
  to <- as.Date(to)
  nOP <- nrow(ph[which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK/OP` == "OP"),])
  nPK <- nrow(ph[which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK/OP` == "PK"),])
  rOP <- which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK/OP` == "OP")
  rPK <- which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK/OP` == "PK")
  M <- nOP + nPK
  
  periodpk <- ph[rPK,]
  periodop <- ph[rOP,]
  
  nPKr <- length(which(periodpk$real == 1))
  nOPr <- length(which(periodop$real == 1))
  
  if(what == "PK")  
  {
    opm <- (1/nOP)*((mh*M) - (mw*nPK))
    
    
    pbpk <- ifelse(length(periodpk$ITALIA[periodpk$real == 1]) > 0, (1/nPK)*sum(periodpk$ITALIA[periodpk$real == 1]), 0)
    pbop <- ifelse(length(periodop$ITALIA[periodop$real == 1]) > 0, (1/nOP)*sum(periodop$ITALIA[periodop$real == 1]), 0)
    pihatpk <- (mw - pbpk)/((1/nPK)*sum(periodpk$ITALIA[periodpk$real == 0]))
    pihatop <- (opm - pbop)/((1/nOP)*sum(periodop$ITALIA[periodop$real == 0]))
    for(i in 1:length(rPK))
    {
      if(ph[rPK[i], "real"] == 0) ph[rPK[i], "ITALIA"] <- pihatpk * unlist(ph[rPK[i], "ITALIA"])
    }
    for(i in 1:length(rOP))
    {
      if(ph[rOP[i], "real"] == 0) ph[rOP[i], "ITALIA"] <- pihatop * unlist(ph[rOP[i], "ITALIA"])
    }
  }
  else
  {
    pkm <- (1/nPK)*((mh*M) - (mw*nOP))
    
    pbpk <- ifelse(length(periodpk$ITALIA[periodpk$real == 1]) > 0, (1/nPK)*sum(periodpk$ITALIA[periodpk$real == 1]), 0)
    pbop <- ifelse(length(periodop$ITALIA[periodop$real == 1]) > 0, (1/nOP)*sum(periodop$ITALIA[periodop$real == 1]), 0)
    pihatpk <- (pkm - pbpk)/((1/nPK)*sum(periodpk$ITALIA[periodpk$real == 0]))
    pihatop <- (mw - pbop)/((1/nOP)*sum(periodop$ITALIA[periodop$real == 0]))
    for(i in 1:length(rPK))
    {
      ph[rPK[i], "ITALIA"] <- pihatpk * unlist(ph[rPK[i], "ITALIA"])
    }
    for(i in 1:length(rOP))
    {
      ph[rOP[i], "ITALIA"] <- pihatop * unlist(ph[rOP[i], "ITALIA"])
    }
  }
  
  return(ph)
}
#################################################################################
WeekRedimensioner <- function(ph, mh, from, to)
{
  #### @BRIEF: if what == PK => mw is referring to PK
  d_f <- data_frame()
  from <- as.Date(from)
  to <- as.Date(to)
  M <- nrow(ph[which(as.Date(ph$date) >= from & as.Date(ph$date) <= to),])
  rows <- which(as.Date(ph$date) >= from & as.Date(ph$date) <= to)
  
  
  period <- ph[rows,]
    
  pb <- ifelse(length(period$pun[period$real == 1]) > 0, (1/M)*sum(period$pun[periodpk$real == 1]), 0)
  
  pihat <- (mh - pb)/mean(period$pun[period$real == 0])
  
  for(i in 1:length(rows))
  {
    ph[rows[i], "pun"] <- pihat * unlist(ph[rows[i], "pun"])
  }
  
  
  return(ph)
}
###################################################################
list_orep <- data.table(read_excel('longterm_pun.xlsx'))
real <- read_excel("DB_Borse_Elettriche_PER MI_17_conMacro - Copy.xlsm", sheet = 2)
#list_orep <- list_orep[,c(3:11)]

#list_orep <- list_ore[1:8760,]
#colnames(list_orep)[9] <- "pun"
#df2 <- list_orep


df2 <- Assembler2(real, list_orep)
#df2 <- Assembler2(real, df2)

df2 <- df2[,-10]
colnames(df2)[10] <- "real"

k2e <- data.table(read_excel("pun_K2E.xlsx"))
colnames(k2e)[1] <- "date"

k2e2 <- Assemblerk2e(real, k2e)
k2e2 <- k2e2[,-12]
colnames(k2e2)[12] <- "real"

#plot(k2e$ITALIA, type = "l", col = "purple")
#plot(unlist(df2[,"pun"]), type = "l", col = "red")
##### correction factor xi_t #####
df4 <- df2
hspk <- read_excel("historical_std.xlsx")
for(m in 3:12)
{
  var_m <- var(df2$pun[which(df2$Month == m)])
  rop <- which(df2$PK.OP == "OP")
  rpk <- which(df2$PK.OP == "PK")
  rm <- which(df2$Month == m)
  xi <- sqrt((unlist(hspk[m,"std"])^2 + 2)/(var_m))
  print(paste("xi in", m, "=", xi))
  
  for(r in rm)
  {
    if(unlist(df2[r,"Week.Day"]) < 6)
    {
      if(unlist(df2[r,"PK.OP"]) == "OP")
      {
        df2[r,"pun"] <- unlist(df2[r,"pun"]) - xi
      }
      else
      {
        df2[r,"pun"] <- unlist(df2[r,"pun"]) + xi
      }
    }
  }
}

plot(df2$pun, type = "l", col = "grey")
#write.xlsx(df2, "longterm_pun.xlsx")

# ### extra variance linked to the unknowness of the future...
# for(m in 3:12)
# {
#   rop <- which(df2$PK.OP == "OP")
#   rpk <- which(df2$PK.OP == "PK")
#   rm <- which(df2$Month == m)
# 
#   for(r in rm)
#   {
#     if(unlist(df2[r,"Week.Day"]) < 6)
#     {
#       if(unlist(df2[r,"PK.OP"]) == "OP")
#       {
#         df2[r,"pun"] <- unlist(df2[r,"pun"]) - 0.5
#       }
#       else
#       {
#         df2[r,"pun"] <- unlist(df2[r,"pun"]) + 0.5
#       }
#     }
#   }
# }
# 
# plot(df2$pun, type = "l", col = "purple")
# df2 <- df2[,1:9]
# df2 <- Assembler2(real, df2)

####ovraprezzo F2 redistribuito su F1 e F3:
diffF2 <- data.table(read_excel("diffF2.xlsx"))
df3 <- df2
for(m in 4:12)
{
  dfm <- df2[which(df2$Month == m),]
  nr <- which(df2$Month == m)
  D2 <- mean(df2$pun[which(df2$AEEG.181.06 == "F2")]) - diffF2$F2[m-3]
  
  nF1 <- nrow(dfm[which(dfm$AEEG.181.06 == "F1"),])
  nF3 <- nrow(dfm[which(dfm$AEEG.181.06 == "F3"),])
  
  for(i in nr)
  {
    if(df2$AEEG.181.06[i] == "F1") df2$pun[i] <- df2$pun[i] + D2*(nF1/(nF1 + nF3))
    else if(df2$AEEG.181.06[i] == "F3") df2$pun[i] <- df2$pun[i] + D2*(nF3/(nF1 + nF3))
    else  df2$pun[i] <- df2$pun[i] - D2
  }
  
}
mean(df3$pun)
mean(df2$pun)

plot(df2$pun, type = "l", col = "orange")

write.xlsx(df2, "longterm_pun2.xlsx", row.names = FALSE)

for(m in 1:12)
{
  print(paste("mean in month", m," = ", mean(df2$pun[which(df2$Month == m)])))
  print(paste("sd in month m = ", sd(df2$pun[which(df2$Month == m)])))
  
}
mean(list_ore$pun)
#############################

df <- Redimensioner_pkop(RPH, 42.10, "2017-04-01", "2017-04-30", "PK")
plot(df$pun, type = "l", col = "grey")
mean(df$pun[as.Date(RPH$date) <= as.Date("2017-04-30") & as.Date(RPH$date) >= as.Date("2017-04-01") & RPH$PK.OP == "PK"])

df2 <- list_orep

### PAST
df2 <- Redimensioner_pkop(df2, 44.40, 47.60, "2017-03-24", "2017-03-26", "PK")
df2 <- Redimensioner_pkop(df2, 42.80, 42.93, "2017-04-24", "2017-04-30", "PK")
df2 <- Redimensioner_pkop(df2, 44.46, 47.60, "2017-03-01", "2017-03-31", "PK")
df2 <- Redimensioner_pkop(df2, 42.40, 43.48, "2017-04-01", "2017-04-30", "PK")
####
#### CURRENT
df2 <- Redimensioner_pkop(df2, 49.95, 54.33, "2017-06-12", "2017-06-18", "PK")
df2 <- Redimensioner_pkop(df2, 49.95, 54.33, "2017-06-19", "2017-06-25", "PK")
df2 <- Redimensioner_pkop(df2, 49.95, 54.33, "2017-06-26", "2017-07-02", "PK")
####

df2 <- Redimensioner_pkop(df2, 48.40, 51.65, "2017-06-01", "2017-06-30", "PK")

df2 <- Redimensioner_pkop(df2, 56.30, 63.35, "2017-07-01", "2017-07-31", "PK")
### remaining months of Q3
#df2 <- Redimensioner_pkop(df2, 49.91, 54.20, "2017-08-01", "2017-09-30", "PK")
######
df2 <- Redimensioner_pkop(df2, 47.94, 50.41, "2017-08-01", "2017-08-31", "PK")
df2 <- Redimensioner_pkop(df2, 49.6, 56.50, "2017-09-01", "2017-09-30", "PK")

###Q4
df2 <- Redimensioner_pkop(df2, 49.10, 58.85, "2017-10-01", "2017-12-31", "PK")

df2 <- Redimensioner_pkop(df2, 44.94, 51.75, "2017-10-01", "2017-10-31", "PK")
df2 <- Redimensioner_pkop(df2, 51.64, 62.98, "2017-11-01", "2017-11-30", "PK")
df2 <- Redimensioner_pkop(df2, 49.33, 56.09, "2017-12-01", "2017-12-31", "PK")

write.xlsx(df2, "longterm_pun.xlsx", row.names = FALSE)



mean(df2$pun[as.Date(df2$date) <= as.Date("2017-06-30") & as.Date(df2$date) >= as.Date("2017-04-01") & df2$PK.OP == "PK"])
mean(df2$pun[as.Date(df2$date) <= as.Date("2017-06-30") & as.Date(df2$date) >= as.Date("2017-06-01")])
mean(df2$pun[as.Date(df2$date) <= as.Date("2017-06-30") & as.Date(df2$date) >= as.Date("2017-04-01")])
mean(df2$pun[as.Date(df2$date) <= as.Date("2017-05-31") & as.Date(df2$date) >= as.Date("2017-05-01")])
mean(df2$pun[as.Date(df2$date) <= as.Date("2017-04-30") & as.Date(df2$date) >= as.Date("2017-04-01")])
mean(df2$pun[as.Date(df2$date) <= as.Date("2017-07-31") & as.Date(df2$date) >= as.Date("2017-07-01")])
mean(df2$pun[as.Date(df2$date) <= as.Date("2017-09-30") & as.Date(df2$date) >= as.Date("2017-07-01") & df2$PK.OP == "PK"])
mean(df2$pun[as.Date(df2$date) <= as.Date("2017-12-31") & as.Date(df2$date) >= as.Date("2017-10-01") & df2$PK.OP == "PK"])
mean(df2$pun[as.Date(df2$date) <= as.Date("2017-09-30") & as.Date(df2$date) >= as.Date("2017-07-01")])
mean(df2$pun[as.Date(df2$date) <= as.Date("2017-12-31") & as.Date(df2$date) >= as.Date("2017-10-01")])

df2 <- Redimensioner_pkop_Fs(df2, 42.40, 43.48, "2017-04-01", "2017-04-30", "PK")
df2 <- Redimensioner_pkop_Fs(df2, 42.90, 43.60, "2017-05-01", "2017-05-31", "PK")
df2 <- Redimensioner_pkop_Fs(df2, 45.85, 49.50, "2017-06-01", "2017-06-30", "PK")

df2 <- Redimensioner_pkop_Fs(df2, 56.30, 63.35, "2017-07-01", "2017-07-31", "PK")
df2 <- Redimensioner_pkop_Fs(df2, 46.20, 49.00, "2017-08-01", "2017-08-31", "PK")
df2 <- Redimensioner_pkop_Fs(df2, 48.90, 55.35, "2017-09-01", "2017-09-30", "PK")

df2 <- Redimensioner_pkop_Fs(df2, 44.94, 51.75, "2017-10-01", "2017-10-31", "PK")
df2 <- Redimensioner_pkop_Fs(df2, 51.64, 62.98, "2017-11-01", "2017-11-30", "PK")
df2 <- Redimensioner_pkop_Fs(df2, 49.33, 56.09, "2017-12-01", "2017-12-31", "PK")

plot(df2$pun, type = "l", col = "salmon")
plot(ph$pun, type = "l", col = "gray")

for(m in 1:12)
{
  mm <- mean(df2$pun[which(lubridate::month(as.Date(df2$date)) == m)])
  print(mm)
}
#Q3
df2 <- Redimensioner_pkop(df2, 48.50, 53.65, "2017-07-01", "2017-09-30", "PK")
#Q4
df2 <- Redimensioner_pkop(df2, 49.60, 58.60, "2017-10-01", "2017-12-31", "PK")

plot(df2$pun, type = "l", col = "magenta")

### 2017/03/15
diffk2e <- data.frame(op = c(0,0,-3.963920579,0.166723517,0.428474413,0.653116911,-7.901892083,-0.95124084,9.513836466,5.172410511,-1.846053886,-3.44081476),
                      pk = c(0,0,-8.579686001,-0.65,-0.2,0,-17.11948935,2.729178576,13.52800805,5.937756087,-10.52805048,3.727927462))

for(m in 3:12)
{
  rop <- which(df2$Month == m & df2$PK.OP == "OP") 
  rpk <- which(df2$Month == m & df2$PK.OP == "PK")
  for(i in 1:length(rop))
  {
    df2[rop[i], "pun"] <- df2[rop[i], "pun"] - rnorm(n = 1, mean = diffk2e[m,"op"], sd = 0.2) 
  }
  for(i in 1:length(rpk))
  {
    df2[rpk[i], "pun"] <- df2[rpk[i], "pun"] - rnorm(n = 1, mean = diffk2e[m,"pk"], sd = 0.2) 
  }
}

plot(df2$pun, type = "l", col = 'blue')

write.xlsx(df2, "longterm_pun.xlsx", row.names = FALSE)

spread <- read_excel("historical_spreads.xlsx")

df3 <- df2


for(m in 1:12)
{
  mpk <- which(df2$Month == m & df2$PK.OP == "PK")
  mop <- which(df2$Month == m & df2$PK.OP == "OP")
  mm <- mean(df2$pun[which(df2$Month == m)])
  Mpk <- mean(unlist(df2[mpk,'pun']))
  Mop <- mean(unlist(df2[mop,'pun']))
                          
  for(i in mpk)
  {
    df2[i,'pun'] <- unlist(df2[i,'pun']) + unlist(spread[m,'spread_pk']) + mm - Mpk
  }
  for(i in mop)
  {
    df2[i,'pun'] <- unlist(df2[i,'pun']) - unlist(spread[m,'spread_op']) + mm - Mop
  }
  
}

df2 <- WeekRedimensioner(df2, 45.05, '2017-03-20', '2017-03-26')
df2 <- WeekRedimensioner(df2, 44.55, '2017-03-27', '2017-04-02')

write.xlsx(Assembler2(real,df2), "longterm_pun.xlsx")


hist(df2$pun[which(df2$AEEG.181.06 == "F1")], breaks = 20, col = "grey")
hist(df2$pun[which(df2$AEEG.181.06 == "F2")], breaks = 20, col = "red")
hist(df2$pun[which(df2$AEEG.181.06 == "F3")], breaks = 20, col = "green")

mean(df2$pun[which(df2$AEEG.181.06 == "F1")])
mean(df2$pun[which(df2$AEEG.181.06 == "F2")])
mean(df2$pun[which(df2$AEEG.181.06 == "F3")])

sd(df2$pun[which(df2$AEEG.181.06 == "F1")])
sd(df2$pun[which(df2$AEEG.181.06 == "F2")])
sd(df2$pun[which(df2$AEEG.181.06 == "F3")])

###### fasce divise in pk op
par(mfrow = c(2,1))
hist(df2$pun[which(df2$AEEG.181.06 == "F1" & df2$PK.OP == "PK")], breaks = 20, col = "grey")
hist(df2$pun[which(df2$AEEG.181.06 == "F1" & df2$PK.OP == "OP")], breaks = 20, col = "skyblue")

par(mfrow = c(2,1))
hist(df2$pun[which(df2$AEEG.181.06 == "F2" & df2$PK.OP == "PK")], breaks = 20, col = "grey")
hist(df2$pun[which(df2$AEEG.181.06 == "F2" & df2$PK.OP == "OP")], breaks = 20, col = "skyblue")

par(mfrow = c(2,1))
hist(df2$pun[which(df2$AEEG.181.06 == "F3" & df2$PK.OP == "PK")], breaks = 20, col = "grey")
hist(df2$pun[which(df2$AEEG.181.06 == "F3" & df2$PK.OP == "OP")], breaks = 20, col = "cyan")

DF <- read_excel("Diff_fasce.xlsx")
df3 <- df2
df2 <- df3
for(m in 3:12)
{
  df2m <- which(df2$Month == m)
  mm <- mean(df2$pun[which(df2$Month == m)])
  for(i in df2m)
  {
    if(df2$AEEG.181.06[i] == "F1")
    {
      df2$pun[i] <- df2$pun[i] + unlist(DF[m,"F1"])
    }
    else if(df2$AEEG.181.06[i] == "F2")
    {
      df2$pun[i] <- df2$pun[i] + unlist(DF[m,"F2"])
    }
    else
    {
      df2$pun[i] <- df2$pun[i] + unlist(DF[m,"F3"])
    }
  }
}

par(mfrow = c(1,1))
plot(df2$pun, type = "l", col = "pink")


