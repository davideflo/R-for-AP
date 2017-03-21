########### Quoting 2018 ############

library(plyr)
library(dplyr)
library(readxl)
library(feather)
library(lubridate)
library(data.table)
library(h2o)


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
      
      twds <- lubridate::wday(new_date)
      twdys <- lubridate::yday(new_date)
      twks <-  lubridate::week(new_date)
      tmonth <- lubridate::month(new_date)
      thol <-  add_holidays_Date(new_date)
      
      df2 <- data.frame(data$pun[i], hol, y, hs, twds, twdys, twks, thol, OP, PK, F1, F2, F3, tmonth)
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
  colnames(d_f) <- c("lpun","holiday","ypun","thour","tweekday","tday","tweek","tholiday", "OP", "PK", "F1", "F2", "F3","tmonth")
  return(d_f)
}
#############################################################################################################
prediction_pun_forward18 <- function(list_ore)
{
  d_f <- data_frame()
  
  seq18 <- seq.POSIXt(as.POSIXct('2018-01-01'), as.POSIXct('2019-01-02'), by = 'hour')[1:8760]
  
  for(i in 1:nrow(list_ore))
  {
    print(i)
    new_date <- as.Date(list_ore$Date[i])
    twds <- lubridate::wday(new_date)
    twdys <- lubridate::yday(new_date)
    twks <-  lubridate::week(new_date)
    tmonth <- lubridate::month(new_date)
    thol <-  add_holidays_Date(new_date)
    tmday <- lubridate::mday(new_date)
    hs <- list_ore$Hour[i] - 1  
    
    PK <- 0
    OP <- 0
    F1 <- 0
    F2 <- 0
    F3 <- 0
  
    if(list_ore$`PK-OP`[i] == 'OP')
    {
      OP <- 1
    }
    else
    {
      PK <- 1
    }  
        
    if(list_ore$`AEEG 181/06`[i] == 'F1')
    {
      F1 <- 1
    }
    else if(list_ore$`AEEG 181/06`[i] == 'F2')
    {
      F2 <- 1
    }
    else
    {
      F3 <- 1
    }
    
      
    df2 <- data.frame(hs, twds, twdys, twks, thol, tmday, OP, PK, F1, F2, F3, tmonth)
    l <- list(data.frame(d_f), df2)
    d_f <- rbindlist(l)
  }
    
  colnames(d_f) <- c("thour","tweekday","tday","tweek","tholiday", "day_month", "OP", "PK", "F1", "F2", "F3","tmonth")
  return(d_f)
}
##############################################################################################################################

#data2 <- data.table(read_excel("C:/Users/utente/Documents/sudd_15-17.xlsx"))

data2 <- read_excel("C:/Users/utente/Documents/misure/redati_2014-2017.xlsx")
colnames(data2) <- c('date', 'pun', "PK/OP", "Fasce")
list_ore <- read_excel("C:/Users/utente/Documents/misure/dati_2014-2017.xlsx", sheet = "Ore")

last_date <- as.Date(as.POSIXct(unlist(data2[nrow(data2),1]), origin = '1970-01-01'), origin = "1970-01-01")
data2$date <- seq.POSIXt(as.POSIXct('2014-01-01'), as.POSIXct(as.character(last_date + 2)), by = 'hour')[1:nrow(data2)]
data2 <- data2[which(lubridate::year(data2$date) >= 2015),]

list_ore18 <- data.table(read_excel("ore_2018.xlsx", sheet = "2018"))

DLD2 <- make_DLdataset_pun_forward2(data2)

library(h2o)
h2o.init(nthreads = -1, max_mem_size = '20g')

######### MONTHWISE MODELS ##########
response <- "ypun"
regressors <- setdiff(colnames(DLD2),c(response, "tmonth", "PK", "OP", "lpun"))

list_ore <- bind_cols(list_ore, pun = data.frame(rep(0, nrow(list_ore) ))) 

pred17 <- prediction_pun_forward2(data2, as.character(last_date + 2), list_ore) ### 2 days ahead from last date of PUN
pred18 <- prediction_pun_forward18(list_ore18)

PFP <- c("PK", "OP")

list_ore18 <- data.frame(list_ore18, pun = rep(0, 8760))

#prediction <- c()
for(m in 1:12)
{
  for(pfp in PFP)
  {
    
    
    model_name <- paste0("NP_gbm_",m,"_",pfp)
    
    if(pfp == "PK") {mDLD2 <- DLD2[which(DLD2$tmonth == m & DLD2$PK == 1),]; mpred17 <- pred17[which(pred17$tmonth == m & pred17$PK ==1),];
    print(paste("PK/OP average spread in month",m," = ", mean(unlist(DLD2[which(DLD2$tmonth == m & DLD2$PK == 1),"lpun"])) - mean(unlist(DLD2[which(DLD2$tmonth == m & DLD2$PK == 0),"lpun"])) ))}
    else {mDLD2 <- DLD2[which(DLD2$tmonth == m & DLD2$OP == 1),]; mpred17 <- pred17[which(pred17$tmonth == m & pred17$OP ==1),]}
    
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
    
    
    #prediction <- c(prediction, yhat17)
  }
  #print(paste("mean PK/OP spread forecasted = ", mean(unlist(list_ore[which(list_ore$Month == m & list_ore$`PK-OP` == "PK"), 9])) - mean(unlist(list_ore[which(list_ore$Month == m & list_ore$`PK-OP` == "OP"), 9])) ))
}

plot(list_ore$rep.0..nrow.list_ore.., type = "l")

####### forecasting 2018 ########
list_ore18 <- data.frame(list_ore18, pun = rep(0, 8760))


for(m in 1:12)
{
  for(pfp in PFP)
  {
    
    model_name <- paste0("NP_gbm_",m,"_",pfp)
    
    if(pfp == "PK") {mpred18 <- pred18[which(pred18$tmonth == m & pred18$PK ==1),]}
    else {mpred18 <- pred18[which(pred18$tmonth == m & pred18$OP ==1),]}
    

    modelgbm <- h2o.loadModel(path = paste0("C:/Users/utente/Documents/pun_forward_models/",model_name, "/", model_name))
    
    
    yhat18 <- h2o.predict(modelgbm, newdata = as.h2o(mpred18))
    yhat18 <- unlist(as.matrix(as.numeric(yhat18$predict)))
    
    for(i in 1:nrow(mpred18))
    {
      mlo <- which(list_ore18$Month == m & list_ore18$Day == mpred18$day_month[i] & list_ore18$Hour == (mpred18$thour[i] + 1))
      list_ore18[mlo,9] <- yhat18[i]
    }
    
    
  }
}

plot(list_ore18$pun, type = "l", col = "YELLOW")
mean(list_ore18$pun)

spread <- read_excel("historical_spreads.xlsx")

df2 <- list_ore18
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

plot(df2$pun, type = "l", col = "brown")

df2$pun[which(df2$pun <= 10)] <- 10
# # # # #  # # # # # # # # # # # # # # # # # # 
hspk <- read_excel("historical_std.xlsx")
for(m in 1:12)
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
df2 <- data.table(df2)

colnames(df2)[1] <- 'date'
df2 <- df2[,1:(ncol(df2)-1)]
df2 <- data.frame(df2, real = rep(0,8760))
df3 <- df2

plot(df2$pun, type = "l", col = "blue")



df2 <- Redimensioner_pkop(df2, 51.34, 61.83, '2018-01-01', '2018-01-31', 'PK')
df2 <- Redimensioner_pkop(df2, 45.42, 52.92, '2018-02-01', '2018-02-28', 'PK')
df2 <- Redimensioner_pkop(df2, 43.50, 49.31, '2018-03-01', '2018-03-31', 'PK')
df2 <- Redimensioner_pkop(df2, 36.90, 36, '2018-04-01', '2018-04-30', 'PK')
df2 <- Redimensioner_pkop(df2, 39.17, 40.01, '2018-05-01', '2018-05-31', 'PK')
df2 <- Redimensioner_pkop(df2, 40.93, 42.44, '2018-06-01', '2018-06-30', 'PK')
df2 <- Redimensioner_pkop(df2, 43.92, 49.38, '2018-07-01', '2018-07-31', 'PK')
df2 <- Redimensioner_pkop(df2, 36.67, 37.54, '2018-08-01', '2018-08-31', 'PK')
df2 <- Redimensioner_pkop(df2, 37.85, 42.26, '2018-09-01', '2018-09-30', 'PK')
df2 <- Redimensioner_pkop(df2, 40.31, 49.64, '2018-10-01', '2018-10-31', 'PK')
df2 <- Redimensioner_pkop(df2, 46.28, 60.58, '2018-11-01', '2018-11-30', 'PK')
df2 <- Redimensioner_pkop(df2, 43.12, 52.98, '2018-12-01', '2018-12-31', 'PK')

df2 <- Redimensioner_pkop(df2, 42.15, 48.00, '2018-01-01', '2018-12-31', 'PK')

plot(df2$pun, type = "l", col = "navy")
mean(df2$pun)

for(m in 1:12)
{
  print(paste("mean month", m, "=",mean(df2$pun[df2$Month == m])))
}

sop <- data.table(read_excel("spread18gen.xlsx", sheet = "spreadop"))
spk <- data.table(read_excel("spread18gen.xlsx", sheet = "spreadpk"))

hist(sop$spreadop, breaks = 20, col = 'blue')
hist(spk$spreadpk, breaks = 20, col = 'red')
library(moments)
spk2 <- spk$spreadpk[!is.na(spk$spreadpk)]
sop2 <- sop$spreadop[!is.na(sop$spreadop)]
skewness(spk2)
skewness(sop2)
mean(spk2)
median(spk2)

for(m in 1:12)
{
  mpk <- which(df2$Month == m & df2$PK.OP == "PK")
  mop <- which(df2$Month == m & df2$PK.OP == "OP")
  mm <- mean(df2$pun[which(df2$Month == m)])
  Mpk <- mean(unlist(df2[mpk,'pun']) - mm)
  Mop <- mean(mm - unlist(df2[mop,'pun']))
  Spk <- sd(unlist(df2[mpk,'pun']) - mm)
  Sop <- sd(mm - unlist(df2[mop,'pun']))
  print(paste("mean pk month", m, "=", Mpk))
  print(paste("mean op month", m, "=", Mop))
  print(paste("std pk month", m, "=", Spk))
  print(paste("std op month", m, "=", Sop))
  print("#############################################")
}

for(m in 1:12)
{
  df2$pun[which(df2$Month == 1 & df2$PK.OP == "PK")] <- df2$pun[which(df2$Month == 1 & df2$PK.OP == "PK")] * 9.58869/sd(df2$pun[which(df2$Month == 1 & df2$PK.OP == "PK")])
  df2$pun[which(df2$Month == 1 & df2$PK.OP == "PK")] <- df2$pun[which(df2$Month == 1 & df2$PK.OP == "PK")] * 9.58869/sd(df2$pun[which(df2$Month == 1 & df2$PK.OP == "PK")])
  
}

write.xlsx(df2, "pun_forward_2018.xlsx")
