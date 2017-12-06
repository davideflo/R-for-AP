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
prediction_pun_forward18 <- function(list_ore)
{
  d_f <- data_frame()
  
  seq18 <- seq.POSIXt(as.POSIXct('2018-01-01'), as.POSIXct('2019-01-02'), by = 'hour')[1:8760]
  
  
  for(i in 1:nrow(list_ore))
  {
    new_date <- as.POSIXct(list_ore$Date[i], origin = '1970-01-01')
    
    if(!is.na(new_date))
    {
      hs <-  lubridate::hour(as.POSIXct(new_date, origin = '1970-01-01'))

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
      
      df2 <- data.frame(new_date, thol, hs, mon, tues, wed, thur, fri, sat, sun, twdys, twks, thol, OP, PK, F1, F2, F3, tmonth)
      
      l <- list(data.frame(d_f), df2)
      d_f <- rbindlist(l)
    }
    
    else
    {
      next
    }
    
  }
  colnames(d_f) <- c("date","holiday","thour","monday","tuesday","wednesday","thursday","friday","satday","sunday","tday","tweek","tholiday", "OP", "PK", "F1", "F2", "F3","tmonth")
  
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
regressors <- setdiff(colnames(DLD2),c(response, "tmonth", "lpun"))

list_ore <- bind_cols(list_ore18, pun = data.frame(rep(0, nrow(list_ore18) ))) 

#pred17 <- prediction_pun_forward2(data2, as.character(last_date + 2), list_ore) ### 2 days ahead from last date of PUN
pred18 <- prediction_pun_forward18(list_ore18)

list_ore <- data.frame(list_ore18, pun = rep(0, 8760))

PFP <- c("PK", "OP")
fasce <- c("F1", "F2")

prediction <- data_frame()
for(m in 1:12)
{
  for(pfp in PFP)
  {
    for(f in fasce)
    {
      
      model_name <- paste0("gbm_",m,"_",pfp,"_",f,"_2018")
      
      if(pfp == "PK") 
      {
        if(f == "F1")
        {
          mDLD2 <- DLD2[which(DLD2$tmonth == m & DLD2$PK == 1 & DLD2$F1 == 1),] 
          mpred18 <- pred18[which(pred18$tmonth == m & pred18$PK ==1 & pred18$F1 == 1),]
          if(nrow(mDLD2) == 0)
          {
            next
          }
        }
        else if(f == "F2")
        {
          mDLD2 <- DLD2[which(DLD2$tmonth == m & DLD2$PK == 1 & DLD2$F2 == 1),] 
          mpred18 <- pred18[which(pred18$tmonth == m & pred18$PK ==1 & pred18$F2 == 1),]
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
          mpred18 <- pred18[which(pred18$tmonth == m & pred18$OP ==1 & pred18$F1 == 1),]
          if(nrow(mDLD2) == 0)
          {
            next
          }
        }
        else if(f == "F2")
        {
          mDLD2 <- DLD2[which(DLD2$tmonth == m & DLD2$OP == 1 & DLD2$F2 == 1),] 
          mpred18 <- pred18[which(pred18$tmonth == m & pred18$OP ==1 & pred18$F2 == 1),]
          if(nrow(mDLD2) == 0)
          {
            next
          }
        }
        
      }
      
      
      mDLD2 <- mDLD2[sample(nrow(mDLD2)),]
      
      
      modelgbm <- h2o.gbm(x = regressors, y = response, training_frame = as.h2o(mDLD2), model_id = model_name,
                          ntrees = 5000, max_depth = 24)
      
      h2o.saveModel(modelgbm, paste0("C://Users/utente/Documents/pun_forward_models/",model_name), force = TRUE)
      print(paste("R2 for", model_name, ":", h2o.r2(modelgbm)))
      
      #modelgbm <- h2o.loadModel(path = paste0("C:/Users/utente/Documents/pun_forward_models/",model_name, "/", model_name))
      
      
      yhat18 <- h2o.predict(modelgbm, newdata = as.h2o(mpred18[,2:19]))
      yhat18 <- unlist(as.matrix(as.numeric(yhat18$predict)))
      
      for(i in 1:nrow(mpred18))
      {
        mlo <- which(list_ore$Month == m & list_ore$Day == mpred18$day_month[i] & list_ore$Hour == (mpred18$thour[i] + 1))
        # mlo <- list_ore[which(list_ore$Month == m),]
        # dmlo <- mlo[which(mlo$Day == mpred17$day_month[i]),]
        # hdmlo <- dmlo[which(dmlo$Hour == mpred17$thour[i])]
        list_ore[mlo,9] <- yhat18[i]
      }
      
      Y <- data.frame(date = mpred18$date, hour = mpred18$thour, y = yhat18)
      l <- list(prediction, Y)
      prediction <- rbindlist(l)
      
    }
  }
  #### F3 all together --> here 
  mDLD2 <- DLD2[which(DLD2$tmonth == m & DLD2$F3 == 1),] 
  mpred18 <- pred18[which(pred18$tmonth == m & pred18$F3 == 1),]
  mDLD2 <- mDLD2[sample(nrow(mDLD2)),]
  model_name <- paste0("gbm_",m,"_F3_2018")
  modelgbm <- h2o.gbm(x = regressors, y = response, training_frame = as.h2o(mDLD2), model_id = model_name,
                      ntrees = 5000, max_depth = 24)
  
  h2o.saveModel(modelgbm, paste0("C://Users/utente/Documents/pun_forward_models/",model_name), force = TRUE)
  print(paste("R2 for", model_name, ":", h2o.r2(modelgbm)))
  yhat18 <- h2o.predict(modelgbm, newdata = as.h2o(mpred18[,2:19]))
  yhat18 <- unlist(as.matrix(as.numeric(yhat18$predict)))
  
  for(i in 1:nrow(mpred18))
  {
    mlo <- which(list_ore$Month == m & list_ore$Day == mpred18$day_month[i] & list_ore$Hour == (mpred18$thour[i] + 1))
    list_ore[mlo,9] <- yhat18[i]
  }
  Y <- data.frame(date = mpred18$date, hour = mpred18$thour, y = yhat18)
  l <- list(prediction, Y)
  prediction <- rbindlist(l)
  
}

plot(unlist(list_ore[,9]), type = 'l', col = 'blue')
write.xlsx(prediction, "longterm_pun.xlsx")
write.xlsx(list_ore, "listore.xlsx")

min(unlist(list_ore[,9]))
prediction <- unlist(list_ore[,9])
length(which(prediction == 0))
plot(prediction, type = "l")

colnames(list_ore)[9] <- "pun"

### correct prediction:
cpred <- rep(0, nrow(pred18))
for(i in 1:nrow(pred18))
{
  if( i %% 1000 == 0) print(i)
  model_name <- paste0("gbm_",pred18$tmonth[i],"_")
  pfp <- f <- 0
  
  if(pred18$OP[i] == 1)
  {
    pfp <- "OP"
  }
  else
  {
    pfp <- "PK"
  }
  
  if(pred18$F1[i] == 1)
  {
    f <- "F1"
  }
  else if(pred18$F2[i] == 1)
  {
    f <- "F2"
  }
  
  model_name <- paste0(model_name, pfp, "_",f, "_2018")
  
  if(pred18$F3[i] == 1)
  {
    model_name <- paste0("gbm_",pred18$tmonth[i],"_F3_2018") 
  }
  
  modelh2o <- h2o.loadModel(paste0("C:/Users/utente/Documents/pun_forward_models/",model_name,"/",model_name))
  yhat <- h2o.predict(modelh2o, newdata = as.h2o(pred18[i,2:19]))
  yhat <- unlist(as.matrix(as.numeric(yhat$predict)))
  cpred[i] <- yhat
}

plot(cpred, type = "l", col = 'red')

write.xlsx(data.frame(cpred), "NEW_PREDICTION_2018.xlsx")

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
####################################################################################################
########################################################################################################################################################
####################################################################################################
########################################################################################################################################################
####################################################################################################
########################################################################################################################################################
####################################################################################################
########################################################################################################################################################
df2 <- data.table(read_excel("pun_forward_2018.xlsx"))
#df2 <- df2[,-1]
DF <- read_excel("Diff_fasce_2018.xlsx")
df3 <- df2
#df2 <- df3
for(m in 1:12)
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

plot(df2$pun, type = 'l', col = "red")

##### redistribuzione fasce
diffF2 <- data.table(read_excel("diffF2_2018.xlsx"))
df3 <- df2
for(m in 1:12)
{
  dfm <- df2[which(df2$Month == m),]
  nr <- which(df2$Month == m)
  D2 <- mean(df2$pun[which(df2$AEEG.181.06 == "F2")]) - diffF2$F2[m]
  
  nF1 <- nrow(dfm[which(dfm$AEEG.181.06 == "F1"),])
  nF3 <- nrow(dfm[which(dfm$AEEG.181.06 == "F3"),])
  
  for(i in nr)
  {
    if(df2$AEEG.181.06[i] == "F1") df2$pun[i] <- df2$pun[i] + D2*(nF1/(nF1 + nF3))
    else if(df2$AEEG.181.06[i] == "F3") df2$pun[i] <- df2$pun[i] + D2*(nF3/(nF1 + nF3))
    else  df2$pun[i] <- df2$pun[i] - D2
  }
  
}

plot(df3$pun, type = "l", col = 'gold')
plot(df2$pun, type = "l", col = 'grey')
####################################################################################################
########################################################################################################################################################
####################################################################################################
########################################################################################################################################################
####################################################################################################
########################################################################################################################################################
####################################################################################################
########################################################################################################################################################
df2 <- data.table(read_excel("pun_forward_2018.xlsx"))
#df2 <- df3
df2$pun <- cpred

df2 <- Redimensioner_pkop(df2, 66.90, 78.40, '2018-01-01', '2018-01-31', 'PK')
df2 <- Redimensioner_pkop(df2, 45.60, 53.90, '2018-02-01', '2018-02-28', 'PK')
df2 <- Redimensioner_pkop(df2, 38.39, 42.61, '2018-03-01', '2018-03-31', 'PK')
df2 <- Redimensioner_pkop(df2, 38.51, 40.81, '2018-04-01', '2018-04-30', 'PK')
df2 <- Redimensioner_pkop(df2, 39.65, 43.46, '2018-05-01', '2018-05-31', 'PK')
df2 <- Redimensioner_pkop(df2, 40.64, 45.30, '2018-06-01', '2018-06-30', 'PK')
df2 <- Redimensioner_pkop(df2, 50.05, 57.06, '2018-07-01', '2018-07-31', 'PK')
df2 <- Redimensioner_pkop(df2, 41.77, 43.40, '2018-08-01', '2018-08-31', 'PK')
df2 <- Redimensioner_pkop(df2, 43.11, 48.85, '2018-09-01', '2018-09-30', 'PK')
df2 <- Redimensioner_pkop(df2, 41.43, 48.64, '2018-10-01', '2018-10-31', 'PK')
df2 <- Redimensioner_pkop(df2, 46.30, 56.61, '2018-11-01', '2018-11-30', 'PK')
df2 <- Redimensioner_pkop(df2, 44.34, 51.48, '2018-12-01', '2018-12-31', 'PK')

df2 <- Redimensioner_pkop(df2, 44.35, 50.50, '2018-01-01', '2018-12-31', 'PK')

df2 <- Redimensioner_pkop(df2, 57.82, 66.59, '2018-02-01', '2018-03-31', 'PK')
df2 <- Redimensioner_pkop(df2, 51.26, 57.67, '2018-04-01', '2018-12-31', 'PK')

### Q1:
df2 <- Redimensioner_pkop(df2, 48.50, 57.40, '2018-01-01', '2018-03-31', 'PK')
### remaining
df2 <- Redimensioner_pkop(df2, 44.40, 51.35, '2018-04-01', '2018-12-31', 'PK')

write.xlsx(df2, "pun_forward_2018.xlsx", row.names = FALSE)
plot(df2$pun, type = "l", col = "green")

plot(df2$pun, type = "l", col = "darkolivegreen")

for(m in 1:12)
{
  print(m)
print(mean(df2$pun[which(df2$Month == m)]))
  print(mean(df2$pun[which(df2$Month == m & df2$PK.OP == 'PK')]))
  print(mean(df2$pun[which(df2$Month == m & df2$PK.OP == 'OP')]))
  print('------------')
}

df9 <- data.table(read_excel("C://Users/utente//Documents//pun_forward_2019.xlsx"))
colnames(df9) <- colnames(df2)
df9 <- Redimensioner_pkop(df9, 49.75, 57.30, '2019-01-01', '2019-12-31', 'PK')

plot(df9$pun, type = 'l', col = 'salmon')
mean(df9$pun)
mean(df9$pun[which(df9$PK.OP == 'PK')])
write.xlsx(df9, "pun_forward_2019.xlsx", row.names = FALSE)


for(m in 1:12)
{
  mm1 <- mean(df2$pun[which(df2$Month == m & df2$AEEG.181.06 == "F1")])
  print(paste("F1 mese", m, mm1))
  mm2 <- mean(df2$pun[which(df2$Month == m & df2$AEEG.181.06 == "F2")])
  print(paste("F2 mese", m, mm2))
  mm3 <- mean(df2$pun[which(df2$Month == m & df2$AEEG.181.06 == "F3")])
  print(paste("F3 mese", m, mm3))
  }

fasce2018 <- data.table(read_excel("fasce.xlsx", sheet = "2018"))
df3 <- df2
plot(df2$pun, type = 'l', col = 'purple', main = "old")

for(m in 1:12)
{
  mm1 <- mean(df2$pun[which(df2$Month == m & df2$AEEG.181.06 == "F1")])
  df2$pun[which(df2$Month == m & df2$AEEG.181.06 == "F1")] <- fasce2018$F1[m]*(df2$pun[which(df2$Month == m & df2$AEEG.181.06 == "F1")]/mm1) 
  mm2 <- mean(df2$pun[which(df2$Month == m & df2$AEEG.181.06 == "F2")])
  df2$pun[which(df2$Month == m & df2$AEEG.181.06 == "F2")] <- fasce2018$F2[m]*(df2$pun[which(df2$Month == m & df2$AEEG.181.06 == "F2")]/mm2) 
  mm3 <- mean(df2$pun[which(df2$Month == m & df2$AEEG.181.06 == "F3")])
  df2$pun[which(df2$Month == m & df2$AEEG.181.06 == "F3")] <- fasce2018$F1[m]*(df2$pun[which(df2$Month == m & df2$AEEG.181.06 == "F3")]/mm3) 
}
plot(df2$date, df2$pun, type = 'l', col = 'red', main = "new")

write.xlsx(df2, "pun_forward_2018.xlsx", row.names = FALSE)

for(m in 1:12)
{
  mm <- mean(df2$pun[which(lubridate::month(as.Date(df2$date)) == m)])
  print(mm)
}


###Q1:
df2 <- Redimensioner_pkop(df2, 46.20, 54.95, '2018-01-01', '2018-03-31', 'PK')
###Q2:
df2 <- Redimensioner_pkop(df2, 37.55, 37.90, '2018-04-01', '2018-06-30', 'PK')
###Q3:
df2 <- Redimensioner_pkop(df2, 38.40, 42.25, '2018-07-01', '2018-09-30', 'PK')
###Q4:
df2 <- Redimensioner_pkop(df2, 45.70, 56.15, '2018-10-01', '2018-12-31', 'PK')



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

write.xlsx(df2, "pun_forward_2018.xlsx", row.names = FALSE)
df2 <- data.table(read_excel("pun_forward_2018.xlsx"))


# ### extra variance linked to the unknowness of the future...
for(m in 1:12)
{
  rop <- which(df2$PK.OP == "OP")
  rpk <- which(df2$PK.OP == "PK")
  rm <- which(df2$Month == m)

  for(r in rm)
  {
    if(unlist(df2[r,"Week.Day"]) < 6)
    {
      if(unlist(df2[r,"PK.OP"]) == "OP")
      {
        df2[r,"pun"] <- unlist(df2[r,"pun"]) - 0.5
      }
      else
      {
        df2[r,"pun"] <- unlist(df2[r,"pun"]) + 0.5
      }
    }
  }
}

# plot(df2$pun, type = "l", col = "purple")
# df2 <- df2[,1:9]
# df2 <- Assembler2(real, df2)
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
############################### 2019 #########################################################################################

df9 <- data.table(read_excel('pun_forward_2019.xlsx'))

df9$real <- rep(0, nrow(df9))

colnames(df9)[1] <- 'date'
colnames(df9)[9] <- 'pun'


df9 <- Redimensioner_pkop(df9, 43.5, 49.25, '2019-01-01', '2019-12-31', 'PK')


write.xlsx(df9, 'pun_forward_2019.xlsx', row.names = FALSE)
plot(df9$date, df9$pun, type = "l", col = 'green')
