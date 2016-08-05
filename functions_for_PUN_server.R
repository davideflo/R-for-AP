### functions for PUN
library(openxlsx)
library(plyr)
library(dtplyr)
library(reshape)
library(stringi)
library(xlsx)
library(vioplot)
library(fda)
#library(h2o)
library(TSA)
library(tseries)
#library(data.table)

#localH2O <- h2o.init()
library(TDA)
library(purrr)

############################################################
convert_day <- function(day)
{
  if(day == "Sun") return("dom")
  else if (day == "Mon") return("lun")
  else if (day == "Tues") return("mar")
  else if (day == "Wed") return("mer")
  else if (day == "Thurs") return("gio")
  else if (day == "Fri") return("ven")
  else return("sab")
}
############################################################
convert_day_to_angle <- function(day)
{
  days <- c("dom","lun","mar","mer","gio","ven","sab")
  ang <- which(days == day)
  return(cos(ang*pi/7))
}
#############################################################
convert_hour_to_angle <- function(ora)
{
  return(sin((ora-12)*pi/24))
}
#############################################################
numeric_days <- function(vec)
{
  nd <- rep(0,length(vec))
  for(i in 1:length(vec))
  {
    nd[i] <- convert_day_to_angle(vec[i])
  }
  return(nd)
}
#############################################################
subsequent_day <- function(day)
{
  if(day == "dom") return("lun")
  else if (day == "lun") return("mar")
  else if (day == "mar") return("mer")
  else if (day == "mer") return("gio")
  else if (day == "gio") return("ven")
  else if (day == "ven") return("sab")
  else return("dom")
}
#############################################################
compute_day_at <- function(today, day_ahead)
{
  days <- c("dom","lun","mar","mer","gio","ven","sab")
  ang <- which(days == today)
  n <- ((ang+day_ahead) %% 7) 
  if(n == 0) n <- 7
  return(days[n])
}
##############################################################
add_days <- function(first_day, year)
{
  dl <- c()
  if(year %% 4 == 0)
  {
    day2 <- subsequent_day(first_day)
    day3 <- subsequent_day(day2)
    day4 <- subsequent_day(day3)
    day5 <- subsequent_day(day4)
    day6 <- subsequent_day(day5)
    day7 <- subsequent_day(day6)
    week <- c(rep(first_day, 24),rep(day2, 24),rep(day3, 24),rep(day4, 24),
              rep(day5, 24),rep(day6, 24),rep(day7, 24))
    nw <- trunc(366/7)
    wr <- 366 - nw*7
    rdl <- c()
    for(i in 1:24*wr)
    {
      rdl <- c(rdl, week[i])
    }
    dl <- c(rep(week,nw),rdl)
  }
  else
  {
    day2 <- subsequent_day(first_day)
    day3 <- subsequent_day(day2)
    day4 <- subsequent_day(day3)
    day5 <- subsequent_day(day4)
    day6 <- subsequent_day(day5)
    day7 <- subsequent_day(day6)
    week <- c(rep(first_day, 24),rep(day2, 24),rep(day3, 24),rep(day4, 24),
              rep(day5, 24),rep(day6, 24),rep(day7, 24))
    nw <- trunc(365/7)
    wr <- 365 - nw*7
    rdl <- c()
    for(i in 1:24*wr)
    {
      rdl <- c(rdl, week[i])
    }
    dl <- c(rep(week,nw),rdl)
  }
  return(dl)
}
###################################################################################
dates <- function(vec)
{
  vec <- unlist(as.character(vec))
  dt <- rep(0, length(vec))
  for(i in 1:length(vec))
  {
    dt[i] <- paste0(stri_sub(vec[i],from = 7,to = 8),"/",stri_sub(vec[i],from = 5,to = 6),"/",stri_sub(vec[i],from = 1,to = 4))
  }
  return(dt)
}
#################################################################################
add_holidays <- function(vd)
{
  ##### codifica numerica delle vacanze
  ## 1 Gennaio = 1, Epifania = 2
  ## Pasqua = 3, Pasquetta = 4
  ## 25 Aprile = 5, 1 Maggio = 6, 2 Giugno = 7,
  ## Ferragosto = 8, 1 Novembre = 9
  ## 8 Dicembre = 10, Natale = 11, S.Stefano = 12, S.Silvestro = 13
  holidays <- rep(0,length(vd))
  pasqua <- c("04/04/2010", "24/04/2011", "08/04/2012", "31/03/2013", "20/04/2014", "05/04/2015", "27/03/2016")
  pasquetta <- c("05/04/2010", "25/04/2011", "09/04/2012", "01/04/2013", "21/04/2014", "06/04/2015", "28/03/2016")
  for(i in 1:length(vd))
  {
    if(stri_sub(vd[i],from=1,to=5) == "01/01") holidays[i] <- 1
    else if(stri_sub(vd[i],from=1,to=5) == "06/01") holidays[i] <- 2
    else if(stri_sub(vd[i],from=1,to=5) == "25/04") holidays[i] <- 5
    else if(stri_sub(vd[i],from=1,to=5) == "01/05") holidays[i] <- 6
    else if(stri_sub(vd[i],from=1,to=5) == "02/06") holidays[i] <- 7
    else if(stri_sub(vd[i],from=1,to=5) == "15/08") holidays[i] <- 8
    else if(stri_sub(vd[i],from=1,to=5) == "01/11") holidays[i] <- 9
    else if(stri_sub(vd[i],from=1,to=5) == "08/12") holidays[i] <- 10
    else if(stri_sub(vd[i],from=1,to=5) == "25/12") holidays[i] <- 11
    else if(stri_sub(vd[i],from=1,to=5) == "26/12") holidays[i] <- 12
    else if(stri_sub(vd[i],from=1,to=5) == "31/12") holidays[i] <- 13
    else if(vd[i] %in% pasqua) holidays[i] <- 3
    else if(vd[i] %in% pasquetta) holidays[i] <- 4
  }
  return(holidays)
}
#########################################################################################
data_successiva <- function(cdate)
{
  d <- unlist(strsplit(cdate, "/"))
  
  day <- as.numeric(d[1])
  month <- as.numeric(d[2])
  year <- as.numeric(d[3])
  
  leap_year <- year %% 4
  
  new_day <- 0
  new_month <- month
  new_year <- year
  
  df31 <- c(1,2,4,6,8,9,11)
  df30 <- c(5,7,10,12)
  
  if(month %in% df31 & day == 31) {new_day <- 1; new_month <- month + 1}
  else if(month %in% df30 & day == 30) {new_day <- 1; new_month <- month + 1}
  else if(month == 2 & leap_year != 0 & day == 28) {new_day <- 1; new_month <- month + 1}
  else if(month == 2 & leap_year == 0 & day == 29) {new_day <- 1; new_month <- month + 1}
  else new_day <- day + 1
  
  if(new_month == 13) new_month <- 1
  
  if(month == 12 & day == 31) new_year <- year + 1
  
  if(new_month < 10) new_month <- paste0("0", new_month)
  
  if(new_day < 10) new_day <- paste0("0", new_day)
  
  new_date <- paste0(new_day, "/", new_month, "/", new_year)
  
  return(new_date)
}
#########################################################################################
associate_meteo_ora <- function(data, meteo, meteovar)
{
  vm <- rep(0, length(data))
  for(d in 1:length(data))
  {
    ir <- which(meteo["Data"] == data[d])
    vm[d] <- as.numeric(meteo[ir,meteovar])
    
  }
  return(vm)
}
#########################################################################################
trova_date_mancanti <- function(date, meteo)
{
  dm <- c()
  for(d in date)
  {
    if(length( which(meteo[,1] == d) ) == 0 ) dm <- c(dm,d)
  }
  return(dm)
}
#########################################################################################
associate_days <- function(ora, day)
{
  vdays <- rep(day, 24)
  for(i in 1:(length(ora)-1) )
  {
    if(ora[i] == 24 & ora[i+1] == 1 | ora[i] == 23 & ora[i+1] == 1 | ora[i] == 25 & ora[i+1] == 1) 
    {
      index <- i+1
      for(j in index:24) vdays[j] <- subsequent_day(day)
    }
  }
  return(vdays)
}
########################################################################################
create_dataset <- function(pun, first_day)
{
  d_f <- data_frame()
  Names <- c(paste0("pun-",24:1), paste0("aust-",24:1), paste0("cors-",24:1), paste0("fran-",24:1), paste0("grec-",24:1),
             paste0("slov-",24:1), paste0("sviz-",24:1), paste0("angleday-",24:1), paste0("holiday-",24:1), "y",paste0("day-",24:1))
  for(i in 1:(nrow(pun)-23))
  {
    #print(i)
    y <- p <- aus <- cors <- fran <- grec <- slov <- sviz <- ora <- dat <- c()
    for(j in i:(i+23))
    {
      p <- c(p, pun[j,"PUN"]); aus <- c(aus, pun[j,"AUST"]); cors <- c(cors, pun[j,"CORS"])
      fran <- c(fran, pun[j,"FRAN"]); grec <- c(grec, pun[j,"GREC"]); slov <- c(slov, pun[j,"SLOV"])
      sviz <- c(sviz, pun[j,"SVIZ"]); ora <- c(ora, pun[j,"Ora"]); dat <- c(dat, pun[j,"Data/Date"]) 
    }
    y <- c(y, pun[(i+24),"PUN"])
    day <- unlist(ifelse(nrow(d_f) > 0, d_f[nrow(d_f),ncol(d_f)], first_day))
    #print(day)
    ds <- dates(dat)
    hol <- add_holidays(ds)
    vdays <- associate_days(ora, day)
    vdays2 <- maply(1:24, function(n) as.character(vdays[n]))
    aday <- maply(1:24, function(n) convert_day_to_angle(vdays2[n]))
    
    adf <- data.frame(t(p), t(aus), t(cors), t(fran), t(grec), t(slov), t(sviz), t(aday), t(hol), y, t(vdays), stringsAsFactors = FALSE)
    colnames(adf) <- Names
    
    d_f <- bind_rows(d_f, adf)
#    d_f %>% map_if(is.factor, as.character) -> d_f
  }
  colnames(d_f) <- Names
  return(d_f)
}
######################################################
future_hour_with_step <- function(ora, step)
{
  now_1 <- 12 + (24/pi)*asin(ora) ## ora input
  now <- (now_1 %% 24) + 1 ## ora corrente
  targeth <- 0 ## ora futura
  if((now + step) <= 24) targeth <- now+step
  else targeth <- now+step - 24
  
  #print(targeth)
  return(convert_hour_to_angle(targeth))
}
######################################################
future_date_at_step <- function(current_date, ora, step)
{
  new_date <- 0
  now_1 <- 12 + (24/pi)*asin(ora) ## ora input
  now <- (now_1 %% 24) + 1 ## ora corrente

  if((now + step) < 24) new_date <- current_date
  else new_date <- data_successiva(current_date)
  
  return(new_date)
}
######################################################
new_day_at_step <- function(current_date, ora, step, day)
{
  new_day <- 0
  new_date <- future_date_at_step(current_date,ora,step)
  
  if(new_date != current_date) new_day <- subsequent_day(day)
  else new_day <- day
  
  return(new_day)
}
######################################################
variables_at_step <- function(ora,day,current_date,step)
{
  new_hour <- future_hour_with_step(ora,step)
  new_day <- convert_day_to_angle(new_day_at_step(current_date,ora,step,day))
  new_date <- future_date_at_step(current_date,ora,step)
  new_hol <- add_holidays(new_date)
  
  return(c(new_hour,new_day,new_hol))
}
#####################################################
augmented_dataset <- function(train1, train2, step, day_ahead)
{
  colnames(train1)[1:2] <- colnames(train2)[1:2] <- c("Data", "ora")
  variables1 <- colnames(train1)
  variables2 <- colnames(train2)
  common <- intersect(variables1, variables2)
  
  ns <- step + 24*day_ahead
  
  train <- rbind(train1[,which(colnames(train1) %in% common)], train2[1:ns,which(colnames(train2) %in% common)])
  
  return(train)
}
#####################################################
create_dataset23 <- function(pun, first_day, varn, meteo, step)
{
  ## step starts from 0, in which case the model predicts the hour after the predictors provided 
  ## and goes to 24, which is the same hour the day after
  d_f <- data_frame()
  Names <- c(paste0(varn,"-",23:1), paste0("aust-",23:1), paste0("cors-",23:1), paste0("fran-",23:1), paste0("grec-",23:1),
             paste0("slov-",23:1), paste0("sviz-",23:1), paste0("angleday-",23:1), paste0("holiday-",23:1), "y",
             paste0("angleora-",23:1),
             paste0("tmin-",23:1), paste0("tmax-",23:1), paste0("tmed-",23:1), paste0("rain-",23:1), paste0("vento-",23:1),
             "target_ora", "target_day", "target_holiday",
             paste0("day-",23:1))
  
  for(i in 1:(nrow(pun)-(23+step)))
  {
    #print(i)
    y <- p <- aus <- cors <- fran <- grec <- slov <- sviz <- ora <- dat <- c()
    for(j in i:(i+22))
    {
      p <- c(p, pun[j,varn]); aus <- c(aus, pun[j,"AUST"]); cors <- c(cors, pun[j,"CORS"])
      fran <- c(fran, pun[j,"FRAN"]); grec <- c(grec, pun[j,"GREC"]); slov <- c(slov, pun[j,"SLOV"])
      sviz <- c(sviz, pun[j,"SVIZ"]); ora <- c(ora, pun[j,2]); dat <- c(dat, pun[j,1]) 
    }
    y <- c(y, pun[(i+23+step),varn])
    
    day <- unlist(ifelse(nrow(d_f) > 0, d_f[nrow(d_f),ncol(d_f)], first_day))
    #print(day)
    ds <- dates(dat)
    hol <- add_holidays(ds)
    vdays <- associate_days(ora, day)
    vdays2 <- maply(1:24, function(n) as.character(vdays[n]))
    aday <- maply(1:24, function(n) convert_day_to_angle(vdays2[n]))
    ahour <- convert_hour_to_angle(ora)
    
    targets <- variables_at_step(ahour[23], day, ds[23], step)
    
    ## togli vdays e metti variabili meteo
    tmin <- associate_meteo_ora(ds, meteo, "Tmin")
    tmax <- associate_meteo_ora(ds, meteo, "Tmax")
    tmed <- associate_meteo_ora(ds, meteo, "Tmedia")
    rain <- associate_meteo_ora(ds, meteo, "Pioggia")
    vm <- associate_meteo_ora(ds, meteo, "Vento_media")
    ### transpose the vectors as they are column vectors in R
    adf <- data.frame(t(p), t(aus), t(cors), t(fran), t(grec), t(slov), t(sviz), t(aday[1:23]), t(hol[1:23]), y, t(ahour[1:23]),
                      t(tmin[1:23]), t(tmax[1:23]), t(tmed[1:23]), t(rain[1:23]), t(vm[1:23]), targets[1],targets[2],targets[3], t(vdays[1:23]), stringsAsFactors = FALSE)
    colnames(adf) <- Names
    
    d_f <- bind_rows(d_f, adf)
  }
  colnames(d_f) <- Names
  return(d_f[,1:349])
}
######################################################
sign_process <- function(Pt)
{
  St <- rep(0, (length(Pt)-1))
  for(i in 1:length(St)) {print(i);St[i] <- sign(Pt[i+1] - Pt[i])}
  
  return(St)
}
######################################################
percentage_greater_than <- function(x, p)
{
  return(length(x[x > p])/length(x))
}
#####################################################
RMSE <- function(x)
{
  return(sqrt(mean(x^2)))
}
#####################################################
visualise_results <- function(dl.model, nd, ndh20)
{
  plot(dl.model)

  pred <- predict(dl.model, ndh20)
  
  pt <- as.numeric(pred$predict) 
  pt <- as.matrix(pt)
  pt <- unlist(pt[,1])
  
  plot(pt, type="l",col="blue", xlab="time", ylab="euro/MWh", main="CSUD15 calcolato vs vero")
  lines(unlist(nd["y"]), type="o",col="red")
  
  yy <- unlist(nd["y"])
  diff <- yy - pt
  print(paste0("mean of errors: ", mean(diff)))
  print(paste0("standard deviation of errors: ", sd(diff)))
  print(paste0("median of errors: ", median(diff)))
  
  
  plot(density(diff),main="distribuzione degli errori")
  hist(diff,freq = FALSE, add=TRUE)
  
  apdiff <- abs(diff)/yy
  
  for(p in c(1:10)/10) print(percentage_greater_than(apdiff,p))
  
  std_diff <- (diff - mean(diff))/sd(diff)
  ssdd <- sample(std_diff, size = 5000)
  
  print(shapiro.test(ssdd))
  qqnorm(diff)
  lines(seq(-20,20,0.0001),seq(-20,20,0.0001),type="l",col="red")
  
  print(paste("correlation target price with errors:", cor(yy,diff))) 
  cor(yy,apdiff) ## <- almost independent
  
  plot(dlts <- stl(ts(pt,frequency=24),s.window="periodic"),col="blue",main="serie stimata")
  plot(se <- stl(ts(unlist(nd["y"]),frequency=24),s.window="periodic"),col="red",main="serie vera")
  #dlts11$time.series
  
  min_season <- dlts$time.series[1:24,1]
  min_season_orig15 <- se$time.series[1:24,1]
  par(mfrow = c(2,1))
  plot(min_season, type="l", col="blue")
  plot(min_season_orig15, type= "o", col="red")
  
  dl.trend <- unlist(dlts$time.series[,2])
  se.trend <- unlist(se$time.series[,2])
  
  plot(dl.trend, type="l", col="blue")
  plot(se.trend, type= "l", col="red")
  
  print(paste("RMSE trend:", RMSE(dl.trend - se.trend)))
  print(paste("RMSE total:", RMSE(pt - unlist(nd["y"]))))
}
####################################################################
prepare_csv <- function(meteo_str)
{
  meteo <- read.delim2(paste0("C:/Users/utente/Documents/PUN/",meteo_str,".csv"), header=FALSE, row.names=NULL,sep=",",colClasses = "character", stringsAsFactors = FALSE)
  n <- nrow(meteo)
  write.table(n,paste0("C:/Users/utente/Documents/PUN/",meteo_str,"nrow.txt"))
  meteo <- meteo[2:nrow(meteo),]
  #date_ <- gsub("-", "/", unlist(meteo[,1]))
  
  duniq <- unique(unlist(meteo[,1]))
  ptmin <- ptmed <- ptmax <- prain <- pwind <- pdate <-c()
  for(du in duniq)
  {
    sub_meteo <- meteo[which(meteo[,1] == du),3:5]
    nrsb <- nrow(sub_meteo) 
    last_date <- 0
    if(nrsb == 24)
    {
      ptmin <- c(ptmin, min(as.numeric(sub_meteo[,1])))
      ptmax <- c(ptmax, max(as.numeric(sub_meteo[,1])))
      ptmed <- c(ptmed, mean(as.numeric(sub_meteo[,1])))
      prain <- c(prain, sum(as.numeric(sub_meteo[,3])))
      pwind <- c(pwind, mean(as.numeric(sub_meteo[,2])))
      dd <- unlist(strsplit(du, "-"))
      pdate <- c(pdate, paste0(dd[3],"/",dd[2],"/",dd[1]))
    }
    else
    {
      last_date <- nsrb 
    }
  }
  Date <- rep(pdate,c(rep(24,length(pdate)-1),last_date))
  tmin <- rep(ptmin,c(rep(24,length(pdate)-1),last_date))
  tmed <- rep(ptmed,c(rep(24,length(pdate)-1),last_date))
  tmax <- rep(ptmax,c(rep(24,length(pdate)-1),last_date))
  rain <- rep(prain,c(rep(24,length(pdate)-1),last_date))
  wind <- rep(pwind,c(rep(24,length(pdate)-1),last_date))
  #mdf <- data.frame(t(Date), t(tmin), t(tmed), t(tmax), t(rain), t(wind))
  mdf <- data.frame(Date, tmin, tmed, tmax, rain, wind)
  colnames(mdf) <- c("Data", "Tmin", "Tmedia", "Tmax", "Pioggia", "Vento_media")
  return(mdf)
}
#################################################################
prepare_meteo <- function(meteotxt, meteo_str)
{
  meteo_s <- prepare_csv(meteo_str)
  variables <- which(colnames(meteotxt) %in% c("Data", "Tmin", "Tmedia", "Tmax", "Pioggia", "Vento_media"))
  meteo_t <- meteotxt[,variables]
  
  #meteo <- data.frame(rbind(meteo_t,meteo_s))
  # data.table vignette
  # data.table 20.9695 times faster than rbind --> https://cran.r-project.org/web/packages/data.table/index.html
  ll <- list(meteo_t,meteo_s)
  meteo <- rbindlist(ll,use.names = TRUE)
  return(meteo)
}
####################################################################
generate_ids <- function(a,h,s)
{
  ids <- c()
  for(i in 1:length(a))
  {
    for(j in 1:length(h))
    {
      for(k in 1:length(s))
      {
        ids <- c(ids, paste0(i,j,k))
      }
    }
  }
 return(ids) 
}
##################################################################
learn_model <- function(predictors, response, trainset, id, testset, s, a, h, se.trend, y,unbounded)
{
  if(a %in% unbounded)
  {
    dl_id <- h2o.deeplearning(predictors, response, training_frame = trainset,model_id = id, validation_frame = testset, standardize = s, activation = a,
                              hidden = h, epochs = 100, max_w2 = 100, l1=1e-5)
  }
  else
  {
    dl_id <- h2o.deeplearning(predictors, response, training_frame = trainset,model_id = id, validation_frame = testset, standardize = s, activation = a,
                              hidden = h, epochs = 100)
  }
  
  pred <- predict(dl_id,testset)
  
  pt <- as.numeric(pred$predict) 
  pt <- as.matrix(pt)
  pt <- unlist(pt[,1])
  
  dlts <- stl(ts(pt,frequency=24),s.window="periodic")
  
  
  min_season <- dlts$time.series[1:24,1]
  
  
  dl.trend <- unlist(dlts$time.series[,2])
  
  
  RMSE_trend <- RMSE(dl.trend - se.trend)
  RMSE_total <- RMSE(pt - y)
  
  return(c(h2o.r2(dl_id, train=TRUE, valid=TRUE),h2o.mse(dl_id, train=TRUE, valid=TRUE), RMSE_trend, RMSE_total))
  
}
##################################################################
learn_model_TC <- function(predictors, response, trainset, id, testset, s, a, h, se.trend, y, unbounded)
{
  out <- tryCatch(
    {
      learn_model(predictors, response, trainset, id, testset, s, a, h, se.trend, y, unbounded)
    },
    error = function(cond)
    {
      message(cond)
      #next
      return(c(0,0,0,0,0,0))
    }
  )
  return(out)
}
##################################################################
brute_force_tuning <- function(trainset,testset,a,h,s)
{
  response <- "y"
  predictors <- setdiff(names(trainset), response)
  unbounded <- c("Rectifier","RectifierWithDropout","Maxout","MaxoutWithDropout")
  ids <- generate_ids(a,h,s)
  
  t_s <- as.data.frame(testset)
  y <- unlist(t_s$y)
  
  se <- stl(ts(y,frequency=24),s.window="periodic")
  min_season_orig15 <- se$time.series[1:24,1]
  se.trend <- unlist(se$time.series[,2])
  
  models <- list()
  for(i in 1:length(a))
  {
    for(j in 1:length(h))
    {
      for(k in 1:length(s)) 
      {
        ir <- which(ids == paste0(i,j,k))
        id <- ids[ir]
        models[[id]] <- learn_model_TC(predictors,response,trainset, id, testset, s[k], a[i], h[[j]], se.trend, y, unbounded)
      }
    }
  }
  return(models)
}
########################################################################
generate_stepped_datasets <- function(prices1, prices2, prices3, meteo)
{
  for(step in 0:24)
  {
    train <- augmented_dataset(prices1, prices2, step)
    test <- augmented_dataset(prices2, prices3, step)
    
    trainset <- create_dataset23(train, "ven", "CSUD", meteocsud, step)
    testset <- create_dataset23(test, "sab", "CSUD", meteocsud, step)
    
    trainseth2o <- as.h2o(trainset)
    testseth2o <- as.h2o(testset)
    
    name1 <- paste0("C:\\Users\\utente\\Documents\\trainset_step_",step,".csv")
    name2 <- paste0("C:\\Users\\utente\\Documents\\testset_step_",step,".csv")  
    
    h2o.exportFile(trainseth2o, name1)
    h2o.exportFile(testseth2o, name2)
  }
}
########################################################################
brute_force_tuning_with_steps <- function(a,h,s,start,end)
{
  results <- data_frame()

  response <- "y"
  
  unbounded <- c("Rectifier","RectifierWithDropout","Maxout","MaxoutWithDropout")

  ids <- generate_ids(a,h,s)

  for(i in 1:length(a))
  {
    for(j in 1:length(h))
    {
      for(k in 1:length(s))
      {
        for(step in start:end)
        {
          nametrain <- paste0("C:\\Users\\utente\\Documents\\trainset_step_",step,".csv")
          nametest <- paste0("C:\\Users\\utente\\Documents\\testset_step_",step,".csv") 
          
          train <- h2o.importFile(nametrain)
          test <- h2o.importFile(nametest)
          
          predictors <- setdiff(names(train), response)
          
          t_s <- as.data.frame(test)
          y <- unlist(t_s$y)
          
          se <- stl(ts(y,frequency=24),s.window="periodic")
          
          se.trend <- unlist(se$time.series[,2])
          
          ir <- which(ids == paste0(i,j,k))
          id <- paste(ids[ir],step, sep="|")
          models <- learn_model_TC(predictors,response,train, id, test, s[k], a[i], h[[j]], se.trend, y, unbounded)
          #models <- data.frame(models)
          results <- bind_rows(results, as.data.frame(models))
          rownames(results)[nrow(results)] <- id
          rm(train); rm(test)
          print(paste("done step:", step))
        }
      }
    }
  }
  colnames(results) <- c("R2.train", "R2.test", "MSE.train", "MSE.test", "RMSE.trend", "RMSE.total")
  xlsx::write.xlsx(results, paste0("results_tuning",a[1],start,"_",end,".xlsx"), row.names=TRUE, col.names = TRUE)
  return(data.frame(results))
}
########################################################################
compare_prediction_given_step <- function(dl.model, testseth2o, step)
{
  testset <- as.data.frame(testseth20)
  
  y <- testset["y"]
  y_s <- y[step:length(y)]
  
  pred <- predict(dl.model, testseth2o)
  
  pt <- as.numeric(pred$predict) 
  pt <- as.matrix(pt)
  pt <- unlist(pt[,1])
  
  
}
##########################################################################
tuning_with_grid <- function(a,h,s)
{
  ids <- generate_ids(a,h,s)
  
  response = "y"
  
  result <- data_frame()
  
  for(step in 0:24)
  {
    nametrain <- paste0("C:\\Users\\utente\\Documents\\trainset_step_",step,".csv")
    nametest <- paste0("C:\\Users\\utente\\Documents\\testset_step_",step,".csv") 
    
    train <- h2o.importFile(nametrain)
    test <- h2o.importFile(nametest)
    
    predictors <- setdiff(names(train), response)
    
    t_s <- as.data.frame(test)
    y <- unlist(t_s$y)
    
    se <- stl(ts(y,frequency=24),s.window="periodic")
    
    se.trend <- unlist(se$time.series[,2])
    
    ir <- which(ids == paste0(i,j,k))
    id <- paste(ids[ir],step, sep="|")
    
    out <- tryCatch(
      {
        grid <- h2o.grid("deeplearning", grid_id = id, x = predictors, y = response, training_frame = train, validation_frame = test, 
                         hyper_params = list(activation = a, hidden = h, standardize = s, epochs = 100))
        model_ids <- grid@model_ids
        models <- lapply(model_ids, function(id) { dl <- h2o.getModel(id); pred <- predict(dl,test);
        pt <- as.numeric(pred$predict);
        pt <- as.matrix(pt);
        pt <- unlist(pt[,1]);
        dlts <- stl(ts(pt,frequency=24),s.window="periodic");
        min_season <- dlts$time.series[1:24,1];
        dl.trend <- unlist(dlts$time.series[,2]);
        RMSE_trend <- RMSE(dl.trend - se.trend);
        RMSE_total <- RMSE(pt - y);
        result <- bind_rows(result, data.frame(id, h2o.r2(dl, train=TRUE, valid=TRUE),h2o.mse(dl, train=TRUE, valid=TRUE), RMSE_trend, RMSE_total));
        })
      }, error = function(cond)
      {
        message(cond)
        result <- bind_rows(result, data.frame(id, 0,0,0,0,0,0));
      }
    )
  }
  colnames(result) <- c("R2.train", "R2.test", "MSE.train", "MSE.test", "RMSE.trend", "RMSE.total")
  xlsx::write.xlsx(result, paste0("results_tuning",a[1],".xlsx"), row.names=TRUE, col.names = TRUE)
  return(data.frame(result))
}
#############################################################################
prepare_dataset_for_prediction <- function(path, path_meteo, varn, step)
{
  Names <- c(paste0(varn,"-",23:1), paste0("aust-",23:1), paste0("cors-",23:1), paste0("fran-",23:1), paste0("grec-",23:1),
             paste0("slov-",23:1), paste0("sviz-",23:1), paste0("angleday-",23:1), paste0("holiday-",23:1),
             paste0("angleora-",23:1),
             paste0("tmin-",23:1), paste0("tmax-",23:1), paste0("tmed-",23:1), paste0("rain-",23:1), paste0("vento-",23:1),
             "target_ora", "target_day", "target_holiday")
  
  pre_path <- "C:/Users/utente/Documents/PUN/"#test_pun.xlsx"
  tp <- openxlsx::read.xlsx(path, sheet="DB Dati", colNames=TRUE)
  db <- tp[(nrow(tp)-23):nrow(tp),]
  
  colnames(db) <- c("Date","Year","Quarter","Month","Week","Week.Day","Day","Hour","Lavorativo/Festivo",
                    "Weekend","PEAK-OFF.PEAK","AEEG","PUN","AUST","BRNN","BSP","CNOR","COAC",  
                    "CORS","CSUD","FOGN","FRAN","GREC","MFTV","NORD","PRGP","ROSN",  
                    "SARD","SICI","SLOV","SUD","SVIZ")
  
  d <- as.Date(db[1,1], origin="1899-12-31")
  dt <- unlist(strsplit(as.character(d, "-")))
  dt <- paste0(dt[3],"/",dt[2],"/",dt[1])
  
  day <- tolower(db[1,"Week.Day"])
  angleday <- convert_day_to_angle(day)
  hours <- as.numeric(db["Hour"])
  angle_hours <- convert_hour_to_angle(hours)
  
  hol <- add_holidays(dt)
  
  y_varn <- as.numeric(db["PUN"])
  aust <- as.numeric(db["AUST"])
  cors <- as.numeric(db["CORS"])
  fran <- as.numeric(db["FRAN"])
  grec <- as.numeric(db["GREC"])
  slov <- as.numeric(db["SLOV"])
  sviz <- as.numeric(db["SVIZ"])
  
  meteo <- read.csv2(paste0(pre_path, path_meteo, ".csv"), header = TRUE, sep = ",", colClasses = "character", stringsAsFactors = FALSE)
  dm <- meteo[nrow(meteo),1]
  ddm <- unlist(strsplit(as.character(dm, "-")))
  ddm <- paste0(ddm[3],"/",ddm[2],"/",ddm[1])
  
  meteo2 <- meteo[which(meteo[,1] == dm),]
  tmin <- min(as.numeric(meteo2[,3]))
  tmax <- max(as.numeric(meteo2[,3]))
  tmed <- mean(as.numeric(meteo2[,3]))
  rain <- sum(as.numeric(meteo2[,5]))
  vm <- mean(as.numeric(meteo2[,4]))
  
  target_day <- convert_day_to_angle(subsequent_day(day))
  target_holiday <- add_holidays(target_day)
  target_hour <- convert_hour_to_angle(step)
  
  df <- data.frame(t(y_varn), t(aust), t(cors), t(fran), t(grec), t(slov), t(sviz), t(rep(angleday,24)), t(rep(hol,24)), t(angle_hours), 
                   t(rep(tmin,24)), t(rep(tmax,24)), t(rep(tmed,24)), t(rep(rain,24)), t(rep(vm,24)), target_hour, target_day, target_holiday)
  
  colnames(df) <- Names
  return(df)
}
#################################################################
mediate_meteos <- function(mi,ro,fi,pa,ca,rc,flag_2016)
{
  df <- data_frame()
  vars <- c("tmin", "tmax", "tmedia", "pioggia", "vento_media")
  if(flag_2016) vars[5] <- "ventomedia"
  for(i in 1:nrow(fi))
  {
    dt <- fi[i,which(tolower(colnames(fi)) == "data")]
    imi <- which(mi[,which(tolower(colnames(mi)) == "data")] == dt)
    iro <- which(ro[,which(tolower(colnames(ro)) == "data")] == dt)
    ipa <- which(pa[,which(tolower(colnames(pa)) == "data")] == dt)
    ica <- which(ca[,which(tolower(colnames(ca)) == "data")] == dt)
    irc <- which(rc[,which(tolower(colnames(rc)) == "data")] == dt)
    
    cv <- rep(0, length(vars))
    
    for(v in vars)
    {
      j <- which(vars == v)
      cv[j] <- mean( c(mi[imi, which(tolower(colnames(mi)) == v)],
                        fi[i, which(tolower(colnames(fi)) == v)],
                        ro[iro, which(tolower(colnames(ro)) == v)],
                        pa[ipa, which(tolower(colnames(pa)) == v)],
                        ca[ica, which(tolower(colnames(ca)) == v)],
                        rc[irc, which(tolower(colnames(rc)) == v)]),na.rm=TRUE)
    }
    df <- bind_rows(df, data.frame(as.character(dt),t(cv)))
  }
  colnames(df) <- c("Data", "Tmin", "Tmax", "Tmedia", "Pioggia", "Vento_media")
  return(df)
}

