### functions for PUN
library(openxlsx)
library(plyr)
library(dplyr)
library(reshape)
library(stringi)
library(xlsx)
library(vioplot)
library(fda)
#library(h2o)
library(TSA)
library(tseries)

#localH2O <- h2o.init()
library(TDA)
library(purrr)

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
  return(sin(ora*pi/24))
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
  vec <- as.character(vec)
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
data_successiva <- function(date)
{
  d <- strsplit(date, "/")
  
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
    if(ora[i] == 24 & ora[i+1] == 1) 
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
create_dataset23 <- function(pun, first_day, varn, meteo)
{
  d_f <- data_frame()
  Names <- c(paste0(varn,"-",23:1), paste0("aust-",23:1), paste0("cors-",23:1), paste0("fran-",23:1), paste0("grec-",23:1),
             paste0("slov-",23:1), paste0("sviz-",23:1), paste0("angleday-",23:1), paste0("holiday-",23:1), "y",
             paste0("angleora-",23:1),
             paste0("tmin-",23:1), paste0("tmax-",23:1), paste0("tmed-",23:1), paste0("rain-",23:1), paste0("vento-",23:1), 
             paste0("day-",23:1))
  for(i in 1:(nrow(pun)-23))
  {
    #print(i)
    y <- p <- aus <- cors <- fran <- grec <- slov <- sviz <- ora <- dat <- c()
    for(j in i:(i+22))
    {
      p <- c(p, pun[j,varn]); aus <- c(aus, pun[j,"AUST"]); cors <- c(cors, pun[j,"CORS"])
      fran <- c(fran, pun[j,"FRAN"]); grec <- c(grec, pun[j,"GREC"]); slov <- c(slov, pun[j,"SLOV"])
      sviz <- c(sviz, pun[j,"SVIZ"]); ora <- c(ora, pun[j,2]); dat <- c(dat, pun[j,1]) 
    }
    y <- c(y, pun[(i+23),varn])
    day <- unlist(ifelse(nrow(d_f) > 0, d_f[nrow(d_f),ncol(d_f)], first_day))
    #print(day)
    ds <- dates(dat)
    hol <- add_holidays(ds)
    vdays <- associate_days(ora, day)
    vdays2 <- maply(1:24, function(n) as.character(vdays[n]))
    aday <- maply(1:24, function(n) convert_day_to_angle(vdays2[n]))
    ahour <- convert_hour_to_angle(ora)
    ## togli vdays e metti variabili meteo
    tmin <- associate_meteo_ora(ds, meteo, "Tmin")
    tmax <- associate_meteo_ora(ds, meteo, "Tmax")
    tmed <- associate_meteo_ora(ds, meteo, "Tmedia")
    rain <- associate_meteo_ora(ds, meteo, "Pioggia")
    vm <- associate_meteo_ora(ds, meteo, "Vento_media")
    ### transpose the vectors as they are column vectors in R
    adf <- data.frame(t(p), t(aus), t(cors), t(fran), t(grec), t(slov), t(sviz), t(aday[1:23]), t(hol[1:23]), y, t(ahour[1:23]),
                      t(tmin[1:23]), t(tmax[1:23]), t(tmed[1:23]), t(rain[1:23]), t(vm[1:23]), t(vdays[1:23]), stringsAsFactors = FALSE)
    colnames(adf) <- Names
    
    d_f <- bind_rows(d_f, adf)
  }
  colnames(d_f) <- Names
  return(d_f[,1:346])
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