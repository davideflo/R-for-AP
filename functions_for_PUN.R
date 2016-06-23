### functions for PUN
library(openxlsx)
library(plyr)
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
sign_process <- function(Pt)
{
  St <- rep(0, (length(Pt)-1))
  for(i in 1:length(St)) {print(i);St[i] <- sign(Pt[i+1] - Pt[i])}
  
  return(St)
}



