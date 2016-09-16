###### prediction for glm ######

library(h2o)
library(readxl)
library(lubridate)
library(dplyr)

### all variables called date are of the type used by lubridate 
### and excel

## source
source("C://Users//utente//Documents//R_code//functions_for_PPIA_server.R")
source("C://Users//utente//Documents//R_code//functions_for_PUN_server.R")
source("C://Users//utente//Documents//prediction2.R")

#########################################################
build_new <- function(df, rh)
{
  ## put the prices in rows
  ## compute old and new holidays
  ## old and new angleday 
  ## all the "new" variables are in the last columns
  dt <- Sys.Date()
  
  untime <- maply(1:nrow(df), function(n) unlist(df[n,1]))
  
  #utc <- as.POSIXct(untime, format="%d/%m/%Y %H:%M:%S", origin = "1970-01-01", tz="UCT")
  utc <- as.POSIXct(untime, origin = "1970-01-01")
  
  
  #ad <- as.character(unlist(df[,1]))
  #add <- unlist(strsplit(ad, " "))
  #add
  lasty <- max(which(as.Date(utc) == (dt-1)))
  
  oggi <- which(as.Date(utc) == dt)
  dft <- df[c(lasty,oggi[1:(length(oggi)-1)]),]

  tda <- unlist(strsplit(as.character(dt),"-"))
  target_data <- paste0(tda[3],"/",tda[2],"/",tda[1])
  hol <- add_holidays(target_data)

  tda2 <- unlist(strsplit(as.character(dt+1),"-"))
  target_data2 <- paste0(tda2[3],"/",tda2[2],"/",tda2[1])
  hol2 <- add_holidays(target_data2)
    
  today <- convert_day_to_angle(convert_day(as.character(lubridate::wday(Sys.Date(), label = TRUE))))
  tomorrow <- convert_day_to_angle(convert_day(as.character(lubridate::wday(Sys.Date()+1, label = TRUE))))
  
  len <- nrow(dft)
  
  if(len == 23)
  {
    df2 <- data.frame(t(dft[c(1:23,23),13]), t(dft[c(1:23,23),14]), t(dft[c(1:23,23),19]), t(dft[c(1:23,23),22]), t(dft[c(1:23,23),23]),
                      t(dft[c(1:23,23),30]), t(dft[c(1:23,23),32]), hol)
    
  }
  
  else if(len == 25)
  {
    df2 <- data.frame(t(dft[c(1:24),13]), t(dft[c(1:24),14]), t(dft[c(1:24),19]), t(dft[c(1:24),22]), t(dft[c(1:24),23]),
                      t(dft[c(1:24),30]), t(dft[c(1:24),32]), hol)
    
  }
  
  else 
  {
    df2 <- data.frame(t(dft[,13]), t(dft[,14]), t(dft[,19]), t(dft[,22]), t(dft[,23]), t(dft[,30]), t(dft[,32]), hol)
  }
  
  values <- unlist(df2[1,(24*0:6) + rh])
  
  df3 <- data.frame(t(values), today, hol, tomorrow, thol)
  
  Names <- c("pun","aust","cors","fran","grec","slov","sviz","angleday","holiday","target_day", "target_holiday")
  colnames(df3) <- Names
  
  return(df3)
  
}
#######################################################
assemble_pm_glm <- function(pn, meteo)
{
  # "meteo" comes from build_meteo_new
  
  ### DEFINE day
  wd <- tolower(as.character(lubridate::wday(Sys.Date(), label = TRUE)))
  
  res <- data.frame(pn[1,1:9], meteo[1,], pn[1,10:11], meteo[2,])
  
  
  Names <- c("pun","aust","cors","fran","grec","slov","sviz","angleday", "holiday",
             "tmin","tmax","tmed","pioggia","vento",
             "target_day","target_holiday","target_tmin","target_tmax","target_tmed","target_pioggia","target_vento")
  
  colnames(res) <- Names
  
  return(res)
  
}
##########################################################
predict_with_glm <- function(date)
{
  res <- restr <- matrix(0, nrow = 24, ncol = 2)
  ## load pun file
  pp <- read_excel("C:\\Users\\utente\\Documents\\PUN\\DB_Borse_Elettriche_PER MI.xlsx", sheet = "DB_Dati")
  ## look for date
  #ppnew <- pp[which(pp[,1] == as.Date(date))]
  ## build "new observation"
  
  meteonew <- build_meteo_new(date)
  
  odie <- as.Date(date)
  untime <- maply(1:nrow(pp), function(n) unlist(pp[n,1]))
  utc <- as.POSIXct(untime, origin = "1970-01-01")
  lasty <- max(which(as.Date(utc) == (odie-1)))
  pun_oggi <- unlist(pp[c(lasty,which(as.Date(utc) == odie)),13])
  pun_oggi <- pun_oggi[1:24]
  ## call all models and make predictions
  
  for(rh in 1:24)
  {
    pn <- build_new(pp, rh)
    apm <- assemble_pm_glm(pn, meteonew)
    xnew <- as.h2o(apm)
    
    #print(paste("step:",step,"da:", da))
    id <- paste0("rhda",rh,"_",1)
    id2 <- paste0("bis_rhda",rh,"_",1)
    model <- h2o.loadModel(paste0("C:\\Users\\utente\\Documents\\PUN\\glm\\models\\",id))
    model2 <- h2o.loadModel(paste0("C:\\Users\\utente\\Documents\\PUN\\glm\\models\\",id2))
    
    x <- predict(model,xnew[1,])
    x2 <- as.numeric(x$predict)
    x3 <- as.matrix(x2)
    x4 <- unlist(x3[,1])
    yhat <- x4 
    res[rh,1] <- yhat
#    restr[, da] <- x4
    x <- predict(model2,xnew[1,])
    x2 <- as.numeric(x$predict)
    x3 <- as.matrix(x2)
    x4 <- unlist(x3[,1])
    yhat <- x4 
    res[rh,2] <- yhat
    
    h2o.rm(model); h2o.rm(model2)
  }

  res <- data.frame(res)
  rownames(res) <- 1:24
#  restr <- data.frame(restr)
#  rownames(restr) <- 1:24  

  names <- c(paste0("prediction_",as.character(odie+1),"modello_completo"),paste0("prediction_",as.character(odie+1),"modello2"))
  colnames(res) <- names
  # colnames(restr) <- names
  # xlsx::write.xlsx(restr,paste0("prediction_PUN_not_corrected_",date,".xlsx"), row.names = FALSE, col.names = TRUE)
  return(res)
}





