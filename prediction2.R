##### script for prediction --- FIXED ONLY ---

library(h2o)
library(readxl)
library(lubridate)

### all variables called date are of the type used by lubridate 
### and excel

## source


build_meteo_new <- function(date)
{
  ## call all meteo datasets for all cities
  ## mediate_meteos
  ## store the results in a matrix with each row being a day
  ## and variables: (tmin, tmax, tmed, rain, wind)
  ## both now and future:
  # 1: tmin.now, tmax.now, tmed.now, rain.now, wind.now
  # 2: tmin.+1, tmax.+1, tmed.+1, rain.+1, wind.+1
  # 3: tmin.+2, tmax.+2, tmed.+2, rain.+2, wind.+2
  #
  res <- matrix(6, nrow = 5, ncol= 5)
  
  var_names <- c("mi","ro","fi","ca","pa","rc")
  
  mi <- read.csv2("C:\\Users\\utente\\Documents\\PUN\\Milano.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  ro <- read.csv2("C:\\Users\\utente\\Documents\\PUN\\Roma.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  fi <- read.csv2("C:\\Users\\utente\\Documents\\PUN\\Firenze.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  ca <- read.csv2("C:\\Users\\utente\\Documents\\PUN\\Cagliari.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  pa <- read.csv2("C:\\Users\\utente\\Documents\\PUN\\Palermo.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  rc <- read.csv2("C:\\Users\\utente\\Documents\\PUN\\Reggio Calabria.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  
  dt <- as.Date(date)
  for(i in 0:5)
  {
    temp <- matrix(0,nrow=5,ncol=5)
    for(n in var_names)
    {
      df <- get(n)
      at <- which(as.Date(df[,1]) == dt)
      df2 <- df[at,3:ncol(df)]
      
      r <- which(var_names == n)
      temp[r,1] <- min(unlist(df2[,1+(i*3)]))
      temp[r,2] <- max(unlist(df2[,1+(i*3)]))
      temp[r,3] <- mean(unlist(df2[,1+(i*3)]))
      temp[r,4] <- mean(unlist(df2[,3+(i*3)])) ### RAIN
      temp[r,5] <- mean(unlist(df2[,2+(i*3)])) ### WIND
    }
    res[i+1,] <- c(unlist(colMeans(temp)))
  }
  res <- data.frame(res)
  colnames(res) <- c("tmin", "tmax", "tmed", "pioggia", "vento")
  
  return(res)
}
#######################################################
build_new <- function(df)
{
  ## put the prices in rows
  ## compute old and new holidays
  ## old and new angleday 
  ## all the "new" variables are in last columns
  dt <- Sys.Date() + 1
  oggi <- which(as.Date(df[,1]) == dt)
  dft <- df[oggi,]
  aday1 <- convert_day_to_anfle(subsequent_day(tolower(as.character(dft[1,6]))))
  
  tda <- unlist(strsplit(as.character(dt),"-"))
  target_data <- paste0(dt[3],"/",dt[2],"/",dt[1])
  hol <- add_holidays(target_data)
  ahour <- convert_hour_to_angle(1:24)
  
  len <- nrow(dft)
  
  if(len == 23)
  {
    df2 <- data.frame(t(dft[c(1:23,23),14]), t(dft[c(1:23,23),15]), t(dft[c(1:23,23),20]), t(dft[c(1:23,23),23]), t(dft[c(1:23,23),24]),
                      t(dft[c(1:23,23),31]), t(dft[c(1:23,23),33]),aday1, hol, t(ahour))
    
  }
  
  else if(len == 25)
  {
    df2 <- data.frame(t(dft[c(1:24),14]), t(dft[c(1:24),15]), t(dft[c(1:24),20]), t(dft[c(1:24),23]), t(dft[c(1:24),24]),
                      t(dft[c(1:24),31]), t(dft[c(1:24),33]),aday1, hol, t(ahour))
    
  }
  
  else 
  {
    df2 <- data.frame(t(dft[,14]), t(dft[,15]), t(dft[,20]), t(dft[,23]), t(dft[,24]), t(dft[,31]), t(dft[,33]),aday1, hol, t(ahour))
  }
  Names <- c(paste0("pun-",24:1), paste0("aust-",24:1), paste0("cors-",24:1), paste0("fran-",24:1), paste0("grec-",24:1),
             paste0("slov-",24:1), paste0("sviz-",24:1),"angleday","holiday",paste0("anglehour-",24:1))
  colnames(df2) <- Names
  
  return(df2)
  
}
#######################################################
assemble_pm <- function(pn, meteo)
{
  res <- matrix(0,nrow=5*24,ncol=207)
  # build rows by column names
  for(i in 1:120)
  {
    step_p <- ifelse(i %% 24 == 0, 24, i %% 24)
    step_m <- ifelse(i %% 5 == 0, 5, i %% 5)
    
    tday <- convert_day_to_angle(compute_day_at(day, step_m))
    
    dt <- Sys.Date() + 1 + step_m
    
    tda <- unlist(strsplit(as.character(dt),"-"))
    target_data <- paste0(dt[3],"/",dt[2],"/",dt[1])
    hol <- add_holidays(target_data)
    
    
    row <- c(unlist(pn),unlist(meteo[1,]), convert_hour_to_angle(step_p),tday,thol,unlist(meteo[1+step_m,]))
    
    res[i,] <- row
  }
  
  res <- data.frame(res)
  
  Names <- c(paste0(varn,"-",24:1), paste0("aust-",24:1), paste0("cors-",24:1), paste0("fran-",24:1), paste0("grec-",24:1),
             paste0("slov-",24:1), paste0("sviz-",24:1), "angleday", "holiday",
             paste0("angleora-",24:1),
             "tmin","tmax","tmed","pioggia","vento",
             "target_ora", "target_day", "target_holiday","target_tmin","target_tmax","target_tmed","target_pioggia","target_vento")
  
  colnames(res) <- Names
  
  return(res)
}
#######################################################
prediction <- function(path, date, meteo)
{
  res <- matrix(0, nrow=24, ncol=5)
  ## load pun file
  pp <- read_excel("DB_Borse_Elettriche_PER MI.xlsx", sheet = "DB_Dati")
  ## look for date
  ppnew <- pp[which(pp[,1] == as.Date(date))]
  ## build "new observation"
  meteonew <- build_meteo_new(date)
  pn <- build_new(ppnew)
  xnew <- as.h2o(assemble_pm(pn, meteonew))
  ## call all models and make predictions
  for(da in 1:5)
  {
    for(step in 1:24)
    {
      id <- paste0("sda",step,"_",da)
      model <- h2o.loadModel(paste0("C:\\Users\\utente\\Documents\\PUN\\fixed\\models",id))
      x <- predict(model,xnew[da,])
      x2 <- as.numeric(x$predict)
      x3 <- as.matrix(x2)
      x4 <- unlist(x3[,1])
      res[step,da] <- x4
    }
  }
  return(res)
}