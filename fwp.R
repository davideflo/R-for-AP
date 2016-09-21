#### week prediction ####

library(readxl)
library(plyr)
library(dplyr)
library(lubridate)
library(h2o)

source("C://Users//utente//Documents//R_code//functions_for_PPIA_server.R")
source("C://Users//utente//Documents//R_code//functions_for_PUN_server.R")
source("C://Users//utente//Documents//glm_dataset.R")

##################################################################################
build_meteo_set <- function(date)
{
  dt <- as.Date(date)
  
  wtp <- find_next_monday(dt)
  
  var_names <- c("mi","ro","fi","ca","pa","rc")
  
  mets <- matrix(0, nrow = length(var_names), ncol = 5)
  
  mi <- read.csv2("C:\\Users\\utente\\Downloads\\Milano_set.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  ro <- read.csv2("C:\\Users\\utente\\Downloads\\Roma_set.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  fi <- read.csv2("C:\\Users\\utente\\Downloads\\Firenze_set.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  ca <- read.csv2("C:\\Users\\utente\\Downloads\\Cagliari_set.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  pa <- read.csv2("C:\\Users\\utente\\Downloads\\Palermo_set.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  rc <- read.csv2("C:\\Users\\utente\\Downloads\\Reggio Calabria_set.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  
  for(n in var_names)
  {
    df <- get(n)
    ix <- which(var_names == n)
    
    for(i in 0:6)
    {
      at <- which(as.Date(df[,1], format = "%Y-%m-%d") == wtp+i)
      df2 <- df[at,]
      
      mets[ix,1] <- mean(unlist(as.numeric(df2[,2])))
      mets[ix,2] <- mean(unlist(as.numeric(df2[,3])))
      mets[ix,3] <- (mets[ix,1] + mets[ix,2])/2
      mets[ix,4] <- mean(unlist(as.numeric(df2[,5])))
      mets[ix,5] <- mean(unlist(as.numeric(df2[,4])))
    }
  }
  
  return(colMeans(mets))
  
}
##################################################################################
build_meteo_week <- function(date)
{
  
  res <- matrix(0, nrow = 7, ncol= 5)
  
  var_names <- c("mi","ro","fi","ca","pa","rc")
  
  mi <- read.csv2("C:\\Users\\utente\\Downloads\\Milano.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  ro <- read.csv2("C:\\Users\\utente\\Downloads\\Roma.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  fi <- read.csv2("C:\\Users\\utente\\Downloads\\Firenze.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  ca <- read.csv2("C:\\Users\\utente\\Downloads\\Cagliari.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  pa <- read.csv2("C:\\Users\\utente\\Downloads\\Palermo.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  rc <- read.csv2("C:\\Users\\utente\\Downloads\\Reggio Calabria.csv", header = FALSE, sep=",", colClasses = "character", stringsAsFactors = FALSE)
  
  
  for(i in 0:6)
  {
    #print(i)
    temp <- matrix(0,nrow=6,ncol=5)
    dt <- as.Date(date) - i
    for(n in var_names)
    {
      df <- get(n)
      at <- which(as.Date(df[,1], format = "%Y-%m-%d") == dt)
      df2 <- df[at,]
      
      r <- which(var_names == n)
 
      row4 <- row5 <- c()
      for(k in 1:nrow(df2))
      {
        if(df2[k,5] == "")
        {
          row4 <- c(row4, k) 
        } else
        {
          row5 <- c(row5, k)
        }
      }
      
      temp[r,1] <- min(as.numeric(unlist(df2[row5,3])))
      temp[r,2] <- max(as.numeric(unlist(df2[row5,3])))
      temp[r,3] <- mean(as.numeric(unlist(df2[row5,3])))
      temp[r,4] <- mean(as.numeric(unlist(df2[row5,5])), na.rm=TRUE) ### RAIN
      temp[r,5] <- mean(as.numeric(unlist(df2[row5,4])), na.rm=TRUE) ### WIND
 
    }
    
    res[i+1,] <- c(unlist(colMeans(temp)))
  }
  res <- data.frame(res)
  colnames(res) <- c("tmin", "tmax", "tmed", "pioggia", "vento")
  
  return(res)
  
}
################################################################################
build_new_week <- function(pp)
{
  dt <- Sys.Date()
  
  untime <- maply(1:nrow(pp), function(n) unlist(pp[n,1]))
  utc <- as.POSIXct(untime, origin = "1970-01-01")
  
  date_range <- unique(as.Date(utc[which(as.Date(utc) >= (dt - 6) & as.Date(utc) <= dt)]))
  
  wtp <- find_next_monday(unique(date_range)[7])
  
  p <- aus <- cors <- fran <- grec <- slov <- sviz <- c()
  mese <- tmese <- c()
  
  hol <- thol <- 0
  
  for(dr in date_range)
  {
    p <- c(p, mean(unlist(pp[which(as.Date(utc) == dr),13]),na.rm = TRUE))
    aus <- c(aus, mean(unlist(pp[which(as.Date(utc) == dr),14]),na.rm = TRUE))
    cors <- c(cors, mean(unlist(pp[which(as.Date(utc) == dr),19]),na.rm = TRUE))
    fran <- c(fran, mean(unlist(pp[which(as.Date(utc) == dr),22]),na.rm = TRUE))
    grec <- c(grec, mean(unlist(pp[which(as.Date(utc) == dr),23]),na.rm = TRUE))
    slov <- c(slov, mean(unlist(pp[which(as.Date(utc) == dr),30]),na.rm = TRUE))
    sviz <- c(sviz, mean(unlist(pp[which(as.Date(utc) == dr),32]),na.rm = TRUE))
    
    tda <- unlist(strsplit(as.character(dr),"-"))
    target_data <- paste0(tda[3],"/",tda[2],"/",tda[1])
    hol <- hol + add_holidays(target_data)
    
    mese <- c(mese, lubridate::month(as.Date(dr, origin = "1970-01-01")))
    
    dt <- wtp + which(date_range == dr)
    
    tda2 <- unlist(strsplit(as.character(dt-1),"-"))
    target_data2 <- paste0(tda2[3],"/",tda2[2],"/",tda2[1])
    thol <- thol + add_holidays(target_data2)
    
    tmese <- c(tmese, lubridate::month(as.Date(dt-1, origin = "1970-01-01")))
  }
  
  df <- data.frame(t(p),t(aus),t(cors),t(fran),t(grec),t(slov),t(sviz),hol,mode(mese),
                   thol,mode(tmese),stringsAsFactors = FALSE)
  
  Names <- c(paste0("pun-",7:1), paste0("aust-",7:1), paste0("cors-",7:1), paste0("fran-",7:1), paste0("grec-",7:1),
             paste0("slov-",7:1), paste0("sviz-",7:1), "holiday", "mese",
             "target_holiday","target_mese")
  colnames(df) <- Names
  
  return(df)
}
#####################################################################################
assemble_pm_week <- function(pp, date)
{
  df <- build_new_week(pp)
  met <- build_meteo_week(date)
  mets <- build_meteo_set(date)
  
  res <- data.frame(df[1,1:51],t(rev(unlist(met[,1]))),t(rev(unlist(met[,2]))),t(rev(unlist(met[,3]))),t(rev(unlist(met[,4]))),t(rev(unlist(met[,5]))),df[1,52:53],
                    t(mets))
  
  colnames(res) <- c(paste0("pun-",7:1), paste0("aust-",7:1), paste0("cors-",7:1), paste0("fran-",7:1), paste0("grec-",7:1),
                     paste0("slov-",7:1), paste0("sviz-",7:1), "holiday", "mese",
                     paste0("tmin-",7:1),paste0("tmax-",7:1),paste0("tmed-",7:1),paste0("pioggia-",7:1),paste0("vento-",7:1),
                     "target_holiday","target_mese","target_tmin","target_tmax","target_tmed","target_pioggia","target_vento")
  
  return(res)
  
}
#####################################################################################
prediction_week <- function(date)
{
  
  pp <- read_excel("C:\\Users\\utente\\Documents\\PUN\\DB_Borse_Elettriche_PER MI.xlsx", sheet = "DB_Dati")
  
  apm <- assemble_pm_week(pp, date)
  
  odie <- as.Date(date)
  
  xnew <- as.h2o(apm)
      
      
  id <- "model_glm"
  model <- h2o.loadModel(paste0("C:\\Users\\utente\\Documents\\PUN\\week\\",id))
      
  x <- predict(model,xnew[1,])
  x2 <- as.numeric(x$predict)
  x3 <- as.matrix(x2)
  x4 <- unlist(x3[,1])
 
  
  id2 <- "model_glm_2016"
  model2 <- h2o.loadModel(paste0("C:\\Users\\utente\\Documents\\PUN\\week\\",id2))
  
  xx <- predict(model2,xnew[1,])
  xx2 <- as.numeric(xx$predict)
  xx3 <- as.matrix(xx2)
  xx4 <- unlist(xx3[,1])
  
  return(c(x4, xx4))
  
}




