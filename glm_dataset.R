##### glm model for prediction #####

library(plyr)
library(readxl)
library(lubridate)
library(h2o)
library(dplyr)

source("R_code//functions_for_PUN_server.R")
source("C://Users//utente//Documents//R_code//functions_for_PPIA_server.R")

###############################################################################################################################################
dates2 <- function(vd)
{
  asdd <- c()
  for(n in 1:length(vd))
  {
    d <- unlist(strsplit(vd[n],"/"))
    ##asdd <- maply(1:length(vd), function(n) as.Date(paste0(unlist(strsplit(vd[n],"/"))[3],"-",unlist(strsplit(vd[n],"/"))[2],"-",unlist(strsplit(vd[n],"/"))[1])) )
    asdd <- c(asdd,as.character(as.Date(paste0(d[3],"-",d[2],"-",d[1]))) )
  }
  return(asdd)
}
###############################################################################################################################################
create_glm_dataset <- function(pun, varn, regressorh, targeth, meteo, day_ahead)
{
  pun <- data.frame(pun)
  vanames <- c(varn,"aust","cors","fran","grec",
             "slov","sviz","angleday","holiday",
             "tmin","tmax", "tmed", "rain", "vento",
             "y","target_day", "target_holiday","target_tmin","target_tmax","target_tmed","target_pioggia","target_vento")
  
  if(day_ahead == 0 & regressorh == targeth)
  {
    print("ERROR: doing meaningless regression (x ~ x)")
    return(0)
  }
  
  variables <- which(tolower(colnames(pun)) %in% c("pun","aust","cors","fran","grec","slov","sviz"))
  
  regh <- which(pun[,2] == regressorh)
  tarh <- which(pun[,2] == targeth)
  
  if(regressorh == 24 | targeth == 24)
  {
    index1 <- index2 <- 0
    for(i in 1:(nrow(pun)-1))
    {
      #print(i)
      #print((pun[i,2] == 23) & (pun[i+1,2] == 1))
      if((pun[i,2] == 23) & (pun[i+1,2] == 1)) index1 <- i+1
      if(pun[i,2] == 24 & pun[i+1,2] == 25) index2 <- i
    }
    if(regressorh == 24) regh <- sort(c(regh,index1), decreasing = FALSE)
    if(targeth == 24) tarh <- sort(c(tarh,index1), decreasing = FALSE)
  }
  
  reg_val <- pun[regh[1:(length(regh)-day_ahead)], variables]
  if(day_ahead > 0) {y <- pun[tarh[(1+day_ahead):length(tarh)],varn]}
  else {y <- pun[tarh,varn]}
  
  dts <- dates(pun[,1])
  
  asdts <- dates2(dts)
  asdts2 <- unique(asdts)
  asdts3 <- maply(1:length(asdts2), function(n) from_dates_to_char(asdts2[n]))
  day <- maply(1:length(asdts2), function(n) convert_day_to_angle(convert_day(as.character(lubridate::wday(asdts2[n],label=TRUE)))))
  hol <- add_holidays(asdts3)
    
  tmin <- tmax <- tmed <- rain <- vm <- ttmin <- ttmax <- ttmed <- train <- tvm <- c()
  
  for(n in 1:(length(asdts3)-day_ahead))
  {
    tmin <- c(tmin, associate_meteo_ora(asdts3[n], meteo, "Tmin"))
    tmax <- c(tmax, associate_meteo_ora(asdts3[n], meteo, "Tmax"))
    tmed <- c(tmed, associate_meteo_ora(asdts3[n], meteo, "Tmedia"))
    rain <- c(rain, associate_meteo_ora(asdts3[n], meteo, "Pioggia"))
    vm <- c(vm, associate_meteo_ora(asdts3[n], meteo, "Vento_media"))
    
    ttmin <- c(ttmin, associate_meteo_ora(asdts3[n+day_ahead], meteo, "Tmin"))
    ttmax <- c(ttmax, associate_meteo_ora(asdts3[n+day_ahead], meteo, "Tmax"))
    ttmed <- c(ttmed, associate_meteo_ora(asdts3[n+day_ahead], meteo, "Tmedia"))
    train <- c(train, associate_meteo_ora(asdts3[n+day_ahead], meteo, "Pioggia"))
    tvm <- c(tvm, associate_meteo_ora(asdts3[n+day_ahead], meteo, "Vento_media"))
  }
  
  df <- data.frame(reg_val, day[1:(length(regh)-day_ahead)], hol[1:(length(regh)-day_ahead)], tmin, tmax, tmed, rain, vm, y,
                   day[(1+day_ahead):(length(tarh))], hol[(1+day_ahead):(length(tarh))], ttmin, ttmax, ttmed, train, tvm, stringsAsFactors = FALSE)
  
  colnames(df) <- vanames
  return(df)
}
#####################################################################################################################################
from_dates_to_char <- function(dt)
{
  return(paste0(unlist(strsplit(as.character(dt),"-"))[3],"/",unlist(strsplit(as.character(dt),"-"))[2],"/",unlist(strsplit(as.character(dt),"-"))[1]))
}
#####################################################################################################################################
get_meteo <- function(met)
{
  cols <- which(tolower(colnames(met)) %in% c("tmin","tmax","tmedia","pioggia","ventomedia"))
  dts <- unlist(met[,2])
  
  nums <- grep("/", dts)
  notn <- (nums[length(nums)]+1):nrow(met)
  
  dts2 <- c(dates2(dts[nums]), maply(1:length(notn), function(n) as.character(as.Date(as.numeric(dts[notn[n]]),origin = '1899-12-30'))))
  
  dts3 <- maply(1:length(dts2), function(n) from_dates_to_char(dts2[n]))
  
  met2 <- data.frame(dts3,met[,cols])
  colnames(met2) <- c("Data", "Tmin","Tmax","Tmedia","Vento_media","Pioggia")
  return(met2)
}
##########################################################################################################################################
generate_glm_datasets <- function(pun, meteo)
{
  gc()
  pun <- data.frame(pun)
  count <- 0
  for(da in 0:2)
  {
    for(rh in 1:24)
    {
      th_a <- c(2:24,1)
      th <- th_a[rh]
      tryCatch(
        {
          start <- Sys.time()
          #aug <- augmented_dataset(data1, data2, step = step , day_ahead = da)
          trainset <- create_glm_dataset(pun, "PUN", rh, th, meteo, da)
          trainset2 <- trainset[1:(nrow(trainset)-31),]
          testset <- trainset[(nrow(trainset)-31):nrow(trainset),]
          
          trainseth2o <- as.h2o(trainset)
          trainset2h2o <- as.h2o(trainset2)
          testseth2o <- as.h2o(testset)
          
          name1 <- paste0("C:\\Users\\utente\\Documents\\PUN\\glm\\trainset_completo_rh_",rh,"_th_",th,"_dayahead_",da,".csv")
          name2 <- paste0("C:\\Users\\utente\\Documents\\PUN\\glm\\trainset2_rh_",rh,"_th_",th,"_dayahead_",da,".csv")  
          name3 <- paste0("C:\\Users\\utente\\Documents\\PUN\\glm\\testset_rh_",rh,"_th_",th,"_dayahead_",da,".csv")
          
          h2o.exportFile(trainseth2o, name1)
          h2o.exportFile(trainset2h2o, name2)
          h2o.exportFile(testseth2o, name3)
          
          rm(trainset); rm(trainseth2o); rm(testset); rm(testseth2o); rm(trainset2); rm(trainset2h2o);
          print(paste("done regressor hour ",rh, " with target hour ", th," day ahead ", da, " and removed the files"))
          
          end <- Sys.time()
          end-start
          
          count <- count + 1
          print(paste0("passages left: ",24*3 - count))
        }, error = function(cond)
        {
          message(cond)
          print(paste("day ahead ", da, " rh ", rh, " and th ", th,  " failed"))
        }
      )
    }
  }
}