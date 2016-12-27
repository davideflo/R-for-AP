##### Functions to treat POD orari 


source("C://Users//utente//Documents//R_code//SparseFunctClust.R")
source("R_code/functions_for_PUN_server.R")


ConvertDate <- function(df)
{
  df <- as.data.frame(df)
  dfc <- as.Date(as.POSIXct(unlist(df['Giorno']), origin = "1970-01-01"))
  df$Giorno <- dfc
  return(df)
}
#######################################################
HourAggregator <- function(df)
{
  M <- matrix(0, nrow = nrow(df), ncol = 24)
  df2 <- data_frame()
  s <- seq(1, 96, by = 4)
  dfloc <- df[,4:99]
  for(i in 1:nrow(df))
  {
    df2 <- bind_rows(df2, df[i,1:2])
    for(j in 1:(length(s)-1))
    {
      M[i, j] <- sum(dfloc[i, s[j]:(s[j+1]-1)])
    }
    M[i, 24] <- sum(dfloc[i, 93:96])
  }
  df2 <- bind_cols(df2, data.frame(M))
  colnames(df2) <- c('Pod', 'Giorno', as.character(1:24))
  return(df2)
}
#####################################################
FilterByDateAndValue <- function(df, date, value, kind)
{
  df2 <- df[which(as.Date(unlist(df['Giorno'])) == as.Date(date)),]
  if(kind == "greater")
  {
    df3 <- df2[which(apply(df2[,3:26], 1, min) >= value),3:26]    
  }
  else
  {
    df3 <- df2[which(apply(df2[,3:26], 1, max) <= value),3:26]    
  }
  return(df3)
}
#############################################################
FilterByDate <- function(df2, datait)
{
  df3 <- df2[which(as.Date(unlist(df2[,2])) == as.Date(datait)),]
  return(df3)
}
#################################################################
CHStatistics <- function(df, K, m)
{
  chs <- rep(0, K)
  x <- seq(1,24, by = 1)
  for( k in 2:(K-2))
  {
    fobj <- FKMSparseClustering(df, x, k, m, method='kmea', maxiter = 50)
    getter <- GetWCSS(df, fobj$CLUSTER)
    Bk <- sum(getter$bcss.perfeature)
    Wk <- getter$wcss
    chs[k] <- (Bk/(k-1))/(Wk/(nrow(df)-k))
  }
  return(chs)
}
##################################################################
FitPolynomial <- function(y, x, degree)
{
  fp <- lm(y ~ poly(x, degree, raw=TRUE))
  return(fp$coefficients)
}
####################################################################
BestFittingPolynomial <- function(y, x, maxdegree)
{
  best <- 0
  bestdegree <- 1
  bestfp <- lm(y ~ poly(x, 1, raw=TRUE))
  best <- sqrt(mean(bestfp$residuals^2))
  for(m in 2:maxdegree)
  {
    fp <- lm(y ~ poly(x, m, raw=TRUE))
    print(summary(fp))
    if(sqrt(mean(bestfp$residuals^2)) < best)
    {
      bestdegree <- m
    }
  }
  return(m)
}
####################################################################
FitDerivatives <- function(df, flag = TRUE)
{
  if(flag)
  {
    argvals <- seq(0,1,len=240)
    nbasis <- 20
    basisobj <- create.bspline.basis(c(0,1),nbasis)
    M <- matrix(0, nrow = nrow(df), ncol = 240)
    for(i in 1:nrow(df))
    {
      interp_df <- approx(argvals[c(1,11,21,31,41,51,61,71,81,91,101,111,121,131,141,151,161,171,181,191,201,211,221,240)], unlist(df[i,]), xout = argvals)
      Ys <- smooth.basis(argvals=argvals, y=interp_df$y, fdParobj=basisobj,returnMatrix=TRUE)
      Dxhat <- eval.fd(argvals, Ys$fd, Lfd=1)
      Dxhat_smooth <- smooth.basis(argvals=argvals, y=Dxhat, fdParobj=basisobj,returnMatrix=TRUE)
      M[i,] <- unlist(Dxhat_smooth$y[,1])
    }
    return(M)
  }
  else
  {
    argvals <- seq(0,1,len=24)
    nbasis <- 20
    basisobj <- create.bspline.basis(c(0,1),nbasis)
    M <- matrix(0, nrow = nrow(df), ncol = 24)
    for(i in 1:nrow(df))
    {
      Ys <- smooth.basis(argvals=argvals, y=unlist(df[i,]), fdParobj=basisobj,returnMatrix=TRUE)
      Dxhat <- eval.fd(argvals, Ys$fd, Lfd=1)
      M[i,] <- unlist(Dxhat[,1])
    }
    return(M)
  }
}
#########################################################################################
TreatData <- function(name)
{
  data <- as.data.frame(read_feather(paste0("C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_",name)))
  data <- ConvertDate(data)
  Agg <- HourAggregator(data)
  write_feather(Agg, paste0("C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_",name))
}
########################################################################################
TSAggregator <- function(df)
{
  days <- as.character(seq.Date(from = as.Date('2016-01-01'), to = as.Date('2016-10-31'), by = 'day'))
  ts <- c()
  for(d in days)
  {
    atd <- FilterByDate(df, d)[,3:26]
    ts <- c(ts, unlist(colSums(atd, na.rm = TRUE)))
  }
  return(ts)
}
#########################################################################################
GetSD <- function(ts, from, to)
{ ### to is one day ahead ###
  dsd <- c()
  Date <- seq.POSIXt(from=as.POSIXct(from), to = as.POSIXct(to), by='hour')
  DT <- data.table(Date[1:(length(Date)-1)], ts, c(1:length(ts)))
  colnames(DT) <- c("Date", "cons", "day.of.year")
  return(DT[,sd(cons),by=floor_date(Date,"day")])
}
#########################################################################################
GetHourlySD <- function(df)
{
  days <- as.character(seq.Date(from = as.Date('2016-01-01'), to = as.Date('2016-10-31'), by = 'day'))
  ts <- c()
  for(d in days)
  {
    atd <- (1/1000)*FilterByDate(df, d)[,3:26]
    ts <- c(ts, unlist(apply(atd, 2, sd, na.rm = TRUE)))
  }
  return(ts)
}
#############################################################################################
AggregateMLR <- function(df)
{
  d_f <- data_frame()
  days <- unique(df$Giorno)
  seqd <- seq.Date(from = as.Date('2016-01-01'), to = as.Date('2016-10-31'), by = 'day')
  for(d in days)
  {
    dd <- which(seqd == d)
    atd <- df[which(df$Giorno == d),3:26]
    df2 <- data.frame(seqd[dd], t((1/1000)*colSums(atd, na.rm = TRUE)))
    d_f <- bind_rows(d_f, df2)
  }
  colnames(d_f) <- c("date", as.character(1:24))
  return(d_f)
}
#############################################################################################
associate_meteo_data <- function(data, meteo, meteovar)
{
  ir <- which(unlist(meteo["Data"]) == data)
  vm <- as.numeric(meteo[ir,meteovar])
  return(vm)
}
#############################################################################################
MakeDatasetMLR <- function(df, meteo, H)
{
  d_f <- data_frame()
  succH <- 0
  if(H == 24)
  {
    succH <- 1
  }
  else
  {
    succH <- H + 1
  }  
  
  dts <- seq.POSIXt(as.POSIXct('2016-01-01'), as.POSIXct(meteo[nrow(meteo),1]), by = 'day')
  Giorno <- 'date'
  for(i in 5:length(dts))
  {
    print(dts[i])
    y <- unlist(df[which(as.Date(unlist(df[Giorno])) == as.Date(dts[i])), as.character(H)])
    target_day <- lubridate::wday(as.Date(dts[i]))
    target_week <- lubridate::week(as.Date(dts[i]))
    target_T <- associate_meteo_data(as.Date(dts[i]), meteo, 'Tmedia')
    if(H == 0)
    {
      x <- unlist(df[which(as.Date(unlist(df[Giorno])) == (as.Date(dts[i]))-2), ])
      #wasday <- lubridate::wday(as.Date(dts[i])-2)
    }
    else
    {
      x <- c(unlist(df[which(as.Date(unlist(df[Giorno])) == (as.Date(dts[i])-3)), as.character(succH:24)]),
             unlist(df[which(as.Date(unlist(df[Giorno])) == (as.Date(dts[i])-2)), as.character(1:H)]))
    }
    df2 <- data.frame(t(x), target_day, target_week, target_T, y)
    colnames(df2) <- c(paste0('H.',24:1), 'target_day', 'target_week', 'target_T', 'y')
    d_f <- bind_rows(d_f, df2)
  }
  return(d_f)
}
################################################################################################################
el_string <- function(string)
{
  ss <- unlist(strsplit(string, "/" ))
  sn <- paste0(unlist(strsplit(ss[3], " "))[1], "-", ss[2], "-", ss[1], " ", unlist(strsplit(ss[3], " "))[2])
  return(sn)
}
################################################################################################################
toPOs_sbil <- function(drc)
{
  tp <- maply(1:length(drc), function(n) {print(as.POSIXct(drc[n], format="%Y-%m-%dT%H:%M")); as.POSIXct(drc[n], format="%Y-%m-%d T%H:%M")}  )
  return(tp)
}
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
  pasqua <- as.Date(c("2010-04-04", "2011-04-24", "2012-04-08", "2013-03-31", "2014-04-20", "2015-04-05", "2016-03-27"))
  pasquetta <- as.Date(c("2010-04-05", "2011-04-25", "2012-04-09", "2013-04-01", "2014-04-21", "2015-04-06", "2016-03-28"))
  
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
################################################################################################################
MakeDatasetMLR_2 <- function(df, meteo, H, initial_date)
{
  d_f <- data_frame()
  succH <- 0
  if(H == 24)
  {
    succH <- 1
  }
  else
  {
    succH <- H + 1
  }  
  
  dts <- seq.Date(as.Date(initial_date), as.Date(meteo[nrow(meteo),1]), by = 'day')
  Giorno <- 'date'
  for(i in 5:length(dts))
  {
    print(dts[i])
    y <- unlist(df[which(as.Date(unlist(df[Giorno])) == as.Date(dts[i])), as.character(H)])
    target_day <- lubridate::wday(as.Date(dts[i]))
    target_week <- lubridate::week(as.Date(dts[i]))
    target_T <- associate_meteo_data(as.Date(dts[i]), meteo, 'Tmedia')
    cd <- unlist(df[which(as.Date(unlist(df[Giorno])) == as.Date(dts[i])), "CD"])
    holidays <- add_holidays_Date(dts[i])
    if(H == 0)
    {
      x <- unlist(df[which(as.Date(unlist(df[Giorno])) == (as.Date(dts[i]))-2), ])
      #wasday <- lubridate::wday(as.Date(dts[i])-2)
    }
    else
    {
      x <- c(unlist(df[which(as.Date(unlist(df[Giorno])) == (as.Date(dts[i])-3)), as.character(succH:24)]),
             unlist(df[which(as.Date(unlist(df[Giorno])) == (as.Date(dts[i])-2)), as.character(1:H)]))
    }
    df2 <- data.frame(t(x), target_day, target_week, target_T, cd, holidays, y)
    colnames(df2) <- c(paste0('H.',24:1), 'target_day', 'target_week', 'target_T', 'change_date', 'holiday', 'y')
    d_f <- bind_rows(d_f, df2)
  }
  return(d_f)
}
####################################################################################################################
NHI <- function(sbil, df)
{
  d_f <- data_frame()
  for(d in 1:nrow(sbil))
  {
    print(d)
    atd <- sbil[d,'date']
    atr <- df[which(unlist(df['date']) == unlist(atd)),2:25]
    print(unlist(sbil[d,2:25]) - unlist(atr))
    df2 <- data.frame(atd, t(unlist(sbil[d,2:25]) - unlist(atr)))
    colnames(df2) <- c('date', as.character(1:24))
    d_f <- bind_rows(d_f, df2)
  }
  return(d_f)
}
####################################################################################################################
GetModel <- function(df, meteo, H, data_inizio)
{
  df8 <- df[which(unlist(df$date) <= as.Date("2016-08-31")),]
  df8 <- cbind(df8, CD = rep(0,nrow(df8)))
  
  DT <- MakeDatasetMLR_2(df8, meteo, H, data_inizio)
  print(data.table(DT))
  ctrl <- list(niterEM = 10, msVerbose = TRUE, optimMethod="L-BFGS-B")
  
  ### gives error as in these webpages:
  ## http://stackoverflow.com/questions/30391347/object-created-inside-function-not-found-by-ggplot 
  ## http://stackoverflow.com/questions/17992424/i-do-not-understand-error-object-not-found-inside-the-function
  ## http://stackoverflow.com/questions/22617354/object-not-found-error-within-a-user-defined-function-eval-function
  
  m3 <- gamm(DT$y ~ DT$`H.24` + DT$`H.23` + DT$`H.22` + DT$`H.21` + DT$`H.20` +
               DT$`H.19` + DT$`H.18` + DT$`H.17` + DT$`H.16` + DT$`H.15` +
               DT$`H.14` + DT$`H.13` + DT$`H.12` + DT$`H.11` + DT$`H.10` +
               DT$`H.9` + DT$`H.8` + DT$`H.7` + DT$`H.6` + DT$`H.5` +
               DT$`H.4` + DT$`H.3` + DT$`H.2` + DT$`H.1` +
               s(target_day, bs = "cc", k = 7) + s(target_week, bs = "cc", k = 35) + s(target_T, bs = "cc") + holiday, data = DT,
             correlation = corARMA(form = ~ 1|target_week, p = 2),control = ctrl)
  
  print(summary(m3$gam))
  plot(m3$gam, scale = 0)
  
  plot(DTN$y, type = "l", lwd = 2, col = 'blue3')
  lines(m3$gam$fitted.values, type = "l", lwd = 2, col = 'red')
  
  print(paste("length fitted values:",length(m3$gam$fitted.values)))
  print(paste("length real values:",length(DTN$y)))
  print(m3$gam$fitted.values)
    
  ggplot(data = data.table(Fitted_values = m3$gam$fitted.values, Residuals = m3$gam$residuals),
         aes(Fitted_values, Residuals)) +
    geom_point(size = 1.5) +
    geom_smooth() +
    geom_hline(yintercept = 0, color = "red", size = 0.8) +
    labs(title = "Fitted vs Residuals")
  
  #yy <- DT$y[-c(89,97)]
  yy <- DT$y
  
  print(mape(yy, m3$gam$fitted.values))
  print(vectorMape(yy, m3$gam$fitted.values))
  print(max(vectorMape(yy, m3$gam$fitted.values)))
  
  return(m3)
  
}
####################################################################################
mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/real)))
}
####################################################################################
vectorMape <- function(real, pred)
{
  return(100 * abs((real - pred)/real))
}
#########################################################################################
GetLinModel <- function(df, meteo, H, data_inizio)
{
  df8 <- df[which(unlist(df$date) <= as.Date("2016-08-31")),]
  df8 <- cbind(df8, CD = rep(0,nrow(df8)))
  
  DTN <- MakeDatasetMLR_2(df8, meteo, H, data_inizio)
  
  m3 <- lm(DTN$y ~ DTN$`H-24` + DTN$`H-23` + DTN$`H-22` + DTN$`H-21` + DTN$`H-20` +
               DTN$`H-19` + DTN$`H-18` + DTN$`H-17` + DTN$`H-16` + DTN$`H-15` +
               DTN$`H-14` + DTN$`H-13` + DTN$`H-12` + DTN$`H-11` + DTN$`H-10` +
               DTN$`H-9` + DTN$`H-8` + DTN$`H-7` + DTN$`H-6` + DTN$`H-5` +
               DTN$`H-4` + DTN$`H-3` + DTN$`H-2` + DTN$`H-1` + target_day:target_week + target_day:holiday + I(target_T^2)
               + target_day:target_T + target_week:target_T+ target_day + target_week + target_T + holiday, data = DTN)
  
  print(summary(m3))
  plot(m3)
  
  plot(DTN$y, type = "l", lwd = 2, col = 'blue3')
  lines(m3$fitted.values, type = "l", lwd = 2, col = 'red')
  
  print(paste("length fitted values:",length(m3$fitted.values)))
  print(paste("length real values:",length(DTN$y)))
  print(m3$fitted.values)
  
  ggplot(data = data.table(Fitted_values = m3$fitted.values, Residuals = m3$residuals),
         aes(Fitted_values, Residuals)) +
    geom_point(size = 1.5) +
    geom_smooth() +
    geom_hline(yintercept = 0, color = "red", size = 0.8) +
    labs(title = "Fitted vs Residuals")
  
  yy <- DTN$y
  #yy <- DTN$y[-c(89,97)]
  
  print(mape(yy, m3$fitted.values))
  print(vectorMape(yy, m3$fitted.values))
  print(max(vectorMape(yy, m3$fitted.values)))
  
  return(m3)
  
}
####################################################################################
  