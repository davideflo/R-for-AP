############### 


library(readxl)
library(plyr)
library(dplyr)
library(data.table)
library(lubridate)
library(zoo)
library(plotly)
library(xlsx)

###########################################################################################
get_Signals2 <- function(dt, Ta, Sl, tau = 0.05, nm = 5, ns = 5, tau2 = 1.4, bVerbose = FALSE)
{
  df <- data_frame()
  rm <- c()
  std5 <- c()
  up <- c()
  low <- c()
  signal_I <- 0
  move <- 0
  for(i in (max(nm, ns)+1):nrow(dt))
  {
    rm <- c(rm, mean(dt$Last[(i-nm+1):(i)]))
    std5 <- c(std5, sd(dt$Last[(i-ns+1):(i)])*(sqrt(ns-1)/sqrt(ns)))
    up <- rm + tau2*std5
    low <- rm - tau2*std5
    valup <- up[length(up)] + tau
    vallow <- low[length(low)] - tau
    valup_II <- up[length(up)] - tau
    vallow_II <- low[length(low)] + tau
    
    if(signal_I == 0)
    {
      if(dt$Last[i] > valup)
      {
        #print("primo segnale")
        signal_I <- 1
        dove_I <- dt$Last[i]
        move <- 1
      }
      else if(dt$Last[i] < vallow)
      {
        #print("primo segnale")
        signal_I <- 1
        dove_I <- dt$Last[i]
        move <- -1
      }
      next
    }
    else
    {
      if(dt$Last[i] < valup_II & move > 0)
      {
        #print("secondo segnale")
        #print(i)
        #print("apro posizione in vendita")
        print(dove_I)
        print(dt$Last[i] - Ta)
        print(dt$Last[i] + Sl)
        d.f <- data.frame(data = dt$`Date GMT`[i], posizione_vendita = 1, posizione_acquisto = 0, first = dt$Last[i], dove = i, mm = rm[length(rm)], devstd = std5[length(std5)],
                          up = up[length(up)], low = low[length(low)], target = dt$Last[i] - Ta, StopLoss = dt$Last[i] + Sl)
        l <- list(df, d.f)
        df <- rbindlist(l)
        signal_I <- 0
        move <- 0
      }
      else if(dt$Last[i] > vallow_II & move < 0)
      {
        #print("secondo segnale")
        #print(i)
        #print("apro posizione in acquisto")
        print(dove_I)
        print(dt$Last[i] + Ta)
        print(dt$Last[i] - Sl)
        d.f <- data.frame(data = dt$`Date GMT`[i], posizione_vendita = 0, posizione_acquisto = 1, first = dt$Last[i], dove = i, mm = rm[length(rm)], devstd = std5[length(std5)],
                          up = up[length(up)], low = low[length(low)], target = dt$Last[i] + Ta, StopLoss = dt$Last[i] - Sl)
        l <- list(df, d.f)
        df <- rbindlist(l)
        signal_I <- 0
        move <- 0
      }
      next
    }
  }
  if(bVerbose){
    plot(dt$Last, type = "l", col = "red", lwd = 1.5)
    lines(rm, type = "l", col = "royalblue4", lwd = 1.5)
    lines(up, type = "l", col = "orangered", lwd = 1.5)
    lines(low, type = "l", col = "orangered", lwd = 1.5)
    points(df$dove[which(df$posizione_vendita == 1)], dt$Last[df$dove[which(df$posizione_vendita == 1)]], pch = 16, col = "gold")
    points(df$dove[which(df$posizione_acquisto == 1)], dt$Last[df$dove[which(df$posizione_acquisto == 1)]], pch = 16, col = "purple")
  }
  return(df)
}
###########################################################################################
get_SignalsLayers <- function(dt, dts, layer, Ta, Sl, tau = 0.05, nm = 5, ns = 5, tau2 = 1.4, bVerbose = FALSE)
{
  #### layer = 1 o 2. funzione lanciata due volte per determinare il livello di posizione aperta.
  #### dt = dataframe con le prime/layered posizioni aperte --> serve solo il valore di Last.
  #### modifica dt in modo che abbia una colonna che tenga conto del layer.
  df <- dts
  
  threshold <- 0 
  
  if(layer == 1)
  {
    threshold <- 0.3   
  }
  else
  {
    threshold <- 0.3
  }
  
  for(j in 1:nrow(dts))
  {
    da <- dts$dove[j]
    dtda <- dt[(da+1):nrow(dt), ]
    layered <- dts$Layer[j]
    buy <- dts$posizione_acquisto[j]
    sell <- dts$posizione_vendita[j]
    #val <- dt$Last[da]
    val <- dts$first[j]
    Target <- dts$target[j]
    StopLoss <- dts$StopLoss[j]
    
    if(da == nrow(dt)) break
    
    for(i in 1:nrow(dtda))
    {
      prices <- orderPrices(c(dtda$Open[i],dtda$High[i],dtda$Low[i],dtda$Last[i]))
      if(layered < layer & buy == 1)
      {
        if(any(prices <= (val - threshold))) 
        {
          d.f <- data.frame(data = dtda$`Date GMT`[i], posizione_vendita = 0, posizione_acquisto = 1, first = (val - threshold), dove = (da + i), 
                            target = Target, StopLoss = StopLoss, Layer = layer)
          l <- list(df, d.f)
          df <- rbindlist(l)
          break
        }
        else if(dtda$Last[i] >= Target)
        {
          break
        }
        else
        {
          next
        }
      }
      else if(layered < layer & sell == 1)
      {
        if(any(prices >= (val + threshold)))
        { 
          d.f <- data.frame(data = dtda$`Date GMT`[i], posizione_vendita = 1, posizione_acquisto = 0, first = (val + threshold), dove = (da + i), 
                            target = Target, StopLoss = StopLoss, Layer = layer)
          l <- list(df, d.f)
          df <- rbindlist(l)  
          break
        }
        else if(dtda$Last[i] <= Target)
        {
          break
        }
        else
        {
          next
        }
      }
    }
  }
  if(bVerbose){
    plot(dt$Last, type = "l", col = "red", lwd = 1.5)
    lines(rm, type = "l", col = "royalblue4", lwd = 1.5)
    lines(up, type = "l", col = "orangered", lwd = 1.5)
    lines(low, type = "l", col = "orangered", lwd = 1.5)
    points(df$dove[which(df$posizione_vendita == 1)], dt$Last[df$dove[which(df$posizione_vendita == 1)]], pch = 16, col = "gold")
    points(df$dove[which(df$posizione_acquisto == 1)], dt$Last[df$dove[which(df$posizione_acquisto == 1)]], pch = 16, col = "purple")
  }
  return(df)
}
###############################################################################################
orderPrices <- function(x)
{
  if(abs(x[2] - x[1]) < abs(x[2] - x[4]))
  {
    return(c(x[1], x[2], x[3], x[4]))
  }
  else if(abs(x[2] - x[1]) > abs(x[2] - x[4]))
  {
    return(c(x[1], x[3], x[2], x[4]))
  }
  else if(abs(x[2] - x[1]) == abs(x[2] - x[4]))
  {
    if(x[1] > x[4]) return(c(x[1], x[3], x[2], x[4]))
    else return(c(x[1], x[2], x[3], x[4]))
  }
}
###############################################################################################
###############################################################################################
get_ClosuresLayers <- function(dt, dts)
{
  print(nrow(dts))
  
  ldf <- data_frame()
  
  for(i in 1:nrow(dts))
  {
    print(i)
    start <- dts$dove[i]
    target <- dts$target[i]
    SL <- dts$StopLoss[i]
    dt2 <- dt[(start+1):nrow(dt),]
    
    lay <- dts$Layer[i]
    
    print(lay)
    
    #vs <- dt$Last[start]
    
    #if(lay > 0)
    #{
    vs <- dts$first[i]
    #}
    
    ven <- dts$posizione_vendita[i]
    acq <- dts$posizione_acquisto[i]
    giorno <- dts$data[i]
    
    
    ora <- ifelse(!is.null(dt$Time[start]),dt$Time[start],0)
    
    if(start == nrow(dt))
    {
      #break
      d.f <- data.frame(data = giorno, ora = 0, val_inizio = 0, posizione_vendita = 0, posizione_acquisto = 0, target = 0, stoploss = 0, chiusura = 0, 
                        d_chiusura = giorno, ora_chiusura = 0, P_L = 0, weight = 0)
      next
    }
    
    for(j in 1:nrow(dt2))
    {
      closure <- 0
      pl <- 0
      data_chiusura <- 0
      prices <- orderPrices(c(dt2$Open[j],dt2$High[j],dt2$Low[j],dt2$Last[j]))
      sel <- 0
      if(ven == 1)
      {
        if(any(prices <= target) & !any(prices >= SL))
        {
          data_chiusura <- dt2$`Date GMT`[j]
          ora_chiusura <- ifelse(!is.null(dt2$Time[j]),dt2$Time[j],0)
          closure <- target
          pl <- (-closure + vs - 0.054)*8760
          d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                            d_chiusura = data_chiusura, ora_chiusura = ora_chiusura, P_L = pl, weight = (2^lay))
          l <- list(ldf, d.f)
          ldf <- rbindlist(l)
          break
        }
        else if(!any(prices <= target) & any(prices >= SL))
        {
          data_chiusura <- dt2$`Date GMT`[j]
          ora_chiusura <- ifelse(!is.null(dt2$Time[j]),dt2$Time[j],0)
          closure <- SL
          pl <- (-closure + vs - 0.054)*8760
          d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                            d_chiusura = data_chiusura, ora_chiusura = ora_chiusura, P_L = pl, weight = (2^lay))
          l <- list(ldf, d.f)
          ldf <- rbindlist(l)
          break
        }
        else if(any(prices <= target) & any(prices >= SL))
        {
          if(min(which(prices <= target)) < min(prices >= SL))
          {
            data_chiusura <- dt2$`Date GMT`[j]
            ora_chiusura <- ifelse(!is.null(dt2$Time[j]),dt2$Time[j],0)
            closure <- target
            pl <- (-closure + vs - 0.054)*8760
            d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                              d_chiusura = data_chiusura, ora_chiusura = ora_chiusura, P_L = pl, weight = (2^lay))
            l <- list(ldf, d.f)
            ldf <- rbindlist(l)
            break
          }
          else
          {
            data_chiusura <- dt2$`Date GMT`[j]
            ora_chiusura <- ifelse(!is.null(dt2$Time[j]),dt2$Time[j],0)
            closure <- SL
            pl <- (closure - vs - 0.054)*8760
            d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                              d_chiusura = data_chiusura, ora_chiusura = ora_chiusura, P_L = pl, weight = (2^lay))
            l <- list(ldf, d.f)
            ldf <- rbindlist(l)
            break
          }
        }
        else
        {
          next
        }
      }
      else if(acq == 1)
      {
        if(any(prices >= target) & !any(prices <= SL))
        {
          data_chiusura <- dt2$`Date GMT`[j]
          ora_chiusura <- ifelse(!is.null(dt2$Time[j]),dt2$Time[j],0)
          closure <- target
          pl <- (closure - vs - 0.054)*8760
          d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                            d_chiusura = data_chiusura, ora_chiusura = ora_chiusura, P_L = pl, weight = (2^lay))
          l <- list(ldf, d.f)
          ldf <- rbindlist(l)
          break
        }
        else if(!any(prices >= target) & any(prices <= SL))
        {
          data_chiusura <- dt2$`Date GMT`[j]
          ora_chiusura <- ifelse(!is.null(dt2$Time[j]),dt2$Time[j],0)
          closure <- SL
          pl <- (closure - vs - 0.054)*8760
          d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                            d_chiusura = data_chiusura, ora_chiusura = ora_chiusura, P_L = pl, weight = (2^lay))
          l <- list(ldf, d.f)
          ldf <- rbindlist(l)
          break
        }
        else if(any(prices >= target) & any(prices <= SL))
        {
          if(min(which(prices <= target)) < min(prices >= SL))
          {
            data_chiusura <- dt2$`Date GMT`[j]
            ora_chiusura <- ifelse(!is.null(dt2$Time[j]),dt2$Time[j],0)
            closure <- target
            pl <- (closure - vs - 0.054)*8760
            d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                              d_chiusura = data_chiusura, ora_chiusura = ora_chiusura, P_L = pl, weight = (2^lay))
            l <- list(ldf, d.f)
            ldf <- rbindlist(l)
            break
          }
          else
          {
            data_chiusura <- dt2$`Date GMT`[j]
            ora_chiusura <- ifelse(!is.null(dt2$Time[j]),dt2$Time[j],0)
            closure <- SL
            pl <- (closure - vs - 0.054)*8760
            d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                              d_chiusura = data_chiusura, ora_chiusura = ora_chiusura, P_L = pl, weight = (2^lay))
            l <- list(ldf, d.f)
            ldf <- rbindlist(l)
            break
          }
        }
        else
        {
          next
        }
      }
    }
  }
  return(ldf)
}
#################################################################################################################################
GetOptimValsLayered <- function(X)
{
  #ger <- data.table(read_excel("H:/Energy Management/13. TRADING/Dati_Bollinger_GER.xlsx", sheet = "DATI NEW"))
  #ger <- data.table(read_excel("H:/Energy Management/13. TRADING/GER_giornaliero.xlsx"))
  #ger <- data.table(read_excel("H:/Energy Management/13. TRADING/GER_17_CAND.xlsx"))
  #ger <- data.table(read_excel("H:/Energy Management/13. TRADING/GER_1718.xlsx")); ger <- ger[,1:5]
  
  ddf <- get_Signals2(ger, X[1], X[2])
  collist <- c(1:5, 10, 11)
  ddf <- as.data.frame(ddf)[,collist]
  ddf$Layer <- rep(0, nrow(ddf))
  ddf1 <- get_SignalsLayers(ger, ddf, 1, X[1], X[2])
  ddf2 <- get_SignalsLayers(ger, ddf1, 2, X[1], X[2])
  
  Ldf <- get_ClosuresLayers(ger, ddf2)
  
  weights <- rep(0, nrow(Ldf))
  for(i in 1:nrow(Ldf))
  {
    if(Ldf$weight[i] == 1) weights[i] <- 1
    else if(Ldf$weight[i] == 2) weights[i] <- 3
    if(Ldf$weight[i] == 4) weights[i] <- 7
  }
  
  return(-sum(Ldf$P_L * weights))
}
write.xlsx(ldf, 'ger1718_bollinger_gior_5gg_1.4_strat_1_1.1_2.xlsx', row.names = FALSE)
