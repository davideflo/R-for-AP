####### TRADING OPTIMIZATION

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
  for(i in max(nm, ns):nrow(dt))
  {
    rm <- c(rm, mean(dt$Last[(i-nm):(i)]))
    std5 <- c(std5, sd(dt$Last[(i-ns):(i)])*(sqrt(ns-1)/sqrt(ns)))
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
        d.f <- data.frame(data = dt$`Date GMT`[i], posizione_vendita = 1, posizione_acquisto = 0, first = dove_I, dove = i, mm = rm[length(rm)], devstd = std5[length(std5)],
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
        d.f <- data.frame(data = dt$`Date GMT`[i], posizione_vendita = 0, posizione_acquisto = 1, first = dove_I, dove = i, mm = rm[length(rm)], devstd = std5[length(std5)],
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
get_Closures2 <- function(dt, dts)
{
  ldf <- data_frame()
  for(i in 1:nrow(dts))
  {
    start <- dts$dove[i]
    target <- dts$target[i]
    SL <- dts$StopLoss[i]
    dt2 <- dt[start:nrow(dts),]
    vs <- dt2$Last[1]
    ven <- dts$posizione_vendita[i]
    acq <- dts$posizione_acquisto[i]
    giorno <- dts$data[i]
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
          closure <- target
          pl <- (closure - vs - 0.054)*8760
          d.f <- data.frame(data = giorno, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                            d_chiusura = data_chiusura, P_L = pl)
          l <- list(ldf, d.f)
          ldf <- rbindlist(l)
          break
        }
        else if(!any(prices <= target) & any(prices >= SL))
        {
          data_chiusura <- dt2$`Date GMT`[j]
          closure <- target
          pl <- (closure - vs - 0.054)*8760
          d.f <- data.frame(data = giorno, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                            d_chiusura = data_chiusura, P_L = pl)
          l <- list(ldf, d.f)
          ldf <- rbindlist(l)
          break
        }
        else if(any(prices <= target) & any(prices >= SL))
        {
          if(min(which(prices <= target)) < min(prices >= SL))
          {
            data_chiusura <- dt2$`Date GMT`[j]
            closure <- target
            pl <- (closure - vs - 0.054)*8760
            d.f <- data.frame(data = giorno, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                              d_chiusura = data_chiusura, P_L = pl)
            l <- list(ldf, d.f)
            ldf <- rbindlist(l)
            break
          }
          else
          {
            data_chiusura <- dt2$`Date GMT`[j]
            closure <- target
            pl <- (closure - vs - 0.054)*8760
            d.f <- data.frame(data = giorno, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                              d_chiusura = data_chiusura, P_L = pl)
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
          closure <- target
          pl <- (closure - vs - 0.054)*8760
          d.f <- data.frame(data = giorno, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                            d_chiusura = data_chiusura, P_L = pl)
          l <- list(ldf, d.f)
          ldf <- rbindlist(l)
          break
        }
        else if(!any(prices >= target) & any(prices <= SL))
        {
          data_chiusura <- dt2$`Date GMT`[j]
          closure <- target
          pl <- (closure - vs - 0.054)*8760
          d.f <- data.frame(data = giorno, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                            d_chiusura = data_chiusura, P_L = pl)
          l <- list(ldf, d.f)
          ldf <- rbindlist(l)
          break
        }
        else if(any(prices >= target) & any(prices <= SL))
        {
          if(min(which(prices <= target)) < min(prices >= SL))
          {
            data_chiusura <- dt2$`Date GMT`[j]
            closure <- target
            pl <- (closure - vs - 0.054)*8760
            d.f <- data.frame(data = giorno, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                              d_chiusura = data_chiusura, P_L = pl)
            l <- list(ldf, d.f)
            ldf <- rbindlist(l)
            break
          }
          else
          {
            data_chiusura <- dt2$`Date GMT`[j]
            closure <- target
            pl <- (closure - vs - 0.054)*8760
            d.f <- data.frame(data = giorno, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                              d_chiusura = data_chiusura, P_L = pl)
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
  return(ldf[which(ldf$data <= ldf$d_chiusura),])
}
#################################################################################################################################
GetOptimVals <- function(X)
{
  ger <- data.table(read_excel("H:/Energy Management/13. TRADING/Dati_Bollinger_GER.xlsx", sheet = "DATI NEW"))
  ddf <- get_Signals2(ger, X[1], X[2])
  ldf <- get_Closures2(ger, ddf)
  return(-sum(ldf$P_L))
}