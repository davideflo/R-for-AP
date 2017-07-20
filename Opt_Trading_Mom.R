###### Momentum #######


###########################################################################################
get_Signals_Mom <- function(dt, Ta, Sl, tau = 0.05, nmom = 5, tau2 = 1.5, bVerbose = FALSE)
{
  df <- data_frame()
  lt <- l5 <- mom <- c()
  signal_I <- 0
  move <- 0
  for(i in (nmom):nrow(dt))
  {
    Mom <- dt$Last[i] - dt$Last[i-4]
    if(signal_I == 0)
    {
      if(Mom > tau + tau2)
      {
        #print("primo segnale")
        signal_I <- 1
        dove_I <- dt$Last[i]
        move <- 1
      }
      else if(Mom < -(tau2 + tau))
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
      if(Mom < (tau2 - tau) & move > 0)
      {
        #print("secondo segnale")
        #print(i)
        #print("apro posizione in vendita")
        d.f <- data.frame(data = dt$`Date GMT`[i], posizione_vendita = 1, posizione_acquisto = 0, first = dove_I, dove = i, target = dt$Last[i] - Ta, StopLoss = dt$Last[i] + Sl)
        l <- list(df, d.f)
        df <- rbindlist(l)
        signal_I <- 0
        move <- 0
      }
      else if(Mom > -(tau2 - tau) & move < 0)
      {
        #print("secondo segnale")
        #print(i)
        #print("apro posizione in acquisto")
        d.f <- data.frame(data = dt$`Date GMT`[i], posizione_vendita = 0, posizione_acquisto = 1, first = dove_I, dove = i, target = dt$Last[i] + Ta, StopLoss = dt$Last[i] - Sl)
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
get_ClosuresMom <- function(dt, dts)
{
  ldf <- data_frame()
  for(i in 1:nrow(dts))
  {
    start <- dts$dove[i]
    target <- dts$target[i]
    SL <- dts$StopLoss[i]
    dt2 <- dt[(start+1):nrow(dt),]
    vs <- dt$Last[start]
    ven <- dts$posizione_vendita[i]
    acq <- dts$posizione_acquisto[i]
    giorno <- dts$data[i]
    ora <- ifelse(!is.null(dt$Time[start]),dt$Time[start],0)
    if(start == nrow(dt))
    {
      break
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
          closure <- target
          pl <- (-closure + vs - 0.054)*8760
          d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                            d_chiusura = data_chiusura, P_L = pl)
          l <- list(ldf, d.f)
          ldf <- rbindlist(l)
          break
        }
        else if(!any(prices <= target) & any(prices >= SL))
        {
          data_chiusura <- dt2$`Date GMT`[j]
          closure <- SL
          pl <- (-closure + vs - 0.054)*8760
          d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
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
            pl <- (-closure + vs - 0.054)*8760
            d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                              d_chiusura = data_chiusura, P_L = pl)
            l <- list(ldf, d.f)
            ldf <- rbindlist(l)
            break
          }
          else
          {
            data_chiusura <- dt2$`Date GMT`[j]
            closure <- SL
            pl <- (closure - vs - 0.054)*8760
            d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
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
          d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                            d_chiusura = data_chiusura, P_L = pl)
          l <- list(ldf, d.f)
          ldf <- rbindlist(l)
          break
        }
        else if(!any(prices >= target) & any(prices <= SL))
        {
          data_chiusura <- dt2$`Date GMT`[j]
          closure <- SL
          pl <- (closure - vs - 0.054)*8760
          d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
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
            d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                              d_chiusura = data_chiusura, P_L = pl)
            l <- list(ldf, d.f)
            ldf <- rbindlist(l)
            break
          }
          else
          {
            data_chiusura <- dt2$`Date GMT`[j]
            closure <- SL
            pl <- (closure - vs - 0.054)*8760
            d.f <- data.frame(data = giorno, ora = ora, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
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
  #return(ldf[which(ldf$data <= ldf$d_chiusura),])
  return(ldf)
}
#################################################################################################################################
GetOptimValsMom <- function(X)
{
  ger <- data.table(read_excel("H:/Energy Management/13. TRADING/Dati_Bollinger_GER.xlsx", sheet = "DATI NEW"))
  #ger <- data.table(read_excel("H:/Energy Management/13. TRADING/GER_giornaliero.xlsx"))
  #ger <- data.table(read_excel("H:/Energy Management/13. TRADING/GER_17_CAND.xlsx"))
  #ger <- data.table(read_excel("H:/Energy Management/13. TRADING/GER_1718.xlsx"))
  ddf <- get_Signals_Mom(ger, X[1], X[2])
  ldf <- get_ClosuresMom(ger, ddf)
  return(-sum(ldf$P_L))
}
#################################################################################################################################
write.xlsx(ldf, 'ger_momentum_ora_5gg_1.5_1_1.xlsx', row.names = FALSE)
