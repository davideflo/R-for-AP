######### Optimization on moving averages ##################

get_SignalsMM <- function(dt, Ta, Sl, MMs = 5, MMl = 15, bVerbose = FALSE, EMA = FALSE)
{
  df <- data_frame()
  ms <- mean(dt$Last[(MMl-MMs+2):(MMl+1)])
  ml <- mean(dt$Last[1:MMl])
  move <- 0
  
  for(i in (MMl+2):nrow(dt))
  {
    if(EMA)
    {
      ms <- c(ms, ((dt$Last[i] - ms[length(ms)])*(2/(MMs+1)) + ms[length(ms)])  )
      #ml <- c(ml, ((dt$Last[i] - ms[length(ml)])*(2/(MMl+1)) + ml[length(ml)]))
      ml <- c(ml, mean(dt$Last[(i-MMl+1):(i)]))
    }
    else
    {
      ms <- c(ms, mean(dt$Last[(i-MMs+1):(i)]))
      ml <- c(ml, mean(dt$Last[(i-MMl+1):(i)]))
    }
    
    lml <- length(ml)
    lms <- length(ms)
    if( (ml[lml] > ms[lms]) & (ml[lml-1] < ms[lms-1]) )
    {
      ### SELL
      #print("primo segnale")
      dove_I <- dt$Last[i]
      move <- 1
      d.f <- data.frame(data = dt$`Date GMT`[i],posizione_vendita = 1,posizione_acquisto = 0,first = dove_I,dove = i, target = dt$Last[i] - Ta,StopLoss = dt$Last[i] + Sl)
      l <- list(df, d.f)
      df <- rbindlist(l)
      move <- 0
    }
    else if((ml[lml] < ms[lms]) & (ml[lml-1] > ms[lms-1]))
    {
      ### BUY
      #print("primo segnale")
      dove_I <- dt$Last[i]
      move <- -1
      d.f <- data.frame(data = dt$`Date GMT`[i],posizione_vendita = 0,posizione_acquisto = 1,first = dove_I,dove = i, target = dt$Last[i] + Ta,StopLoss = dt$Last[i] - Sl)
      l <- list(df, d.f)
      df <- rbindlist(l)
      move <- 0
    }
    else next
  }  
      
  if(bVerbose){
    plot(dt$Last, type = "l", col = "red", lwd = 1.5)
    lines(ml, type = "l", col = "royalblue4", lwd = 1.5)
    lines(ms, type = "l", col = "orangered", lwd = 1.5)
    points(df$dove[which(df$posizione_vendita == 1)], dt$Last[df$dove[which(df$posizione_vendita == 1)]], pch = 16, col = "gold")
    points(df$dove[which(df$posizione_acquisto == 1)], dt$Last[df$dove[which(df$posizione_acquisto == 1)]], pch = 16, col = "purple")
  }
  return(df)
}
################################################################################################################
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
get_ClosuresMM <- function(dt, dts)
{
    ldf <- data_frame()
    for(i in 1:nrow(dts))
    {
      start <- dts$dove[i]
      target <- dts$target[i]
      SL <- dts$StopLoss[i]
      dt2 <- dt[(start+1):nrow(dt),]
      vs <- dts$first[i]
      ven <- dts$posizione_vendita[i]
      acq <- dts$posizione_acquisto[i]
      giorno <- dts$data[i]
      
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
            ora_chiusura <- dt2$Time[j]
            closure <- target
            pl <- (-closure + vs - 0.054)*8760
            d.f <- data.frame(data = giorno, ora = ora_chiusura, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                              d_chiusura = data_chiusura, P_L = pl)
            l <- list(ldf, d.f)
            ldf <- rbindlist(l)
            break
          }
          else if(!any(prices <= target) & any(prices >= SL))
          {
            data_chiusura <- dt2$`Date GMT`[j]
            ora_chiusura <- dt2$Time[j]
            closure <- SL
            pl <- (-closure + vs - 0.054)*8760
            d.f <- data.frame(data = giorno, ora = ora_chiusura, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
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
              ora_chiusura <- dt2$Time[j]
              closure <- target
              pl <- (-closure + vs - 0.054)*8760
              d.f <- data.frame(data = giorno, ora = ora_chiusura, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                                d_chiusura = data_chiusura, P_L = pl)
              l <- list(ldf, d.f)
              ldf <- rbindlist(l)
              break
            }
            else
            {
              data_chiusura <- dt2$`Date GMT`[j]
              ora_chiusura <- dt2$Time[j]
              closure <- SL
              pl <- (closure - vs - 0.054)*8760
              d.f <- data.frame(data = giorno, ora = ora_chiusura, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
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
            ora_chiusura <- dt2$Time[j]
            closure <- target
            pl <- (closure - vs - 0.054)*8760
            d.f <- data.frame(data = giorno, ora = ora_chiusura, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                              d_chiusura = data_chiusura, P_L = pl)
            l <- list(ldf, d.f)
            ldf <- rbindlist(l)
            break
          }
          else if(!any(prices >= target) & any(prices <= SL))
          {
            data_chiusura <- dt2$`Date GMT`[j]
            ora_chiusura <- dt2$Time[j]
            closure <- SL
            pl <- (closure - vs - 0.054)*8760
            d.f <- data.frame(data = giorno, ora = ora_chiusura, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
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
              ora_chiusura <- dt2$Time[j]
              closure <- target
              pl <- (closure - vs - 0.054)*8760
              d.f <- data.frame(data = giorno, ora = ora_chiusura, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
                                d_chiusura = data_chiusura, P_L = pl)
              l <- list(ldf, d.f)
              ldf <- rbindlist(l)
              break
            }
            else
            {
              data_chiusura <- dt2$`Date GMT`[j]
              ora_chiusura <- dt2$Time[j]
              closure <- SL
              pl <- (closure - vs - 0.054)*8760
              d.f <- data.frame(data = giorno, ora = ora_chiusura, val_inizio = vs, posizione_vendita = ven, posizione_acquisto = acq, target = target, stoploss = SL, chiusura = closure, 
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
    return(ldf)
}
#################################################################################################################################
GetOptimValsMM <- function(X)
{
  ger <- data.table(read_excel("H:/Energy Management/13. TRADING/Dati_Bollinger_GER.xlsx", sheet = "DATI NEW"))
  #ger <- data.table(read_excel("H:/Energy Management/13. TRADING/GER_giornaliero.xlsx"))
  #ger <- data.table(read_excel("H:/Energy Management/13. TRADING/GER_1718.xlsx"))
  ddf <- get_SignalsMM(ger, X[1], X[2],MMs = 5, MMl = 15, bVerbose = TRUE, EMA = TRUE)
  ldf <- get_ClosuresMM(ger, ddf)
  return(-sum(ldf$P_L))
}
###############################################################################################
write.xlsx(ldf, 'ger17_ora_mmesp5st15_0.6&0.3.xlsx', row.names = FALSE)
