######### TRADING ############

library(readxl)
library(plyr)
library(dplyr)
library(data.table)
library(lubridate)
library(zoo)
library(plotly)
library(xlsx)


# save.pkg.list <- installed.packages()[is.na(installed.packages()[ , "Priority"]), 1]
# save(save.pkg.list, file="pkglist.Rdata")
# load("pkglist.Rdata")
# install.packages(save.pkg.list)
# update.packages(checkBuilt=TRUE)

## classes: http://www.pitt.edu/~njc23/Lecture4.pdf
## http://adv-r.had.co.nz/OO-essentials.html

#############################################################################################
#############################################################################################
## with abuse of language, open means I buy, close means I sell...
setClass("Position", representation(Open = "numeric", Close = "numeric", PL = "numeric", DT = "data.table", atOpen = "numeric", atClose = "numeric",
                                    Target = "numeric", StopLoss = "numeric"))

setGeneric("initialize_pos", function(p){p@Open <- p@Close <- p@PL <- p@atOpen <- p@atClose <- p@Target <- p@StopLoss <- 0; return(p)})
setMethod(f = "initialize_pos", signature = "Position", definition = function(p){p@Open <- p@Close <- p@PL <- p@atOpen <- p@atClose <- p@Target <- p@StopLoss <- 0; return(p)})

setGeneric("setOpen", function(p){p@Open <- 1; return(p)})
setMethod(f = "setOpen", signature = "Position", definition = function(p){p@Open <- 1; return(p)})

setGeneric("setClose",function(p) {p@Close = 1; return(p)})
setMethod(f = "setClose", signature = "Position", definition = function(p) {p@Close <- 1; return(p)})

setGeneric("set_atOpen", function(p,i) {p@atOpen <- i; return(p)})
setMethod(f = "set_atOpen", signature = "Position", definition = function(p,i) {p@atOpen <- i; return(p)})

setGeneric("set_atClose", function(p,i) {p@atClose <- i; return(p)})
setMethod(f = "set_atClose", signature = "Position", definition = function(p,i) {p@atClose <- i; return(p)})

setGeneric("setPL", function(p,PL) {p@PL <- PL; return(p)})
setMethod(f = "setPL", signature = "Position", definition = function(p,PL) {p@PL <- PL; return(p)})

setGeneric("setData", function(p,dt) {p@DT <- dt; return(p)})
setMethod(f = "setData", signature = "Position", definition = function(p,dt) {p@DT <- dt; return(p)})

setGeneric("setTarget", function(p){
  if(p@Open == 1)
  {p@Target <- p@DT$Last[p@atOpen] + 0.06;
   p@StopLoss <- p@DT$Last[p@atOpen] - 0.03}
  else if(p@Close == 1)
  {
    p@Target <- p@DT$Last[p@atClose] - 0.06;
    p@StopLoss <- p@DT$Last[p@atClose] + 0.03
  }; return(p)})
setMethod(f = "setTarget", signature = "Position", definition = function(p){
  if(p@Open == 1)
  {p@Target <- p@DT$Last[p@atOpen] + 0.06;
  p@StopLoss <- p@DT$Last[p@atOpen] - 0.03}
  else if(p@Close == 1)
  {
    p@Target <- p@DT$Last[p@atClose] - 0.06;
    p@StopLoss <- p@DT$Last[p@atClose] + 0.03
  }; return(p)})

setGeneric("findTarget", function(p)
  {
  if(p@Open == 1)
  {
    for(i in 1:nrow(p@DT))
    {
      if(pDT$Last[i] == p@Target | pDT$Last[i] == p@StopLoss)
      {
        p <- setClose(p)
        p <- set_atClose(p, i)
      }
    }
  }
  else if(p@Close == 1)
  {
    for(i in 1:nrow(p@DT))
    {
      if(pDT$Last[i] == p@Target | pDT$Last[i] == p@StopLoss)
      {
        p <- setOpen(p)
        p <- set_atOpen(p, i)
      }
    }
  }
  return(p)
})
setMethod(f = "findTarget", signature = "Position", definition = function(p)
{
  if(p@Open == 1)
  {
    for(i in 1:nrow(p@DT))
    {
      if(pDT$Last[i] == p@Target | pDT$Last[i] == p@StopLoss)
      {
        p <- setClose(p)
        p <- set_atClose(p, i)
      }
    }
  }
  else if(p@Close == 1)
  {
    for(i in 1:nrow(p@DT))
    {
      if(pDT$Last[i] == p@Target | pDT$Last[i] == p@StopLoss)
      {
        p <- setOpen(p)
        p <- set_atOpen(p,i)
      }
    }
  }
  return(p)
})

setGeneric("computePL", function(p){if(p@Open + p@Close == 2){
  p@PL <- (p@DT$Last[max(p@atOpen, p@atClose)] - p@DT$Last[min(p@atOpen, p@atClose)] - 0.054);return(p)}
  else
  {
    return(0)
  }})
setMethod(f = "computePL", signature = "Position", definition = function(p){if(p@Open + p@Close == 2){
  p@PL <- (p@DT$Last[max(p@atOpen, p@atClose)] - p@DT$Last[min(p@atOpen, p@atClose)] - 0.054);return(p)}
  else
  {
    return(0)
  }})
### vettore di position diventa una list

setClass("Portfolio", representation(LoP = "list", PLTot = "numeric", DT = "data.table", signal = "numeric", val_signal = "numeric", target_signal = "numeric", move = "numeric"))

setGeneric("initialize", function(p){p@LoP <- list(); p@PLTot <- p@signal <- p@val_signal <- p@target_signal <- p@move <- 0; return(p)})
setMethod(f = "initialize", signature = "Portfolio", definition = function(p){p@LoP <- list(); p@PLTot <- p@signal <- p@val_signal <- p@target_signal <- p@move <- 0; return(p)})

setGeneric("resetSignals", function(p) {p@signal <- p@val_signal <- p@target_signal <- p@move <- 0; return(p)})
setMethod(f = "resetSignals", signature = "Portfolio", definition = function(p) {p@signal <- p@val_signal <- p@target_signal <- p@move <- 0; return(p)})

setGeneric("signalize", function(p, ix, valup, vallow, tau)
{
  if(p@signal == 0)
  {
    if(p@DT$Last[ix] > valup)
    {
      print("found 1st signal to sell")
      print(p@DT$`Date GMT`[ix])
      p@signal <- 1
      p@val_signal <- p@DT$Last[ix]
      p@target_signal <- valup - tau
      p@move <- 1
    }
    else if(p@DT$Last[ix] < vallow)
    {
      print("found 1st signal to buy")
      print(p@DT$`Date GMT`[ix])
      p@signal <- 1
      p@val_signal <- p@DT$Last[ix]
      p@target_signal <- vallow + tau
      p@move <- -1
    }
  }
  else
  {
    if(p@move > 0 & p@DT$Last[ix] < p@target_signal)
    {
      print("open a new sell position")
      print(p@DT$`Date GMT`[ix])
      p <- openNewPosition(p, 0, 1, 0, dt = p@DT[ix:nrow(p@DT),], 0, 1, p@target_signal - 0.06, p@target_signal + 0.03)
      p <- resetSignals(p)
    }
    else if(p@move < 0 & p@DT$Last[ix] > p@target_signal)
    {
      print("open a new buy position")
      print(p@DT$`Date GMT`[ix])
      p <- openNewPosition(p, 1, 0, 0, dt = p@DT[ix:nrow(p@DT),], 1, 0, p@target_signal + 0.06, p@target_signal - 0.03)
      p <- resetSignals(p)
    }
  }
  return(p)
})
setMethod(f = "signalize", signature = "Portfolio", definition = function(p, ix, valup, vallow, tau)
{
  if(p@signal == 0)
  {
    if(p@DT$Last[ix] > valup)
    {
      print("found 1st signal to sell")
      print(p@DT$`Date GMT`[ix])
      p@signal <- 1
      p@val_signal <- p@DT$Last[ix]
      p@target_signal <- valup - tau
      p@move <- 1
    }
    else if(p@DT$Last[ix] < vallow)
    {
      print("found 1st signal to buy")
      print(p@DT$`Date GMT`[ix])
      p@signal <- 1
      p@val_signal <- p@DT$Last[ix]
      p@target_signal <- vallow + tau
      p@move <- -1
    }
  }
  else
  {
    if(p@move > 0 & p@DT$Last[ix] < p@target_signal)
    {
      print("open a new sell position")
      print(p@DT$`Date GMT`[ix])
      p <- openNewPosition(p, 0, 1, 0, dt = p@DT[ix:nrow(p@DT),], 0, 1, p@target_signal - 0.06, p@target_signal + 0.03)
      p <- resetSignals(p)
    }
    else if(p@move < 0 & p@DT$Last[ix] > p@target_signal)
    {
      print("open a new buy position")
      print(p@DT$`Date GMT`[ix])
      p <- openNewPosition(p, 1, 0, 0, dt = p@DT[ix:nrow(p@DT),], 1, 0, p@target_signal + 0.06, p@target_signal - 0.03)
      p <- resetSignals(p)
    }
  }
  return(p)
})

setGeneric("AddPosition", function(p, pos){p@LoP <- c(p@LoP, pos); return(p)})
setMethod(f = "AddPosition",signature = "Portfolio", definition = function(p, pos){p@LoP <- c(p@LoP, pos); return(p)})

setGeneric("openNewPosition", function(p, open, close, pl, dt, atopen, atclose, target, stoploss){
  pos <- new("Position")
  pos <- initialize_pos(pos)
  if(open == 1) {pos <- setOpen(pos);pos <- set_atOpen(pos, atopen)}
  else{pos <- setClose(pos);pos <- set_atClose(pos,atclose)}
  pos <- setPL(pos, pl)
  pos <- setData(pos,dt)
  pos <- setTarget(pos)
  p <- AddPosition(p, pos)
  return(p)})
setMethod(f = "openNewPosition", signature = "Portfolio", definition = function(p, open, close, pl, dt, atopen, atclose, target, stoploss){
  pos <- new("Position")
  pos <- initialize_pos(pos)
  if(open == 1) {pos <- setOpen(pos);pos <- set_atOpen(pos, atopen)}
  else{pos <- setClose(pos);pos <- set_atClose(pos, atclose)}
  pos <- setPL(pos, pl)
  pos <- setData(pos,dt)
  pos <- setTarget(pos)
  p <- AddPosition(p, pos)
  return(p)})

setGeneric("setData", function(p,dt) {p@DT <- dt; return(p)})
setMethod(f = "setData", signature = "Portfolio", definition = function(p,dt) {p@DT <- dt; return(p)})

setGeneric("computeTOTPL", function(p) 
  {
    tot = 0
    for(l in 1:length(p@LoP))
    {tot <- tot + computeP&L(p)}
    p@PLTot <- tot
    return(p)
  })
setMethod(f = "computeTOTPL", signature = "Portfolio", definition = function(p) 
{
  tot = 0
  for(l in 1:length(p@LoP))
  {tot <- tot + computeP&L(p)}
  p@PLTot <- tot
  return(p)
})
#############################################################################################
#############################################################################################
mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=1)}
#############################################################################################
generatePortfolio <- function(P, dt)
{
  tau <- 0.05
  P <- setData(P,dt)
  rm <- c()
  std5 <- c()
  up <- c()
  low <- c()
  for(i in 5:nrow(dt))
  {
    rm <- c(rm, mean(dt$Last[(i-4):(i)]))
    std5 <- c(std5, sd(dt$Last[(i-4):(i)])*(2/sqrt(5)))
    up <- rm + 1.4*std5
    low <- rm - 1.4*std5
    valup <- round(up[length(up)] + 0.05,2)
    vallow <- round(low[length(low)] - 0.05,2)
    P <- signalize(P, i, valup, vallow, tau)
  }
  for(l in 1:length(P@LoP))
  {
    P@LoP[[l]] <- findTarget(P@LoP[[l]])
  }
  P <- computeTOTPL(P)
  return(P)
}
##############################################################################################
rollstd <- function(x, n = 5)
{
  rs <- c()
  for(i in n:length(x))
  {
    rs <- c(rs, sd(x[(i-(n-1)):i]))
  }
  return(rs)
}
###############################################################################################
get_Signals <- function(dt)
{
  df <- data_frame()
  tau <- 0.05
  rm <- c()
  std5 <- c()
  up <- c()
  low <- c()
  signal_I <- 0
  move <- 0
  for(i in 5:nrow(dt))
  {
    rm <- c(rm, mean(dt$Last[(i-4):(i)]))
    std5 <- c(std5, sd(dt$Last[(i-4):(i)])*(2/sqrt(5)))
    up <- rm + 1.4*std5
    low <- rm - 1.4*std5
    valup <- up[length(up)] + tau
    vallow <- low[length(low)] - tau
    valup_II <- up[length(up)] - tau
    vallow_II <- low[length(low)] + tau
    
    if(signal_I == 0)
    {
        if(dt$Last[i] > valup)
      {
        print("primo segnale")
        signal_I <- 1
        dove_I <- dt$Last[i]
        move <- 1
      }
      else if(dt$Last[i] < vallow)
      {
        print("primo segnale")
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
        print("secondo segnale")
        #print(i)
        print("apro posizione in vendita")
        d.f <- data.frame(data = dt$`Date GMT`[i], posizione_vendita = 1, posizione_acquisto = 0, first = dove_I, dove = i, mm = rm[length(rm)], devstd = std5[length(std5)],
                          up = up[length(up)], low = low[length(low)], target = dt$Last[i] - 0.6, StopLoss = dt$Last[i] + 0.3)
        l <- list(df, d.f)
        df <- rbindlist(l)
        signal_I <- 0
        move <- 0
      }
      else if(dt$Last[i] > vallow_II & move < 0)
      {
        print("secondo segnale")
        #print(i)
        print("apro posizione in acquisto")
        d.f <- data.frame(data = dt$`Date GMT`[i], posizione_vendita = 0, posizione_acquisto = 1, first = dove_I, dove = i, mm = rm[length(rm)], devstd = std5[length(std5)],
                          up = up[length(up)], low = low[length(low)], target = dt$Last[i] + 0.6, StopLoss = dt$Last[i] - 0.3)
        l <- list(df, d.f)
        df <- rbindlist(l)
        signal_I <- 0
        move <- 0
      }
      next
    }
  }
  plot(dt$Last, type = "l", col = "red", lwd = 1.5)
  lines(rm, type = "l", col = "royalblue4", lwd = 1.5)
  lines(up, type = "l", col = "orangered", lwd = 1.5)
  lines(low, type = "l", col = "orangered", lwd = 1.5)
  points(df$dove[which(df$posizione_vendita == 1)], dt$Last[df$dove[which(df$posizione_vendita == 1)]], pch = 16, col = "gold")
  points(df$dove[which(df$posizione_acquisto == 1)], dt$Last[df$dove[which(df$posizione_acquisto == 1)]], pch = 16, col = "purple")
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
get_Closures <- function(dt, dts)
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
###############################################################################################
ger <- dt <- data.table(read_excel("H:/Energy Management/13. TRADING/Dati_Bollinger_GER.xlsx", sheet = "DATI NEW"))

plot(ger$Last, type = "l", col = "violet")
hist(ger$Last, breaks = 40, col = "violet")
plot(diff(ger$Last), type = 'l', col = "steelblue")
hist(diff(ger$Last), breaks = 40, col = "steelblue")
plot(diff(ger$Last, lag = 2), type = 'l', col = "steelblue2")
lag.plot(ger$Last, col = 'blue', pch = 16)
acf(ger$Last, type = "correlation", lag.max = 500)
lines(ger$Last/max(ger$Last), type = "l", col = "violet")

mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=1)}
ger5 <- mav(ger$Last)
std5 <- rollstd(ger$Last, 5)
plot(ger$Last, type = "l", col = "violet", lwd = 1.5)
lines(ger5, type = "l", col = "royalblue4", lwd = 1.5)
lines((ger5 - 1.4*std5), type = "l", col = "orangered", lwd = 1.5)
lines((ger5 + 1.4*std5), type = "l", col = "orangered", lwd = 1.5)

acf(apply(ger[,3:6],1, sd), type = "correlation", lag.max = 100)

p <- ger %>%
  plot_ly(x = ~ger$`Date GMT`, type="candlestick",
          open = ~ger$Open, close = ~ger$Last,
          high = ~ger$High, low = ~ger$Low) %>%
  layout(title = "Basic Candlestick Chart")
p

plot(apply(ger[,3:6],1, sd), type = "l", col = "red")


ddf <- get_Signals(ger)
ldf <- get_Closures(ger, ddf)
sum(ldf$P_L)

library(plot3D)
x <- seq(0, 1, by = 0.01)
y <- seq(0, 1,by = 0.01)
M <- mesh(x, y)

alpha <- M$x
beta <- M$y

z <- matrix(0, nrow = 101, ncol = 101)
for(i in 1:101)
{
  for(j in 1:101)
  {
    z[i,j] <- (-1) * GetOptimVals(c(x[i], y[j]))
  }
}

surf3D(x = M$x,y = M$y, z = z, colkey=FALSE,bty="b2",main="P&L")

z_max <- apply(z, 1, max) ###6 --> x
z_max <- apply(z, 2, max) ###72 --> y

x[6]
y[1]

z_max <- apply(z, 1, max) ###101 --> x
z_max <- apply(z, 2, max) ###72 --> y

x[101]
y[93]

optim(c(0.5, 0.3), GetOptimVals, method = "L-BFGS-B", lower = c(0,0), upper = c(1,1))
