######### TRADING ############

library(readxl)
library(plyr)
library(dplyr)
library(data.table)
library(lubridate)
library(zoo)
library(plotly)


# save.pkg.list <- installed.packages()[is.na(installed.packages()[ , "Priority"]), 1]
# save(save.pkg.list, file="pkglist.Rdata")
# load("pkglist.Rdata")
# install.packages(save.pkg.list)
# update.packages(checkBuilt=TRUE)

## classes: http://www.pitt.edu/~njc23/Lecture4.pdf
## http://adv-r.had.co.nz/OO-essentials.html

#############################################################################################
#############################################################################################
setClass("Position", representation(Open = "numeric", Close = "numeric", PL = "numeric", DT = "data.table", atOpen = "numeric", atClose = "numeric"))

setGeneric("setOpen", function(p){p@Open <- 1})
setMethod(f = "setOpen", signature = "Position", definition = function(p){p@Open <- 1})

setGeneric("setClose",function(p) {p@Close = 1})
setMethod(f = "setClose", signature = "Position", definition = function(p) {p@Close = 1})

setGeneric("set_atOpen", function(p,i) {p@atOpen = i})
setMethod(f = "set_atOpen", signature = "Position", definition = function(p,i) {p@atOpen = i})

setGeneric("set_atClose", function(p,i) {p@atClose = i})
setMethod(f = "set_atClose", signature = "Position", definition = function(p,i) {p@atClose = i})

setGeneric("setData", function(p,dt) {x@DT = dt})
setMethod(f = "setData", signature = "Position", definition = function(p,dt) {x@DT = dt})

setGeneric("computeP&L", function(p){if(p@Open + p@Close == 2){
  return(p@DT[max(p@atOpen, p@atClose), "Last"] - p@DT[min(p@atOpen, p@atClose), "Last"] - 0.054)}
  else
  {
    return(0)
  }})
setMethod(f = "computeP&L", signature = "Position", definition = function(p){if(p@Open + p@Close == 2){
  return(p@DT[max(p@atOpen, p@atClose), "Last"] - p@DT[min(p@atOpen, p@atClose), "Last"] - 0.054)}
  else
    {
      return(0)
    }})
### vettore di position diventa una list
setClass("Portfolio", representation())
#############################################################################################
#############################################################################################


##############################################################################################
rollstd <- function(x, n)
{
  rs <- c()
  for(i in n:length(x))
  {
    rs <- c(rs, sd(x[(i-4):i]))
  }
  return(rs)
}
###############################################################################################

ger <- data.table(read_excel("H:/Energy Management/13. TRADING/Dati_Bollinger_GER.xlsx"))

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
plot(ger$Last, type = "l", col = "violet")
lines(ger5, type = "l", col = "royalblue4")
lines((ger5 - 1.4*std5), type = "l", col = "orangered")
lines((ger5 + 1.4*std5), type = "l", col = "orangered")

acf(apply(ger[,3:6],1, sd), type = "correlation", lag.max = 100)

p <- ger %>%
  plot_ly(x = ~ger$`Date GMT`, type="candlestick",
          open = ~ger$Open, close = ~ger$Last,
          high = ~ger$High, low = ~ger$Low) %>%
  layout(title = "Basic Candlestick Chart")
p

plot(apply(ger[,3:6],1, sd), type = "l", col = "red")
