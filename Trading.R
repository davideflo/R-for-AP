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
