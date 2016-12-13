#### GAMM models for PUN ###
library(readxl)
library(mgcv)
library(lubridate)
library(data.table)

data <- read_excel('dati_2014-2016.xlsx')
colnames(data)
dim(data)

ConvertDate <- function(df)
{
  df <- as.data.frame(df)
  dfc <- as.POSIXct(unlist(df[,1]), origin = "1970-01-01")
  df$Giorno <- dfc
  return(df[,c(3,2)])
}

cdata <- ConvertDate(data)

plot(cdata, type = 'l', lwd = 2)

Date <- seq.POSIXt(from=as.POSIXct('2014-01-01'), to = as.POSIXct('2016-12-14'), by='hour')
DM <- data.frame(Date[1:(length(Date)-1)], unlist(data[,2]), c(1:nrow(data)))
colnames(DM) <- c("Date", "daily_mean", "day.of.year")

fit <- gamm(daily_mean ~ s(day.of.year, bs = "cc"), data = DM)
summary(fit)

plot(scale(unlist(cdata[,2])), type = 'l', lwd = 2)
lines(scale(fit$gam$fitted.values), type = 'l', lwd = 2, col = 'blue')

DT <- as.data.table(cdata)
DT[,sd(pun),by=floor_date(Date[1:(length(Date)-1)],"day")]
plot(DT[,sd(pun),by=floor_date(Date[1:(length(Date)-1)],"day")], type = 'l', lwd = 2)

DT2 <- DT[,sd(pun),by=floor_date(Date[1:(length(Date)-1)],"day")]
colnames(DT2) <- c("dd", "sd_pun")
DT2 <- data.frame(DT2)
DT2['dd'] <- c(1:nrow(DT2))

fit2 <- gamm(sd_pun ~ s(dd, bs = "cc"), data = DT2)
summary(fit2)

plot(scale(unlist(DT2[,2])), type = 'l', lwd = 2)
lines(scale(fit2$gam$fitted.values), type = 'l', lwd = 2, col = 'blue')
abline(v = 365, col = 'blue', lwd = 2)
abline(v = 730, col = 'red', lwd = 2)

### file di K2E per forward pun: 
### H:\Energy Management\04. WHOLESALE\07. ANALISI MERCATO\01. Indici Mercato\Indici_MERCATO_2016
