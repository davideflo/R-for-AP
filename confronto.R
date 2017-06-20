##### confronto curve pun forward
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(feather)
library(lubridate)
library(data.table)
library(entropy)
library(distrEx)

################################################################################################
filterDay <- function(df, weekday)
{
  return(df[which(lubridate::wday(as.Date(unlist(df["date"]), origin = '1899-12-30')) == weekday),])
}
################################################################################################

ltp <- read_excel('longterm_pun.xlsx')
fabb <- read_excel('Forecast annuale.xlsx')
k2e <- read_excel('FWD2017.xlsx', sheet = 'Sheet1')

### NORD
x <- ltp$pun * unlist(fabb[,3]/1000)
y <- k2e$ITALIA * unlist(fabb[,3]/1000)

mean(x-y)
mean(ltp$pun - k2e$ITALIA)
plot(x-y, type = 'l')
plot(ltp$pun - k2e$ITALIA, type = 'l')
sd(x-y)
sd(ltp$pun - k2e$ITALIA)

#### CNORD
x <- ltp$pun * unlist(fabb[,4]/1000)/sum(unlist(fabb[,4]/1000))
y <- k2e$ITALIA * unlist(fabb[,4]/1000)/sum(unlist(fabb[,4]/1000))
mean(x - y)

### CSUD
x <- ltp$pun * unlist(fabb[,5]/1000)
y <- k2e$ITALIA * unlist(fabb[,5]/1000)
mean(x/sum(unlist(fabb[,5]/1000))-y/sum(unlist(fabb[,5]/1000)))
           
### SUD
x <- ltp$pun * unlist(fabb[,6]/1000)
y <- k2e$ITALIA * unlist(fabb[,6]/1000)
mean(x/sum(unlist(fabb[,6]/1000))-y/sum(unlist(fabb[,6]/1000)))

####SICI
x <- ltp$pun * unlist(fabb[,7]/1000)
y <- k2e$ITALIA * unlist(fabb[,7]/1000)
mean(x/sum(unlist(fabb[,7]/1000))-y/sum(unlist(fabb[,7]/1000)))

#### SARD
x <- ltp$pun * unlist(fabb[,8]/1000)
y <- k2e$ITALIA * unlist(fabb[,8]/1000)
mean(x/sum(unlist(fabb[,8]/1000))-y/sum(unlist(fabb[,8]/1000)))

x <- (x - mean(x))/sd(x)
y <- (y - mean(y))/sd(y)

hist(x - y, breaks = 20)
plot(x-y, type = 'l')
abline(h = 1)
abline(h = -1)

##############################################################################################################
### analysis of the distribusions of F1,F2, F3 given PK or OP
### https://cran.r-project.org/web/packages/distrEx/distrEx.pdf

real <- data.table(read_excel("DB_Borse_Elettriche_PER MI_17_conMacro - Copy.xlsm", sheet = 2))
real <- real[which(!is.na(real[,13])), 1:13]
colnames(real)[13] <- "PUN"
colnames(real)[1] <- "date"


dt <- data.table(read_excel("conddistr.xlsx"))

par(mfrow = c(3,2))
hist(dt$PUN[which(dt$`AEEG 181/06` == "F1" & (dt$`PEAK-OFF PEAK` == "PK" |dt$`PEAK-OFF PEAK` == "P"))], breaks = 20, col = 'red', main = "L(F1|PK)")
hist(dt$PUN[which(dt$`AEEG 181/06` == "F2" & (dt$`PEAK-OFF PEAK` == "PK" |dt$`PEAK-OFF PEAK` == "P"))], breaks = 20, col = 'magenta', main = "L(F2|PK)")
hist(dt$PUN[which(dt$`AEEG 181/06` == "F2" & dt$`PEAK-OFF PEAK` == "OP")], breaks = 20, col = 'grey', main = "L(F2|OP)")
hist(dt$PUN[which(dt$`AEEG 181/06` == "F3" & (dt$`PEAK-OFF PEAK` == "PK" |dt$`PEAK-OFF PEAK` == "P"))], breaks = 20, col = 'green', main = "L(F3|PK)")
hist(dt$PUN[which(dt$`AEEG 181/06` == "F3" & dt$`PEAK-OFF PEAK` == "OP")], breaks = 20, col = 'navy', main = "L(F3|OP)")

print(mean(dt$PUN[which(dt$`AEEG 181/06` == "F1" & (dt$`PEAK-OFF PEAK` == "PK" |dt$`PEAK-OFF PEAK` == "P"))]))
#hist(dt$PUN[which(dt$`AEEG 181/06` == "F1" & dt$`PEAK-OFF PEAK` == "OP")], breaks = 20)
print(mean(dt$PUN[which(dt$`AEEG 181/06` == "F2" & (dt$`PEAK-OFF PEAK` == "PK" |dt$`PEAK-OFF PEAK` == "P"))]))
print(mean(dt$PUN[which(dt$`AEEG 181/06` == "F2" & dt$`PEAK-OFF PEAK` == "OP")]))
print(mean(dt$PUN[which(dt$`AEEG 181/06` == "F3" & (dt$`PEAK-OFF PEAK` == "PK" |dt$`PEAK-OFF PEAK` == "P"))]))
print(mean(dt$PUN[which(dt$`AEEG 181/06` == "F3" & dt$`PEAK-OFF PEAK` == "OP")]))

print(sd(dt$PUN[which(dt$`AEEG 181/06` == "F1" & (dt$`PEAK-OFF PEAK` == "PK" |dt$`PEAK-OFF PEAK` == "P"))]))
print(sd(dt$PUN[which(dt$`AEEG 181/06` == "F2" & (dt$`PEAK-OFF PEAK` == "PK" |dt$`PEAK-OFF PEAK` == "P"))]))
print(sd(dt$PUN[which(dt$`AEEG 181/06` == "F2" & dt$`PEAK-OFF PEAK` == "OP")]))
print(sd(dt$PUN[which(dt$`AEEG 181/06` == "F3" & (dt$`PEAK-OFF PEAK` == "PK" |dt$`PEAK-OFF PEAK` == "P"))]))
print(sd(dt$PUN[which(dt$`AEEG 181/06` == "F3" & dt$`PEAK-OFF PEAK` == "OP")]))


dtx <- data.table(read_excel("longterm_pun.xlsx"))
#dtx <- df2

par(mfrow = c(3,2))
hist(dtx$pun[which(dtx$AEEG.181.06 == "F1" & (dtx$PK.OP == "PK" | dtx$PK.OP == "P"))], breaks = 20, col = 'red2', main = "AP L(F1|PK)")
hist(dtx$pun[which(dtx$AEEG.181.06 == "F2" & (dtx$PK.OP == "PK" | dtx$PK.OP == "P"))], breaks = 20, col = 'magenta', main = "AP L(F2|PK)")
hist(dtx$pun[which(dtx$AEEG.181.06 == "F2" & dtx$PK.OP == "OP")], breaks = 20, col = 'grey', main = "AP L(F2|OP)")
hist(dtx$pun[which(dtx$AEEG.181.06 == "F3" & (dtx$PK.OP == "PK" | dtx$PK.OP == "P"))], breaks = 20, col = 'green', main = "AP L(F3|PK)")
hist(dtx$pun[which(dtx$AEEG.181.06 == "F3" & dtx$PK.OP == "OP")], breaks = 20, col = 'navy', main = "AP L(F3|OP)")

quantile(dtx$pun[which(dtx$AEEG.181.06 == "F1" & (dtx$PK.OP == "PK" | dtx$PK.OP == "P"))], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dtx$pun[which(dtx$AEEG.181.06 == "F2" & (dtx$PK.OP == "PK" | dtx$PK.OP == "P"))], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dtx$pun[which(dtx$AEEG.181.06 == "F2" & dtx$PK.OP == "OP")], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dtx$pun[which(dtx$AEEG.181.06 == "F3" & (dtx$PK.OP == "PK" | dtx$PK.OP == "P"))], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dtx$pun[which(dtx$AEEG.181.06 == "F3" & dtx$PK.OP == "OP")], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))

sd(dtx$pun[which(dtx$AEEG.181.06 == "F1" & (dtx$PK.OP == "PK" | dtx$PK.OP == "P"))])
sd(dtx$pun[which(dtx$AEEG.181.06 == "F2" & (dtx$PK.OP == "PK" | dtx$PK.OP == "P"))])
sd(dtx$pun[which(dtx$AEEG.181.06 == "F2" & dtx$PK.OP == "OP")])
sd(dtx$pun[which(dtx$AEEG.181.06 == "F3" & (dtx$PK.OP == "PK" | dtx$PK.OP == "P"))])
sd(dtx$pun[which(dtx$AEEG.181.06 == "F3" & dtx$PK.OP == "OP")])

par(mfrow = c(3,1))
hist(dtx$pun[which(dtx$AEEG.181.06 == "F1")], breaks = 20, col = 'red', main = "AP L(F1)")
hist(dtx$pun[which(dtx$AEEG.181.06 == "F2")], breaks = 20, col = 'magenta', main = "AP L(F2)")
hist(dtx$pun[which(dtx$AEEG.181.06 == "F3")], breaks = 20, col = 'green', main = "AP L(F3)")


dtk <- data.table(read_excel("Copy of FWD2017-2018.xlsx"))
colnames(dtk)[1] <- "date"

par(mfrow = c(3,2))
hist(dtk$ITALIA[which(dtk$Fasce == "F1" & (dtk$`PK/OP` == "PK" | dtk$`PK/OP` == "P"))], breaks = 20, col = 'red', main = "K2E L(F1|PK)")
hist(dtk$ITALIA[which(dtk$Fasce == "F2" & (dtk$`PK/OP` == "PK" | dtk$`PK/OP` == "P"))], breaks = 20, col = 'magenta', main = "K2E L(F2|PK)")
hist(dtk$ITALIA[which(dtk$Fasce == "F2" & dtk$`PK/OP` == "OP")], breaks = 20, col = 'grey', main = "K2E L(F2|OP)")
hist(dtk$ITALIA[which(dtk$Fasce == "F3" & (dtk$`PK/OP` == "PK" | dtk$`PK/OP` == "P"))], breaks = 20, col = 'green', main = "K2E L(F3|PK)")
hist(dtk$ITALIA[which(dtk$Fasce == "F3" & dtk$`PK/OP` == "OP")], breaks = 20, col = 'navy', main = "K2E L(F3|OP)")

quantile(dtk$ITALIA[which(dtk$Fasce == "F1" & (dtk$`PK/OP` == "PK" | dtk$`PK/OP` == "P"))], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dtk$ITALIA[which(dtk$Fasce == "F2" & (dtk$`PK/OP` == "PK" | dtk$`PK/OP` == "P"))], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dtk$ITALIA[which(dtk$Fasce == "F2" & dtk$`PK/OP` == "OP")], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dtk$ITALIA[which(dtk$Fasce == "F3" & (dtk$`PK/OP` == "PK" | dtk$`PK/OP` == "P"))], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dtk$ITALIA[which(dtk$Fasce == "F3" & dtk$`PK/OP` == "OP")], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))

sd(dtk$ITALIA[which(dtk$Fasce == "F1" & (dtk$`PK/OP` == "PK" | dtk$`PK/OP` == "P"))])
sd(dtk$ITALIA[which(dtk$Fasce == "F2" & (dtk$`PK/OP` == "PK" | dtk$`PK/OP` == "P"))])
sd(dtk$ITALIA[which(dtk$Fasce == "F2" & dtk$`PK/OP` == "OP")])
sd(dtk$ITALIA[which(dtk$Fasce == "F3" & (dtk$`PK/OP` == "PK" | dtk$`PK/OP` == "P"))])
sd(dtk$ITALIA[which(dtk$Fasce == "F3" & dtk$`PK/OP` == "OP")])

par(mfrow = c(3,1))
hist(dtk$ITALIA[which(dtk$Fasce == "F1")], breaks = 20, col = 'red', main = "K2E L(F1)")
hist(dtk$ITALIA[which(dtk$Fasce == "F2")], breaks = 20, col = 'magenta', main = "K2E L(F2)")
hist(dtk$ITALIA[which(dtk$Fasce == "F3")], breaks = 20, col = 'green', main = "K2E L(F3)")

###### "distance" (KL-Divergence) between the distribution -- Hellinger Distance ####
HellingerDistance <- function(dist1, dist2)
{
 d1 <- density(dist1)
 d2 <- density(dist2)
 m <- min(d1$x, d2$x)
 M <- max(d1$x, d2$x)
 d1 <- density(dist1, from = m, to = M, n = 1024)
 d2 <- density(dist2, from = m, to = M, n = 1024)
 
 dS <- (sqrt(d1$y) - sqrt(d2$y))^2
 
 I <- 0
 for(i in 1:(length(d1$x)-1))
 {
  I <- I + (dS[i+1] + dS[i])*(d1$x[i+1] - d1$x[i])/2  
 }
 
 return((1/2)*I)
 
}
###################################################################################
### I.T.O. PK/OP:

### AP vs real2017:
HellingerDistance(real$PUN[which(real$`PEAK-OFF PEAK` == "PK" | real$`PEAK-OFF PEAK` == "P")], dtx$pun[which(dtx$PK.OP == "PK" | dtx$PK.OP == "P")])
HellingerDistance(real$PUN[which(real$`PEAK-OFF PEAK` == "OP")], dtx$pun[which(dtx$PK.OP == "OP")])

### K2E vs real2017:
HellingerDistance(real$PUN[which(real$`PEAK-OFF PEAK` == "PK" | real$`PEAK-OFF PEAK` == "P")], dtk$ITALIA[which(dtk$`PK/OP` == "PK" | dtk$`PK/OP` == "P")])
HellingerDistance(real$PUN[which(real$`PEAK-OFF PEAK` == "OP")], dtk$ITALIA[which(dtk$`PK/OP` == "OP")])

### I.T.O. F1, F2, F3:

### AP vs real2017:
HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F1")], dtx$pun[which(dtx$AEEG.181.06 == "F1")])
HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F2")], dtx$pun[which(dtx$AEEG.181.06 == "F2")])
HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F3")], dtx$pun[which(dtx$AEEG.181.06 == "F3")])

### K2E vs real2017:
HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F1")], dtk$ITALIA[which(dtk$Fasce == "F1")])
HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F2")], dtk$ITALIA[which(dtk$Fasce == "F2")])
HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F3")], dtk$ITALIA[which(dtk$Fasce == "F3")])

### I.T.O. PK/OP divided in F1, F2, F3:

HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F1" & (real$`PEAK-OFF PEAK` == "PK" | real$`PEAK-OFF PEAK` == "P"))],
                                 dtx$pun[which(dtx$AEEG.181.06 == "F1" & (dtx$PK.OP == "PK" | dtx$PK.OP == "P"))])
HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F2" & (real$`PEAK-OFF PEAK` == "PK" | real$`PEAK-OFF PEAK` == "P"))],
                  dtx$pun[which(dtx$AEEG.181.06 == "F2" & (dtx$PK.OP == "PK" | dtx$PK.OP == "P"))])
HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F2" & real$`PEAK-OFF PEAK` == "OP")],
                  dtx$pun[which(dtx$AEEG.181.06 == "F2" & dtx$PK.OP == "OP")])
HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F3" & (real$`PEAK-OFF PEAK` == "PK" | real$`PEAK-OFF PEAK` == "P"))],
                  dtx$pun[which(dtx$AEEG.181.06 == "F3" & (dtx$PK.OP == "PK" | dtx$PK.OP == "P"))])
HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F3" & real$`PEAK-OFF PEAK` == "OP")],
                  dtx$pun[which(dtx$AEEG.181.06 == "F3" & dtx$PK.OP == "OP")])


HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F1" & (real$`PEAK-OFF PEAK` == "PK" | real$`PEAK-OFF PEAK` == "P"))],
                  dtk$ITALIA[which(dtk$Fasce == "F1" & (dtk$`PK/OP` == "PK" | dtk$`PK/OP` == "P"))])
HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F2" & (real$`PEAK-OFF PEAK` == "PK" | real$`PEAK-OFF PEAK` == "P"))],
                  dtk$ITALIA[which(dtk$Fasce == "F2" & (dtk$`PK/OP` == "PK" | dtk$`PK/OP` == "P"))])
HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F2" & real$`PEAK-OFF PEAK` == "OP")],
                  dtk$ITALIA[which(dtk$Fasce == "F2" & dtk$`PK/OP` == "OP")])
HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F3" & (real$`PEAK-OFF PEAK` == "PK" | real$`PEAK-OFF PEAK` == "P"))],
                  dtk$ITALIA[which(dtk$Fasce == "F3" & (dtk$`PK/OP` == "PK" | dtk$`PK/OP` == "P"))])
HellingerDistance(real$PUN[which(real$`AEEG 181/06` == "F3" & real$`PEAK-OFF PEAK` == "OP")],
                  dtk$ITALIA[which(dtk$Fasce == "F3" & dtk$`PK/OP` == "OP")])



### day-by-day: 1 = "Sunday", ..., 7 = "Saturday"

HellingerDistance(filterDay(data.frame(real), 1)$PUN, filterDay(data.frame(dtx),1)$pun)
HellingerDistance(filterDay(data.frame(real), 2)$PUN, filterDay(data.frame(dtx),2)$pun)
HellingerDistance(filterDay(data.frame(real), 3)$PUN, filterDay(data.frame(dtx),3)$pun)
HellingerDistance(filterDay(data.frame(real), 4)$PUN, filterDay(data.frame(dtx),4)$pun)
HellingerDistance(filterDay(data.frame(real), 5)$PUN, filterDay(data.frame(dtx),5)$pun)
HellingerDistance(filterDay(data.frame(real), 6)$PUN, filterDay(data.frame(dtx),6)$pun)
HellingerDistance(filterDay(data.frame(real), 7)$PUN, filterDay(data.frame(dtx),7)$pun)

HellingerDistance(filterDay(data.frame(real), 1)$PUN, filterDay(data.frame(dtk),1)$ITALIA)
HellingerDistance(filterDay(data.frame(real), 2)$PUN, filterDay(data.frame(dtk),2)$ITALIA)
HellingerDistance(filterDay(data.frame(real), 3)$PUN, filterDay(data.frame(dtk),3)$ITALIA)
HellingerDistance(filterDay(data.frame(real), 4)$PUN, filterDay(data.frame(dtk),4)$ITALIA)
HellingerDistance(filterDay(data.frame(real), 5)$PUN, filterDay(data.frame(dtk),5)$ITALIA)
HellingerDistance(filterDay(data.frame(real), 6)$PUN, filterDay(data.frame(dtk),6)$ITALIA)
HellingerDistance(filterDay(data.frame(real), 7)$PUN, filterDay(data.frame(dtk),7)$ITALIA)

#####################################################################################################################
Redimensioner <- function(ph, mh, mw, from, to)
{
  #### @BRIEF: if what == PK => mw is referring to PK
  d_f <- data_frame()
  from <- as.Date(from)
  to <- as.Date(to)
  nOP <- nrow(ph[which(as.Date(ph$Date) >= from & as.Date(ph$Date) <= to & ph$`PEAK-OFF PEAK` == "OP"),])
  nPK <- nrow(ph[which(as.Date(ph$Date) >= from & as.Date(ph$Date) <= to & ph$`PEAK-OFF PEAK` == "PK"),])
  rOP <- which(as.Date(ph$Date) >= from & as.Date(ph$Date) <= to & ph$`PEAK-OFF PEAK` == "OP")
  rPK <- which(as.Date(ph$Date) >= from & as.Date(ph$Date) <= to & (ph$`PEAK-OFF PEAK` == "PK" | ph$`PEAK-OFF PEAK` == "P"))
  M <- nOP + nPK
  
  periodpk <- ph[rPK,]
  periodop <- ph[rOP,]
 
  
  opm <- (1/nOP)*((mh*M) - (mw*nPK))
    
    
  pihatpk <- (mw)/((1/nPK)*sum(periodpk$PUN))
  pihatop <- (opm)/((1/nOP)*sum(periodop$PUN))
  for(i in 1:length(rPK))
  {
    ph[rPK[i], "PUN"] <- pihatpk * unlist(ph[rPK[i], "PUN"])
  }
  for(i in 1:length(rOP))
  {
    ph[rOP[i], "PUN"] <- pihatop * unlist(ph[rOP[i], "PUN"])
  }
  
  
  return(ph)
}
#################################################################################################################################


F1 <- c() 
pk <- c()
bsl <- c()
Y <- c()
for( x in 1:200)
{
  y <- rnorm(n = 1, mean = 60.64, sd = 15.43)
  if(y > 60.64)
  {
    dt <- Redimensioner(dt, 60.64, y, from = '2015-01-01', to = '2015-01-31')
    F1 <- c(F1, mean(dt$PUN[which(dt$Month == 1 & dt$`AEEG 181/06` == "F3")]))
    pk <- c(pk, mean(dt$PUN[which(dt$Month == 1 & dt$`PEAK-OFF PEAK` == "OP")]))
    bsl <- c(bsl, mean(dt$PUN[which(dt$Month == 1)]))
    Y <- c(Y, y)
  }
}

plot(bsl, F1, pch = 16, col = "red")
plot(Y, F1, pch = 16, col = "blue")
plot(pk, F1, pch = 16, col = "green")



F1 <- c() 
F2 <- c() 
F3 <- c() 
op <- c()
pk <- c()
bsl <- c()
Y <- c()
for(m in 1:12)
{
  F1 <- c(F1, mean(dt$PUN[which(dt$Month == m & dt$`AEEG 181/06` == "F1")]))
  F2 <- c(F2, mean(dt$PUN[which(dt$Month == m & dt$`AEEG 181/06` == "F2")]))
  F3 <- c(F3, mean(dt$PUN[which(dt$Month == m & dt$`AEEG 181/06` == "F3")]))
  pk <- c(pk, mean(dt$PUN[which(dt$Month == m & dt$`PEAK-OFF PEAK` == "PK")]))
  op <- c(op, mean(dt$PUN[which(dt$Month == m & dt$`PEAK-OFF PEAK` == "OP")]))
  bsl <- c(bsl, mean(dt$PUN[which(dt$Month == m)]))
}

DF <- data.frame(f1 = F1, f2 = F2, f3 = F3, P = pk, O = op, B = bsl)

pairs(DF)
