##### confronto curve pun forward
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(feather)
library(lubridate)


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

real <- data.table(read_excel("DB_Borse_Elettriche_PER MI_17_conMacro - Copy.xlsm", sheet = 2))

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


dt <- data.table(read_excel("longterm_pun.xlsx"))

par(mfrow = c(3,2))
hist(dt$pun[which(dt$AEEG.181.06 == "F1" & (dt$PK.OP == "PK" | dt$PK.OP == "P"))], breaks = 20, col = 'red', main = "AP L(F1|PK)")
hist(dt$pun[which(dt$AEEG.181.06 == "F2" & (dt$PK.OP == "PK" | dt$PK.OP == "P"))], breaks = 20, col = 'magenta', main = "AP L(F2|PK)")
hist(dt$pun[which(dt$AEEG.181.06 == "F2" & dt$PK.OP == "OP")], breaks = 20, col = 'grey', main = "AP L(F2|OP)")
hist(dt$pun[which(dt$AEEG.181.06 == "F3" & (dt$PK.OP == "PK" | dt$PK.OP == "P"))], breaks = 20, col = 'green', main = "AP L(F3|PK)")
hist(dt$pun[which(dt$AEEG.181.06 == "F3" & dt$PK.OP == "OP")], breaks = 20, col = 'navy', main = "AP L(F3|OP)")

quantile(dt$pun[which(dt$AEEG.181.06 == "F1" & (dt$PK.OP == "PK" | dt$PK.OP == "P"))], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dt$pun[which(dt$AEEG.181.06 == "F2" & (dt$PK.OP == "PK" | dt$PK.OP == "P"))], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dt$pun[which(dt$AEEG.181.06 == "F2" & dt$PK.OP == "OP")], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dt$pun[which(dt$AEEG.181.06 == "F3" & (dt$PK.OP == "PK" | dt$PK.OP == "P"))], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dt$pun[which(dt$AEEG.181.06 == "F3" & dt$PK.OP == "OP")], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))

sd(dt$pun[which(dt$AEEG.181.06 == "F1" & (dt$PK.OP == "PK" | dt$PK.OP == "P"))])
sd(dt$pun[which(dt$AEEG.181.06 == "F2" & (dt$PK.OP == "PK" | dt$PK.OP == "P"))])
sd(dt$pun[which(dt$AEEG.181.06 == "F2" & dt$PK.OP == "OP")])
sd(dt$pun[which(dt$AEEG.181.06 == "F3" & (dt$PK.OP == "PK" | dt$PK.OP == "P"))])
sd(dt$pun[which(dt$AEEG.181.06 == "F3" & dt$PK.OP == "OP")])

par(mfrow = c(3,1))
hist(dt$pun[which(dt$AEEG.181.06 == "F1")], breaks = 20, col = 'red', main = "AP L(F1)")
hist(dt$pun[which(dt$AEEG.181.06 == "F2")], breaks = 20, col = 'magenta', main = "AP L(F2)")
hist(dt$pun[which(dt$AEEG.181.06 == "F3")], breaks = 20, col = 'green', main = "AP L(F3)")


dt <- data.table(read_excel("Copy of FWD2017-2018.xlsx"))

par(mfrow = c(3,2))
hist(dt$ITALIA[which(dt$Fasce == "F1" & (dt$`PK/OP` == "PK" | dt$`PK/OP` == "P"))], breaks = 20, col = 'red', main = "K2E L(F1|PK)")
hist(dt$ITALIA[which(dt$Fasce == "F2" & (dt$`PK/OP` == "PK" | dt$`PK/OP` == "P"))], breaks = 20, col = 'magenta', main = "K2E L(F2|PK)")
hist(dt$ITALIA[which(dt$Fasce == "F2" & dt$`PK/OP` == "OP")], breaks = 20, col = 'grey', main = "K2E L(F2|OP)")
hist(dt$ITALIA[which(dt$Fasce == "F3" & (dt$`PK/OP` == "PK" | dt$`PK/OP` == "P"))], breaks = 20, col = 'green', main = "K2E L(F3|PK)")
hist(dt$ITALIA[which(dt$Fasce == "F3" & dt$`PK/OP` == "OP")], breaks = 20, col = 'navy', main = "K2E L(F3|OP)")

quantile(dt$ITALIA[which(dt$Fasce == "F1" & (dt$`PK/OP` == "PK" | dt$`PK/OP` == "P"))], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dt$ITALIA[which(dt$Fasce == "F2" & (dt$`PK/OP` == "PK" | dt$`PK/OP` == "P"))], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dt$ITALIA[which(dt$Fasce == "F2" & dt$`PK/OP` == "OP")], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dt$ITALIA[which(dt$Fasce == "F3" & (dt$`PK/OP` == "PK" | dt$`PK/OP` == "P"))], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))
quantile(dt$ITALIA[which(dt$Fasce == "F3" & dt$`PK/OP` == "OP")], probs = c(0.5,0.6,0.7,0.8,0.9,0.95))

sd(dt$ITALIA[which(dt$Fasce == "F1" & (dt$`PK/OP` == "PK" | dt$`PK/OP` == "P"))])
sd(dt$ITALIA[which(dt$Fasce == "F2" & (dt$`PK/OP` == "PK" | dt$`PK/OP` == "P"))])
sd(dt$ITALIA[which(dt$Fasce == "F2" & dt$`PK/OP` == "OP")])
sd(dt$ITALIA[which(dt$Fasce == "F3" & (dt$`PK/OP` == "PK" | dt$`PK/OP` == "P"))])
sd(dt$ITALIA[which(dt$Fasce == "F3" & dt$`PK/OP` == "OP")])

par(mfrow = c(3,1))
hist(dt$ITALIA[which(dt$Fasce == "F1")], breaks = 20, col = 'red', main = "K2E L(F1)")
hist(dt$ITALIA[which(dt$Fasce == "F2")], breaks = 20, col = 'magenta', main = "K2E L(F2)")
hist(dt$ITALIA[which(dt$Fasce == "F3")], breaks = 20, col = 'green', main = "K2E L(F3)")






