

library(WriteXLS)
Sys.getenv("JAVA_HOME")
Sys.setenv(JAVA_HOME='C:/Programmi (x86)/Java/jre1.8.0_77')
library(rJava)
library(xlsx)

#churn <- read.xlsx2("X:/2016_Situazione aziende in uscita e nuovi ingressi/2016-04-11_RECESSI_GAS.xlsx", 2, colClasses="character")
churn <- read.xlsx2("rec.xlsx", 1, colClasses="character")
#churn2 <- read.xlsx2("rec.xlsx", 2, colClasses="character")
churn <- as.character(churn)

rows <- which(churn["DURATA.DELLA.FORNITURA..MESI."] != "")

typeof(unlist(churn["DURATA.DELLA.FORNITURA..MESI."]))

churn <- churn[rows, ]

dim(churn) # 16 churners

durata <- as.numeric(as.character(unlist(churn["DURATA.DELLA.FORNITURA..MESI."])))

table(durata)

breaks <- c(3.5,4.5,8.5,9.5,10.5,12.5,13.5,18.5,20.5,21.5,24.5,25.5)
hist(durata, breaks = breaks, freq = TRUE)

barplot(durata)
plot(density(durata))

den <- density(durata)
den$x
den$y
max(den$y) # about 0.05
mode(den$x)
median(den$x)
mean(den$x)
den$x[which(den$y == max(den$y))] ### mode of the distribution: 9.81, mean = median = 14.5 ---> sd = 6.89 months

#### percentage of churners below break-even point:
length(which(durata <= 12))/length(durata) ## 0.4375 






