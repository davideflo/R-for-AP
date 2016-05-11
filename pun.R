
library(WriteXLS)
library(rJava)
library(openxlsx)
library(TSA)

prices <- read.xlsx("Anno 2016_03.xlsx", sheet=1, colNames=TRUE)

pun <- unlist(prices["PUN"])
pun
typeof(pun)
plot(1:length(pun), pun, type = "l")
acf(pun)
stl(pun)
