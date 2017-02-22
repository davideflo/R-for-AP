##### analysis correlation hourly less pivotals to non-hourly
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(gplots)
library(feather)
library(lubridate)
library(data.table)
library(fda)

source("C://Users//utente//Documents//R_code//functions_for_corr_meteo_pun.R")
source("C://Users//utente//Documents//R_code//functions_for_POD_orari.R")
source("C://Users//utente//Documents//glm_dataset.R")
source("R_code/similar_days_model.R")


terna <- read_excel("C:\\Users\\utente\\Documents\\misure\\aggregato_sbilanciamento.xlsx")
#dttm <- as.POSIXct( strptime(terna$`DATA RIFERIMENTO CORRISPETTIVO`, '%d/%m/%Y %H:%M', tz = "CET"))
#terna$`DATA RIFERIMENTO CORRISPETTIVO`  <- dttm

agg <- get_Table_similar_days2(terna, "CNOR", "FABBISOGNO REALE")
mo <- get_Table_similar_days2(terna, "CNOR", "MO [MWh]")
mno <- data.frame(agg[,1:6], agg[,7:30] + mo[,7:30])

pivotali <- read_excel('pivotali.xlsx', sheet = "cnord")

cnord <- as.data.frame(read_feather("C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_cnord"))

pivcnord <- cnord[cnord$Pod %in% pivotali$`PIVOTALI CNORD`,]
pivcnord$Giorno <- strftime(pivcnord$Giorno, format = "%Y-%m-%d")
aggpivcn <- HourlyAggregator2(pivcnord)

pivcn <- data_frame()
days <- seq.Date(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "day")
for(d in days)
{
  d <- as.Date(d, origin="1970-01-01")
  atd <- colSums(aggpivcn[which(aggpivcn$Giorno == as.Date(d)),3:26])
  d_f <- data.frame(as.Date(d), t(atd))
  pivcn <- bind_rows(pivcn, d_f)
}
colnames(pivcn) <- c('Giorno', as.character(1:24))

matplot(t(pivcn[,2:25]/1000), type = "l")

mo <- mo[which(lubridate::year(mo$date) == 2016),c(1,7:30)]
mo[,c(2:25)] <- (-1)*mo[,c(2:25)]

diffmo <- data.frame(mo$date, mo[,c(2:25)] - pivcn[,2:25]/1000)

matplot(t(diffmo[,2:25]), type = "l")

# NO <- as.matrix(mno[,2:25])
# O <- as.matrix(mo[,2:25])
# 
# Fb <-  create.fourier.basis(c(1,24), nbasis=23)
# NOb <- smooth.basis(1:24, t(NO), Fb)$fd
# Ob <- smooth.basis(1:24, t(O), Fb)$fd
# 
# 
# ccm <- cor.fd(1:24, NOb, 1:24, Ob)
# library(plot3D)
# m3 <- mesh(seq(1, 24, length.out = 24),seq(1, 24, length.out = 24))
# surf3D(m3$x, m3$y, ccm)#, colvar = Bs, colkey = FALSE, facets = FALSE)

mno <- mno[which(lubridate::year(mno$date) == 2016),c(1,7:30)]
diag(cor(mo[,c(2:25)], mno[,c(2:25)]))
barplot(diag(cor(mo[,c(2:25)], mno[,c(2:25)])), names.arg = as.character(1:24), las = 2)

tspivcn <- c()
for(i in 1:nrow(pivcn))
{
  tspivcn <- c(tspivcn, unlist(pivcn[i,2:25]/1000))
}

plot(tspivcn, type = "l")

plot(stl(ts(tspivcn, frequency = 24), s.window = "per"))

greatest <- aggpivcn[which(aggpivcn$Pod == aggpivcn$Pod[1]),]
tsg <- c()
for(i in 1:nrow(pivcn))
{
  tsg <- c(tsg, unlist(greatest[i,3:26]/1000))
}

plot(tsg, type = "l", col = "blue")

plot(dec <- stl(ts(tsg, frequency = 24), s.window = "per"), col = "blue")
plot(dec$time.series[1:24,1], type = "l", col = "blue")
