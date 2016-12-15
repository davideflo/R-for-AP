##### correlation meteo - pun ###

library(readxl)
library(dplyr)
library(gamair)

source("C://Users//utente//Documents//R_code//functions_for_PUN_server.R")
source("C://Users//utente//Documents//R_code//functions_for_corr_meteo_pun.R")
source("C://Users//utente//Documents//prediction2.R")
source("C://Users//utente//Documents//glm_dataset.R")


pun16 <- read_excel("C:/Users/utente/Documents/PUN/Anno 2016_08.xlsx", sheet="Prezzi-Prices")

mi6 <- read_excel("C:/Users/utente/Documents/PUN/Milano 2016.xlsx", sheet= 1)
ro6 <- read_excel("C:/Users/utente/Documents/PUN/Roma 2016.xlsx", sheet= 1)
fi6 <- read_excel("C:/Users/utente/Documents/PUN/Firenze 2016.xlsx", sheet= 1)
pa6 <- read_excel("C:/Users/utente/Documents/PUN/Palermo 2016.xlsx", sheet= 1)
ca6 <- read_excel("C:/Users/utente/Documents/PUN/Cagliari 2016.xlsx", sheet= 1)
rc6 <- read_excel("C:/Users/utente/Documents/PUN/Reggio Calabria 2016.xlsx", sheet= 1)

mi6 <- get_meteo(mi6)
ro6 <- get_meteo(ro6)
fi6 <- get_meteo(fi6)
pa6 <- get_meteo(pa6)
ca6 <- get_meteo(ca6)
rc6 <- get_meteo(rc6)

meteoav16 <- data.frame(mediate_meteos(mi6, ro6, fi6, pa6, ca6, rc6, FALSE))

dailyp <- c()
for(i in 1:nrow(meteoav16))
{
  days <- dates(unlist(pun16[,1]))
  atday <- pun16[which(days == meteoav16[i,1]),"PUN"]
  dailyp <- c(dailyp, mean(unlist(atday)))
}

df <- data.frame(dailyp, meteoav16[,2:6])

pairs(df)

### or try this: http://stats.stackexchange.com/questions/9506/stl-trend-of-time-series-using-r
plot(se <- stl(ts(unlist(mi6["Tmedia"]), frequency = 1),s.window=9))
##############################################################################################

dm <- read.csv2("PUN/daily_means_2016.csv", sep = ",", header= TRUE, colClasses = "character", stringsAsFactors = FALSE)
dm <- as.numeric(unlist(dm[,2]))

plot(dm, type="l")

#within(dm, Date <- as.Date(paste(year, month, day.of.month, sep = "-")))
Date <- seq.Date(from=as.Date('2016-01-01'), to = as.Date('2016-08-31'), by='day')
DM <- data.frame(Date, dm, c(1:length(dm)))
colnames(DM) <- c("Date", "daily_mean", "day.of.year")

fit <- gamm(daily_mean ~ s(day.of.year, bs = "cc"), data = DM)

wseas <- find_weekly_seasonality(DM)
plot(wseas, type= "l", col = "blue")

pp <- read_excel("C:\\Users\\utente\\Documents\\PUN\\DB_Borse_Elettriche_PER MI.xlsx", sheet = "DB_Dati")

pp_last <- get_last_month(pp, Sys.Date())
seasonDB <- find_weekly_seasonality_DB(pp_last)
plot(seasonDB, type="l", col="red")
points(seasonDB,pch=16,col="black")

sdDB <- find_daily_sd_DB(pp_last)
