##### correlation meteo - pun ###

library(readxl)
library(dplyr)

source("C://Users//utente//Documents//R_code//functions_for_PUN_server.R")

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

