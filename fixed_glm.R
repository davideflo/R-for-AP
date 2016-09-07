###### generate all glm datasets #######

library(readxl)
library(plyr)
library(dplyr)

source("C://Users//utente//Documents//R_code//functions_for_PPIA_server.R")
source("C://Users//utente//Documents//R_code//functions_for_PUN_server.R")
source("C://Users//utente//Documents//glm_dataset.R")


prices16 <- read_excel("C:/Users/utente/Documents/PUN/Anno 2016_08.xlsx", sheet="Prezzi-Prices")

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

meteoav16 <- mediate_meteos(mi6, ro6, fi6, pa6, ca6, rc6, FALSE)

library(h2o)

create_glm_dataset(prices16, "PUN", 24, 1, meteoav16, 1)

generate_glm_datasets(prices16, meteoav16)

test <- read.csv2("C:/Users/utente/Documents/PUN/Default Dataset_2016-09-07.csv", header = FALSE, sep = ",", colClasses = "character", stringsAsFactors = FALSE)
