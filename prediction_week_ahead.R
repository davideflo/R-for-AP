################### prediction week ahead ################

library(h2o)

source("C://Users//utente//Documents//fwp.R")

date <- "2016-10-03"

met <- build_meteo_week(date)
met2 <- build_meteo_set(date)

pp <- read_excel("C:\\Users\\utente\\Documents\\PUN\\DB_Borse_Elettriche_PER MI.xlsx", sheet = "DB_Dati")

pn <- build_new_week(pp)

apm <- assemble_pm_week(pp, date)

apm[is.na(apm)] <- 0
apm[is.infinite(unlist(apm))] <- 0
apm[is.null(apm)] <- 0

sum(is.na(apm)) ## 0! OK 

start <- Sys.time()
res3 <- prediction_week(date) + 10
end <- Sys.time()
end - start