#### test prediction2

source("C://Users//utente//Documents//prediction2.R")

date <- "2016-08-29"

met <- build_meteo_new(date)


pp <- read_excel("C:\\Users\\utente\\Documents\\PUN\\DB_Borse_Elettriche_PER MI.xlsx", sheet = "DB_Dati")

pn <- build_new(pp)

apm <- assemble_pm(pn, met)
