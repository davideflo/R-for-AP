########## test for prediction glm ########

source("C://Users//utente//Documents//R_code//prediction_for_glm.R")
source("C://Users//utente//Documents//R_code//functions_for_PPIA_server.R")

date <- "2016-09-16"

met <- build_meteo_new(date)

pp <- read_excel("C:\\Users\\utente\\Documents\\PUN\\DB_Borse_Elettriche_PER MI.xlsx", sheet = "DB_Dati")

pn <- build_new(pp, 2)

apm <- assemble_pm_glm(pn, met)

sum(is.na(apm)) ## 0! OK 

res <- predict_with_glm(date)
