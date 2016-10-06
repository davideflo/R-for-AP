#### test prediction2

source("C://Users//utente//Documents//prediction2.R")
source("C://Users//utente//Documents//R_code//functions_for_PPIA_server.R")

### se lancio piu volte il java aumenta la memoria ?
h2o.init(nthreads = -1, max_mem_size = '20g')

date <- "2016-10-06"

met <- build_meteo_new(date)

pp <- read_excel("C:\\Users\\utente\\Documents\\PUN\\DB_Borse_Elettriche_PER MI.xlsx", sheet = "DB_Dati")

pn <- build_new(pp)

apm <- assemble_pm(pn, met)

sum(is.na(apm)) ## 0! OK 

start <- Sys.time()
res <- prediction(date)
colMeans(res)
xlsx::write.xlsx(res,paste0("prediction_PUN_",date,".xlsx"), row.names = FALSE, col.names = TRUE)
end <- Sys.time()
print(end-start)

pp <- read_excel("C:\\Users\\utente\\Documents\\PUN\\DB_Borse_Elettriche_PER MI.xlsx", sheet = "DB_Dati")

pn <- build_new_average(pp)

apm <- assemble_pm_average(pn, met)
AVres <- prediction_average(date) 

res2 <- prediction_weekend(date)

#########################################################
 
start <- Sys.time()
test <- get_Prediction(date)
end <- Sys.time()
end - start

start <- Sys.time()
test2 <- get_Prediction(as.Date(date), TRUE)
end <- Sys.time()
end - start

hm <- call_corrector_hourly(get_last_month(pp,date),date)
gm <- call_corrector_mean(get_last_month(pp,date),date)
res2 <- res[,2] + hm
