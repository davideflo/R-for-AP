#### test prediction2

source("C://Users//utente//Documents//prediction2.R")
source("C://Users//utente//Documents//R_code//functions_for_PPIA_server.R")


h2o.init(nthreads = -1, max_mem_size = '20g')

date <- "2016-09-05"

met <- build_meteo_new(date)

pp <- read_excel("C:\\Users\\utente\\Documents\\PUN\\DB_Borse_Elettriche_PER MI.xlsx", sheet = "DB_Dati")

pn <- build_new(pp)

apm <- assemble_pm(pn, met)

sum(is.na(apm)) ## 0! OK 

start <- Sys.time()
res <- prediction(date) ### 6.153753 mins --- 9.241325 mins with bootstrap CIs
end <- Sys.time()
print(end-start)

#res <- data.frame(res)
#rownames(res) <- c(1:24)
#colnames(res) <- paste(as.character(Sys.Date() + 1:5))

xlsx::write.xlsx(res,paste0("prediction_PUN_",date,".xlsx"), row.names = FALSE, col.names = TRUE)


#### BOOTSTRAP WITH ERROR DATABASE
for(i in 1:24)
{
  print(paste0("CI for 1 day ahead and step", i))
  bc <- bootstrap_f_r_errors(res[i,2], i, 1)
  print(paste("L =", bc[1], "yp", res[i,2], "U =", bc[2]))
}

#### BOOTSTRAP WITH PUN DATABASE
for(i in 1:24)
{
  print(paste0("CI for 1 day ahead and step", i))
  bc <- bootstrap_f_r(res[i,1], i, 1)
  print(paste("L =", bc[1], "yp", res[i,1], "U =", bc[2]))
}

