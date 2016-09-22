###### weights optimization for bagged prediction ######

library(readxl)
library(h2o)

source("C://Users//utente//Documents//prediction2.R")
source("C://Users//utente//Documents//R_code//prediction_for_glm.R")
source("C://Users//utente//Documents//R_code//functions_for_PPIA_server.R")

h2o.init(nthreads = -1, max_mem_size = '20g')

pp <- read_excel("C:\\Users\\utente\\Documents\\PUN\\DB_Borse_Elettriche_PER MI.xlsx", sheet = "DB_Dati")

ds <- seq.Date(from = as.Date("2016-09-09"), to = Sys.Date(), by = "day")

Q <- QMDL <- QMglm <- c()

untime <- maply(1:nrow(pp), function(n) unlist(pp[n,1]))
utc <- as.POSIXct(untime, origin = "1970-01-01")

for(d in ds)
{
  d <- as.Date(d, origin = "1970-01-01")
  print(as.character(d))
  mm <- mean(unlist(pp[which(as.Date(utc, origin = "1970-01-01") == d),13]), na.rm = TRUE)
  Q <- c(Q, mm)
  pred_dl <- prediction(as.character(d))
  QMDL <- c(QMDL, colMeans(pred_dl)[2])
  rm(pred_dl)
  pred_glm <- predict_with_glm(as.character(d))
  QMglm <- c(QMglm, colMeans(pred_glm)[1])
  rm(pred_glm)
}

opt <- c()
x <- seq(0, 1, 0.0001)
for(q in x)
{
  tr <- mean((Q - q*(QMDL - QMglm) - QMglm)^2)
  opt <- c(opt, tr)
}

plot(x, opt, type="l", lwd = 2, col = sample.int(100, size = 1))

min(opt) ### <- minimum
x[which(opt == min(opt))] ### <- point of minimum

mean(Q - QMDL)
mean(Q - QMglm)

