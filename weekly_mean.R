#### analysis on weekly means ####

source("C://Users//utente//Documents//R_code//prediction_for_glm.R")
source("C://Users//utente//Documents//R_code//functions_for_PPIA_server.R")

pp <- read_excel("C:\\Users\\utente\\Documents\\PUN\\DB_Borse_Elettriche_PER MI.xlsx", sheet = "DB_Dati")

ds <- seq.Date(from = as.Date("2016-01-01"), to = Sys.Date(), by = "day")

untime <- maply(1:nrow(pp), function(n) unlist(pp[n,1]))
utc <- as.POSIXct(untime, origin = "1970-01-01")

mp <- c()
for(d in ds)
{
  d <- as.Date(d, origin = "1970-01-01")
  print(as.character(d))
  mm <- mean(unlist(pp[which(as.Date(utc, origin = "1970-01-01") == d),13]), na.rm = TRUE)
  mp <- c(mp, mm)
}

mpt <- ts(mp)
acf(mp, lag = 100)
plot(mpt, type="l")

wws <- unique(maply(1:length(ds), function(n) week(ds[n])))
wm <- c()
for(i in wws)
{
  mm <- mean(unlist(pp[which(week(as.Date(utc, origin = "1970-01-01")) == i),13]), na.rm = TRUE)
  wm <- c(wm, mm)
}

plot(wm, type="l")
acf(ts(wm))

### lag plot. N.B.: the weeks in excel and R differ!!!

lp <- matrix(0, nrow = 38, ncol = 2)
for(i in 1:38)
{
  lp[i,1] <- wm[i]
  lp[i,2] <- wm[i+1]
}

fit <- lm(lp[,2]  ~ lp[,1])
summary(fit)
plot(fit)

mean(mean(fit$residuals^2))
