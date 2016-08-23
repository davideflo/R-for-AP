##### script for prediction

library(h2o)
library(readxl)
library(lubridate)

### all variables called date are of the type used by lubridate 
### and excel

## source

build_meteo_new <- function(date)
{
	## call all meteo datasets for all cities
	## mediate_meteos
	## store the results in a matrix with each row being a day
	## and variables: (tmin, tmax, tmed, rain, wind)
	## both now and future:
	# 1: tmin.now, tmax.now, tmed.now, rain.now, wind.now
	# 2: tmin.+1, tmax.+1, tmed.+1, rain.+1, wind.+1
	# 3: tmin.+2, tmax.+2, tmed.+2, rain.+2, wind.+2
	#
}
#######################################################
build_new <- function(df)
{
	## put the prices in rows
	## compute old and new holidays
	## old and new angleday 
	## all the "new" variables are in last columns
}
#######################################################
assemble_pm <- function(pn, meteo)
{
	res <- matrix(0,nrow=5,ncol=ncol(pn)+ncol(meteo)+22)
	# build rows by column names

	# for(i in 1:5)
	# {
	# 	row <- c(pn[i,1:(ncol(pn)-2)])
	# }
}
#######################################################
prediction <- function(path, date, meteo)
{
	res <- matrix(0, nrow=24, ncol=5)
	## load pun file
	pp <- read_excel("DB_Borse_Elettriche_PER MI.xlsx", sheet = "DB_Dati")
	## look for date
	ppnew <- pp[which(pp[,1] == as.Date(date))]
	## build "new observation"
	meteonew <- build_meteo_new(date)
	pn <- build_new(ppnew)
	xnew <- as.h2o(assemble_pm(pn, meteonew))
	## call all models and make predictions
	for(da in 1:5)
	{
		for(step in 1:24)
		{
			id <- paste0("sda",step,"_",da)
			model <- h2o.loadModel(paste0("C:\\Users\\utente\\Documents\\PUN\\fixed\\models",id))
			x <- predict(model,xnew[da,])
			x2 <- as.numeric(x$predict)
			x3 <- as.matrix(x2)
			x4 <- unlist(x3[,1])
			res[step,da] <- x4
		}
	}
	return(res)
}