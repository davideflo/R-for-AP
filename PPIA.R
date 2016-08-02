### PUN PREDICTION IN ACTION ###

source("R_code/functions_for_PPIA_server.R")

prices10 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2010.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices11 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2011.xlsx", sheet="Prezzi-Prices", colNames=TRUE)

meteocsud <- read.csv2("C:/Users/utente/Documents/PUN/storico_roma.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)


test <- create_dataset_day_ahead(prices10,"ven","CSUD", meteocsud, 0, day_ahead = 0, hb = 23)
