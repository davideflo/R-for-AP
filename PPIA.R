### PUN PREDICTION IN ACTION ###

source("R_code/functions_for_PPIA_server.R")

prices10 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2010.xlsx", sheet="Prezzi-Prices", colNames=TRUE)
prices11 <- openxlsx::read.xlsx("C:/Users/utente/Documents/PUN/Anno 2011.xlsx", sheet="Prezzi-Prices", colNames=TRUE)

meteocsud <- read.csv2("C:/Users/utente/Documents/PUN/storico_roma.txt", header=TRUE, sep="\t",colClasses = "character", stringsAsFactors = FALSE)

### test1: normal-old-revised-function with step = day_ahead = 0

start <- Sys.time()
test <- create_dataset_day_ahead(prices10,"ven","CSUD", meteocsud, 0, day_ahead = 0, hb = 23)
end <- Sys.time()
taken <- end-start
taken ### 20 minutes

### test2: old-revised-function with step = 0 and day_ahead = 1

aug <- augmented_dataset(prices10,prices11,step = 0,day_ahead = 1)

start <- Sys.time()
test2 <- create_dataset_day_ahead(aug,"ven","CSUD", meteocsud, 0, day_ahead = 1, hb = 23)
end <- Sys.time()
taken <- end-start
taken ### 16 minutes

### test3: old-revised-function with step = 6 and day_ahead = 4

aug2 <- augmented_dataset(prices10,prices11,step = 6,day_ahead = 4)

start <- Sys.time()
test3 <- create_dataset_day_ahead(aug2,"ven","CSUD", meteocsud, step = 6, day_ahead = 4, hb = 23)
end <- Sys.time()
taken <- end-start
taken ### 17 minutes

### test4: with old-revised-function step = 0, day_ahead = 1 must equal step = 24, day_ahead = 0

aug3 <- augmented_dataset(prices10,prices11,step = 24,day_ahead = 0)

start <- Sys.time()
test4 <- create_dataset_day_ahead(aug3,"ven","CSUD", meteocsud, 24, day_ahead = 0, hb = 23)
end <- Sys.time()
taken <- end-start
taken ### 14 minutes

### test 5: new-defined-function with step = day_ahead = 0

start <- Sys.time()
test5 <- create_dataset_days_ahead(prices10,"ven","CSUD", meteocsud, step = 0, day_ahead = 0, hb = 23)
end <- Sys.time()
taken <- end-start
taken ### 




