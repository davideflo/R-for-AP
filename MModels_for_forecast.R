#### Meteo model for forecast


library(plyr)
library(dplyr)
library(readxl)
library(feather)
library(lubridate)
library(data.table)
library(randomForest)
library(RFinfer)

### it's better to model the weather dependency with a GAMM model
### DWT is absolute mean; DTW has the mean compared to a "basal consumption"

DWT = data.table(read_excel('C:\\Users\\utente\\Documents\\Sbilanciamento\\DWT.xlsx'))

plot(DWT$y, type = 'l')
