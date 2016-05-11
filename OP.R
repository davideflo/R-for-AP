library(openxlsx)
library(dplyr)
library(reshape)
library(stringi)
library(xlsx)

source("functions_for_OP.R")

## do not run:
#test2 <- openxlsx::read.xlsx("Z:/AREA ENERGY MANAGEMENT GAS/Deal Chiusi/Riepilogo Deal Chiusi.xlsx", sheet = 1, colNames = TRUE)
## end: do not run

# op <- read.xlsx("OP 16-17.xlsx", sheet = 5, colNames = TRUE)
# 
# gen <- op[2:14,2:32]
# feb <- op[2:14,33:61]
# 
# somme <- apply(gen, 1, sum)
# 
# 
# 
# 
# somme_in_MWh <- somme * (fattore_conversione/100) 
# 
# costi <- c(23.26054,
#            21.75,
#            21.75,
#            20.7,18.9,20.35)
# 
# a <- somme_in_MWh[5:10]*costi
# a

### carico i dati (database di tutti i clienti AxoPower)
# NB: leggi con: openxlsx #

tabella <- openxlsx::read.xlsx("160413-150847-2144.xlsx", sheet = 1, colNames = TRUE)
pdr_axo <- openxlsx::read.xlsx("pdr_axo.xlsx", sheet = 1, colNames = TRUE)
pdr_axo <- pdr_axo[!is.na(pdr_axo)]
head(tabella)
dim(tabella)
library(WriteXLS)

variabili <- c("CODICE_PRODOTTO", "PDR", "DATA_INI_DEC_PROD_T", "DATA_FIN_DEC_PROD_T", "PROFILO_PRELIEVO", "CONSUMO_CONTR_ANNUO") 
cols <- which(names(tabella) %in% variabili)

tabella <- tabella[,cols]
tabella <- data.frame(tabella)

prodotti <- unique(unlist(tabella["CODICE_PRODOTTO"]))
inizio <- unique(unlist(tabella["DATA_INI_DEC_PROD_T"]))
fine <- unique(unlist(tabella["DATA_FIN_DEC_PROD_T"]))
profilo <- unique(unlist(tabella["PROFILO_PRELIEVO"]))

length(prodotti)
length(inizio)
length(fine)
length(profilo)



#pivot <- aggregate(cbind(CODICE_PRODOTTO,DATA_INI_DEC_PROD_T,DATA_FIN_DEC_PROD_T,PROFILO_PRELIEVO)~CONSUMO_CONTR_ANNUO, data=tabella, FUN=sum)
pivot <- cast(tabella, CONSUMO_CONTR_ANNUO ~ .)
#with(df, tapply(tabella$CONSUMO_CONTR_ANNUO, list(tabella$CODICE_PRODOTTO,tabella$DATA_INI_DEC_PROD_T,tabella$DATA_FIN_DEC_PROD_T,tabella$PROFILO_PRELIEVO), sum))
pivot2 <- group_by(tabella, CODICE_PRODOTTO,DATA_INI_DEC_PROD_T,DATA_FIN_DEC_PROD_T,PROFILO_PRELIEVO)


#agg <- aggregate(tabella, by = list(tabella$CODICE_PRODOTTO, tabella$DATA_INI_DEC_PROD_T, tabella$DATA_FIN_DEC_PROD_T, tabella$PROFILO_PRELIEVO, tabella$CONSUMO_CONTR_ANNUO), FUN = sum)

######## generazione di tutte le combinazioni di prodotto | data_inizio | data_fine | profilo_di_consumo

aggregati <- data_frame()
for(prod in prodotti)
{
  aggregati2 <- data_frame()
  rows <- which(tabella["CODICE_PRODOTTO"] == prod)
  tabella2 <- tabella[rows,]
  PDR <- unique(tabella2["PDR"][[1]])
  pdr_ind <- c()
  for(pdr in PDR)
  {
    ind <- which(tabella2["PDR"] == pdr)
    pdr_ind <- c(pdr_ind, max(ind))
  }
  data <- tabella2[pdr_ind,3:6]
  a <- unique(unlist(data[,1]))
  for(i in a)
  {
    rows2 <- which(data[,1] == i)
    data2 <- data[rows2, 2:4]
    b <- unique(unlist(data2[,1]))
    for(j in b)
    {
      rows3 <- which(data2[,1] == j)
      data3 <- data2[rows3, 2:3]
      d <- unique(unlist(data3[,1]))
      for(k in d)
      {
        rows4 <- which(data3[,1] == k)
        s <- sum(data3[rows4,2])
        df <- data.frame(cbind(prod, i, j, k, s))
        if(s != 0) aggregati2 <- bind_rows(aggregati2, df)
      }
    }
  }
  if(nrow(aggregati2) > 0) 
  {
    colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
    aggregati2$consumo <- as.numeric(as.character(aggregati2$consumo))
    aggregati <- bind_rows(aggregati, aggregati2)
  }
}
colnames(aggregati) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")


write.xlsx(aggregati, "combinazioni_prodotti.xlsx", sheetName="Sheet1")

head(aggregati)
dim(aggregati)

###### ri-classificazione dei prodotti tenendo conto delle prevsioni di passaggio da fisso a variabile #########
### Se un prodotto passa a variabile, si aggiunge una nuova linea

month13_in_interval <- function(string_date)
{
  bool <- FALSE
  splitted_date <- as.numeric(unlist(strsplit(as.character(string_date), "/")))
  
  day <- splitted_date[1]
  month <- splitted_date[2]
  year <- splitted_date[3]
  
  fm <- ((month + 12) %% 13) + 1
  fy <- year + 1
  
  if((fm <= 12) & (fy <= 2017)) 
  {
    bool <- TRUE
    if(fm < 10) new_month <- paste0("0", fm)
    else new_month <- fm
    if(day < 10) new_day <- paste0("0", day)
    else new_day <- day
    new_date <- paste0(new_day, "/", new_month, "/", fy)
  }
    
  return(list(bool, new_date))
}

check_enddate <- function(enddate)
{
  bool <- FALSE
  splitted_date <- as.numeric(unlist(strsplit(as.character(enddate), "/")))
  
  day <- splitted_date[1]
  month <- splitted_date[2]
  year <- splitted_date[3]
  
  if(year > 2017) {bool <- TRUE}
  
  return(bool)
}

day_before <- function(string_date)
{
  splitted_date <- as.numeric(unlist(strsplit(as.character(string_date), "/")))
  day <- splitted_date[1]
  month <- splitted_date[2]
  year <- splitted_date[3]
  
  leap_year <- year %% 4
  
  df31 <- c(1,2,4,6,8,9,11)
  df30 <- c(5,7,10,12)
  
  if(month == 1) new_month <- 12
  else new_month <- month - 1
  if( (month == 3) & (leap_year == 0) ) new_day <- 29
  else if((month == 3) & (leap_year != 0)) new_day <- 28
  else if(month %in% df31) new_day <- 31
  else if(month %in% df30) new_day <- 30
  if(month == 1) new_year <- year - 1
  else new_year <- year
  
  if(new_month < 10) new_month <- paste0("0", new_month)

  new_date <- paste0(new_day, "/", new_month, "/", new_year)
  return(new_date)
}

transform_date <- function(date)
{
  dv <- strsplit(date, "/")
  return(as.numeric(unlist(dv)))
}

compare_dates <- function(date_pc, date) 
{
  ## returns TRUE if date <= date_pc 
  bool <- FALSE
  dv_pc <- transform_date(date_pc)
  dv <- transform_date(date)
  if(dv[3] < dv_pc[3]) {bool <- TRUE}
  else if((dv[3] == dv_pc[3]) & (dv[2] < dv_pc[2])) {bool <- TRUE}
  else if((dv[3] == dv_pc[3]) & (dv[2] == dv_pc[2]) & (dv[1] <= dv_pc[1])) {bool <- TRUE}
  return(bool)
}

take_maximum_date <- function(date_pc, date)
{
  if(compare_dates(date_pc,date)) return(date_pc)
  else return(date)
}

take_minimum_date <- function(date_pc, date)
{
  if(!compare_dates(date_pc,date)) return(date_pc)
  else return(date)
}

counter <- 0
aggregati_forecast <- data_frame()
for( i in 1:nrow(aggregati))
{
  #print(i)
  x <- unname(aggregati[i,])
  first_letter <- stri_sub(x[[1]], 1, 1)
  #ForV <- stri_sub(x[[1]], 6, 6)
  dl <- month13_in_interval(x[[2]])
  bVerbose_year <- check_enddate(x[[3]])
  if( (first_letter == "L")  & (dl[[1]]) & (bVerbose_year) )
  {
    counter <- counter + 1
    new_cod <- paste0("R", stri_sub(x[[1]], 2, nchar(x[[1]]))) 
    new_date <- dl[[2]]
    old <- unname(data.frame(cbind(x[[1]], x[[2]], day_before(new_date), x[[4]], x[[5]])))
    updated <- unname(data.frame(cbind(new_cod, new_date, x[[3]], x[[4]], x[[5]])))

    df <- data.frame(cbind(c(as.character(old[[1]]), as.character(updated[[1]])), 
                           c(as.character(old[[2]]), as.character(updated[[2]])), 
                           c(as.character(old[[3]]), as.character(updated[[3]])), 
                           c(as.character(old[[4]]), as.character(updated[[4]])), 
                           c(as.character(old[[5]]), as.character(updated[[5]]))))
                           #c(as.numeric(old[[5]]), as.numeric(updated[[5]]))))
    #df <- data.frame(rbind(old, updated))
    #df <- data_frame(bind_rows(old, updated))
    #df <- do.call(rbind, c(old, updated))
    
    
    colnames(df) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
    
    aggregati_forecast <- bind_rows(aggregati_forecast, df)
  }
  else if (compare_dates(as.character(x[[3]]), "01/01/2016"))
  {
    df2 <- unname(data.frame(cbind(as.character(x[[1]]), "01/01/2016", as.character(x[[3]]), as.character(x[[4]]), as.character(x[[5]]))))
    colnames(df2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
    aggregati_forecast <- bind_rows(aggregati_forecast, df2)
  }
  else {next}
}

colnames(aggregati_forecast) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
aggregati_forecast$consumo <- as.numeric(aggregati_forecast$consumo)

write.xlsx(aggregati_forecast, "combinazioni_prodotti_con_passaggi.xlsx", sheetName="Sheet1")

######################################################################################################################
######################################################################################################################
######## VERIFICA SUL CAMBIAMENTO DA FISSO A VARIABILE #############################################################

ver <- openxlsx::read.xlsx("per_verifica.xlsx", sheet = 1, colNames = TRUE)
ver <- ver[2:nrow(ver),] 

index_attivi <- c()
clienti <- unique(unlist(ver[,3]))
for(pv in clienti)
{
  if(!is.na(pv)){
    rows <- which(ver[,3] == pv)
    if(length(rows) > 1)
    {
      index_attivi <- c(index_attivi, max(rows))
    }
    else if(length(rows) == 1)
    {
      index_attivi <- c(index_attivi, rows)
    }
    else {next}
  }

}

attivi <- ver[index_attivi,]
prod_fix <- matrix(0, nrow = nrow(attivi), ncol = 7)
# nella prima colonna metto il prodotto, data prima "F" e data ultima "F", se AxoPower shipper
for(i in 1:nrow(attivi))
{
  if("F" %in% attivi[i,12:35])
  {
    prima_f <- colnames(attivi)[min(which(attivi[i,] == "F"))]
    ultima_f <- colnames(attivi)[max(which(attivi[i,] == "F"))]
    prod_fix[i, 1] <- attivi[i,6]
    prod_fix[i, 2] <- prima_f
    prod_fix[i, 3] <- ultima_f
    prod_fix[i, 5] <- attivi[i, 10]
    prod_fix[i, 6] <- attivi[i, 11]
    prod_fix[i, 7] <- attivi[i, 2]
    if(attivi[i, 9] == "0001808491-AXOPOWER SRL") {prod_fix[i,4] <- 1}
  }
  else{
    prod_fix[i, 1] <- attivi[i,6]
  }
}

ok <- rep(0, nrow(aggregati_forecast))
for(i in 1:nrow(aggregati_forecast))
{
  first_letter <- stri_sub(aggregati_forecast[i,1], 1, 1)
  FV <- stri_sub(aggregati_forecast[i,1], 6, 6)
  if(first_letter == "L" & FV == "F")
  {
    rows <- which(prod_fix[,1] == as.character(aggregati_forecast[i,1]))
    rows2 <- which(prod_fix[,5] == as.character(aggregati_forecast[i,4]))
    r <- intersect(rows, rows2)
    if(length(r) == 1)
    {
      if((prod_fix[r,2] == aggregati_forecast[i,2]) & (prod_fix[r,3] == aggregati_forecast[i,3])) ok[i] <- 1
    }
    if(length(r) >1)
    {
      if(all(prod_fix[r,2] == aggregati_forecast[i,2]) & all(prod_fix[r,3] == aggregati_forecast[i,3]) ) ok[i] <- 1
    }
  }
}
res <- data.frame(cbind(aggregati_forecast, ok))
head(res)

change_F_to_V <- function(row, tab)
{
  bool <- FALSE
  type <- tab[row,14:37]
  prima_f <- 0
  prima_v <- 0
  ultima_f <- 0
  if(("F" %in% type)  & ("V" %in% type))
  {
    prima_f <- colnames(type)[min(which(type == "F"))]
    #u_f <- colnames(type)[max(which(type == "F"))]
    #u_f <- as.character(unlist(strsplit(u_f, "/")))
    prima_v <- colnames(type)[min(which(type == "V"))]
    ultima_f <- day_before(prima_v)
    bool <- TRUE
  }
  return(list(bool, prima_f, ultima_f, prima_v))
}

prodotti <- unique(unlist(attivi[,6]))
prodotti <- prodotti[-which(prodotti == "SUPERI_E_QFISSA")]
aggregati <- data_frame()
for(prod in prodotti)
{
  aggregati2 <- data_frame()
  rows <- which(attivi[,6] == prod)
  tabella2 <- attivi[rows,]
  
  data_inizio <- unique(unlist(tabella2[,8]))
  for(di in data_inizio)
  {
    rows2 <- which(tabella2[,8] == di)
    data2 <- tabella2[rows2,]
    data_fine <- unique(unlist(data2[,10]))
    for(df in data_fine)
    {
      rows3 <- which(data2[,10] == df)
      data3 <- data2[rows3,]
      prof <- unique(unlist(data3[,12]))
      for(p in prof)
      {
        rows4 <- which(data3[,12] == p)
        data4 <- data3[rows4,]
        for(j in 1:length(rows4))
        {
          first_letter <- stri_sub(prod, 1, 1)
          FV <- stri_sub(prod, 6, 6)
          sf1 <- 0
          sf <- 0
          sv <- 0
          cfv <- change_F_to_V(j, data4)
          if(cfv[[1]] == "TRUE" & first_letter == "L" & FV == "F" & compare_dates(df,"31/12/2017"))
          {
            sf1 <- sf1 + as.numeric(as.character(data4[j,13]))
            sv <- sv + as.numeric(as.character(data4[j,13]))
            new_prod_name <- paste0("R", stri_sub(prod, 2, nchar(prod)))
            tkm <- take_maximum_date("01/01/2016",cfv[[2]])
            min_fin <- take_minimum_date("31/12/2017", df)
            #print(tkm)
            fisso <- data.frame(cbind(prod, tkm, cfv[[3]], p, sf1))
            #print(fisso)
            colnames(fisso) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
            variabile <- data.frame(cbind(new_prod_name, cfv[[4]], min_fin, p, sv))
            #print(variabile)
            colnames(variabile) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
            aggregati2 <- bind_rows(aggregati2, fisso)
            aggregati2 <- bind_rows(aggregati2, variabile)
            colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
          }
          else
          {
            sf <- sf + as.numeric(as.character(data4[j,13]))
            fisso <- data.frame(cbind(prod, di, df, p, sf))
            #print(fisso)
            colnames(fisso) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
            aggregati2 <- bind_rows(aggregati2, fisso)
            colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
          }
        }
      }
    }
  }
  if(nrow(aggregati2) > 0) 
  {
    colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
    aggregati2$consumo <- as.numeric(as.character(aggregati2$consumo))
    aggregati <- bind_rows(aggregati, aggregati2)
  }
}
colnames(aggregati) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")

aggregati

aggregati_forecast <- aggregati
####################################################################################################################
####################################################################################################################

head(aggregati_forecast)
aggregati_forecast[1:10,]
dim(aggregati_forecast)


prodotti_finali <- aggregati_forecast$prodotto
length(prodotti_finali)
length(unique(unlist(prodotti_finali)))
length(prodotti)

which(prodotti %in% unique(unlist(prodotti_finali)) )




# 
# WriteXLS(aggregati, paste0("combinazioni_prodotto_PGC_MF_1404.xlsx"), row.names = FALSE, col.names=TRUE)
# write.xlsx(aggregati, "/combinazioni_prodotto_PGC_MF_1404.xlsx")

profili_consumo <- openxlsx::read.xlsx("profili.xlsx", sheet = 1, colNames = TRUE)

prof_consumo <- data.frame(profili_consumo[2:nrow(profili_consumo),3:38])
head(prof_consumo)

######
## calcolo i profili di consumo per tutti i prodotti

prodotti <- unique(prodotti_finali)

res_per_prodotto <- matrix(0, nrow = length(prodotti), ncol = length(prof_consumo[,1]))

for(j in 1:length(prodotti))
{
#  print(prod)
  rows <- which(aggregati_forecast[,1] == prodotti[j])
  x <- aggregati_forecast[rows,]
  mat_per_prodotto <- matrix(0, nrow = nrow(prof_consumo), ncol = length(rows))
  for(i in 1:nrow(x))
  {
#    print(i)
    y <- x[i,]
    data_in_y <- unlist(y[2])
    data_fin_y <- unlist(y[3])
    prof_y <- unlist(y[4])
    cons_y <- unlist(y[5])
    COL <- which(names(prof_consumo) == prof_y)
    if( compare_dates("01/01/2016", data_in_y)  & !compare_dates("31/12/2017", data_fin_y)) ## data inizio prima di genaio 2016 e data fine dopo dicembre 2017
    {
      mat_per_prodotto[,i] <- cons_y * prof_consumo[,COL]
    }
    else if(!compare_dates("01/01/2016", data_in_y)  & !compare_dates("31/12/2017", data_fin_y)) ## data inizio dopo genaio 2016 e data fine dopo dicembre 2017
    {
      start <- which(prof_consumo[,1] == data_in_y)
      res <- c(rep(0, start-1), cons_y * prof_consumo[(start):nrow(prof_consumo),COL])
      mat_per_prodotto[,i] <- res
    }
    else if(compare_dates("01/01/2016", data_in_y)  & compare_dates("31/12/2017", data_fin_y)) ## data inizio prima di genaio 2016 e data fine prima di dicembre 2017
    {
      end <- which(prof_consumo[,1] == data_fin_y)
      if(length(end) > 0)
      {
        len <- nrow(prof_consumo) - end
        res <- c( cons_y * prof_consumo[1:end,COL] , rep(0, len))
        mat_per_prodotto[,i] <- res
      }
    }
    else ## ## data inizio dopo genaio 2016 e data fine prima di dicembre 2017
    {
      start <- which(prof_consumo[,1] == data_in_y)
      end <- which(prof_consumo[,1] == data_fin_y)
      len <- nrow(prof_consumo) - end
      res <- c(rep(0, start-1), cons_y * prof_consumo[(start):end,COL], rep(0, len))
      mat_per_prodotto[,i] <- res
    }
  }
  somma_profili <- apply(mat_per_prodotto, 1,sum)
  
  res_per_prodotto[j,] <- somma_profili/100
}

head(res_per_prodotto)
#res_per_prodotto2 <- data_frame(bind_cols(as.data.frame(prodotti), res_per_prodotto))
dim(res_per_prodotto)

res_per_prodotto <- data.frame(res_per_prodotto)
colnames(res_per_prodotto) <- prof_consumo[,1]
rownames(res_per_prodotto) <- prodotti

head(res_per_prodotto)

write.xlsx(res_per_prodotto, "profilo_per_prodotto.xlsx", sheetName="Sheet1", row.names = TRUE, col.names = TRUE)

mesi <- c("01","02","03","04","05","06","07","08","09","10","11","12")
#giorni <- grep(paste0("01/",mese), prof_consumo[,1])
giorni <- c(1,32,61,92,122,153,183,214,245,275,306,336,367,398,426,457,487,518,548,579,610,640,671,701)

matplot(1:731, t(res_per_prodotto), type = "l", xlab="tempo", ylab="profilo", main="profilo di consumo per prodotto")
axis(1, at=giorni[c(1,12,24)], labels=prof_consumo[giorni[c(1,12,24)],1])
text(seq(1, 731, by=24), par("usr")[3] - 0.2, labels = prof_consumo[giorni[c(1,12,24)],1], srt = 45, pos = 1, xpd = TRUE)

day_by_day_sum <- colSums(res_per_prodotto)
day_by_day_sum_2016 <- colSums(res_per_prodotto[,1:366])
tot_2016 <- sum(day_by_day_sum_2016)
sum(day_by_day_sum)

plot(1:731, day_by_day_sum, type="l")
#### check dei profili con i dati del DB in excel


#plot(1:731, day_by_day_sum, type = "l", col = "blue")
#axis(1, at=giorni[c(1,12,24)], labels=prof_consumo[giorni[c(1,12,24)],1])


################
### calcolo del consumo considerando:
## 1) solo AxoPower come shipper 
## 2) solo il portafoglio di fissi solo shipper AxoPower.


#shipper_axo <- read.xlsx("solo_shipper_axo.xlsx", sheetIndex = 1, colNames = TRUE)
#profili3 <- read.xlsx("profilo_per_prodotto3.xlsx", sheetIndex = 1, colNames = TRUE)

###############################
### products = lista dei prodotti
### M = sotto-matrice di aggregati_forecast
compute_profiles <- function(products, M)
{
  res_per_prodotto <- matrix(0, nrow = length(products), ncol = length(prof_consumo[,1]))
  
  for(j in 1:length(products))
  {
    print(products[j])
    rows <- which(M[,1] == products[j])
    x <- M[rows,]
    mat_per_prodotto <- matrix(0, nrow = nrow(prof_consumo), ncol = length(rows))
    for(i in 1:nrow(x))
    {
       print(i)
      y <- x[i,]
      data_in_y <- unlist(y[2])
      data_fin_y <- unlist(y[3])
      prof_y <- unlist(y[4])
      cons_y <- unlist(y[5])
      COL <- which(names(prof_consumo) == prof_y)
      if( compare_dates("01/01/2016", data_in_y)  & !compare_dates("31/12/2017", data_fin_y)) ## data inizio prima di genaio 2016 e data fine dopo dicembre 2017
      {
        mat_per_prodotto[,i] <- cons_y * prof_consumo[,COL]
      }
      else if(!compare_dates("01/01/2016", data_in_y)  & !compare_dates("31/12/2017", data_fin_y)) ## data inizio dopo genaio 2016 e data fine dopo dicembre 2017
      {
        start <- which(prof_consumo[,1] == data_in_y)
        res <- c(rep(0, start-1), cons_y * prof_consumo[(start):nrow(prof_consumo),COL])
        mat_per_prodotto[,i] <- res
      }
      else if(compare_dates("01/01/2016", data_in_y)  & compare_dates("31/12/2017", data_fin_y)) ## data inizio prima di genaio 2016 e data fine prima di dicembre 2017
      {
        end <- which(prof_consumo[,1] == data_fin_y)
        if(length(end) > 0)
        {
          len <- nrow(prof_consumo) - end
          res <- c( cons_y * prof_consumo[1:end,COL] , rep(0, len))
          mat_per_prodotto[,i] <- res
        }
      }
      else ## ## data inizio dopo genaio 2016 e data fine prima di dicembre 2017
      {
        start <- which(prof_consumo[,1] == data_in_y)
        end <- which(prof_consumo[,1] == data_fin_y)
        len <- nrow(prof_consumo) - end
        res <- c(rep(0, start-1), cons_y * prof_consumo[(start):end,COL], rep(0, len))
        mat_per_prodotto[,i] <- res
      }
    }
    somma_profili <- apply(mat_per_prodotto, 1,sum)
    
    res_per_prodotto[j,] <- somma_profili/100
  }
  return(res_per_prodotto)
}
##############################

######################################################################################################
######################################################################################################

change_name <- function(name)
{
  split <- c()
  for(i in 1:nchar(name))
    {split <- c(split, stri_sub(name, i, i))}
      
  let <- c("A", "B", "C", "D")        
  if( split[1] == "L" & split[6] == "F" & split[3] %in% let)
  {
    if(paste0(split[8],split[9],split[10],split[11]) == "R508")
    {
      newname <-  paste0("R", stri_sub(name, 2, nchar(name)))
    }
    else
    {
      anno <- as.numeric(paste0(split[8],split[9],split[10],split[11]))
      if(anno <= 1501 & split[3] != "D")
      {
        newname <- paste0("LG",split[3],"_MF_R508")
      }
      else if(anno < 1501 & split[3] == "D")
      {
        newname <- "LGC_MF_R508"
      }
      else if(anno == 1501 & split[3] == "D")
      {
        newname <- "LGD_MF_R508"
      }
      else if(anno > 1501)
      {
        newname <- paste0("R", stri_sub(name, 2, nchar(name)))
      }
    }
    return(newname)
  }
  else {return(name)}
}


###################################################################################################### above is deprecated 
######################################################################################################
######################################################################################################

shipper <- "0001808491-AXOPOWER SRL"
ver <- openxlsx::read.xlsx("per_verifica.xlsx", sheet = 1, colNames = TRUE)
ver <- ver[2:nrow(ver),] 

index_attivi <- c()
for(i in 1:nrow(ver))
{
  if(sum(ver[i, 38:768], na.rm=TRUE) != 0)
  {
    index_attivi <- c(index_attivi,i)
  }
}

length(index_attivi)

attivi <- ver[index_attivi,]
head(attivi)
dim(attivi)
dim(ver)

R <- which(attivi[,11] == shipper)
attivi <- attivi[R,]
dim(attivi)

prodotti2 <- unique(unlist(attivi[,6]))
prodotti2 <- prodotti2[-which(prodotti2 == "SUPERI_E_QFISSA")]
aggregati_AP <- compute_combinations2(attivi, prodotti2) ################################################### BREAKS DOWN ######################

head(aggregati_AP)
soloship <- as.character(unique(unlist(aggregati_AP[,1])))
soloship <- soloship[-which(soloship == "SUPERI_E_QFISSA")]

profili_shipper_AxoPower <- compute_profiles(soloship, aggregati_AP)
write.xlsx(aggregati_AP, "aggregati_AP.xlsx", sheetName = "sheet1", col.names = TRUE)


soloship_fissi <- c()
for(i in 1:length(soloship))
{
  fl <- stri_sub(soloship[i], 1, 1)
  FV <- stri_sub(soloship[i], 6,6)
  if (fl == "L" & FV == "F") soloship_fissi <- c(soloship_fissi, soloship[i])
}

profili_shipper_AxoPower_fissi <- compute_profiles(soloship_fissi, aggregati_AP)

####################################################################################################
####################################################################################################
###### BRUTE FORCE:

ver2 <- openxlsx::read.xlsx("per_verifica.xlsx", sheet = 1, colNames = TRUE)
ver <- openxlsx::read.xlsx("Report_214.xlsx", sheet = 1, colNames = TRUE)

head(ver)
#ver <- ver[2:nrow(ver),] 

# index_attivi <- c()
# clienti <- unique(unlist(ver[,3]))
# for(pv in clienti)
# {
#   if(!is.na(pv)){
#     rows <- which(ver[,3] == pv)
#     if(length(rows) > 1)
#     {
#       index_attivi <- c(index_attivi, max(rows))
#     }
#     else if(length(rows) == 1)
#     {
#       index_attivi <- c(index_attivi, rows)
#     }
#     else {next}
#   }
#   
# }
# length(index_attivi)


#attivi <- ver[index_attivi,]
#attivi[is.na(attivi)] <- 0
attivi <- ver
attivi <- ver2
rm(ver)

attivi2 <- ver[-4980,]
colnames(attivi)[37] <- "CONSUMO_DISTRIBUTORE"
colnames(attivi)[38] <- "CONSUMO_DISTRIBUTORE_2"
rownames(attivi) <- NULL
rownames(ver) <- NULL

attivi <- anni_competenza(attivi, "01/01/2016")
attiv <- extract_relevant(attivi)
attiv <- extract_relevant_val(attivi)
test <- compute_combinations_DEF(attiv)
test <- compute_combinations_DEF_val(attiv)
test2 <- compute_combinations2(attiv)

ver[which(ver[,6] == "LG1_BF_SIPA"),1:13]
ver[which(ver[,6] == "LG1_BF_FRBR"),1:13]
attiv[which(attiv[,6] == "LG1_BF_SIPA"),1:13]

test[which(test[,1] == "LG1_BF_SIPA"),]
test[which(test[,1] == "LG1_BF_FRBR"),]
test[which(test[,1] == "LG1_BI_BRED"),]
test[which(test[,1] == "LG1_BF_BRED"),]
test[which(test[,1] == "LGD_MF_1501"),]
test[which(test[,1] == "RGD_MF_R508"),]
test[which(test[,1] == "LG1_BF_CTC0"),]

colnames(attivi2)[13] <- "CONSUMO_DISTRIBUTORE"
attivi2 <- anni_competenza(attivi2, "01/01/2016")
attiv2 <- extract_relevant(attivi2)
test2 <- compute_combinations_DEF(attiv2)
test2[which(test2[,1] == "LG1_BF_SIPA"),]

xlsx::write.xlsx(data.frame(test), "test_new.xlsx", row.names = FALSE, col.names = TRUE)

names(attivi)
### profili di prelievo/consumo da colonna 38 a colonna 768. Fisso/Variabile da colonna 14 a 37. shipper --> 11a colonna
### profilo totale:
profilo_totale <- colSums(attivi[,38:768], na.rm = TRUE)
plot(1:731, profilo_totale, type="l", col = "blue")
### profilo solo shipper AxoPower:
soloship <- grep("axopower", tolower(unlist(attivi[,11])))
profilo_shipper_axopower <- colSums(attivi[soloship, 38:768], na.rm = TRUE)
lines(1:731, profilo_totale, type="l", lwd=1.5,col = "red")

#sum_by_month("01","2016", attivi[38:768])
### profilo solo fissi solo AxoPower shipper
find_FFs <- function(subvec)
{
  vec_f <- rep(0,24)
  for(i in 1:length(subvec))
  {
    if(subvec[i] == "F") vec_f[i] <- 1
  }
  return(vec_f)
}

convert_month_to_days <- function(vec_f)
{
  conv <- c()
  two_years <- c(31, 29, 31,30,31,30,31,31,30,31,30,31,31, 28, 31,30,31,30,31,31,30,31,30,31)
  for(i in 1:24)
  {
    if(vec_f[i] == 1) {conv <- c(conv, rep(1, two_years[i]))}
    else  {conv <- c(conv, rep(0, two_years[i]))}
  }
  return(conv)
}

find_FFs_tab <- function(subtab)
{
  mat_f <- matrix(0, nrow = nrow(subtab), ncol = 731)
  for(i in 1:nrow(subtab))
  {
    vec <- find_FFs(subtab[i,])
    mat_f[i,] <- convert_month_to_days(vec)
  }
  return(mat_f)
}

lista_fissi <- find_FFs_tab(attivi[,14:37])
colnames(lista_fissi) <- colnames(res_per_prodotto)

profili_solo_fissi_Axopower <- matrix(0, nrow = nrow(attivi), ncol = 731)
for(i in 1:nrow(attivi))
{
  profili_solo_fissi_Axopower[i,] <- as.matrix(lista_fissi[i,] * attivi[i, 38:768])
}
head(profili_solo_fissi_Axopower)
colnames(profili_solo_fissi_Axopower) <- colnames(lista_fissi)
somma_profili_shipper_AxoPower_fissi <- colSums(profili_solo_fissi_Axopower)

######################################################################################################
######################################################################################################
pdr_axo <- unique(pdr_axo)
prod_to_pdr <- tabella[which(tabella[,1]%in%pdr_axo),2]
prod_to_pdr <- c(prod_to_pdr, "LG1_BV_MAMO")


exist_similar <- function(str)
{
  bool <- FALSE
  similar <- NULL
  last <- stri_sub(str, 2, nchar(str))
  for(prod in prodotti)
  {
    fl <- stri_sub(prod, 1, 1)
    llp <- stri_sub(prod, 2, nchar(prod))
    if(fl == "R" & llp == last) {bool <- TRUE; similar <- prod}
  }
  return(list(bool, similar))
}


soloship <- c()
soloship_fissi <- c()
for(prod in prod_to_pdr)
{
  first_letter <- stri_sub(prod, 1, 1)
  FV <- stri_sub(prod, 6,6)
  sim <- exist_similar(prod)
  if(first_letter == "L" & sim[[1]]) soloship <- c(soloship, prod, sim[[2]])
  if(first_letter == "L" & FV == "F") soloship_fissi <- c(soloship_fissi, prod)
}

soloship <- unique(soloship)
soloship_fissi <- unique(soloship_fissi)

rows_soloship <- which(unlist(aggregati_forecast[,1]) %in% soloship)
rows_soloship_fissi <- which(unlist(aggregati_forecast[,1]) %in% soloship_fissi)


AP_shipper <- aggregati_forecast[rows_soloship,]
AP_shipper_fissi <- aggregati_forecast[rows_soloship_fissi,]

profili_shipper_AxoPower <- compute_profiles(soloship, AP_shipper)
profili_shipper_AxoPower_solofissi <- compute_profiles(soloship_fissi, AP_shipper_fissi)

dim(profili_shipper_AxoPower)
dim(profili_shipper_AxoPower_fissi)

#ssa <- as.character(shipper_axo[,1])
#ssa <- unique(soloship)
#ssa <- ssa[ssa != "SUPERI_E_QFISSA"]
#length(ssa)

#rows_ssa <- which(rownames(res_per_prodotto) %in% ssa)
#length(rows_ssa)

#prodotto_ssa <- res_per_prodotto[rows_ssa,]
#dim(prodotto_ssa)

#somma_profili_ssa <- colSums(prodotto_ssa)
matplot(1:731, t(profili_shipper_AxoPower), type = "l",xlab="tempo", ylab="profilo", main="profilo di consumo per prodotto solo shipper AxoPower")
axis(1, at=giorni[c(1,12,24)], labels=prof_consumo[giorni[c(1,12,24)],1])
matplot(1:731, t(profili_shipper_AxoPower_solofissi), type = "l",xlab="tempo", ylab="profilo", main="profilo di consumo per prodotto solo shipper AxoPower solo fissi")
axis(1, at=giorni[c(1,12,24)], labels=prof_consumo[giorni[c(1,12,24)],1])

profili_shipper_AxoPower<- data.frame(profili_shipper_AxoPower)
colnames(profili_shipper_AxoPower) <- colnames(res_per_prodotto)
rownames(profili_shipper_AxoPower) <- soloship
#rownames(somma_profili_ssa) <- "somma profili solo shipper AxoPower"
profili_shipper_AxoPower_fissi <- data.frame(profili_shipper_AxoPower_fissi)
colnames(profili_shipper_AxoPower_fissi) <- colnames(res_per_prodotto)
rownames(profili_shipper_AxoPower_fissi) <- soloship_fissi


somma_profili_shipper_AxoPower <- colSums(profili_shipper_AxoPower)
plot(1:731, somma_profili_shipper_AxoPower, type="l", xlab = "tempo", ylab = "profilo", main = "somma profili solo shipper AxoPower")
write.xlsx(somma_profili_shipper_AxoPower, "somma_profili_soloAP.xlsx", sheetName = "sheet1", row.names = TRUE, col.names = TRUE)

somma_profili_shipper_AxoPower_fissi <- colSums(profili_shipper_AxoPower_fissi)
plot(1:731, somma_profili_shipper_AxoPower_fissi, type="l", xlab = "tempo", ylab = "profilo", main = "somma profili solo shipper AxoPower fissi")
write.xlsx(profili_shipper_AxoPower_fissi, "somma_profili_soloAP_fissi2.xlsx", sheetName = "sheet1", row.names = TRUE, col.names = TRUE)


# find_F <- function(cod)
# {
#   dec <- unlist(strsplit(cod, "_"))
#   if(length(dec) == 1) dec <- unlist(strsplit(cod, "-"))
#   last <- stri_sub(dec[2],-1,-1)
#   if(last == "F")
#     return(TRUE)
#   return(FALSE)
# }
# 
# find_fissi <- function(pro)
# {
#   row_index <- c()
#   for(i in 1:length(pro))
#   {
# #    print(i)
#     if(find_F(pro[i])) row_index <- c(row_index, i)
#   }
#   return(row_index)
# }
# 
# ssa_fissi <- find_fissi(prodotti)
# length(ssa_fissi)
# 
# prodotto_ssafissi <- res_per_prodotto[ssa_fissi,]
# prodotti[ssa_fissi]
# dim(prodotto_ssafissi)
# 
# somma_profili_ssafissi <- colSums(prodotto_ssafissi)
# plot(1:731, somma_profili_ssafissi, type = "l", col = "purple",xlab="tempo", y_lab="profilo", main="confronto profili solo fisso(porpora) e solo shipper AxoPower(rosso)")
# axis(1, at=giorni[c(1,12,24)], labels=prof_consumo[giorni[c(1,12,24)],1])
# lines(1:731, somma_profili_ssa, type = "l", col = "red")
# 
# somma_profili_ssafissi <- data.frame(t(somma_profili_ssafissi))
# colnames(somma_profili_ssafissi) <- colnames(res_per_prodotto)
# rownames(somma_profili_ssafissi) <- "somma profili solo fissi solo shipper AxoPower"
# 
# 
# write.xlsx(somma_profili_ssafissi, "somma_profili_solofissi_soloAP.xlsx", sheetName = "sheet1", row.names = TRUE, col.names = TRUE)

############
######## ACQUISTI A TERMINE #########

split_date <- function(string_date)
{
  splitted_date <- as.character(unlist(strsplit(as.character(string_date), "/")))
  day <- splitted_date[1]
  month <- splitted_date[2]
  year <- splitted_date[3]
  return(c(day, month, year))
}

take_month <- function(month, year, res)
{
  index_list <- c()
  for(i in 1:length(colnames(res)))
  {
    name <- colnames(res)[i]
    sdt <- split_date(name)
    if(sdt[2] == month & sdt[3] == year) index_list <- c(index_list, i)
  }
  return(index_list)
}

sum_by_month <- function(month, year, res)
{
  
  ii <- take_month(month, year, res)
  sum_month <- sum(res[,ii], na.rm = TRUE)
  
  return(sum_month)
}


PSV <- openxlsx::read.xlsx("PSV.xlsx", sheet = 1, colNames = TRUE)
date <- c(paste0("0",1:9,"/",2016), paste0(10:12,"/",2016), paste0("0",1:9,"/",2017), paste0(10:12,"/",2017)) 
colnames(PSV)[15:38] <- date

fattore_conversione <- 1.057275
fattore_conv_100 <- fattore_conversione/100

profilo_shipper_axopower_per_mese <- rep(0, 24)
prodotti_attivi <- rep(0,24)
PSV_per_mese <- rep(0,24)

for(d in 1:length(date))
{
  dd <- as.character(unlist(strsplit(date[d], "/")))
  #tm <- take_month(dd[1], dd[2], profili_shipper_AxoPower)
  #number_of_days <- length(tm)
  #profilo_shipper_axopower_per_mese[d] <- sum_by_month(dd[1], dd[2], profili_shipper_AxoPower)
  #not_zero <- which(profili_shipper_AxoPower[,tm[1]] != 0)
  #prodotti_attivi[d] <- length(not_zero)
  
  
  rows <- which(!is.na(PSV[date[d]]))
  
  PSV_per_mese[d] <- sum(PSV[rows,4] * number_of_days * 24 * fattore_conversione * 1/100 * PSV[rows,13] * PSV[rows,which(names(PSV) == date[d])] ,na.rm = TRUE)
  
}

############################################
#### CALCOLO MERCATO A PRONTI
### bisogna dare i nomi a tutte le colonne
stoc <- openxlsx::read.xlsx("stoccaggio.xlsx", sheet = 1, colNames = TRUE)
tot_termine <- openxlsx::read.xlsx("tot_mercato_termine.xlsx", sheet = 1)
gia <- openxlsx::read.xlsx("giacenza.xlsx", sheet = 1,colNames = FALSE)

valore_mensile_stoccaggio <- rep(0,15)
fabbisogno_mensile_APshipper <- rep(0,24)
date <- c(paste0("0",1:9,"/",2016), paste0(10:12,"/",2016), paste0("0",1:9,"/",2017), paste0(10:12,"/",2017)) 
profili_shipper_AxoPower <- attivi[, 38:768]
colnames(profili_shipper_AxoPower) <- colnames(attivi[, 38:768])
colnames(curve_profili) <-prof_consumo[,1]


for(i in 1:length(date))
{
  year <- unlist(strsplit(as.character(date[i]), "/"))
#  fabbisogno_mensile_APshipper[i] <- sum_by_month(year[1], year[2], profili_shipper_AxoPower)#attivi[, 38:768])
  fabbisogno_mensile_APshipper[i] <- sum_by_month(year[1], year[2], curve_profili)#attivi[, 38:768])
  
    inizio <- paste0("01/", date[i])
  if(i %in% c(4,6,9,11)) fine <- paste0("30/", date[i])
  else if(i %in% c(1,3,5,7,8,10,12)) fine <- paste0("31/", date[i])
  else if((i == 2) & (as.numeric(year[2]) %% 4 == 0)) fine <- paste0("29/", date[i])
  else fine <- paste0("28/", date[i])
  print(inizio)
  print(fine)
  print(inizio %in% unlist(stoc[,1]) & fine %in% unlist(stoc[,1]))
  if(inizio %in% unlist(stoc[,1]) & fine %in% unlist(stoc[,1]))
  {
    row_in <- which(stoc[,1] == inizio)
    row_fi <- which(stoc[,1] == fine)
    print(stoc[row_in,11])
    print(stoc[row_fi,11])
    print("#####")
    valore_mensile_stoccaggio[i] <- as.numeric(stoc[row_in,11]) - as.numeric(stoc[row_fi,11])
  } 
  
}

valore_mensile_stoccaggio_e <- valore_mensile_stoccaggio/1000 * 21.875
valore_mensile_stoccaggio <- (valore_mensile_stoccaggio/1000)/(fattore_conversione/100)
termine <- tot_termine[,1]

valore_mensile_stoccaggio <- data.frame(t(valore_mensile_stoccaggio))
termine <- data.frame(t(termine))

colnames(valore_mensile_stoccaggio) <- date[1:length(valore_mensile_stoccaggio)]
colnames(termine) <- date[1:length(termine)]


mercato_a_pronti <- rep(0, 12)
for(i in 1:15)
{
  mercato_a_pronti[i] <- fabbisogno_mensile_APshipper[i] - (valore_mensile_stoccaggio[i] + termine[i])
}

########################################
## VALORIZZAZIONE MERCATO A PRONTI
lg <- openxlsx::read.xlsx("listing_giornaliero.xlsx", sheet = 1, colNames = TRUE)
lg <- lg[,2:3]
nameslg <- lg[,1]
lg <- unlist(lg[,2])
lg <- t((data.frame(as.numeric(lg))))
colnames(lg) <- nameslg


valore_mercato_pronti <- rep(0,15)

(colSums(attivi[,38:68],na.rm=TRUE) - (valore_mensile_stoccaggio[1]/31 + termine[1]/31))          ### in m3
lg[1:31]*(colSums(attivi[,38:68],na.rm=TRUE) - (valore_mensile_stoccaggio[1]/31 + termine[1]/31)) ### in euro

take_month2_profili <- function(month, year, res)
{
  index_list <- c()
  for(i in 1:length(colnames(res)))
  {
    name <- colnames(res)[i]
    sdt <- split_date(name)
    if(as.numeric(sdt[2]) == month & as.numeric(sdt[3]) == year) index_list <- c(index_list, i)
  }
  return(index_list)
}


take_month2 <- function(month, year, res)
{
  index_list <- c()
  for(i in 1:length(colnames(res)))
  {
    name <- colnames(res)[i]
    sdt <- split_date(name)
    if(as.numeric(sdt[1]) == month & as.numeric(sdt[2]) == year) index_list <- c(index_list, i)
  }
  return(index_list)
}


compute_open_position_per_month <- function(fabbisogno, vmstoccaggio, termine, listing, month, year)
{
  days <- 0
  if(as.numeric(month) %in% c(1,3,5,7,8,10,12)) days <- 31
  else if(as.numeric(month) %in% c(4,6,9,11)) days <- 30
  else if(as.numeric(year) %% 4 == 0 & as.numeric(month) == 2) days <- 29
  else days <- 28
  
  fabb_mese <- fabbisogno[,take_month2_profili(month, year, fabbisogno)]
  print("fabb")
  print(colSums(fabb_mese,na.rm=TRUE))
  print("prezzi")
  l_mese <- listing[take_month2_profili(month, year, listing)]
  print(l_mese)
  #if(length(l_mese) == 1) l_mese <- rep(l_mese, days)
  st <- as.numeric(rep(vmstoccaggio[take_month2(month,year,vmstoccaggio)]/days, days))
  te <- as.numeric(rep(termine[take_month2(month,year,termine)]/days, days))
  print("stoccaggio")
  print(st)
  print("termine")
  print(te)
  op <- l_mese * (colSums(fabb_mese, na.rm=TRUE) - ( st + te) )
  print("op")
  print(op)  
  return(op)
}

stoc <- stoc[3:nrow(stoc),]
stoc_gen <- as.numeric(stoc[92:123,12])

stoc_prog <- rep(0, 31)
for(i in 1:31) stoc_prog[i] <- stoc_gen[i] - stoc_gen[i+1] 

sp <- (stoc_prog/1000)/(fattore_conversione/100)

compute_open_position_stoc <- function(fabbisogno, vmstoccaggio, termine, listing, month, year)
{
  days <- 0
  if(as.numeric(month) %in% c(1,3,5,7,8,10,12)) days <- 31
  else if(as.numeric(month) %in% c(4,6,9,11)) days <- 30
  else if(as.numeric(year) %% 4 == 0 & as.numeric(month) == 2) days <- 29
  else days <- 28
  
  fabb_mese <- fabbisogno[,take_month2_profili(month, year, fabbisogno)]
  print("fabb")
  print(colSums(fabb_mese,na.rm=TRUE))
  print("prezzi")
  l_mese <- listing[take_month2_profili(month, year, listing)]
  print(l_mese)
  #if(length(l_mese) == 1) l_mese <- rep(l_mese, days)
  
  
  te <- as.numeric(rep(termine[take_month2(month,year,termine)]/days, days))
  print("stoccaggio")
  print(vmstoccaggio)
  print("termine")
  print(te)
  op <- l_mese * (colSums(fabb_mese, na.rm=TRUE) - ( vmstoccaggio + te) )
  print("op")
  print(op)  
  return(op)
}



gpm <- c(31,29,31,30,31,30,31,31,30,31,30,31, 31,28,31)

for(i in 1:length(mercato_a_pronti))
{
  if(i <= 3)
  {
    valore_mercato_pronti[i] <- lg[]*mercato_a_pronti[i]
  }
  
}

valore_mercato_pronti


x <- c(31,29,31,30,31,30,31,31,30,31,30,31)

fattore_conversione <- 1.057275
gia2 <- c(gia[1,1] - gia[32,1], gia[32,1] - gia[61,1], gia[61,1])
gia2 <- (gia2/1000)*(100/fattore_conversione)
gia2 <- c(gia2[1]/31, gia2[2]/29, gia2[3]/31)
tt <- c(661509,	631509,	571509,	138331,	138331,	138331,	138331,	138331	,138331	,695433	,695433,	695433,	340157,	340157,	340157)
tt <- as.numeric(termine)
tt2 <- c(tt[1:12]/x, tt[13:15]/c(31,28,31))
tt3 <- c(tt2[1:3], tt[4:9]/183, tt[10:12]/92 , tt[13:15]/90)
tt3 <- c(tt2[1:3], rep(sum(tt[4:9])/183, 6), rep(sum(tt[10:12])/92, 3), rep(sum(tt[13:15])/90, 3))

y <- termine[1:12]
X <- 1:366
Y <- c()
for(i in 1:12)
{
  Y <- c(Y, rep(y[i], x[i]))
}

##################################################
##################################################
##################################################
########## OPEN POSITION IN VOLUMI ###############

compute_profile_fixed_APS <- function(ver)
{
  result <- rep(0,731)
  dates <- c(paste0("01/","0",1:9,"/",2016), paste0("01/",10:12,"/",2016), paste0("01/","0",1:9,"/",2017), paste0("01/",10:12,"/",2017)) 
  for(dt in dates)
  {
    dts <- split_date(dt)
    #print(dts)
    fixes <- which(ver[dt] == "F")
    month <- take_month(dts[2], dts[3], ver[,38:768])
    #print(length(month))
    VV <- ver[,38:768]
    #print(month)
    fix <- as.data.frame.matrix(VV[fixes,month])
    result[month] <- colSums(fix, na.rm = TRUE)
  }
  #ver[,14:37]
  res <- data.frame(t(result))
  colnames(res) <- colnames(ver)[38:768]
  return(res)
}

mkt <- c(rep(tt[1]/31,31), rep(tt[2]/29, 29), rep(tt[3]/31,31),rep(sum(tt[4:9])/183, 183), rep(sum(tt[10:12])/92, 92),rep(340157.48/90, 90),rep(0, 365-90) )
stok <- c(as.numeric(stoc[95:185,11]), rep(0,366-91), rep(0,365))
stok_prog <- rep(0, 731)
for(i in 1:730) stok_prog[i] <- stok[i] - stok[i+1] 

prof <- compute_profile_fixed_APS(ver)
stok_prog <- (stok_prog/(10*fattore_conversione))
open_position <- prof - (mkt + stok_prog)

plot(122:731, open_position[122:731], type = "l", lwd = 2, xlab = "time", ylab = "profilo", col = "navy",main="open position in Sm3 da 01/05/2016")
abline(h=0) 
xlsx::write.xlsx(open_position[122:731], "open_position.xlsx", row.names = TRUE, col.names = TRUE)


##############################################

plot(1:731, profilo_shipper_axopower, type = "l", lwd = 2, xlab = "time", ylab = "profilo", col = "blue")
##lines(1:731, compute_profile_fixed_APS(ver), type = "l", lwd = 1, xlab = "time", ylab = "profilo", col = "black")
plot(1:731, colSums(ver[,38:768], na.rm=TRUE), type = "l", lwd = 2, xlab = "time", ylab = "profilo", col = "red")
rect(0, 0, 31, gia2[1]+tt3[1], col = "grey")
rect(31, 0, 61, gia2[2]+tt3[2], col = "grey")
rect(61, 0, 93, gia2[3]+tt3[3], col = "grey")
#rect(0, 0, 31, tt2[1], col = "magenta")
#rect(31, 0, 61, tt2[2], col = "magenta")
#rect(61, 0, 93, tt2[3], col = "magenta")
rect(93, 0, 123, tt3[4], col = "magenta")
rect(123, 0, 155, tt3[5], col = "magenta")
rect(155, 0, 186, tt3[6], col = "magenta")
rect(186, 0, 218, tt3[7], col = "magenta")
rect(218, 0, 250, tt3[8], col = "magenta")
rect(250, 0, 281, tt3[9], col = "magenta")
rect(285, 0, 317, tt3[10], col = "magenta")
rect(317, 0, 348, tt3[11], col = "magenta")
rect(348, 0, 380, tt3[12], col = "magenta")
rect(380, 0, 411, tt3[13], col = "magenta")
rect(411, 0, 439, tt3[14], col = "magenta")
rect(439, 0, 470, tt3[15], col = "magenta")
lines(1:731, colSums(ver[,38:768], na.rm=TRUE), type = "l", lwd = 2, xlab = "time", ylab = "profilo", col = "black")

#polygon(c(1:31,rep(gia2[1],31)),c(1:31,rep(0,31)), col = "grey")
#polygon(c(32:61,rep(0,29)),c(32:61,rep(gia2[2],29)), col = "grey")
#polygon(c(62:93,rep(0,31)),c(62:93,rep(gia2[3],31)), col = "grey")



###############################################################################################################################
###############################################################################################################################

### ACQUISTO DA SHIPPER (automaticamente bilanciato)

btb <- openxlsx::read.xlsx("back_to_back.xlsx", sheet = 1, colNames = TRUE)
#### mesi: colonne da 6 a 17, acquisto: colonna 36, data inizio e data fine: 23 e 25.
btb <- btb[2:nrow(btb),]

compute_month <- function(j)
{
  nm <- 0
  
  if(j <= 12) nm <- j %% 13
  else nm <- (j %% 13) + 1
  
  return(nm)
}


profilo_acquisti_shipper_terzi <- function(btb)
{
  prelievi <- btb[,6:17]
  date <- btb[,c(23,25)]
  acquisti <- btb[,36]/100
  prof_acquisti <- matrix(0, nrow = nrow(btb), ncol = 24)
  colnames(prof_acquisti) <- days <- c(paste0("01/","0",1:9,"/",2016), paste0("01/",10:12,"/",2016), paste0("01/","0",1:9,"/",2017), paste0("01/",10:12,"/",2017)) 
  
  for(i in 1:nrow(btb))
  {
    for(j in 1:24)
    {
      if( compare_dates(days[j], date[i,1] ) & !compare_dates(days[j], date[i,2]))
      {
        #print(i)
        print(j)
        nm <- compute_month(j)
        prof_acquisti[i,j] <- as.numeric(prelievi[i,nm]) * as.numeric(acquisti[i])
        
      }
    }
    print(prof_acquisti[,j])
  }
  return(prof_acquisti)
}

colSums(profilo_acquisti_shipper_terzi(btb))


##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
### PARTE VENDITA

### rigenerazione tabella di passaggi fissi-variabili partendo da DB
take_actives <- function(ven)
{
  crs1 <- ven["CLIENTE_RAGIONE_SOCIALE"]
  crs <- unique(unlist(crs1))
  actives <- c()
  for(c in crs)
  {
    rows <- which(ven["CLIENTE_RAGIONE_SOCIALE"] == c)
    pdr <- unique(unlist(ven[rows,"PDR"]))
    for(p in pdr)
    {
      R <- which(ven["PDR"] == p)
      actives <- c(actives, max(R))
    }
  }
  return(ven[actives,])
}

#### USARE COMPUTE_COMBINATIONS2 #####
compute_combinations_for_sell <- function(attivi)
{
  aggregati <- data_frame()
  count <- 0
  clienti <- as.character(unique(unlist(attivi[,3])))
  for(prod in clienti)
  {
    
    aggregati2 <- data_frame()
    rows <- which(attivi[,3] == prod)
    tabella2 <- attivi[rows,]
    prod2 <- attivi[rows,6]
    data_inizio <- unique(unlist(tabella2[,8]))
    for(di in data_inizio)
    {
      rows2 <- which(tabella2[,8] == di)
      data2 <- tabella2[rows2,]
      data_fine <- unique(unlist(data2[,10]))
      for(df in data_fine)
      {
        rows3 <- which(data2[,10] == df)
        data3 <- data2[rows3,]
          for(j in 1:length(rows3))
          {
            #print(paste("number of rows = ", length(rows3)))
            first_letter <- stri_sub(prod2, 1, 1)
            FV <- stri_sub(prod2, 6, 6)
            cfv <- change_F_to_V(j, data3)
            if(cfv[[1]] == "TRUE" & first_letter == "L" & FV == "F" & compare_dates(df,"31/12/2017"))
            {
              new_prod_name <- paste0("R", stri_sub(prod2, 2, nchar(prod2)))
              tkm <- take_maximum_date("01/01/2016",cfv[[2]])
              min_fin <- take_minimum_date("31/12/2017", df)
              #print(tkm)
              fisso <- data.frame(cbind(prod2, tkm, cfv[[3]]))
              #print(fisso)
              colnames(fisso) <- c("prodotto","data inizio", "data fine")
              variabile <- data.frame(cbind(new_prod_name, cfv[[4]], min_fin))
              #print(variabile)
              colnames(variabile) <- c("prodotto","data inizio", "data fine")
              aggregati2 <- bind_rows(aggregati2, fisso)
              aggregati2 <- bind_rows(aggregati2, variabile)
              colnames(aggregati2) <- c("prodotto","data inizio", "data fine")
              #print(prod2)
              
              #print(count)
            }
            else
            {
              fisso <- data.frame(cbind(prod2, di, df))
              #print(fisso)
              colnames(fisso) <- c("prodotto","data inizio", "data fine")
              aggregati2 <- bind_rows(aggregati2, fisso)
              colnames(aggregati2) <- c("prodotto","data inizio", "data fine")
              count <- count + 1
            }
            count <- count + 1
          }
      }
    }
    if(nrow(aggregati2) > 0) 
    {
      colnames(aggregati2) <- c("prodotto","data inizio", "data fine")
      aggregati <- bind_rows(aggregati, aggregati2)
    }
  }
  colnames(aggregati) <- c("prodotto","data inizio", "data fine")
  print(count)
  return(aggregati)
}

sell <- compute_combinations_for_sell(attivi)
nrow(attivi)
head(sell)

ver <- openxlsx::read.xlsx("per_verifica.xlsx", sheet = 1, colNames = TRUE)
ver <- ver[2:nrow(ver),] 
head(ver)


not_sup <- ver[-which(ver[,6] == "SUPERI_E_QFISSA"),]

e_list <- c()
for(i in 1:nrow(ver))
{
  if(("F" %in% ver[i,14:37]) | ("V" %in% ver[i,14:37]))
  {
    e_list <- c(e_list, i)
  }
}
  
length(e_list)
head(e_list)
ver <- ver[e_list,]

rm(sell2)
sell2 <- compute_combinations_for_sell(ver)
nrow(ver)


#####
pm <- openxlsx::read.xlsx("profili_mensili.xlsx", sheet = 1, colNames = TRUE)
listing <- openxlsx::read.xlsx("listing.xlsx", sheet = 1, colNames = TRUE)
listing <- unlist(listing[[1]])
head(pm)
listing
vendite <- openxlsx::read.xlsx("combinazioni_prodotti_con_passaggi2.xlsx", sheet = 1, colNames = TRUE)
head(vendite)
vendite <- vendite[,-c(2,3,5)]
colnames(vendite) <- vendite[1,]
vendite <- vendite[2:nrow(vendite),] 
head(vendite)
vendite[is.na(vendite)] <- 0
vendite[is.na(vendite)] <- 0

####### 
### colonne di vendite:
### QTMVC 2016: 11 - 22
### CPR 2016: 23 - 34
### GRAD 2016: 35 - 46
### CCR 2016: 47 - 58
### QTINT 2016: 59 - 70
### QTPSV 016: 71 - 82
### QVDVAR 2016: 83 - 94
### CCVDVAR 2016: 95 - 106
############
### QTMVC 2017: 107 - 118
### CPR 2017: 119 - 130
### GRAD 2017: 131 - 142
### CCR 2017: 143 - 154
### QTINT 2017: 155 - 166
### QTPSV 2017: 167 - 178
### QVDVAR 2017: 179 - 190
### CCVDVAR 2017: 191 - 202


total_sellings <- function(vendite, pm, listing)
{
  #total <- rep(0, nrow(vendite))
  total_matrix <- matrix(0, nrow = nrow(vendite), ncol = 24)
  
  for(i in 1:nrow(vendite))
  {
    P0 <- as.numeric(vendite[i,9])
    Prif <- 0 
    fixed <- vendite[i,10]   
    if (fixed != "brent") {Prif <- as.numeric(vendite[i,10])}
    print(i)
    inizio <- split_date(vendite[i,2])
    fine <- split_date(vendite[i,3])
    inizio <- paste0("01/",inizio[2],"/", inizio[3])
    fine <- paste0("01/",fine[2], "/",fine[3])
    pc <- which(colnames(pm) == vendite[i,4])
    profm <- (as.numeric(vendite[i,5]) * pm[,pc])
    
    dnames2016 <- colnames(vendite[i, 11:22])
    dnames2017 <- colnames(vendite[i, 107:118])
    
    qvdvar2016 <- ccvdvar2016 <- qvdvar2017 <- ccvdvar2017 <- rep(0, 12)
    
    qtmvc2016 <- as.numeric(unlist(vendite[i, 11:22]))
    cpr2016 <- as.numeric(unlist(vendite[i, 23:34]))
    grad2016 <- as.numeric(unlist(vendite[i, 35:46]))
    ccr2016 <- as.numeric(unlist(vendite[i, 47:58]))
    qtint2016 <- as.numeric(unlist(vendite[i, 59:70]))
    qtpsv2016 <- as.numeric(unlist(vendite[i, 71:82]))
    
    qtmvc2017 <- as.numeric(unlist(vendite[i, 107:118]))
    cpr2017 <- as.numeric(unlist(vendite[i, 119:130]))
    grad2017 <- as.numeric(unlist(vendite[i, 131:142]))
    ccr2017 <- as.numeric(unlist(vendite[i, 143:154]))
    qtint2017 <- as.numeric(unlist(vendite[i, 155:166]))
    qtpsv2017 <- as.numeric(unlist(vendite[i, 167:178]))
    
    if(vendite[i,8] == "S" & vendite[i,7] == "0")
    {
      ccvdvar2016 <- as.numeric(unlist(vendite[i, 95:106]))  
      ccvdvar2017 <- as.numeric(unlist(vendite[i, 191:202]))
    }
    
    else if(vendite[i,8] == "0" & vendite[i,7] == "S")
    {
      qvdvar2016 <- as.numeric(unlist(vendite[i, 83:94]))
      qvdvar2017 <- as.numeric(unlist(vendite[i, 179:190]))
    }
    
    s_i <- e_i <- 0
    bool_s_2016 <- bool_e_2016 <- FALSE
    
    if( compare_dates(inizio, "01/01/2016") )
    {
      s_i <- which(dnames2016 == inizio)
      bool_s_2016 <- TRUE
      if(length(s_i) == 0)
      {
        s_i <- which(dnames2017 == inizio)
        bool_s_2016 <- FALSE
      }
    }
    
    else
    {
      s_i <- 1
      bool_s_2016 <- TRUE
    }
    
    if( compare_dates("31/12/2017", fine) )
    {
      e_i <- which(dnames2017 == fine)
      bool_e_2016 <- FALSE
      if(length(e_i) == 0)
      {
        e_i <- which(dnames2016 == fine)
        bool_e_2016 <- TRUE
      }
    }
    
    else
    {
      e_i <- 12
      bool_e_2016 <- FALSE
    }
    
    compute_fixed_price <- function(P0,bool_s_2016, bool_e_2016, s_i, e_i)
    {
      result <- rep(0,24)
      if(bool_s_2016 == TRUE & bool_e_2016 == TRUE)
      {
        result[s_i:e_i] <- (P0 * profm[s_i:e_i])/100  +  (profm[s_i:e_i] * (qtmvc2016[s_i:e_i] + cpr2016[s_i:e_i] + grad2016[s_i:e_i] +
                          ccr2016[s_i:e_i] +
                          qtint2016[s_i:e_i] +
                          qtpsv2016[s_i:e_i] +
                          qvdvar2016[s_i:e_i] +
                          ccvdvar2016[s_i:e_i]))/100
      }
      
      else if(bool_s_2016 == TRUE & bool_e_2016 == FALSE)
      {
        result1 <- result2 <- rep(0, 12)
        result1[s_i:12] <- (P0 * profm[s_i:12])/100 + (profm[s_i:12] * (qtmvc2016[s_i:12] + cpr2016[s_i:12] + grad2016[s_i:12] +
                                            ccr2016[s_i:12] +
                                            qtint2016[s_i:12] +
                                            qtpsv2016[s_i:12] +
                                            qvdvar2016[s_i:12] +
                                            ccvdvar2016[s_i:12]))/100
        
        result2[1:e_i] <- (P0 * profm[1:e_i])/100 + (profm[1:e_i] * (qtmvc2017[1:e_i] + cpr2017[1:e_i] + grad2017[1:e_i] +
                                            ccr2017[1:e_i] +
                                            qtint2017[1:e_i] +
                                            qtpsv2017[1:e_i] +
                                            qvdvar2017[1:e_i] +
                                            ccvdvar2017[1:e_i]))/100
        result <- c(result1,result2)
      }
      
      else if(bool_s_2016 == FALSE & bool_e_2016 == TRUE)
      {
        print("there is something wrong")
      }
      
      else
      {
        result[s_i:e_i] <- (P0 * profm[s_i:e_i])/100 + (profm[s_i:e_i] * (qtmvc2017[s_i:e_i] + cpr2017[s_i:e_i] + grad2017[s_i:e_i] +
                                            ccr2017[s_i:e_i] +
                                            qtint2017[s_i:e_i] +
                                            qtpsv2017[s_i:e_i] +
                                            qvdvar2017[s_i:e_i] +
                                            ccvdvar2017[s_i:e_i]))/100
      }
      #return(sum(result))
      return(result)
    }
    
    compute_indexed_price <- function(P0, Prif, listing, bool_s_2016, bool_e_2016, s_i, e_i)
    {
      result <- rep(0, 24)
      listing_2016 <- listing[1:12]
      listing_2017 <- listing[13:24]
      if(bool_s_2016 == TRUE & bool_e_2016 == TRUE)
      {
        result[s_i:e_i] <- ((P0 + (listing_2016[s_i:e_i] - Prif)) * profm[s_i:e_i])/100  + (profm[s_i:e_i] * (qtmvc2016[s_i:e_i] + cpr2016[s_i:e_i] + grad2016[s_i:e_i] +
                                            ccr2016[s_i:e_i] +
                                            qtint2016[s_i:e_i] +
                                            qtpsv2016[s_i:e_i] +
                                            qvdvar2016[s_i:e_i] +
                                            ccvdvar2016[s_i:e_i]))/100
      }
      
      else if(bool_s_2016 == TRUE & bool_e_2016 == FALSE)
      {
        result1 <- result2 <- rep(0, 12)
        result1[s_i:12] <- ((P0 + (listing_2016[s_i:12] - Prif)) * profm[s_i:12])/100  + (profm[s_i:12] * (qtmvc2016[s_i:12] + cpr2016[s_i:12] + grad2016[s_i:12] +
                                            ccr2016[s_i:12] +
                                            qtint2016[s_i:12] +
                                            qtpsv2016[s_i:12] +
                                            qvdvar2016[s_i:12] +
                                            ccvdvar2016[s_i:12]))/100
        result2[1:e_i] <- ((P0 + (listing_2017[1:e_i] - Prif)) * profm[1:e_i])/100  + (profm[1:e_i] * (qtmvc2017[1:e_i] + cpr2017[1:e_i] + grad2017[1:e_i] +
                                           ccr2017[1:e_i] +
                                           qtint2017[1:e_i] +
                                           qtpsv2017[1:e_i] +
                                           qvdvar2017[1:e_i] +
                                           ccvdvar2017[1:e_i]))/100
        result <- c(result1, result2)
      }
      
      else if(bool_s_2016 == FALSE & bool_e_2016 == TRUE)
      {
        print("there is something wrong")
      }
      
      else
      {
        result[s_i:e_i] <- ((P0 + (listing_2017[s_i:e_i] - Prif)) * profm[s_i:e_i])/100  + (profm[s_i:e_i] * (qtmvc2017[s_i:e_i] + cpr2017[s_i:e_i] + grad2017[s_i:e_i] +
                                            ccr2017[s_i:e_i] +
                                            qtint2017[s_i:e_i] +
                                            qtpsv2017[s_i:e_i] +
                                            qvdvar2017[s_i:e_i] +
                                            ccvdvar2017[s_i:e_i]))/100
      }
      #return(sum(result))
      return(result)
    }
    
    
    compute_price <- function(P0, Prif, fixed, listing, bool_s_2016, bool_e_2016, s_i, e_i)
    {
      if(fixed == "brent")
      {
        return(compute_fixed_price(P0, bool_s_2016, bool_e_2016,s_i, e_i))
      }
      else
      {
        return(compute_indexed_price(P0, Prif, listing,bool_s_2016, bool_e_2016, s_i, e_i))
      }
    }
      
      
    total_matrix[i,] <- compute_price(P0, Prif, fixed, listing, bool_s_2016, bool_e_2016, s_i, e_i)  
    
  }# chiude for i
  
  DF <- data.frame(cbind(vendite[,1:3], total_matrix))
  
  #return(total)
  return(DF)
}

test <- vendite[1:10,]
total_sellings(test, pm, listing)

TOT <- total_sellings(vendite, pm, listing)
head(TOT)
TOT
TOT[which(TOT[,1] == "LGD_MF_1501"),]
TOT[which(TOT[,1] == "RGD_MF_1502"),]

source("sellings_with_components.R")

TM <- total_sellings_per_components(vendite, pm, listing)


#######################################################################
#######################################################################

estrai_tutto <- function()
{
  sell <- colSums(total_sellings(vendite, pm, listing)[,4:27])
  terzi <- colSums(profilo_acquisti_shipper_terzi(btb))
  termine 
  valore_mensile_stoccaggio_e
  pronti <- c()
  for(i in 1:12) pronti <- c(pronti, sum(compute_open_position_per_month(profili_shipper_AxoPower, valore_mensile_stoccaggio, termine, lg, i,2016),na.rm=TRUE))
  
  M <- matrix(0, nrow = 5, ncol=12)
  
  M[1,] <- unname(sell[1:12])
  M[2,] <- unname(terzi[1:12])
  M[3,] <- unname(termine[1:12])
  M[4,] <-  unname(pronti[1:12])
  M[5,] <- unname(valore_mensile_stoccaggio[1:12])
  
  M <- rbind(unname(sell[1:12]), unname(terzi[1:12]), unname(termine[1:12]), unname(pronti), unname(valore_mensile_stoccaggio_e[1:12]))
  
  # df <- data.frame(rbind(unname(sell[1:12]), unname(terzi[1:12]), unname(termine[1:12]), unname(pronti), unname(valore_mensile_stoccaggio[1:12])))
  # colnames(DF) <- c("gennaio", "febbraio","marzo","aprile","maggio","giugno",
  #                   "luglio", "agosto","settembre","ottobre","novembre","dicembre")
  # 
  return(M)
}


DF <- estrai_tutto()


a <- c(899265.5 , 885352.8 ,1011205.9 , 565991.1  ,277004.9 , 266784.3,  253839.7 , 252834.8  ,273687.7,  406158.3  ,747841.9, 1035687.1)

b <- c(183521.92, 173497.29, 132259.99,  84749.26,  91021.19  ,71770.89,  68378.10  ,32922.75,  71382.60,  51838.66,  36518.91,  58920.18)

#d <- c(661508.7, 631508.7, 571508.7 ,138330.7 ,138330.7 ,138330.7 ,138330.7 ,138330.7, 138330.7 ,695433.1 ,695433.1 ,695433.1)
d <- tot_termine[,2]

e <- c(156508.0662,	140778.2299	,134374.5575	,27216	,28123.2,	27216	,28123.2	,28123.2,	27216,	152706,	147780,	152706)#,	55428	,50064	,55428)

f <- c(170673.34 ,120968.27 ,57966.65 , 0 , 0,  0 , 0 , 0 , 0,  0,  0,  0)

TM <- rbind(TM[,1:12], b)                  
TM <- rbind(TM, d)
TM <- rbind(TM, pronti[1:12])
TM <- rbind(TM, f)

colnames(TM) <- c("gennaio", "febbraio","marzo","aprile","maggio","giugno",
                  "luglio", "agosto","settembre","ottobre","novembre","dicembre")

rownames(TM) <- c("PGas", "qtmvc", "cpr", "grad", "ccr", "qtint", "qtpsv", "qvdvar", "ccvdvar", "total","acquisti da terzi", "mercato a termine", "mercato a pronti", "stoccaggio")


xlsx::write.xlsx(TM, "estrazione.xlsx", row.names = TRUE, col.names = TRUE)

##############################################################################
##############################################################################
##############################################################################
 ####### VERIFICA  CON DATI A CONSUNTIVO

cons <- openxlsx::read.xlsx("dati_consuntivo_gennaio.xlsx", sheet = 1, colNames = TRUE)
cons[is.na(cons)] <- 0

pgas <- as.numeric(cons["PGAS"][[1]])
sum(pgas[pgas > 0] ,na.rm=TRUE)
sum(as.numeric(cons["CONSUMO_SMC"][[1]]) ,na.rm=TRUE)


not_axo <- ver[-which(ver["SHIPPER"] == "0001808491-AXOPOWER SRL"),]
head(not_axo)

ana <- openxlsx::read.xlsx("anagrafica_originale.xlsx", sheet = 1, colNames = TRUE)
na <- ana[1,]
ana <- ana[2:nrow(ana),]
colnames(ana) <- na
ana[is.na(ana)] <- 0
head(ana)

not_axo <- ana[-which(ana["SHIPPER"] == "0001808491-AXOPOWER SRL"),]
head(not_axo)

test <- openxlsx::read.xlsx("test.xlsx", sheet = 1, colNames = TRUE)

for( i in 1:nrow(test))
{
  print(test[i, "PDR"])
  cd <- test[i,"CONSUMO_DISTRIBUTORE"]
  cod <- test[i,"PROFILO_PRELIEVO"]
  print(cod)
  col <- which(colnames(pm) == cod)
  print(colnames(pm)[col])
  print(as.numeric(cd) * pm[,col])
}

variabili <- c("FATTURA_TOTALE",	"SITO_IMPONIBILE",	"SITO_IVA",	"SITO_TOTALE",	"IVA"	,"CONSUMO_SMC",	"PGAS",	"CMEM_AXO",	"CONSUMO_GAS"	,"GRAD",
               "CCR"	,"QTPSV"	,"QTPSV_AXO",	"QTINT",	"QTINT_AXO",	"QTMCV_MAGGIORAZIONI_CV",	"CPR",	"VENDITA_GAS_CLIENTI"	,"QVD_FISSA"	,"QVD_VARIABILE",	"CCVD_FISSA"	,
               "CCVD_VARIABILE",	"RET",	"GST"	,"UG3T"	,"QTI",	"QTI_S1",	"QTI_S2",	"TERMINE_FISSO",	"TRASPORTO_PENALE_SUP_CAP",	"DISTRIBUZIONE_GAS"	,"ACCISE_GAS"	,
               "IMPOSTA_BOLLO",	"DEPOSITI_CAUZIONALI_GAS"	,"ADEGUAMENTO_CONTR_ALLACC",	"GESTIONE_SWITCHOUT_CL_RETAIL",	"SCONTI_ENERGIA",	"INT_MORA"	,
               "SPESE_INVIO_FATTURA",	"ONERE_GARANZIA_GAS"	,"VALORE_PCS_FATT"	,"VOLUME_FATT_ADEG_PCS",	"IMPORTO_PCS_FATT",	"SBIL_GAS",	"ONERI_CRV",	"PORTAFOGLIO" ,"FIX/INDEX")


num <- as.data.frame.matrix(cons[,which(colnames(cons) %in% variabili)])
num <- data.frame(num)

colSums(num)

#xlsx::write.xlsx(colSums(num), "cons.xlsx", row.names = TRUE, col.names = TRUE)


###############################################################
###############################################################
###############################################################
############ CALCOLO TRANSFER PRICE ###########################

TP <- openxlsx::read.xlsx("TP.xlsx", sheet = 1, colNames = TRUE)
TP[is.na(TP)] <- 0

pm <- openxlsx::read.xlsx("profili_mensili.xlsx", sheet = 1, colNames = TRUE)

pers_list <- openxlsx::read.xlsx("pers_list.xlsx", sheet = 1, colNames = FALSE)

rows <- which(TP[,2] %in% unlist(pers_list[,1]))

TP2 <- TP[c(rows,985),]
rows2 <- which(TP[,2] == "LGD_MF_1506")
TPr <- TP[rows2,]


TOT_m3 <- function(prod, pm)
{
  tot_m3 <- rep(0, 24)
  for(i in 14:37)
  {
    print(i)
    nz <- which(prod[,i] != 0)
    if(length(nz) > 0)
    {
      nzp <- prod[nz,]
      ds <- split_date(colnames(prod)[i])
      print(ds)
      for(j in 1:nrow(nzp))
      {
        col <- which(colnames(pm) == nzp[j,"profilo"])
        #print((pm[,col] * nzp[j,"consumo"]))
        tot_m3[(i-13)] <- tot_m3[(i-13)] + (pm[as.numeric(ds[2]),col] * nzp[j,"consumo"])
      }
    } 
  }
    
  return(tot_m3)
}

TOT_m3g <- function(prod, pm)
{
  tot_m3 <- rep(0, 731)
  x16 <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  x17 <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  for(i in 1:nrow(prod))
  {
    prof <- prod[i,"profilo"]
    cons <- prod[i,"consumo"]
    col <- which(colnames(pm) == prof)
    c2y <- as.numeric(prod[i,14:37] > 0)
    m2y <- rep(c2y, c(x16, x17))
    tot_m3 <- tot_m3 + (pm[,col]/100 * m2y * cons)
  }
  
  return(tot_m3)
}

tot <- TOT_m3(TP, pm)
tot_test <- TOT_m3(TP2,pm)
TOT_m3(TP2[15,],pm)
totg <- TOT_m3g(TP, prof_consumo)


##### questo da risultati di excel:
compute_TP <- function(TP, pm)
{
  totm3 <- TOT_m3(TP, pm)
  mat <- matrix(0,nrow=20,ncol = 24)
  TPprof <- rep(0,24)
  for(i in 1:24)
  {
    m <- i + 13
    print(i)
    nz <- which(TP[,m] != 0)
    print(paste("nz",length(nz)))
      if(length(nz) > 0)
      {
        nzp <- TP[nz,]
        ds <- split_date(colnames(TP)[m])
        for(j in 1:nrow(nzp))
        {
          #print(j)
          col <- which(colnames(pm) == nzp[j,"profilo"])
          TPprof[i] <- TPprof[i] + ((nzp[j,m]) * (pm[as.numeric(ds[2]),col] * nzp[j,"consumo"]))
          #TPprof[i] <- TPprof[i] + ((nzp[j,m]+nzp[j,12]) * (pm[as.numeric(ds[2]),col] * nzp[j,"consumo"]))
          #mat[j,i] <- ((nzp[j,m]+nzp[j,12]) * (pm[as.numeric(ds[2]),col] * nzp[j,"consumo"]))/100
        }
     }
  }
    
  print(TPprof/100)
  TP2 <- TPprof/totm3
  #print(mat)
  return(TP2)
}

compute_TPg <- function(prod, pm)
{
  totm3 <- TOT_m3g(prod, pm)
  TPp <- rep(0,731)
  
  x16 <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  x17 <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  for(i in 1:nrow(prod))
  {
    prof <- prod[i,"profilo"]
    cons <- prod[i,"consumo"]
    col <- which(colnames(pm) == prof)
    c2y <- as.numeric(prod[i,14:37] > 0) * as.numeric(prod[i,14:37])
    m2y <- rep(c2y, c(x16, x17))
    TPp <- TPp + (pm[,col] * m2y * cons)
  }
  print(TPp/100)
  TP2 <- TPp/totm3
  return(TP2)
}


# compute_TP2 <- function(TP, pm)
# {
#   totm3 <- TOT_m3(TP, pm)
#   TPprof <- rep(0,24)
#   for(i in 1:24)
#   {
#     m <- i + 16
#     ds <- split_date(colnames(TP)[m])
#     for(j in 1:nrow(TP))
#     {
#       col <- which(colnames(pm) == TP[j,"profilo"])
#       TPprof[i] <- TPprof[i] + ((TP[j,m]+TP[j,16]) * (pm[as.numeric(ds[2]),col] * TP[j,"consumo"]))
#     }
#   }
#   print(TPprof)
#   TP <- TPprof/totm3
#   return(TP)
# }

tp <- compute_TP(TP,pm)
tp2 <- compute_TP(TP2,pm)

sum(tp2[1:12]*tot_test[1:12]/100)
  
plot(1:24, tp2, type = "l", col="red")

for(j in 1:nrow(TP2))
{
  col <- which(colnames(pm) == TP2[j,"profilo"])
  #print(col)
  print(TP2[j,3])
  print((pm[,col] * TP2[j,"consumo"]))
  print("##########")
}


#############################################################
######## da aggiungere al file "estrazione.xlsx" ############


forcest <- openxlsx::read.xlsx("forcest.xlsx", sheet = 1, colNames = TRUE)
est <- openxlsx::read.xlsx("estrazione.xlsx", sheet = 1, colNames = TRUE)

as.data.frame.matrix(est[10,2:13]) - colSums(as.data.frame.matrix(est[11:14,2:13]))

tot_costi_supero_capacita <- -1.3 * tot/100
### tot gas venduto = tot gas acquistato = TOT_m3
### tot ricavi - tot costi:
#margine_AP <- as.numeric(forcest[1,2:13]) - colSums(as.data.frame.matrix(forcest[2:5,2:13])) <---- vedi riga 1963
### tot costi TP:
tot_TP <- tp*tot/100
### margine energy management
margine_EM <- tp*tot/100 - colSums(as.data.frame.matrix(forcest[2:5,2:13]))
### margine commerciale:
margine_comm <- as.data.frame.matrix(est[10,2:13]) - tot_TP[1:12]
### prezzi unitari:
### prezzo di vendita unitario sui ricavi
ric_unit <- as.numeric(forcest[1,2:13])/tot
### costo di approvvigionamento unitario sull'acquistato
unit_approv <- colSums(as.data.frame.matrix(forcest[2:5,2:13]))/tot
### margine unitario commerciale
unit_comm <- (as.numeric(forcest[1,2:13]) - colSums(as.data.frame.matrix(forcest[2:5,2:13])))/tot

df <- data.frame(rbind(c("costi supero",tot_costi_supero_capacita[1:12]), 
                 c("tot costi TP",tot_TP[1:12]),
                 c("margine EM",margine_EM[1:12]),
                 c("margine comm",margine_comm[1:12]),
                 c("ricavi unitari",ric_unit[1:12]),
                 c("approvi unitario", unit_approv[1:12]),
                 c("unit comm", unit_comm[1:12]),
                 c("tot_gas_acq", tot[1:12]),
                 c("tot_gas_vend", tot[1:12])))

xlsx::write.xlsx(df, "estrazione2.xlsx", row.names = FALSE, col.names = TRUE)
#xlsx::write.xlsx(data.frame(test3), "test3.xlsx", row.names = FALSE, col.names = TRUE)

###############################################
###############################################
###############################################
##### VERIFICA A CONSUNTIVO PER TRANSFER PRICE

r181 <- openxlsx::read.xlsx("2016 Report 181.xlsx", sheet = "Report_181", colNames = TRUE)
nms <- r181[1,]
r181 <- r181[2:nrow(r181),]
r181[is.na(r181)] <- 0
colnames(r181) <- nms
head(r181)

gen <- which(r181["MESE_COMP"] == "1")
length(gen)
jan <- r181[gen,]
rm(r181)
dim(jan)

gen <- which(r181["MESE_COMP"] == "2")
jan <- r181[gen,]
gen <- which(r181["MESE_COMP"] == "3")
jan <- r181[gen,]


conprod <- unique(unlist(jan["CD_PRODOTTO"]))
length(conprod)
conprod <- conprod[which(conprod != "SUPERI_E_QFISSA")]
length(conprod)

aggregate_consumi <- function(prod, jan)
{
  mat <- matrix(0, nrow = length(prod), ncol=2)
  for(p in 1:length(prod))
  {
    rows <- which(jan["CD_PRODOTTO"] == prod[p])
    mat[p,1] <- prod[p]
    mat[p,2] <- sum(as.numeric(jan[rows, "CONSUMO_SMC"]))
  }
  return(mat)
}

M <- aggregate_consumi(conprod, jan)

sum(as.numeric(M[,2]))

find_TP_at_date <- function(prod, TP, M, date)
{
  mat <- matrix(0, nrow = length(prod), ncol=3)
  for(i in 1:length(prod))
  {
    print(i)
    print(prod[i])
    ri <- which(unlist(TP["prodotto"]) %in% as.character(prod[i]))
    print(paste("righe", ri))
    rinz <- which(TP[ri,date] != 0)
    print(paste("righe non 0", rinz))
    print(rinz[1])
    if(length(rinz) > 0)
    {
      mat[i,1] <- M[i,1]
      mat[i,2] <- M[i,2]
      mat[i,3] <- TP[ri[rinz[1]], date] #+ TP[ri[rinz[1]],"rischi"]
      print(mat[i,])
      print(TP[ri[rinz[1]],date])
    }
  }
  return(mat)
}


MM <- find_TP_at_date(conprod, TP, M, "01/01/2016")
MM <- find_TP_at_date(conprod, TP, M, "01/02/2016")
MM <- find_TP_at_date(conprod, TP, M, "01/03/2016")

tp_gen_consuntivo <- sum( (as.numeric(MM[,2])*as.numeric(MM[,3])) )/sum(as.numeric(MM[,2]))
#tp_gen_consuntivo <- sum( (as.numeric(MM[,2])*as.numeric(MM[,3])) )/sum(as.numeric(M[,2]))
tp_gen_consuntivo*sum(as.numeric(MM[,2]))/100


M3 <- cbind(MM,M)

#####################################################################
############## VERIFICA RICAVI A CONSUNTIVO #########################
#####################################################################

variabili <- c("FATTURA_TOTALE",	"SITO_IMPONIBILE",	"SITO_IVA",	"SITO_TOTALE",	"IVA"	,"CONSUMO_SMC",	"PGAS",	"CMEM_AXO",	"CONSUMO_GAS"	,"GRAD",
               "CCR"	,"QTPSV"	,"QTPSV_AXO",	"QTINT",	"QTINT_AXO",	"QTMCV_MAGGIORAZIONI_CV",	"CPR",	"VENDITA_GAS_CLIENTI"	,"QVD_FISSA"	,"QVD_VARIABILE",	"CCVD_FISSA"	,
               "CCVD_VARIABILE",	"RET",	"GST"	,"UG3T"	,"QTI",	"QTI_S1",	"QTI_S2",	"TERMINE_FISSO",	"TRASPORTO_PENALE_SUP_CAP",	"DISTRIBUZIONE_GAS"	,"ACCISE_GAS"	,
               "IMPOSTA_BOLLO",	"DEPOSITI_CAUZIONALI_GAS"	,"ADEGUAMENTO_CONTR_ALLACC",	"GESTIONE_SWITCHOUT_CL_RETAIL",	"SCONTI_ENERGIA",	"INT_MORA"	,
               "SPESE_INVIO_FATTURA",	"ONERE_GARANZIA_GAS"	,"VALORE_PCS_FATT"	,"VOLUME_FATT_ADEG_PCS",	"IMPORTO_PCS_FATT",	"SBIL_GAS",	"ONERI_CRV",	"PORTAFOGLIO" ,"FIX/INDEX")

cols <- which(colnames(jan) %in% variabili)
ricavi <- jan[,cols]


for(i in 1:ncol(ricavi)) {ricavi[,i] <- as.numeric(ricavi[,i]); 
ricavi[is.na(ricavi)] <- 0;
print(typeof(ricavi[,i])); 
print(paste(i, which(is.na(ricavi[,i]))))}

somme <- colSums(ricavi)


componenti <- data.frame(cbind("GRAD" = somme["GRAD"],
                               "ccR" = somme["CCR"],
                               "QTPSV" = somme["QTPSV_AXO"],
                               "QTINT" = somme["QTINT_AXO"],
                               "CPR" = somme["CPR"],
                               "QVDVAR+CCVDVAR" = somme["QVD_VARIABILE"] + somme["CCVD_VARIABILE"],
                               "AD_PCS" = somme["IMPORTO_PCS_FATT"]))
magg_CQ <- somme["QTMCV_MAGGIORAZIONI_CV"]

gas <- somme["PGAS"] + somme["CMEM_AXO"] + somme["CONSUMO_GAS"]

PR <- openxlsx::read.xlsx("2016.04.20 P&L gas 2016.xlsx", sheet = "Esposizione vs GME", colNames = TRUE)

head(PR)
colnames(PR)
op <- PR[,6]*PR[,26]
op <- op[1:31]
pronti_attivo <- sum(op[op > 0])
pronti_passivo <- sum(op[op <= 0])


tot_ricavi <- gas + sum(unlist(componenti[1,])) + magg_CQ + pronti_attivo

################################################################
###############################################################
##### TP solo fissi AxoPower e percentuale posizione scoperta

AP <- which(ver["SHIPPER"] == "0001808491-AXOPOWER SRL")
AP <- ver[AP,]

pers_fissi <- c("LGP-BI-IVPR", "LGP-BI-PIUS", "LGP-BI-MITC")
nf <- c()
for(i in 1:nrow(AP))
{
  name <- AP[i,"CODICE_PRODOTTO"] 
  if(name %in% pers_fissi)
  {
    nf <- c(nf,i)
  }
  
  lorr <- stri_sub(name, 1, 1)
  forv <- stri_sub(name, 6, 6)
  if(forv == "F" & lorr != "R")
  {
    nf <- c(nf,i)
  }
}
apf <- AP[nf,]

apf2 <- anni_competenza(apf, "01/01/2016")
apf3 <- extract_relevant_val(apf2)

fissi2 <- compute_combinations_DEF_val(apf3)
unique(unlist(fissi2["prodotto"]))


nf2 <- c()
for(i in 1:nrow(fissi2))
{
  name <- fissi2[i,"prodotto"]
  if(name %in% pers_fissi)
  {
    nf2 <- c(nf2,i)
  }
  lorr <- stri_sub(name, 1, 1)
  forv <- stri_sub(name, 6, 6)
  if(forv == "F" & lorr != "R")
  {
    nf2 <- c(nf2,i)
  }
}
f2 <- fissi2[nf2,]

unique(unlist(f2["prodotto"]))
setdiff(unique(unlist(ver[which(ver["SHIPPER"] == "0001713124-ENEL TRADE S.P.A"),"CODICE_PRODOTTO"])), unique(unlist(f2["prodotto"])))

TP_fissi <- rep(0,24)
consumo_fissi <- rep(0, 24)
not_found <- c()
for(i in 1:nrow(f2))
{
  prod <- as.character(f2[i,"prodotto"])
  di <- as.character(f2[i,"data inizio"])
  df <- take_minimum_date(as.character(f2[i,"data fine"]),"31/12/2017")
  prof <- as.character(f2[i,"profilo"])
  cons <- as.numeric(f2[i,"consumo"])
  
  ipd <- which(unlist(TP["prodotto"]) %in% prod)
  idi <- which(unlist(TP["data.inizio"]) %in% di)
  idf <- which(unlist(TP["data.fine"]) %in% df)
  ipf <- which(unlist(TP["profilo"]) %in% prof)
  
  #index <- intersect(intersect(ipd, idi), intersect(idf,ipf) )
  index <- Reduce(intersect, list(ipd,idi,idf,ipf))
  
  
  if(prod == "LGR-MF-DIRI")
  {
    index <- which(TP["prodotto"] == prod)
  }
  
  else if(prod == "BGA_MF_1403")
  {
    next
  }
  
  if(length(index) > 1)
  {
    #ico <- which(TP[index,5] == cons)
    index <- index[1]
    not_found <- c(not_found,i)
  }
  else if(length(index) == 0)
  {
    print(paste("ERROR: ", i,"-", prod, "CORRESPONDENCE NOT FOUND"))
    not_found <- c(not_found, i)
  }
  print(paste("i:",i))
  print(index)
  #print(paste("TP at index:", sum(as.numeric(TP[index,14:37]))))
  col <- which(colnames(pm) == prof)
  
  c2y <- c(as.numeric(pm[,col]),as.numeric(pm[,col])) * as.numeric(TP[index,14:37] > 0)
  
  consumo_fissi <- consumo_fissi + (c2y * cons)
  print(paste("cumulative TP:", TP_fissi))
  TP_fissi <- TP_fissi + (as.numeric(TP[index,14:37]) * (c2y * cons))
  
}

TPF <- TP_fissi/consumo_fissi
TPF*consumo_fissi/100
xlsx::write.xlsx(data.frame(rbind(TP_fissi/100, TPF)), "Transfer_Prices.xlsx", row.names = FALSE, col.names = FALSE)

mancanti <- read.table("pm.txt")
presenti <- unique(unlist(TP[which(unlist(TP["prodotto"]) %in% unlist(f2["prodotto"])),"prodotto"]))



tpf <- compute_TP(TP[which(unlist(TP["prodotto"]) %in% unlist(f2["prodotto"])),],pm)
tot_fap <- TOT_m3(TP[which(unlist(TP["prodotto"]) %in% unlist(f2["prodotto"])),],pm)

open_position_fap <- tot_fap - (mstok_prog + c(as.numeric(tot_termine[,1]), c(0,0,0,0,0,0,0,0,0)))

perc_scopertura <- open_position_fap/tot_fap

totg <- TOT_m3g(TP[which(unlist(TP["prodotto"]) %in% unlist(f2["prodotto"])),],prof_consumo)
#TPg <- compute_TPg(TP, prof_consumo)
x16 <- c(31,29,31,30,31,30,31,31,30,31,30,31)
x17 <- c(31,28,31,30,31,30,31,31,30,31,30,31)

pg <- c(661508.7/31, 631508.7/29,571508.7/31,(138330.7)*6/183,(695433.1*3)/92,(340157.5)*3/90 )


stokm <- rep(mstok_prog/c(x16,x17), c(x16,x17))
term <- c(rep(pg[1], 31),rep(pg[2], 29),rep(pg[3], 31), rep(pg[4], 183), rep(pg[5], 92), rep(pg[6], 90), rep(0, 275))
  

ST <- stokm + term
op_fap_g <- totg - (stokm + term)
plot(1:731, totg, type="l", lwd = 2, xlab="tempo", ylab="fabbisogno",main="fabbisogno, mercato a termine e stoccaggio")
abline(h=0, col = "red", lwd=2)
rect(0, 0, 31,pg[1]+(mstok_prog[1]/31), col = "grey")
rect(31, 0, 60,pg[2]+(mstok_prog[2]/29), col = "grey")
rect(60, 0, 91,pg[3]+(mstok_prog[3]/31), col = "grey")
rect(91, 0, 274,pg[4], col = "grey")
rect(274, 0, 366,pg[5], col = "grey")
rect(366, 0, 456,pg[6], col = "grey")
lines(1:731, totg, type="l", lwd = 2)

plot(1:731, op_fap_g, type="l", lwd = 2, xlab="tempo", ylab="open position",main="open position")

uutp <- unique(unlist(TP[which(unlist(TP["prodotto"]) %in% unlist(f2["prodotto"])),"prodotto"]))
xlsx::write.xlsx(data.frame(op_fap_g), "open position.xlsx", row.names = FALSE, col.names = FALSE)

count <- 0
for(i in 1:nrow(f2))
{
  if(compare_dates("01/07/2016", as.character(f2[i,"data inizio"])) & compare_dates(as.character(f2[i,"data fine"]), "01/07/2016")) count <- count + 1
}

length(which(f2["data inizio"] == "01/01/2016"))
dim(f2)
