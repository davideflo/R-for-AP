#########################################################################################
######################## DEPRECATED FUNCTIONS FOR OP.R ##################################
#########################################################################################

compute_combinations2 <- function(attivi)
{
  aggregati <- data_frame()
  prodotti <- unique(unlist(attivi["CODICE_PRODOTTO"]))
  tot_consumo <- sum(attivi["CONSUMO_CONTR_ANNUO"], na.rm = TRUE)
  check <- 0
  for(prod in prodotti)
  {
    print(prod)
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
              new_prod_name <- change_name(prod)
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
              if(!is.na(sf1)) {check <- check + sf1}
            }
            else
            {
              sf <- sf + as.numeric(as.character(data4[j,13]))
              fisso <- data.frame(cbind(prod, di, df, p, sf))
              #print(fisso)
              colnames(fisso) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
              aggregati2 <- bind_rows(aggregati2, fisso)
              colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
              if(!is.na(sf)) {check <- check + sf}
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
  print(paste("il consumo totale e' rispettato:", check == tot_consumo))
  return(aggregati)
}

