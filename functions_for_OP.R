library(openxlsx)
library(dplyr)
library(reshape)
library(stringi)
library(xlsx)

fattore_conversione <- 1.057275

extract_relevant <- function(ao)
{
  customers <- unique(unlist(ao["CLIENTE_RAGIONE_SOCIALE"]))
  rows <- c()
  for(cust in customers)
  {
    rows2 <- which(ao["CLIENTE_RAGIONE_SOCIALE"] == cust)
    #datac <- ao[rows2,]
    pdrc <- unique(unlist(ao[rows2,"PDR"]))
    for(pdr in pdrc)
    {
      if(!is.na(pdr))
      {
        rows3 <- which(ao["PDR"] == pdr)
        date_fine <- unique(unlist(ao[rows3, "DATA_FIN_DEC_PROD_T"]))
        data2 <- ao[rows3,]
        print(paste(pdr, max(rows3)))
        for(df in date_fine)
        {
          rows4 <- which(data2[,"DATA_FIN_DEC_PROD_T"] == df)
          rows <- c(rows, max(rows3[rows4]))
        }
        
      }
      else
      {
        rows <- c(rows, max(rows2))
      }
    }
  }
  rows <- unique(rows)
  return(ao[rows,])
}
###########################################################################################
extract_relevant_val <- function(ao)
{
  customers <- unique(unlist(ao["CLIENTE_RAGIONE_SOCIALE"]))
  rows <- c()
  for(cust in customers)
  {
    rows2 <- which(ao["CLIENTE_RAGIONE_SOCIALE"] == cust)
    #datac <- ao[rows2,]
    pdrc <- unique(unlist(ao[rows2,"PDR"]))
    for(pdr in pdrc)
    {
      if(!is.na(pdr))
      {
        rows3 <- which(ao["PDR"] == pdr)
        date_fine <- unique(unlist(ao[rows3, "D_VALIDO_AL_T"]))
        data2 <- ao[rows3,]
        print(paste(pdr, max(rows3)))
        for(df in date_fine)
        {
          rows4 <- which(data2[,"D_VALIDO_AL_T"] == df)
          rows <- c(rows, max(rows3[rows4]))
        }
        
      }
      else
      {
        rows <- c(rows, max(rows2))
      }
    }
  }
  rows <- unique(rows)
  return(ao[rows,])
}

###########################################################################################
month13_in_interval <- function(string_date)
{
  print(string_date)
  bool <- FALSE
  splitted_date <- as.numeric(unlist(strsplit(as.character(string_date), "/")))
  
  day <- splitted_date[1]
  month <- splitted_date[2]
  year <- splitted_date[3]
  
  fm <- ((month + 12) %% 13) + 1
  fy <- year + 1
  
  new_date <- ""
  
  if((fm <= 12) & (fy %in% c(2017, 2018))) 
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

anni_competenza <- function(attivi, date)
{
  idlist <- c()
  for(i in 1:nrow(attivi))
  {
    if(compare_dates(attivi[i,"D_VALIDO_AL_T"],date)) idlist <- c(idlist,i)
  }
  return(attivi[idlist,])
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

change_F_to_V2 <- function(row, tab)
{
  bool <- FALSE
  prima_f <- tab[row,"D_VALIDO_DAL_T"]
  m13 <- month13_in_interval(prima_f)
  
  prima_v <- 0
  ultima_f <- 0
  
 if(m13[[1]])
 {
   ultima_f <- day_before(m13[[2]])
   prima_v <- m13[[2]]
   bool <- TRUE
 }
 return(list(bool, prima_f, ultima_f, prima_v)) 
}

isolate_fixes <- function(ao)
{
  fix <- data_frame()
  unfixlist <- c()
  # check:
  ao <- anni_competenza(ao, "01/01/2017")
  ao <- extract_relevant_val(ao)
  for(i in 1:nrow(ao))
  {
    prod <- ao[i,"CODICE_PRODOTTO"]
    df <- ao[i,"D_VALIDO_AL_T"]
    let1 <- stri_sub(prod, 1, 1)
    let6 <- stri_sub(prod, 6, 6)
    cfv <- change_F_to_V2(i, ao)  
    if(cfv[[1]] & let1 == "L" & let6 == "F")
    {
      min_fin <- take_minimum_date("31/12/2018", df)
      new_fix <- ao[i,]
      new_fix["CODICE_PRODOTTO"] <- change_name(prod)
      new_fix["D_VALIDO_DAL_T"] <- cfv[[4]]
      new_fix["D_VALIDO_AL_T"] <- min_fin
      
      data_fin_plus_one <- month13_in_interval(cfv[[4]])
      plnp <- stri_sub(new_fix["CODICE_PRODOTTO"], 1, 1)
      
      if(data_fin_plus_one[[1]] & plnp != "R")
      {
        cn <- day_before(data_fin_plus_one[[2]])
        new_fix["D_VALIDO_AL_T"] <- cn
      }
      
      colnames(new_fix) <- colnames(ao)
      fix <- bind_rows(fix, data.frame(new_fix))
    
    }
    else
    {
      unfixlist <- c(unfixlist, i)
    }
  }
  #AO <- data.frame(ao[-unfixlist,])
  AO <- data.frame(ao)
  AO <- bind_rows(AO, fix)
  nf <- c()
  for(i in 1:nrow(AO))
  {
    name <- AO[i,"CODICE_PRODOTTO"]
    lorr <- stri_sub(name, 1, 1)
    forv <- stri_sub(name, 6, 6)
    if(forv == "F" & lorr != "R")
    {
      nf <- c(nf,i)
    }
  }
  return(AO[nf,])
}

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
######################################################################################################
compute_profiles_DEF <- function(M, prof_consumo)
{
  products <- unique(unlist(M["prodotto"]))
  res_per_prodotto <- matrix(0, nrow = length(products), ncol = length(prof_consumo[,1]))
  
  for(j in 1:length(products))
  {
    print(products[j])
    rows <- which(M["prodotto"] == as.character(products[j]))
    x <- M[rows,]
    mat_per_prodotto <- matrix(0, nrow = nrow(prof_consumo), ncol = length(rows))
    for(i in 1:nrow(x))
    {
      print(i)
      y <- x[i,]
      data_in_y <- unlist(y[2])
      data_fin_y <- unlist(y[3])
      prof_y <- unlist(y[4])
      cons_y <- as.numeric(as.character(unlist(y[5])))
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
    colnames(res_per_prodotto) <-prof_consumo[,1]
  }
  return(res_per_prodotto)
}
###################################################################################################
compute_profiles_DEF15 <- function(M, prof_consumo)
{
  products <- unique(unlist(M["prodotto"]))
  res_per_prodotto <- matrix(0, nrow = length(products), ncol = length(prof_consumo[,1]))
  
  for(j in 1:length(products))
  {
    print(products[j])
    rows <- which(M["prodotto"] == as.character(products[j]))
    x <- M[rows,]
    mat_per_prodotto <- matrix(0, nrow = nrow(prof_consumo), ncol = length(rows))
    for(i in 1:nrow(x))
    {
      print(i)
      y <- x[i,]
      data_in_y <- unlist(y[2])
      data_fin_y <- unlist(y[3])
      prof_y <- unlist(y[4])
      cons_y <- as.numeric(as.character(unlist(y[5])))
      COL <- which(names(prof_consumo) == prof_y)
      if( compare_dates("01/01/2015", data_in_y)  & !compare_dates("31/12/2016", data_fin_y)) ## data inizio prima di genaio 2015 e data fine dopo dicembre 2016
      {
        mat_per_prodotto[,i] <- cons_y * prof_consumo[,COL]
      }
      else if(!compare_dates("01/01/2015", data_in_y)  & !compare_dates("31/12/2016", data_fin_y)) ## data inizio dopo genaio 2015 e data fine dopo dicembre 2016
      {
        start <- which(prof_consumo[,1] == data_in_y)
        if(length(start) > 0)
        {
          res <- c(rep(0, start-1), cons_y * prof_consumo[(start):nrow(prof_consumo),COL])
          mat_per_prodotto[,i] <- res
        }
      }
      else if(compare_dates("01/01/2015", data_in_y)  & compare_dates("31/12/2016", data_fin_y)) ## data inizio prima di genaio 2015 e data fine prima di dicembre 2016
      {
        end <- which(prof_consumo[,1] == data_fin_y)
        if(length(end) > 0)
        {
          len <- nrow(prof_consumo) - end
          res <- c( cons_y * prof_consumo[1:end,COL] , rep(0, len))
          mat_per_prodotto[,i] <- res
        }
      }
      else ## ## data inizio dopo genaio 2015 e data fine prima di dicembre 2016
      {
        start <- which(prof_consumo[,1] == data_in_y)
        end <- which(prof_consumo[,1] == data_fin_y)
        if(length(start) > 0 & length(end) > 0)
        {
          len <- nrow(prof_consumo) - end
          res <- c(rep(0, start-1), cons_y * prof_consumo[(start):end,COL], rep(0, len))
          mat_per_prodotto[,i] <- res
        }
      }
    }
    somma_profili <- apply(mat_per_prodotto, 1,sum)
    
    res_per_prodotto[j,] <- somma_profili/100
    colnames(res_per_prodotto) <-prof_consumo[,1]
  }
  return(res_per_prodotto)
}

######################################################################################################

change_name <- function(name)
{
  split <- c()
  if(name == "LGR_MF_DIRI") {return(name)}
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

Change_Name <- function(prod, cfv2, cfv3, df, p, sf, sv, cfv4)
{
  cn <- data_frame()
  
  new_prod_name <- change_name(prod)
  tkm <- take_maximum_date("01/01/2017",cfv2)
  min_fin <- take_minimum_date("31/12/2018", df)
  if(as.numeric(split_date(min_fin))[1] == 29 & as.numeric(split_date(min_fin))[2] == 2 & as.numeric(split_date(min_fin))[3] %% 4 != 0)
  {
    min_fin <- paste0("28/02/",split_date(min_fin)[3])
  }
  #print(tkm)
  fisso <- data.frame(cbind(prod, tkm, cfv3, as.character(p), as.character(as.numeric(sf))))
  #print(fisso)
  colnames(fisso) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
  if(as.numeric(split_date(cfv4))[1] == 29 & as.numeric(split_date(cfv4))[2] == 2 & as.numeric(split_date(cfv4))[3] %% 4 != 0)
  {
    cfv4 <- paste0("28/02/",split_date(cfv4)[3])
  }
  variabile <- data.frame(cbind(new_prod_name, cfv4, min_fin, as.character(p), as.character(as.numeric(sv))))
  #print(variabile)
  colnames(variabile) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
  
  cn <- bind_rows(cn,fisso)
  cn <- bind_rows(cn,variabile)
  
  data_fin_plus_one <- month13_in_interval(cfv4)
  plnp <- stri_sub(new_prod_name, 1, 1)
  
  if(data_fin_plus_one[[1]] & plnp != "R")
  {
    cn[2,3] <- day_before(data_fin_plus_one[[2]])
    if(as.numeric(split_date(cn[2,3]))[1] == 29 & as.numeric(split_date(cn[2,3]))[2] == 2 & as.numeric(split_date(cn[2,3]))[3] %% 4 != 0)
    {
      cn[2,3] <- paste0("28/02/",split_date(cn[2,3])[3])
    }
    nnp <- paste0("R", stri_sub(new_prod_name, 2, nchar(new_prod_name)))
    if(as.numeric(split_date(data_fin_plus_one[[2]]))[1] == 29 & as.numeric(split_date(data_fin_plus_one[[2]]))[2] == 2 & as.numeric(split_date(data_fin_plus_one[[2]]))[3] %% 4 != 0)
    {
      data_fin_plus_one[[2]] <- paste0("28/02/",split_date(data_fin_plus_one[[2]])[3])
    }
    new_prod_line <- data.frame(cbind(nnp, data_fin_plus_one[[2]], min_fin, as.character(p), as.character(as.numeric(sv))))
    colnames(new_prod_line) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
    cn <- bind_rows(cn, new_prod_line)
  }
  return(cn)
}

compute_combinations2 <- function(attivi)
{
  aggregati <- data_frame()
  attivi2 <- anni_competenza(attivi, "01/01/2016")
  prodotti <- unique(unlist(attivi["CODICE_PRODOTTO"]))
  prodotti <- prodotti[-which(prodotti %in% c("SUPERI_E_QFISSA","P_FISSO_DIR","P_FISSO_IND"))]
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
          print(data4)
          j <- 1
          #for(j in 1:length(rows4))
          #{
          print(paste("rows4:", length(rows4)))
            first_letter <- stri_sub(prod, 1, 1)
            FV <- stri_sub(prod, 6, 6)
            sf <- sum(as.numeric(data4[1:length(rows4),13]))
            sv <- sum(as.numeric(data4[1:length(rows4),13]))
            print(paste("sf:", sf))  
            cfv <- change_F_to_V(j, data4)
            if(cfv[[1]] == "TRUE" & first_letter == "L" & FV == "F" & compare_dates(df,"31/12/2017"))
            {
              
              # new_prod_name <- change_name(prod)
              # tkm <- take_maximum_date("01/01/2016",cfv[[2]])
              # min_fin <- take_minimum_date("31/12/2017", df)
              # #print(tkm)
              # fisso <- data.frame(cbind(prod, tkm, cfv[[3]], p, sf))
              # print(fisso)
              # colnames(fisso) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
              # variabile <- data.frame(cbind(new_prod_name, cfv[[4]], min_fin, p, sv))
              # print(variabile)
              # colnames(variabile) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
              # aggregati2 <- bind_rows(aggregati2, fisso)
              # aggregati2 <- bind_rows(aggregati2, variabile)
              CN <- Change_Name(prod, cfv[[2]], cfv[[3]], df, p, sf, sv, cfv[[4]])
              aggregati2 <- bind_rows(aggregati2, CN)
              colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
              if(!is.na(sf)) {check <- check + sf}
            }
            else
            {
              tkm <- take_maximum_date("01/01/2016",di)
              ## se non tkm metti di
              min_fin <- take_minimum_date("31/12/2017", df)
              fisso <- data.frame(cbind(prod, tkm, min_fin, p, sf))
              #print(fisso)
              colnames(fisso) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
              aggregati2 <- bind_rows(aggregati2, fisso)
              colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
              if(!is.na(sf)) {check <- check + sf}
            }
            print(paste(prod, ": check", check, "sf", sf))
          #}
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
  print(paste("consumo totale:", tot_consumo))
  print(paste("consumo combinazioni:", check))
  print(paste("il consumo totale e' rispettato:", check == tot_consumo))
  return(aggregati)
}
##################################################################################################################
compute_combinations_DEF <- function(attivi)
{
  aggregati <- data_frame()
  attivi2 <- anni_competenza(attivi, "01/01/2016")
  prodotti <- unique(unlist(attivi["CODICE_PRODOTTO"]))
  prodotti <- prodotti[-which(prodotti %in% c("SUPERI_E_QFISSA","P_FISSO_DIR","P_FISSO_IND"))]
  no_distr <- union(which(is.na(attivi["CONSUMO_DISTRIBUTORE"])),which(attivi["CONSUMO_DISTRIBUTORE"] == "0"))
  tot_consumo <- sum(attivi[no_distr,"CONSUMO_CONTR_ANNUO"], na.rm = TRUE) + sum(as.numeric(attivi[-no_distr,"CONSUMO_DISTRIBUTORE"]), na.rm = TRUE)
  check <- 0
  for(prod in prodotti)
  {
    print(prod)
    aggregati2 <- data_frame()
    rows <- which(attivi["CODICE_PRODOTTO"] == prod)
    tabella2 <- attivi[rows,]
    
    data_inizio <- unique(unlist(tabella2["DATA_INI_DEC_PROD_T"]))
    for(di in data_inizio)
    {
      rows2 <- which(tabella2["DATA_INI_DEC_PROD_T"] == di)
      data2 <- tabella2[rows2,]
      data_fine <- unique(unlist(data2["DATA_FIN_DEC_PROD_T"]))
      for(df in data_fine)
      {
        rows3 <- which(data2["DATA_FIN_DEC_PROD_T"] == df)
        data3 <- data2[rows3,]
        prof <- unique(unlist(data3["PROFILO_PRELIEVO"]))
        sf  <- sv <- 0
        for(p in prof)
        {
          rows4 <- which(data3["PROFILO_PRELIEVO"] == p)
          data4 <- data3[rows4,]
          j <- 1
          
          nas <- which(is.na(data4["CONSUMO_DISTRIBUTORE"]))
          zeri <- which(data4["CONSUMO_DISTRIBUTORE"] == "0")
          rest <- setdiff(1:length(rows4), union(nas, zeri))
          
          #print(paste("rows4:", length(rows4)))
          first_letter <- stri_sub(prod, 1, 1)
          FV <- stri_sub(prod, 6, 6)
          sf <- sum(as.numeric(data4[rest,"CONSUMO_DISTRIBUTORE"]), na.rm = TRUE) + sum(as.numeric(data4[union(nas,zeri),"CONSUMO_CONTR_ANNUO"]), na.rm = TRUE)
          sv <- sum(as.numeric(data4[rest,"CONSUMO_DISTRIBUTORE"]), na.rm = TRUE) + sum(as.numeric(data4[union(nas,zeri),"CONSUMO_CONTR_ANNUO"]), na.rm = TRUE)
          #print(paste("sf:", sf))  
          cfv <- change_F_to_V2(j, data4)
          print(cfv[[1]] & first_letter == "L" & FV == "F" & compare_dates(df,"31/12/2017"))
          if(cfv[[1]] & first_letter == "L" & FV == "F" & compare_dates(df,"31/12/2017") & prod != "LGR_MF_DIRI")
          {
            CN <- Change_Name(prod, cfv[[2]], cfv[[3]], df, p, sf, sv, cfv[[4]])
            print(CN)
            aggregati2 <- bind_rows(aggregati2, CN)
            colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
            if(!is.na(sf)) {check <- check + sf}
          }
          else if (prod == "LGR_MF_DIRI")
          {
            fisso <- data.frame(cbind(prod, "01/01/2016", "31/12/2017", as.character(p), as.character(as.numeric(sf))))
            colnames(fisso) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
            aggregati2 <- bind_rows(aggregati2, fisso)
            colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
            if(!is.na(sf)) {check <- check + sf}
          }
          else
          {
            tkm <- take_maximum_date("01/01/2016",di)
            ## se non tkm metti di
            min_fin <- take_minimum_date("31/12/2017", df)
            fisso <- data.frame(cbind(prod, tkm, min_fin, as.character(p), as.character(as.numeric(sf))))
            
            colnames(fisso) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
            aggregati2 <- bind_rows(aggregati2, fisso)
            colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
            if(!is.na(sf)) {check <- check + sf}
          }
          #print(paste(prod, ": check", check, "sf", sf))
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
  print(paste("consumo totale:", tot_consumo))
  print(paste("consumo combinazioni:", check))
  print(paste("il consumo totale e' rispettato:", (as.numeric(check) - as.numeric(tot_consumo)) == 0))
  return(aggregati)
}
###############################################################################################################################
compute_combinations_DEF_val <- function(attivi)
{
  aggregati <- data_frame()
  attivi2 <- anni_competenza(attivi, "01/01/2017")
  prodotti <- unique(unlist(attivi2["CODICE_PRODOTTO"]))
  #prodotti <- prodotti[-which(prodotti %in% c("SUPERI_E_QFISSA","P_FISSO_DIR","P_FISSO_IND"))]
  no_distr <- union(which(is.na(attivi2["CONSUMO_DISTRIBUTORE"])),which(attivi2["CONSUMO_DISTRIBUTORE"] == "0"))
  tot_consumo <- sum(as.numeric(attivi2[no_distr,"CONSUMO_CONTR_ANNUO"], na.rm = TRUE)) + sum(as.numeric(attivi2[-no_distr,"CONSUMO_DISTRIBUTORE"]), na.rm = TRUE)
  check <- 0
  for(prod in prodotti)
  {
    print(prod)
    aggregati2 <- data_frame()
    rows <- which(attivi2["CODICE_PRODOTTO"] == prod)
    tabella2 <- attivi2[rows,]
    
    data_inizio <- unique(unlist(tabella2["D_VALIDO_DAL_T"]))
    for(di in data_inizio)
    {
      rows2 <- which(tabella2["D_VALIDO_DAL_T"] == di)
      data2 <- tabella2[rows2,]
      data_fine <- unique(unlist(data2["D_VALIDO_AL_T"]))
      for(df in data_fine)
      {
        rows3 <- which(data2["D_VALIDO_AL_T"] == df)
        data3 <- data2[rows3,]
        prof <- unique(unlist(data3["PROFILO_PRELIEVO"]))
        sf  <- sv <- 0
        for(p in prof)
        {
          rows4 <- which(data3["PROFILO_PRELIEVO"] == p)
          data4 <- data3[rows4,]
          j <- 1
          
          nas <- which(is.na(data4["CONSUMO_DISTRIBUTORE"]))
          zeri <- which(data4["CONSUMO_DISTRIBUTORE"] == "0")
          rest <- setdiff(1:length(rows4), union(nas, zeri))
          
          #print(paste("rows4:", length(rows4)))
          first_letter <- stri_sub(prod, 1, 1)
          FV <- stri_sub(prod, 6, 6)
          sf <- sum(as.numeric(data4[rest,"CONSUMO_DISTRIBUTORE"]), na.rm = TRUE) + sum(as.numeric(data4[union(nas,zeri),"CONSUMO_CONTR_ANNUO"]), na.rm = TRUE)
          sv <- sum(as.numeric(data4[rest,"CONSUMO_DISTRIBUTORE"]), na.rm = TRUE) + sum(as.numeric(data4[union(nas,zeri),"CONSUMO_CONTR_ANNUO"]), na.rm = TRUE)
          #print(paste("sf:", sf))  
          cfv <- change_F_to_V2(j, data4)
          print(cfv[[1]] & first_letter == "L" & FV == "F" & compare_dates(df,"31/12/2018"))
          if(cfv[[1]] & first_letter == "L" & FV == "F" & compare_dates(df,"31/12/2018"))
          {
            CN <- Change_Name(prod, cfv[[2]], cfv[[3]], df, p, sf, sv, cfv[[4]])
            print(CN)
            if(compare_dates(cfv[[3]],"01/01/2017"))
            {
              aggregati2 <- bind_rows(aggregati2, CN)
            }
            else
            {
              aggregati2 <- bind_rows(aggregati2, CN[2,])
            }
#            aggregati2 <- bind_rows(aggregati2, CN)
            colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
            if(!is.na(sf)) {check <- check + sf}
          }
          else
          {
            tkm <- take_maximum_date("01/01/2017",di)
            ## se non tkm metti di
            min_fin <- take_minimum_date("31/12/2018", df)
            fisso <- data.frame(cbind(prod, tkm, min_fin, as.character(p), as.character(as.numeric(sf))))
            
            colnames(fisso) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
            aggregati2 <- bind_rows(aggregati2, fisso)
            colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo")
            if(!is.na(sf)) {check <- check + sf}
          }
          #print(paste(prod, ": check", check, "sf", sf))
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
  print(paste("consumo totale:", tot_consumo))
  print(paste("consumo combinazioni:", check))
  print(paste("il consumo totale e' rispettato:", (as.numeric(check) - as.numeric(tot_consumo)) == 0))
  return(aggregati)
}

###############################################################################################################################
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
  #### res ha gia SOLO le colonne con le date
  index_list <- c()
  for(i in 1:length(colnames(res)))
  {
    name <- colnames(res)[i]
    sdt <- split_date(name)
    if(sdt[2] == month & sdt[3] == year) index_list <- c(index_list, i)
  }
  return(index_list)
}

take_monthV <- function(month, year, res)
{
  #### res ha gia SOLO le colonne con le date
  index_list <- c()
  for(i in 1:length(names(res)))
  {
    name <- names(res)[i]
    sdt <- split_date(name)
    if(sdt[2] == month & sdt[3] == year) index_list <- c(index_list, i)
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
    if(as.numeric(sdt[2]) == month & as.numeric(sdt[3]) == year) index_list <- c(index_list, i)
  }
  return(index_list)
}

sum_by_month <- function(month, year, res)
{
  
  ii <- take_month(month, year, res)
  sum_month <- sum(res[,ii])
  
  return(sum_month)
}

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
        #print(j)
        nm <- compute_month(j)
        prof_acquisti[i,j] <- as.numeric(prelievi[i,nm]) * as.numeric(acquisti[i])
      }
    }
  }
  return(prof_acquisti)
}

compute_profile_fixed_APS <- function(ver)
{
  result <- rep(0,731)
  dates <- c(paste0("01/","0",1:9,"/",2016), paste0("01/",10:12,"/",2016), paste0("01/","0",1:9,"/",2017), paste0("01/",10:12,"/",2017)) 
  for(dt in dates)
  {
    dts <- split_date(dt)
    fixes <- which(ver[dt] == "F")
    month <- take_month(dts[2], dts[3], ver)
    result[month] <- colSums(ver[fixes,month], na.rm = TRUE)
  }
  #ver[,14:37]
  res <- data.frame(result)
  colnames(res) <- colnames(ver)[38:768]
  return(res)
}

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
##################################################################################################
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
#######################################################################################################################

TOT_m3 <- function(prod, pm)
{
  tot_m3 <- rep(0, 24)
  #totm <- matrix(0,nrow=nrow(prod), ncol=24)
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
        #(pm[,col] * nzp[j,"consumo"])
        tot_m3[(i-13)] <- tot_m3[(i-13)] + (pm[as.numeric(ds[2]),col] * as.numeric(nzp[j,"consumo"]))
      }
    } 
  }
  
  return(tot_m3)
}

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

find_TP_at_date <- function(prod, TP, M, date)
{
  mat <- matrix(0, nrow = length(prod), ncol=3)
  for(i in 1:length(prod))
  {
    print(i)
    print(prod[i])
    ri <- which(unlist(TP["prodotto"]) %in% prod[i])
    rinz <- which(TP[ri,date] != 0)
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

sum_in_year <- function(op, year)
{
  dn <- 1:12
  dlist <- c()
  for(d in dn)
  {
    dlist <- c(dlist, sum(as.numeric(op[take_month2(d, year, op)])))
  }
  return(dlist)
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

TOT_m3mat <- function(prod, pm)
{
  
  totm <- matrix(0,nrow=nrow(prod), ncol=24)
  for(i in 1:nrow(prod))
  {
    col <- which(colnames(pm) == prod[i,"profilo"])
    c2y <- as.numeric(prod[i,14:37] > 0)
    totm[i,] <- as.numeric(pm[,col]) * as.numeric(prod[i,"consumo"]) * c2y
  }
  return(totm)
}
#############################################################################################
compute_combinations_DEF_val_Agenti <- function(attivi)
{
  aggregati <- data.frame()
  attivi[is.na(attivi)] <- 0
  attivi2 <- anni_competenza(attivi, "01/01/2017")
  prodotti <- as.character(unique(unlist(attivi2["CODICE_PRODOTTO"])))
  #prodotti <- prodotti[-which(prodotti %in% c("SUPERI_E_QFISSA","P_FISSO_DIR","P_FISSO_IND"))]
  no_distr <- union(which(is.na(attivi2["CONSUMO_DISTRIBUTORE"])),which(attivi2["CONSUMO_DISTRIBUTORE"] == "0"))
  tot_consumo <- sum(as.numeric(attivi2[no_distr,"CONSUMO_CONTR_ANNUO"], na.rm = TRUE)) + sum(as.numeric(attivi2[-no_distr,"CONSUMO_DISTRIBUTORE"]), na.rm = TRUE)
  check <- 0
  for(prod in prodotti)
  {
    print(prod)
    aggregati2 <- data_frame()
    rows <- which(attivi2["CODICE_PRODOTTO"] == prod)
    Tabella2 <- attivi2[rows,]
    
    agentiloc <- as.character(unique(unlist(Tabella2["AGENZIA"])))
    
    for(al in agentiloc)
    {
      print(al)
      righe <- which(Tabella2["AGENZIA"] == al)
      tabella2 <- Tabella2[righe,]    
    
      data_inizio <- unique(unlist(tabella2["D_VALIDO_DAL_T"]))
      for(di in data_inizio)
      {
        print(di)
        rows2 <- which(tabella2["D_VALIDO_DAL_T"] == di)
        data2 <- tabella2[rows2,]
        data_fine <- unique(unlist(data2["D_VALIDO_AL_T"]))
        for(df in data_fine)
        {
          print(df)
          rows3 <- which(data2["D_VALIDO_AL_T"] == df)
          data3 <- data2[rows3,]
          prof <- unique(unlist(data3["PROFILO_PRELIEVO"]))
          sf  <- sv <- 0
          for(p in prof)
          {
            print(p)
            rows4 <- which(data3["PROFILO_PRELIEVO"] == p)
            data4 <- data3[rows4,]
            j <- 1
            
            nas <- which(is.na(data4["CONSUMO_DISTRIBUTORE"]))
            zeri <- which(data4["CONSUMO_DISTRIBUTORE"] == "0")
            rest <- setdiff(1:length(rows4), union(nas, zeri))
            
            #print(paste("rows4:", length(rows4)))
            first_letter <- stri_sub(prod, 1, 1)
            FV <- stri_sub(prod, 6, 6)
            sf <- sum(as.numeric(data4[rest,"CONSUMO_DISTRIBUTORE"]), na.rm = TRUE) + sum(as.numeric(data4[union(nas,zeri),"CONSUMO_CONTR_ANNUO"]), na.rm = TRUE)
            sv <- sum(as.numeric(data4[rest,"CONSUMO_DISTRIBUTORE"]), na.rm = TRUE) + sum(as.numeric(data4[union(nas,zeri),"CONSUMO_CONTR_ANNUO"]), na.rm = TRUE)
            #print(paste("sf:", sf))  
            cfv <- change_F_to_V2(j, data4)
            print(cfv[[1]] & first_letter == "L" & FV == "F" & compare_dates(df,"31/12/2018"))
            if(cfv[[1]] & first_letter == "L" & FV == "F" & compare_dates(df,"31/12/2018"))
            {
              CN <- Change_Name(prod, cfv[[2]], cfv[[3]], df, p, sf, sv, cfv[[4]])
              if(compare_dates(cfv[[3]],"01/01/2017"))
              {
                CN2 <- data.frame(CN, al)
              }
              else
              {
                CN2 <- data.frame(CN[2,],al)
              }
              colnames(CN2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo", "agenzia")
              print(CN2)
              #aggregati2 <- bind_rows(aggregati2, CN2)
              aggregati2 <- rbind(aggregati2, CN2)
              colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo", "agenzia")
              if(!is.na(sf)) {check <- check + sf}
            }
            else
            {
              tkm <- take_maximum_date("01/01/2017",di)
              ## se non tkm metti di
              min_fin <- take_minimum_date("31/12/2018", df)
              fisso <- data.frame(cbind(prod, tkm, min_fin, as.character(p), as.character(as.numeric(sf)), al))
              
              colnames(fisso) <- c("prodotto","data inizio", "data fine", "profilo", "consumo", "agenzia")
              #aggregati2 <- bind_rows(aggregati2, fisso)
              aggregati2 <- rbind(aggregati2, fisso)
              colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo", "agenzia")
              if(!is.na(sf)) {check <- check + sf}
            }
            #print(paste(prod, ": check", check, "sf", sf))
          }
        }
      }
    }
    if(nrow(aggregati2) > 0) 
    {
      colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo", "agenzia")
      aggregati2$consumo <- as.numeric(as.character(aggregati2$consumo))
      #aggregati <- bind_rows(aggregati, aggregati2)
      aggregati <- rbind(aggregati, aggregati2)
    }
  }
  colnames(aggregati) <- c("prodotto","data inizio", "data fine", "profilo", "consumo", "agenzia")
  print(paste("consumo totale:", tot_consumo))
  print(paste("consumo combinazioni:", check))
  print(paste("il consumo totale e' rispettato:", (as.numeric(check) - as.numeric(tot_consumo)) == 0))
  return(aggregati)
}
#############################################################################################
compute_combinations_DEF_val_countPDR <- function(attivi)
{
  aggregati <- data.frame()
  attivi[is.na(attivi)] <- 0
  attivi2 <- anni_competenza(attivi, "01/01/2017")
  prodotti <- as.character(unique(unlist(attivi2["CODICE_PRODOTTO"])))
  #prodotti <- prodotti[-which(prodotti %in% c("SUPERI_E_QFISSA","P_FISSO_DIR","P_FISSO_IND"))]
  no_distr <- union(which(is.na(attivi2["CONSUMO_DISTRIBUTORE"])),which(attivi2["CONSUMO_DISTRIBUTORE"] == "0"))
  tot_consumo <- sum(as.numeric(attivi2[no_distr,"CONSUMO_CONTR_ANNUO"], na.rm = TRUE)) + sum(as.numeric(attivi2[-no_distr,"CONSUMO_DISTRIBUTORE"]), na.rm = TRUE)
  check <- 0
  for(prod in prodotti)
  {
    print(prod)
    aggregati2 <- data_frame()
    rows <- which(attivi2["CODICE_PRODOTTO"] == prod)
    Tabella2 <- attivi2[rows,]
    
    agentiloc <- as.character(unique(unlist(Tabella2["AGENZIA"])))
    
    for(al in agentiloc)
    {
      print(al)
      righe <- which(Tabella2["AGENZIA"] == al)
      tabella2 <- Tabella2[righe,]    
      
      data_inizio <- unique(unlist(tabella2["D_VALIDO_DAL_T"]))
      for(di in data_inizio)
      {
        print(di)
        rows2 <- which(tabella2["D_VALIDO_DAL_T"] == di)
        data2 <- tabella2[rows2,]
        data_fine <- unique(unlist(data2["D_VALIDO_AL_T"]))
        for(df in data_fine)
        {
          print(df)
          rows3 <- which(data2["D_VALIDO_AL_T"] == df)
          data3 <- data2[rows3,]
          prof <- unique(unlist(data3["PROFILO_PRELIEVO"]))
          sf  <- sv <- 0
          for(p in prof)
          {
            print(p)
            rows4 <- which(data3["PROFILO_PRELIEVO"] == p)
            data4 <- data3[rows4,]
            j <- 1
            
            pdratt <- ivaatt <- 0
            
            pdratt <- length(unique(unlist(data4["PDR"])))
            ivaatt <- length(unique(unlist(data4["PARTITA_IVA"])))
            
            print(pdratt)
            print(ivaatt)
            
            
            nas <- which(is.na(data4["CONSUMO_DISTRIBUTORE"]))
            zeri <- which(data4["CONSUMO_DISTRIBUTORE"] == "0")
            rest <- setdiff(1:length(rows4), union(nas, zeri))
            
            #print(paste("rows4:", length(rows4)))
            first_letter <- stri_sub(prod, 1, 1)
            FV <- stri_sub(prod, 6, 6)
            sf <- sum(as.numeric(data4[rest,"CONSUMO_DISTRIBUTORE"]), na.rm = TRUE) + sum(as.numeric(data4[union(nas,zeri),"CONSUMO_CONTR_ANNUO"]), na.rm = TRUE)
            sv <- sum(as.numeric(data4[rest,"CONSUMO_DISTRIBUTORE"]), na.rm = TRUE) + sum(as.numeric(data4[union(nas,zeri),"CONSUMO_CONTR_ANNUO"]), na.rm = TRUE)
            #print(paste("sf:", sf))  
            cfv <- change_F_to_V2(j, data4)
            print(cfv[[1]] & first_letter == "L" & FV == "F" & compare_dates(df,"31/12/2018"))
            if(cfv[[1]] & first_letter == "L" & FV == "F" & compare_dates(df,"31/12/2018"))
            {
              CN <- Change_Name(prod, cfv[[2]], cfv[[3]], df, p, sf, sv, cfv[[4]])
              PI <- data.frame(c(pdratt,0),c(0,pdratt),c(ivaatt,0),c(0,ivaatt))
              if(nrow(CN) == 3)
              {
                PI <- data.frame(c(pdratt,0, 0),c(0,pdratt,pdratt),c(ivaatt,0,0),c(0,ivaatt, ivaatt))
              }
              if(compare_dates(cfv[[3]],"01/01/2017"))
              {
                CN2 <- data.frame(CN, al, PI)
              }
              else
              {
                CN2 <- data.frame(CN[2,],al, PI)
              }
              colnames(CN2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo", "agenzia", "pdr attuali", "pdr prospect", "p_iva attuali", "p_iva prospect")
              print(CN2)
              #aggregati2 <- bind_rows(aggregati2, CN2)
              aggregati2 <- rbind(aggregati2, CN2)
              colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo", "agenzia", "pdr attuali", "pdr prospect", "p_iva attuali", "p_iva prospect")
              if(!is.na(sf)) {check <- check + sf}
            }
            else
            {
              tkm <- take_maximum_date("01/01/2017",di)
              ## se non tkm metti di
              min_fin <- take_minimum_date("31/12/2018", df)
              fisso <- data.frame(prod, tkm, min_fin, as.character(p), as.character(as.numeric(sf)), al, pdratt, 0, ivaatt, 0)
              
              colnames(fisso) <- c("prodotto","data inizio", "data fine", "profilo", "consumo", "agenzia", "pdr attuali", "pdr prospect", "p_iva attuali", "p_iva prospect")
              #aggregati2 <- bind_rows(aggregati2, fisso)
              aggregati2 <- rbind(aggregati2, fisso)
              colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo", "agenzia", "pdr attuali", "pdr prospect", "p_iva attuali", "p_iva prospect")
              if(!is.na(sf)) {check <- check + sf}
            }
            #print(paste(prod, ": check", check, "sf", sf))
          }
        }
      }
    }
    if(nrow(aggregati2) > 0) 
    {
      colnames(aggregati2) <- c("prodotto","data inizio", "data fine", "profilo", "consumo", "agenzia", "pdr attuali", "pdr prospect", "p_iva attuali", "p_iva prospect")
      aggregati2$consumo <- as.numeric(as.character(aggregati2$consumo))
      #aggregati <- bind_rows(aggregati, aggregati2)
      aggregati <- rbind(aggregati, aggregati2)
    }
  }
  colnames(aggregati) <- c("prodotto","data inizio", "data fine", "profilo", "consumo", "agenzia", "pdr attuali", "pdr prospect", "p_iva attuali", "p_iva prospect")
  print(paste("consumo totale:", tot_consumo))
  print(paste("consumo combinazioni:", check))
  print(paste("il consumo totale e' rispettato:", (as.numeric(check) - as.numeric(tot_consumo)) == 0))
  return(aggregati)
}
###################################################################################################################################################


