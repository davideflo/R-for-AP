###############################################
###### Utility functions for BUDGET GAS #######
###############################################

library(lubridate)
library(plyr)
library(openxlsx)
library(dplyr)
library(reshape)
library(stringi)
library(xlsx)
library(Hmisc)

###########################################################################################
read_file_anagrafica <- function(ao)
{
  ao[is.na(ao)] <- 0
  vdi <- as.Date(ao$D_VALIDO_DAL, origin = '1899-12-30')
  vdf <- as.Date(ao$D_VALIDO_AL, origin = '1899-12-30')
  ao$D_VALIDO_DAL_T <- maply(1:length(vdi), function(n) paste0(  ifelse(lubridate::day(vdi[n]) < 10, paste0("0",lubridate::day(vdi[n])), lubridate::day(vdi[n])), "/",
                                                              ifelse(lubridate::month(vdi[n]) < 10, paste0("0",lubridate::month(vdi[n])), lubridate::month(vdi[n])),"/",
                                                              lubridate::year(vdi[n])))
  ao$D_VALIDO_AL_T <- maply(1:length(vdf), function(n) paste0(  ifelse(lubridate::day(vdf[n]) < 10, paste0("0",lubridate::day(vdf[n])), lubridate::day(vdf[n])), "/",
                                                                 ifelse(lubridate::month(vdf[n]) < 10, paste0("0",lubridate::month(vdf[n])), lubridate::month(vdf[n])),"/",
                                                                 lubridate::year(vdf[n])))
  ao <- ao[,c(1:28,68,29,69,30:67)]
  return(ao)
}
###########################################################################################
month13_in_interval <- function(string_date, year)
{
  ### @BRIEF: year is the current year
  print(string_date)
  bool <- FALSE
  splitted_date <- as.numeric(unlist(strsplit(as.character(string_date), "/")))
  
  day <- splitted_date[1]
  month <- splitted_date[2]
  year <- splitted_date[3]
  
  fm <- ((month + 12) %% 13) + 1
  fy <- year + 1
  
  new_date <- ""
  
  if((fm <= 12) & (fy %in% c(year, year+1))) 
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
#######################################################################################
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
#######################################################################################
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
#######################################################################################
transform_date <- function(date)
{
  dv <- strsplit(date, "/")
  return(as.numeric(unlist(dv)))
}
#########################################################################################
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
##############################################################################################
anni_competenza <- function(attivi, date)
{
  idlist <- c()
  for(i in 1:nrow(attivi))
  {
    if(compare_dates(attivi[i,"D_VALIDO_AL_T"],date)) idlist <- c(idlist,i)
  }
  return(attivi[idlist,])
}
#############################################################################################
take_maximum_date <- function(date_pc, date)
{
  if(compare_dates(date_pc,date)) return(date_pc)
  else return(date)
}
#############################################################################################
take_minimum_date <- function(date_pc, date)
{
  if(!compare_dates(date_pc,date)) return(date_pc)
  else return(date)
}
##########################################################################################################################################################################
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
##########################################################################################################################################################################
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
##########################################################################################################################################################################
isolate_fixes <- function(ao, year)
{
  ### @BRIEF: year is the current year
  fix <- data_frame()
  unfixlist <- c()
  # check:
  ao <- anni_competenza(ao, paste0("01/01/", year))
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
      min_fin <- take_minimum_date(paste0("31/12/",year+1), df)
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
##########################################################################################################################################################################
split_date <- function(string_date)
{
  splitted_date <- as.character(unlist(strsplit(as.character(string_date), "/")))
  day <- splitted_date[1]
  month <- splitted_date[2]
  year <- splitted_date[3]
  return(c(day, month, year))
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
##########################################################################################################################################################################
Change_Name <- function(prod, cfv2, cfv3, df, p, sf, sv, cfv4, year)
{
  cn <- data_frame()
  
  new_prod_name <- change_name(prod)
  tkm <- take_maximum_date(paste0("01/01/", year),cfv2)
  min_fin <- take_minimum_date(paste0("31/12/", year+1), df)
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
##########################################################################################################################################################################
compute_combinations_DEF_val <- function(attivi, year)
{
  ### @BIREF: year is the first of the two years for the budget --> basically, the current year
  aggregati <- data_frame()
  attivi2 <- anni_competenza(attivi, paste0("01/01/", year))
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
          print(cfv[[1]] & first_letter == "L" & FV == "F" & compare_dates(df,paste0("31/12/", year+1)))
          if(cfv[[1]] & first_letter == "L" & FV == "F" & compare_dates(df,paste0("31/12/", year+1)))
          {
            CN <- Change_Name(prod, cfv[[2]], cfv[[3]], df, p, sf, sv, cfv[[4]], year)
            print(CN)
            if(compare_dates(cfv[[3]],paste0("01/01/", year)))
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
            tkm <- take_maximum_date(paste0("01/01/", year),di)
            ## se non tkm metti di
            min_fin <- take_minimum_date(paste0("31/12/", year+1), df)
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
########################################################################################################################
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
#######################################################################################################################
##################################################################################################
round_date <- function(aggregati)
{
  for(i in 1:nrow(aggregati))
  {
    print(i)
    splitted <- strsplit(aggregati$`data fine`[i],"/")
    if(IsMiddleMonth(splitted))
    {
      month <- ifelse(as.numeric(splitted[[1]][2])-1 == 0, 12, as.numeric(splitted[[1]][2])-1)
      aggregati$`data fine`[i] <- paste0(monthDays(as.Date(paste0(splitted[[1]][3],"-",month,"-01"))),"/",as.numeric(splitted[[1]][2])-1,"/",splitted[[1]][3])
    }
  }
  return(aggregati)
}
################################################################################################
AggiornaVendite <- function(filename)
{
  vendite <- openxlsx::read.xlsx(paste0("C:/Users/d_floriello/Documents/",filename), sheet = 1, colNames = TRUE, startRow = 6)
  vendite <- vendite[,c(3,6:14,18:209,4,5)]
  vdi <- as.Date(vendite$data.inizio, origin = '1899-12-30')
  vdf <- as.Date(vendite$data.fine, origin = '1899-12-30')
  
  vendite$data.inizio <- maply(1:length(vendite$data.inizio), function(n) paste0(ifelse(lubridate::day(vdi[n]) < 10, paste0("0",lubridate::day(vdi[n])), lubridate::day(vdi[n])), "/", 
                                                                                 ifelse(lubridate::month(vdi[n]) < 10, paste0("0",lubridate::month(vdi[n])), lubridate::month(vdi[n])), "/", 
                                                                                 lubridate::year(vdi[n])))
  vendite$data.fine <- maply(1:length(vendite$data.fine), function(n) paste0(ifelse(lubridate::day(vdf[n]) < 10, paste0("0",lubridate::day(vdf[n])), lubridate::day(vdf[n])), "/", 
                                                                             ifelse(lubridate::month(vdf[n]) < 10, paste0("0",lubridate::month(vdf[n])), lubridate::month(vdf[n])), "/", 
                                                                             lubridate::year(vdf[n])))
  return(vendite)
}
############################################################################################
AggiornaTP <- function(filename)
{
  vendite <- openxlsx::read.xlsx(paste0("C:/Users/d_floriello/Documents/",filename), sheet = 1, colNames = TRUE, startRow = 6)
  vendite <- vendite[,c(3,6:17,210:233,4,5)]
  vdi <- as.Date(vendite$data.inizio, origin = '1899-12-30')
  vdf <- as.Date(vendite$data.fine, origin = '1899-12-30')
  
  vendite$data.inizio <- maply(1:length(vendite$data.inizio), function(n) paste0(ifelse(lubridate::day(vdi[n]) < 10, paste0("0",lubridate::day(vdi[n])), lubridate::day(vdi[n])), "/", 
                                                                                 ifelse(lubridate::month(vdi[n]) < 10, paste0("0",lubridate::month(vdi[n])), lubridate::month(vdi[n])), "/", 
                                                                                 lubridate::year(vdi[n])))
  vendite$data.fine <- maply(1:length(vendite$data.fine), function(n) paste0(ifelse(lubridate::day(vdf[n]) < 10, paste0("0",lubridate::day(vdf[n])), lubridate::day(vdf[n])), "/", 
                                                                             ifelse(lubridate::month(vdf[n]) < 10, paste0("0",lubridate::month(vdf[n])), lubridate::month(vdf[n])), "/", 
                                                                             lubridate::year(vdf[n])))
  return(vendite)
}
############################################################################################
is_active_month <- function(date, inizio, fine)
{
  bool <- FALSE
  if(compare_dates(date, inizio) & compare_dates(fine, date)) bool <- TRUE
  return(bool)
}
###########################################################################################
find_active_in_month <- function(date, vendite)
{
  actives <- c()
  for(i in 1:nrow(vendite))
  {
    inizio <- vendite[i,2]
    fine <- vendite[i,3]
    if(is_active_month(date, inizio, fine)) actives <- c(actives, i)
  }
  return(actives)
}
############################################################################################
total_sellings_per_components <- function(vendite, pm, listing, listingG, year)
{
  ### @PARAM: year is the current year
  #total <- rep(0, nrow(vendite))
  total_matrix <- matrix(0, nrow = 10, ncol = 24)
  months <- c(paste0("01/","0",1:9,"/",year), paste0("01/",10:12,"/",year), paste0("01/","0",1:9,"/",year+1), paste0("01/",10:12,"/",year+1)) 
  
  for(m in months)
  {
    act <- find_active_in_month(m, vendite)
    
    SPGas <- 0
    Sqtmvc <- Scpr <- Sgrad <- Sccr <- Sqtint <- Sqtpsv <- Sqvdvar <- Sccvdvar <- 0
    for(i in act)
    {
      P0 <- as.numeric(vendite[i,9])
      Prif <- 0 
      tipo <- vendite[i,"TIPO"]   
      if (toupper(tipo) == "VAR") {Prif <- as.numeric(vendite[i,10])}
      
      pc <- which(colnames(pm) == vendite[i,4])
      profm <- (as.numeric(vendite[i,5]) * pm[,pc])
      
      inizio <- split_date(vendite[i,2])
      fine <- split_date(vendite[i,3])
      inizio <- paste0("01/",inizio[2],"/", inizio[3])
      fine <- paste0("01/",fine[2], "/",fine[3])
      
      dnames2016 <- colnames(vendite[i, 11:22])
      dnames2017 <- colnames(vendite[i, 107:118])
      
      qtmvc <- cpr <- grad <- ccr <- qtint <- qtpsv <- qvdvar <- ccvdvar <- rep(0, 12)
      lg <- rep(0,12)
      
      YEAR <- split_date(m)[3]
      MONTH <- as.numeric(split_date(m)[2])
      
      if(YEAR == as.character(year))
      {
        qtmvc <- as.numeric(unlist(vendite[i, 11:22]))
        cpr <- as.numeric(unlist(vendite[i, 23:34]))
        grad <- as.numeric(unlist(vendite[i, 35:46]))
        ccr <- as.numeric(unlist(vendite[i, 47:58]))
        qtint <- as.numeric(unlist(vendite[i, 59:70]))
        qtpsv <- as.numeric(unlist(vendite[i, 71:82]))
        lg <- listing[1:12]
        
        if(vendite[i,"prodotto"] == "LG1_BP_CIES") lg <- listingG[1:12]
        
        
        if(vendite[i,8] == "SI" & vendite[i,7] == "0")
        {
          ccvdvar <- as.numeric(unlist(vendite[i, 95:106]))  
        }
        
        else if(vendite[i,8] == "0" & vendite[i,7] == "SI")
        {
          qvdvar <- as.numeric(unlist(vendite[i, 83:94]))
        }
      }
      else
      {
        qtmvc <- as.numeric(unlist(vendite[i, 107:118]))
        cpr <- as.numeric(unlist(vendite[i, 119:130]))
        grad <- as.numeric(unlist(vendite[i, 131:142]))
        ccr <- as.numeric(unlist(vendite[i, 143:154]))
        qtint <- as.numeric(unlist(vendite[i, 155:166]))
        qtpsv <- as.numeric(unlist(vendite[i, 167:178]))
        lg <- listing[13:24]
        
        if(vendite[i,"prodotto"] == "LG1_BP_CIES") lg <- listingG[13:24]
        
        if(vendite[i,8] == "SI" & vendite[i,7] == "0")
        {
          ccvdvar <- as.numeric(unlist(vendite[i, 191:202]))
        }
        
        else if(vendite[i,8] == "0" & vendite[i,7] == "SI")
        {
          qvdvar <- as.numeric(unlist(vendite[i, 179:190]))
        }
      }
      
      Sqtmvc <- Sqtmvc + profm[MONTH] * qtmvc[MONTH]/100 
      Scpr <- Scpr + profm[MONTH] * cpr[MONTH]/100  
      Sgrad <- Sgrad + profm[MONTH] * grad[MONTH]/100 
      Sccr <- Sccr + profm[MONTH] * ccr[MONTH]/100 
      Sqtint <- Sqtint + profm[MONTH] * qtint[MONTH]/100 
      Sqtpsv <- Sqtpsv + profm[MONTH] * qtpsv[MONTH]/100 
      Sqvdvar <- Sqvdvar + profm[MONTH] * qvdvar[MONTH]/100 
      Sccvdvar <- Sccvdvar + profm[MONTH] * ccvdvar[MONTH]/100
      
      if(toupper(tipo) == "FIX")
      {
        SPGas <- SPGas + (P0 * profm[MONTH])/100 
      }
      else
      {
        SPGas <- SPGas + ((P0 + (lg[MONTH] - Prif))/100 * profm[MONTH])
      }
    }
    
    J <- which(months == m)
    
    total_matrix[1,J] <- SPGas
    total_matrix[2,J] <- Sqtmvc
    total_matrix[3,J] <- Scpr
    total_matrix[4,J] <- Sgrad
    total_matrix[5,J] <- Sccr
    total_matrix[6,J] <- Sqtint
    total_matrix[7,J] <- Sqtpsv
    total_matrix[8,J] <- Sqvdvar
    total_matrix[9,J] <- Sccvdvar
    total_matrix[10,J] <- sum(total_matrix[1:9,J])
    print(total_matrix[,J])
    
  }
  
  TM <- data.frame(total_matrix)
  rownames(TM) <- c("PGas", "qtmvc", "cpr", "grad", "ccr", "qtint", "qtpsv", "qvdvar", "ccvdvar", "total")
  colnames(TM) <- months
  
  return(TM)
}  
#############################################################################################################
##################################################################################################
IsMiddleMonth <- function(date)
{
  bVerbose <- 0
  if(as.numeric(date[[1]][3]) %% 4 == 0 & as.numeric(date[[1]][2]) == 2 & as.numeric(date[[1]][1]) != 29)
  {
    bVerbose <- TRUE
  }
  else if(as.numeric(date[[1]][2]) %in% c(4,6,9,11) & as.numeric(date[[1]][1]) != 30)
  {
    bVerbose <- TRUE
  }
  else if(as.numeric(date[[1]][2]) %in% c(1,3,5,7,8,10,12) & as.numeric(date[[1]][1]) != 31)
  {
    bVerbose <- TRUE
  }
  else
  {
    bVerbose <- FALSE
  }
  return(bVerbose)
}
##################################################################################################
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
##################################################################################
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


