#######################################################################
############### FUNZIONI SPECIFICHE PER BILANCIO FORECAST #############
#######################################################################
library(Hmisc)
library(lubridate)
library(plyr)

read_file_anagrafica <- function(ao)
{
  #names <- ao[1,]
  #ao2 <- ao[2:nrow(ao),]
  #colnames(ao2) <- names
  ao[is.na(ao)] <- 0
  return(ao)
}
#######
extract_rc_4_clusters <- function(ao)
{
  for(i in 1:nrow(ao))
  {
    if(ao[i,"CONSUMO_DISTRIBUTORE"] == "0")
    {
      ao[i,"CONSUMO_DISTRIBUTORE"] <- ao[i,"CONSUMO_CONTR_ANNUO"]
    }
  }
  variabili <- c("PDR", "CLIENTE_RAGIONE_SOCIALE", "CODICE_PRODOTTO", "DATA_INI_DEC_PROD_T", "DATA_FIN_DEC_PROD_T", "PROFILO_PRELIEVO", "CONSUMO_DISTRIBUTORE") 
  #variabili <- c("PDR", "CLIENTE_RAGIONE_SOCIALE", "CODICE_PRODOTTO", "inizio", "fine", "PROFILO_PRELIEVO", "CONSUMO_DISTRIBUTORE") 
  cols <- which(names(ao) %in% variabili)
  rows <- which(!(unlist(ao["CODICE_PRODOTTO"]) %in% c("SUPERI_E_QFISSA","P_FISSO_DIR","P_FISSO_IND")))
  
  return(ao[rows,cols])
}
######
extract_rc_4_clusters_shipper <- function(ao, shipper)
{
  ao <- ao[which(ao["SHIPPER"] == shipper),]
  for(i in 1:nrow(ao))
  {
    if(ao[i,"CONSUMO_DISTRIBUTORE"] == "0")
    {
      ao[i,"CONSUMO_DISTRIBUTORE"] <- ao[i,"CONSUMO_CONTR_ANNUO"]
    }
  }
  variabili <- c("PDR", "CLIENTE_RAGIONE_SOCIALE", "CODICE_PRODOTTO", "DATA_INI_DEC_PROD_T", "DATA_FIN_DEC_PROD_T", "PROFILO_PRELIEVO", "CONSUMO_DISTRIBUTORE") 
  #variabili <- c("PDR", "CLIENTE_RAGIONE_SOCIALE", "CODICE_PRODOTTO", "inizio", "fine", "PROFILO_PRELIEVO", "CONSUMO_DISTRIBUTORE") 
  cols <- which(names(ao) %in% variabili)
  rows <- which(!(ao["CODICE_PRODOTTO"] %in% c("SUPERI_E_QFISSA","P_FISSO_DIR","P_FISSO_IND")))
  
  return(ao[rows,cols])
}
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