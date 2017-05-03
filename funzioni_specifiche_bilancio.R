#######################################################################
############### FUNZIONI SPECIFICHE PER BILANCIO FORECAST #############
#######################################################################
library(Hmisc)

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
round_date <- function(aggregati)
{
  for(i in 1:nrow(aggregati))
  {
    print(i)
    splitted <- strsplit(aggregati$`data fine`[i],"/")
    if(as.numeric(splitted[[1]][1]) != 31 | as.numeric(splitted[[1]][1]) != 28 | as.numeric(splitted[[1]][1]) != 30)
    {
      if(as.numeric(splitted[[1]][1]) < 15)
      {
        month <- ifelse(as.numeric(splitted[[1]][2])-1 == 0, 12, as.numeric(splitted[[1]][2])-1)
        aggregati$`data fine`[i] <- paste0(monthDays(as.Date(paste0(splitted[[1]][3],"-",month,"-01"))),"/",as.numeric(splitted[[1]][2])-1,"/",splitted[[1]][3])
      }
      else
      {
        aggregati$`data fine`[i] <- paste0(monthDays(as.Date(paste0(splitted[[1]][3],"-",as.numeric(splitted[[1]][2]),"-01"))),"/",as.numeric(splitted[[1]][2]),"/",splitted[[1]][3])
      }
    }
  }
  return(aggregati)
}
