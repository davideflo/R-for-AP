#######################################################################
############### FUNZIONI SPECIFICHE PER BILANCIO FORECAST #############
#######################################################################

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

