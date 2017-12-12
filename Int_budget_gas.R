#############################################################
########## Intermediate file for GAS Budget #################
#############################################################


source("C://Users//d_floriello//Documents//RCODE//Budget_gas_functions.R")

MakeCluster <- function(filename, year)
{
  ao <- openxlsx::read.xlsx(filename, sheet = 1, colNames = TRUE)
  ao <- read_file_anagrafica(ao)
  
  aggregati <- compute_combinations_DEF_val(ao, year)
  aggregati2 <- round_date(aggregati)
  xlsx::write.xlsx(data.frame(aggregati2), "cluster prodotti aggiornato.xlsx", row.names = FALSE, col.names = TRUE)
  
  print("Cluster fatti")
  
}
###########################################################################################################

MakeGASBudget <- function(filename, year)
{
  ## carica profili di consumo
  profili_consumo <- openxlsx::read.xlsx("profili.xlsx", sheet = 1, colNames = TRUE)
  pm <- openxlsx::read.xlsx("profili_mensili.xlsx", sheet = 1, colNames = TRUE)
  prof_consumo <- data.frame(profili_consumo[2:nrow(profili_consumo),3:38])
  
  ## scorciatoia:
  vendite <- AggiornaVendite(filename)
  vendite[is.na(vendite)] <- 0
  vendite <- vendite[which(vendite$prodotto != "0"),]
  aggregati <- vendite[,1:5]
  
  ## acquisti a termine
  tot_termine <- openxlsx::read.xlsx("tot_mercato_termine.xlsx", sheet = 1)
  termine <- tot_termine[,3]
  termine <- data.frame(t(termine))
  
  ## stoccaggio
  
  ## parte di vendita
  listing <- openxlsx::read.xlsx("listing.xlsx", sheet = 1, colNames = TRUE)
  listing <- unlist(listing[[1]])
  head(pm)
  listing
  
  listingG <- rep(0, 24)
  
  TM <- total_sellings_per_components(vendite, pm, listing, listingG, 2017)
  TM <- data.frame(TM)
  
  ## calcolo del transfer price & consumo totale (meglio modficare il file per tenere solo TP --> suppongo di avere il file giusto)
  TP <- AggiornaTP(filename)
  TP[is.na(TP)] <- 0
  
  tot <- TOT_m3(TP, pm)
  tp <- compute_TP(TP,pm)

    TPe <- TP[which(TP["shipper"] == "ENEL TRADE"),]
  
  tot_enel <- TOT_m3(TPe,pm)
  
  
  prodenel <- c("LGB_MF_1502","RGB_MF_1502","LGD_MF_1506","RGD_MF_1506","LGC_MF_1510","RGC_MF_1510","LGA_MF_1603","RGA_MF_1603","LG1_BF_BRED","LG1_BI_BRED","LG1-BF-LIFE","LG1-BF-POPL","LG1_BF_SIPA",
                "LG0-BI-CGNX","LGP-BI-TCNR","LG0-BI-VRGN","LG1-BI-KONE","LGP-BI-SPIC","LGP-BI-IVEF", "LGB_MF_1603","LGP-BI-FERR","LGP-BI-VALR", "LGD_MF_1510", "LGB_MF_1601", "LGD_MF_1508","LGP-BI-PIUS",
                "RGD_MF_1510","LGC_MF_1612","RGC_MF_1612", "RGB_MF_1601", "LGC_MF_1610","RGC_MF_1610","LGD_UF_1707")
  acq <- c(18.26,18.26,24.05,24.05,24.65,24.65,24.65,24.65,23.10,23.10,24.73,24.65,24.75,23.65,23.12,22.30,25.50,16.11,16.60,24.65,17.30,16.00,16.90,16.90,16.90,17.23,16.90,16.90,16.90,16.00,16.00,16.00,16.00)
  
  AC <- data.frame(t(acq))
  colnames(AC) <- prodenel
  ## acquisto da shipper terzi GIUSTO:
  mat_enel <- cbind(TPe["prodotto"],TOT_m3mat(TPe,pm))
  
  ### missing products:
  pe <- unique(unlist(mat_enel[,1]))
  print(paste0("mancano ",length(pe) - sum(pe%in%prodenel)," prodotti di ENEL"))
  
  ME <- matrix(0,nrow=nrow(mat_enel),ncol=24)
  for(i in 1:nrow(mat_enel))
  {
    print(i)
    print(mat_enel[i,1])
    
    x <- as.matrix(as.numeric(mat_enel[i,2:25]) * AC[,which(colnames(AC) == mat_enel[i,1])])
    ME[i,] <- x
  }
  
  ME <- ME/100
  terzi <- colSums(ME)
  
  ## solo AP
  tp_ap <- compute_TP(TP[which(TP["shipper"] == "AXOPOWER"),],pm) # solo AP shipper fissi
  tot_ap <- tot - tot_enel
  
  mstok_prog <- rep(0,24)
  
  mmkt2 <- c(as.numeric(tot_termine[,3]), rep(0,9))
  op <- tot_ap - (mstok_prog + mmkt2)
  
  totale_costi <- (op * listing/100) + terzi + c(as.numeric(tot_termine[,2]), rep(0,9)) 
  
  
  tot_costi_supero_capacita <- -1.3 * tot/100
  ### tot gas venduto = tot gas acquistato = TOT_m3
  ### tot ricavi - tot costi:
  #margine_AP <- as.numeric(forcest[1,2:13]) - colSums(as.data.frame.matrix(forcest[2:5,2:13])) <---- vedi riga 1963
  ### tot costi TP:
  tot_TP <- tp*tot/100
  ## margine AxoPower:
  margine_ap <- TM[10,] - totale_costi + tot_costi_supero_capacita
  ### margine energy management
  margine_EM <- tot_TP - totale_costi
  ### margine commerciale:
  margine_comm <- TM[10,] - tot_TP
  ### prezzi unitari:
  ### prezzo di vendita unitario sui ricavi
  ric_unit <- (TM[10,]/tot)*100
  ### costo di approvvigionamento unitario sull'acquistato
  unit_approv <- (totale_costi/tot)*100
  ### margine unitario commerciale
  unit_comm <- (margine_comm/tot)*100
  
  df <- data.frame(rbind(terzi,
                         c(tot_termine[,2],rep(0,9)),
                         op * listing/100,
                         mstok_prog,
                         tot_costi_supero_capacita, 
                         tot_TP,
                         margine_ap,
                         margine_EM,
                         margine_comm,
                         ric_unit,
                         unit_approv,
                         unit_comm,
                         tot,
                         tot))
  
  
  TM2 <- rbind(TM, df)
  
  
  colnames(TM2) <- c(paste0(c("gennaio-", "febbraio-","marzo-","aprile-","maggio-","giugno-",
                     "luglio-", "agosto-","settembre-","ottobre-","novembre-","dicembre-"),year),
                     paste0(c("gennaio-", "febbraio-","marzo-","aprile-","maggio-","giugno-",
                              "luglio-", "agosto-","settembre-","ottobre-","novembre-","dicembre-"),year+1))
                     
  rownames(TM2) <- c("PGas", "qtmvc", "cpr", "grad", "ccr", "qtint", "qtpsv", "qvdvar", "ccvdvar", "total","acquisti da terzi", "mercato a termine", "mercato a pronti", "stoccaggio",
                     "costi supero","tot costi TP","margine Axopower","margine EM","margine comm","prezzo di vendita unitario sui ricavi",
                     "costo di approvvigionamento unitario sull'acquistato", "margine unitario commerciale", "tot gas acq", "tot gas vend")
  
  
  xlsx::write.xlsx(TM2,"estrazione_bilancio_forecast.xlsx", row.names = TRUE, col.names = TRUE)
  
}

