###############################################################################
######################## BILANCIO FORECAST AUTOMATIZZATO ######################
###############################################################################

source("functions_for_OP.R")
source("sellings_with_components.R")
source("funzioni_specifiche_bilancio.R")

##### ATTENTION!!!!!!###############################################
## QUANDO VENGONO CARICATI I FILE, CONVERTI LE DATE IN TESTO !!!!!
## PARTENDO DALL'ANAGRAFICA ORIGINALE LE COLONNE CON LE DATE SONO: 
## DATA_INI_DEC_PROD (--> DATA_INI_DEC_PROD_T);
## DATA_FIN_DEC_PROD (--> DATA_FIN_DEC_PROD_T)
####################################################################

### CONTROLLA CHE SIA .XLSX
file <- "Z:/AREA ENERGY MANAGEMENT GAS/Davide_temp/160413-150847-214.xlsx"

## importa anagrafica
#ao <- openxlsx::read.xlsx("anagrafica_originale.xlsx", sheet = 1, colNames = TRUE)
ao <- openxlsx::read.xlsx("Report_214_26-01-2017.xlsx", sheet = 1, colNames = TRUE)
ao <- read_file_anagrafica(ao)

## sistema anagrafica in modo leggibile per R
#ao <- read_file_anagrafica(ao)

## prendi colonne e righe giuste
ao <- extract_rc_4_clusters(ao)
ao_AP <- extract_rc_4_clusters_shipper(ao, "0001808491-AXOPOWER SRL")

## isola i fissi
ap_fissi <- isolate_fixes(ao_AP)

#########################################################################################################
rows <- which(!(unlist(ao["CODICE_PRODOTTO"]) %in% c("SUPERI_E_QFISSA","P_FISSO_DIR","P_FISSO_IND")))  ##
ao <- ao[rows,]                                                                                        ##
#########################################################################################################
## aggrega i prodotti
aggregati <- compute_combinations_DEF_val(ao)
##################################################################################################################################
aggregati <- compute_combinations_DEF_val_Agenti(ao)                                                                             #
xlsx::write.xlsx(data.frame(aggregati), "cluster prodotti aggiornato con agenzie.xlsx", row.names = FALSE, col.names = TRUE)     #
##################################################################################################################################
xlsx::write.xlsx(data.frame(aggregati), "cluster prodotti aggiornato.xlsx", row.names = FALSE, col.names = TRUE)
#aggregati_AP <- compute_combinations_DEF(ao_AP)
#aggregati_fissi <- compute_combinations_DEF(ap_fissi)

## carica i profili di consumo
profili_consumo <- openxlsx::read.xlsx("profili.xlsx", sheet = 1, colNames = TRUE)
pm <- openxlsx::read.xlsx("profili_mensili.xlsx", sheet = 1, colNames = TRUE)
prof_consumo <- data.frame(profili_consumo[2:nrow(profili_consumo),3:38])

## scorciatoia:
vendite <- openxlsx::read.xlsx("vendite_aggiornato.xlsx", sheet = 1, colNames = TRUE)
head(vendite)

vendite[is.na(vendite)] <- 0
vendite[is.na(vendite)] <- 0
aggregati <- vendite[,1:5]
## calcola le curve di consumo
# curve_profili <- compute_profiles_DEF(aggregati, prof_consumo)
# curve_profili_AP <- compute_profiles_DEF(aggregati_AP, prof_consumo)
# curve_profili_fissi <- compute_profiles_DEF(aggregati_fissi, prof_consumo)
# 
# matplot(1:731, t(curve_profili), type="l")

## mercato a termine (carica il file): prima colonna = mercato a termine in volumi; seconda colonna = valore mercato a termine
tot_termine <- openxlsx::read.xlsx("tot_mercato_termine.xlsx", sheet = 1)
termine <- tot_termine[,3]
termine <- data.frame(t(termine))

## stoccaggio
stoc <- openxlsx::read.xlsx("stoccaggio.xlsx", sheet = 1, colNames = TRUE)
gia <- openxlsx::read.xlsx("giacenza.xlsx", sheet = 1,colNames = FALSE)

## mercato a pronti
x <- c(31,29,31,30,31,30,31,31,30,31,30,31)

fattore_conversione <- 1.057275
gia2 <- c(gia[1,1] - gia[32,1], gia[32,1] - gia[61,1], gia[61,1])
gia2 <- (gia2/1000)*(100/fattore_conversione)
gia2 <- c(gia2[1]/31, gia2[2]/29, gia2[3]/31)
tt <- as.numeric(termine)
## usare da riga 78 a riga 83. 
mkt <- c(rep(tt[1]/31,31), rep(tt[2]/29, 29), rep(tt[3]/31,31),rep(sum(tt[4:9])/183, 183), rep(sum(tt[10:12])/92, 92),rep(340157.48/90, 90),rep(0, 365-90) )
stok <- c(as.numeric(stoc[95:185,11]), rep(0,366-91), rep(0,365))
stok_prog <- rep(0, 731)
for(i in 1:730) stok_prog[i] <- stok[i] - stok[i+1] 

stok_prog <- (stok_prog/(10*fattore_conversione))

## modifica e usa il risultato di TOT_m3!!!
###open_position <- colSums(curve_profili_fissi) - (mkt + stok_prog)


## parte di vendita
pm <- openxlsx::read.xlsx("profili_mensili.xlsx", sheet = 2, colNames = TRUE)
listing <- openxlsx::read.xlsx("listing.xlsx", sheet = 1, colNames = TRUE)
listing <- unlist(listing[[1]])
head(pm)
listing

listingG <- openxlsx::read.xlsx("listing_G.xlsx", sheet = 1, colNames = TRUE)
listingG <- unlist(listingG[[1]])
listingG
###### file ripassato da Tecla con i prezzi dei vari prodotti (lascia solo la parte di vendita)
#vendite <- openxlsx::read.xlsx("vendite_aggiornato.xlsx", sheet = 1, colNames = TRUE)
#head(vendite)
#vendite <- vendite[,-c(2,3,5)]
#colnames(vendite) <- vendite[1,]
#vendite <- vendite[2:nrow(vendite),] 
#head(vendite)

TM <- total_sellings_per_components(vendite, pm, listing, listingG)
#tm <- total_selling(vendite,pm,listing)
TM <- data.frame(TM)

## calcolo del transfer price & consumo totale (meglio modficare il file per tenere solo TP --> suppongo di avere il file giusto)
TP <- openxlsx::read.xlsx("TP.xlsx", sheet = 1, colNames = TRUE)
TP[is.na(TP)] <- 0

tot <- TOT_m3(TP, pm)
tp <- compute_TP(TP,pm)
#plot(1:24, tp, type = "l", col="red", xlab = "tempo", ylab = "TP", main = "Andamento Transfer Price")


# a1 <- c("LGC_MF_1510" , "01/02/2016", "31/01/2017"  ,  "T2E3"   ,"27715",0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0)
# a2 <- c("RGC_MF_1510",  "01/02/2017", "31/12/2017",    "T2E3",   "27715",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1)
# 
# 
# 
# a <- data.frame(rbind(a1,a2))
# colnames(a) <- colnames(TP[,])
# 
# index <- intersect(intersect(in1,in2), intersect(in3,in4))
# index <- intersect(index, in5)

TPe <- TP[which(TP["shipper"] == "ENEL TRADE"),]

tot_enel <- TOT_m3(TPe,pm)
#te <- cbind(unlist(TP[index,"prodotto"]),TOT_m3mat(TP[index,],pm))

prodenel <- c("LGB_MF_1502","RGB_MF_1502","LGD_MF_1506","RGD_MF_1506","LGC_MF_1510","RGC_MF_1510","LGA_MF_1603","RGA_MF_1603","LG1_BF_BRED","LG1_BI_BRED","LG1-BF-LIFE","LG1-BF-POPL","LG1_BF_SIPA",
             "LG0-BI-CGNX","LGP-BI-TCNR","LG0-BI-VRGN","LG1-BI-KONE","LGP-BI-SPIC","LGP-BI-IVEF", "LGB_MF_1603","LGP-BI-FERR","LGP-BI-VALR", "LGD_MF_1510", "LGB_MF_1601", "LGD_MF_1508","LGP-BI-PIUS",
             "RGD_MF_1510","LGC_MF_1612","RGC_MF_1612")
acq <- c(18.26,18.26,24.05,24.05,24.65,24.65,24.65,24.65,23.10,23.10,24.73,24.65,24.75,23.65,23.12,22.30,25.50,16.11,16.60,24.65,17.30,16.00,16.90,16.90,16.90,17.23,16.90,16.90,16.90)

AC <- data.frame(t(acq))
colnames(AC) <- prodenel
## acquisto da shipper terzi GIUSTO:
mat_enel <- cbind(TPe["prodotto"],TOT_m3mat(TPe,pm))

### missing products:
pe <- unique(unlist(mat_enel[,1]))
length(pe) - sum(pe%in%prodenel)

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
#tot_ap <- TOT_m3(TP[which(unlist(TP["prodotto"]) %in% unlist(ap3["prodotto"])),],pm) # solo AP shipper fissi
tot_ap <- tot - tot_enel

## aggrego e esporto in excel -- calcolo open position -- nel bilancio forecast ci va l'open position calcolata su TUTTO il fabbisogno. NON solo AP fissi.
x16 <- c(31,29,31,30,31,30,31,31,30,31,30,31)
x17 <- c(31,28,31,30,31,30,31,31,30,31,30,31)

#pg <- c(661508.7/31, 631508.7/29,571508.7/31,(138330.7)*6/183,(695433.1*3)/92,(340157.5)*3/90 )

date <- openxlsx::read.xlsx("date.xlsx", sheet = 1, colNames = FALSE)
#date <- as.Date(date[2,],origin = "1899-12-30")

c_stok <- c(21.875, 21.875, 21.875)


#mkt <- data.frame(t(mkt))
stok_prog <- data.frame(t(stok_prog))
#colnames(mkt) <- colnames(date[1,])
colnames(stok_prog) <- unlist(date[,2])


#mmkt <- c(sum_in_year(mkt, "2016"), sum_in_year(mkt, "2017"))
mstok_prog <- c(sum_in_year(stok_prog, "2016"), sum_in_year(stok_prog, "2017"))


#stokm <- rep(mstok_prog/c(x16,x17), c(x16,x17))
#term <- c(rep(pg[1], 31),rep(pg[2], 29),rep(pg[3], 31), rep(pg[4], 183), rep(pg[5], 92), rep(pg[6], 90), rep(0, 275))
#term <- tt

mmkt2 <- c(as.numeric(tot_termine[,3]), 0,0,0,0,0,0,0,0,0)
#ST <- stokm + term
#op <- tot_fap - (mstok_prog + mmkt2)
op <- tot_ap - (mstok_prog + mmkt2)

#grafici
# plot(1:731, colSums(curve_profili), type="l", lwd = 2, xlab="tempo", ylab="fabbisogno",main="fabbisogno totale, mercato a termine e stoccaggio")
# plot(1:731, totg, type="l", lwd = 2, xlab="tempo", ylab="fabbisogno",main="fabbisogno, mercato a termine e stoccaggio")
# abline(h=0, col = "red", lwd=2)
# rect(0, 0, 31,pg[1]+(mstok_prog[1]/31), col = "grey")
# rect(31, 0, 60,pg[2]+(mstok_prog[2]/29), col = "grey")
# rect(60, 0, 91,pg[3]+(mstok_prog[3]/31), col = "grey")
# rect(91, 0, 274,pg[4], col = "grey")
# rect(274, 0, 366,pg[5], col = "grey")
# rect(366, 0, 456,pg[6], col = "grey")
# lines(1:731, totg, type="l", lwd = 2)
# 
# plot(1:24, op, type="l", lwd = 2, xlab="tempo", ylab="open position",main="open position")

#mop <- c(sum_in_year(open_position, "2016"), sum_in_year(open_position, "2017"))
## per lo stoccaggio il prezzo = euro/MWh. Ecco da dove arriva la discrepanza. Bisogna o calcolare il prezzo al m3 o tenere in MWh l'unita di misura
totale_costi <- (op * listing/100) + terzi + (mstok_prog * c_stok*1.05275/100) + c(as.numeric(tot_termine[,2]), 0,0,0,0,0,0,0,0,0) 

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
                       c(tot_termine[,2],0,0,0,0,0,0,0,0,0),
                       op * listing/100,
                       (mstok_prog * c_stok*1.05275/100),
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


colnames(TM2) <- c("gennaio-2016", "febbraio-2016","marzo-2016","aprile-2016","maggio-2016","giugno-2016",
                  "luglio-2016", "agosto-2016","settembre-2016","ottobre-2016","novembre-2016","dicembre-2016",
                  "gennaio-2017", "febbraio-2017","marzo-2017","aprile-2017","maggio-2017","giugno-2017",
                  "luglio-2017", "agosto-2017","settembre-2017","ottobre-2017","novembre-2017","dicembre-2017")

rownames(TM2) <- c("PGas", "qtmvc", "cpr", "grad", "ccr", "qtint", "qtpsv", "qvdvar", "ccvdvar", "total","acquisti da terzi", "mercato a termine", "mercato a pronti", "stoccaggio",
                  "costi supero","tot costi TP","margine Axopower","margine EM","margine comm","prezzo di vendita unitario sui ricavi",
                  "costo di approvvigionamento unitario sull'acquistato", "margine unitario commerciale", "tot gas acq", "tot gas vend")


xlsx::write.xlsx(TM2,"estrazione_bilancio_forecast.xlsx", row.names = TRUE, col.names = TRUE)




