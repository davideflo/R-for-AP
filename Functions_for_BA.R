### Functions for Business Analysis
library(openxlsx)
library(reshape)
library(stringi)
library(plyr)
library(dplyr)
library(xlsx)
library(vioplot)
library(fda)
library(h2o)
library(TSA)
library(tseries)
library(rnn)

############################################################
compute_margine_netto <- function(ag)
{
  for(i in 1:nrow(ag))
  {
    margine_ap <- ag[i,"Margine_listino"] + ag[i,"Importo.PCV"] + ag[i,"Pereq.Totale"]
    #margine_ap <- (ag[i,"Energia"]/1000)*ag[i,"Margine Unitario"] + ag[i,"Importo PCV"] + ag[i,"Prezzo perequazione"] * ag[i,"Energia"]
    margine_ag <- ag[i, "Gettone"] + ag[i, "Ricorrente_Totale"]
    #margine_ag <- ag[i, "Gettone"] + ag[i, "Ricorrente_per_listino"] * (ag[i,"Energia"]/1000)
    ag[i,"Margine_listino+PCV+Pereq"] <- margine_ap
    ag[i,"Gettone+Ricorrente"] <- margine_ag
    ag[i,"Margine_netto"] <- margine_ap - margine_ag
  }
  return(ag)
}
###########################################################
IRG <- function(ag, pod)
{
  cust <- ag[which(ag["Codice.POD"] == pod),]
  MN <- cumsum(unlist(cust["Margine_netto"]))
  print(MN)
  if(length(which(MN >= 0)) > 0) tau <- min(which(MN >= 0))
  else tau <- -1
  return(tau)
}
###########################################################
IRG_vs_edl <- function(ag)
{
  pods <- unique(unlist(ag["Codice.POD"]))
  an <- matrix(0, nrow = length(pods),ncol=5)
  for(i in 1:length(pods))
  {
    l <- length(which(ag["Codice.POD"] == pods[i]))
    an[i,1] <- pods[i]
    an[i,2] <- IRG(ag, pods[i])
    an[i,3] <- ((sum(unlist(ag[which(ag["Codice.POD"] == pods[i]),"Energia"]))/1000)/l)*12
    an[i,4] <- l
    an[i,5] <- stri_sub(ag[which(ag["Codice.POD"] == pods[i])[1],"Cod..prodotto"],from = 3, to = 3)
  }
  an <- data.frame(an)
  colnames(an) <- c("POD", "IRG", "Energia media annua", "durata", "listino")
  return(an)
}
##########################################################
UEM_process <- function(ag, pod)
{
  x <- ag[which(ag["Codice.POD"] == pod),]
  y <- as.numeric(unlist(x["Margine.Unitario"])) - as.numeric(unlist(x["Ricorrente_per_listino"]))
  return(y)
}













