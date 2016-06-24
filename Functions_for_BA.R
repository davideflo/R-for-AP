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
  #print(MN)
  if(length(which(MN >= 0)) > 0) tau <- min(which(MN >= 0))
  else tau <- -1
  return(tau)
}
###########################################################
IRG_agenzia <- function(ag)
{
  tau <- c()
  pods <- unique(unlist(ag["Codice.POD"]))
  
  for(pod in pods)
  {
    tau <- c(tau, IRG(ag,pod))
    taup <- tau[tau > 0]
    taun <- tau[tau <= 0]
  }
  return(c(mean(taup), mean(taun)))
}
###########################################################
IRG_vs_edl <- function(ag)
{
  pods <- unique(unlist(ag["Codice.POD"]))
  an <- data_frame()
  for(i in 1:length(pods))
  {
    l <- length(which(ag["Codice.POD"] == pods[i]))
    an2 <- data.frame(as.character(pods[i]), as.numeric(IRG(ag, pods[i])),
                      as.numeric(as.character(((sum(unlist(as.numeric(ag[which(ag["Codice.POD"] == pods[i]),"Energia.(incl..perdite)"])))/1000)/l)*12)),
                      as.numeric(as.character(l)),
                      as.character(stri_sub(ag[which(ag["Codice.POD"] == pods[i])[1],"Cod..prodotto"],from = 3, to = 3)))
    an <- rbind.data.frame(an, an2)
  }
#  an <- data.frame(an)
  colnames(an) <- c("POD", "IRG", "Energia media annua", "durata", "listino")
  return(an)
}
##########################################################
EM_agenzia <- function(ag)
{
  energy <- c()
  pods <- unique(unlist(ag["Codice.POD"]))
  
  for(pod in pods) {l <- length(which(ag["Codice.POD"] == pod));energy <- c(energy, 
                               as.numeric(as.character(((sum(unlist(as.numeric(ag[which(ag["Codice.POD"] == pod),"Energia.(incl..perdite)"])))/1000)/l)*12)))
  }
  return(mean(energy))
}
##########################################################
DM_agenzia <- function(ag)
{
  dur <- c()
  pods <- unique(unlist(ag["Codice.POD"]))
  
  for(pod in pods) dur <- c(dur,length(which(ag["Codice.POD"] == pod)))
  return(mean(dur))
}
##########################################################
UEM_process <- function(ag, pod)
{
  x <- ag[which(ag["Codice.POD"] == pod),]
  y <- as.numeric(unlist(x["Margine.Unitario"])) - as.numeric(unlist(x["Ricorrente_per_listino"]))
  return(y)
}
####################################################################
create_dataset_BA <- function(ag)
{
  il <- c()
  for(i in 1:nrow(ag))
  {
    if(sum(maply(3:ncol(ag), function(n)as.numeric(ag[i,n])), na.rm=TRUE) == 0) il <- c(il, i)
  }
  ii <- which(ag[il,"Codice.POD"] == "Totale pod")
  il <- c(il, (il[ii]+2):nrow(ag))
  agmod <- ag[setdiff(1:nrow(ag),il),]
  return(agmod)
}
###################################################################
active <- function(ag)
{
  pods <- unique(unlist(ag["Codice.POD"]))
  act <- matrix(0, nrow=length(pods), ncol=12*4)
  rownames(act) <- pods
  for(pod in pods)
  {
    cust <- ag[which(ag["Codice.POD"] == pod),]
    for(i in 1:nrow(cust))
    {
      indice_col <- 12*(as.numeric(cust[i,"Anno.elab."]) - 2013) + as.numeric(cust[i,"Mese.rif."])
      act[which(rownames(act) == pod),indice_col] <- 1
    }
  }
  return(act)
}
#################################################################
get_duration_processes <- function(act)
{
  Nt <- Rt <- At <- rep(0, ncol(act))
  for(i in 1:nrow(act))
  {
    cs <- cumsum(act[i,])
    count <- min(which(cs == max(cs)))
    for(j in 1:(length(cs)-1))
    {
      #print(j)
      bool <- j < count
      #print(bool)
      if(bool)
      {
        #print(paste("cond:", cs[j] != 0 & cs[j+1] == cs[j]))
        if(cs[j] == 1) Nt[j] <- Nt[j] + 1
        else if(cs[j] != 0 & cs[j+1] > cs[j]) At[j] <- At[j] + 1
        else if(cs[j] != 0 & cs[j+1] == cs[j]) {Rt[j] <- (Rt[j] + 1) ; At[j] <- At[j] + 1;} #At[j+1] <- At[j] - 1}
      }
      else
      {
        if(cs[j] == 1) Nt[j] <- Nt[j] + 1
        else if(cs[j] != 0 & cs[j+1] > cs[j]) At[j] <- At[j] + 1
        #else if(cs[j] != 0 & cs[j+1] == cs[j]) {At[j+1] <- At[j] - Rt[j]}
      }
    }
  }
  return(list(Nt,At,Rt))
}
####################################################################
get_duration_statistics <- function(Nt,Rt,At)
{
  Tt <- rep(0, length(At))
  for(i in 1:length(At))
  {
    if(At[i] != 0) Tt[i] <- (Nt[i] + Rt[i])/At[i]
  }
  return(Tt)
}
###############################################################
rolling_clienti_pod <- function(ag)
{
  pods <- unique(unlist(ag["Codice.POD"]))
  rs <- unique(unlist(ag["Ragione.sociale"]))
  
  tot_pod <- length(pods)
  tot_rs <- length(rs)
  
  rolling_pod <- length(unique(ag[which(ag[,"Stato"] == "NON IN FORNITURA"),"Codice.POD"]))/tot_pod
  rolling_rs <- length(unique(ag[which(ag[,"Stato"] == "NON IN FORNITURA"),"Ragione.sociale"]))/tot_rs
  
  return(c(rolling_pod,rolling_rs))
}
########################################################################
monthwise_rolling <- function(ag)
{
  years <- c(2015, 2016)
  months <- c(1:12)
  mat1 <- mat2 <- rep(0, 12*2)
  
  for(y in years)
  {
    for(m in months)
    {
      ag1 <- ag[which(as.numeric(ag["Anno.elab."]) == y & as.numeric(ag["Mese.rif"]) == m),]
      res <- rolling_clienti_pod(ag1)
      indice_col <- 12*(y-2015) + m
      mat1[indice_col] <- res[1]
      mat2[indice_col] <- res[2]
    }
  }
  return(list(mat1, mat2))
}
#########################################################################
marginality_ratio <- function(ag)
{
  map <- mag <- 0
  pods <- unique(unlist(ag["Codice.POD"]))
  
  for(pod in pods)
  {
    csap <- cumsum(as.numeric(ag[which(ag["Codice.POD"] == pod),"Margine_listino+PCV+Pereq"]))
    csag <- cumsum(as.numeric(ag[which(ag["Codice.POD"] == pod),"Gettone+Ricorrente"]))
    map <- map + csap[length(csap)]
    mag <- mag + csag[length(csag)]
  }
  return(mag/(mag+map))
}
###########################################################################
marginality_ratio_finale <- function(ag)
{
  ag1 <- ag[which(ag[,"Stato"] == "IN FORNITURA"),]
  return(marginality_ratio(ag1))
}
###########################################################################
pod_attivi_at <- function(ag, y, m)
{
  pods <- unique(unlist(ag["Codice.POD"]))
  finiti <- c()
  
  for(pod in pods)
  {
    ag1 <- ag[which(ag["Codice.POD"] == pod),]
    ic <- 12*(y-2015) + m
    if(nrow(ag1) < ic) finiti <- c(finiti, pod)
  }
  rim <- setdiff(pods, finiti)   
  return(ag[rim,])
}
###########################################################################
monthwise_marginality_ratio <- function(ag)
{
  years <- c(2015, 2016)
  months <- c(1:12)
  ratio <- rep(0, 12*2)
  
  for(y in years)
  {
    for(m in months)
    {
      ag1 <- pod_attivi_at(ag,y,m)
      ag2 <- ag1[which(as.numeric(ag1["Anno.elab."]) <= y & as.numeric(ag1["Mese.rif"]) <= m),]
      indice_col <- 12*(y-2015) + m
      ratio[indice_col] <- marginality_ratio(ag2)
    }
  }
  return(ratio)
}
############################################################################
weigthing_coin <- function(ag)
{
  pods <- unique(unlist(ag["Codice.POD"]))
  mat <- matrix(0, nrow=length(pods), ncol=4)
  rownames(mat) <- pods
  colnames(mat) <- c("gettone_per_ricorrente","gettone_per_margine","gettone_per_ric_unit","energia_media_annua")
  for(pod in pods)
  {
    i <- which(rownames(mat) == pod)
    l <- length(which(ag["Codice.POD"] == pod))
    csg <- cumsum(as.numeric(ag[which(ag["Codice.POD"] == pod),"Gettone"]))
    csric <- cumsum(as.numeric(ag[which(ag["Codice.POD"] == pod),"Ricorrente_per_listino"]))
    csmag <- cumsum(as.numeric(ag[which(ag["Codice.POD"] == pod),"Gettone+Ricorrente"]))
    mat[i,1] <- csg[length(csg)]/csric[length(csric)]
    mat[i,2] <- csg[length(csg)]/csmag[length(csmag)]
    mat[i,3] <- csg[length(csg)]/csric[1]
    mat[i,4] <- ((sum(unlist(as.numeric(ag[which(ag["Codice.POD"] == pod),"Energia.(incl..perdite)"])))/1000)/l)*12
  }
  return(data.frame(mat))
}




