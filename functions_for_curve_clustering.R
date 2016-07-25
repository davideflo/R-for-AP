library(openxlsx)
library(plyr)
library(dplyr)
library(reshape)
library(stringi)
library(xlsx)

source("functions_for_OP.R")
############################################################################################
clean_data <- function(cott)
{
  PDR <- unique(unlist(cott["PDR"]))
  dates <- unique(unlist(cott["DATA_TESTO"]))
  mat_mis <- matrix(0, nrow = length(PDR), ncol = length(dates))
  mat_mis <- data.frame(mat_mis)
  colnames(mat_mis) <- dates
  for(i in 1:length(PDR))
  {
    print(PDR[i])
    rows <- which(cott["PDR"] == PDR[i])
    mis <- cott[rows,c("DATA_TESTO","LETT_MIS","MATR_MIS")]
    dates2 <- mis[,"DATA_TESTO"]
    ld2 <- length(dates2)
    for(j in 1:(ld2 - 1) )
    {
      if(ld2 > 1)
      {
        col1 <- which(colnames(mat_mis) == dates2[j])
        if(mis[j+1,3] == mis[j,3]) mat_mis[i,col1] <- as.numeric(mis[j+1,2]) - as.numeric(mis[j,2])
        else mat_mis[i,col1] <- 0
      }
      else {
        next
      }          
    }
  }
  rownames(mat_mis) <- PDR
  return(mat_mis)
}
######################################################################################
find_negatives <- function(mese, neg, MESE)
{
  me <- c()
  for(i in 1:nrow(mese))
  {
    if(!all(mese[i,] >= 0)) me <- c(me,rownames(mese)[i])
  }
  neg$MESE<- me
  return(neg)
}
###################################################################
remove_negatives <- function(mese, MESE)
{
  neg <- c()
  nam <- find_negatives(mese, neg, MESE)
  neg <- which(rownames(mese)%in%unlist(nam))
  if(length(neg) > 0){
    neg <- (-1)*unlist(neg)
    print(neg)
    return(mese[neg,])
  }
  else return(mese)
}
###################################################################
take_rdays_leap <- function(mese, MESE)
{
  if(MESE %in% c("GENNAIO", "MARZO","MAGGIO", "LUGLIO", "AGOSTO","OTTOBRE","DICEMBRE"))
    return(mese[,1:31])
  else if(MESE %in% c("APRILE", "GIUGNO","SETTEMBRE", "NOVEMBRE"))
    return(mese[,1:30])
  else
    return(mese[,1:29])
}
#####################################################################
take_rdays <- function(mese, MESE)
{
  if(MESE %in% c("GENNAIO", "MARZO","MAGGIO", "LUGLIO", "AGOSTO","OTTOBRE","DICEMBRE"))
    return(mese[,1:31])
  else if(MESE %in% c("APRILE", "GIUGNO","SETTEMBRE", "NOVEMBRE"))
    return(mese[,1:30])
  else
    return(mese[,1:28])
}
##################################################################
read_and_plot_data <- function(MESE)
{
  mese <- openxlsx::read.xlsx("LETTURE GIORNALIERE GAS.xlsx", sheet = MESE, colNames = TRUE)
  mese[is.na(mese)] <- 0
  mese <- clean_data(mese)
  mese2 <- remove_negatives(mese, MESE)
  mese2 <- take_rdays_leap(mese2,MESE)
  matplot(1:ncol(mese2), t(mese2), type="l", lwd=1.5, xlab="giorni",ylab = "misurazioni gas", main=MESE)
  return(mese2)
}
#######################################################################################
read_and_plot_data_2015 <- function(MESE)
{
  mese <- openxlsx::read.xlsx("letture giornaliere gas gen-sett-15.xlsx", sheet = MESE, colNames = TRUE)
  mese[is.na(mese)] <- 0
  mese <- clean_data(mese)
  mese2 <- remove_negatives(mese, MESE)
  mese2 <- take_rdays(mese2,MESE)
  matplot(1:ncol(mese2), t(mese2), type="l", lwd=1.5, xlab="giorni",ylab = "misurazioni gas", main=MESE)
  return(mese2)
}
##########################################################################################
read_and_plot_data_2014 <- function(MESE)
{
  mese <- openxlsx::read.xlsx("letture giornaliere gas 2014.xlsx", sheet = MESE, colNames = TRUE)
  mese[is.na(mese)] <- 0
  mese <- clean_data(mese)
  mese2 <- remove_negatives(mese, MESE)
  mese2 <- take_rdays(mese2,MESE)
  matplot(1:ncol(mese2), t(mese2), type="l", lwd=1.5, xlab="giorni",ylab = "misurazioni gas", main=MESE)
  return(mese2)
}
############################################################################################
read_and_plot_data_2013 <- function(MESE)
{
  mese <- openxlsx::read.xlsx("letture giornaliere gas 2013.xlsx", sheet = MESE, colNames = TRUE)
  mese[is.na(mese)] <- 0
  mese <- clean_data(mese)
  mese2 <- remove_negatives(mese, MESE)
  mese2 <- take_rdays(mese2,MESE)
  matplot(1:ncol(mese2), t(mese2), type="l", lwd=1.5, xlab="giorni",ylab = "misurazioni gas", main=MESE)
  return(mese2)
}
###################################################################################################
aggregate_months <- function(m1, m2)
{
  pdr1 <- rownames(m1)
  pdr2 <- rownames(m2)
  pdr <- intersect(pdr1,pdr2)
  agg_mat <- matrix(0, nrow=length(pdr),ncol=(ncol(m1)+ncol(m2)))
  for(i in 1:length(pdr))
  {
    print(pdr[i])
    agg_mat[i,] <- unlist(c(m1[which(rownames(m1) == pdr[i]),1:(ncol(m1)-1)],0,m2[which(rownames(m2) == pdr[i]),1:ncol(m2)]))
    if(m2[which(rownames(m2) == pdr[i]),1] - m1[which(rownames(m1) == pdr[i]),ncol(m1)] >= 0) 
      agg_mat[i,ncol(m1)] <- m2[which(rownames(m2) == pdr[i]),1] - m1[which(rownames(m1) == pdr[i]),ncol(m1)]
    else agg_mat[i,ncol(m1)] <- 0
  }
  agg_mat <- data.frame(agg_mat)
  rownames(agg_mat) <- pdr
  colnames(agg_mat) <- c(colnames(m1),colnames(m2))
  return(agg_mat)
}
##################################################################
CH_statistics <- function(data, K,mi)
{
  argvals <- seq(0, 1, length.out = ncol(data))
  CH <- c()
  n <- nrow(data)
  for(i in 2:(K-1))
  {
    fkm <- FKMSparseClustering(data, argvals, K=i, m= 0.8, method="kmea",maxiter=mi)
    bcss <- fkm$OBJ
    w <- fkm$W
    wcss <- GetWCSS(data,fkm$CLUSTER,w)
    CH <- c(CH, (bcss/i-1)/(wcss$wcss/(n-i)) )
  }
  return(CH)
}
#################################################################
plot_clusters_mean <- function(data,K,mfr,mfc,name,plot=TRUE)
{
  argvals <- seq(0, 1, length.out = ncol(data))
  fkm <- FKMSparseClustering(data, argvals, K=K, m= 0.8, method="kmea")
  
  nk <- list(NULL)
  mat_means <- matrix(0, nrow=K, ncol=ncol(data))
  for(i in 1:K)
  {
    
    #nk$i <- c(names(fkm$CLUSTER[which(fkm$CLUSTER == i)]))
    nk[[i]] <- c(names(fkm$CLUSTER[which(fkm$CLUSTER == i)]))
    print(nk[[i]])
    print(paste("#cluster", i, "=", length(unlist(nk[[i]]))))
    mat_means[i,] <- unlist(colMeans(data[which(rownames(data)%in%unlist(nk[[i]])),]))
  }
  if(plot)
  {
    par(mfrow=c(mfr,mfc))
    for(i in 1:K) plot(argvals, mat_means[i,], type="l", lwd=2, col= i,xlab="giorni",ylab=paste("mean cluster",i), main = name)    
  }

  return(nk)
}
#####################################################################
extract_cons <- function(ao, pdrs)
{## terza colonna = 1 se consumo_distributore 
  mat <- matrix(0,nrow=length(pdrs),ncol=3)
  for(p in 1:length(pdrs))
  {
    if(pdrs[p] != "0")
    {
      rows <- which(ao["PDR"] == pdrs[p])
      if(length(rows > 0))
      {
        mr <- max(rows)
        mat[p,2] <- ao[mr,"PROFILO_PRELIEVO"]
        if(ao[mr,"CONSUMO_DISTRIBUTORE"] != 0){
        mat[p,1] <- ao[mr,"CONSUMO_DISTRIBUTORE"]
        mat[p,3] <- 1
        }
        else mat[p,1] <- ao[mr,"CONSUMO_CONTR_ANNUO"]
      }
    }
  }
  mat <- data.frame(mat)
  rownames(mat) <- pdrs
  #mat <- mat[which(mat[,2] != "0"),]
  return(mat)
}
####################################################################  
expected_monthly_consumption <- function(mat, month, year, prof) 
  ### month e' del tipo "GENNAIO", "FEBBRAIO", ecc...
{
  res <- matrix(0,nrow=nrow(mat), ncol = nrow(prof))
  for(i in 1:nrow(mat))
  {
    if(mat[i,2] != "0")
    {
      col <- which(colnames(prof) == mat[i,2])
      res[i,] <- as.numeric(as.character(mat[i,1])) * as.numeric(as.character((unlist(prof[,col]))))/100
    }
  }
  res <- data.frame(res)
  colnames(res) <- unlist(prof["Data_T"])
  rownames(res) <- rownames(mat)
  
  month2 <- map_months(month)
  mese <- take_month(month2, "2016",res)
  return(res[,mese])
}
###################################################################
plot_overlapping_pdr_curves <- function(mat1, mat2, pdr,mese)
{
  act <- as.numeric(as.character(mat2[which(rownames(mat2) == pdr),]))
  est <- as.numeric(as.character(mat1[which(rownames(mat1) == pdr),]))
  if(max(act) >= max(est))
  {
    plot(1:length(act), act, type="l", col="black", lwd=2, xlab="giorni",ylab="cons",main=mese)
    lines(1:length(est), est, type="l", col="red", lwd=2)  
  }
  else
  {
    plot(1:length(est), est, type="l", col="black", lwd=2, xlab="giorni",ylab="cons",main=mese)
    lines(1:length(act), act, type="l", col="red", lwd=2)
  }
}
################################################################
curves_given_pdr <- function(mat1, mat2, pdr)
{
  act <- as.numeric(as.character(mat2[which(rownames(mat2) == pdr),]))
  est <- as.numeric(as.character(mat1[which(rownames(mat1) == pdr),]))
  return(rbind(act,est))
}
###############################################################
compute_mean <- function(mat, pdr_list)
{
  n <- length(pdr_list)
  meaned <- rep(0,ncol(mat))
  {
    for(p in pdr_list)
    {
      meaned <- meaned + mat[which(rownames(mat) == p),]
    }
  }
  return(meaned/n)
}
###############################################################
sing_error <- function(i, mc, tc)
{
  #print(unlist(mc) - unlist(tc[i,]))
  return(unlist(mc) - unlist(tc[i,]))
}
##############################################################
ddistribution_error <- function(mean_curve, th_curve)
{
  mat <- matrix(0, nrow = nrow(th_curve), ncol = ncol(th_curve))
  mat <- data.frame(mat)
  colnames(mat) <- colnames(th_curve)
  rownames(mat) <- rownames(th_curve)
  for(i in 1:nrow(mat))
  {
    mat[i,] <- sing_error(i, mean_curve, th_curve)
  }
  return(mat)
}
###############################################################
sing_error_rt <- function(i, mc, tc)
{
  return(unlist(mc[i,]) - unlist(tc[i,]))
}
###############################################################
ddistribution_error_rt <- function(mean_curve, th_curve)
{
  mat <- maply(1:nrow(th_curve), function(i) sing_error_rt(i, mean_curve, th_curve))
  return(mat)
}
#############################################################
vplot31 <- function(err1)
{
  par(mfrow=c(1,1))
  vioplot(err1[,1],err1[,2],err1[,3],err1[,4],err1[,5],err1[,6],err1[,7],err1[,8],err1[,9],err1[,10],err1[,11],err1[,12],
                err1[,13],err1[,14],err1[,15],err1[,16],err1[,17],err1[,18],err1[,19],err1[,20],err1[,21],
                err1[,22],err1[,23],err1[,24],err1[,25],err1[,26],err1[,27],err1[,28],err1[,29],err1[,30],
                err1[,31])
}
############################################################
vplot30 <- function(err1)
{
  par(mfrow=c(1,1))
  vioplot(err1[,1],err1[,2],err1[,3],err1[,4],err1[,5],err1[,6],err1[,7],err1[,8],err1[,9],err1[,10],err1[,11],err1[,12],
          err1[,13],err1[,14],err1[,15],err1[,16],err1[,17],err1[,18],err1[,19],err1[,20],err1[,21],
          err1[,22],err1[,23],err1[,24],err1[,25],err1[,26],err1[,27],err1[,28],err1[,29],err1[,30])
}
#####################################################################
vplot28 <- function(err1)
{
  par(mfrow=c(1,1))
  vioplot(err1[,1],err1[,2],err1[,3],err1[,4],err1[,5],err1[,6],err1[,7],err1[,8],err1[,9],err1[,10],err1[,11],err1[,12],
          err1[,13],err1[,14],err1[,15],err1[,16],err1[,17],err1[,18],err1[,19],err1[,20],err1[,21],
          err1[,22],err1[,23],err1[,24],err1[,25],err1[,26],err1[,27],err1[,28])
}
#####################################################################
vplot29 <- function(err1)
{
  par(mfrow=c(1,1))
  vioplot(err1[,1],err1[,2],err1[,3],err1[,4],err1[,5],err1[,6],err1[,7],err1[,8],err1[,9],err1[,10],err1[,11],err1[,12],
          err1[,13],err1[,14],err1[,15],err1[,16],err1[,17],err1[,18],err1[,19],err1[,20],err1[,21],
          err1[,22],err1[,23],err1[,24],err1[,25],err1[,26],err1[,27],err1[,28],err1[,29])
}
####################################################################
get_distributions_error <- function(df,bVerbose,name,month,year,prof)
  ##month = GENNAIO, FEBBRAIO, MARZO,... ecc
{
  file <- paste0("C:/Users/d_floriello/Documents/error_distr/curves_",month,"_",year,".xlsx") 
  xlsx::write.xlsx(df, file, row.names = TRUE, col.names = TRUE)
  
  clust <- plot_clusters_mean(df,3,1,3,name,plot=bVerbose)
  mott1 <- extract_cons(ver,clust[[1]])
  mott2 <- extract_cons(ver,clust[[2]])
  mott3 <- extract_cons(ver,clust[[3]])
  
  p1 <- unlist(rownames(mott1))
  #cott1m <- compute_mean(df, p1)
  cott1m <- df[which(rownames(df)%in%p1),]
  p2 <- unlist(rownames(mott2))
  #cott2m <- compute_mean(df, p2)
  cott2m <- df[which(rownames(df)%in%p2),]
  p3 <- unlist(rownames(mott3))
  #cott3m <- compute_mean(df, p3)
  cott3m <- df[which(rownames(df)%in%p3),]
  
  file1 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster1_mean_curve_pre_",month,"_",year,".xlsx") 
  file2 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster2_mean_curve_pre_",month,"_",year,".xlsx") 
  file3 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster3_mean_curve_pre_",month,"_",year,".xlsx") 
  xlsx::write.xlsx(data.frame(colMeans(cott1m)), file1, row.names = FALSE, col.names = TRUE)
  xlsx::write.xlsx(data.frame(colMeans(cott2m)), file2, row.names = FALSE, col.names = TRUE)
  xlsx::write.xlsx(data.frame(colMeans(cott3m)), file3, row.names = FALSE, col.names = TRUE)
  
  mott1 <- mott1[which(mott1[,2] != "0"),]
  mott2 <- mott2[which(mott2[,2] != "0"),]
  mott3 <- mott3[which(mott3[,2] != "0"),]
  
  pi1 <- intersect(p1, rownames(mott1))
  pi2 <- intersect(p2, rownames(mott2))
  pi3 <- intersect(p3, rownames(mott3))
  
  mott1 <- mott1[which(rownames(mott1) %in% pi1),]
  mott2 <- mott2[which(rownames(mott2) %in% pi2),]
  mott3 <- mott3[which(rownames(mott3) %in% pi3),]
  
  sott1 <- tc_emc(mott1, month, year,prof)
  sott2 <- tc_emc(mott2, month, year,prof)
  sott3 <- tc_emc(mott3, month, year,prof)
  
  if(month == "FEBBRAIO" & year != "2016")
  {
    sott1 <- sott1[,1:28]
    sott2 <- sott2[,1:28]
    sott3 <- sott3[,1:28]
  }
  
  cott1m <- data.frame(cott1m)
  colnames(cott1m) <- colnames(df)
  cott2m <- data.frame(cott2m)
  colnames(cott2m) <- colnames(df)
  cott3m <- data.frame(cott3m)
  colnames(cott3m) <- colnames(df)
  
  cott1m <- cott1m[rownames(cott1m) %in% pi1,]
  cott2m <- cott2m[which(rownames(cott2m) %in% pi2),]
  cott3m <- cott3m[rownames(cott3m) %in% pi3,]
  
  if(nrow(mott1) == 0) cott1m <- sott1 <- rbind(rep(2500, ncol(df)),rep(2500, ncol(df)))
  if(nrow(mott2) == 0) cott2m <- sott2 <- rbind(rep(2500, ncol(df)),rep(2500, ncol(df)))
  if(nrow(mott3) == 0) cott3m <- sott3 <- rbind(rep(2500, ncol(df)),rep(2500, ncol(df)))
  
  m1m <- colMeans(cott1m)
  m2m <- colMeans(cott2m)
  m3m <- colMeans(cott3m)
  
  m1m <- data.frame(t(m1m))
  m2m <- data.frame(t(m2m))
  m3m <- data.frame(t(m3m))
  
  colnames(m1m) <- colnames(df)
  colnames(m2m) <- colnames(df)
  colnames(m3m) <- colnames(df)
  
  ## una di queste e' quella finta
  file1 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster1_mean_curve_post_",month,"_",year,".xlsx") 
  file2 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster2_mean_curve_post_",month,"_",year,".xlsx") 
  file3 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster3_mean_curve_post_",month,"_",year,".xlsx") 
  xlsx::write.xlsx(m1m, file1, row.names = FALSE, col.names = TRUE)
  xlsx::write.xlsx(m2m, file2, row.names = FALSE, col.names = TRUE)
  xlsx::write.xlsx(m3m, file3, row.names = FALSE, col.names = TRUE)
  
  cott1mu <- normalise_curves(m1m)
  cott2mu <- normalise_curves(m2m)
  cott3mu <- normalise_curves(m3m)
  cott1mu <- data.frame(cott1mu)
  print(dim(cott1mu))
  colnames(cott1mu) <- colnames(df)
  cott2mu <- data.frame(cott2mu)
  print(dim(cott2mu))
  colnames(cott2mu) <- colnames(df)
  cott3mu <- data.frame(cott3mu)
  colnames(cott3mu) <- colnames(df)
  print(dim(cott3mu))
  
  file1 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster1_mean_curve_post_unit_",month,"_",year,".xlsx") 
  file2 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster2_mean_curve_post_unit_",month,"_",year,".xlsx") 
  file3 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster3_mean_curve_post_unit_",month,"_",year,".xlsx") 
  xlsx::write.xlsx(cott1mu, file1,row.names = FALSE, col.names = TRUE)
  xlsx::write.xlsx(cott2mu, file2, row.names = FALSE, col.names = TRUE)
  xlsx::write.xlsx(cott3mu, file3,row.names = FALSE, col.names = TRUE)
  
  sott1u <- normalise_curves(sott1)
  sott2u <- normalise_curves(sott2)
  sott3u <- normalise_curves(sott3)
  
  u1m <- normalise_curves(cott1m)
  u2m <- normalise_curves(cott2m)
  u3m <- normalise_curves(cott3m)
  
  print(paste("dim cott1m =", dim(cott1m)))
  print(paste("dim sott1u =", dim(sott1u)))
  err1 <- ddistribution_error(cott1mu, sott1u)
  print(paste("dim cott2m =", dim(cott2m)))
  print(paste("dim sott2u =", dim(sott2u)))
  err2 <- ddistribution_error(cott2mu, sott2u)
  print(paste("dim cott3m =", dim(cott3m)))
  print(paste("dim sott3u =", dim(sott3u)))
  err3 <- ddistribution_error(cott3mu, sott3u)
  
  int1 <- intersect(rownames(sott1u), rownames(u1m))
  int2 <- intersect(rownames(sott2u), rownames(u2m))
  int3 <- intersect(rownames(sott3u), rownames(u3m))
  
  if(length(rownames(u1m)) > 0) {err1m <- ddistribution_error_rt(u1m[rownames(u1m) %in% int1,], sott1u[rownames(sott1u) %in% int1,])}
  else {err1m <- rep(0, length(colnames(sott1u)))}
  if(length(rownames(u2m)) > 0) {err2m <- ddistribution_error_rt(u2m[rownames(u2m) %in% int2,], sott2u[rownames(sott2u) %in% int2,])}
  else {err2m <- rep(0, length(colnames(sott2u)))}
  if(length(rownames(u3m)) > 0) {err3m <- ddistribution_error_rt(u3m[rownames(u3m) %in% int3,], sott3u[rownames(sott3u) %in% int3,])}
  else {err3m <- rep(0, length(colnames(sott3u)))}
  
  month2 <- map_months(month)

  if(!bVerbose & month%in%c(1,3,5,7,8,10,12)) {vplot31(err1); vplot31(err2); vplot31(err3)}
  else if(!bVerbose & month2%in%c(4, 6, 9, 11)) {vplot30(err1); vplot30(err2); vplot30(err3)}
  else if(!bVerbose & month2%in%c(2) & as.numeric(year) %% 4 == 0) {vplot29(err1); vplot29(err2); vplot29(err3)}
  else if(!bVerbose & month2%in%c(2) & as.numeric(year) %% 4 != 0) {vplot28(err1); vplot28(err2); vplot28(err3)}
  
  # err1 <- data.frame(err1)
  # colnames(err1) <- colnames(df)
  # rownames(err1) <- rownames(cott1m)
  # err2 <- data.frame(err2)
  # colnames(err2) <- colnames(df)
  # rownames(err2) <- rownames(cott2m)
  # err3 <- data.frame(err3)
  # colnames(err3) <- colnames(df)
  # rownames(err3) <- rownames(cott3m)
  # 
  # err1m <- as.data.frame(err1m)
  # err2m <- data.frame(err2m)
  # err3m <- data.frame(err3m)
  # 
 
  
  file1 <- paste0("C:/Users/d_floriello/Documents/error_distr/",month,"_",year,"_cluster_1_mean_vs_theoretical.xlsx")
  file2 <- paste0("C:/Users/d_floriello/Documents/error_distr/",month,"_",year,"_cluster_2_mean_vs_theoretical.xlsx")
  file3 <- paste0("C:/Users/d_floriello/Documents/error_distr/",month,"_",year,"_cluster_3_mean_vs_theoretical.xlsx")
  xlsx::write.xlsx(err1m, file1,row.names = TRUE, col.names = TRUE)
  xlsx::write.xlsx(err2m, file2, row.names = TRUE, col.names = TRUE)
  xlsx::write.xlsx(err3m, file3,  row.names = TRUE, col.names = TRUE)
  print("qui")
  file1 <- paste0("C:/Users/d_floriello/Documents/error_distr/",month,"_",year,"_cluster_1.csv")
  file2 <- paste0("C:/Users/d_floriello/Documents/error_distr/",month,"_",year,"_cluster_2.csv")
  file3 <- paste0("C:/Users/d_floriello/Documents/error_distr/",month,"_",year,"_cluster_3.csv")
  # xlsx::write.xlsx(err1, file1,row.names = TRUE, col.names = TRUE)
  # xlsx::write.xlsx(err2, file2, row.names = TRUE, col.names = TRUE)
  # xlsx::write.xlsx(err3, file3,  row.names = TRUE, col.names = TRUE)
  write.table(err1, file1, sep=",", row.names = TRUE, col.names = TRUE)
  write.table(err2, file2, sep=",", row.names = TRUE, col.names = TRUE)
  write.table(err3, file3, sep=",", row.names = TRUE, col.names = TRUE)
  
}
#######################################################################
map_months <- function(month)
{
  x <- c("gennaio","febbraio","marzo","aprile","maggio","giugno","luglio","agosto","settembre","ottobre","novembre","dicembre")
  y <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  if(is.character(month))
    return(y[which(toupper(x) == month)])
  else 
    return(which(toupper(x) == month))
}
###################################################################
take_profile_cons <- function(ao, pdr, month, year)
{
  cons <- 0
  cmon <- paste0("01/",map_months(month),year)
  pa <- ao[which(ao["PDR"] == pdr),]
  rows <- c()
  for(i in 1:nrow(pa))
  {
    if(compare_dates(cmon, pa[i,"D_VALIDO_DAL_T"]) & compare_dates(cmon, pa[i,"D_VALIDO_AL_T"]))
      rows <- c(rows,i)
  }
  if(length(rows) > 1)
    rows <- max(rows)
  if(pa[rows,"CONSUMO_DISTRIBUTORE"] != "0")
    cons <- pa[rows,"CONSUMO_DISTRIBUTORE"]
  else cons <- pa[rows,"CONS_CONTR_ANNUO"]
  profilo <- pa[rows,"PROFILO_PRELIEVO"]
  return(c(cons, profilo))
}
###################################################################
get_month_year <- function(df)
{
  x <- c("gennaio","febbraio","marzo","aprile","maggio","giugno","luglio","agosto","settembre","ottobre","novembre","dicembre")
  mes <- stri_sub(df,from = 2, to = 4)
  ann <- stri_sub(df,from = 5, to = 6)
  X <- toupper(x[grep(mes,x)])
  Y <- 2016
  if(ann != "") Y <- paste0("20",ann)
  return(c(X,Y))
}
#################################################################
compute_integral <- function(curve)
{
  argvals <- seq(0,1,length.out = length(curve))
  area <- 0
  for(i in 1:(length(curve)-1))
  {
    #area <- area + ((curve[i+1]+curve[i])*(argvals[i+1] - argvals[i])/2)
    area <- area + ((curve[i+1]+curve[i])*(1/2))
  }
  return(area)
}
######################################################################
normalise_curves <- function(mat)
{
  matn <- matrix(0, nrow=nrow(mat),ncol=ncol(mat))
  matn <- data.frame(matn)
  rownames(matn) <- rownames(mat)
  colnames(matn) <- colnames(mat)
  for(i in 1:nrow(mat))
  {
    if(sum(mat[i,]) > 0) matn[i,] <- unlist(mat[i,])/compute_integral(unlist(mat[i,]))
  }
  row_sub <- apply(matn, 1, function(row) sum(row) > 0 )
  matn <- matn[row_sub,]
  return(matn)
}
######################################################################
assign_to_cluster <- function(curve, month, year)
{
  # curve is normalised
  #month <- current_month
  #year <- as.numeric(current_year) - 1
  file1 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster1_mean_curve_pre_",month,"_",year,".xlsx") 
  file2 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster2_mean_curve_pre_",month,"_",year,".xlsx") 
  file3 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster3_mean_curve_pre_",month,"_",year,".xlsx") 
  if(file.exists(file1) | file.exists(file2) | file.exists(file3))
  {
    me1 <- openxlsx::read.xlsx(file1, sheet = 1, rowNames = FALSE, colNames = TRUE)
    me2 <- openxlsx::read.xlsx(file2, sheet = 1, rowNames = FALSE, colNames = TRUE)
    me3 <- openxlsx::read.xlsx(file3, sheet = 1, rowNames = FALSE, colNames = TRUE)
    diff1 <- mean((as.numeric(unlist(curve)) - as.numeric(unlist(me1)))^2)
    diff2 <- mean((as.numeric(unlist(curve)) - as.numeric(unlist(me2)))^2)
    diff3 <- mean((as.numeric(unlist(curve)) - as.numeric(unlist(me3)))^2)
    vd <- c(diff1,diff2,diff3)
    
    return(c(which.min(vd),get(paste0("me",which.min(vd)))))
  }
  else return(curve)
}
#####################################################################
assign_to_cluster_SDE <- function(curve, month, year)
{
  # curve is normalised
  #month <- current_month
  #year <- as.numeric(current_year) - 1
  file1 <- paste0("C:/Users/d_floriello/Documents/SDE_mod/cluster1_mean_curve_pre_",month,"_",year,".xlsx") 
  file2 <- paste0("C:/Users/d_floriello/Documents/SDE_mod/cluster2_mean_curve_pre_",month,"_",year,".xlsx") 
  file3 <- paste0("C:/Users/d_floriello/Documents/SDE_mod/cluster3_mean_curve_pre_",month,"_",year,".xlsx") 
  if(file.exists(file1) | file.exists(file2) | file.exists(file3))
  {
    me1 <- openxlsx::read.xlsx(file1, sheet = 1, rowNames = FALSE, colNames = TRUE)
    me2 <- openxlsx::read.xlsx(file2, sheet = 1, rowNames = FALSE, colNames = TRUE)
    me3 <- openxlsx::read.xlsx(file3, sheet = 1, rowNames = FALSE, colNames = TRUE)
    diff1 <- mean((as.numeric(unlist(curve)) - as.numeric(unlist(me1)))^2)
    diff2 <- mean((as.numeric(unlist(curve)) - as.numeric(unlist(me2)))^2)
    diff3 <- mean((as.numeric(unlist(curve)) - as.numeric(unlist(me3)))^2)
    vd <- c(diff1,diff2,diff3)
    
    return(c(which.min(vd),get(paste0("me",which.min(vd)))))
  }
  else return(curve)
}
##########################################################################
fci <- function(vm, errs, B = 1000)
{
  ups <- lows <- rep(0,ncol(errs)-1)
  for(n in 2:ncol(errs))
  {
    bs <- data.matrix(maply(1:100, function(i) sample(as.numeric(errs[,n]), B, replace = TRUE)))
    boots <- vm[n-1] - rowMeans(bs)
    ups[n-1] <- vm[n-1] + quantile(boots,probs=0.975)
    lows[n-1] <- vm[n-1] - quantile(boots,probs=0.025)
  }
  return(rbind(ups, vm, lows))
}
##########################################################################
fcis <- function(vm, err, B = 1000)
{
  ups <- lows <- 0
  bs <- maply(1:100, function(i) sample(as.numeric(err), B, replace = TRUE))
  boots <- vm - rowMeans(bs)
  ups <- vm + quantile(boots,probs=0.975)
  lows <- vm - quantile(boots,probs=0.025)
  return(rbind(ups, vm, lows))
}
#####################################################################
forecast_consumption <- function(curve, pdr, cons, month, year)
{
  if(month == "MAGGIO" & year == 2014) month <- "GIUGNO" ## or year <- 2015??
  file1 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster1_mean_curve_pre_",month,"_",year,".xlsx") 
  file2 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster2_mean_curve_pre_",month,"_",year,".xlsx") 
  file3 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster3_mean_curve_pre_",month,"_",year,".xlsx") 
  clu1 <- openxlsx::read.xlsx(file1, sheet = 1, colNames = TRUE)
  clu2 <- openxlsx::read.xlsx(file2, sheet = 1, colNames = TRUE)
  clu3 <- openxlsx::read.xlsx(file3, sheet = 1, colNames = TRUE)
  file <- paste0("C:/Users/d_floriello/Documents/error_distr/curves_",month,"_",year,".xlsx") 
  #print(file1)
  #print(file2)
  #print(file3)
  #print(file)
  if(file.exists(file))
  {
    mcc <- openxlsx::read.xlsx(file,sheet = 1, colNames = TRUE)
    rownames(mcc) <- mcc[,1]
    mcc <- mcc[,2:ncol(mcc)]
    if(pdr %in% rownames(mcc))
    {
      prev <- as.numeric(mcc[which(rownames(mcc) == pdr),])
      exp_val <- mean(as.numeric(prev))
      diff1 <- mean((as.numeric(unlist(prev)) - as.numeric(unlist(clu1)))^2)
      diff2 <- mean((as.numeric(unlist(prev)) - as.numeric(unlist(clu2)))^2)
      diff3 <- mean((as.numeric(unlist(prev)) - as.numeric(unlist(clu3)))^2)
      vd <- c(diff1,diff2,diff3)
      cluster <- which.min(vd)
      m1 <- mean(as.numeric(unlist(clu1)))
      m2 <- mean(as.numeric(unlist(clu2)))
      m3 <- mean(as.numeric(unlist(clu3)))
      vm <- c(m1, m2, m3)
      cluster_assigned <- which(vm ==vm[cluster])
      vmmin <- which.min(vm)
      vmmax <- which.max(vm)
      if(cluster_assigned == vmmin)
      {
        trend <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/error_distr/diff_diff_civ.xlsx", sheet = 1, colNames = TRUE)
        mesi <- ttm(as.numeric(map_months(month)))
        if(as.numeric(year) %% 2 == 0 & month != "MAGGIO") mesi <- mesi[which( mesi < 8 | mesi > 19)]
        else if(as.numeric(year) %% 2 == 0 & month == "MAGGIO") mesi <- 1
        else mesi <- mesi[which( mesi >= 8 | mesi <= 19)]
        prev <- prev + as.numeric(trend[mesi,2])/length(unlist(clu1))
      }
      else if (cluster_assigned == vmmax)
      {
        trend <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/error_distr/diff_diff_ind.xlsx", sheet = 1, colNames = TRUE)
        mesi <- ttm(as.numeric(map_months(month)))
        if(as.numeric(year) %% 2 == 0 & month != "MAGGIO") mesi <- mesi[which( mesi < 8 | mesi > 19)]
        else if(as.numeric(year) %% 2 == 0 & month == "MAGGIO") mesi <- 1
        else mesi <- mesi[which( mesi >= 8 | mesi <= 19)]
        prev <- prev + as.numeric(trend[mesi,2])/length(unlist(clu1))
      }
      else
      {
        trend <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/error_distr/diff_diff_medi.xlsx", sheet = 1, colNames = TRUE)
        mesi <- ttm(as.numeric(map_months(month)))
        if(as.numeric(year) %% 2 == 0 & month != "MAGGIO") mesi <- mesi[which( mesi < 8 | mesi > 19)]
        else if(as.numeric(year) %% 2 == 0 & month == "MAGGIO") mesi <- 1
        else mesi <- mesi[which( mesi >= 8 | mesi <= 19)]
        prev <- prev + as.numeric(trend[mesi,2])/length(unlist(clu1))
      }
      err <- openxlsx::read.xlsx(paste0("C:/Users/d_floriello/Documents/error_distr/",month,"_",year,"_cluster_",cluster_assigned,"_mean_vs_theoretical.xlsx"),colNames = TRUE)
      
    }
    else
    {
      prev_e <- unlist(assign_to_cluster(unlist(curve), month, year))
      kbar <- get(paste0("clu",prev_e[1]))
      Zbar <- compute_integral(as.numeric(unlist(kbar)))
      prev <- (cons/Zbar)*unlist(prev_e[2:length(prev_e)])
      
      err <- openxlsx::read.xlsx(paste0("C:/Users/d_floriello/Documents/error_distr/",month,"_",year,"_cluster_",prev_e[1],"_mean_vs_theoretical.xlsx"),colNames = TRUE)
      
      m1 <- mean(as.numeric(unlist(clu1)))
      m2 <- mean(as.numeric(unlist(clu2)))
      m3 <- mean(as.numeric(unlist(clu3)))
      vm <- c(m1, m2, m3)
      vmmin <- which.min(vm)
      vmmax <- which.max(vm)
      if(prev_e[1] == vmmin)
      {
        trend <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/error_distr/diff_diff_civ.xlsx", sheet = 1, colNames = TRUE)
        mesi <- ttm(as.numeric(map_months(month)))
        if(as.numeric(year) %% 2 == 0 & month != "MAGGIO") mesi <- mesi[which( mesi < 8 | mesi > 19)]
        else if(as.numeric(year) %% 2 == 0 & month == "MAGGIO") mesi <- 1
        else mesi <- mesi[which( mesi >= 8 | mesi <= 19)]
        prev <- prev - as.numeric(trend[mesi,2])/length(unlist(clu1))
      }
      else if (prev_e[1] == vmmax)
      {
        trend <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/error_distr/diff_diff_ind.xlsx", sheet = 1, colNames = TRUE)
        mesi <- ttm(as.numeric(map_months(month)))
        if(as.numeric(year) %% 2 == 0 & month != "MAGGIO") mesi <- mesi[which( mesi < 8 | mesi > 19)]
        else if(as.numeric(year) %% 2 == 0 & month == "MAGGIO") mesi <- 1
        else mesi <- mesi[which( mesi >= 8 | mesi <= 19)]
        prev <- prev - as.numeric(trend[mesi,2])/length(unlist(clu1))
      }
      else
      {
        trend <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/error_distr/diff_diff_medi.xlsx", sheet = 1, colNames = TRUE)
        mesi <- ttm(as.numeric(map_months(month)))
        if(as.numeric(year) %% 2 == 0 & month != "MAGGIO") mesi <- mesi[which( mesi < 8 | mesi > 19)]
        else if(as.numeric(year) %% 2 == 0 & month == "MAGGIO") mesi <- 1
        else mesi <- mesi[which( mesi >= 8 | mesi <= 19)]
        prev <- prev - as.numeric(trend[mesi,2])/length(unlist(clu1))
      }
    }
    
    prev[prev < 0] <- 0
    interval <- fci(prev, err)
    #print(prev)
    return(rbind(prev, interval[1,], interval[3,]))
  }
  
  else
  {
    return(curve) #+ trend[mesi,1]/ncol(clu1) + trend[mesi,2]/ncol(clu1))
  }
}
#################################################################
tc_emc <- function(mott, month,year,prof)
{
  out <- tryCatch(
    {expected_monthly_consumption(mott, month, year,prof)},
    error = function(cond) { return(1)}
  )
  return(out)
}
################################################################
ttm <- function(n)
{
  if(n == 1) return(c(8,20))
  else if(n == 2) return(c(9,21))
  else if(n == 3) return(c(10,22))
  else if(n == 4) return(c(11,23))
  else if(n == 5) return(12)
  else if(n == 6) return(c(1,13))
  else if(n == 7) return(c(2,14))
  else if(n == 8) return(c(3,15))
  else if(n == 9) return(c(4,16))
  else if(n == 10) return(c(5,17))
  else if(n == 11) return(c(6,18))
  else return(c(7,19))
}
################################################################
correct_obs <- function(df)
{
  for(i in 1:nrow(df))
  {
    for(j in 1:ncol(df))
    {
      if(df[i, j] > 6000) df[i,j] <- (1/2)*(df[i,j-1] + df[i,j+1])
    }
  }
  return(df)
}
################################################################
plot_errors <- function(clu_mean, mat, month, year,cluster)
{
  for(i in 1:length(clu_mean))
  {
    err_vec <- clu_mean[i] - mat[,i]^3
    #areas <- maply(1:nrow(mat), function(n) compute_integral(unlist(mat[n,])))
    areas <- maply(1:nrow(mat), function(n) sum(unlist(mat[n,])))
    plot(areas, err_vec,main=paste("error vs area giorno", i, month, year))
  }
}
################################################################
estimate_models <- function(clu_mean, mat, matv, month, year,cluster)
{
  for(i in 1:length(clu_mean))
  {
    err_vec <- clu_mean[i] - mat[,i]
    err_ver <- clu_mean[i] - matv[,i]
    #areas <- maply(1:nrow(mat), function(n) compute_integral(unlist(mat[n,])))
    areas <- maply(1:nrow(mat), function(n) sum(unlist(mat[n,])))
    fit <- lm(err_ver ~ areas + err_vec)
    adjRsq <- 1 - (sum(fit$residuals^2)/length(err_ver) - 3)/(sum((err_ver - mean(err_ver))^2)/length(err_ver) - 1)
    print(paste("R2-adj:", adjRsq))
    file <- paste0("C:/Users/d_floriello/Documents/SDE_mod/modello_",month,"_",year,"_giorno_",i,"_cluster_",cluster,".rds")
    saveRDS(fit, file)
  }
}
##############################################################
estimate_rotated_models <- function(clu_mean, mat, matv, month, year,cluster)
{
#  R <- matrix(0,2,2)
#  R[1,1] <- R[2,2] <- cos(-pi/3); R[2,1] <- sin(-pi/3); R[1,2] <- -sin(-pi/3) 
  for(i in 1:length(clu_mean))
  {
    err_vec <- clu_mean[i] - mat[,i]
    err_ver <- clu_mean[i] - matv[,i]
    #areas <- maply(1:nrow(mat), function(n) compute_integral(unlist(mat[n,])))
    areas <- maply(1:nrow(mat), function(n) sum(unlist(mat[n,])))
    data <- data.frame(cbind(verr=err_ver,err=err_vec,area=areas))
    # Rdata <- c()
    # for(j in 1:nrow(data))
    # {
    #   Rdata <- rbind(Rdata, t(R%*%data[j,]))
    # }
    fit <- randomForest(err_ver ~ areas + err_vec)
    adjRsq <- 1 - (sum(fit$residuals^2)/length(err_ver) - 3)/(sum((err_ver - mean(err_ver))^2)/length(err_ver) - 1)
    print(paste("R2-adj:", adjRsq))
    file <- paste0("C:/Users/d_floriello/Documents/SDE_mod/rot_modello_",month,"_",year,"_giorno_",i,"_cluster_",cluster,".rds")
    saveRDS(fit, file)
  }
}
##################################################################
forecast_consumption_via_SDE <- function(curve, month, year)
{
  area <- compute_integral(unlist(curve))
  pre <- assign_to_cluster_SDE(curve,month,year)
  cluster <- pre[1]
  prediction <- unlist(pre[2:length(pre)])
  ups <- lows <- rep(0, ncol(curve))
  for(i in 1:ncol(curve))
  {
    err <- unlist(prediction[i]) - unlist(curve[i])
    print(paste0("C:/Users/d_floriello/Documents/SDE_mod/modello_",month,"_",year,"_giorno_",i,"_cluster_",cluster,".rds"))
    fit <- readRDS(paste0("C:/Users/d_floriello/Documents/SDE_mod/modello_",month,"_",year,"_giorno_",i,"_cluster_",cluster,".rds"))
    prediction[i] <- as.numeric(prediction[i]) #+ predict(fit, data.frame(areas=area,err_vec=err))
    if(prediction[i] < 0) prediction[i] <- 0
    ci <- fcis(prediction[i], fit$residuals)
    ups[i] <- ci[1]; lows[i] <- ci[3]
  }
  return(rbind(prediction, ups, lows))
}
#################################################################
forecast_consumption_via_SDE_rotated <- function(curve, month, year)
{
  # R <- matrix(0,2,2)
  # R[1,1] <- R[2,2] <- cos(-pi/3); R[2,1] <- sin(-pi/3); R[1,2] <- -sin(-pi/3) 
  #area <- compute_integral(unlist(curve))
  area <- sum(unlist(curve))
  # data <- cbind(area, area)
  # Rdata <- c()
  # for(j in 1:nrow(data))
  # {
  #   Rdata <- rbind(Rdata, t(R%*%data[j,]))
  # }
  pre <- assign_to_cluster_SDE(curve,month,year)
  cluster <- pre[1]
  prediction <- unlist(pre[2:length(pre)])
  ups <- lows <- rep(0, ncol(curve))
  for(i in 1:ncol(curve))
  {
    err <- unlist(prediction[i]) - unlist(curve[i])
    print(paste0("C:/Users/d_floriello/Documents/SDE_mod/rot_modello_",month,"_",year,"_giorno_",i,"_cluster_",cluster,".rds"))
    fit <- readRDS(paste0("C:/Users/d_floriello/Documents/SDE_mod/rot_modello_",month,"_",year,"_giorno_",i,"_cluster_",cluster,".rds"))
    prediction[i] <- as.numeric(prediction[i]) - predict(fit, data.frame(areas=area,err_vec=err))
    if(prediction[i] < 0) prediction[i] <- 0
    ci <- fcis(prediction[i], fit$mse)
    ups[i] <- ci[1]; lows[i] <- ci[3]
  }
  return(rbind(prediction, ups, lows))
}
#######################################################
mode <- function(v)
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
######################################################
V_shaped <- function(err_vec, areas)
{
  bool <- FALSE
  index <- which.min(areas)
  q <- err_vec[index]
  mod <- mode(err_vec)
  
}
######################################################
take_max_permonth <- function(prof_consumo, COL)
{
  dates <- c(paste0("01/","0",1:9,"/",2015), paste0("01/",10:12,"/",2015), paste0("01/","0",1:9,"/",2016), paste0("01/",10:12,"/",2016))
  vim <- rep(0, length(dates))
  for(i in 1:length(dates))
  {
    month <- stri_sub(dates[i], from = 4, to = 5)
    year <- stri_sub(dates[i], from = 7, to = 10)
    
    vm <- data.frame(t(unlist(prof_consumo[,COL])))
    names(vm) <- prof_consumo[,1]
    indeces <- take_month(month, year, vm)
    vim[i] <- max(prof_consumo[indeces, COL])
  }
  return(vim)
}
######################################################
take_sum_permonth <- function(prof_consumo, COL)
{
  dates <- c(paste0("01/","0",1:9,"/",2015), paste0("01/",10:12,"/",2015), paste0("01/","0",1:9,"/",2016), paste0("01/",10:12,"/",2016))
  vim <- rep(0, length(dates))
  for(i in 1:length(dates))
  {
    month <- stri_sub(dates[i], from = 4, to = 5)
    year <- stri_sub(dates[i], from = 7, to = 10)
    
    vm <- data.frame(t(unlist(prof_consumo[,COL])))
    names(vm) <- prof_consumo[,1]
    indeces <- take_month(month, year, vm)
    vim[i] <- sum(prof_consumo[indeces, COL])
  }
  return(vim)
}
#####################################################
active <- function(di, df)
{
  dates <- c(paste0("01/","0",1:9,"/",2015), paste0("01/",10:12,"/",2015), paste0("01/","0",1:9,"/",2016), paste0("01/",10:12,"/",2016))
  act <- rep(0, 24)
  for(i in 1:24)
  {
    if(compare_dates(dates[i], di) & compare_dates(df, dates[i])) act[i] <- 1
  }
  return(act)
}
####################################################
compute_max_prof <- function(M, prof_consumo)
{
  data_in_y <- unlist(M[2])
  data_fin_y <- unlist(M[3])
  prof_y <- unlist(M[4])
  cons_y <- as.numeric(as.character(unlist(M[5])))
  COL <- which(names(prof_consumo) == prof_y)
  mpm <- take_max_permonth(prof_consumo,COL)
  spm <- take_sum_permonth(prof_consumo,COL)
  rcpm <- mpm/spm
  act <- active(data_in_y, data_fin_y)
  res <- cons_y * mpm/100 * act
  return(res)
}
######################################################################
compute_max_prof_riconciliazione <- function(M, prof_consumo)
{
  data_in_y <- unlist(M[2])
  data_fin_y <- unlist(M[3])
  prof_y <- unlist(M[4])
  cons_y <- as.numeric(as.character(unlist(M[5])))
  COL <- which(names(prof_consumo) == prof_y)
  mpm <- take_max_permonth(prof_consumo,COL)
  spm <- take_sum_permonth(prof_consumo,COL)
  rcpm <- mpm/spm
  act <- active(data_in_y, data_fin_y)
  res <- cons_y * rcpm * act
  return(res)
}
########################################################################
compute_max_prof_hp <- function(M, prof_consumo, snam, sum_remi)
{
  data_in_y <- unlist(M[2])
  data_fin_y <- unlist(M[3])
  prof_y <- unlist(M[4])
  cons_y <- as.numeric(as.character(unlist(M[5])))
  COL <- which(names(prof_consumo) == prof_y)
  mpm <- take_max_permonth(prof_consumo,COL)
  spm <- take_sum_permonth(prof_consumo,COL)
  rcpm <- mpm/spm
  act <- active(data_in_y, data_fin_y)
  CM <- (snam * c(31,29,31,30,31,30,31,31,30,31,30,31,31,29,31,30,31,30,31,31,30,31,30,31)/1.3)
  res <- sum(CM[3:14] * (cons_y/sum_remi)) * mpm * act
  return(res)
}
######################################################################
compute_max_prof_riconciliazione_hp <- function(M, prof_consumo,snam,sum_remi)
{
  data_in_y <- unlist(M[2])
  data_fin_y <- unlist(M[3])
  prof_y <- unlist(M[4])
  cons_y <- as.numeric(as.character(unlist(M[5])))
  COL <- which(names(prof_consumo) == prof_y)
  mpm <- take_max_permonth(prof_consumo,COL)
  spm <- take_sum_permonth(prof_consumo,COL)
  rcpm <- mpm/spm
  act <- active(data_in_y, data_fin_y)
  CM <- (snam * c(31,29,31,30,31,30,31,31,30,31,30,31,31,29,31,30,31,30,31,31,30,31,30,31)/1.3)
  res <- CM * (cons_y/sum_remi) * rcpm * act
  
  
  return(res)
}
#########################################################################
compute_sum_remi <- function(re)
{
  sum_remi <- 0
  for(k in 1:nrow(re))
  {
    sum_remi <- sum_remi + ifelse(re[k,"CONSUMO_DISTRIBUTORE"] != "0", as.numeric(as.character(re[k,"CONSUMO_DISTRIBUTORE"])), as.numeric(as.character(re[k,"CONSUMO_CONTR_ANNUO"])))
  }
  return(sum_remi)
}
#####################################################################
return_num_days <- function(m)
{
  if(m %in% c(1,13,3,15,5,17,7,19,8,20,10,22,12,24)) return(31)
}
#####################################################################
Fhd2 <- function(M)
{
  #xx <- seq(0, 1, 0.001)
  xx <- seq(-1, 2, 0.001)
  ftot <- rep(0, length(xx))
  for(remi in rownames(M))
  {
    M3 <- M[which(rownames(M) == remi),c(6:24,30:48)]
    mm <- max(M3[1:18])
    f <- c()
    for(x in xx)
    {
      S <- 3*mm*(1+x) 
      #index <- which(xx == x)
      for(j in 1:19)
      {
        y <- mm*(1+x)
        z <- M3[j+19] - 1.1*y
        #if(z > 0.1*y)
        if(z > 0)
        {
          S <- S + 3.5*(abs(z))
        }
        else
        {
          S <- S + 0
        }
      }
      f <- c(f, S)
    }
    ftot <- ftot + f
  }
  return(ftot)
}
#####################################################################
cluster_by_and_optimise <- function(ver2, M, from, to)
{
  rl <- c()
  mat <- matrix(0,nrow=nrow(M),ncol=1)
  rownames(mat) <- rownames(M)
  for(i in 1:nrow(M))
  {
    len <- length(unique(unlist(ver2[which(ver["COD_REMI"] == rownames(M)[i]),"PDR"])))
    j <- which(rownames(mat) == rownames(M)[i])
    mat[j,1] <- len
    if(len >= from & len < to) rl <- c(rl, i)
  }
  Mloc <- M[rl,]
  xlsx::write.xlsx(data.frame(mat), paste0("C:/Users/d_floriello/Documents/plot_remi/num_pdr2", to,".xlsx"), row.names=TRUE, col.names = TRUE)
  opt <- Fhd2(Mloc)
  xx <- seq(-1, 2, 0.001)
  plot(xx,opt,type="l",lwd=2, col="red",xlab= "% banda di sicurezza", ylab="euro", main=paste("remi con numero pdr >", from, "e < di", to))
  return(opt)
}
####################################################################
Fhd <- function(remi, M2)
{
  xx <- seq(0, 1, 0.001)
  M3 <- M2[which(rownames(M2) == remi),c(6:24,30:48)]
  mm <- max(M3[1:18])
  f <- c()
  if(length(M3) > 0)
  {
    for(x in xx)
    {
      S <-  + 3*mm*(1+x)
      #index <- which(xx == x)
      for(j in 1:19)
      {
        y <- mm*(1+x)
        z <- M3[j+19] - 1.1*y
        #if(z > 0.1*y)
        if(z > 0)
        {
          S <- S + 3.5*(abs(z))
        }
        else
        {
          S <- S + 0 
        }
      }
      f <- c(f, S)
    }
  }
  plot(xx,f,type="l",lwd=2, col="red",xlab= "% banda di sicurezza", ylab="euro", main=rf)
  return(c(f,mm))
}
#######################################################################
compute_consumption <- function(ag,prof,n,str_cons)
{
  col <- which(colnames(prof) == ag[n,"PROFILO_PRELIEVO"])
  print(n);print(ag[n,"PDR"]);print(ag[n,"D_VALIDO_DAL_T"]);print(ag[n,"D_VALIDO_AL_T"]) 
  s <- sum(as.numeric(prof[,col]) * active(ag[n,"D_VALIDO_DAL_T"], ag[n,"D_VALIDO_AL_T"])[5:16] * as.numeric(ag[n,str_cons]))
  return(s)
}
######################################################################
comparison_data <- function(ag,prof)
{
  #ag <- extract_relevant_val(ag)
  remi <- unique(unlist(ag["COD_REMI"]))
  mat <- matrix(0,nrow=length(remi),ncol=26)
  rownames(mat) <- remi
  mesi <- c("gen", "feb", "mar", "apr", "mag", "giu", "lug", "ago", "set", "ott", "nov", "dic")
  nomi <- paste0("CONSUMO_", toupper(mesi))
  for(re in remi)
  {
    i <- which(rownames(mat) == re)
    ag2 <- extract_relevant_val(ag[which(ag["COD_REMI"] == re),])
    mat[i,1] <- sum(maply(1:nrow(ag2), function(n) compute_consumption(ag2,prof,n,"CONSUMO_CONTR_ANNUO")))
    #mat[i,1] <- sum(as.numeric(unlist(ag2["CONSUMO_CONTR_ANNUO"])))
    #mat[i,14] <- sum(as.numeric(unlist(ag2["CONSUMO_DISTRIBUTORE"])))
    mat[i,14] <- sum(maply(1:nrow(ag2), function(n) compute_consumption(ag2,prof,n,"CONSUMO_DISTRIBUTORE")))
    
    Cc <- Dc <- rep(0,12)
    for(j in 1:nrow(ag2))
    {
      col <- which(colnames(prof) == ag2[j,"PROFILO_PRELIEVO"])
      data_in_y <- ag2[j,"D_VALIDO_DAL_T"]
      data_fin_y <- ag2[j,"D_VALIDO_AL_T"]
      c2y <- active(data_in_y, data_fin_y) # second half because it's 2015/2016
      Cc <- Cc + as.numeric(prof[,col]) * as.numeric(ag2[j,"CONSUMO_CONTR_ANNUO"]) * c2y[5:16]
      Dc <- Dc + as.numeric(prof[,col]) * as.numeric(ag2[j,nomi]) * c2y[5:16]
    }
    mat[i,2:13] <- Cc
    mat[i,15:26] <- Dc
    
  }
  mat <- data.frame(mat)
  colnames(mat) <- c("consumo_contr", paste0("consumo_contr_",mesi),"consumo_distr", paste0("consumo_distr_",mesi))
  return(mat)
}
## http://www.senoecoseno.it/zahara-nilsson-la-svedese-che-ci-ha-fatto-perdere-la-vista-30-foto/30/
####################################################################
penali <- function(remi, M2, x=0)
{
  M <- M2[which(rownames(M2) == remi),c(5:16,29:40)]
  mm <- max(M[1:12])
  mm2 <- max(M[13:24])
  f <- c()
  if(mm > 0 & mm2 > 0)
  {  
    S <- 3*mm*(1+x)
    for(j in 1:12)
    {
      y <- mm*(1+x)
      z <- M[j+12] - 1.1*y
      if(z > 0)
      {
        S <- S + 3.5*(abs(z))
        f <- c(f, TRUE)
      }
      else
      {
        S <- S + 0 
        f <- c(f, FALSE)
      }
      #print(f)
    }
    
    #f <- c(f, S)
    return(f)  
  }
  else return(rep(0,12))
}
####################################################################
simple_optim <- function(remi, M)
{
  x <- 0
  Mo <- M[which(rownames(M) == remi),c(5:16,29:40)]
  m1 <- max(Mo[1:12])
  m2 <- max(Mo[13:24])
  if(m1 > 0 & m2 > 0) x <- (m2 - 1.1*m1)/m1
  if(x > 1) x <- 1
  return(x)
}
###################################################################
Fhdn <- function(remi, M)
{
  xx <- seq(-1, 1, 0.001)
  Mo <- M[which(rownames(M) == remi),c(5:16,29:40)]
  mm <- max(Mo[1:12])
  mm2 <- max(Mo[13:24])
  if(mm > 0 & mm2 > 0)
  {  
    f <- c()
    if(length(Mo) > 0)
    {
      for(x in xx)
      {
        S <- 3*mm*(1+x)
        for(j in 1:12)
        {
          y <- mm*(1+x)
          z <- Mo[j+12] - 1.1*y
          #if(z > 0.1*y)
          if(z > 0)
          {
            S <- S + 3.5*(abs(z))
          }
          else
          {
            S <- S + 0 
          }
        }
        f <- c(f, S)
      }
    }
  }
  else f <- rep(0,length(xx))
  plot(xx,f,type="l",lwd=2, col="red",xlab= "% banda di sicurezza", ylab="euro", main=rf)
  return(c(f,mm))
}
#####################################################
sel <- function(dat, expr) {
  eval(substitute(expr), dat)
}
#####################################################
sumprod_by_cluster <- function(Sol, j, npdr)
{
  res <- list()
   
  p10 <- which(as.numeric(npdr[,1]) <= 10)
  np10 <- rownames(npdr)[p10]
  sumprod <- 0
  S <- 0
  for(n in np10)
  {
    i <- which(rownames(Sol) == n)
    sumprod <- sumprod + as.numeric(Sol[i,j])*as.numeric(Sol[i,j+1])
    S <- S + as.numeric(Sol[i,j+1])
  }
  res[[paste0(j,"cluster 10")]] <- sumprod/S
  
  p10 <- which(as.numeric(npdr[,1]) >= 11 & as.numeric(npdr[,1]) <= 30)
  np10 <- rownames(npdr)[p10]
  sumprod <- 0
  S <- 0
  for(n in np10)
  {
    i <- which(rownames(Sol) == n)
    sumprod <- sumprod + as.numeric(Sol[i,j])*as.numeric(Sol[i,j+1])
    S <- S + as.numeric(Sol[i,j+1])
  }
  res[[paste0(j,"cluster 30")]] <- sumprod/S
  
  p10 <- which(as.numeric(npdr[,1]) > 30)
  np10 <- rownames(npdr)[p10]
  sumprod <- 0
  S <- 0
  for(n in np10)
  {
    i <- which(rownames(Sol) == n)
    sumprod <- sumprod + as.numeric(Sol[i,j])*as.numeric(Sol[i,j+1])
    S <- S + as.numeric(Sol[i,j+1])
  }
  res[[paste0(j,"cluster >30")]] <- sumprod/S
  
  return(res)
}

