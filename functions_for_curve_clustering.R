library(openxlsx)
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
  return(mat)
}
####################################################################  
expected_monthly_consumption <- function(mat, month, year, prof)
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
  mese <- take_month(month, year,res)
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
  return(unlist(mc) - unlist(tc[i,]))
}
##############################################################
ddistribution_error <- function(mean_curve, th_curve)
{
  mat <- maply(1:nrow(th_curve), function(i) sing_error(i, mean_curve, th_curve))
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
  file <- paste0("C:/Users/d_floriello/Documents/error_distr/curves_",month,"_",year,".csv") 
  write.table(df, file, sep =",",row.names = TRUE, col.names = TRUE)
  
  clust <- plot_clusters_mean(df,3,1,3,name,plot=bVerbose)
  mott1 <- extract_cons(ver,clust[[1]])
  mott2 <- extract_cons(ver,clust[[2]])
  mott3 <- extract_cons(ver,clust[[3]])
  
  sott1 <- expected_monthly_consumption(mott1, month, year,prof)
  sott2 <- expected_monthly_consumption(mott2, month, year,prof)
  sott3 <- expected_monthly_consumption(mott3, month, year,prof)
  
  p1 <- unlist(rownames(mott1))
  #cott1m <- compute_mean(df, p1)
  cott1m <- df[which(rownames(df)%in%p1),]
  p2 <- unlist(rownames(mott2))
  #cott2m <- compute_mean(df, p2)
  cott2m <- df[which(rownames(df)%in%p2),]
  p3 <- unlist(rownames(mott3))
  #cott3m <- compute_mean(df, p3)
  cott3m <- df[which(rownames(df)%in%p3),]
  
  cott1m <- data.frame(cott1m)
  colnames(cott1m) <- colnames(df)
  cott2m <- data.frame(cott2m)
  colnames(cott2m) <- colnames(df)
  cott3m <- data.frame(cott3m)
  colnames(cott3m) <- colnames(df)
  
  file1 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster1_mean_curve_",month,"_",year,".csv") 
  file2 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster2_mean_curve_",month,"_",year,".csv") 
  file3 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster3_mean_curve_",month,"_",year,".csv") 
  write.table(cott1m, file1, sep =",",row.names = FALSE, col.names = TRUE)
  write.table(cott2m, file2, sep =",", row.names = FALSE, col.names = TRUE)
  write.table(cott3m, file3, sep =",",row.names = FALSE, col.names = TRUE)
  
  cott1mu <- unlist(cott1m)/compute_integral(unlist(cott1m))
  cott2mu <- unlist(cott2m)/compute_integral(unlist(cott2m))
  cott3mu <- unlist(cott3m)/compute_integral(unlist(cott3m))
  cott1mu <- data.frame(cott1mu)
  colnames(cott1mu) <- colnames(df)
  cott2mu <- data.frame(cott2mu)
  colnames(cott2mu) <- colnames(df)
  cott3mu <- data.frame(cott3mu)
  colnames(cott3mu) <- colnames(df)
  
  file1 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster1_mean_curve_unit_",month,"_",year,".csv") 
  file2 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster2_mean_curve_unit_",month,"_",year,".csv") 
  file3 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster3_mean_curve_unit_",month,"_",year,".csv") 
  write.table(cott1mu, file1, sep =",",row.names = FALSE, col.names = TRUE)
  write.table(cott2mu, file2, sep =",", row.names = FALSE, col.names = TRUE)
  write.table(cott3mu, file3, sep =",",row.names = FALSE, col.names = TRUE)
  
  sott1u <- normalise_curves(sott1)
  sott2u <- normalise_curves(sott2)
  sott3u <- normalise_curves(sott3)
  
  err1 <- ddistribution_error_rt(cott1mu, sott1u)
  err2 <- ddistribution_error_rt(cott2mu, sott2u)
  err3 <- ddistribution_error_rt(cott3mu, sott3u)
  
  month2 <- map_months(month)

  if(bVerbose & month%in%c(1,3,5,7,8,10,12)) {vplot31(err1); vplot31(err2); vplot31(err3)}
  else if(bVerbose & month2%in%c(4, 6, 9, 11)) {vplot30(err1); vplot30(err2); vplot30(err3)}
  else if(bVerbose & month2%in%c(2) & as.numeric(year) %% 4 == 0) {vplot29(err1); vplot29(err2); vplot29(err3)}
  else if(bVerbose & month2%in%c(2) & as.numeric(year) %% 4 != 0) {vplot28(err1); vplot28(err2); vplot28(err3)}
  
  err1 <- data.frame(err1)
  colnames(err1) <- colnames(df)
  rownames(err1) <- rownames(cott1m)
  err2 <- data.frame(err2)
  colnames(err2) <- colnames(df)
  rownames(err2) <- rownames(cott2m)
  err3 <- data.frame(err3)
  colnames(err3) <- colnames(df)
  rownames(err3) <- rownames(cott3m)
  
  file1 <- paste0("C:/Users/d_floriello/Documents/error_distr/",month,"_",year,"_cluster_1.csv")
  file2 <- paste0("C:/Users/d_floriello/Documents/error_distr/",month,"_",year,"_cluster_2.csv")
  file3 <- paste0("C:/Users/d_floriello/Documents/error_distr/",month,"_",year,"_cluster_3.csv")
  write.table(err1, file1, sep=",",row.names = TRUE, col.names = TRUE)
  write.table(err2, file2, sep= ",", row.names = TRUE, col.names = TRUE)
  write.table(err3, file3, sep=",",  row.names = TRUE, col.names = TRUE)
  
  # year_before <- as.numeric(year) - 1
  # if(year_before >= 2013)
  # {
  #   
  # }
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
  for(i in 1:nrow(mat))
  {
    matn[i,] <- unlist(mat[i,])/compute_integral(unlist(mat[i,]))
  }
  return(matn)
}
######################################################################
assign_to_cluster <- function(curve, month, year)
{
  # curve is normalised
  #month <- current_month
  #year <- as.numeric(current_year) - 1
  file1 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster1_mean_curve_unit_",month,"_",year,".csv") 
  file2 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster2_mean_curve_unit_",month,"_",year,".csv") 
  file3 <- paste0("C:/Users/d_floriello/Documents/error_distr/cluster3_mean_curve_unit_",month,"_",year,".csv") 
  if(file.exists(file1) | file.exists(file2) | file.exists(file3))
  {
    me1 <- read.csv2(file1, sep =",",row.names = FALSE, header = TRUE, colClasses = "character", stringsAsFactors = FALSE)
    me2 <- read.csv2(file2, sep =",",row.names = FALSE, header = TRUE, colClasses = "character", stringsAsFactors = FALSE)
    me3 <- read.csv2(file3, sep =",",row.names = FALSE, header = TRUE, colClasses = "character", stringsAsFactors = FALSE)
    diff1 <- mean((curve - as.numeric(me1))^2)
    diff2 <- mean((curve - as.numeric(me2))^2)
    diff3 <- mean((curve - as.numeric(me3))^2)
    vd <- c(diff1,diff2,diff3)
    
    return(c(which.min(vd),get(paste0("me",which.min(vd)))))
  }
  else return(curve)
}
#####################################################################
fci <- function(vm, errs, B = 1000)
{
  ups <- lows <- rep(0,ncol(errs))
  for(n in 1:ncol(errs))
  {
    bs <- maply(1:100, function(i) sample(errs[,n], B, replace = TRUE))
    boots <- vm[n] - rowMeans(bs)
    ups[n] <- vm[n] + quantile(boots,probs=0.975)
    lows[n] <- vm[n] + quantile(boots,probs=0.025)
  }
  return(rbind(ups, vm, lows))
}
#####################################################################
forecast_consumption <- function(curve, pdr, cons, month, year)
{
  file <- paste0("C:/Users/d_floriello/Documents/error_distr/curves_",month,"_",year,".csv") 
  if(file.exists(file))
  {
    mcc <- read.csv2(file, sep =",",header = TRUE, colClasses = "character", stringsAsFactors = FALSE)
    prev <- 0
    if(pdr %in% rownames(mcc))
      prev <- mcc[which(rownames(mcc) == pdr),]
    else
    {
      prev_e <- assign_to_cluster(curve, month, year)
      kbar <- read.csv2(paste0("C:/Users/d_floriello/Documents/error_distr/cluster",prev_e[1],"_mean_curve_",month,"_",year,".csv"), 
                        sep =",",header = TRUE, colClasses = "character", stringsAsFactors = FALSE)
      Zbar <- compute_integral(as.numeric(unlist(kbar)))
      prev <- (cons/Zbar)*unlist(prev_e[2:length(prev_e)])
    }
    err <- read.csv2(paste0("C:/Users/d_floriello/Documents/error_distr/",month,"_",year,"_cluster_",prev_e[1],".csv"),
                     sep =",",header = TRUE, colClasses = "character", stringsAsFactors = FALSE)
    interval <- fci(err)
    
    prev <- prev - interval[2,]
    
    return(rbind(prev, interval[1,], interval[3,]))
  }
  else
  {
    return(curve)
  }
}



