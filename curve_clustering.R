library(openxlsx)
library(dplyr)
library(reshape)
library(stringi)
library(xlsx)
library(vioplot)
library(fda)
library(h2o)
library(TSA)
library(tseries)
library(rnn)
localH2O <- h2o.init()


source("functions_for_curve_clustering.R")
source("SparseFunctClust.R")
source("funtional_gap_statistic.R")
source("functions_for_OP.R")

### dati 2015-2016:
cgen15 <- read_and_plot_data_2015("GENNAIO")
cfeb15 <- read_and_plot_data_2015("FEBBRAIO")
cmar15 <- read_and_plot_data_2015("MARZO")
capr15 <- read_and_plot_data_2015("APRILE")
cmag15 <- read_and_plot_data_2015("MAGGIO")
cgiu15 <- read_and_plot_data_2015("GIUGNO")
clug15 <- read_and_plot_data_2015("LUGLIO")
cago15 <- read_and_plot_data_2015("AGOSTO")
cset15 <- read_and_plot_data_2015("SETTEMBRE")
cott15 <- read_and_plot_data("OTTOBRE")
cnov15 <- read_and_plot_data("NOVEMBRE")
cdic15 <- read_and_plot_data("DICEMBRE")
cgen <- read_and_plot_data("GENNAIO")
cfeb <- read_and_plot_data("FEBBRAIO")
cmar <- read_and_plot_data("MARZO")
capr <- read_and_plot_data("APRILE")
### dati 2014
cgen14 <- read_and_plot_data_2014("GENNAIO")
cfeb14 <- read_and_plot_data_2014("FEBBRAIO")
cmar14 <- read_and_plot_data_2014("MARZO")
capr14 <- read_and_plot_data_2014("APRILE")
cmag14 <- read_and_plot_data_2014("MAGGIO")
cgiu14 <- read_and_plot_data_2014("GIUGNO")
clug14 <- read_and_plot_data_2014("LUGLIO")
cago14 <- read_and_plot_data_2014("AGOSTO")
cset14 <- read_and_plot_data_2014("SETTEMBRE")
cott14 <- read_and_plot_data_2014("OTTOBRE")
cnov14 <- read_and_plot_data_2014("NOVEMBRE")
cdic14 <- read_and_plot_data_2014("DICEMBRE")
### dati 2013
cgen13 <- read_and_plot_data_2013("GENNAIO")
cfeb13 <- read_and_plot_data_2013("FEBBRAIO")
cmar13 <- read_and_plot_data_2013("MARZO")
capr13 <- read_and_plot_data_2013("APRILE")
cmag13 <- read_and_plot_data_2013("MAGGIO")
cgiu13 <- read_and_plot_data_2013("GIUGNO")
clug13 <- read_and_plot_data_2013("LUGLIO")
cago13 <- read_and_plot_data_2013("AGOSTO")
cset13 <- read_and_plot_data_2013("SETTEMBRE")
cott13 <- read_and_plot_data_2013("OTTOBRE")
cnov13 <- read_and_plot_data_2013("NOVEMBRE")
cdic13 <- read_and_plot_data_2013("DICEMBRE")

#########
test <- matrix(0,nrow=nrow(cnov15),ncol=ncol(cnov15))
for(i in 1:nrow(cnov15))
{
  test[i,] <- unlist(cnov15[i,])/compute_integral(unlist(cnov15[i,]))
}
test[is.nan(test)] <- 0
par(mfrow=c(1,2))
for(i in 1:100)
{
  plot(1:30,cnov15[i,1:30],type="l",lwd=2,col="black")
  plot(1:30,test[i,1:30],type="l",lwd=2,col="red")
}


matplot(1:30, t(test[,1:30]), type="l")

otno <- aggregate_months(cott,cnov)
otno <- aggregate_months(otno,cdic)
otno <- aggregate_months(otno,cgen)
otno <- aggregate_months(otno,cfeb)
otno <- aggregate_months(otno,cmar)
otno <- aggregate_months(otno,capr)

matplot(1:ncol(cott), t(cott), type="l", lwd=2, xlab = "giorni", ylab = "misure gas", main="ott 2015 - apr 2016")

for(i in 1:nrow(cott))
{
  argvals = seq(0,1,len=ncol(cott))
  nbasis = 20
  basisobj = create.bspline.basis(c(0,1),nbasis)
  Ys = smooth.basis(argvals=argvals, y=unlist(cott[i,]), fdParobj=basisobj,returnMatrix=TRUE)
  plot(Ys)
  lines(argvals, unlist(cott[1,]),type="l", col = "blue")
}

fkm <- FKMSparseClustering(cott, argvals, K=3, m= 0.8, method="kmea")
matplot(argvals, t(cott),type="l", lwd=2,col = fkm$CLUSTER)
gap <- FKMSparseClustering.permute(cott, argvals, K=2, method="kmea", nperm=50)

## 35 = il numero di profili di prelievo 
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
for(df in dfs)
{
  if(is.data.frame(get(df)) & nrow(get(df)) >= 100)
  {
    print(dim(get(df)))
    ch <- CH_statistics(get(df),20,mi=60)
    par(mfrow=c(1,1))
    plot(2:19, ch, type="l", lwd=2, col= "red", xlab="# clusters", ylab="CH-statistic", main=df)
    points(2:19, ch, pch=16, col="black")    
  }
}

ver <- openxlsx::read.xlsx("Report_214.xlsx", sheet = 1, colNames = TRUE)
ver[is.na(ver)] <- 0
profili_consumo <- openxlsx::read.xlsx("profili.xlsx", sheet = 1, colNames = TRUE)
prof <- data.frame(profili_consumo[2:nrow(profili_consumo),3:38])
for(df in dfs)
{
  if(is.data.frame(get(df)) & nrow(get(df)) >= 100)
  {
    print(df)
    my <- get_month_year(df)
    get_distributions_error(get(df),TRUE,df,my[1],my[2],prof)
  }
}
### evoluzione temporale dei consumi osservati ### 
cons_ind <- cons_medi <- cons_civ <- c()
diff_ind <- diff_medi <- diff_civ <- c()
for(df in dfs)
{
  if(is.data.frame(get(df)) & nrow(get(df)) >= 100)
  {
    data <- get(df)
    pdrl <- plot_clusters_mean(get(df),3,1,3,df) 
    mott1 <- extract_cons(ver,pdrl[[1]])
    mott2 <- extract_cons(ver,pdrl[[2]])
    mott3 <- extract_cons(ver,pdrl[[3]])
    if(length(pdrl[[1]]) == max(length(pdrl[[1]]),length(pdrl[[2]]),length(pdrl[[3]])))
    {
      pl <- unlist(pdrl[[1]])
      ints <- maply(1:length(pdrl[[1]]), function(n) compute_integral(data[which(rownames(data) == pl[n]),]))
      cons_civ <- c(cons_civ, sum(ints))
    }
    else if(length(pdrl[[1]]) == min(length(pdrl[[1]]),length(pdrl[[2]]),length(pdrl[[3]])))
    {
      
    }
    else
    {
      
    }
  }
}
#### test algoritmo ####
profili_consumo <- openxlsx::read.xlsx("profili.xlsx", sheet = 1, colNames = TRUE)
prof <- data.frame(profili_consumo[2:nrow(profili_consumo),3:38])
pm <- openxlsx::read.xlsx("profili_mensili.xlsx", sheet = 1, colNames = TRUE)

forecast_error <- c()
num <- sample.int(length(dfs), size = 10)
for(i in 1:10)
{
  
  df <- get(dfs[num[i]])
  nr <- nrow(df)
  nrp <- floor(nr*0.1)
  indices <- sample.int(nr,nrp)
  test <- df[indices,]
  ex <- extract_cons(ver,rownames(test))
  
  month <- get_month_year(dfs[num[i]])[1]
  year <- get_month_year(dfs[num[i]])[2]
  
  sott1 <- expected_monthly_consumption(ex, map_months(month), year,prof)
  for(j in 1:nrow(sott1))
  {
    pdr <- rownames(sott1)[j]
    cr <- pm[as.numeric(map_months(month)),which(colnames(pm) == ex[j,2])]
    cm <- as.numeric(ex[j,1]) * as.numeric(cr)
    if(cm == 0) cm <- 1
    fc <- forecast_consumption(sott1, pdr, cm, month, as.numeric(year)-1)
    
  }

}



############## PDR aggregati in REMI ##########

pdrl <- c()
for(df in dfs)
{
  if(is.data.frame(get(df)))
  {
    pdrl <- c(pdrl,rownames(get(df))) 
  }
}

remil <- list()
lens <- c()
REMI <- unique(unlist(ver["COD_REMI"]))
for(remi in REMI)
{
  remil[[remi]] <- unique(unlist(ver[which(ver["COD_REMI"] == remi),"PDR"]))
  lens <- c(lens, length(unique(unlist(ver[which(ver["COD_REMI"] == remi),"PDR"]))))
}

pdrl <- list()
for(df in dfs)
{
  if(is.data.frame(get(df)) & nrow(get(df)) >= 100)
  {
    pdrl <- plot_clusters_mean(get(df),3,1,3,df) 
  }
}

argvals = seq(0,1,len=ncol(cnov))
clust_ott <- 
clust_ott <- plot_clusters_mean(cnov15,3,1,3,"novembre")



mott1 <- extract_cons(ver,clust_ott[[1]])
mott2 <- extract_cons(ver,clust_ott[[2]])
mott3 <- extract_cons(ver,clust_ott[[3]])

table(mott1[,2])
table(mott2[,2])
table(mott3[,2])



par(mfrow=c(1,1))
plot(1:nrow(mott3), as.numeric(as.character(mott3[,1])), pch=16)
points(1:nrow(mott2), as.numeric(as.character(mott2[,1])), col="red", pch=16)
points(1:nrow(mott1), as.numeric(as.character(mott1[,1])), col="blue", pch=16)

profili_consumo <- openxlsx::read.xlsx("profili.xlsx", sheet = 1, colNames = TRUE)
prof <- data.frame(profili_consumo[2:nrow(profili_consumo),3:38])

sott1 <- expected_monthly_consumption(mott1, "11", "2016",prof)
sott2 <- expected_monthly_consumption(mott2, "10", "2016",prof)
sott3 <- expected_monthly_consumption(mott3, "10", "2016",prof)

for(i in 1:nrow(sott1)){
sott1[i,] <- unlist(sott1[i,])/compute_integral(unlist(sott1[i,]))
sott1[i,is.nan(unlist(sott1[i,]))] <- 0
}

par(mfrow=c(1,2))
matplot(1:ncol(cnov15[,1:30]), t(cnov15[,1:30]), type="l", lwd=2, xlab = "giorni", ylab = "misure gas", main="ott 2015 - apr 2016")
matplot(1:ncol(sott1), t(sott1), type="l", lwd=2, xlab = "giorni", ylab = "misure gas", main="ott 2015 - apr 2016")
par(mfrow=c(1,2))
for(i in 1:100)
{
  plot(1:30,test[i,1:30],type="l",lwd=2,col="black")
  plot(1:30,sott1[i,1:30],type="l",lwd=2,col="red")
}

err <- ddistribution_error_rt(test[which(rownames(cnov15) %in% p1),], sott1)
vplot30(err)

errm <- ddistribution_error(colMeans(test[which(rownames(cnov15) %in% p1),]), sott1)
vplot30(errm)


cott <- cott[,1:31]
plot_overlapping_pdr_curves(sott2,cott,"01611428001177", "ottobre")

M <- curves_given_pdr(sott2,cott,"01611428001177")
diff <- M[1,] - M[2,]
plot(1:31, diff,col="navy",lwd=2,xlab="giorni",ylab="differenze reali-stimati")
mean(diff)
sd(diff)

mese <- "ottobre"
p1 <- unlist(rownames(mott1))
cott1m <- compute_mean(cott, p1)
plot(1:length(cott1m[1:31]), cott1m[1:31], type="l", col="black", lwd=2, xlab="giorni",ylab="cons",main=mese)
lines(1:length(cott1m[1:31]), colMeans(sott1), type="l", col="red", lwd=2)
p2 <- unlist(rownames(mott2))
cott2m <- compute_mean(cott, p2)
plot(1:length(cott2m[1:31]), cott2m[1:31], type="l", col="black", lwd=2, xlab="giorni",ylab="cons",main=mese)
lines(1:length(cott2m[1:31]), colMeans(sott2[1:31]), type="l", col="red", lwd=2)
p3 <- unlist(rownames(mott3))
cott3m <- compute_mean(cott[1:31], p3)
plot(1:length(cott3m), cott3m, type="l", col="black", lwd=2, xlab="giorni",ylab="cons",main=mese)
lines(1:length(cott3m), colMeans(sott3[1:31]), type="l", col="red", lwd=2)

err1 <- ddistribution_error(cott1m[1:31], sott1)
vplot31(err1)

diff1 <- unlist(cott1m[1:31]) - unlist(colMeans(sott1))
diff2 <- unlist(cott2m[1:31]) - unlist(colMeans(sott2))
diff3 <- unlist(cott3m[1:31]) - unlist(colMeans(sott3))

plot(1:31, diff1,type="l",col="magenta")
lines(1:31,diff2,type="l",col="gold")
lines(1:31,diff3,type="l",col="brown")












