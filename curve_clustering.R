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
library(TDA)

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
otno <- aggregate_months(cott,cnov)
otno <- aggregate_months(otno,cdic)
otno <- aggregate_months(otno,cgen)
otno <- aggregate_months(otno,cfeb)
otno <- aggregate_months(otno,cmar)
otno <- aggregate_months(otno,capr)

matplot(1:ncol(cott), t(cott), type="l", lwd=2, xlab = "giorni", ylab = "misure gas", main="ott 2015 - apr 2016")

# for(i in 1:nrow(cott))
# {
#   argvals = seq(0,1,len=ncol(cott))
#   nbasis = 20
#   basisobj = create.bspline.basis(c(0,1),nbasis)
#   Ys = smooth.basis(argvals=argvals, y=unlist(cott[i,]), fdParobj=basisobj,returnMatrix=TRUE)
#   plot(Ys)
#   lines(argvals, unlist(cott[1,]),type="l", col = "blue")
# }
# 
# fkm <- FKMSparseClustering(cott, argvals, K=3, m= 0.8, method="kmea")
# matplot(argvals, t(cott),type="l", lwd=2,col = fkm$CLUSTER)
# gap <- FKMSparseClustering.permute(cott, argvals, K=2, method="kmea", nperm=50)


## 35 = il numero di profili di prelievo 

# for(df in dfs)
# {
#   if(is.data.frame(get(df)) & nrow(get(df)) >= 100)
#   {
#     print(dim(get(df)))
#     ch <- CH_statistics(get(df),20,mi=60)
#     par(mfrow=c(1,1))
#     plot(2:19, ch, type="l", lwd=2, col= "red", xlab="# clusters", ylab="CH-statistic", main=df)
#     points(2:19, ch, pch=16, col="black")    
#   }
# }
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
dfs <- dfs[1:37]
dfs <- dfs[c(26,5,17,20,1,35,32,29,8,15,12,27,6,23,18,21,2,36,33,30,9,16,13,28,7,24,19,22,3,37,34,31,10,14,11,25,4)]

pm <- openxlsx::read.xlsx("profili_mensili.xlsx", sheet = 1, colNames = TRUE)
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
    get_distributions_error(get(df),FALSE,df,my[1],my[2],prof)
  }
}

for(df in dfs)
{
  if(is.data.frame(get(df)) & nrow(get(df)) >= 100)
  {
    print(df)
    pcm <- plot_clusters_mean(get(df),3,1,1,df,FALSE)
    pcm1 <- extract_cons(ver2, pcm[[1]])
    pcm2 <- extract_cons(ver2, pcm[[2]])
    pcm3 <- extract_cons(ver2, pcm[[3]])
    ### assegna consumi medi, massimi e minimi al cluster giusto e salva quei valori
  }
}

### evoluzione temporale dei consumi osservati totali### 
cons_ind <- cons_medi <- cons_civ <- c()
diff_ind <- diff_medi <- diff_civ <- c()
for(df in dfs)
{
  if(is.data.frame(get(df)) & nrow(get(df)) >= 100)
  {
    data <- get(df)
    my <- get_month_year(df)
    pdrl <- plot_clusters_mean(get(df),3,1,3,df,plot=TRUE) 
    mott1 <- extract_cons(ver,pdrl[[1]])
    mott2 <- extract_cons(ver,pdrl[[2]])
    mott3 <- extract_cons(ver,pdrl[[3]])

    pl1 <- unlist(pdrl[[1]])
    pl1 <- pl1[which(mott1[,2] != "0")]
    mott1 <- mott1[which(mott1[,2] != "0"),]
    pl2 <- unlist(pdrl[[2]])
    pl2 <- pl2[which(mott2[,2] != "0")]
    mott2 <- mott2[which(mott2[,2] != "0"),]
    pl3 <- unlist(pdrl[[3]])
    pl3 <- pl3[which(mott3[,2] != "0")]
    mott3 <- mott3[which(mott3[,2] != "0"),]
    
        
    if(length(pdrl[[1]]) == max(length(pdrl[[1]]),length(pdrl[[2]]),length(pdrl[[3]])))
    {
      ints <- unlist(maply(1:length(pl1), function(n) compute_integral(data[which(rownames(data) == pl1[n]),])))
      th_cons <- unlist(maply(1:length(pl1), function(n) as.numeric(as.character(mott1[n,1])) * as.numeric(pm[as.numeric(map_months(my[1])),which(colnames(pm)==mott1[n,2])])))
      diff <- ints - th_cons
      cons_civ <- c(cons_civ, sum(ints))
      diff_civ <- c(diff_civ, mean(diff))
    }
    else if(length(pdrl[[1]]) == min(length(pdrl[[1]]),length(pdrl[[2]]),length(pdrl[[3]])))
    {
      ints <- unlist(maply(1:length(pl1), function(n) compute_integral(data[which(rownames(data) == pl1[n]),])))
      th_cons <- unlist(maply(1:nrow(mott1), function(n) as.numeric(as.character(mott1[n,1])) * as.numeric(pm[as.numeric(map_months(my[1])),which(colnames(pm)==mott1[n,2])])))
      diff <- ints - th_cons
      cons_ind <- c(cons_ind, sum(ints))
      diff_ind <- c(diff_ind, mean(diff))
    }
    else
    {
      ints <- unlist(maply(1:length(pl1), function(n) compute_integral(data[which(rownames(data) == pl1[n]),])))
      th_cons <-unlist(maply(1:nrow(mott1), function(n) as.numeric(as.character(mott1[n,1])) * as.numeric(pm[as.numeric(map_months(my[1])),which(colnames(pm)==mott1[n,2])])))
      diff <- ints - th_cons
      cons_medi <- c(cons_medi, sum(ints))
      diff_medi <- c(diff_medi, mean(diff))
    }
    
    if(length(pdrl[[2]]) == max(length(pdrl[[1]]),length(pdrl[[2]]),length(pdrl[[3]])))
    {
      ints <- unlist(maply(1:length(pl2), function(n) compute_integral(data[which(rownames(data) == pl2[n]),])))
      th_cons <- unlist(maply(1:nrow(mott2), function(n) as.numeric(as.character(mott2[n,1])) * as.numeric(pm[as.numeric(map_months(my[1])),which(colnames(pm)==mott2[n,2])])))
      diff <- ints - th_cons
      cons_civ <- c(cons_civ, sum(ints))
      diff_civ <- c(diff_civ, mean(diff))
    }
    else if(length(pdrl[[2]]) == min(length(pdrl[[1]]),length(pdrl[[2]]),length(pdrl[[3]])))
    {
      ints <- unlist(maply(1:length(pl2), function(n) compute_integral(data[which(rownames(data) == pl2[n]),])))
      th_cons <- unlist(maply(1:nrow(mott2), function(n) as.numeric(as.character(mott2[n,1]))* as.numeric(pm[as.numeric(map_months(my[1])),which(colnames(pm)==mott2[n,2])])))
      diff <- ints - th_cons
      cons_ind <- c(cons_ind, sum(ints))
      diff_ind <- c(diff_ind, mean(diff))
    }
    else
    {
      ints <- unlist(maply(1:length(pl2), function(n) compute_integral(data[which(rownames(data) == pl2[n]),])))
      th_cons <- unlist(maply(1:nrow(mott2), function(n) as.numeric(as.character(mott2[n,1])) * as.numeric(pm[as.numeric(map_months(my[1])),which(colnames(pm)==mott2[n,2])])))
      diff <- ints - th_cons
      cons_medi <- c(cons_medi, sum(ints))
      diff_medi <- c(diff_medi, mean(diff))
    }
    
    if(length(pdrl[[3]]) == max(length(pdrl[[1]]),length(pdrl[[2]]),length(pdrl[[3]])))
    {
      ints <- unlist(maply(1:length(pl3), function(n) compute_integral(data[which(rownames(data) == pl3[n]),])))
      th_cons <- unlist(maply(1:nrow(mott3), function(n) as.numeric(as.character(mott3[n,1])) * as.numeric(pm[as.numeric(map_months(my[1])),which(colnames(pm)==mott3[n,2])])))
      diff <- ints - th_cons
      cons_civ <- c(cons_civ, sum(ints))
      diff_civ <- c(diff_civ, mean(diff))
    }
    else if(length(pdrl[[3]]) == min(length(pdrl[[1]]),length(pdrl[[2]]),length(pdrl[[3]])))
    {
      ints <- unlist(maply(1:length(pl3), function(n) compute_integral(data[which(rownames(data) == pl3[n]),])))
      th_cons <- unlist(maply(1:nrow(mott3), function(n) as.numeric(as.character(mott3[n,1])) * as.numeric(pm[as.numeric(map_months(my[1])),which(colnames(pm)==mott3[n,2])])))
      diff <- ints - th_cons
      cons_ind <- c(cons_ind, sum(ints))
      diff_ind <- c(diff_ind, mean(diff))
    }
    else
    {
      ints <- unlist(maply(1:length(pl3), function(n) compute_integral(data[which(rownames(data) == pl3[n]),])))
      th_cons <- unlist(maply(1:nrow(mott3), function(n) as.numeric(as.character(mott3[n,1])) * as.numeric(pm[as.numeric(map_months(my[1])),which(colnames(pm)==mott3[n,2])])))
      diff <- ints - th_cons
      cons_medi <- c(cons_medi, sum(ints))
      diff_medi <- c(diff_medi, mean(diff))
    }
  }
}
## plot of the results:
diffs <- c("diff_civ","diff_medi","diff_ind")
conss <- c("cons_civ","cons_medi","cons_ind")
diff_ind[is.nan(diff_ind)] <- 0

for(d in diffs)
{
  plot(1:length(get(d)), get(d),type="l",lwd=2,xlab="time",ylab="diff", main=paste0("differences", d))
  plot(dog <- stl(ts( c(get(d),get(d)),frequency=2 ),s.window=7))
  tr <- dog$time.series[1:23,2:3]
  xlsx::write.xlsx(data.frame(tr), paste0("C:/Users/d_floriello/Documents/error_distr/diff_",d,".xlsx"), row.names=FALSE, col.names = TRUE)
}
for(d in conss)
{
  plot(1:length(get(d)), get(d),type="l",lwd=2,xlab="time",ylab="cons", main=paste0("consumption evolution", d))
  plot(stl(ts( c(get(d),get(d)),frequency=2 ),s.window=7))
}

istl <- stl(ts( c(diff_ind,diff_ind),frequency=2 ),s.window=7)
tt <-  istl$time.series[,2]
sp_test <- smooth.spline(x=tt[1:23], spar=0.2, cv = NA)
plot(sp_test)
lines(sp_test, type="l")
lines(predict(sp_test, seq(0, 1,len=23), type="l",col="blue"))

#### test algoritmo ####
profili_consumo <- openxlsx::read.xlsx("profili.xlsx", sheet = 1, colNames = TRUE)
prof <- data.frame(profili_consumo[2:nrow(profili_consumo),3:38])
pm <- openxlsx::read.xlsx("profili_mensili.xlsx", sheet = 1, colNames = TRUE)

## per evitare che non trovi i file excel:
dfs <- dfs[15:37]

forecast_error <- c()
prev_error <- c()
upper_mean <- lower_mean <- c()
num <- sample.int(length(dfs), size = 10)
for(i in 1:10)
{
  
  df <- get(dfs[num[i]])
  nr <- nrow(df)
  nrp <- floor(nr*0.1)
  indices <- sample.int(nr,nrp)
  test <- df[indices,]
  ex <- extract_cons(ver,rownames(test))
  print(ex)
  ex <- ex[which(ex[,1] != "0"),]
  
  month <- get_month_year(dfs[num[i]])[1]
  year <- get_month_year(dfs[num[i]])[2]
  
  print(year)
  print(month)
  
  sott1 <- expected_monthly_consumption(ex, month, year,prof)
  for(j in 1:nrow(sott1))
  {
    pdr <- rownames(sott1)[j]
    cr <- pm[as.numeric(map_months(month)),which(colnames(pm) == ex[j,2])]
    cm <- as.numeric(ex[j,1]) * as.numeric(cr)
    if(cm == 0) cm <- 1
    fc <- forecast_consumption(sott1[j,], pdr, cm, month, year)
    
    if(month %in% c("APRILE", "MAGGIO", "GIUGNO", "LUGLIO", "AGOSTO", "SETTEMBRE")) forecast_error <- c(forecast_error, (-1)*mean(fc[1,] - unlist(test[j,]))^2)
    else forecast_error <- c(forecast_error, mean(fc[1,] - unlist(test[j,]))^2)
    prev_error <- c(prev_error, mean(unlist(sott1[j,]) - unlist(test[j,]))^2)
    upper_mean <- c(upper_mean, mean(fc[2,]))
    lower_mean <- c(lower_mean, mean(fc[3,]))
  }

}

plot(1:length(forecast_error), abs(forecast_error), type="l", lwd=2, col="navy")
lines(1:length(prev_error), prev_error, type="o", lwd=2, col="gold")
upper_mean; lower_mean
forecast_error[forecast_error > 250] <- 0
plot(1:length(forecast_error), forecast_error, type="l", lwd=2, col="blue")
############## PDR aggregati in REMI ##########

pdrl <- c()
for(df in dfs)
{
  if(is.data.frame(get(df)))
  {
    pdrl <- c(pdrl,rownames(get(df))) 
  }
}

ver2 <- extract_relevant_val(ver)
remil <- list()
profil <- list()
lens <- c()
REMI <- unique(unlist(ver2["COD_REMI"]))
for(remi in REMI)
{
  remil[[remi]] <- unique(unlist(ver2[which(ver["COD_REMI"] == remi),"PDR"]))
  profil[[remi]] <- unlist(ver[which(ver2["COD_REMI"] == remi),"PROFILO_PRELIEVO"])
  lens <- c(lens, length(unique(unlist(ver2[which(ver["COD_REMI"] == remi),"PDR"]))))
}

pdrl <- list()
for(df in dfs)
{
  if(is.data.frame(get(df)) & nrow(get(df)) >= 100)
  {
    pdrl <- plot_clusters_mean(get(df),3,1,3,df) 
  }
}

mar15 <- openxlsx::read.xlsx("max_allocato.xlsx", sheet = "marzo 2015", colNames = TRUE)
apr15 <- openxlsx::read.xlsx("max_allocato.xlsx", sheet = "aprile 2015", colNames = TRUE)
mag15 <- openxlsx::read.xlsx("max_allocato.xlsx", sheet = "maggio 2015", colNames = TRUE)
giu15 <- openxlsx::read.xlsx("max_allocato.xlsx", sheet = "giugno 2015", colNames = TRUE)
lug15 <- openxlsx::read.xlsx("max_allocato.xlsx", sheet = "luglio 2015", colNames = TRUE)
ago15 <- openxlsx::read.xlsx("max_allocato.xlsx", sheet = "agosto 2015", colNames = TRUE)
set15 <- openxlsx::read.xlsx("max_allocato.xlsx", sheet = "settembre 2015", colNames = TRUE)
ott15 <- openxlsx::read.xlsx("max_allocato.xlsx", sheet = "ottobre 2015", colNames = TRUE)
nov15 <- openxlsx::read.xlsx("max_allocato.xlsx", sheet = "novembre 2015", colNames = TRUE)
dic15 <- openxlsx::read.xlsx("max_allocato.xlsx", sheet = "dicembre 2015", colNames = TRUE)
gen16 <- openxlsx::read.xlsx("max_allocato.xlsx", sheet = "gennaio 2016", colNames = TRUE)
feb16 <- openxlsx::read.xlsx("max_allocato.xlsx", sheet = "febbraio 2016", colNames = TRUE)
mar16 <- openxlsx::read.xlsx("max_allocato.xlsx", sheet = "marzo 2016", colNames = TRUE)
apr16 <- openxlsx::read.xlsx("max_allocato.xlsx", sheet = "aprile 2016", colNames = TRUE)

dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
dfs <- dfs[c(1:13,17)]

remi_found <- c()
for(df in dfs)
{
   remi_found <- c(remi_found, as.character(get(df)[,1]))
}

remi_found <- unique(remi_found)
dfs <- dfs[c(10,2,9,7,8,1,14,13,12,4,6,5,11,3)]

profili_consumo <- openxlsx::read.xlsx("profili15.xlsx", sheet = 1, colNames = TRUE)
prof <- data.frame(profili_consumo[2:nrow(profili_consumo),3:38])

diff_matrix <- matrix(0, nrow=length(remi_found), ncol = 24)
rownames(diff_matrix) <- remi_found
setwd("plot_remi")
for(rf in remi_found)
{
  #re <- ver2[which(ver2["COD_REMI"] == rf),]
  re <- ver[which(ver["COD_REMI"] == rf),]
  if(nrow(re) > 0)
  {
    cons <- rep(0, nrow(re))
    for(i in 1:nrow(re)) cons[i] <- ifelse(re[i,"CONSUMO_DISTRIBUTORE"] != "0", as.numeric(as.character(re[i,"CONSUMO_DISTRIBUTORE"])), as.numeric(as.character(re[i,"CONSUMO_CONTR_ANNUO"])))
    agg <- data.frame(cbind(prodotto = re["CODICE_PRODOTTO"], data.inizio =re["D_VALIDO_DAL_T"], 
                            data.fine = re["D_VALIDO_AL_T"], profilo= re["PROFILO_PRELIEVO"], 
                            consumo = cons))
    colnames(agg) <- c("prodotto", "data inizio", "data fine", "profilo", "consumo")
    profili <- colSums(compute_profiles_DEF15(agg, prof))
    max_rf <- rep(0, length(dfs))
    for(j in 1:length(dfs))
    {
      if(rf %in% get(dfs[j])[,1]) max_rf[j] <- get(dfs[j])[which(get(dfs[j])[,1] == rf),2] 
    }
    names(max_rf) <- c("03/2015","04/2015","05/2015","06/2015","07/2015","08/2015","09/2015",
                       "10/2015","11/2015","12/2015","01/2016","02/2016","03/2016","04/2016")
    pb <- c(min(take_monthV("03", "2015", profili)),min(take_monthV("04", "2015", profili)), min(take_monthV("05", "2015", profili)),
            min(take_monthV("06", "2015", profili)),min(take_monthV("07", "2015", profili)),min(take_monthV("08", "2015", profili)),
            min(take_monthV("09", "2015", profili)),min(take_monthV("10", "2015", profili)),min(take_monthV("11", "2015", profili)),
            min(take_monthV("12", "2015", profili)),min(take_monthV("01", "2016", profili)),min(take_monthV("02", "2016", profili)),
            min(take_monthV("03", "2016", profili)),min(take_monthV("04", "2016", profili)))
    mm <- max(max(max_rf),max(profili))
    mypath <- paste0("plot_",rf, ".jpg")
    jpeg(file=mypath, quality=100)
    plot(1:length(profili), profili, type="l",ylim = c(0, mm+1),xlab="giorni", ylab="consumo",main = paste("fabbisogno per remi", rf, "con max allocazione mensile"))
    #lines(c(90, 120, 151, 181, 212, 243, 273, 304, 334, 365, 396, 425, 456,486), max_rf, type="l",lwd=2,col="red")
    lines(pb, max_rf, type="l",lwd=2,col="red")
    abline(v = which(names(profili) == "01/03/2015"))
    dev.off()
    index <- which(rownames(diff_matrix) == rf)
    diff_matrix[index,3] <- max(profili[take_monthV("03", "2015", profili)]) - max_rf[1]
    diff_matrix[index,4] <- max(profili[take_monthV("04", "2015", profili)]) - max_rf[2]
    diff_matrix[index,5] <- max(profili[take_monthV("05", "2015", profili)]) - max_rf[3]
    diff_matrix[index,6] <- max(profili[take_monthV("06", "2015", profili)]) - max_rf[4]
    diff_matrix[index,7] <- max(profili[take_monthV("07", "2015", profili)]) - max_rf[5]
    diff_matrix[index,8] <- max(profili[take_monthV("08", "2015", profili)]) - max_rf[6]
    diff_matrix[index,9] <- max(profili[take_monthV("09", "2015", profili)]) - max_rf[7]
    diff_matrix[index,10] <- max(profili[take_monthV("10", "2015", profili)]) - max_rf[8]
    diff_matrix[index,11] <- max(profili[take_monthV("11", "2015", profili)]) - max_rf[9]
    diff_matrix[index,12] <- max(profili[take_monthV("12", "2015", profili)]) - max_rf[10]
    diff_matrix[index,13] <- max(profili[take_monthV("01", "2015", profili)]) - max_rf[11]
    diff_matrix[index,14] <- max(profili[take_monthV("02", "2015", profili)]) - max_rf[12]
    diff_matrix[index,15] <- max(profili[take_monthV("03", "2015", profili)]) - max_rf[13]
    diff_matrix[index,16] <- max(profili[take_monthV("04", "2015", profili)]) - max_rf[14]
  }
}
xlsx::write.xlsx(data.frame(diff_matrix), paste0("C:/Users/d_floriello/Documents/plot_remi/differenze.xlsx"), row.names=TRUE, col.names = TRUE)

matplot(1:24, t(diff_matrix), type="l",lwd=2, xlab="mesi", ylab="differenze", main="grafico differenze mensili curve teoriche - allocato SNAM")

nonzero_diff <- c()
for(i in 1:nrow(diff_matrix))
{
  if(sum(diff_matrix[i,]) != 0) nonzero_diff <- c(nonzero_diff, i)
}
nzd <- diff_matrix[nonzero_diff,]
colMeans(nzd)
apply(nzd,2,var)
curva_max <- apply(nzd, 2, max)
curva_min <- apply(nzd, 2, min)
curva_up <- apply(nzd, 2, function(x) quantile(x,probs=0.975))
curva_low <- apply(nzd, 2, function(x) quantile(x,probs=0.025))
plot(1:24, colMeans(nzd), type="l", lwd=2, ylim = c(min(curva_min), max(curva_max)),xlab="mesi",ylab="differenza", main="curva media, massima e minima delle differenze")
lines(1:24, curva_max, type="l", lwd=2,col="red")
lines(1:24, curva_min, type="l", lwd=2,col="blue")
lines(1:24, curva_up, type="l", lwd=2,col="pink")
lines(1:24, curva_low, type="l", lwd=2,col="green")
#################################################################################
####### costruzione database storico dei consumi ##########
#################################################################################

## dopo averlo ordinato:
dfs <- dfs[10:37]
surv_pdrs <- rownames(get(dfs[28]))
counts <- length(surv_pdrs)
for(i in 28:1)
{
  extracted <- rownames(get(dfs[i]))
  surv_pdrs <- intersect(surv_pdrs, extracted)
  counts <- c(counts, length(surv_pdrs))
}

## da 17 a 28 
agg <- aggregate_months(get(dfs[17]), get(dfs[18]))
for(i in 19:28)
{
  print(i)
  agg <- aggregate_months(agg, get(dfs[i]))
}

for(i in 1:nrow(agg))
{
  if(sum(agg[i,220:240]) > 15000) print(i)
}

agg[69, "30/07/2015"] <- 161
agg[25, "14/12/2015"] <- (12+82)/2

matplot(1:366, t(agg), type="l", xlab= "giorni", ylab = "consumi", main = "consumo giornaliero maggio 2015 - aprile 2016")
xlsx::write.xlsx(data.frame(agg), "DB_consumo_giornaliero_mag15_apr16.xlsx",row.names = TRUE, col.names = TRUE)

acf(agg[37,], lag.max=7, type="correlation")
plot(stl(ts(unlist(agg[37,]), frequency = 4),s.window=7))
library(coda)
autocorr(coda::as.ts.mcmc(agg[35,]))

tab <- extract_cons(ver, rownames(agg))

#########################################################################################
######## consumption estimation via SDE model ###########################################
#########################################################################################

for(df in dfs)
{
  DF <- correct_obs(get(df))
  my <- get_month_year(df)
  month <- my[1]; year <- my[2]
  print(my)
  pcm <- plot_clusters_mean(DF,3,1,1,df,FALSE)
  pcm1 <- extract_cons(ver2, pcm[[1]])
  pcm2 <- extract_cons(ver2, pcm[[2]])
  pcm3 <- extract_cons(ver2, pcm[[3]])
  # if(df != "cgiu14")
  # {
  #   pcm1 <- pcm1[which(as.character(pcm1[,2]) != "0"),]
  #   pcm2 <- pcm2[which(as.character(pcm2[,2]) != "0"),]
  #   pcm3 <- pcm3[which(as.character(pcm3[,2]) != "0"),]
  #   
  # }
  k1 <- colMeans(DF[which(rownames(DF) %in% rownames(pcm1)),])
  k2 <- colMeans(DF[which(rownames(DF) %in% rownames(pcm2)),])
  k3 <- colMeans(DF[which(rownames(DF) %in% rownames(pcm3)),])
  file1 <- paste0("C:/Users/d_floriello/Documents/SDE_mod/cluster1_mean_curve_pre_",month,"_",year,".xlsx") 
  file2 <- paste0("C:/Users/d_floriello/Documents/SDE_mod/cluster2_mean_curve_pre_",month,"_",year,".xlsx") 
  file3 <- paste0("C:/Users/d_floriello/Documents/SDE_mod/cluster3_mean_curve_pre_",month,"_",year,".xlsx") 
  xlsx::write.xlsx(data.frame(k1), file1, row.names = FALSE, col.names = TRUE)
  xlsx::write.xlsx(data.frame(k2), file2, row.names = FALSE, col.names = TRUE)
  xlsx::write.xlsx(data.frame(k3), file3, row.names = FALSE, col.names = TRUE)
  ec1 <- expected_monthly_consumption(pcm1, my[1], my[2], prof)
  ec2 <- expected_monthly_consumption(pcm2, my[1], my[2], prof)
  ec3 <- expected_monthly_consumption(pcm3, my[1], my[2], prof)
  #plot_errors(k1,ec1,month,year,1)
  #plot_errors(k2,ec2,month,year,2)
  #plot_errors(k3,ec3,month,year,3)
  ecv1 <- DF[which(rownames(DF)%in%rownames(pcm1)),]
  ecv2 <- DF[which(rownames(DF)%in%rownames(pcm2)),]
  ecv3 <- DF[which(rownames(DF)%in%rownames(pcm3)),]
  estimate_models(k1,ec1,ecv1,month,year,1)
  estimate_models(k2,ec2,ecv2,month,year,2)
  estimate_models(k3,ec3,ecv3,month,year,3)
  estimate_rotated_models(k1,ec1,ecv1,month,year,1)
  estimate_rotated_models(k2,ec2,ecv2,month,year,2)
  estimate_rotated_models(k3,ec3,ecv3,month,year,3)
}

dfs <- dfs[6:length(dfs)]
source("functions_for_curve_clustering.R")
forecast_error <- forecast_error_rot <- c()
prev_error <- c()
upper_mean <- lower_mean <- upper_mean_rot <- lower_mean_rot <-c()
num <- sample.int(length(dfs), size = 10)
gm <- c()
for(i in 1:10)
{
  df <- get(dfs[num[i]])
  df <- correct_obs(df)
  nr <- nrow(df)
  nrp <- floor(nr*0.1)
  indices <- sample.int(nr,nrp)
  test <- df[indices,]
  ex <- extract_cons(ver,rownames(test))
  print(ex)
  ex <- ex[which(ex[,1] != "0"),]
  
  month <- get_month_year(dfs[num[i]])[1]
  year <- get_month_year(dfs[num[i]])[2]
  
  print(year)
  print(month)
  
  sott1 <- expected_monthly_consumption(ex, month, year,prof)
  for(j in 1:nrow(sott1))
  {
    fc <- forecast_consumption_via_SDE(sott1[j,], month, year)
    fcr <- forecast_consumption_via_SDE_rotated(sott1[j,], month, year)
    #if(month %in% c("APRILE", "MAGGIO", "GIUGNO", "LUGLIO", "AGOSTO", "SETTEMBRE")) forecast_error <- c(forecast_error, (-1)*mean(fc[1,] - unlist(test[j,]))^2)
    #else forecast_error <- c(forecast_error, mean(fc[1,] - unlist(test[j,]))^2)
    forecast_error <- c(forecast_error, mean(abs(fc[1,] - unlist(test[j,]))))
    forecast_error_rot <- c(forecast_error, mean(abs(fcr[1,] - unlist(test[j,]))))
    prev_error <- c(prev_error, mean(abs(unlist(sott1[j,]) - unlist(test[j,]))))
    if( mean(abs(fcr[1,] - unlist(test[j,]))) <= mean(abs(unlist(sott1[j,]) - unlist(test[j,]))))
    {
      gm <- c(gm, paste(month, year, rownames(sott1[j,])))
    }
    upper_mean <- c(upper_mean, mean(fc[2,]))
    lower_mean <- c(lower_mean, mean(fc[3,]))
    upper_mean_rot <- c(upper_mean_rot, mean(fcr[2,]))
    lower_mean_rot <- c(lower_mean_rot, mean(fcr[3,]))
  }
  
}

plot(1:length(forecast_error), forecast_error, type="l", lwd=2)
lines(1:length(forecast_error_rot), forecast_error_rot, type="c", lwd=2, col="blue")
lines(1:length(prev_error), prev_error, type="o", lwd=2,col="red")

consumi <- c()
for(i in 1:nrow(ver2))
{
  if(ver2[i,"CONSUMO_DISTRIBUTORE"] != "0") consumi <- c(consumi, ver2[i,"CONSUMO_DISTRIBUTORE"])
  else consumi <- c(consumi, ver2[i,"CONSUMO_CONTR_ANNUO"])
}
consumi <- as.numeric(as.character(consumi))
plot(1:length(consumi), consumi, pch=16, col="red")
plot(rep(0,length(consumi)), consumi, pch=16, col="red")
consumi_less <- consumi[consumi < 200000]
plot( consumi_less,rep(0,length(consumi_less)), pch=16, col="purple")
