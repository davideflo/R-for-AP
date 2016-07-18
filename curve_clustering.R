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
REMI <- unique(unlist(ver["COD_REMI"]))
for(remi in REMI)
{
  remil[[remi]] <- unique(unlist(ver2[which(ver["COD_REMI"] == remi),"PDR"]))
  profil[[remi]] <- unlist(ver[which(ver2["COD_REMI"] == remi),"PROFILO_PRELIEVO"])
  lens <- c(lens, length(unique(unlist(ver2[which(ver["COD_REMI"] == remi),"PDR"]))))
}
############### distribuzioni pdr per remi ###########################################
hist(lens, breaks = seq(0,120, 5), xlab="numero pdr per remi", main="istogramma di numero pdr per remi")
br <- c(1  , 2  , 3 ,  4  , 5 ,  6  , 7 ,  8  , 9 , 10  ,11 , 12  ,13 , 14  ,15  ,16  ,17,  18,  19,  20  ,21 , 22  ,23 , 25,  39,  45  ,48 , 74  ,86 , 94 ,111)
plot(br, sort(cumsum(table(lens)/sum(table(lens)))), type="o",col="red", xlab="numero pdr per remi", ylab="percentuale cumulata")
######################################################################################

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
#dfs <- dfs[c(1:13,17)]
dfs <- dfs[c(1:14)]

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
#setwd("plot_remi")
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

M <- matrix(0,nrow = length(remi_found), ncol = 48)
## un remi per ogni riga e nelle colonne dispari ci sono le CG per mese j, mentre nelle colonne pari
## ci sono i max allocati snam per mese j-1.
rownames(M) <- remi_found
for(rf in remi_found)
{
  re <- ver[which(ver["COD_REMI"] == rf),]
  {
    if(nrow(re) > 0)
    {
      cons <- rep(0, nrow(re))
      for(i in 1:nrow(re)) cons[i] <- ifelse(re[i,"CONSUMO_DISTRIBUTORE"] != "0", as.numeric(as.character(re[i,"CONSUMO_DISTRIBUTORE"])), as.numeric(as.character(re[i,"CONSUMO_CONTR_ANNUO"])))
      agg <- data.frame(prodotto = re["CODICE_PRODOTTO"], data.inizio =re["D_VALIDO_DAL_T"], 
                              data.fine = re["D_VALIDO_AL_T"], profilo= re["PROFILO_PRELIEVO"], 
                              consumo = cons)
      colnames(agg) <- c("prodotto", "data inizio", "data fine", "profilo", "consumo")
      CG <- compute_max_prof(agg, prof)
      CG <- colSums(CG)
      max_rf <- rep(0, length(dfs))
      for(j in 1:length(dfs))
      {
        if(rf %in% get(dfs[j])[,1]) max_rf[j] <- get(dfs[j])[which(get(dfs[j])[,1] == rf),2] 
      }
      names(max_rf) <- c("03/2015","04/2015","05/2015","06/2015","07/2015","08/2015","09/2015",
                         "10/2015","11/2015","12/2015","01/2016","02/2016","03/2016","04/2016")
      max_rf <- c(0,0,max_rf,rep(0,8))
      index <- which(rownames(M) == rf)
      for(j in 1:24)
      {
        M[index,j] <- CG[j]
        M[index,(24+j)] <- max_rf[j]
      }
    }
  }
}

il <- c()
for(i in 1:nrow(M))
{
  if(sum(M[i,]) != 0) il <- c(il, i)
}
M <- M[il,]

F <- function(x)
{
  S <- P <- 0
  M <- M[,c(6:24,30:48)]
  for(i in 1:nrow(M))
  {
    for(j in 1:19)
    {
      y <- M[i,j]*((1.1)+x)
      z <- unlist(M[i,j+19] - M[i,j]*(1.1))
      z2 <- unlist(M[i,j+19] - y)[1]
      if(z2 < 0)
      {
        print("entro z")
        S <- S + 3*M[i,j]*(1+x) + 3.5*(abs(z2)); P <- P + M[i,j+19]
      }
      else
      {
        S <- S + 3*M[i,j]*(1+x) 
      }
      print(S)
    }
  }
  print(P)
  return(S)
}

Fr <- function(x)
{
  ### cambia Mo in M!!!!!
  S <- 0
  mat <- matrix(0, nrow = nrow(M), ncol = 19)
  M <- M[,c(6:24,30:48)]
  for(i in 1:nrow(M))
  {
    for(j in 1:19)
    {
      y <- M[i,j]*((1.1)+x)
      z <- unlist(M[i,j+19] - M[i,j]*(1.1))
      z2 <- unlist(M[i,j+19] - y)[1]
      if(z2 < 0)
      {
        mat[i,j] <- 3*M[i,j]*(1+x) + 3.5*(abs(z2)); 
      }
      else
      {
        mat[i,j] <- 3*M[i,j]*(1+x) 
      }
    }
  }
  return(mat)
}

F_r <- function(M, remi)
{
  xx <- seq(0,1,0.001)
  Mo <- M[which(rownames(M) == remi),c(6:24,30:48)]
  mm <- max(Mo)
  f <- c()
  if(length(Mo) > 0)
  {
    for( x in xx)
    {
      S <- 0
      for(j in 1:19)
      {
        y <- mm*(1+x)
        z2 <- (Mo[j+19] - 1.1*y)
        if(z2 > 0)
        {
         S <- S + 3/12*mm*(1+x) + 3.5*(abs(z2)); 
        }
        else
        {
          S <- S + 3*mm*(1+x) 
        }
        #print(paste("S mese", j+5, ":", S))
      }
      f <- c(f,S)
    }
  plot(xx,f,type="l",lwd=2, col="red",xlab= "% banda di sicurezza", ylab="euro", main=rf)
  return(f)
  }
}


F_r2 <- function(x, M, remi, snam)
{
  ### cambia Mo in M!!!!!
  S <- 0
  Mo <- M[which(rownames(M) == remi),6:24]
  if(length(Mo) > 0)
  {
    for(j in 1:19)
    {
      y <- Mo[j]*((1.1)+x)
      z <- unlist(snam - Mo[j]*(1.1))
      z2 <- unlist(snam - y)
      #print(paste("snam", snam))
      #print(paste("Mo", Mo[j]))
      #print(paste("y",y))
      #print(paste("z2",z2))
      if(z2 < y)
      {
        S <- S + 3*Mo[j]*(1+x) + 3.5*(abs(z2)); 
      }
      else
      {
        S <- S + 3*Mo[j]*(1+x) 
      }
      #print(paste("S mese", j+5, ":", S))
    }
    return(S)
  }
}

xx <- seq(0,1, 0.0001)
yy <- F(xx)
plot(xx, yy, type="l",lwd=2)

mm <- Fr(0.5)
mm <- rbind(mm, colSums(mm))
matplot(1:19, t(mm), type="l", lwd=2)

s <- F_r(xx[1], "34952601")
rf <- "34324201"
#for(rf in sample(remi_found,size=10))
S <- rep(0, length(xx))
for(rf in rownames(M))  
{
  s <- c()
  for(x in xx)
  {
    s <- c(s,F_r(x, rf))
  }
  S <- S + s
}
plot(xx,S,type="l",lwd=2, xlab= "banda di sicurezza", ylab="euro")

snam <- seq(0, 100, 10)
res <- matrix(0, nrow=length(snam), ncol=length(xx))
rf <- sample(rownames(M),size=1)
for(i in 1:nrow(res))
{
  #print(i)
  for(j in 1:ncol(res))
  {
    #print(j)
    res[i,j] <- F_r2(xx[j], M[,1:24], rf,snam[i])
  }
}

filled.contour(x=xx, y=snam, z=t(res), zlim = range(res, finite = TRUE),color.palette = cm.colors)
contour(x=xx, y=snam, z=t(res))               
image(x=xx, y=snam, z=t(res))

cg_star <- matrix(0,nrow = nrow(M), ncol = 2)
rownames(cg_star) <- rownames(M)
for(i in 1:nrow(M))
{
  sol <- c()
  for(x in xx)
  {
    sol <- c(sol, F_r(x, rownames(M)[i]))
  }
#  print(paste("x*", xx[which.min(sol)]))
  cg_star[i,1] <- mean(M[i,6:24]) *(1 + xx[which.min(sol)])
  cg_star[i,2] <- mean(M[i,6:24]) *(xx[which.min(sol)])
}


##############################################################
############ grafici su storico #############################
dfs2 <- c("cmar15", "capr15", "cmag15","cgiu15","clug15","cago15","cset15","cott15","cnov15","cdic15","cgen","cfeb","cmar","capr")

diff_matrix <- matrix(0, nrow=length(remi_found), ncol = 24)
rownames(diff_matrix) <- remi_found
#setwd("plot_remi")
for(i in 1:length(remi_found))
{
  rf <- remi_found[i]
  #re <- ver2[which(ver2["COD_REMI"] == rf),]
  re <- extract_relevant_val(ver[which(ver["COD_REMI"] == rf),])
  profili <- rep(0, 731)
  cont <- 0
  if(nrow(re) > 0)
  {
     for(k in 1:nrow(re))
     {
       profili_temp <- rep(0, 731)
       cons <- rep(0, nrow(re))
       pdr <- unique(unlist(re[k,"PDR"]))
       cons <- ifelse(re[k,"CONSUMO_DISTRIBUTORE"] != "0", as.numeric(as.character(re[k,"CONSUMO_DISTRIBUTORE"])), as.numeric(as.character(re[k,"CONSUMO_CONTR_ANNUO"])))
       agg <- data_frame(prodotto = re[k,"CODICE_PRODOTTO"], data.inizio = as.character(re[k,"D_VALIDO_DAL_T"]), 
                               data.fine = as.character(re[k,"D_VALIDO_AL_T"]), profilo= as.character(re[k,"PROFILO_PRELIEVO"]), 
                               consumo = cons)
       colnames(agg) <- c("prodotto", "data inizio", "data fine", "profilo", "consumo")
       profili_temp <- data.frame(compute_profiles_DEF15(agg, prof))
       colnames(profili_temp) <- prof[,1]
       for(ds in dfs2)
       {
         if(pdr %in% rownames(get(ds)))
         {
           DF <- correct_obs(get(ds))
           my <- get_month_year(ds)
           month <- map_months(my[1])
           index <- take_month(month, my[2], profili_temp)
           index2 <- which(rownames(DF) == pdr)
           profili_temp[,index] <- DF[index2,]
         }
       }
      profili <- profili + profili_temp
      }
    max_rf <- rep(0, length(dfs))
    for(j in 1:length(dfs))
    {
      #rf <- rownames(M)[i]
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
    mypath <- paste0("C:/Users/d_floriello/Documents/plot_remi/plot_true_",rf, ".jpg")
    jpeg(file=mypath, quality=100)
    plot(1:length(profili), profili, type="l",ylim = c(0, mm+1),xlab="giorni", ylab="consumo",main = paste("fabbisogno per remi dati storici", rf, "con max allocazione mensile"))
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

xlsx::write.xlsx(data.frame(diff_matrix), paste0("C:/Users/d_floriello/Documents/plot_remi/differenze_dati_veri.xlsx"), row.names=TRUE, col.names = TRUE)

matplot(1:24, t(diff_matrix), type="l",lwd=2, xlab="mesi", ylab="differenze", main="grafico differenze mensili curve storiche - allocato SNAM")
lines(1:24, colMeans(diff_matrix), type="o", lwd=2)

#####################################################################
############## matrice M per l'ottimizzazione ###############
#####################################################################
M2 <- matrix(0,nrow = length(remi_found), ncol = 48)
## un remi per ogni riga e nelle colonne dispari ci sono le CG per mese j, mentre nelle colonne pari
## ci sono i max allocati snam per mese j-1.
rownames(M2) <- remi_found
for(rf in remi_found)
{
  re <- ver[which(ver["COD_REMI"] == rf),]
  CG <- rep(0, 24)
  {
    if(nrow(re) > 0)
    {
      for(k in 1:nrow(re))
      {
        CG_temp <- rep(0, 24)
        pdr <- unique(unlist(re[k,"PDR"]))
        cons <- ifelse(re[k,"CONSUMO_DISTRIBUTORE"] != "0", as.numeric(as.character(re[k,"CONSUMO_DISTRIBUTORE"])), as.numeric(as.character(re[k,"CONSUMO_CONTR_ANNUO"])))
        agg <- data_frame(prodotto = re[k,"CODICE_PRODOTTO"], data.inizio = as.character(re[k,"D_VALIDO_DAL_T"]), 
                          data.fine = as.character(re[k,"D_VALIDO_AL_T"]), profilo= as.character(re[k,"PROFILO_PRELIEVO"]), 
                          consumo = cons)
        colnames(agg) <- c("prodotto", "data inizio", "data fine", "profilo", "consumo")
        #profili_temp <- data.frame(compute_profiles_DEF15(agg, prof))
        CG_temp <- compute_max_prof(agg, prof)
        #colnames(profili_temp) <- prof[,1]
        for(ds in dfs2)
        {
          if(pdr %in% rownames(get(ds)))
          {
            DF <- correct_obs(get(ds))
            my <- get_month_year(ds)
            month <- map_months(my[1])
            #index <- take_month(month, my[2], profili_temp)
            index <- which(dfs2 == ds)
            index2 <- which(rownames(DF) == pdr)
            CG_temp[index+2] <- max(DF[index2,])
          }
        }
        CG_temp <- CG_temp*active(agg$`data inizio`, agg$`data fine`)
        CG <- CG + CG_temp
      }
      max_rf <- rep(0, length(dfs))
      for(j in 1:length(dfs))
      {
        if(rf %in% get(dfs[j])[,1]) max_rf[j] <- get(dfs[j])[which(get(dfs[j])[,1] == rf),2] 
      }
      names(max_rf) <- c("03/2015","04/2015","05/2015","06/2015","07/2015","08/2015","09/2015",
                         "10/2015","11/2015","12/2015","01/2016","02/2016","03/2016","04/2016")
      max_rf <- c(0,0,max_rf,rep(0,8))
      index <- which(rownames(M2) == rf)
      for(j in 1:24)
      {
        M2[index,j] <- CG[j]
        M2[index,(24+j)] <- max_rf[j]
      }
    }
  }
}

il <- c()
for(i in 1:nrow(M2))
{
  if(sum(M2[i,]) != 0) il <- c(il, i)
}
M2 <- M2[il,]

####################################################################################
####### ottimizzazione banda di sicurezza su database storico #############
####################################################################################

Fh <- function(x, remi, M2)
{
  S <- 0
  M2 <- M2[which(rownames(M2) == remi),c(6:24,30:48)]
  if(length(M2) > 0)
  {
    for(j in 1:19)
    {
      y <- M2[j]*((1.1)+x)
      y2 <- M2[j]*(1+x)
#      y <- M2[j]*(0.9 - x)
      z <- unlist(M2[j+19] - M2[j]*(1.1))
      z2 <- unlist(M2[j+19] - y)
      if(z2 > 0.1*y)
      {
        S <- S + 0.03*M2[j]*(1+x) + 3.5*(abs(z2) - 0.03*M2[j]*(1+x));
#        S <- S + 0.03*M2[j]*(1+x) + 3.5*(abs(z2))
      }
      else
      {
        S <- S + 0.03*M2[j]*(1+x) 
      }
    }
    return(S)
  }
}

Fhabs <- function(remi, M2)
{
  M2 <- M2[which(rownames(M2) == remi),c(6:24,30:48)]
  xx <- seq(0, 2*max(unlist(M2)), 10)
  f <- c()
  if(length(M2) > 0)
  {
    for(x in 1:length(xx))
    {
      S <- 0
      for(dx in x:length(xx))
      {
        for(j in 1:19)
        {
          y <- xx[x]
          z <- unlist(M2[j+19] - y)
          if(-z > 0.1*y)
          {
            S <- S + 0.03*xx[x] + 3.5*(abs(z) - 0.03*(xx[dx] - xx[x]))
          }
          else
          {
            S <- S + 0.03*xx[x] + 0.03*(xx[dx] - xx[x])
          }
        }
      }
      f <- c(f, S)
    }
  }
  plot(xx,f,type="l",lwd=2, col="blue",xlab= "capacita' in m3", ylab="euro", main=rf)
  return(f)
}


Fhd <- function(remi, M2)
{
  xx <- seq(-1, 1, 0.0001)
  M3 <- M2[which(rownames(M2) == remi),c(6:24,30:48)]
  mm <- max(M3[1:18])
  f <- c()
  if(length(M3) > 0)
  {
    for(x in xx)
    {
      S <- 0
      #index <- which(xx == x)
      for(j in 1:19)
      {
        y <- mm*(1+x)
        z <- M3[j+19] - 1.1*y
        #if(z > 0.1*y)
        if(z > 0)
        {
          S <- S + 3/12*mm*(1+x) + 3.5*(abs(z))
        }
        else
        {
          S <- S + 3/12*mm*(1+x) 
        }
      }
      f <- c(f, S)
    }
  }
  plot(xx,f,type="l",lwd=2, col="red",xlab= "% banda di sicurezza", ylab="euro", main=rf)
  return(c(f,mm))
}



xx <- seq(-1, 1, 0.0001)

sol <- matrix(0, nrow = nrow(M2), ncol = 3)
rownames(sol) <- rownames(M2)
colnames(sol) <- c("% ottima", "capacita ottima", "minimo costo")
for(rf in rownames(M2))  
{
  i <- which(rownames(sol) == rf)
  s <- Fhd(rf, M2)
  sol[i,1] <- xx[which.min(s[1:(length(s)-1)])]
  sol[i,2] <- s[length(s)]*(1+sol[i,1])
  sol[i,3] <- min(s[1:(length(s)-1)])
 # for(x in xx)
 # {
    #s <- c(s,Fh(x, rf, M2))
    #Fhabs(rf, M2)
    
#  }
#  plot(xx,s,type="l",lwd=2, col="blue",xlab= "banda di sicurezza", ylab="euro", main=rf)
  #S <- S + s
}

#####################################################################################
####### ottimizzazione remi-wise con anche effetto di riconciliazione ##############
#####################################################################################
M3 <- matrix(0,nrow = nrow(M2), ncol = 48)
rownames(M3) <- rownames(M2)
M4 <- matrix(0,nrow = nrow(M2), ncol = 48)
rownames(M4) <- rownames(M2)
M5 <- matrix(0,nrow = nrow(M2), ncol = 48)
rownames(M5) <- rownames(M2)
M6 <- matrix(0,nrow = nrow(M2), ncol = 48)
rownames(M6) <- rownames(M2)

for(rf in rownames(M2))
{
  re <- ver[which(ver["COD_REMI"] == rf),]
  CG <- CG_hp <- CGr <- CGrhp <- rep(0, 24)
  {
    if(nrow(re) > 0)
    {
      max_rf2 <- rep(0, length(dfs))
      for(j in 1:length(dfs))
      {
        if(rf %in% get(dfs[j])[,1]) max_rf2[j] <- get(dfs[j])[which(get(dfs[j])[,1] == rf),2] 
      }
      names(max_rf2) <- c("03/2015","04/2015","05/2015","06/2015","07/2015","08/2015","09/2015",
                         "10/2015","11/2015","12/2015","01/2016","02/2016","03/2016","04/2016")
      max_rf <- c(0,0,max_rf2,rep(0,8))
      
      sum_remi <- compute_sum_remi(re)
      
      for(k in 1:nrow(re))
      {
        CG_temp <-  CG_temp_hp <- CGr_temp <- CGrhp_temp <- rep(0, 24)
        pdr <- unique(unlist(re[k,"PDR"]))
        cons <- ifelse(re[k,"CONSUMO_DISTRIBUTORE"] != "0", as.numeric(as.character(re[k,"CONSUMO_DISTRIBUTORE"])), as.numeric(as.character(re[k,"CONSUMO_CONTR_ANNUO"])))
        agg <- data_frame(prodotto = re[k,"CODICE_PRODOTTO"], data.inizio = as.character(re[k,"D_VALIDO_DAL_T"]), 
                          data.fine = as.character(re[k,"D_VALIDO_AL_T"]), profilo= as.character(re[k,"PROFILO_PRELIEVO"]), 
                          consumo = cons)
        colnames(agg) <- c("prodotto", "data inizio", "data fine", "profilo", "consumo")
        #profili_temp <- data.frame(compute_profiles_DEF15(agg, prof))
        CG_temp <- compute_max_prof(agg, prof)
        CG_temp_hp <- compute_max_prof_hp(agg,prof,max_rf,sum_remi)
        #colnames(profili_temp) <- prof[,1]
        CGr_temp <- compute_max_prof_riconciliazione(agg, prof)
        CGrhp_temp <- compute_max_prof_riconciliazione_hp(agg,prof,max_rf,sum_remi)
        for(ds in dfs2)
        {
          if(pdr %in% rownames(get(ds)))
          {
            DF <- correct_obs(get(ds))
            my <- get_month_year(ds)
            month <- map_months(my[1])
            #index <- take_month(month, my[2], profili_temp)
            index <- which(dfs2 == ds)
            index2 <- which(rownames(DF) == pdr)
            CG_temp[index+2] <- max(DF[index2,])
            CG_temp_hp[index+2] <- max(DF[index2,])
            CGr_temp[index+2] <- max(DF[index2,])
            CGrhp_temp[index+2] <- max(DF[index2,])
          }
        }
        CG_temp <- CG_temp*active(agg$`data inizio`, agg$`data fine`)
        CG_temp_hp <- CG_temp_hp*active(agg$`data inizio`, agg$`data fine`)
        CGr_temp <- CGr_temp*active(agg$`data inizio`, agg$`data fine`)
        CGrhp_temp <- CGrhp_temp*active(agg$`data inizio`, agg$`data fine`)
        
        #print(CG_temp);print(CG_temp_hp);print(CGr_temp);print(CGrhp_temp);
        
        CG <- CG + CG_temp
        CG_hp <- CG_hp + CG_temp_hp
        CGr <- CGr + CGr_temp
        CGrhp <- CGrhp + CGrhp_temp
      }
      
      index6 <- which(rownames(M6) == rf)
      index3 <- which(rownames(M3) == rf)
      index4 <- which(rownames(M4) == rf)
      index5 <- which(rownames(M5) == rf)
      for(j in 1:24)
      {
        M6[index6,j] <- CG[j]
        M6[index6,(24+j)] <- max_rf[j]
        M3[index3,j] <- CG_hp[j]
        M3[index3,(24+j)] <- max_rf[j]
        M4[index4,j] <- CGr[j]
        M4[index4,(24+j)] <- max_rf[j]
        M5[index5,j] <- CGrhp[j]
        M5[index5,(24+j)] <- max_rf[j]
      }
    }
  }
}

xlsx::write.xlsx(data.frame(M2), paste0("C:/Users/d_floriello/Documents/plot_remi/mat_ottimizzazione_CA_vero.xlsx"), row.names=TRUE, col.names = TRUE)
xlsx::write.xlsx(data.frame(M3), paste0("C:/Users/d_floriello/Documents/plot_remi/mat_ottimizzazione_CA_stima.xlsx"), row.names=TRUE, col.names = TRUE)
xlsx::write.xlsx(data.frame(M4), paste0("C:/Users/d_floriello/Documents/plot_remi/mat_ottimizzazione_RIC_vero.xlsx"), row.names=TRUE, col.names = TRUE)
xlsx::write.xlsx(data.frame(M5), paste0("C:/Users/d_floriello/Documents/plot_remi/mat_ottimizzazione_RIC_stima.xlsx"), row.names=TRUE, col.names = TRUE)

M2 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/plot_remi/mat_ottimizzazione_CA_vero.xlsx", sheet = 1, rowNames = TRUE, colNames = TRUE)
M3 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/plot_remi/mat_ottimizzazione_CA_stima.xlsx", sheet = 1, rowNames = TRUE, colNames = TRUE)
M4 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/plot_remi/mat_ottimizzazione_RIC_vero.xlsx", sheet = 1, rowNames = TRUE, colNames = TRUE)
M5 <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/plot_remi/mat_ottimizzazione_RIC_stima.xlsx", sheet = 1, rowNames = TRUE, colNames = TRUE)

M2 <- as.matrix(data.matrix(data.frame(M2)))
M3 <- as.matrix(data.matrix(data.frame(M3)))
M4 <- as.matrix(data.matrix(data.frame(M4)))
M5 <- as.matrix(data.matrix(data.frame(M5)))

Sol <- matrix(0,nrow = nrow(M2), ncol = 12)
rownames(Sol) <- rownames(M2)
colnames(Sol) <- c("% ottima CA teorico", "capacita ottima CA teorico", "minimo costo CA teorico",
                   "% ottima CA stima", "capacita ottima CA stima", "minimo costo CA stima",
                   "% ottima RIC teorico", "capacita ottima RIC teorico", "minimo costo RIC teorico",
                   "% ottima RIC stima", "capacita ottima RIC stima", "minimo costo RIC stima")
xx <- seq(-1, 1, 0.001)
for(rf in rownames(M2))  
{
  i <- which(rownames(Sol) == rf)
#  s1 <- Fhd(rf, M2); s2 <- Fhd(rf, M3); s3 <- Fhd(rf, M4); s4 <- Fhd(rf, M5)
  s1 <- Fhdn(rf, M2); s2 <- Fhdn(rf, M3); s3 <- Fhdn(rf, M4); s4 <- Fhdn(rf, M5)
  
  Sol[i,1] <- xx[which.min(s1[1:(length(s1)-1)])]
  Sol[i,2] <- unlist(s1[length(s1)])*(1+Sol[i,1])
  Sol[i,3] <- min(s1[1:(length(s1)-1)])
  
  Sol[i,4] <- xx[which.min(s2[1:(length(s2)-1)])]
  Sol[i,5] <- unlist(s2[length(s2)])*(1+Sol[i,4])
  Sol[i,6] <- min(s2[1:(length(s2)-1)])
  
  Sol[i,7] <- xx[which.min(s3[1:(length(s3)-1)])]
  Sol[i,8] <- unlist(s3[length(s3)])*(1+Sol[i,7])
  Sol[i,9] <- min(s3[1:(length(s3)-1)])
  
  Sol[i,10] <- xx[which.min(s4[1:(length(s4)-1)])]
  Sol[i,11] <- unlist(s4[length(s4)])*(1+Sol[i,10])
  Sol[i,12] <- min(s4[1:(length(s4)-1)])
}

result <- sumprod_by_cluster(Sol, "", npdr)

npdr <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/plot_remi/num_pdr150.xlsx", sheet = 1, rowNames = TRUE, colNames = TRUE)


rf <- "34216701"
s1 <- Fhd(rf, M2)
M2[which(rownames(M2) == rf),]

xlsx::write.xlsx(data.frame(Sol), paste0("C:/Users/d_floriello/Documents/plot_remi/SOL_OTTIMIZZAZIONE.xlsx"), row.names=TRUE, col.names = TRUE)

perc_ott <- Sol[,1]
perc_ric <- Sol[,10]

length(perc_ott[perc_ott > 0])/length(perc_ott)
length(perc_ric[perc_ric > 0])

hist(perc_ott, main = "istogramma percentuale sicurezza ottima CA")
hist(perc_ric, main = "istogramma percentuale sicurezza ottima RIC")

mean(perc_ott[perc_ott > 0])
mean(perc_ric[perc_ric > 0])
median(perc_ott[perc_ott > 0])
median(perc_ric[perc_ric > 0])

length(which(perc_ott != 0))
length(which(perc_ric != 0))

confronto <- Sol[,c(1,4,7,10)]

r1 <- which(confronto[,1] > 0)
r2 <- which(confronto[,4] > 0)

both <- confronto[intersect(r1, r2),c(1,4)]

remi_CA <- rownames(confronto[which(confronto[,1] > 0),])
remi_RIC <- rownames(confronto[which(confronto[,4] > 0),])

pdr_ca <- c()
for(r in remi_CA)
{
  pdr_ca <- c(pdr_ca, length(unique(unlist(ver2[which(ver["COD_REMI"] == r),"PDR"]))))
}

pdr_ric <- c()
for(r in remi_RIC)
{
  pdr_ric <- c(pdr_ric, length(unique(unlist(ver2[which(ver["COD_REMI"] == r),"PDR"]))))
}

mean(pdr_ric)
mean(pdr_ca)
median(pdr_ric)
median(pdr_ca)
var(pdr_ric)
var(pdr_ca)

hist(pdr_ric)
hist(pdr_ca)

magg_ca <- rownames(both[which(both[,1] >= both[,2]),])
magg_ric <- rownames(both[which(both[,1] < both[,2]),])

#### grandezza remi su cui PREVALE l'effetto RIC su CA (quindi ci devono essere entrambi)
p_ric <- c()
for(r in magg_ric)
{
  p_ric <- c(p_ric, length(unique(unlist(ver2[which(ver["COD_REMI"] == r),"PDR"]))))
}

mean(p_ric)
median(p_ric)
var(p_ric)
### grandezza remi su cui PREVALE CA
p_ca <- c()
for(r in magg_ca)
{
  p_ca <- c(p_ca, length(unique(unlist(ver2[which(ver["COD_REMI"] == r),"PDR"]))))
}

mean(p_ca)
median(p_ca)
var(p_ca)
######################################################################################
############### ottimizzazione con clusterizzazione ##################################
######################################################################################
ver2 <- ver2[which(ver["SHIPPER"] == "0001808491-AXOPOWER SRL"),]


f10 <- cluster_by_and_optimise(ver2, M2, from = 0, to = 11)
f10_ric <- cluster_by_and_optimise(ver2, M5, from = 0, to = 11)
f30 <- cluster_by_and_optimise(ver2, M2, from = 11, to = 31)
f30_ric <- cluster_by_and_optimise(ver2, M5, from = 11, to = 31)
finf <- cluster_by_and_optimise(ver2, M2, from = 31, to = 150)
finf_ric <- cluster_by_and_optimise(ver2, M5, from = 31, to = 150)

xx <- seq(-1, 2, 0.001)
Sol_agg <- data.frame(min(f10), xx[which.min(f10)],
                      min(f10_ric), xx[which.min(f10_ric)],
                      min(f30), xx[which.min(f30)],
                      min(f30_ric), xx[which.min(f30_ric)],
                      min(finf), xx[which.min(finf)],
                      min(finf_ric), xx[which.min(finf_ric)])

colnames(Sol_agg) <- c("costo minimo CA cluster 10", "% ottima CA cluster 10",
                       "costo minimo RIC cluster 10", "% ottima RIC cluster 10",
                       "costo minimo CA cluster 30", "% ottima CA cluster 30",
                       "costo minimo RIC cluster 30", "% ottima RIC cluster 30",
                       "costo minimo CA cluster >30", "% ottima CA cluster >30",
                       "costo minimo RIC cluster >30", "% ottima RIC cluster >30")

xlsx::write.xlsx(Sol_agg, paste0("C:/Users/d_floriello/Documents/plot_remi/SOL_OTTIMIZZAZIONE_AGGREGATA.xlsx"), row.names=TRUE, col.names = TRUE)

###### errore e stima di x* con i consumi storici

es <- matrix(0, nrow=nrow(M),ncol=6)
rownames(es) <- rownames(M)
for(i in 1:nrow(M))
{
  re <- extract_relevant_val(ver[which(ver["COD_REMI"] == rownames(M)[i]),])
  cg <- rep(0,14)
  cont <- 0
  if(nrow(re) > 0)
  {
    for(j in 1:nrow(re)) 
    {
      cons <- ifelse(re[j,"CONSUMO_DISTRIBUTORE"] != "0", as.numeric(as.character(re[j,"CONSUMO_DISTRIBUTORE"])), as.numeric(as.character(re[j,"CONSUMO_CONTR_ANNUO"])))
      prodotto <- as.character(re[j,"CODICE_PRODOTTO"]); data.inizio  <- as.character(re[j,"D_VALIDO_DAL_T"]); 
      data.fine <- as.character(re[j,"D_VALIDO_AL_T"]); profilo <- as.character(re[j,"PROFILO_PRELIEVO"]) 
      COL <- which(names(prof) == profilo)
      mpm <- take_max_permonth(prof,COL)
      act <- active(data.inizio, data.fine)
      mat <- cons * (mpm/100) * act
      pdr <- as.character(re[j,"PDR"])
#      ii <- which(re["PDR"] == pdr)
      for(ds in dfs2)
      {
#        print(ds)
        index <- which(dfs2 == ds)
#        print(paste("index qui:", index))
        if(pdr %in% rownames(get(ds)))
        {
          cont <- cont + 1
          D <- correct_obs(get(ds))
          cg[index] <- cg[index] + max(D[which(rownames(D) == pdr),])
#          print(paste("index:", index))
#          print(paste("cg index:", cg[index]))
        }
        else
        {
          cg[index] <- cg[index] + mat[index]
#          print(paste("index:", index))
#          print(paste("cg index:", cg[index]))
#          print(paste("mat index:", mat[index]))
        }
      }
  }
  ms <- M[i, 27:40]
  cgs <- cg_star[which(rownames(cg_star) == rownames(M)[i]),1]
#  print(paste("ms:", ms))
  error <- ms - 1.1*cg
#  print(paste("error:", error))
  val <- sum(3*cg) + sum(as.numeric(error[4:14] < 0)*abs(ms[4:14] - 1.1*cg[4:14])*3.5)
#  print(paste("val:", val))
  es[i,1] <- mean(error)
  es[i,2] <- val
  es[i,3] <- mean(ms - 1.1*cgs)
  es[i,4] <- sum(3*cgs) + sum(as.numeric(error[4:14] < 0)*abs(ms[4:14] - 1.1*cgs)*3.5)
  es[i,5] <- cont/(nrow(re)*11)
  es[i,6] <- nrow(re)
  }
}

xlsx::write.xlsx(data.frame(es), "errore_su_storico.xlsx", row.names=TRUE, col.names = TRUE)

################################################################################
################### confronto dati contrattuali e distributore #################
################################################################################
ver <- openxlsx::read.xlsx("Report_214.xlsx", sheet = 1, colNames = TRUE)
ver[is.na(ver)] <- 0
pm <- openxlsx::read.xlsx("profili_mensili.xlsx", sheet = 1, colNames = TRUE)
ver2 <- extract_relevant_val(ver[which(ver["CONSUMO_DISTRIBUTORE"] != "0"),])

comp <- comparison_data(ver2,pm)

Ms <- M2[,25:48]
Msy <- Ms[,5:16]

comp2 <- comp[which(rownames(comp) %in% rownames(Msy)),]
Msy <- Msy[which(rownames(Msy) %in% rownames(comp)),]

LCc <- LDc <- matrix(0,nrow=nrow(comp2),ncol=12)
rownames(LCc) <- rownames(LDc) <- rownames(comp2)
for(rn in rownames(comp2))
{
  i <- which(rownames(Msy) == rn)
  i2 <- which(rownames(comp2) == rn)
  ic <- which(rownames(LCc) == rn)
  id <- which(rownames(LDc) == rn)
  
  LCc[ic,] <- maply(1:12, function(n) Msy[i,n] - comp2[i2,n+1])
  LDc[id,] <- maply(1:12, function(n) Msy[i,n] - comp2[i2,n+14])
  
}
matplot(t(LCc), type="l",lwd=2, ylab = "diff", main = "curve delle differenze allocato - consumo contr")
matplot(t(LDc), type="l",lwd=2, ylab = "diff", main = "curve delle differenze allocato - consumo distr")

colMeans(LCc)
colMeans(LDc)

plot(colMeans(LCc), type="l",lwd=2, ylab = "diff", main = "curve delle differenze medie allocato - consumo")
lines(colMeans(LDc), type="l",lwd=2,col="blue")

mean(colMeans(LCc))
mean(colMeans(LDc))

apply(LCc, 2, var)
apply(LDc, 2, var)

mean(apply(LCc, 2, var))
mean(apply(LDc, 2, var))

var(apply(LCc, 2, var))
var(apply(LDc, 2, var))

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
