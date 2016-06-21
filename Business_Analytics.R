#### Business Analytics ####
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
library(ggplot2)

source("Functions_for_BA.R")
data <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/Business Analysis/TestP.xlsx", sheet = 2, colNames = TRUE)
data[is.na(data)] <- 0; data[is.null(data)] <- 0

tot_cons <- sum(as.numeric(unlist(data["Pot.kWh/Ann."])))
tcM <- tot_cons/1000

data["Pot.kWh/Ann."] <- as.numeric(unlist(data["Pot.kWh/Ann."]))/tot_cons

op <- order(data["Pot.kWh/Ann."], decreasing = FALSE)
pE <- sort(as.numeric(unlist(data["Pot.kWh/Ann."])), decreasing=FALSE)

lasts <- c()
for(i in nrow(data):1)
{
  lasts <- c(lasts, i)
  print("lasts:"); print(lasts)
  others <- setdiff(1:nrow(data), lasts)
  #print(paste("others",others))
  #skpe <- sum(as.numeric(unlist(data[others,"Pot.kWh/Ann."]))) - sum(as.numeric(unlist(data[lasts,"Pot.kWh/Ann."])))
  skpe <- sum(pE[others]) - sum(pE[lasts])
  print(paste("left", sum(pE[others]))); print(paste("left", sum(pE[lasts])))
  print( paste("diff", skpe))
  if(skpe <= 0) break
}
## clienti pivotali:
data[op[lasts],]
sum(as.numeric(unlist(data[op[lasts], 7])))

boxplot(tot_cons*as.numeric(unlist(data["Pot.kWh/Ann."])))

##########################################################################################
## analisi consumi energia ###############################################

pods <- unique(unlist(data["Codice.POD"]))
for(pod in pods)
{
  cust <- data[which(data["Codice.POD"] == pod),]
  plot(1:nrow(cust), unlist(cust["Energia"]), type="o", lwd=2,col="red",xlab="mese",ylab="kWh",main = paste("consumo per POD", pod))
}

data <- compute_margine_netto(data)
TAU <- c()
for(pod in pods)
{
  TAU <- c(TAU, IRG(data, pod))
}

vs <- IRG_vs_edl(data)
vs2 <- cbind(as.numeric(as.character(unlist(vs["Energia media annua"])))/1000,as.numeric(as.character(unlist(vs["IRG"]))),unlist(vs["listino"]))
vs3 <- cbind(as.numeric(as.character(unlist(vs["durata"]))),as.numeric(as.character(unlist(vs["IRG"]))),unlist(vs["listino"]))


##pairs(cbind(as.numeric(unlist(vs["IRG"])), as.numeric(unlist(vs["Energia TOT"])), as.numeric(unlist(vs["durata"]))))
hist(as.numeric(as.character(unlist(vs["IRG"]))), breaks = seq(-1,max(as.numeric(as.character(unlist(vs["IRG"])))),1))
plot(as.numeric(as.character(unlist(vs["Energia media annua"])))/1000, as.numeric(as.character(unlist(vs["IRG"]))), pch=16, lwd=2,col="navy",
     xlab="energia media annua in GWh", ylab="IRG", main="IRG vs Energia media annua")
plot(as.numeric(as.character(unlist(vs["durata"]))), as.numeric(as.character(unlist(vs["IRG"]))), pch=16, lwd=2,col="pink",
     xlab="durata in mesi", ylab="IRG", main="IRG vs durata")
plot(unlist(vs["listino"]), as.numeric(as.character(unlist(vs["IRG"]))), pch=16, lwd=2,col="green",
     xlab="listino", ylab="IRG", main="IRG vs listino")
plot(unlist(vs["listino"]), as.numeric(as.character(unlist(vs["Energia media annua"]))),col=1:4,
     xlab="listino", ylab="Energia media annua", main="Energia media annua vs listino")

a <- ggplot(data = as.data.frame(vs2), aes(x = as.numeric(as.character(unlist(vs["Energia media annua"])))/1000, y = as.numeric(as.character(unlist(vs["IRG"]))),
                           col = (vs["listino"])))
a <- a + geom_point(size = 3)
a <- a + xlab("Energia media annua in GWh") + ylab("IRG") + ggtitle("IRG vs energia media annua")  + scale_color_discrete(name = "Listino")
a

a2 <- ggplot(data = as.data.frame(vs3), aes(x = as.numeric(as.character(unlist(vs["durata"]))), y = as.numeric(as.character(unlist(vs["IRG"]))),
                                           col = (vs["listino"])))
a2 <- a2 + geom_point(size = 3)
a2 <- a2 + xlab("durata in mesi") + ylab("IRG") + ggtitle("IRG vs durata")  + scale_color_discrete(name = "Listino")
a2

plot(unlist(vs["listino"]), as.numeric(as.character(unlist(vs["durata"]))),col=4:1,
     xlab="listino", ylab="durata in mesi", main="durata vs listino")

qplot(as.numeric(as.character(unlist(vs["IRG"]))), geom = "histogram", binwidth = 1, xlim = c(-2,17),colour = I("black"), fill = I("white")) +
  xlab("IRG")
#qplot(as.numeric(as.character(unlist(vs["IRG"]))), geom = "density", xlim = c(-2,17))


mean_process <- c()
mean_process_w <- c()
for(i in 1:nrow(vs))
{
  print(UEM_process(data, as.character(vs[i,1])))
  mean_process <- c(mean_process, mean(UEM_process(data, as.character(vs[i,1]))))
  mean_process_w <- c(mean_process_w, mean(UEM_process(data, as.character(vs[i,1])))/100)
}

vs4 <- cbind(vs3[,2], mean_process, vs[,5])

a4 <- ggplot(data = as.data.frame(vs4), aes(x = as.numeric(as.character(unlist(vs4[,2]))), y = as.numeric(as.character(unlist(vs["IRG"]))),
                                            col = (vs["listino"])))
a4 <- a4 + geom_point(size = 3)
a4 <- a4 + xlab("margine unitario energia medio") + ylab("IRG") + ggtitle("IRG vs margine unitario energia medio")  + scale_color_discrete(name = "Listino")
a4


less <- vs[which(TAU > 0 & TAU <= 6),]
more <- vs[which(TAU > 6),]

hist(as.numeric(as.character(less[,3])),breaks = seq(0,7000,100))
hist(as.numeric(as.character(more[,3])),breaks = seq(0,7000,100))
hist(as.numeric(as.character(less[,4])),breaks = seq(0,45,5))
hist(as.numeric(as.character(more[,4])),breaks = seq(0,45,5))

#################################################################################################
#################################################################################################

nomi <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/Business Analysis/MAG16.xlsm", sheet = "Agenti", colNames = FALSE)

D <- data_frame()
for(i in 4:70)
{
    data <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/Business Analysis/MAG16.xlsm", sheet = i, colNames = TRUE)
    data[is.na(data)] <- 0
    data <- create_dataset_BA(data)
    df <- data.frame(IRG_vs_edl(data),agenzia = nomi[i-3,1])
    D <- rbind.data.frame(D, df)
    
    act <- active(data)
    gdp <- get_duration_processes(act)
    
    mm <- max(c(max(abs(gdp[[1]])),max(abs(gdp[[2]])), max(abs(gdp[[3]]))))
    
    plot(1:48, gdp[[1]], type="o",lwd=2,col="blue",ylim=c(-mm,mm),xlab="mesi",ylab="numero clienti", main=nomi[i-3,1])
    lines(1:48, gdp[[2]], type="o",lwd=2,col="green")
    lines(1:48, gdp[[3]], type="o",lwd=2,col="red")

}

a <- ggplot(data = D, aes(x = as.numeric(as.character(unlist(D["Energia.media.annua"])))/1000, y = as.numeric(as.character(unlist(D["IRG"]))),
                                           col = (D["agenzia"])))
a <- a + geom_point(size = 3)
a <- a + xlab("Energia media annua in GWh") + ylab("IRG") + ggtitle("IRG vs energia media annua")  + scale_color_discrete(name = "agenzia")
a

a2 <- ggplot(data = D, aes(x = as.numeric(as.character(unlist(D["Energia.media.annua"])))/1000, y = as.numeric(as.character(unlist(D["durata"]))),
                          col = (D["agenzia"])))
a2 <- a2 + geom_point(size = 3)
a2 <- a2 + xlab("Energia media annua in GWh") + ylab("durata in mesi") + ggtitle("durata vs energia media annua")  + scale_color_discrete(name = "agenzia")
a2

a3 <- ggplot(data = D, aes(x = as.numeric(as.character(unlist(D["Energia.media.annua"])))/1000, y = as.numeric(as.character(unlist(D["IRG"]))),
                          col = (D["listino"])))
a3 <- a3 + geom_point(size = 3)
a3 <- a3 + xlab("Energia media annua in GWh") + ylab("IRG") + ggtitle("IRG vs energia media annua")  + scale_color_discrete(name = "listino")
a3
