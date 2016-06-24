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
library(jpeg)

nomi <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/Business Analysis/MAG16.xlsm", sheet = "Agenti", colNames = FALSE)

D <- data_frame()
DW <- data_frame()
rpa <-rpa2 <- pgm <- mr <- taup <- taun <- energy <- dur <- rep(0, 67)
names(rpa) <- names(rpa2) <- names(pgm) <- names(mr) <- names(taup) <- names(taun) <- names(energy) <- names(dur) <- nomi[,1]
for(i in 4:70)
{
    data <- openxlsx::read.xlsx("C:/Users/d_floriello/Documents/Business Analysis/MAG16.xlsm", sheet = i, colNames = TRUE)
    data[is.na(data)] <- 0
    data <- create_dataset_BA(data)
    df <- data.frame(IRG_vs_edl(data),agenzia = nomi[i-3,1])
    dfw <- data.frame(weigthing_coin(data), agenzia = nomi[i-3,1])
    D <- rbind.data.frame(D, df)
    DW <- rbind.data.frame(DW, dfw)
    
    act <- active(data)
    gdp <- get_duration_processes(act)
    Tt <- get_duration_statistics(gdp[[1]],gdp[[3]],gdp[[2]])
    
    rpa[i-3] <- mean(Tt)
    pgm[i-3] <- mean(dfw[,2])
    rpa2[i-3] <- rolling_clienti_pod(data)[1]
    mr[i-3] <- marginality_ratio_finale(data)
    energy[i-3] <- EM_agenzia(data)
    dur[i-3] <- DM_agenzia(data)
    tau_ag <- IRG_agenzia(data)
    
    taup[i-3] <- tau_ag[1]
    taun[i-3] <- tau_ag[2]
    
    mm <- max(c(max(abs(gdp[[1]])),max(abs(gdp[[2]])), max(abs(gdp[[3]]))))
    mypath <- paste0("C:/Users/d_floriello/Documents/Business Analysis/plot_",nomi[i-3,1],".jpg")
    jpeg(file=mypath, quality=100)
    plot(1:48, gdp[[1]], type="o",lwd=2,col="blue",ylim=c(-mm,mm),xlab="mesi",ylab="numero clienti", main=nomi[i-3,1])
    lines(1:48, gdp[[2]], type="o",lwd=2,col="green")
    lines(1:48, gdp[[3]], type="o",lwd=2,col="red")
    lines(1:48, Tt, type="o",lwd=2,col="black")
    dev.off()
}
colnames(DW) <- c("gettone_per_ricorrente","gettone_per_margine","gettone_per_ric_unit","energia_media_annua","agenzia")

### prova di smoothing processo At ###
At <- gdp[[2]]
for(i in 1:length(At))
{
  argvals = seq(0,1,len=length(At))
  nbasis = 20
  basisobj = create.bspline.basis(c(0,1),nbasis)
  Ys = smooth.basis(argvals=argvals, y=At, fdParobj=basisobj,returnMatrix=TRUE)
  plot(Ys)
  lines(argvals, At,type="l", col = "blue")
}

dYs <- deriv.fd(as.fd(Ys), Lfdobj = int2Lfd(1),returnMatrix = TRUE)
plot(dYs)
########## grafici ###############
### peso gettone vs rolling pod
bb <- data.frame(rpa2,pgm)
bb[is.na(bb)] <- 0

bb <- bb[which(bb[,2] > 0 & bb[,1] > 0),]

#fit <- lm(rpa2~I(pgm - 0.4))
fit <- lm(bb[,1]~I(bb[,2] - 0.4))
summary(fit)

b <- ggplot(data = bb, aes(x = as.numeric(as.character(unlist(bb["rpa2"]))), y = as.numeric(as.character(unlist(bb["pgm"]))),
                          col = (rownames(bb))))
b <- b + geom_point(size = 3) + geom_smooth(method="lm", formula = as.numeric(as.character(unlist(bb["pgm"])))~as.numeric(as.character(unlist(bb["rpa2"])))) 
b <- b + xlab("rolling medio per agenzia") + ylab("peso del gettone") + ggtitle("peso del gettone vs rolling pod")  + scale_color_discrete(name = "agenzia")
b

ggplot(bb, aes(x=(bb[,2]), y=bb[,1])) + geom_point() + geom_smooth(method=lm) + ylab("rolling medio per agenzia") + xlab("peso del gettone") + ggtitle("peso del gettone vs rolling pod")  + scale_color_discrete(name = "agenzia")

### peso gettone vs marginalita'
bb1 <- data.frame(mr,pgm)
bb1[is.na(bb1)] <- 0
bb1 <- bb1[which(bb1[,2] > 0 & bb1[,1] > 0),]

fit2 <- lm(mr~I(pgm - 0.4))
summary(fit2)

b1 <- ggplot(data = bb1, aes(x = bb1["mr"], y = bb1["pgm"],col =rownames(bb1)))
b1 <- b1 + geom_point(size = 3) 
b1 <- b1 + xlab("rapporto marginalita'") + ylab("peso del gettone") + ggtitle("peso del gettone vs rapporto marginalita'")  + scale_color_discrete(name = "agenzia")
b1

ggplot(bb1, aes(x=(bb1[,2]), y=bb1[,1])) + geom_point() + geom_smooth(method=lm) + ylab("rapporto marginalita'") + xlab("peso del gettone") + ggtitle("peso del gettone vs rapporto marginalita'")

### peso gettone vs IRG agenzia
bb2 <- data.frame(taup,pgm)
bb2[is.na(bb2)] <- 0
bb2 <- bb2[which(bb2[,2] > 0 & bb2[,1] > 0),]

fit3 <- lm(bb2[,1]~I(bb2[,2] - 0.4))
summary(fit3)

b2 <- ggplot(data = bb2, aes(x = bb2["pgm"], y = bb2["taup"],col =rownames(bb2)))
b2 <- b2 + geom_point(size = 3) 
b2 <- b2 + xlab("peso del gettone") + ylab("IRG") + ggtitle("IRG vs peso del gettone")  + scale_color_discrete(name = "agenzia")
b2

ggplot(bb2, aes(x=(bb2[,2]), y=bb2[,1])) + geom_point() + geom_smooth(method=lm) + ylab("IRG") + xlab("peso del gettone") + ggtitle("IRG vs peso del gettone")

### rapporto marginalita' vs IRG agenzia
bb3 <- data.frame(taup,mr)
bb3[is.na(bb3)] <- 0
bb3 <- bb3[which(bb3[,2] > 0 & bb3[,1] > 0),]

fit4 <- lm(bb3[,1]~I(bb3[,2]))
summary(fit4)

b3 <- ggplot(data = bb3, aes(x = bb3["mr"], y = bb3["taup"],col =rownames(bb3)))
b3 <- b3 + geom_point(size = 3) 
b3 <- b3 + xlab("rapporto marginalita'") + ylab("IRG") + ggtitle("IRG vs rapporto marginalita'")  + scale_color_discrete(name = "agenzia")
b3

ggplot(bb3, aes(x=(bb3[,2]), y=bb3[,1])) + geom_point() + geom_smooth(method=lm) + geom_text(aes(label=rownames(bb3)),hjust=0, vjust=1) + ylab("IRG") + xlab("rapporto marginalita'") + ggtitle("IRG vs rapporto marginalita'")

### energia media agenzia vs IRG agenzia
bb4 <- data.frame(taup,energy)
bb4[is.na(bb4)] <- 0
bb4 <- bb4[which(bb4[,1] > 0),]

fit5 <- lm(bb4[,1]~I(bb4[,2]))
summary(fit5)

b4 <- ggplot(data = bb4, aes(x = bb4["energy"], y = bb4["taup"],col =rownames(bb4)))
b4 <- b4 + geom_point(size = 3) 
b4 <- b4 + xlab("energia media annua") + ylab("IRG") + ggtitle("IRG vs energia media annua")  + scale_color_discrete(name = "agenzia")
b4

ggplot(bb4, aes(x=(bb4[,2]), y=bb4[,1])) + geom_point() + geom_smooth(method=lm) + geom_text(aes(label=rownames(bb4)),hjust=0, vjust=1) + ylab("IRG") + xlab("energia media annua") + ggtitle("IRG vs energia media annua")

### durata media agenzia vs IRG agenzia
bb5 <- data.frame(taup,dur)
bb5[is.na(bb5)] <- 0
bb5 <- bb5[which(bb5[,1] > 0 & bb5[,1] < 20 & bb5[,2] > 12),]
#bb5 <- bb5[which(bb5[,1] > 0 & bb5[,1] < 20),]


fit6 <- lm(bb5[,1]~I(bb5[,2]))
summary(fit6)

b5 <- ggplot(data = bb5, aes(x = bb5["dur"], y = bb5["taup"],col =rownames(bb5)))
b5 <- b5 + geom_point(size = 3) 
b5 <- b5 + xlab("durata media agenzia") + ylab("IRG") + ggtitle("IRG vs durata media agenzia")  + scale_color_discrete(name = "agenzia")
b5

### durata media agenzia vs energia media agenzia
bb6 <- data.frame(energy,dur)
bb6[is.na(bb6)] <- 0
bb6 <- bb6[which(bb6[,1] > 0 & bb6[,2] > 12),]


b6 <- ggplot(data = bb6, aes(x = bb6["dur"], y = bb6["energy"],col =rownames(bb6)))
b6 <- b6 + geom_point(size = 3) 
b6 <- b6 + xlab("durata media agenzia") + ylab("energia media annua") + ggtitle("energia media annua vs durata media agenzia")  + scale_color_discrete(name = "agenzia")
b6

### durata media agenzia vs rapporto agenzia agenzia
bb7 <- data.frame(mr,dur)
bb7[is.na(bb7)] <- 0
bb7 <- bb7[which(bb7[,1] > 0),]

fit8 <- lm(bb7[,1]~I(bb7[,2]))
summary(fit7)

b7 <- ggplot(data = bb7, aes(x = bb7["mr"], y = bb7["dur"],col =rownames(bb7)))
b7 <- b7 + geom_point(size = 3) 
b7 <- b7 + xlab("rapporto marginalita'") + ylab("durata media energia") + ggtitle("rapporto marginalita' vs durata media agenzia")  + scale_color_discrete(name = "agenzia")
b7

### IRG agenzia vs rapporto agenzia
bb8 <- data.frame(dur,pgm)
bb8[is.na(bb8)] <- 0
bb8 <- bb8[which(bb8[,2] > 0),]

fit9 <- lm(bb8[,1]~I(bb8[,2]))
summary(fit8)

b8 <- ggplot(data = bb8, aes(x = bb8["pgm"], y = bb8["dur"],col = rownames(bb8)))
b8 <- b8 + geom_point(size = 3) 
b8 <- b8 + xlab("peso del gettone") + ylab("durata media agenzia") + ggtitle("durata media vs peso del gettone")  + scale_color_discrete(name = "agenzia")
b8


### peso del gettone vs marginalita' by listino
bb2 <- data.frame(D["IRG"],DW["gettone_per_margine"],D["listino"])
which(is.na(bb2["gettone_per_margine"]))
bb2 <- bb2[which(!is.na(bb2["gettone_per_margine"])),]

b2 <- ggplot(data = bb2, aes(x = bb2["gettone_per_margine"], y = bb2["IRG"],col =bb2["listino"]))
b2 <- b2 + geom_point(size = 3) 
b2 <- b2 + xlab("peso del gettone") + ylab("IRG") + ggtitle("peso del gettone vs IRG")  + scale_color_discrete(name = "listino")
b2

ggplot(bb1, aes(x=(bb1[,2]), y=bb1[,1])) + geom_point() + geom_smooth(method=lm) + ylab("rapporto marginalita'") + xlab("peso del gettone") + ggtitle("peso del gettone vs rapporto marginalita'")

###########
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

a4 <- ggplot(data = DW, aes(x = as.numeric(as.character(unlist(DW["energia_media_annua"])))/1000, y = as.numeric(as.character(unlist(DW["gettone_per_margine"]))),
                           col = (DW["agenzia"])))
a4 <- a4 + geom_point(size = 3)
a4 <- a4 + xlab("Energia media annua in GWh") + ylab("gettone/margine agenzia") + ggtitle("peso del gettone sul margine agenzia vs energia media annua")  + scale_color_discrete(name = "agenzia")
a4

D2 <- data.frame(D["IRG"],DW["gettone_per_margine"],DW["agenzia"])

a5 <- ggplot(data = D2, aes(x = as.numeric(as.character(unlist(D2["IRG"]))), y = as.numeric(as.character(unlist(D2["gettone_per_margine"]))),
                           col = (D2["agenzia"])))
a5 <- a5 + geom_point(size = 3)
a5 <- a5 + xlab("IRG") + ylab("gettone/margine agenzia") + ggtitle("peso del gettone sul margine agenzia vs IRG")  + scale_color_discrete(name = "agenzia")
a5

library(plot3D)
scatter3D(x = as.numeric(as.character(unlist(D2["IRG"]))), y = as.numeric(as.character(unlist(D2["gettone_per_margine"]))),
          z=as.numeric(as.character(unlist(D["Energia.media.annua"])))/1000, pch = 20, phi = 45, bty ="g", colvar = as.numeric(as.character(unlist(D2["IRG"]))),
          xlab = "IRG",
          ylab ="gettone/margine agenzia", zlab = "energia media annua")

scatter3D(x = taup, y = mr,
          z=dur, pch = 20, phi = 30, bty ="g", colvar = taup,
          xlab = "IRG",
          ylab ="rapporto marginalita'", zlab = "durata media agenzia")

#######################################
hc_ema <- hclust(dist(D$Energia.media.annua))
plot(hc_ema)



