######### Functional model for Out Of Sample ###############

library(plyr)
library(dplyr)
library(data.table)
library(lubridate)
library(fda)
library(readxl)
library(MASS)

#############################################################################################
MakeFunctionalDatasetRegressor <- function(oos)
{
  dfr <- data_frame()
  dts <- unique(as.Date(oos$X__1))
  for(d in dts)
  {
    regressor <- oos[min(which(as.Date(oos$X__1) == d)),57:80]
    vars <- c(2:20,45:56)
    chron <- as.data.frame(oos)[min(which(as.Date(oos$X__1) == d)),vars]
    DF <- data.frame(chron, regressor)
    l <- list(dfr, DF)
    dfr <- rbindlist(l)
  }
  colnames(dfr) <- c(colnames(oos)[vars], colnames(oos)[57:80])
  return(dfr)
}
#############################################################################################
MakeFunctionalDatasetY <- function(oos)
{
  dfr <- data_frame()
  dts <- unique(as.Date(oos$X__1))
  for(d in dts)
  {
    yoos <- oos$yoos[which(as.Date(oos$X__1) == d)]
    if(length(yoos) == 24)
    {
      DF <- data.frame(t(yoos))
      l <- list(dfr, DF)
      dfr <- rbindlist(l)
    }
  }
  colnames(dfr) <- c(paste0("y",0:23))
  return(dfr)
}
#############################################################################################
get_Basis <- function()
{
  
  h <- 23 ##### 23 is correct!!!!
  BASIS <- matrix(0, nrow = 23, ncol = 24)
  BASIS[1,] <- (1/sqrt(23))*rep(1, 24)
  BASIS[2,] <- (1/sqrt(23/2))*sin((2*pi/h)*1:24)
  BASIS[3,] <- (1/sqrt(23/2))*cos((2*pi/h)*1:24)
  BASIS[4,] <- (1/sqrt(23/2))*sin((2*pi/h)*2*(1:24))
  BASIS[5,] <- (1/sqrt(23/2))*cos((2*pi/h)*2*(1:24))
  BASIS[6,] <- (1/sqrt(23/2))*sin((2*pi/h)*3*(1:24))
  BASIS[7,] <- (1/sqrt(23/2))*cos((2*pi/h)*3*(1:24))
  BASIS[8,] <- (1/sqrt(23/2))*sin((2*pi/h)*4*(1:24))
  BASIS[9,] <- (1/sqrt(23/2))*cos((2*pi/h)*4*(1:24))
  BASIS[10,] <- (1/sqrt(23/2))*sin((2*pi/h)*5*(1:24))
  BASIS[11,] <- (1/sqrt(23/2))*cos((2*pi/h)*5*(1:24))
  BASIS[12,] <- (1/sqrt(23/2))*sin((2*pi/h)*6*(1:24))
  BASIS[13,] <- (1/sqrt(23/2))*cos((2*pi/h)*6*(1:24))
  BASIS[14,] <- (1/sqrt(23/2))*sin((2*pi/h)*7*(1:24))
  BASIS[15,] <- (1/sqrt(23/2))*cos((2*pi/h)*7*(1:24))
  BASIS[16,] <- (1/sqrt(23/2))*sin((2*pi/h)*8*(1:24))
  BASIS[17,] <- (1/sqrt(23/2))*cos((2*pi/h)*8*(1:24))
  BASIS[18,] <- (1/sqrt(23/2))*sin((2*pi/h)*9*(1:24))
  BASIS[19,] <- (1/sqrt(23/2))*cos((2*pi/h)*9*(1:24))
  BASIS[20,] <- (1/sqrt(23/2))*sin((2*pi/h)*10*(1:24))
  BASIS[21,] <- (1/sqrt(23/2))*cos((2*pi/h)*10*(1:24))
  BASIS[22,] <- (1/sqrt(23/2))*sin((2*pi/h)*11*(1:24))
  BASIS[23,] <- (1/sqrt(23/2))*cos((2*pi/h)*11*(1:24))
  
  return(BASIS)
}
#############################################################################################
func_R2 <- function(f, g)
{
  R2 <- 1 - mean(apply(f-g, 1, L2Norm)/apply(g - colMeans(g), 1, L2Norm))       #(L2Norm(f - g))/(g - colMeans(g))
  return(R2)
}
#############################################################################################
L2Norm <- function(f)
{
  return(mean((f)^2))
}
#############################################################################################

oos <- data.table(read_excel('C:/Users/utente/Documents/Sbilanciamento/nord_to_R.xlsx'))


####### FUNCTIONAL PART
Xregr2 <- MakeFunctionalDatasetRegressor(oos)
Yoos <- MakeFunctionalDatasetY(oos)

Xregr <- Xregr2[1:143,]

Fbasis <-  create.fourier.basis(c(1,24), nbasis=23)
FXregr <- smooth.basis(1:24, t(Xregr[,32:55]), Fbasis)$fd
FYoos <- smooth.basis(1:24, t(Yoos), Fbasis)$fd

D <- t(FYoos$coefs) 
C <- t(FXregr$coefs)

Bh <- solve(t(C)%*%C)%*%t(C)%*%D
Yhat <- C%*%Bh%*%get_Basis()

matplot(1:24, t(Yhat), type = 'l')
matplot(1:24, t(Yoos), type = 'l')

Ydiff <- Yoos - Yhat

####### DISCRETE PART
Z <- Xregr[,1:31]

inverse_MP <- ginv(t(as.matrix(Z)) %*% as.matrix(Z))
rhat <- inverse_MP%*%t(as.matrix(Z))%*%as.matrix(Ydiff)
RH <- as.matrix(Z)%*%rhat  #### discrete part

YYH <- Yhat + RH #### predicted curve

matplot(1:24, t(YYH), type = 'l')

Epsilon <- as.matrix(Yoos) - YYH 
print(paste("R2:", func_R2(YYH, as.matrix(Yoos))))

matplot(1:24, t(Epsilon), type = "l")
maxerr <- apply(Epsilon, 1, max)


###### ONLY WORKING DAYS
Yoos <- Yoos[which(Xregr$Sab == 0 & Xregr$Dom == 0 & Xregr$hol == 0),]
Yoos <- Yoos[-77,]
Xregr <- Xregr[which(Xregr$Sab == 0 & Xregr$Dom == 0 & Xregr$hol == 0),]
Xregr <- Xregr[-77,]

FXregr <- smooth.basis(1:24, t(Xregr[,32:55]), Fbasis)$fd
FYoos <- smooth.basis(1:24, t(Yoos), Fbasis)$fd

D <- t(FYoos$coefs) 
C <- t(FXregr$coefs)

Bh <- solve(t(C)%*%C)%*%t(C)%*%D
Yhat <- C%*%Bh%*%get_Basis()


matplot(1:24, t(Yhat), type = 'l')
matplot(1:24, t(Yoos), type = 'l')

Ydiff <- Yoos - Yhat

####### DISCRETE PART
Z <- Xregr[,1:31]

inverse_MP <- ginv(t(as.matrix(Z)) %*% as.matrix(Z))
rhat <- inverse_MP%*%t(as.matrix(Z))%*%as.matrix(Ydiff)
RH <- as.matrix(Z)%*%rhat  #### discrete part

YYH <- Yhat + RH #### predicted curve

matplot(1:24, t(YYH), type = 'l')

Epsilon <- as.matrix(Yoos) - YYH 
print(paste("R2:", func_R2(YYH, as.matrix(Yoos))))

matplot(1:24, t(Epsilon), type = "l")
maxerr <- apply(Epsilon, 1, max)

EA <- Epsilon/as.matrix(Yoos)
matplot(1:24, t(EA), type = "l")

maxea <- apply(EA, 1, max)

######## ONLY WEEKENDS AND HOLIDAYS
FXregr <- smooth.basis(1:24, t(Xregr[which(Xregr$Sab == 1 | Xregr$Dom == 1 | Xregr$hol == 1),32:55]), Fbasis)$fd
FYoos <- smooth.basis(1:24, t(Yoos[which(Xregr$Sab == 1 | Xregr$Dom == 1 | Xregr$hol == 1),]), Fbasis)$fd

D <- t(FYoos$coefs) 
C <- t(FXregr$coefs)

Bh <- solve(t(C)%*%C)%*%t(C)%*%D
Yhat <- C%*%Bh%*%get_Basis()

matplot(1:24, t(Yhat), type = 'l')
matplot(1:24, t(Yoos[which(Xregr$Sab == 1 | Xregr$Dom == 1 | Xregr$hol == 1),]), type = 'l')

Ydiff <- Yoos[which(Xregr$Sab == 1 | Xregr$Dom == 1 | Xregr$hol == 1),] - Yhat

####### DISCRETE PART
Z <- Xregr[which(Xregr$Sab == 1 | Xregr$Dom == 1 | Xregr$hol == 1),1:31]

inverse_MP <- ginv(t(as.matrix(Z)) %*% as.matrix(Z))
rhat <- inverse_MP%*%t(as.matrix(Z))%*%as.matrix(Ydiff)
RH <- as.matrix(Z)%*%rhat  #### discrete part

YYH <- Yhat + RH #### predicted curve

matplot(1:24, t(YYH), type = 'l')

Epsilon <- as.matrix(Yoos[which(Xregr$Sab == 1 | Xregr$Dom == 1 | Xregr$hol == 1),]) - YYH 
print(paste("R2:", func_R2(YYH, as.matrix(Yoos[which(Xregr$Sab == 1 | Xregr$Dom == 1 | Xregr$hol == 1),]))))

matplot(1:24, t(Epsilon), type = "l")
maxerr <- apply(Epsilon, 1, max)

EA <- Epsilon/as.matrix(Yoos[which(Xregr$Sab == 1 | Xregr$Dom == 1 | Xregr$hol == 1),])
matplot(1:24, t(EA), type = "l")

maxea <- apply(EA, 1, max)
maxea[23] <- 0

