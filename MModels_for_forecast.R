#### Meteo model for forecast
#### Analysis of weekly correlations

library(plyr)
library(dplyr)
library(readxl)
library(feather)
library(lubridate)
library(data.table)
library(randomForest)
library(RFinfer)
library(fda)
library(roahd)
library(plot3D)
library(mgcv)
library(gplots)


##############################################################
DailyTimeCorrelation <- function(df, lm)
{
  dt <- data_frame()
  for(i in 2:52)
  {
    cm <- as.numeric(unlist(acf(df$Mon[1:i], lag.max = lm, plot = FALSE))[2])
    ctu <- as.numeric(unlist(acf(df$Tue[1:i], lag.max = lm, plot = FALSE))[2])
    cw <- as.numeric(unlist(acf(df$Wed[1:i], lag.max = lm, plot = FALSE))[2])
    cth <- as.numeric(unlist(acf(df$Thu[1:i], lag.max = lm, plot = FALSE))[2])
    cf <- as.numeric(unlist(acf(df$Fri[1:i], lag.max = lm, plot = FALSE))[2])
    csa <- as.numeric(unlist(acf(df$Sat[1:i], lag.max = lm, plot = FALSE))[2])
    csu <- as.numeric(unlist(acf(df$Sun[1:i], lag.max = lm, plot = FALSE))[2])
    dt <- bind_rows(dt, data.frame(cm, ctu, cw, cth, cf, csa, csu))
  }
  return(dt)
}
##############################################################

### it's better to model the weather dependency with a GAMM model
### DWT is absolute mean; DTW has the mean compared to a "basal consumption"

DWT = data.table(read_excel('C:\\Users\\utente\\Documents\\Sbilanciamento\\DWT.xlsx'))

DWT[,c('X__1','pday','pioggia','ponte','dls','edls'):= NULL]
plot(DWT$y, type = 'l')

heatmap.2(cor(DWT), dendrogram = 'none')

ctrl <- list(niterEM = 500, msVerbose = TRUE, optimMethod="L-BFGS-B")

gmodel <- eval(substitute(gamm(y ~ Lun + Mar + Mer + Gio + Ven + Sab +
                              Dom + Gen + Feb + March + Apr + Mag +
                              Giu + Lug + Ago + Set + Ott + Nov +
                              Dic + hol +
                              s(tmax, bs = "cc") +
                              s(vento, bs = "cc"), data = DWT,
                              control = ctrl, niterPQL = 500)), DWT)


##################################### WEEKLY CORRELATIONS ##################################################


WC = data.table(read_excel('C:\\Users\\utente\\Documents\\Sbilanciamento\\weekly_correlations.xlsx'))
WC = WC[,2:8]

boxplot(WC)

matplot(t(WC), type = 'l')

Fbasis <-  create.bspline.basis(c(1,7), nbasis=7)
fWC <- smooth.basis(1:7, t(WC), Fbasis)

plot(fWC)

fdwc <- fData(seq(1, 7, 1), WC)

ccf = cov_fun(fdwc)
plot(ccf)

cor(WC)

PCA <- pca.fd(fWC$fd, nharm = 3)

matplot(PCA$harmonics$coefs, type = 'l')


dtc <- DailyTimeCorrelation(WC, 1)

x <- seq(1, 7, by = 1)
y <- seq(2, 52,by = 1)
M <- mesh(x, y)

alpha <- M$x
beta <- M$y

z <- matrix(0, nrow = length(x), ncol = length(y))
for(i in 1:7)
{
  for(j in 1:50)
  {
    z[i,j] <- unlist(dtc[j,i])
  }
}

surf3D(x = M$x,y = M$y, z = z, colkey=TRUE,bty="b2",main="DTC")
