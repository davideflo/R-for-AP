##### Functions to treat POD orari 


source("C://Users//utente//Documents//R_code//SparseFunctClust.R")

ConvertDate <- function(df)
{
  df <- as.data.frame(df)
  dfc <- as.Date(as.POSIXct(unlist(df['Giorno']), origin = "1970-01-01"))
  df$Giorno <- dfc
  return(df)
}
#######################################################
HourAggregator <- function(df)
{
  M <- matrix(0, nrow = nrow(df), ncol = 24)
  df2 <- data_frame()
  s <- seq(1, 96, by = 4)
  dfloc <- df[,4:99]
  for(i in 1:nrow(df))
  {
    df2 <- bind_rows(df2, df[i,1:2])
    for(j in 1:(length(s)-1))
    {
      M[i, j] <- sum(dfloc[i, s[j]:(s[j+1]-1)])
    }
    M[i, 24] <- sum(dfloc[i, 93:96])
  }
  df2 <- bind_cols(df2, data.frame(M))
  colnames(df2) <- c('Pod', 'Giorno', as.character(1:24))
  return(df2)
}
#####################################################
FilterByDateAndValue <- function(df, date, value, kind)
{
  df2 <- df[which(as.Date(unlist(df['Giorno'])) == as.Date(date)),]
  if(kind == "greater")
  {
    df3 <- df2[which(apply(df2[,3:26], 1, min) >= value),3:26]    
  }
  else
  {
    df3 <- df2[which(apply(df2[,3:26], 1, max) <= value),3:26]    
  }
  return(df3)
}
#############################################################
FilterByDate <- function(df2, datait)
{
  df3 <- df2[which(as.Date(unlist(df2[,2])) == as.Date(datait)),]
  return(df3)
}
#################################################################
CHStatistics <- function(df, K, m)
{
  chs <- rep(0, K)
  x <- seq(1,24, by = 1)
  for( k in 2:(K-2))
  {
    fobj <- FKMSparseClustering(df, x, k, m, method='kmea', maxiter = 50)
    getter <- GetWCSS(df, fobj$CLUSTER)
    Bk <- sum(getter$bcss.perfeature)
    Wk <- getter$wcss
    chs[k] <- (Bk/(k-1))/(Wk/(nrow(df)-k))
  }
  return(chs)
}
##################################################################
FitPolynomial <- function(y, x, degree)
{
  fp <- lm(y ~ poly(x, degree, raw=TRUE))
  return(fp$coefficients)
}
####################################################################
BestFittingPolynomial <- function(y, x, maxdegree)
{
  best <- 0
  bestdegree <- 1
  bestfp <- lm(y ~ poly(x, 1, raw=TRUE))
  best <- sqrt(mean(bestfp$residuals^2))
  for(m in 2:maxdegree)
  {
    fp <- lm(y ~ poly(x, m, raw=TRUE))
    print(summary(fp))
    if(sqrt(mean(bestfp$residuals^2)) < best)
    {
      bestdegree <- m
    }
  }
  return(m)
}
####################################################################
FitDerivatives <- function(df)
{
  argvals <- seq(0,1,len=24)
  nbasis <- 20
  basisobj <- create.bspline.basis(c(0,1),nbasis)
  M <- matrix(0, nrow = nrow(df), ncol = 24)
  for(i in 1:nrow(df))
  {
    Ys <- smooth.basis(argvals=argvals, y=unlist(df[i,]), fdParobj=basisobj,returnMatrix=TRUE)
    Dxhat <- eval.fd(argvals, Ys$fd, Lfd=1)
    M[i,] <- unlist(Dxhat[,1])
  }
  return(M)
}
