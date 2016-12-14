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
FitDerivatives <- function(df, flag = TRUE)
{
  if(flag)
  {
    argvals <- seq(0,1,len=240)
    nbasis <- 20
    basisobj <- create.bspline.basis(c(0,1),nbasis)
    M <- matrix(0, nrow = nrow(df), ncol = 240)
    for(i in 1:nrow(df))
    {
      interp_df <- approx(argvals[c(1,11,21,31,41,51,61,71,81,91,101,111,121,131,141,151,161,171,181,191,201,211,221,240)], unlist(df[i,]), xout = argvals)
      Ys <- smooth.basis(argvals=argvals, y=interp_df$y, fdParobj=basisobj,returnMatrix=TRUE)
      Dxhat <- eval.fd(argvals, Ys$fd, Lfd=1)
      Dxhat_smooth <- smooth.basis(argvals=argvals, y=Dxhat, fdParobj=basisobj,returnMatrix=TRUE)
      M[i,] <- unlist(Dxhat_smooth$y[,1])
    }
    return(M)
  }
  else
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
}
#########################################################################################
TreatData <- function(name)
{
  data <- as.data.frame(read_feather(paste0("C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_",name)))
  data <- ConvertDate(data)
  Agg <- HourAggregator(data)
  write_feather(Agg, paste0("C:\\Users\\utente\\Documents\\misure\\misure_orarie\\dati_aggregati_",name))
}
########################################################################################
TSAggregator <- function(df)
{
  days <- as.character(seq.Date(from = as.Date('2016-01-01'), to = as.Date('2016-10-31'), by = 'day'))
  ts <- c()
  for(d in days)
  {
    atd <- FilterByDate(df, d)[,3:26]
    ts <- c(ts, unlist(colSums(atd, na.rm = TRUE)))
  }
  return(ts)
}
#########################################################################################
GetSD <- function(ts, from, to)
{ ### to is one day ahead ###
  dsd <- c()
  Date <- seq.POSIXt(from=as.POSIXct(from), to = as.POSIXct(to), by='hour')
  DT <- data.table(Date[1:(length(Date)-1)], ts, c(1:length(ts)))
  colnames(DT) <- c("Date", "cons", "day.of.year")
  return(DT[,sd(cons),by=floor_date(Date,"day")])
}
#########################################################################################
GetHourlySD <- function(df)
{
  days <- as.character(seq.Date(from = as.Date('2016-01-01'), to = as.Date('2016-10-31'), by = 'day'))
  ts <- c()
  for(d in days)
  {
    atd <- (1/1000)*FilterByDate(df, d)[,3:26]
    ts <- c(ts, unlist(apply(atd, 2, sd, na.rm = TRUE)))
  }
  return(ts)
}
#############################################################################################
AggregateMLR <- function(df)
{
  d_f <- data_frame()
  days <- unique(df$Giorno)
  seqd <- seq.Date(from = as.Date('2016-01-01'), to = as.Date('2016-10-31'), by = 'day')
  for(d in days)
  {
    dd <- which(seqd == d)
    atd <- df[which(df$Giorno == d),3:26]
    df2 <- data.frame(seqd[dd], t((1/1000)*colSums(atd, na.rm = TRUE)))
    d_f <- bind_rows(d_f, df2)
  }
  colnames(d_f) <- c("date", as.character(1:24))
  return(d_f)
}
  
  
  
  
  
  