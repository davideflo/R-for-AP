##############
### @BRIEF: functions for "similar days" model
###         forecast hourly consumption based on the same day of the week before
#############

###### INFO: functions in alphabetical order and only for this model
library(psych) #### contains trace
library(MASS) #### for the Moore-Penrose pseudo-inverse


source("R_code/functions_for_POD_orari.R")

#########################################################################################
### @PARAM: mat is either the functional regressor or the functional response
Expand_in_FBasis <- function(mat)
{
  Fbasis <-  create.fourier.basis(c(1,24), nbasis=23)
  
  FXreg <- smooth.basis(1:24, t(mat), Fbasis)$fd
  
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
  
  return(t(FXreg$coefs)%*%BASIS)
}
#########################################################################################
### @ param: f and g are the matrices of the functional data. Let's pose f := \hat{y}(x) and g:= y(x)
func_R2 <- function(f, g)
{
  R2 <- 1 - mean(apply(f-g, 1, L2Norm)/apply(g - colMeans(g), 1, L2Norm))       #(L2Norm(f - g))/(g - colMeans(g))
  return(R2)
}
#########################################################################################
#### @param: Xreg is the dataset with the functional data, Y is the target set of functions and Z contains the discrete regressors
FunctionalRegression <- function(dfr, newdfr)
{
  Xreg <- dfr[,9:32]
  Y <- dfr[,41:64]
  
  disc <- setdiff(colnames(dfr), colnames(dfr)[c(3,5,35,37, 9:32, 41:64)])
  discv <- which(colnames(dfr) %in% disc)
  
  Z <- as.data.frame(dfr)[,discv]
  
  Fbasis <-  create.fourier.basis(c(1,24), nbasis=23)
  
  FXreg <- smooth.basis(1:24, t(Xreg), Fbasis)$fd
  FY <- smooth.basis(1:24, t(Y), Fbasis)$fd
  
  D <- t(FY$coefs) 
  C <- t(FXreg$coefs)
 
#  J <- diag(1, nrow(Y))
  
  Bh <- solve(t(C)%*%C)%*%t(C)%*%D
  
  Yhat2 <- C%*%Bh%*%get_Basis() #### functional part
  
  fdiff <- Y - Yhat2

  inverse_MP <- ginv(t(as.matrix(Z)) %*% as.matrix(Z))
  rhat <- inverse_MP%*%t(as.matrix(Z))%*%as.matrix(fdiff)
  RH <- as.matrix(Z)%*%rhat  #### discrete part
  
  YYH <- Yhat2 + RH #### predicted curve

  
  Epsilon <- as.matrix(Y) - YYH 
  print(paste("R2:", func_R2(YYH, as.matrix(Y))))
  
  
}
#########################################################################################
### @PARAM: returns only the basis
get_Basis <- function()
{
  #Fbasis <-  create.fourier.basis(c(1,24), nbasis=23)
  
  #FXreg <- smooth.basis(1:24, t(mat), Fbasis)$fd
  
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
#########################################################################################
### @PARAM: data from hourly measurement
get_Table_similar_days <- function(df)
{
  aggdf <- Aggregator(df)
  d_f <- data_frame()
  for(i in 1:nrow(aggdf))
  {
    d <- as.Date(aggdf$date[i])
    numd <- lubridate::yday(d)
    numw <- lubridate::week(d)
    wd <- lubridate::wday(d)
    hol <- add_holidays_Date(d)
    cd <- daylight_saving(d)
    df2 <- data.frame(as.Date(d), numd, numw, wd, hol, cd, t(unlist(aggdf[i,2:25])))
    d_f <- bind_rows(d_f, df2)
  }
  colnames(d_f) <- c("date", "num_day", "num_week", "weekday", "holiday", "change_date", as.character(1:24))
  return(d_f)
}
##########################################################################################
### @PARAM: data from TERNA's report
###         variable gives the choice of which measurement can be taken
get_Table_similar_days2 <- function(df, zona, variable)
{
  tdf <- df[which(df$`CODICE RUC` == paste0("UC_DP1608_", zona)),]
  tdf$`DATA RIFERIMENTO CORRISPETTIVO` <- strptime(tdf$`DATA RIFERIMENTO CORRISPETTIVO`, '%d/%m/%Y %H:%M')
  days <- unique(as.Date(tdf$`DATA RIFERIMENTO CORRISPETTIVO`))
  d_f <- data_frame()
  for(i in 1:length(days))
  {
    vec <- rep(0, 24)
    d <- days[i]
    numd <- lubridate::yday(d)
    numw <- lubridate::week(d)
    wd <- lubridate::wday(d)
    hol <- add_holidays_Date(d)
    cd <- daylight_saving(d)
    atdf <- tdf[which(as.Date(tdf$`DATA RIFERIMENTO CORRISPETTIVO`) == d),]
    
    for(h in 0:23)
    {
      j <- which(lubridate::hour(atdf$`DATA RIFERIMENTO CORRISPETTIVO`) == h)
      
      if(length(j) == 1) vec[h+1] <- unlist(atdf[j,variable])
      else if(length(j) > 1) vec[h+1] <- sum(unlist(atdf[j,variable]), na.rm = TRUE)
      else next
    }
    
    df2 <- data.frame(as.Date(d), numd, numw, wd, hol, cd, t(vec))
    d_f <- bind_rows(d_f, df2)
  }
  colnames(d_f) <- c("date", "num_day", "num_week", "weekday", "holiday", "change_date", as.character(1:24))
  return(d_f)
}
###########################################################################################
### @ param: f could be either of type g(x) - h(x) or a single function 
L2Norm <- function(f)
{
  return(mean((f)^2))
}
###########################################################################################
### @PARAM: dfr is the final dataset from similar day model
###         optim gets two vectors and the first one if transformed into a matrix
LeastSquareOptimizer <- function(dfr, penalization, lambda = 1, mu = 1, tol = 1e-6)
{
  Xreg <- dfr[,9:32]
  Y <- dfr[,41:64]
  disc <- setdiff(colnames(dfr), colnames(dfr)[c(3,5,35,37, 9:32, 41:64)])
  discv <- which(colnames(dfr) %in% disc)
  Z <- as.data.frame(dfr)[,discv]
  
  #FBX <- Expand_in_FBasis(Xreg)
  #FBY <- Expand_in_FBasis(Y)
  
  Fbasis <-  create.fourier.basis(c(1,24), nbasis=23)
  FXreg <- smooth.basis(1:24, t(Xreg), Fbasis)$fd
  FY <- smooth.basis(1:24, t(Y), Fbasis)$fd
  
  D <- t(FY$coefs) 
  C <- t(FXreg$coefs)
  Base <- get_Basis()
  J <- diag(1, nrow(Y)) ### \in R^{43*43}
  
  Discrete_Laplacian <- function(A)
  {
    dl <- 0
    nr <- nrow(A)
    nc <- ncol(A)
    for(i in 2:(nr-1))
    {
      for(j in 2:(nc-1))
      {
        dl <- dl + A[i-1,j] + A[i+1,j] + A[i,j-1] + A[i,j+1] - 4*A[i,j]
      }
    }
    return(dl)
  }
  
  product_MatrixVector <- function(Z, b)
  {
    prod <- rep(0, nrow(Z))
    for(i in 1:nrow(Z))
    {
      prod[i] <- sum(unlist(Z[i,]) * b)
    }
    return(prod)
  }
  
  if(penalization == "none")
  {
    LMISE <- function(B2, beta, lambda, mu)
    {
      B <- vect_to_mat(B2)
      ### B \in R^{23*23} ### the matrix of the "base changing"
      LS <- tr( t(C%*%B-D)%*%J%*%(C%*%B-D)) + sum(t(product_MatrixVector(Z,beta))*(product_MatrixVector(Z,beta)))
      return(LS)
    }
  }
  else if(penalization == "beta")
  {
    LMISE <- function(B2, beta, lambda, mu)
    {
      B <- vect_to_mat(B2)
      ### B \in R^{23*23} ### the matrix of the "base changing"
      LS <- tr( t(C%*%B-D)%*%J%*%(C%*%B-D)) + sum(t(product_MatrixVector(Z,beta))*(product_MatrixVector(Z,beta))) + lambda * sum(beta^2)
      return(LS)
    }
  }
  else if(penalization == "B")
  {
    LMISE <- function(B2, beta, lambda, mu)
    {
      B <- vect_to_mat(B2)
      ### B \in R^{23*23} ### the matrix of the "base changing"
      LS <- tr( t(C%*%B-D)%*%J%*%(C%*%B-D)) + sum(t(product_MatrixVector(Z,beta))*(product_MatrixVector(Z,beta))) + mu * Discrete_Laplacian(B)
      return(LS)
    }
  }
  else if(penalization == "full")
  {
    LMISE <- function(B2, beta, lambda, mu)
    {
      B <- vect_to_mat(B2)
      ### B \in R^{23*23} ### the matrix of the "base changing"
      LS <- tr( t(C%*%B-D)%*%J%*%(C%*%B-D)) + sum(t(product_MatrixVector(Z,beta))*(product_MatrixVector(Z,beta))) + lambda * sum(beta^2) + mu * Discrete_Laplacian(B)
      return(LS)
    }
  }
  else
  {
    stop("penalization method not implemented")
  }
  
  
  
  
  ### @ method: iteratively minimizing LMISE:
  ###           first minimize in B, then in beta and go on until a stopping criterion is met
  l0 = 0
  B0 = rnorm(n = 23^2)
  #beta0 <- rnorm(n = 12)
  beta0 <- rep(0, 12)
  
  B0star <- optim(par = B0, beta = beta0, lambda = lambda, mu = mu, LMISE)$par
  beta0star <- optim(par = beta0, B2 = B0star, lambda = lambda, mu = mu, LMISE)$par
  
  lstar = LMISE(B0star, beta0star, lambda, mu)
                    
  while(abs(l0 - lstar) >= tol)
  {
    l0 = lstar
    B0star <- optim(par = B0, beta = beta0star, lambda = lambda, mu = mu, LMISE)$par
    beta0star <- optim(par = beta0, B2 = B0star, lambda = lambda, mu = mu, LMISE)$par
    
    lstar = LMISE(B0star, beta0star, lambda, mu)
  }
  
  #result <- optim(par = c(B2 = c(rnorm(n = 23^2))), beta = rnorm(n = 12), LMISE)
  return( out <- list(Bstar = B0star, betastar = beta0star))
  
  
}
###########################################################################################
### @PARAM: data from hourly measurement
make_dataset_similar_day <- function(df, meteo, final_date, weekday)
{
  d_f <- data_frame()
  aggdf <- get_Table_similar_days(df)
  meteo2 <- get_meteo2(meteo, final_date)
  
  onlydf <- aggdf[which(aggdf$weekday == weekday),]
  
  for(i in 1:nrow(onlydf))
  {
    d <- as.Date(onlydf$date[i])
    print(d)
    numd <- lubridate::yday(d)
    numw <- lubridate::week(d)
    wd <- lubridate::wday(d)
    hol <- add_holidays_Date(d)
    cd <- daylight_saving(d)
    tmedia <- associate_meteo_data(d, meteo2, "Tmedia")
    vento <- associate_meteo_data(d, meteo2, "VENTOMEDIA")
    #rain <- get_rain(meteo, d)
    rain <- get_rain_num(meteo, d)
    regr <- unlist(onlydf[i,7:30])
    
    next_week <- d + 7
    print(next_week)
    if(next_week %in% onlydf$date)
    {
      index <- which(onlydf$date == next_week)
      tnumd <- lubridate::yday(next_week)
      tnumw <- lubridate::week(next_week)
      twd <- lubridate::wday(next_week)
      thol <- add_holidays_Date(next_week)
      tcd <- daylight_saving(next_week)
      ttmedia <- associate_meteo_data(next_week, meteo2, "Tmedia")
      #train <- get_rain(meteo, d)
      train <- get_rain_num(meteo, next_week)
      tvento <- associate_meteo_data(next_week, meteo2, "VENTOMEDIA")
      y <- unlist(onlydf[index,7:30])
      df2 <- data.frame(numd, numw, wd, hol, cd, tmedia, vento, rain, t(regr),
                        tnumd, tnumw, twd, thol, tcd, ttmedia, tvento, train, t(y))
      colnames(df2) <- c("num_day", "num_week", "weekday", "holiday", "change_date", "Tmedia", "vento", "pioggia", paste0("regr",as.character(1:24)),
                         "tnum_day", "tnum_week", "tweekday", "tholiday", "tchange_date", "tTmedia", "tvento", "tpioggia", paste0("y",as.character(1:24)))
      l <- list(d_f, df2)
      d_f <- rbindlist(l)
    }
    
    else{
      break
    }
    
  }
  return(d_f)
  
}
###########################################################################################
### @PARAM: data from TERNA measurement
make_dataset_similar_day_TERNA <- function(df, meteo, final_date, weekday, zona, variabile)
{
  d_f <- data_frame()
  aggdf <- get_Table_similar_days2(df, zona, variabile)
  meteo2 <- get_meteo2015(meteo)
  
  onlydf <- aggdf[which(aggdf$weekday == weekday),]
  
  for(i in 1:nrow(onlydf))
  {
    d <- as.Date(onlydf$date[i])
    print(d)
    numd <- lubridate::yday(d)
    numw <- lubridate::week(d)
    wd <- lubridate::wday(d)
    hol <- add_holidays_Date(d)
    cd <- daylight_saving(d)
    tmedia <- associate_meteo_data(d, meteo2, "Tmedia")
    vento <- associate_meteo_data(d, meteo2, "VENTOMEDIA")
    #rain <- get_rain(meteo, d)
    rain <- get_rain_num(meteo, d)
    regr <- unlist(onlydf[i,7:30])
    
    next_week <- d + 7
    print(next_week)
    if(next_week %in% onlydf$date)
    {
      index <- which(onlydf$date == next_week)
      tnumd <- lubridate::yday(next_week)
      tnumw <- lubridate::week(next_week)
      twd <- lubridate::wday(next_week)
      thol <- add_holidays_Date(next_week)
      tcd <- daylight_saving(next_week)
      ttmedia <- associate_meteo_data(next_week, meteo2, "Tmedia")
      #train <- get_rain(meteo, d)
      train <- get_rain_num(meteo, next_week)
      tvento <- associate_meteo_data(next_week, meteo2, "VENTOMEDIA")
      y <- unlist(onlydf[index,7:30])
      df2 <- data.frame(numd, numw, wd, hol, cd, tmedia, vento, rain, t(regr),
                        tnumd, tnumw, twd, thol, tcd, ttmedia, tvento, train, t(y))
      colnames(df2) <- c("num_day", "num_week", "weekday", "holiday", "change_date", "Tmedia", "vento", "pioggia", paste0("regr",as.character(1:24)),
                         "tnum_day", "tnum_week", "tweekday", "tholiday", "tchange_date", "tTmedia", "tvento", "tpioggia", paste0("y",as.character(1:24)))
      l <- list(d_f, df2)
      d_f <- rbindlist(l)
    }
    
    else{
      break
    }
    
  }
  return(d_f)
  
}
#########################################################################################
### @ param: dfr1 is the matrix of the functional regressors, dfr2 is the matrix of the discrete regressors
###          B is the \hat{B}^star coming from the optimization, as well as beta
###          This function computes the predicted consumption curves
predict_SimilarDays <- function(dfr1, dfr2, B, beta)
{
  Xreg <- dfr1
  
  # disc <- setdiff(colnames(dfr), colnames(dfr)[c(3,5,35,37, 9:32, 41:64)])
  # discv <- which(colnames(dfr) %in% disc)
  # Z <- as.data.frame(dfr)[,discv]
 
  Z <- dfr2
  
  Fbasis <-  create.fourier.basis(c(1,24), nbasis=23)
  FXreg <- smooth.basis(1:24, t(Xreg), Fbasis)$fd

  C <- t(FXreg$coefs)
  #J <- diag(1, nrow(Y)) ### \in R^{43*43}
  Base <- get_Basis()
  
  #yhat <- matrix(0, nrow = nrow(Xreg), ncol = 24)
  
  Dhat <- C%*%B%*%Base
  
  return(Dhat)
  
}
##############################################################################################
vect_to_mat <- function(vec)
{
  mat <- matrix(0, 23, 23)
  s <- seq(1, 23^2, 23)
  for(i in 1:length(s))
  {
    if(s[i] < 507) {mat[,i] <- vec[s[i]:(s[i+1]-1)]}
    else {mat[,i] <- vec[s[i]:529]}
  }
  return(mat)
}