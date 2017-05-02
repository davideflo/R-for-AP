###### simulazione CL
library(Rsolnp)

###################################################################################################################
ExpectedPayOff <- function(Q)
{
  QJ <- Q[1]
  QR <- Q[2]
  QA <- Q[3]
  vJ <- 2.60
  vR <- 2.70
  vA <- 5
  vM <- 8.50
  
  EF <- 0
  x <- seq(0, 1, 0.01)
  for(p in x)
  {
    for(q in x[which(x == p):100])
    {
      for(r in x[which(x == q):100])
      {
        EF <- p*vJ*QJ - (1 - p)*(QR*vR + QA*vA + (100 - QJ - QR - QA)*vM) 
        EF <- EF + q*vA*QA - (1 - q)*(QJ*vJ + QR*vR + (100 - QJ - QR - QA)*vM) 
        EF <- EF + r*vR*QR - (1 - r)*(QJ*vJ + QA*vA + (100 - QJ - QR - QA)*vM) 
        EF <- EF + (1 - p - q - r)*(100 - QJ - QR - QA)*vM - (p + q + r)*(QJ*vJ + QA*vA + QR*vR)
      }
    }
  }
  
  return((-1)*EF)
}
###################################################################################################################
ExpectedGain <- function(Q)
{
  QJ <- Q[1]
  QR <- Q[2]
  QA <- Q[3]
  QM <- Q[4]
  
  vJ <- 2.70
  vR <- 2.70
  vA <- 5
  vM <- 8.50
  
  EG <- 0
  x <- seq(0, 1, 0.01)
  for(p in x)
  {
    for(q in x[which(x == p):100])
    {
      for(r in x[which(x == q):100])
      {
        #for(s in x[which(x == r):100])
        # EG <- p*(vJ*QJ - (QR + QA + QM))
        # EG <- EG + q*(vR*QR - (QJ + QA + QM)) 
        # EG <- EG + r*(vA*QA - (QJ + QR + QM)) 
        # EG <- EG + (1 - p - q - r)*(vA*QA - (QJ + QA + QR))
        EG <- p*(-vJ*QJ + (QR + QA + QM))
        EG <- EG + q*(-vR*QR + (QJ + QA + QM)) 
        EG <- EG + r*(-vA*QA + (QJ + QR + QM)) 
        EG <- EG + (1 - p - q - r)*(-vM*QM + (QJ + QA + QR))
      }
    }
  }
  
  return(EG)
}
####################################################################################
SimpleExpectedLoss <- function(Q)
{
  QJ <- Q[1]
  QR <- Q[2]
  QA <- Q[3]
  QM <- Q[4]
  
  vJ <- 2.70
  vR <- 2.70
  vA <- 5
  vM <- 8.50
  
  SEG <- 0
  P <- c(1/vJ, 1/vR, 1/vA, 1/vM)/sum(c(1/vJ, 1/vR, 1/vA, 1/vM))
  PJ <- P[1]
  PR <- P[2]
  PA <- P[3]
  PM <- P[4]
  
  SEG <- PJ*(-vJ*QJ + (QR + QA + QM))
  SEG <- SEG + PR*(-vR*QR + (QJ + QA + QM))
  SEG <- SEG + PA*(-vA*QA + (QJ + QR + QM))
  SEG <- SEG + PM*(-vM*QM + (QJ + QR + QA))
  
  return(SEG)
}
####################################################################################
ExpectedTreeLoss <- function(Q)
{
  QJ <- Q[1]
  QR <- Q[2]
  QA <- Q[3]
  QM <- Q[4]
  
  vJ <- 2.70
  vR <- 2.70
  vA <- 5
  vM <- 8.50
  
  B <- 1000
  
  pb <- vM/(vM + vJ)
  qb <- vA/(vA + vR)
  rb <- vR/(vR + vJ)
  sb <- vA/(vA + vJ)
  sigmab <- vM/(vM + vR)
  taub <- vM/(vM + vA)
  
  ETL <- 0
  counts <- 0
  
  while(counts < B)
  {
    p <- runif(1, min = pb, max = 1)
    q <- runif(1, min = qb, max = 1)
    r <- runif(1, min = 0, max = 1)
    s <- runif(1, min = sb, max = 1)
    sigma <- runif(1, min = sigmab, max = 1)
    tau <- runif(1, min = taub, max = 1)
    ### J:
    ETL <- p*q*r*(QA+QR+QM - QJ*vJ) + p*q*(1 - r)*(QA+QR+QM - QJ*vJ) + p*(1 - q)*s*(QA+QR+QM - QJ*vJ) + p*(1-q)*(1-s)*(QA+QR+QM - QJ*vJ) + (1-p)*(QJ)
    ### R:
    ETL <- ETL + p*q*(1-r)*(QA+QJ+QM - QR*vR) + p*q*r*(QA+QJ+QM - QR*vR) + q*(1-p)*sigma*(QA+QJ+QM - QR*vR) + q*(1-p)*(1-sigma)*(QA+QJ+QM - QR*vR) + (1-q)*(QR)
    ### A:
    ETL <- ETL + p*(1-q)*(1-s)*(QR+QJ+QM - QA*vA) + p*(1-q)*s*(QR+QJ+QM - QA*vA) + (1-q)*(1-p)*tau*(QA+QJ+QM - QA*vA) + (1-q)*(1-p)*(1-tau)*(QR+QJ+QM - QA*vA) + q*(QA)
    ### M:
    ETL <- ETL + (1-p)*q*(1-sigma)*(QA+QJ+QR - QM*vM) + (1-p)*q*sigma*(QA+QJ+QR - QA*vA) + (1-q)*(1-p)*(1-tau)*(QA+QJ+QR - QM*vM) + (1-q)*(1-p)*tau*(QA+QJ+QR - QM*vM) + p*(QM)
    
    counts <- counts + 1

  }
  
  
  return(ETL)
  
  
  
  
}
###################################################################################################################
VExpectedGain <- function(Q)
{
  QJ <- Q[1]
  QR <- Q[2]
  QA <- Q[3]
  QM <- Q[4]
  
  vJ <- 2.70
  vR <- 2.70
  vA <- 5
  vM <- 8.50
  
  vEG <- c()
  x <- seq(0, 1, 0.01)
  for(p in x)
  {
    for(q in x)
    {
      for(r in x)
      {
        EG <- r*p*(-vJ*QJ + (QR + QA + QM))
        EG <- EG + (1-r)*q*(-vR*QR + (QJ + QA + QM)) 
        EG <- EG + (1-r)*(1-q)*(-vA*QA + (QJ + QR + QM)) 
        EG <- EG + r*(1-p)*(-vM*QM + (QJ + QA + QR))
        vEG <- c(vEG, EG)
      }
    }
  }
  
  return(vEG)
}
####################################################################################
ExpectedGain2 <- function(Q)
{
  QJ <- Q[1]
  QR <- Q[2]
  QA <- Q[3]
  QM <- Q[4]
  
  vJ <- 2.70
  vR <- 2.70
  vA <- 5
  vM <- 8.50
  
  EG <- 0
  x <- seq(0, 1, 0.01)
  for(p in x)
  {
    for(q in x)
    {
      for(r in x)
      {
        EG <- r*p*(-vJ*QJ + (QR + QA + QM))
        EG <- EG + (1-r)*q*(-vR*QR + (QJ + QA + QM)) 
        EG <- EG + (1-r)*(1-q)*(-vA*QA + (QJ + QR + QM)) 
        EG <- EG + r*(1-p)*(-vM*QM + (QJ + QA + QR))
      }
    }
  }
  
  return(EG)
}
####################################################################################
VExpectedGain3 <- function(Q)
{
  QJ <- Q[1]
  QR <- Q[2]
  QA <- Q[3]
  QM <- Q[4]
  
  vJ <- 2.70
  vR <- 2.70
  vA <- 5
  vM <- 8.50
  
  vEG <- c()
  x <- seq(0, 1, 0.01)
  for(p in x)
  {
    for(q in x)
    {
      for(r in x)
      {
        EG <- r*p*(-vJ*QJ + (QR + QA + QM))
        EG <- EG + (1-r)*q*(-vR*QR + (QJ + QA + QM)) 
        EG <- EG + (1-r)*(1-q)*(-vA*QA + (QJ + QR + QM)) 
        EG <- EG + r*(1-p)*(-vM*QM + (QJ + QA + QR))
        vEG <- c(vEG, EG)
      }
    }
  }
  
  return(mean(vEG))
}
####################################################################################

#solnp(c(0.25,0.25,0.25,25,25,25), ExpectedPayOff, )
### feasible region: ui %*% theta - ci >= 0


# ui <- matrix(c(-1,-1,-1,0,0,0,
#                0,0,0,-1,-1,-1,
#                -1,0,0,0,0,0,
#                0,-1,0,0,0,0,
#                0,0,-1,0,0,0,
#                0,0,0,-1,0,0,
#                0,0,0,0,-1,0,
#                0,0,0,0,0,-1,
#                1,0,0,0,0,0,
#                0,1,0,0,0,0,
#                0,0,1,0,0,0,
#                0,0,0,1,0,0,
#                0,0,0,0,1,0,
#                0,0,0,0,0,1), ncol = 6, byrow = TRUE)
# 
# theta <- c(0.20,0.20,0.20,20,20,20)
# ci <- c(1,100,1,1,1,100,100,100,0,0,0,0,0,0)
# 
# ui %*% theta - ci

ui <- matrix(c(-1,-1,-1,
               -1,0,0,
               0,-1,0,
               0,0,-1,
               1,0,0,
               0,1,0,
               0,0,1), ncol = 3, byrow = TRUE)

theta <- c(20,20,20)
ci <- c(-100,-100,-100,-100,0,0,0)

ui %*% theta - ci

constrOptim(c(10, 10, 10), ExpectedPayOff, method = "Nelder-Mead", ui = ui, ci = ci)
#constrOptim(c(10, 10, 10), ExpectedPayOff, method = "L-BFGS-B", ui = ui, ci = ci)


ExpectedPayOff(theta/3)
ExpectedGain(c(37,38,20,5))

ui2 <- matrix(c(-1,-1,-1,-1,
               1,0,0,0,
               0,1,0,0,
               0,0,1,0,
               0,0,0,1), ncol = 4, byrow = TRUE)

theta2 <- c(36,38,20,5)
ci2 <- c(-100,0,0,0,0)

ui2 %*% theta2 - ci2

constrOptim(theta2, ExpectedGain, method = "Nelder-Mead", ui = ui2, ci = ci2)

ExpectedGain(c(6.943307e+01, 3.056693e+01, 2.921135e-07, 1.448057e-06))

#### se vJ = 2.60
c(7.919506e-08, 6.349933e+01, 3.650067e+01, 2.360770e-07) %*% c(2.60, 2.70, 5, 8.50) - 300
#### se vJ = 2.70
c(2.845611e+01, 2.784068e-02, 7.151605e+01, 1.478676e-08) %*% c(2.60, 2.70, 5, 8.50) - 300

constrOptim(c(20,20,20,20), SimpleExpectedLoss, method = "Nelder-Mead", ui = ui2, ci = ci2)

SimpleExpectedLoss(c(2.394065e+01, 7.605933e+01, 1.312176e-05, 1.497689e-06))

constrOptim(c(10,10,10,10), ExpectedTreeLoss, method = "Nelder-Mead", ui = ui2, ci = ci2)


veg <- VExpectedGain(c(2.845611e+01, 2.784068e-02, 7.151605e+01, 1.478676e-08))

constrOptim(c(10,10,10,10), ExpectedGain2, method = "Nelder-Mead", ui = ui2, ci = ci2)

veg <- VExpectedGain(c(9.998531e+01, 7.341097e-10, 1.084993e-07, 1.468459e-02))

constrOptim(c(10,10,10,10), VExpectedGain3, method = "Nelder-Mead", ui = ui2, ci = ci2)
