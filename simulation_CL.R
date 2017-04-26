###### simulazione CL
library(Rsolnp)


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
