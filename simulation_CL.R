###### simulazione CL
library(Rsolnp)


ExpectedPayOff <- function(p = 0, q = 0, r = 0, QJ = 0, QR = 0, QA = 0)
{
  vJ <- 2.60
  vR <- 2.70
  vA <- 5
  vM <- 8.50
  
  EF <- 0
  
  EF <- p*vJ*QJ - (1 - p)*(QR*vR + QA*vA + (100 - QJ - QR - QA)*vM) 
  EF <- EF + q*vA*QA - (1 - q)*(QJ*vJ + QR*vR + (100 - QJ - QR - QA)*vM) 
  EF <- EF + r*vR*QR - (1 - r)*(QJ*vJ + QA*vA + (100 - QJ - QR - QA)*vM) 
  EF <- EF + (1 - p - q - r)*(100 - QJ - QR - QA)*vM - (p + q + r)*(QJ*vJ + QA*vA + QR*vR)
  
  return((-1)*EF)
}


#solnp(c(0.25,0.25,0.25,25,25,25), ExpectedPayOff, )
### feasible region: ui %*% theta - ci >= 0


ui <- matrix(c(-1,-1,-1,0,0,0,
               0,0,0,-1,-1,-1,
               -1,0,0,0,0,0,
               0,-1,0,0,0,0,
               0,0,-1,0,0,0,
               0,0,0,-1,0,0,
               0,0,0,0,-1,0,
               0,0,0,0,0,-1,
               1,0,0,0,0,0,
               0,1,0,0,0,0,
               0,0,1,0,0,0,
               0,0,0,1,0,0,
               0,0,0,0,1,0,
               0,0,0,0,0,1), ncol = 6, byrow = TRUE)

theta <- c(0.20,0.20,0.20,20,20,20)
ci <- c(1,100,1,1,1,100,100,100,0,0,0,0,0,0)

ui %*% theta - ci

constrOptim(c(0.20, 0.20, 0.20, 10, 10, 10), ExpectedPayOff, method = "Nelder-Mead", ui = ui, ci = c(-1,-100,-1,-1,-1,-100,-100,-100,0,0,0,0,0,0))

ExpectedPayOff(theta)
