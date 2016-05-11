is_active_month <- function(date, inizio, fine)
{
  bool <- FALSE
  if(compare_dates(date, inizio) & compare_dates(fine, date)) bool <- TRUE
  return(bool)
}

find_active_in_month <- function(date, vendite)
{
  actives <- c()
  for(i in 1:nrow(vendite))
  {
    inizio <- vendite[i,2]
    fine <- vendite[i,3]
    if(is_active_month(date, inizio, fine)) actives <- c(actives, i)
  }
  return(actives)
}


total_sellings_per_components <- function(vendite, pm, listing)
{
  #total <- rep(0, nrow(vendite))
  total_matrix <- matrix(0, nrow = 10, ncol = 24)
  months <- c(paste0("01/","0",1:9,"/",2016), paste0("01/",10:12,"/",2016), paste0("01/","0",1:9,"/",2017), paste0("01/",10:12,"/",2017)) 
  
  for(m in months)
  {
    act <- find_active_in_month(m, vendite)
    
    SPGas <- 0
    Sqtmvc <- Scpr <- Sgrad <- Sccr <- Sqtint <- Sqtpsv <- Sqvdvar <- Sccvdvar <- 0
    for(i in act)
    {
      P0 <- as.numeric(vendite[i,9])
      Prif <- 0 
      fixed <- vendite[i,10]   
      if (fixed != "brent") {Prif <- as.numeric(vendite[i,10])}
      
      pc <- which(colnames(pm) == vendite[i,4])
      profm <- (as.numeric(vendite[i,5]) * pm[,pc])
      
      inizio <- split_date(vendite[i,2])
      fine <- split_date(vendite[i,3])
      inizio <- paste0("01/",inizio[2],"/", inizio[3])
      fine <- paste0("01/",fine[2], "/",fine[3])
      
      dnames2016 <- colnames(vendite[i, 11:22])
      dnames2017 <- colnames(vendite[i, 107:118])
      
      qtmvc <- cpr <- grad <- ccr <- qtint <- qtpsv <- qvdvar <- ccvdvar <- rep(0, 12)
      lg <- rep(0,12)
      
      YEAR <- split_date(m)[3]
      MONTH <- as.numeric(split_date(m)[2])
      
      if(YEAR == "2016")
      {
        qtmvc <- as.numeric(unlist(vendite[i, 11:22]))
        cpr <- as.numeric(unlist(vendite[i, 23:34]))
        grad <- as.numeric(unlist(vendite[i, 35:46]))
        ccr <- as.numeric(unlist(vendite[i, 47:58]))
        qtint <- as.numeric(unlist(vendite[i, 59:70]))
        qtpsv <- as.numeric(unlist(vendite[i, 71:82]))
        lg <- listing[1:12]
        
        
        if(vendite[i,8] == "S" & vendite[i,7] == "0")
        {
          ccvdvar <- as.numeric(unlist(vendite[i, 95:106]))  
        }
        
        else if(vendite[i,8] == "0" & vendite[i,7] == "S")
        {
          qvdvar <- as.numeric(unlist(vendite[i, 83:94]))
        }
      }
      else
      {
        qtmvc <- as.numeric(unlist(vendite[i, 107:118]))
        cpr <- as.numeric(unlist(vendite[i, 119:130]))
        grad <- as.numeric(unlist(vendite[i, 131:142]))
        ccr <- as.numeric(unlist(vendite[i, 143:154]))
        qtint <- as.numeric(unlist(vendite[i, 155:166]))
        qtpsv <- as.numeric(unlist(vendite[i, 167:178]))
        lg <- listing[13:24]
        
        if(vendite[i,8] == "S" & vendite[i,7] == "0")
        {
          ccvdvar <- as.numeric(unlist(vendite[i, 191:202]))
        }
        
        else if(vendite[i,8] == "0" & vendite[i,7] == "S")
        {
          qvdvar <- as.numeric(unlist(vendite[i, 179:190]))
        }
      }
      
       Sqtmvc <- Sqtmvc + profm[MONTH] * qtmvc[MONTH]/100 
       Scpr <- Scpr + profm[MONTH] * cpr[MONTH]/100  
       Sgrad <- Sgrad + profm[MONTH] * grad[MONTH]/100 
       Sccr <- Sccr + profm[MONTH] * ccr[MONTH]/100 
       Sqtint <- Sqtint + profm[MONTH] * qtint[MONTH]/100 
       Sqtpsv <- Sqtpsv + profm[MONTH] * qtpsv[MONTH]/100 
       Sqvdvar <- Sqvdvar + profm[MONTH] * qvdvar[MONTH]/100 
       Sccvdvar <- Sccvdvar + profm[MONTH] * ccvdvar[MONTH]/100
      
       if(fixed == "brent")
       {
         SPGas <- SPGas + (P0 * profm[MONTH])/100 
       }
       else
       {
         SPGas <- SPGas + ((P0 + (lg[MONTH] - Prif)) * profm[MONTH])/100
       }
    }
    
    J <- which(months == m)
    
    total_matrix[1,J] <- SPGas
    total_matrix[2,J] <- Sqtmvc
    total_matrix[3,J] <- Scpr
    total_matrix[4,J] <- Sgrad
    total_matrix[5,J] <- Sccr
    total_matrix[6,J] <- Sqtint
    total_matrix[7,J] <- Sqtpsv
    total_matrix[8,J] <- Sqvdvar
    total_matrix[9,J] <- Sccvdvar
    total_matrix[10,J] <- sum(total_matrix[1:9,J])
    print(total_matrix[,J])
    
  }
  
  TM <- data.frame(total_matrix)
  rownames(TM) <- c("PGas", "qtmvc", "cpr", "grad", "ccr", "qtint", "qtpsv", "qvdvar", "ccvdvar", "total")
  colnames(TM) <- months
  
  return(TM)
}  
    
        

