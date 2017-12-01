
library(plyr)
library(dplyr)
library(readxl)
library(lubridate)
library(data.table)
library(xlsx)

#################################################################################
Assembler2 <- function(real, ph)
{
  rows <- which(unlist(!is.na(real[,13])))
  real <- real[rows,]
  ### comparison step
  last_date <- as.Date(ph$date[max(which(ph$real == 1))])
  mld <- max(which(ph$real == 1))
  errors <- unlist(real[(mld+1):nrow(real),13]) - ph$pun[(mld+1):nrow(real)]
  r <- (mld+1):nrow(real)
  #write.xlsx(data.frame(ph[r,1:(ncol(ph)-2)],Errors = errors), "C:/Users/utente/Documents/forward_pun_model_error/errors.xlsx", row.names = FALSE, append = TRUE)
  ### assembling step
  re <- rep(0, nrow(ph))
  
  for(i in 1:length(rows))
  {
    ph[i, "pun"] <- unlist(real[rows[i],13])
    re[i] <- 1
  }
  ph <- data.frame(ph, real = re)
  return(ph)
}
#################################################################################
Redimensioner_pkop <- function(ph, mh, mw, from, to, what)
{
  #### @BRIEF: if what == PK => mw is referring to PK
  d_f <- data_frame()
  from <- as.Date(from)
  to <- as.Date(to)
  nOP <- nrow(ph[which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK.OP` == "OP"),])
  nPK <- nrow(ph[which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK.OP` == "PK"),])
  rOP <- which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & ph$`PK.OP` == "OP")
  rPK <- which(as.Date(ph$date) >= from & as.Date(ph$date) <= to & (ph$`PK.OP` == "PK" | ph$`PK.OP` == "P"))
  M <- nOP + nPK
  
  periodpk <- ph[rPK,]
  periodop <- ph[rOP,]
  
  nPKr <- length(which(periodpk$real == 1))
  nOPr <- length(which(periodop$real == 1))
  
  if(what == "PK")  
  {
    opm <- (1/nOP)*((mh*M) - (mw*nPK))
    
    
    pbpk <- ifelse(length(periodpk$pun[periodpk$real == 1]) > 0, (1/nPK)*sum(periodpk$pun[periodpk$real == 1]), 0)
    pbop <- ifelse(length(periodop$pun[periodop$real == 1]) > 0, (1/nOP)*sum(periodop$pun[periodop$real == 1]), 0)
    pihatpk <- (mw - pbpk)/((1/nPK)*sum(periodpk$pun[periodpk$real == 0]))
    pihatop <- (opm - pbop)/((1/nOP)*sum(periodop$pun[periodop$real == 0]))
    for(i in 1:length(rPK))
    {
      if(ph[rPK[i], "real"] == 0) ph[rPK[i], "pun"] <- pihatpk * unlist(ph[rPK[i], "pun"])
    }
    for(i in 1:length(rOP))
    {
      if(ph[rOP[i], "real"] == 0) ph[rOP[i], "pun"] <- pihatop * unlist(ph[rOP[i], "pun"])
    }
  }
  else
  {
    pkm <- (1/nPK)*((mh*M) - (mw*nOP))
    
    pbpk <- ifelse(length(periodpk$pun[periodpk$real == 1]) > 0, (1/nPK)*sum(periodpk$pun[periodpk$real == 1]), 0)
    pbop <- ifelse(length(periodop$pun[periodop$real == 1]) > 0, (1/nOP)*sum(periodop$pun[periodop$real == 1]), 0)
    pihatpk <- (pkm - pbpk)/((1/nPK)*sum(periodpk$pun[periodpk$real == 0]))
    pihatop <- (mw - pbop)/((1/nOP)*sum(periodop$pun[periodop$real == 0]))
    
    for(i in 1:length(rPK))
    {
      ph[rPK[i], "pun"] <- pihatpk * unlist(ph[rPK[i], "pun"])
    }
    for(i in 1:length(rOP))
    {
      ph[rOP[i], "pun"] <- pihatop * unlist(ph[rOP[i], "pun"])
    }
  }
  
  return(ph)
}
#################################################################################

df9 <- data.table(read_excel("C://Users//utente//Documents//pun_forward_2019.xlsx"))
colnames(df9) <- c("date","Month","Day","Hour","Week.Day","Quarter","PK.OP","AEEG.181.06","pun","real")
df9 <- Redimensioner_pkop(df9, 49.75, 57.30, '2019-01-01', '2019-12-31', 'PK')

plot(df9$pun, type = 'l', col = 'salmon')
mean(df9$pun)
mean(df9$pun[which(df9$PK.OP == 'PK')])
write.xlsx(df9, "C://Users//utente//Documents//pun_forward_2019.xlsx", row.names = FALSE)
