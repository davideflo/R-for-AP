####### File Intermedio #####

source("C:/Users/t_carrubba/Documents/R_code/Funzioni_Quoting.R")


Esegui_Quoting <- function(tipo_quoting, y1, y2)
{
  list_orep <- data.table(read_excel(paste0('C:/Users/t_carrubba/Documents/shinyapp/pun_forward_', y1,'.xlsx')))
  
  ################################################################################################################
  ########### Quando esce il pun consuntivo 2018, cancella i '#' davanti alle righe 14, 17,18,19 #################
  ################################################################################################################
    
  #real <- read_excel("C:/Users/t_carrubba/Documents/shinyapp/DB_Borse_Elettriche_PER MI_17_conMacro_072017.xlsm", sheet = 2)
  df2 <- list_orep
  
  #df2 <- Assembler2(real, df2)
  #df2 <- df2[,-10]
  #colnames(df2)[10] <- "real"
  
  df8 <- data.table(read_excel(paste0('C:/Users/t_carrubba/Documents/shinyapp/pun_forward_', y2,'.xlsx')))
  
  mercato <- data.table(read_excel('C:/Users/t_carrubba/Documents/shinyapp/prova.xlsx'))
  
  mercato_Tecla <- TFileReader(y1, y2)
  
  m_old <- mean(df2$pun)
  m_old8 <- mean(df8$pun)
  
  if(tipo_quoting == "quoting completo")
  {
    
    for(i in 1:nrow(mercato))
    {
      #print(i)
      ft <- AnalyzePeriod(unlist(mercato[i,"Periodo"]), y1, y2)
      from <- ft$from
      to <- ft$to
      #print(EstraiAnno(from))
      
      if(EstraiAnno(from) == y1 & EstraiAnno(to) == y1)
      {
        df2 <- Redimensioner_pkop(df2, unlist(mercato[i,"BSL"]), unlist(mercato[i,"PK"]), from, to, "PK")
      }
      else if(EstraiAnno(from) == y2 & EstraiAnno(to) == y2)
      {
        df8 <- Redimensioner_pkop(df8, unlist(mercato[i,"BSL"]), unlist(mercato[i,"PK"]), from, to, "PK")
      }
      else
      {
        break
      }
    }
    
  }
  else if(tipo_quoting == "quoting Tecla")
  {
    for(i in 1:nrow(mercato_Tecla))
    {
      from <- as.character(mercato_Tecla$inizio[i])
      to <- as.character(mercato_Tecla$fine[i])
      if(EstraiAnno(from) == y1 & EstraiAnno(to) == y1)
      {
        df2 <- Redimensioner_pkop(df2, mercato_Tecla$BSL[i], mercato_Tecla$PK[i], from, to, "PK")
      }
      else if(EstraiAnno(from) == y2 & EstraiAnno(to) == y2)
      {
        df8 <- Redimensioner_pkop(df8, mercato_Tecla$BSL[i], mercato_Tecla$PK[i], from, to, "PK")
      }
      else
      {
        break
      }
    }
  }
  
  else
  {
    print("Quoting non trovato/previsto")
  }
  
  m7new <- round(mean(df2$pun),2)
  m8new <- round(mean(df8$pun),2)
  
  print(paste("Vecchio Baseload", y1," =", m_old, "nuovo Baseload ", y1, "=", m7new))
  print(paste("Vecchio Baseload", y2," =", m_old8, "nuovo Baseload ", y2, "=", m8new))
  
  par(mfrow = c(2,1))
  plot(df2$date,df2$pun, type = "l", col = "blue", xlab = y1, ylab = paste0("pun ",y1), main = paste0("curva oraria pun ",y1 ))
  plot(df8$date,df8$pun, type = "l", col = "red", xlab = y2, ylab = paste0("pun ",y2), main = paste0("curva oraria pun ",y2 ))
  
      
  path7 <- paste0("C:/Users/utente/Documents/prova/pun_forward_",y1, ".xlsx")   
  path8 <- paste0("C:/Users/utente/Documents/prova/pun_forward_",y2, ".xlsx")   
  
  write.xlsx(df2, path7, row.names = FALSE)
  write.xlsx(df8, path8, row.names = FALSE)
  
  print("Quoting fatto e salvato in C:/Users/t_carrubba/Documents/shinyapp/")
  
}