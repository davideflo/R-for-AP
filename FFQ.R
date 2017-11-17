##### Generalized functions for Quoting

library(shiny)
library(plyr)
library(dplyr)
library(readxl)
library(feather)
library(lubridate)
library(data.table)
library(xlsx)

################################################################################################
AnalyzePeriod <- function(s, y1, y2)
  #### @PARAM: y1 and y2 are the two consecutive years of which we need to compute the quoting
{
  sy1 <- y1 - 2000
  sy2 <- y2 - 2000
  mesi <- c("Gennaio_", "Febbraio_", "Marzo_", "Aprile_", "Maggio_", "Giugno_", 
            "Luglio_", "Agosto_", "Settembre_", "Ottobre_", "Novembre_", "Dicembre_")
  
  mesi17 <- paste0(mesi, sy1)
  mesi18 <- paste0(mesi, sy2)
  
  splitted <- strsplit(s, "-")
  YEAR <- 0
  
  if(length(splitted[[1]]) > 1)
  {
    #### weeks
    split1 <- strsplit(splitted$Periodo[1], "/")
    split2 <- strsplit(splitted$Periodo[2], "/")
    from <- paste0(as.numeric(split1[[1]][1]),'-',as.numeric(split1[[1]][2]),'-',as.numeric(split1[[1]][3]))
    to <- paste0(as.numeric(split2[[1]][1]),'-',as.numeric(split2[[1]][2]),'-',as.numeric(split2[[1]][3]))
    YEAR <- as.numeric(split2[[1]][1])
  }
  else if(length(splitted[[1]]) == 1)
  {
    if(tolower(s) %in% tolower(mesi17))
    {
      YEAR <- y1
      mese <- ifelse(which(tolower(s) == tolower(mesi17)) < 10, paste0('0',which(tolower(s) == tolower(mesi17))), which(tolower(s) == tolower(mesi17)))
      from <- paste0(YEAR, '-', mese, '-01')
      to <- paste0(YEAR, '-', mese, '-', days_in_month(as.Date(from)))
    }
    else if(tolower(s) %in% tolower(mesi18))
    {
      YEAR <- y2
      mese <- ifelse(which(tolower(s) == tolower(mesi18)) < 10, paste0('0',which(tolower(s) == tolower(mesi18))), which(tolower(s) == tolower(mesi18)))
      from <- paste0(YEAR, '-', mese, '-01')
      to <- paste0(YEAR, '-', mese, '-', days_in_month(as.Date(from)))
    }
    else if(s == paste0('Q1_',sy1))
    {
      YEAR <- y1
      from <- paste0(YEAR,'-01-01')
      to <- paste0(YEAR,'-03-31')
    }
    else if(s == paste0('Q2_',sy1))
    {
      YEAR <- y1
      from <- paste0(YEAR,'-04-01')
      to <- paste0(YEAR,'-06-30')
    }
    else if(s == paste0('Q3_',sy1))
    {
      YEAR <- y1
      from <- paste0(YEAR,'-07-01')
      to <- paste0(YEAR,'-09-30')
    }
    else if(s == paste0('Q4_',sy1))
    {
      YEAR <- y1
      from <- paste0(YEAR,'-10-01')
      to <- paste0(YEAR,'-12-31')
    }
    else if(s == paste0('Q1_',sy2))
    {
      YEAR <- y2
      from <- paste0(YEAR,'-01-01')
      to <- paste0(YEAR,'-03-31')
    }
    else if(s == paste0('Q2_',sy2))
    {
      YEAR <- y2
      from <- paste0(YEAR,'-04-01')
      to <- paste0(YEAR,'-06-30')
    }
    else if(s == paste0('Q3_',sy2))
    {
      YEAR <- y2
      from <- paste0(YEAR,'-07-01')
      to <- paste0(YEAR,'-09-30')
    }
    else if(s == paste0('Q4_',sy2))
    {
      YEAR <- y2
      from <- paste0(YEAR,'-10-01')
      to <- paste0(YEAR,'-12-31')
    }
    else
    {### BSL annuale
      YEAR <- y2
      from <- paste0(YEAR, '-01-01')
      to <- paste0(YEAR, '-12-31')
    }
  }
  return(list(from = from, to = to))
}
###################################################################
EstraiAnno <- function(ft)
{
  splitted <- strsplit(ft, "-")
  return(as.numeric(splitted[[1]][1]))
}
###################################################################
GetQ <- function(m)
{
  if(m <= 3) return("Q1")
  else if(m > 3 & m <= 6) return("Q2")
  else if(m > 6 & m <= 9) return("Q3")
  else return("Q4")
}
###################################################################
