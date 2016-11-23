library(XML)
library(dplyr)
library(readxl)
library(ggplot2)
library(zoo)


data <- xmlParse("C:/Users/utente/Documents/misure/12883450152_03728900964_201609_PDO_20161004072732_1DP1608.xml")

xml_data <- xmlToList(data)
xml_df <- xmlToDataFrame(xml_data)

length(xml_data$DatiPod$Curva)
xml_data$DatiPod$Curva$Ea

pod <- xml_data$DatiPod$Pod

df <- data_frame()

for( i in 10:30)
{
  txt <- as.character(i)
  xml_data$DatiPod$Curva$Ea$text == txt
}

data <- read_excel("C:/Users/utente/Documents/misure/a davide.xlsx")


pod <- unique(unlist(data$POD))
color <- c()
for( i in 1:nrow(data))
{
  for(j in 1:length(pod))
  {
    if(data[i,"POD"] == pod[j])
    {
      color <- c(color, j)
    }
  }
}

matplot(t(data[,6:29]), type = "l", lwd = 2, col = color)

data300 <- data[which(apply(data[,6:29],1,max) <= 350),6:29]

matplot(t(data300), type = "l", lwd = 2, col = color[which(apply(data[,6:29],1,max) <= 350)])

for( p in pod)
{
  matplot(t(data[which(data["POD"] == p),6:29]), type="l", lwd = 2, main = p)
}

for( p in pod)
{
  wp <- which(data["POD"] == p)
  sp <- c()
  for(j in 1:length(wp))
  {
    sp <- c(sp, unlist(data[wp[j],6:29]))
  }
  plot(sp, type="l", lwd = 2, main = p)
}

