##### Take screenshot from R --> to be called from python

library(webshot)

today <- Sys.Date()

path_to_save <- paste0("C:/Users/utente/Documents/prova/", as.character(today),".png")

webshot("http://www.terna.it/DesktopModules/GraficoTerna/GraficoTernaEsteso/ctlGraficoTerna.aspx?sLang=it-IT", path_to_save)

print("FATTO")