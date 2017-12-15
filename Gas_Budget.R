###########################################
######## GAS Budget #######################
###########################################

source("C://Users//d_floriello//Documents//RCODE//Int_budget_gas.R")

#### Cluster dei prodotti
MakeCluster("C://Users//d_floriello//Documents//2017.11.06_Report_214.xlsx",2017)

#### Bilancio gas
MakeGASBudget("Z://AREA ENERGY MANAGEMENT GAS//Davide_temp//DB_13-12-2017_2018 prova.xlsx", 2018)
  