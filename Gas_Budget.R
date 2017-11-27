###########################################
######## GAS Budget #######################
###########################################

source("C://Users//d_floriello//Documents//RCODE//Int_budget_gas.R")

#### Cluster dei prodotti
MakeCluster("C://Users//d_floriello//Documents//2017.11.06_Report_214.xlsx",2017)

#### Bilancio gas
MakeGASBudget("C://Users//d_floriello//Documents//DB_07-11-2017.xlsx", 2017)
  