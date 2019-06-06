    # Récupération des données
  transfers <- read.csv("data/clean_transfers.csv")
  
  # Division en classes
  
  # Calcul du nombre de niveaux optimal, selon la règle de Sturges
  nblevels = nclass.Sturges(transfers$Transfer_fee)
  
  # Création des facteurs
  # transfers$Transfer_fee <- cut(transfers$Transfer_fee, breaks = nblevels)
  
  # Transfer_fee
  scatter.smooth(x=transfers$Market_value, y=transfers$Transfer_fee, main="Transfer_fee ~ Market_value")
  linearMod <- lm(transfers$Transfer_fee ~ transfers$Market_value, data = transfers)
  summary(linearMod)

  AIC(linearMod)
  BIC(linearMod)