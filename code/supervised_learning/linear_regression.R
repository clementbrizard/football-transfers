    # Récupération des données
  transfers <- read.csv("data/clean_transfers.csv")
  
  # Plus value selon age -> inutile je pense
  scatter.smooth(x=transfers$Market_value, y=transfers$Transfer_fee, main="Plus_value ~ Age")
  linearMod <- lm(transfers$plus_value ~ transfers$Age , data = transfers)
  summary(linearMod)

  AIC(linearMod)
  BIC(linearMod)