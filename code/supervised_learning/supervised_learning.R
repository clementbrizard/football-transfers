# Récupération des données
transfers <- read.csv("data/clean_transfers.csv")

# Division en classes

# Calcul du nombre de niveaux optimal, selon la règle de Sturges
nblevels = nclass.Sturges(transfers$Transfer_fee)

# Création des facteurs
transfers$Transfer_fee <- cut(transfers$Transfer_fee, breaks = nblevels)