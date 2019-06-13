source("code/SY09/separ1.R")
source("code/SY09/separ2.R")
library(nnet)
library(Hmisc)

# Récupération des données
transfers <- read.csv("data/clean_transfers.csv")
transfers <- subset(transfers, select = -c(Team_from,Team_to,League_from,League_to))

# Découpage de la variable plus_value en facteurs

# Découpage négatif / positif
plus_value_min <- min(transfers$plus_value)
plus_value_max <- max(transfers$plus_value)
plus_value_range <- plus_value_max - plus_value_min
cuts <- seq(plus_value_min,plus_value_max,by = round(plus_value_range/nblevels))

transfers$plus_value <- cut(transfers$plus_value, breaks = c(plus_value_min,0,plus_value_max))

X <- subset(transfers,select = -c(plus_value))
z <- transfers$plus_value
Xtest <- separ2(X,z)$Xtst
Xapp <- separ2(X,z)$Xapp
ztest <- as.factor(separ2(X,z)$ztst)
zapp <- as.factor(separ2(X,z)$zapp)
Eapp <- cbind(zapp,Xapp)

# Entraînement du modèle

model <- multinom(zapp~., data=Eapp, MaxNWts =5000)
predictions <- predict(model,Xtest)
erreur = ztest[ztest != predictions]
taux_erreur = length(erreur)/length(ztest)
