library(e1071)
library(Hmisc)
library(randomForest)
source("code/SY09/separ1.R")
source("code/SY09/separ2.R")

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

# Séparation des sous-ensembles d'apprentissage et de test
X <- subset(transfers,select = -c(plus_value))
z <- transfers$plus_value
Xtest <- separ2(X,z)$Xtst
Xapp <- separ2(X,z)$Xapp
ztest <- as.factor(separ2(X,z)$ztst)
zapp <- as.factor(separ2(X,z)$zapp)
Eapp <- cbind(zapp,Xapp)

# BayesClassifier

model <- naiveBayes(zapp ~ ., data = Eapp)
print(model)
predictions <- predict(model, Xtest)
#Mesure de l'erreur commise
confusion <- table(ztest,predictions)
prop.table(confusion)
erreur = ztest[ztest != predictions]
taux_erreur = length(erreur)/length(ztest)

# RandomForest

model <- randomForest(zapp ~ ., data = Eapp, ntree = 1000, na.action = na.omit)
predictions <- predict(model, Xtest)
#Mesure de l'erreur commise
confusion <- table(ztest,predictions)
prop.table(confusion)
erreur = ztest[ztest != predictions]
taux_erreur = length(erreur)/length(ztest)

# AdaBoost