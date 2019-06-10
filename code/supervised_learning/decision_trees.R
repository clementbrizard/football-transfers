library(rpart)
library(rpart.plot)
library(RWeka)
library(C50)
source("code/SY09/separ1.R")
source("code/SY09/separ2.R")

transfers <- read.csv("data/clean_transfers.csv")

# Division en classes

# Calcul du nombre de niveaux optimal, selon la règle de Sturges
nblevels = nclass.Sturges(transfers$plus_value)

# Création des facteurs
transfers$plus_value <- cut(transfers$plus_value, breaks = nblevels)


X <- subset(transfers,select = -c(plus_value))
z <- transfers$plus_value
Xtest <- separ2(X,z)$Xtst
Xapp <- separ2(X,z)$Xapp
ztest <- as.factor(separ2(X,z)$ztst)
zapp <- as.factor(separ2(X,z)$zapp)
Eapp <- cbind(zapp,Xapp)

# Arbres de décision binaires

#binary.tree <- rpart(zapp ~ Xapp$Position + Xapp$Age + Xapp$League_from + Xapp$League_to + Xapp$Season, control = rpart.control(minsplit = 1000,cp = 0.05))
#pruned.binary.tree <- prune(binary.tree,cp = 0.05)

# Algo C4.5

# fit model
fit <- J48(zapp~., data=Eapp)
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, Xtest)
# summarize accuracy
erreur = ztest[ztest != predictions]
taux_erreur = length(erreur)/length(ztest)

# Algo C5.0

# fit model
fit <- C5.0(zapp~., data=Eapp, trials=10)
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, Xtest)
# summarize accuracy
table(predictions, ztest)
erreur = ztest[ztest != predictions]
taux_erreur = length(erreur)/length(ztest)
