library(rpart)
library(rpart.plot)
library(RWeka)
library(partykit)
library(C50)
library(ipred)
library(Hmisc)
source("code/SY09/separ1.R")
source("code/SY09/separ2.R")

# Récupération des données
transfers <- read.csv("data/clean_transfers.csv")

# Découpage de la variable plus_value en facteurs

# Découpage négatif / positif
plus_value_min <- min(transfers$plus_value)
plus_value_max <- max(transfers$plus_value)
plus_value_range <- plus_value_max - plus_value_min
cuts <- seq(plus_value_min,plus_value_max,by = round(plus_value_range/nblevels))

transfers$plus_value <- cut(transfers$plus_value, breaks = c(plus_value_min,0,plus_value_max))

transfers <- subset(transfers, select = -c(Team_from,Team_to,League_from,League_to))


# Séparation des sous-ensembles d'apprentissage et de test
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

# Boosted CART

# fit model
fit <- bagging(zapp~., data=Eapp)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit,Xtest)
# summarize accuracy
erreur = ztest[ztest != predictions]
taux_erreur = length(erreur)/length(ztest)

# Algo C4.5

# fit model
WOW(J48)
fit <- J48(zapp~., data=Eapp, control = Weka_control(C = 0.28))
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, Xtest)
# summarize accuracy
erreur = ztest[ztest != predictions]
taux_erreur = length(erreur)/length(ztest)

# Algo C5.0

# fit model

fit <- C5.0(x = Xapp, y = zapp,trials = 10, control = C5.0Control(CF = 0.32))
# summarize the fit
plot(fit)
# make predictions
predictions <- predict(fit, Xtest)
# summarize accuracy
table(predictions, ztest)
erreur = ztest[ztest != predictions]
taux_erreur = length(erreur)/length(ztest)
