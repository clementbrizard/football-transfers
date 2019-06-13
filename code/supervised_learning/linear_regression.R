source("code/SY09/separ1.R")
source("code/SY09/separ2.R")

# Récupération des données
transfers <- read.csv("data/clean_transfers.csv")
transfers <- subset(transfers, select = -c(Team_from,Team_to,League_from,League_to))
  
X <- subset(transfers,select = -c(plus_value))
z <- transfers$plus_value
Xtest <- separ2(X,z)$Xtst
Xapp <- separ2(X,z)$Xapp
ztest <- as.factor(separ2(X,z)$ztst)
zapp <- as.factor(separ2(X,z)$zapp)

# Plus value selon age et market_value
scatter.smooth(y=transfers$plus_value,x=transfers$Age + transfers$Market_value, main="Plus_value ~ Age")
linearMod <- lm(transfers$plus_value ~ transfers$Age + transfers$Market_value, data = transfers)
summary(linearMod)

AIC(linearMod)
BIC(linearMod)


## Nouvelle version
  ## Dire la même chose quw ce qui a été écrit dans le rapport en rajoutant que la séparation en train
  ## et test a été faite en choisissant 2/3 des individus en train et 1/3 en test
  ## Que nous avons scale les données avant de les utiliser
  ## nous utilisons la fonction step pour la sélection de variables est récursive en s'appuyant 
  # sur le critère AIC (minimiser la fonction de vraisemblance) pour avoir les variables significatives
  # et donc pour éviter le overfitting.

regData <- transfers[,c(2,10,11,12)]

app <- scale(regData[1:2294,-1])
y <- regData[1:2294, 1]
app <- as.data.frame(cbind(app, y))

tst <- as.data.frame(scale(regData[2295:3440,-1]))
y.tst <- regData[2295:3440, 1]

lm.model <- step(lm(y~., data = app), direction="backward")
ypred <- predict(lm.model, newdata = tst)
mean((y.tst-ypred)^2) # l'erreur = 8.307076 , qui est vraiment pas mal !
