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

regData <- dataset[,c(2,8,9,10)]

app <- scale(regData[1:2294,-4])
y <- regData[1:2294, 4]
app <- as.data.frame(cbind(app, y))

tst <- as.data.frame(scale(regData[2295:3440,-4]))
y.tst <- regData[2295:3440, 4]

lm.model <- step(lm(y~., data = app), direction="backward")
ypred <- predict(lm.model, newdata = tst)
mean((y.tst-ypred)^2) # l'erreur = 1.285385e+13 , super petit c'est bizarre car c'est TROP BIEN comme erreur


  ## avec cv : 

norm <- as.data.frame(apply(regData[,-c(4)], 2, scale))
class <- regData$plus_value
regData <- as.data.frame(cbind(norm, class = class))


p <- ncol(regData)
n <- nrow(regData)
K<-10
folds=sample(1:K,n,replace=TRUE)
cv.lm<-rep(0,10)
for(i in (1:10)){
  for(j in (1:K)){
    lm.model <- step(lm(class~., data = regData[folds!=j,]), direction="backward")
    test <- regData[folds==j,]
    
    ypred <- predict(lm.model, newdata = test)
    cv.lm[i] <- cv.lm[i] + mean((test[,c(p)]-ypred)^2)
  }
  cv.lm[i]<-cv.lm[i]/K
}
cv.lm <- mean(cv.lm)  
cv.lm # avec step : 1.474589e-15 --> très petit, sans step : 1.809001e-15 pas si grand que ça
