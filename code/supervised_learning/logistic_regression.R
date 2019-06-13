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



## Nouvelle version
  ## On applique le modèle de LR avec 10-fold cross validation

# on prend toutes les vars quant sauf la plus value
quantdata <- transfers[,c(2,10,11)]
# plus valu
class <- transfers[,c(12)]
    ## on set à 0 val qui sont <= 0 et à 1 les vals > 1
class[class<=0] <- 0
class[class>0] <- 1

  ## on normalise les vars quant
norm = as.data.frame(apply(quantdata, 2, scale))
  ## on remet tout comme avant mais avec les vars quant scaled et class = pvalue binaire
LRdata <- as.data.frame(cbind(cbind(norm,transfers[,-c(2,10,11,12)]), class=class))

  ## 10 cross validation for LR
##on teste avec une validation croisée par 10, k = 10, pour obtenir une estimation plus précise 
# de la performance en prévision du modèle
n <- nrow(transfers)
K<-10
folds=sample(1:K,n,replace=TRUE)
CV.lr<-rep(0,10)
for(i in (1:10)){
  for(j in (1:K)){
    fit <- multinom(class ~ ., data=LRdata[folds!=j,])
    test <- LRdata[folds==j,]
    
    pred <- predict(fit, newdata=test)
    perf <- table(test$class, pred)
    CV.lr[i] <- CV.lr[i] + (1-sum(diag(perf))/(nrow(test)))
  }
  CV.lr[i]<-CV.lr[i]/K
}
CV.lr <- mean(CV.lr)  
CV.lr ## error = 0.09728916