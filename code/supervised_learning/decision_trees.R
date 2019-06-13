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




## Nouvelle version
# Algo j48 ## non fonctionnel pour l'instant 
p <- ncol(LRdata)
K<-10
folds=sample(1:K,n,replace=TRUE)
CV.j48<-rep(0,10)
for(i in (1:10)){
  for(j in (1:K)) {
    j48 <- J48(class~., data=LRdata[folds!=j,], control = Weka_control(C = 0.28))
    test <- LRdata[folds==j,]
    yhat.bag<-predict(c50,newdata=test)
    perf.bag <-table(test$class,yhat.bag)
    CV.j48[i] <- CV.j48[i] + (1-sum(diag(perf.bag))/nrow(test))
  }
  #CV.c50[i]<-CV.c50[i]/K
}
CV.j48 <- mean(CV.j48)
CV.j48

# Algo C4.5 ## non fonctionnel pour l'instant 
    ## sans cv 
train <- LRdata[1:2294,-c(10)]
ytain <- LRdata[1:2294,c(10)]
test <- LRdata[2294:3440,-c(10)]
ytest <- LRdata[2294:3440,-c(10)]

fit <- C5.0(x = train, y = ytain,trials = 10, control = C5.0Control(CF = 0.32))
predictions <- predict(fit, test)
perf.bag <- table(predictions, ytest)
(1-sum(diag(perf.bag))/nrow(test)) # erreur sans cv : 0.6931125


    ## avec cv
p <- ncol(LRdata)
K<-10
folds=sample(1:K,n,replace=TRUE)
CV.c50<-rep(0,10)
for(i in (1:10)){
  for(j in (1:K)) {
    train <- LRdata[folds!=j,]
    c50 <- C5.0(x = train, y = train$class,trials = 10, control = C5.0Control(CF = 0.32),rules=TRUE)
    test <- LRdata[folds==j,]
    yhat.bag<-predict(c50,newdata=test)
    perf.bag <-table(test$class,yhat.bag)
    print(nrow(test))
    print(1 - sum(diag(perf.bag)))
    CV.for[i] <- CV.for[i] + ((1-sum(diag(perf.bag)))/nrow(test))
  }
  CV.c50[i]<-CV.c50[i]/K
}
CV.c50 <- mean(CV.c50)
CV.c50
