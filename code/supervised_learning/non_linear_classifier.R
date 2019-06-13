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


## Nouvelle version

  # 1 - BayesClassifier
## on réutilise LRdata car c'est le bon dataset = vars quant sont normées et pvalue est binaire

    ##version qui ressemble à LR mais qui nous renvoie une erreur bizzare
K<-10
folds=sample(1:K,n,replace=TRUE)
CV.nv<-rep(0,10)
for(i in (1:10)){
  for(j in (1:K)){
    nb <- naiveBayes(class ~ ., data=LRdata[folds!=j,])
    test <- LRdata[folds==j,]
    
    pred <- predict(nb, newdata=test)
    perf <- table(test$class, pred)
    CV.nv[i] <- CV.nv[i] + (1-sum(diag(perf))/(nrow(test)))
  }
  CV.nv[i]<-CV.nv[i]/K
}
CV.nv <- mean(CV.nv)
CV.nv

    ## autre méthode pour la cross validation Et ca fonctionne
CVgroup <- function(k,datasize,seed){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]
  temp <- sample(n,datasize)
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x,function(x) dataseq[temp==x])
  return(cvlist)
}

cvlist = CVgroup(10,n,1)


NB <- function(n,data,cvlist){
  lerr = rep(0,n)
  for (i in 1:n){
    y = as.factor(data[,ncol(data)])
    X = data[,-ncol(data)]
    data = as.data.frame(cbind(X,y))
    train <- data[-cvlist[[i]],]  #????????????cvgroup???????????????
    test <- data[cvlist[[i]],]
    ntst = nrow(test)
    model <-naiveBayes(y ~ ., data=train)
    pred <- predict(model,newdata=test[,-ncol(test)])
    perf <- table(as.factor(test$y),pred)
    lerr[i]=1-sum(diag(perf))/ntst
  }
  return(mean(lerr))
}
resNB = NB(10,LRdata,cvlist)
resNB # avec original teams : 0.3008721, without original teams : 0.2863372


# 1 - RandomForest
p <- ncol(LRdata)
K<-10
folds=sample(1:K,n,replace=TRUE)
CV.for<-rep(0,10)
for(i in (1:10)){
  for(j in (1:K)) {
    rf<-randomForest(class~.,data=LRdata[folds!=j,],mtry=p)
    test <- LRdata[folds==j,]
    yhat.bag<-predict(rf,newdata=test,type='response')
    perf.bag <-table(test$class,yhat.bag)
    CV.for[i] <- CV.for[i] + (1-sum(diag(perf.bag))/nrow(test))
  }
  CV.for[i]<-CV.for[i]/K
}
CV.for <- mean(CV.for)