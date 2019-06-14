
transfers <- read.csv("data/clean_transfers.csv", header = T)
dataset <- transfers[,-c(3,5,7,9,13)]
dataset$plus_value[is.na(dataset$plus_value)] <- 0
str(dataset)


## I - AFM

install.packages("FactoMineR")
library(FactoMineR)

res.famd <- FAMD(dataset, graph = FALSE)
fviz_screeplot(res.famd) #plot famd

# Graphique des variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution ?? la premi??re dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution ?? la deuxi??me dimension
fviz_contrib(res.famd, "var", axes = 5)


  ## clustering based on AFM
res.hcpc <- HCPC(res.famd, graph = FALSE)
plot(res.hcpc, choice = "3D.map")


## II - Supervised

  # 1 - Linear regression

regData <- dataset[,-c(1,3,4,5,6,7)]

app <- scale(regData[1:2294,-3])
y <- regData[1:2294, 3]
app <- as.data.frame(cbind(app, y))

tst <- as.data.frame(scale(regData[2295:3440,-3]))
y.tst <- regData[2295:3440, 3]

lm.model <- step(lm(y~., data = app), direction="backward")
ypred <- predict(lm.model, newdata = tst)
mean((y.tst-ypred)^2) # l'erreur = 2.866465e+13 , super petit c'est bizarre car c'est TROP BIEN comme erreur


## avec cv : 

norm <- as.data.frame(apply(regData[,-c(3)], 2, scale))
class <- regData$plus_value
regData <- as.data.frame(cbind(norm, class = class))


p <- ncol(regData)
n <- nrow(regData)
K<-10
folds=sample(1:K,n,replace=TRUE)
cv.lm<-rep(0,10)
for(i in (1:10)){
  for(j in (1:K)){
    lm.model <- lm(class~., data = regData[folds!=j,])
    test <- regData[folds==j,]
    
    ypred <- predict(lm.model, newdata = test)
    cv.lm[i] <- cv.lm[i] + mean((test[,c(p)]-ypred)^2)
  }
  cv.lm[i]<-cv.lm[i]/K
}
cv.lm <- mean(cv.lm)  
cv.lm # avec step : 3.663871e+13 , sans step : 3.674823e+13


  ## 2 - LR

# plus valu
class <- regData[,c(3)]
## on set ?? 0 val qui sont <= 0 et ?? 1 les vals > 1
class[class<=0] <- 0
class[class>0] <- 1

class <- as.factor(class)

LRdata <- as.data.frame(cbind(cbind(norm,dataset[,c(1,3,4,5,6,7)]), class=class))

## 10 cross validation for LR
##on teste avec une validation crois??e par 10, k = 10, pour obtenir une estimation plus pr??cise 
# de la performance en pr??vision du mod??le
n <- nrow(LRdata)
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
CV.lr ## error : 0.2743716


  ## 3 - Bayes Classifier

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
CV.nv ## error : 0.2864167, avec l autre m??thode : 0.280814 . So pareil, on choisit l erreur qu'on veut


  ## 4 - Random Forest

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
CV.for  # 0.2484483


  ## 5 - Algo j48 || C48 ?## non fonctionnel pour l'instant car je n'arrive pas ?? avoir la library
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
  CV.c50[i]<-CV.c50[i]/K
}
CV.j48 <- mean(CV.j48)
CV.j48

  ## 6 - Boosted CART
p <- ncol(LRdata)
K<-10
folds=sample(1:K,n,replace=TRUE)
CV.bag<-rep(0,10)
for(i in (1:10)){
  for(j in (1:K)) {
    train <- LRdata[folds!=j,]
    test <- LRdata[folds==j,]
    
    bag <- bagging(class~., data=train) ## tree
    yhat <-predict(bag,newdata=test) ## predict pvalue
    perd <-table(test$class,yhat) # compare
    error <- (1-sum(diag(perd))/(nrow(test))) ## error
    CV.bag[i] <- CV.bag[i] + error
  }
  CV.bag[i]<-CV.bag[i]/K
}
CV.bag <- mean(CV.bag)
CV.bag # 0.2547597

# 7 - Algo C5,0

## sans cv 
train <- LRdata[1:2294,-c(9)]
ytrain <- LRdata[1:2294,c(9)]
test <- LRdata[2294:3440,-c(9)]
ytest <- LRdata[2294:3440,-c(9)]

fit <- C5.0(x = train, y = ytrain)
plot(fit,type="s",main="Decision Tree")
predictions <- predict(fit, test)
perf.bag <- table(predictions, ytest)
(1-sum(diag(perf.bag))/nrow(test)) # erreur sans cv : 0.7541412


## avec cv
p <- ncol(LRdata)
K<-10
folds=sample(1:K,n,replace=TRUE)
CV.c50<-rep(0,10)
for(i in (1:10)){
  for(j in (1:K)) {
    train <- LRdata[folds!=j,]
    test <- LRdata[folds==j,]
    
    c50 <- C5.0(x = train[,-c(p)], y = train$class) ## tree
    yhat <-predict(c50,newdata=test[,-c(p)]) ## predict pvalue
    perd <-table(test$class,yhat) # compare
    #error <- (1-(sum(diag(perd))/sum(perd))) ## error
    CV.c50[i] <- CV.c50[i] + (1-sum(diag(perd))/nrow(test))
  }
  CV.c50[i]<-CV.c50[i]/K
}
CV.c50 <- mean(CV.c50)
CV.c50  ## 0.2596045





