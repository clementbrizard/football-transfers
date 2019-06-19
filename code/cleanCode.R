
transfers <- read.csv("data/clean_transfers.csv", header = T)
dataset <- transfers[,-c(3,5,7,9,13)]
dataset$plus_value[is.na(dataset$plus_value)] <- 0
str(dataset)


## I - AFM

install.packages("FactoMineR")
library(FactoMineR)

res.famd <- FAMD(dataset, graph = FALSE)
fviz_screeplot(res.famd, addlabels=TRUE) #plot famd

# Graphique des variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution aux dimensions
par(mfrow = c(2,2))
fviz_contrib(res.famd, "var", axes = 1)
fviz_contrib(res.famd, "var", axes = 2)
fviz_contrib(res.famd, "var", axes = 3)
fviz_contrib(res.famd, "var", axes = 4)
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
summary(lm.model)
ypred <- predict(lm.model, newdata = tst)
mean((y.tst-ypred)^2) # l'erreur = 2.866465e+13 , super petit c'est bizarre car c'est TROP BIEN comme erreur


  ## ++ analysis for linear reg

model <- step(lm(plus_value~., data = regData), direction="backward")
p <- length(lm.model$coefficients) - 1
yi <- regData$plus_value
yihat.lm <- fitted(lm.model)
yihat.model <- fitted(model)
mean((yi-yihat)^2) #907.5632

plot(yi, yihat, asp = 1)
abline(0, 1)
# on remarque que la majoirt?? des points s'approche beaucoup de la droite x = y donc l'erreur est tr??s faible
# entre les observations et les pr??dictions. Or il y en a aussi qui s'??loignent beaucoup


# raw residuals
rres <- resid(lm.model)
plot(yihat.lm , rres)

# Standardized residuals
rstd <- rstandard(lm.model)
plot(yihat.lm , rstd)

# Les r??sidus studentis??s = r??sidus r??duits
rstu <- rstudent(lm.model)
plot(yihat.lm , rstu)
abline(h=2,col="red")
abline(h=-2,col="red")

number_outliers <- length(which(rstu > 2 | rstu < -2))
(number_outliers/nrow(regData))*100 #--> 2.5% des observations sont des points aberrants


# QQ plot
par(mfrow = c(3, 3))
par(mfcol = c(2,2))
qqnorm(rres, asp = 1)
qqline(rres, dist = qnorm)

# QQ-plot des r??sidus standardis??s
qqnorm(rstd, asp = 1)
qqline(rstd, dist = qnorm)

# QQ-plot des r??sidus studentis??s
qqnorm(rstu, asp = 1)
qqline(rstu, dist = qnorm)

# Des diff??rents graphes de Q-Q plot, on remarque que celui des raw residus est tr??s vertical, tandis
# que les autres repr??sente une forme particuli??re donc nous ne pouvons pas conclure que 
# les r??sidus  sont  ind??pendants et identiquement distribu??s (iid) !! Une des hypoth??ses du mod??le n'est pas valide

#test de la normalit?? des r??sidus
shapiro.test(rres)$p.value # 4.676641e-47
shapiro.test(rstd)$p.value # 2.601035e-47
# on a dans les 2 tests des p-value qui est tr??s petites donc on rej??te l'hyp de normalit??


# Calcul de l???influence (hat-values, leverage,hii???)
hv <- hatvalues(lm.model)
h <- 2*(p + 1)/n
outliers_hv <- which(hv > h)
plot(yihat.lm, y, asp=1)
abline(0, 1)
points(yihat.lm[outliers_hv], y[outliers_hv], pch = 19)
# leverage
mean_leverage <- mean(hatvalues(lm.model))
stability_leverage <- ((p+1)/n >= mean_leverage) # mod??le stable
# Du graphe et de la comparaison entre la moyenne des hi et (p+1)/n, nous pouvons d??duire que la 
# r??gression est stable ## je ne sais pas quoi conclure sur la var


pts_aberrants <- which(rstu > 2 | rstu < -2)
newdata <- regData
for (k in pts_aberrants) {
  newdata <- newdata[-k,]
}


app <- scale(newdata[1:2235,-3])
y <- newdata[1:2235, 3]
app <- as.data.frame(cbind(app, y))

tst <- as.data.frame(scale(newdata[2236:3353,-3]))
y.tst <- newdata[2236:3353, 3]

lm.model <- step(lm(y~., data = app), direction="backward")
summary(lm.model)
ypred <- predict(lm.model, newdata = tst)
mean((y.tst-ypred)^2)  #  2.955722e+13 m??me en enlevant les points aberrants l erreur est toujours grande !


  ## on essaye avec dummy variables
  # dummy for position
regData <- dataset
table(regData$Position)
levels(regData$Position) <- c(1,2,3,4)
regData$Position<- as.numeric(as.character(regData$Position))

  # dummy Type_Team_from
table(regData$Type_Team_from)
levels(regData$Type_Team_from) <- c(1,2,3,4,5,6,7)
regData$Type_Team_from<- as.numeric(as.character(regData$Type_Team_from))

  # dummy Type_League_from
table(regData$Type_League_from)
levels(regData$Type_League_from) <- c(1,2,3,4,5,6)
regData$Type_League_from<- as.numeric(as.character(regData$Type_League_from))

# dummy Type_Team_to
table(regData$Type_Team_to)
levels(regData$Type_Team_to) <- c(1,2,3,4,5,6)
regData$Type_Team_to<- as.numeric(as.character(regData$Type_Team_to))

# dummy Type_League_to
table(regData$Type_League_to)
levels(regData$Type_League_to) <- c(1,2,3,4)
regData$Type_League_to<- as.numeric(as.character(regData$Type_League_to))

# dummy Season
table(regData$Season)
levels(regData$Season) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
regData$Season<- as.numeric(as.character(regData$Season))





app <- scale(regData[1:2294,-9])
y <- regData[1:2294, 9]
app <- as.data.frame(cbind(app, y))

tst <- as.data.frame(scale(regData[2295:3440,-9]))
y.tst <- regData[2295:3440, 9]

lm.model <- step(lm(y~., data = app), direction="backward")
summary(lm.model)
ypred <- predict(lm.model, newdata = tst)
mean((y.tst-ypred)^2) # 2.725123e+13

  ### 

norm <- as.data.frame(apply(regData[,-c(9)], 2, scale))
class <- regData$plus_value
regData <- as.data.frame(cbind(norm, class = class))


p <- ncol(regData)
n <- nrow(regData)
K<-10
folds=sample(1:K,n,replace=TRUE)
cv.lm<-rep(0,10)
for(i in (1:10)){
  for(j in (1:K)){
    lm.model <- lm(class ~ Position + Age + Type_Team_from + Type_League_from + Type_Team_to + Type_League_to + Season + Market_value, data = regData[folds!=j,])
    test <- regData[folds==j,]
    
    ypred <- predict(lm.model, newdata = test)
    cv.lm[i] <- cv.lm[i] + mean((test[,c(p)]-ypred)^2)
  }
  cv.lm[i]<-cv.lm[i]/K
}
cv.lm <- mean(cv.lm)  
cv.lm # 3.416861e+13


## avec cv : 

norm <- as.data.frame(apply(regData[,-c(3)], 2, scale))
class <- regData$plus_value
regData <- as.data.frame(cbind(norm, class = class))

scatter.smooth(x=regData$Market_value+regData$Market_value, y=regData$class, main="Plus_value ~ Age+Market_value")
   
shapiro.test(regData$class)
 ##
cor(regData$Age, regData$Market_value)
cor(regData$Age, regData$class)
cor(regData$class, regData$Market_value)

  ## another method to verify error

library(DAAG)
cvResults <- suppressWarnings(CVlm(data=regData, form.lm=class ~ ., m=10, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')

  ## m??thode principale
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

library(nnet)
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

library(e1071)
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

library(randomForest) 
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

library(RWeka)

levels(LRdata$class)[levels(LRdata$class) = 0] <- "Mauvaise_affaire"
levels(LRdata$class)[levels(LRdata$class) = 1] <- "Bonne_affaire"
p <- ncol(LRdata)
K<-10
folds=sample(1:K,n,replace=TRUE)
CV.j48<-rep(0,10)
for(i in (1:10)){
  for(j in (1:K)) {
    j48 <- J48(class~., data=LRdata[folds!=j,], control = Weka_control(C = 0.28))
    test <- LRdata[folds==j,]
    yhat.bag<-predict(j48,newdata=test)
    perf.bag <-table(test$class,yhat.bag)
    CV.j48[i] <- CV.j48[i] + (1-sum(diag(perf.bag))/nrow(test))
  }
  CV.j48[i]<-CV.j48[i]/K
}
CV.j48 <- mean(CV.j48)
CV.j48 # 0.2670414

  ## 6 - Boosted CART
p <- ncol(LRdata)
K<-10
folds=sample(1:K,n,replace=TRUE)
CV.bag<-rep(0,10)
for(i in (1:10)){
  for(j in (1:K)) {
    train <- LRdata[folds!=j,]
    test <- LRdata[folds==j,]
    
    bag <- bagging(class ~ ., data=train) ## tree
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



## binary tree

n <- nrow(LRdata)
K<-10
folds=sample(1:K,n,replace=TRUE)
CV.bt<-rep(0,10)
for(i in (1:10)){
  for(j in (1:K)){
    train <- LRdata[folds!=j,]
    tree <- rpart(train$class ~ . , data=train)
    
    test <- LRdata[folds==j,]
    
    yhat<-predict(tree,newdata=test,type='class')
    perf <-table(test$class,yhat)
    CV.bt[i] <- CV.bt[i]  + (1-sum(diag(perf))/nrow(test))
  }
  CV.bt[i]<-CV.bt[i]/K
}
CV.bt <- mean(CV.bt)  
CV.bt # 0.2428844




n <- nrow(LRdata)
K<-10
folds=sample(1:K,n,replace=TRUE)
CV.bt<-rep(0,10)
for(i in (1:10)){
  for(j in (1:K)){
    train <- LRdata[folds!=j,]
    tree <- rpart(train$class ~ Age+Market_value+Position+Type_Team_from+Type_League_from+Type_Team_to+ Type_League_to+Season , data=train, control = rpart.control(minsplit = 1000,cp = 0.05))
    
    test <- LRdata[folds==j,]
    
    yhat<-predict(tree,newdata=test,type='class')
    perf <-table(test$class,yhat)
    CV.bt[i] <- CV.bt[i]  + (1-sum(diag(perf))/nrow(test))
  }
  CV.bt[i]<-CV.bt[i]/K
}
CV.bt <- mean(CV.bt)  
CV.bt # sans le controll 0.2380931 , avec le controll : 0.2936062


