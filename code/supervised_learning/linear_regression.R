source("code/SY09/separ1.R")
source("code/SY09/separ2.R")

# Récupération des données
transfers <- read.csv("data/clean_transfers.csv")
  
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