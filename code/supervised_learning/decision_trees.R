library(rpart)
library(rpart.plot)
source("code/SY09/separ1.R")
source("code/SY09/separ2.R")

transfers <- read.csv("data/clean_transfers.csv")

X <- subset(transfers,select = -c(plus_value))
z <- transfers$plus_value
Xtest <- separ2(X,z)$Xtst
Xapp <- separ2(X,z)$Xapp
ztest <- as.factor(separ2(X,z)$ztst)
zapp <- as.factor(separ2(X,z)$zapp)

# Arbres de décision binaires

binary.tree <- rpart(zapp ~ Xapp$Position + Xapp$Age + Xapp$Team_from + Xapp$League_from + Xapp$Team_to + Xapp$League_to + Xapp$Season)
pruned.binary.tree <- prune(binary.tree,cp = 0.05)
