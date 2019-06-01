##Sys.setlocale("LC_ALL", "UTF-8") ##--> for mac
##setwd("/Users/air/Documents/GI06/SY09/Projet/sy09")

transfers <- read.csv("data/clean_transfers.csv", header = T)

  ## AFM

# Dans notre cas, nous avons un mélange de variables qualitatives et quantitatives
# Donc une ACP ne peut pas s'appliquer. De ce fait, nous appliquons AFM (Analyse Factorielle Multiple)
# qui est dédiée aux tableaux de données où les variables sont structurées en groupes.
# Plusieurs jeux de variables (continues ou qualitatives) sont ainsi étudiées simultanément

install.packages("FactoMineR")
library(FactoMineR)
mfa <- MFA(transfers, group = rep(1,7), type = c("n","c","n","n","n","c","c"), ncp=5,
           name.group=colnames(transfers), graph=FALSE)
summary(mfa)
# Le summary nous permettra d'avoir un résumé des principaux résultats numériques. En effet, le premier tableau
# de ce résumé (Eigenvalues) comprend les valeurs propres et les valeurs d'inertie associées à chaque dimension,
# dans notre cas la première dimension récupère que 6% de l'information/intertie.
# Les tableaux qui suivent permettent de voir la qualité de la représentation et la contribution à la construction
# de chaque variable (regroupées et triées par leur nature) et chaque individu à chaque dimension.

plot(mfa, invisible = "quali", select = "contrib 5") ## avoir les 5 individus qui ont le plus contribués
# à la construction des axes
plot(mfa, choix="var") ## on remarque une corrélation entre les 2 vars Makrket_value et Transfer_fee ce qui a pu être
# prédictible

dimdesc(mfa)
# cette fonction nous donne une description automatique des dimensions factorielles.
# on aura dim1 qui est décrite par les vars quantitatives les + corrélées positivement et négativement. Puis
# par les vars quali les plus liées