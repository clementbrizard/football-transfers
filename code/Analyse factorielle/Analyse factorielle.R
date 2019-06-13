##Sys.setlocale("LC_ALL", "UTF-8") ##--> for mac
##setwd("/Users/air/Documents/GI06/SY09/Projet/sy09")

## http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/#graph-of-variables


transfers <- read.csv("data/clean_transfers.csv", header = T)

  ## AFM

# Dans notre cas, nous avons un m??lange de variables qualitatives et quantitatives
# Donc une ACP ne peut pas s'appliquer. De ce fait, nous appliquons AFM (Analyse Factorielle Multiple)
# qui est d??di??e aux tableaux de donn??es o?? les variables sont structur??es en groupes.
# Plusieurs jeux de variables (continues ou qualitatives) sont ainsi ??tudi??es simultan??ment

install.packages("FactoMineR")
library(FactoMineR)


dataset <- transfers[,-c(3,6)]

str(dataset)
res.famd <- FAMD(dataset, graph = FALSE)
fviz_screeplot(res.famd)

var <- get_famd_var (res.famd)
var
# Graphique des variables
fviz_famd_var (res.famd, repel = TRUE)
# Contribution ?? la premi??re dimension
fviz_contrib (res.famd, "var", axes = 1)
# Contribution ?? la deuxi??me dimension
fviz_contrib (res.famd, "var", axes = 5)

ind <- get_famd_ind(res.famd)
plot(ind$contrib)


## Notes
# Nous aurions pu faire une analyse factorielle pour les vars quant avec ACP s??par??e de l analyse
# Factorielle des vars quali avec une 

### Biblio
# 1 - http://www.numdam.org/article/RSA_2002__50_4_5_0.pdf


res.hcpc <- HCPC(res.famd, graph = FALSE)
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Taille du text
          palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = "jco",           # Couleur du rectangle
          labels_track_height = 0.8      # Augment l'espace pour le texte
)

plot(res.hcpc, choice = "3D.map")


sapply(transfers, function(x) length(unique(x)))


### old version 
Age <- transfers$Age
LeagueFrom <- transfers$League_from
transfers <- transfers[,-c(2,4)]

dataset <- transfers[,c(1,3,5,4,6,7,2,8,9,10)]
mfa <- MFA(dataset, group = c(1,2,2,1,4), type = c(rep("n",4),"s"), ncp=5,
           name.group=c("Position","Teams","Leagues","Season","Player_caracteristics"),
           graph=FALSE)
summary(mfa)
# Le summary nous permettra d'avoir un r??sum?? des principaux r??sultats num??riques. En effet, le premier tableau
# de ce r??sum?? (Eigenvalues) comprend les valeurs propres et les valeurs d'inertie associ??es ?? chaque dimension,
# dans notre cas la premi??re dimension r??cup??re que 6% de l'information/intertie.
# Les tableaux qui suivent permettent de voir la qualit?? de la repr??sentation et la contribution ?? la construction
# de chaque variable (regroup??es et tri??es par leur nature) et chaque individu ?? chaque dimension.

plot(mfa, invisible = "quali", select = "contrib 5") ## avoir les 5 individus qui ont le plus contribu??s
# ?? la construction des axes
plot(mfa, choix="var") ## on remarque une corr??lation entre les 2 vars Makrket_value et Transfer_fee ce qui a pu ??tre
# pr??dictible

dimdesc(mfa)
# cette fonction nous donne une description automatique des dimensions factorielles.
# on aura dim1 qui est d??crite par les vars quantitatives les + corr??l??es positivement et n??gativement. Puis
# par les vars quali les plus li??es

install.packages("factoextra")
library("factoextra")

## The proportion of variances retained by the different dimensions (axes)
fviz_screeplot(mfa)

## extract the results for groups of variables
group <- get_mfa_var(mfa, "group")
# Coordinates of groups
head(group$coord)
# Cos2: quality of representation on the factore map
head(group$cos2)
# Contributions to the  dimensions
head(group$contrib)

# plot allows us to see the contribution of the groups in dimension 1 and 2
fviz_mfa_var(mfa, "group")

# Contribution to the first dimension
fviz_contrib(mfa, "group", axes = 1)
# Contribution to the second dimension
fviz_contrib(mfa, "group", axes = 2)



## Study quantitive vars
quanti.var <- get_mfa_var(mfa, "quanti.var")
quanti.var
# Coordinates
head(quanti.var$coord)
# Cos2: quality on the factore map
head(quanti.var$cos2)
# Contributions to the dimensions
head(quanti.var$contrib)

fviz_mfa_var(mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE)
# Contributions to dimension 1
fviz_contrib(mfa, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco")


## quali
quali.var <- get_mfa_var(mfa, "quali.var")
quali.var
# Coordinates
head(quali.var$coord)
# Cos2: quality on the factore map
head(quali.var$cos2)
# Contributions to the dimensions
head(quali.var$contrib)


fviz_mfa_var(mfa, "quali.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE)
# Contributions to dimension 1
fviz_contrib(mfa, choice = "quali.var", axes = 1, top = 20,
             palette = "jco")




### Meilleure m??thode pour analyse factorielle
###http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/76-afdm-analyse-factorielle-des-donnees-mixtes-avec-r-l-essentiel/

dataset <- dataset[,-10]
## Notre dataset est expliqu?? par 3 vars quanti et 6 vars quali

## alpha = 0.05
cor.test(dataset$Age,dataset$Transfer_fee,method="pearson") #p-value = 0.2267 < 0.05 donc  significantly correlated
cor.test(dataset$Age,dataset$Market_value,method="pearson")
cor.test(dataset$Market_value,dataset$Transfer_fee,method="pearson")

## all the vars are corr
## on applique l'ACM sur 