setwd("~/Documents/UTC/GI04/SY09/Projet/sy09")
transfers <- read.csv("data/top250-00-19.csv", header = T)
summary(transfers)

colnames(transfers)

# Combien de variables ?
ncol(transfers)

# Combien de transferts ?
nrow(transfers)
length(levels(transfers$Season))
as.data.frame(table(transfers$Season))

help(table)

unique(sort(transfers$League_from))

pays <- levels(transfers$League_from)[1:40]
nrow(subset(transfers, League_from %in% pays))
nrow(subset(transfers, League_to %in% pays))
transfers2 <- subset(transfers, !(League_from %in% pays))
transfers3 <- subset(transfers2, !(League_to %in% pays))

# factorisation des ligues par pays
pays <- as.vector(levels(transfers$League_from))
pays <- pays[1:40]

# A faire
   # Que faire des NA dans Market Value ?
      # Etude de la corrélation -> remplacement par une moyenne
