transfers <- read.csv("top-250-football-transfers-from-2000-to-2018/top250-00-19.csv", header = T)
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

# A faire
   # Que faire des NA dans Market Value ?
      # Etude de la corrélation -> remplacement par une moyenne