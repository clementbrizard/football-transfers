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






# A faire
   # Factorisation League_from et League_to (doublons, orthographe, langue)
   # Que faire des NA dans Market Value ?