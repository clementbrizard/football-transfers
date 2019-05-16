transfers <- read.csv("../data/top250-00-19.csv", header = T)

summary(transfers)

colnames(transfers)

# Combien de variables ?
ncol(transfers)

# Combien de transferts ?
nrow(transfers)
length(levels(transfers$Season))
as.data.frame(table(transfers$Season))

modalseason <- levels(transfers$Season)
transfers$Season <- factor(transfers$Season,levels = modalseason, ordered = T)

unique(sort(transfers$League_from))

#suppression des ligues invalides
pays <- levels(transfers$League_from)[1:40]
nrow(subset(transfers, League_from %in% pays))
nrow(subset(transfers, League_to %in% pays))
transfers2 <- subset(transfers, !(League_from %in% pays))
transfers3 <- subset(transfers2, !(League_to %in% pays))
                    
is.quant <- sapply(transfers3, function(x) is.numeric(x))
transfers.quant <- transfers3[,is.quant]
boxplot(transfers.qual)
#beaucoup de valeurs atypiques pour Market_Value et Transfer_Fee

plot(transfers3$Season,transfers3$Market_value)
#pas de données avant 2004-2005
#légère croissance en moyenne mais extrema++

is.qual <- sapply(transfers3, function(x) is.factor(x))
transfers.qual <- transfers3[,is.qual]

def.par <- par(no.readonly = T)
par(mfrow = c(length(is.qual)/2,length(is.qual)/2))
for (i in 1:length(is.qual))
{
  barplot(summary(transfers3[,i]))
}

# étude de la corrélation des colonnes

for(i in 2:dim(transfers3)[2])
{
  for(j in i:dim(transfers3)[2])
  {
    chisq <- chisq.test(transfers3[,i],transfers3[,j])
    pvalue <- chisq$p.value
    if ((pvalue < 0.05) && (i != j))
    {
      print(c(colnames(transfers3)[i],colnames(transfers3)[j]))
    }
  }
}

# A faire
   # Que faire des NA dans Market Value ?
      # Etude de la corrélation -> remplacement par une moyenne