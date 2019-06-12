# Classification automatique

## I - Introduction

  # À ce niveau, nous avons un dataset de 7 variables dont 4 sont quantitatives dont le nombre de
  # classes varient entre 4 et 19. Par conséquent, le clustering nous parait utiles dans notre cas.
  # La classification automatique s'applique majoritairement sur des variables quantitatives, elle peut
  # tout de même être appliquée sur des variables qualitatives mais cela peut ne pas produire des
  # r??sultats pertinents. Nous allons tout de m??me l'appliquer.

## II - Matrice de dissimilarit??

  # Cette matrice va nous permettre de conna??tre la diff??rence et l'??loignement des individus de notre
  # ensemble de donn??es, ce qui va nous permettre plus tard dans notre analyse de regrouper
  # les plus proches ou s??parer les plus ??loign??s

  # Notre cas est particulier ! Nous savons calculer la distance entre des points de donn??es num??riqucares
  # mais ici nous avons des donn??es cat??goriques (factors). De ce fait, nous allons utilis??
  # Gower distance.

library(cluster)
gower.dist <- daisy(transfers, metric = c("gower"))
class(gower.dist)

  # Puisque notre nettoyage de donn??es a ??t?? bien fait, nous n'avons pas eu de probl??mes au niveau de
  # cette op??raiton car nous avons d??j?? pens?? ?? s'assurer que les variables qualitatives ??taient de classe
  # factor non pas String ou autre.


## III - Clustering

  # Nous allons utilis?? l'alagorithme de la classificaiton ascendante hi??rarchique. Afin de d??cider
  # quelle approche nous allons utilis??, nous avons d??cid?? de tracer les 2 et puis de voir
  # laquelle est la plus ??quilibr??e.

## 1 - DIVISIVE [HAUT VERS LE BAS]

divisive.clust <- diana(as.matrix(gower.dist), diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")

## 2 - Agglomerative (bas vers le haut)
aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c,main = "Agglomerative, complete linkages")

  # Après un moment d'hésitation devant les plot des deux approches, nous avons décidé d'appliquer l'algorithme
  # du clustering avec le résultat des deux et trancher sur lequel est le meilleur plus tard dans l'analyse.
  # Nous avons tout de même hésité de dire que le dendrogramme de l'approche Agglomerative était plus
  # équilibré


## 3 - Décider entre les 2 approches
install.packages("fpc")
library(fpc)

  # Puisque nous ne sommes pas arrivés à trancher entre les deux approches, nous avons décidé de continuer
  # notre méthode de clustering et de comparer les résultats en appliquant les 2 approches.
  # À ce niveau nous allons créer les clusters puis les évaluer en vérifiant leur compacité et leur séparation

  # Étape 1 : fonction pour créer les clusters 

cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}

  # Étape 2 : Générer les clusters et choix de l'approche

  # Le choix du nombre de clusters a été un vrai défi puisque nous travailons avec des variables qualitatives
  # et on peut se retrouver avec des clusters qui n'ont aucun sens car la combinaison de leurs valeurs
  # est limitée. De ce fait, nous avons essayé de ne pas prendre un grand ou un très petit nombre de
  # clusters d'où notre choix final --> nombre de clusters = 7.

# avec approche divisive
stats.df.divisive <- cstats.table(gower.dist, divisive.clust, 7)
stats.df.divisive

# avec approche Agglo
stats.df.aggl <-cstats.table(gower.dist, aggl.clust.c, 7) #complete linkages looks like the most balanced approach
stats.df.aggl


  # Des deux résultats, nous pouvons remarquer qu'avec l'approche Agglomerative nous avons une 
  # disproportionnalité de la taille des clusters (test1 : clusters- 1 size = 4669.00 et Cluster- 2  size = 31.00)
  # ce qui rend les nombres d'observations dans les clusters incomparables. Ceci peut être dû au fait que
  # le jeu de données est déséquilibré d'où certains groupes d'observations peuvent l'emporter
  # sur les autres durant l'analyse. D'un autre côté, avec l'approche divisive, nous obtenons des résultats
  # plus cohérents et une répartition des sizes de clusters plus équilibrés.
  # En conclusion, nous allons continuer la méthode de clustering avec l'approche "DIVISIVE".


  # Étape 3 : Vérifier le choix du nombre des clusters

  # Pour cette partie, nous allons utliser une méthode vue en cours " Méthode du coude ". Pour cela,
  # nous allons choisir un nombre de clusters un peu plus supérieur à

library("ggplot2")
ggplot(data = data.frame(t(stats.df.divisive)),
       aes(x=cluster.number, y=within.cluster.ss)) + geom_point()+ geom_line()+
  ggtitle("Divisive clustering") + labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

  # On remarque bien que la mesure de la proximité des observations plus elle est basse, plus les
  # observations au sein des clusters sont proches.
  # Nous avons un coup distinctif = une diminution mineure de la somme des carrés quand le nombre des
  # clusters = 4 mais dans ce cas on aura une classe avec une ou deux observations seulement.


  # Conclusion : DIVISIVE clustering est le plus équilibré à notre avis tel que la taille des clusters peut
  # être comparable et on choisit 4 clusters pour cette méthode


library("ggplot2")
library("reshape2")
library("purrr")
library("dplyr")
install.packages("dendextend")
library("dendextend")

dendro <- as.dendrogram(divisive.clust)
dendro.col <- dendro %>%
  set("branches_k_color", k = 5, value =   c("darkslategray", "darkslategray4", "darkslategray3", "gold3","darkcyan")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)

ggd1 <- as.ggdend(dendro.col)
ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 5")








