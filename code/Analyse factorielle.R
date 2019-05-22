##Sys.setlocale("LC_ALL", "UTF-8") ##--> for mac
##setwd("/Users/air/Documents/GI06/SY09/Projet/sy09")

transfers <- read.csv("data/top250-00-19.csv", header = T)

    ## I - Reprise de l analyse exploratoire

## pour comprendre et utiliser les donn??es filtr??es
## --> question d'avoir des choses coh??rentes

nrow(transfers)
transfers$Season ## on a 19 saison

## on enl??ve la ligne dont l'age = 0
transfers <- transfers[transfers$Age != 0,] ## on a 4699 mnt

## on enl??ve les lignes dont league from and to are name of countries not leagues
transfers <- subset(transfers, !(League_from %in% pays | League_to %in% pays))


    ## II - Suite Analyse epxloratoire

## 1 - more details about data 

n = nrow(transfers)

## Meilleure visualisation de la distribution et la fr??quence de chaque classe des vars quant
## dans le dataset

freqPosition = table(transfers$Position) # Distribution de la var position
relFreqPosition = freqPosition/n # relative frequencies
cbind(freqPosition,relFreqPosition)


freqTeamFrom = table(transfers$Team_from)
relFreqTeamFrom = freqTeamFrom/n # relative frequencies
cbind(freqTeamFrom,relFreqTeamFrom)

freqTeamTo = table(transfers$Team_to) # Distribution de la var TeamTo
relFreqTeamTo = freqTeamTo/n # relative frequencies
cbind(freqTeamTo,relFreqTeamTo)

freqLeagueFrom = table(transfers$League_from) # Distribution de la var LeagueFrom
relFreqLeagueFrom = freqLeagueFrom/n # relative frequencies
cbind(freqLeagueFrom,relFreqLeagueFrom)

freqLeagueTo = table(transfers$League_to) # Distribution de la var LeagueTo
relFreqLeagueTo = freqLeagueTo/n # relative frequencies
cbind(freqLeagueTo,relFreqLeagueTo)
  ## for season it already been done

## we can plot the frequencies or just check them out to get an idea for example of
## which position is more reccurent or which league buys the most players

  ## Maybe do an independance test between the different qualitative variables by setting
  ## the contingency table 

  ## voir comment faire une matrice de corr??lation sur des vars qualitatives et quantitatives
  ## en m??me temps



## 2 - Analyse factorielle
  ## site-web utilis?? : http://fermin.perso.math.cnrs.fr/Files/ACP-AFM.html
  ## http://larmarange.github.io/analyse-R/analyse-des-correspondances-multiples.html
  ## http://pierre.jeandenand.free.fr/Cours/Statistiques%20avec%20Rgui.pdf

## Dans notre cas, nous avons plusieurs variables qualitatives donc il faut plut??t faire une
## ACM qu'une ACP [ACM est une extension de AFC qui est d??di??e ?? appliquer une ACP sur 2 vars quali]
## sauf que nous avons aussi 3 vars quantitatives et ACM s'applique que sur des vars qual

  # M??thode 1 : nous allons transformer les vars quant en vars qual puis utiliser ACM

newdata <- transfers

  # transformation quant --> qual
newdata$Age <- cut(transfers$Age, breaks = 10)
newdata$Transfer_fee <- cut(transfers$Transfer_fee, breaks = 10)
newdata$Market_value <- cut(transfers$Market_value, breaks = 10)

  # ACM

install.packages("ade4", dep = TRUE)
library(ade4)
acm.transf <- dudi.acm(newdata) ## fonctionne pas et ce n'est pas ?? cause de Market_value m??me si ??a contient
# des Na. j ai fait des recherches et j'ai trouv?? ??a comme explication :

  ## un des probl??mes majeurs de l'analyse est le fait que les variables prises en compte comportent un tr??s 
  ## grand nombre de modalit??s (d'o?? le faible pourcentage d'infos apport?? par ton axe principal...).
  ## Peut-??tre une premi??re piste serait-elle de faire des regroupements en sous-classes dans les variables de 
  ##ayant beaucoup de modalit??s diff??rentes ?

  ## conclusion --> faut faire du clustering (d'ailleurs c'est demand?? dans l'??nonc??
  ## --> m??thode ?? avanc??e ?? de clustering ou de discrimination)

   # M??thode 2 : L'analyse de Hill et Smith 
mix.data <- transfers
## 1 prob rencontr?? pour cette m??thode sont les na !!! du coup faut les virer
mix.data <- na.omit(mix.data) ## 

mix.transf <- dudi.mix(mix.data, nf=5)

  ## pour cette m??thode les params interessants :
    ## $cr contient les coe????cients de d??termination ou les rapports de corr??lation
    ## , $eig contient les pouvoirs de synth??se
    ## $l1 contient les variables de synth??se
    ## $co permet de positionner les variables num??riques et les cat??gories des variables qualitatives
  
scatter(mix.transf) ## repr??sentation --> j arrive pas ?? l avoir dunno why mais je pense qu'il faut faire un
## clustering puis apr??s r??-excuter cette m??thode



