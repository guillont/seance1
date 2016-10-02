---
  title: "Séance1"
author: "Mikaël Gruber & Théo Guillon"
date: "30 septembre 2016"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


On charge dans un premier temps les librairies nécessaires.

```{r, warning=FALSE}

library(ggplot2)
library(ggplot2movies)
library(data.table)
library(MASS)
library(plyr)

```

Nous allons travailler principalement sur la série de données Cars93.
Grâce à l'instruction str(), nous allons pouvoir avoir un bref descriptif des variables contenues dans ce jeu de données.


```{r Cars93}
str(Cars93)
```

Nous avons donc 93 observations, qui correspondent chacune à des voitures différentes de marques diverses, avec pour chacune d'elles 27 variables, décrivant les caractéristiques principales de chacun d'entre elles.

#Bar Plot

```{r }

data_summary <- function(data, varname, groupnames){
summary_func <- function(x, col){
c(mean = mean(x[[col]], na.rm=TRUE),
sd = sd(x[[col]], na.rm=TRUE))
}
data_sum<-ddply(data, groupnames, .fun=summary_func,
varname)
data_sum <- rename(data_sum, c("mean" = varname))
return(data_sum)
}


```

La fonction data_summary permet de récupérer la moyenne et l'écart-type d'une variable de notre choix (varname) pour chaque modalité d'une ou plusieurs variables qualitatives (groupnames).

On applique donc cette fonction aux variables Price, Cylinders et Origin, pour obtenir la table suivante : 
  
  ```{r }

datacar <- data_summary(Cars93, varname="Price", 
                        groupnames=c("Cylinders", "Origin"))
datacar

```

On affiche donc le barplot suivant : 
  
  ```{r, warning=FALSE}


p <- ggplot(datacar, aes(x=Cylinders, y=Price, fill=Origin))

p <- p +   geom_bar(stat="identity", position=position_dodge()) 

p <- p  + geom_errorbar(aes(ymin=Price-sd, ymax=Price+sd), width=.2,position=position_dodge(.9))

p + ggtitle("Barplot montrant la moyenne des prix\n en fonction du nombre de cylindres et de l'origine ")

```

On observe que les voitures ne provenant pas des USA sont en moyenne plus chères, que le prix croît avec le nombre de cylindre, et qu'une seule des voitures possède un moteur rotatif.


#Boxplot

```{r }

p <- ggplot(Cars93, aes(Type , Price))+ geom_boxplot()
p+ ggtitle("Boxplot montrant la fluctuation des prix pour chaque type de voiture ")

```

On observe que la plus grande disparité dans les prix concerne les voitures de taille moyenne. De plus, les médianes sont bien différentes selon les types de voiture. Pour les vans et les voitures de petite taille, les prix semblent assez similaires pour chacune des voitures. On note la présence de certains points abérrants.

#Violin Plot

```{r, message=FALSE, warning=FALSE}

library(Hmisc)
p <- ggplot(Cars93, aes(Type, Price))+ geom_violin(draw_quantiles=c(0.25,0.5,0.75))
p <- p + stat_summary(fun.data="mean_sdl",fun.args=list(mult=1),geom="crossbar",width=0.2) 
p+ ggtitle("Violinplot montrant la fluctuation des prix\n pour chaque type de voiture ")
```

On peut observer des comportements identiques en terme de prix pour les voitures de type Large, Compact, Midsize et Sporty, avec une concentration assez forte sous la médiane puis des points dispersés au dessus.L'étude du violin plot permet de distinguer 3 types de prix pour les vans.

# BeanPlot

```{r, message=FALSE, warning=FALSE}
library(beanplot)
beanplot(Price ~ AirBags , data=Cars93)
title("Répartition des prix par rapport à la présence de AirBags ")
```

Le beanplot est un mixte entre le violin et le box plot. Le prix augmente en fonction du nombre de airbags dans la voiture.

#Tabplot

```{r, message=FALSE, warning=FALSE}

library(tabplot)
tableplot(Cars93, select=c(Price,Rev.per.mile,Weight,Wheelbase,Length,Type),
          sortCol="Type",title="Répartition de variables en fonction du type de voiture ")

```

Le tabplot permet de visualiser de potentielles relations entre les variables, en les triant selon la valeur d'une variable, ici le type de voiture. Cela nous permet également dans notre cas de voir la proportion de chaque type. On constate que les voitures larges sont plus longues, plus lourdes et ont une valeur de Rev.per.mile plus faible.

#Parallel Coordinates

```{r, warning=FALSE}


parcoord(Cars93[,c(5,13,19,21)],col=Cars93$Origin)

par(xpd = TRUE)
legend(x = 1, y = -.2, cex = 0.7,
legend = as.character(levels(Cars93$Origin)),
fill = unique(Cars93$Origin), horiz = TRUE)


```

Une autre manière d'observer des relations entre nos variables est le parallel coordinates, qui consiste à afficher en abscisse les variables et de relier les points correspondants pour chaque observation. L'apparition de droite parallèles permet alors de suspecter une potentielle corrélation entre variables.
L'origine de la voiture ne permet pas de constituer des groupes 

#Série temporelle

```{r, warning=FALSE}


str(economics)

meanpop <- mean(economics$pop)
meanune <- mean(economics$unemploy)
sdpop <- sd(economics$pop)
sdune <- sd(economics$unemploy)
econorm <- cbind(c(1:length(economics$date)),(economics$pop - meanpop)/sdpop,(economics$unemploy - meanune)/sdune)
econorm <- data.frame(econorm)
head(econorm)
```

Après avoir normalisé les variables "pop" et "unemploy" dans le data frame econorm, on trace ces 2 distributions.

```{r}
a <- ggplot() + geom_line(data = econorm, aes(x = X1, y = X2,color="blue",linetype="dashed"))
a <- a + geom_line(data = econorm, aes(x = X1, y = X3,color="red"))
a + ggtitle("Evolution en parallèle de la population\n en rouge et du taux de chômage en bleu entre 1967 et 2015 ")
```


```{r, message=FALSE, warning=FALSE}

library(boot)
library(dygraphs)
plot(decompose(drivers))
dygraph(drivers,main="Série temporelle intéractive des accidents de voiture entre 1969 et 1984")

head((decompose(drivers))$seasonal)
x <- table((decompose(drivers))$seasonal)
pie(x,main="Coefficients de saisonnalité")
```

On effectue la décomposition saisonnière de la série drivers. Grâce à la réalisation du diagramme circulaire des coefficients de saisonnalité (camemebert pour les intimes), on en déduit que notre série est de prériodicité annuelle.

# Bubble chart

```{r, warning=FALSE}
g <- ggplot(Cars93,aes(Length, Price)) + geom_point(aes(colour=Origin,size=1.5,shape=AirBags))
g <- g + geom_hline(yintercept=25)
g <- g + geom_vline(xintercept=200)
g+ ggtitle("Bubble Chart montrant le prix d'une voiture\n en fonction de son type, son origine et son nombre d'airbags ")


```

On trace ici le prix d'une voiture en fonction de sa longueur, et on visualise également le type de voiture avec la forme des points, et l'origine avec la couleur.
On en déduit qu'au dessus d'un certain seuil(25), toutes les voitures possèdent un airbag, et qu'au dessus d'une certaine longueur(200), toutes les voitures proviennent des USA.

```{r, warning=FALSE}

g <- ggplot(Cars93, aes(Length, Price, fill = Origin))  + geom_point(aes(colour=Origin))+ facet_wrap(~ Type)
g + ggtitle("Prix en fonction de la longueur pour chaque type de voiture")

```

A l'aide de l'instruction + facet_wrap(), on peut effectuer autant de tracés que de valeurs de la variable que l'on renseigne, ici 6 pour chaque type de voiture.
On constate alors que pour les voitures compactes, celles fabriquées aux USA sont moins chères que les autres ; et que les voitures larges proviennent uniquement des USA

```{r, warning=FALSE}
library(threejs)
scatterplot3js(Cars93$Cylinders, Cars93$Price, Cars93$Horsepower,main="Représentation du prix en fonction du nombre de cylindres et de chevaux",color=c("black","steelblue","red")[unclass(Cars93$Origin)])

```

La librairie threejs nous permet d'ajouter un axe pour observer une variable supplémentaire. Ainsi, on peut représenter sur un même graphique le lien entre le prix, le nombre de cylindres et le nombre de chevaux d'une voiture. On peut également jouer sur la couleur des points pour faire apparaître une variable catégorielle.

#Diagramme circulaire

```{r, warning=FALSE}
pie(table(Cars93$AirBags), main="Répartition du nombre de Airbags pour chaque voiture")
```

Quasiment 50% des voitures de notre base n'ont qu'un seul AirBag, et ce pour le conducteur.

#Composition multi-catégorique : Airbags et type de voiture

```{r, warning=FALSE}
p <- ggplot(Cars93, aes(Type, fill=AirBags)) + geom_bar()
p + ggtitle("Proportion du nombre d'airgbags pour chaque catégorie")
```

Ce graphique nous permet de voir le nombre de voitures pour chaque type et la proportion d'airbags dans chacune de ces catégories. On constate ainsi que les petites voitures et les vans n'ont pas d'airbags pour les passagers.

#Histogramme

```{r, warning=FALSE}
hist(Cars93$Length,main="Histogramme de la longueur des voitures")
```

L'histogramme est oblique à gauche (étalé à droite), avec des longueurs comprises pour la plupart entre 170 et 200.

```{r, warning=FALSE}

d <- density(Cars93$Price)
plot(d, type="n", main="Densité des prix des voitures")
polygon(d, col="lightgray", border="gray")
```

On remarque que le graphique est lui aussi étalé à droite et que la plupart des voitures ont un prix inférieur à 30.

```{r, warning=FALSE}
g <- ggplot(Cars93, aes(Price, fill = Type)) +
geom_histogram(binwidth = 5)
g+ ggtitle("Histogramme des prix comportant le type de voiture")
```

Contrairement à la densité tracée au dessus, la segmentation étant plus large, certaines informations sont perdues, notamment le deuxième pic présent aux alentours d'un prix de 30. Mais cela nous permet d'avoir le type de voiture en fonction de la tranche de prix, allant de 5 en 5.


```{r, warning=FALSE}
g <- ggplot(Cars93, aes(Horsepower, fill = Type, colour=Type)) +
geom_density(alpha = 0.05) +  xlim(0, 250)
g+ ggtitle("Densités de Horsepower pour chaque type de voiture")
```

Ce graphique permet de combiner les deux approches précédentes. Nous pouvons remarquer que les distributions sont extrémement différentes en fonction du type de voiture. Une remarque nous saute au yeux. Toutes les voitures sportives n'ont pas forcément un nombre de chevaux élevé. 

# Densité des compositions

```{r, warning=FALSE}
g <- ggplot(Cars93, aes(Price, ..count.., fill = AirBags)) +
geom_density(position = "fill")
g +ggtitle("Proportion de airbags pour chaque prix")

```

On retrouve le fait que plus la voiture est chère, plus elle est sûre (présence de 2 airbags)
