---
  title: "S�ance1"
author: "Mika�l Gruber & Th�o Guillon"
date: "30 septembre 2016"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


On charge dans un premier temps les librairies n�cessaires.

```{r, warning=FALSE}

library(ggplot2)
library(ggplot2movies)
library(data.table)
library(MASS)
library(plyr)

```

Nous allons travailler principalement sur la s�rie de donn�es Cars93.
Gr�ce � l'instruction str(), nous allons pouvoir avoir un bref descriptif des variables contenues dans ce jeu de donn�es.


```{r Cars93}
str(Cars93)
```

Nous avons donc 93 observations, qui correspondent chacune � des voitures diff�rentes de marques diverses, avec pour chacune d'elles 27 variables, d�crivant les caract�ristiques principales de chacun d'entre elles.

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

La fonction data_summary permet de r�cup�rer la moyenne et l'�cart-type d'une variable de notre choix (varname) pour chaque modalit� d'une ou plusieurs variables qualitatives (groupnames).

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

On observe que les voitures ne provenant pas des USA sont en moyenne plus ch�res, que le prix cro�t avec le nombre de cylindre, et qu'une seule des voitures poss�de un moteur rotatif.


#Boxplot

```{r }

p <- ggplot(Cars93, aes(Type , Price))+ geom_boxplot()
p+ ggtitle("Boxplot montrant la fluctuation des prix pour chaque type de voiture ")

```

On observe que la plus grande disparit� dans les prix concerne les voitures de taille moyenne. De plus, les m�dianes sont bien diff�rentes selon les types de voiture. Pour les vans et les voitures de petite taille, les prix semblent assez similaires pour chacune des voitures. On note la pr�sence de certains points ab�rrants.

#Violin Plot

```{r, message=FALSE, warning=FALSE}

library(Hmisc)
p <- ggplot(Cars93, aes(Type, Price))+ geom_violin(draw_quantiles=c(0.25,0.5,0.75))
p <- p + stat_summary(fun.data="mean_sdl",fun.args=list(mult=1),geom="crossbar",width=0.2) 
p+ ggtitle("Violinplot montrant la fluctuation des prix\n pour chaque type de voiture ")
```

On peut observer des comportements identiques en terme de prix pour les voitures de type Large, Compact, Midsize et Sporty, avec une concentration assez forte sous la m�diane puis des points dispers�s au dessus.L'�tude du violin plot permet de distinguer 3 types de prix pour les vans.

# BeanPlot

```{r, message=FALSE, warning=FALSE}
library(beanplot)
beanplot(Price ~ AirBags , data=Cars93)
title("R�partition des prix par rapport � la pr�sence de AirBags ")
```

Le beanplot est un mixte entre le violin et le box plot. Le prix augmente en fonction du nombre de airbags dans la voiture.

#Tabplot

```{r, message=FALSE, warning=FALSE}

library(tabplot)
tableplot(Cars93, select=c(Price,Rev.per.mile,Weight,Wheelbase,Length,Type),
          sortCol="Type",title="R�partition de variables en fonction du type de voiture ")

```

Le tabplot permet de visualiser de potentielles relations entre les variables, en les triant selon la valeur d'une variable, ici le type de voiture. Cela nous permet �galement dans notre cas de voir la proportion de chaque type. On constate que les voitures larges sont plus longues, plus lourdes et ont une valeur de Rev.per.mile plus faible.

#Parallel Coordinates

```{r, warning=FALSE}


parcoord(Cars93[,c(5,13,19,21)],col=Cars93$Origin)

par(xpd = TRUE)
legend(x = 1, y = -.2, cex = 0.7,
legend = as.character(levels(Cars93$Origin)),
fill = unique(Cars93$Origin), horiz = TRUE)


```

Une autre mani�re d'observer des relations entre nos variables est le parallel coordinates, qui consiste � afficher en abscisse les variables et de relier les points correspondants pour chaque observation. L'apparition de droite parall�les permet alors de suspecter une potentielle corr�lation entre variables.
L'origine de la voiture ne permet pas de constituer des groupes 

#S�rie temporelle

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

Apr�s avoir normalis� les variables "pop" et "unemploy" dans le data frame econorm, on trace ces 2 distributions.

```{r}
a <- ggplot() + geom_line(data = econorm, aes(x = X1, y = X2,color="blue",linetype="dashed"))
a <- a + geom_line(data = econorm, aes(x = X1, y = X3,color="red"))
a + ggtitle("Evolution en parall�le de la population\n en rouge et du taux de ch�mage en bleu entre 1967 et 2015 ")
```


```{r, message=FALSE, warning=FALSE}

library(boot)
library(dygraphs)
plot(decompose(drivers))
dygraph(drivers,main="S�rie temporelle int�ractive des accidents de voiture entre 1969 et 1984")

head((decompose(drivers))$seasonal)
x <- table((decompose(drivers))$seasonal)
pie(x,main="Coefficients de saisonnalit�")
```

On effectue la d�composition saisonni�re de la s�rie drivers. Gr�ce � la r�alisation du diagramme circulaire des coefficients de saisonnalit� (camemebert pour les intimes), on en d�duit que notre s�rie est de pr�riodicit� annuelle.

# Bubble chart

```{r, warning=FALSE}
g <- ggplot(Cars93,aes(Length, Price)) + geom_point(aes(colour=Origin,size=1.5,shape=AirBags))
g <- g + geom_hline(yintercept=25)
g <- g + geom_vline(xintercept=200)
g+ ggtitle("Bubble Chart montrant le prix d'une voiture\n en fonction de son type, son origine et son nombre d'airbags ")


```

On trace ici le prix d'une voiture en fonction de sa longueur, et on visualise �galement le type de voiture avec la forme des points, et l'origine avec la couleur.
On en d�duit qu'au dessus d'un certain seuil(25), toutes les voitures poss�dent un airbag, et qu'au dessus d'une certaine longueur(200), toutes les voitures proviennent des USA.

```{r, warning=FALSE}

g <- ggplot(Cars93, aes(Length, Price, fill = Origin))  + geom_point(aes(colour=Origin))+ facet_wrap(~ Type)
g + ggtitle("Prix en fonction de la longueur pour chaque type de voiture")

```

A l'aide de l'instruction + facet_wrap(), on peut effectuer autant de trac�s que de valeurs de la variable que l'on renseigne, ici 6 pour chaque type de voiture.
On constate alors que pour les voitures compactes, celles fabriqu�es aux USA sont moins ch�res que les autres ; et que les voitures larges proviennent uniquement des USA

```{r, warning=FALSE}
library(threejs)
scatterplot3js(Cars93$Cylinders, Cars93$Price, Cars93$Horsepower,main="Repr�sentation du prix en fonction du nombre de cylindres et de chevaux",color=c("black","steelblue","red")[unclass(Cars93$Origin)])

```

La librairie threejs nous permet d'ajouter un axe pour observer une variable suppl�mentaire. Ainsi, on peut repr�senter sur un m�me graphique le lien entre le prix, le nombre de cylindres et le nombre de chevaux d'une voiture. On peut �galement jouer sur la couleur des points pour faire appara�tre une variable cat�gorielle.

#Diagramme circulaire

```{r, warning=FALSE}
pie(table(Cars93$AirBags), main="R�partition du nombre de Airbags pour chaque voiture")
```

Quasiment 50% des voitures de notre base n'ont qu'un seul AirBag, et ce pour le conducteur.

#Composition multi-cat�gorique : Airbags et type de voiture

```{r, warning=FALSE}
p <- ggplot(Cars93, aes(Type, fill=AirBags)) + geom_bar()
p + ggtitle("Proportion du nombre d'airgbags pour chaque cat�gorie")
```

Ce graphique nous permet de voir le nombre de voitures pour chaque type et la proportion d'airbags dans chacune de ces cat�gories. On constate ainsi que les petites voitures et les vans n'ont pas d'airbags pour les passagers.

#Histogramme

```{r, warning=FALSE}
hist(Cars93$Length,main="Histogramme de la longueur des voitures")
```

L'histogramme est oblique � gauche (�tal� � droite), avec des longueurs comprises pour la plupart entre 170 et 200.

```{r, warning=FALSE}

d <- density(Cars93$Price)
plot(d, type="n", main="Densit� des prix des voitures")
polygon(d, col="lightgray", border="gray")
```

On remarque que le graphique est lui aussi �tal� � droite et que la plupart des voitures ont un prix inf�rieur � 30.

```{r, warning=FALSE}
g <- ggplot(Cars93, aes(Price, fill = Type)) +
geom_histogram(binwidth = 5)
g+ ggtitle("Histogramme des prix comportant le type de voiture")
```

Contrairement � la densit� trac�e au dessus, la segmentation �tant plus large, certaines informations sont perdues, notamment le deuxi�me pic pr�sent aux alentours d'un prix de 30. Mais cela nous permet d'avoir le type de voiture en fonction de la tranche de prix, allant de 5 en 5.


```{r, warning=FALSE}
g <- ggplot(Cars93, aes(Horsepower, fill = Type, colour=Type)) +
geom_density(alpha = 0.05) +  xlim(0, 250)
g+ ggtitle("Densit�s de Horsepower pour chaque type de voiture")
```

Ce graphique permet de combiner les deux approches pr�c�dentes. Nous pouvons remarquer que les distributions sont extr�mement diff�rentes en fonction du type de voiture. Une remarque nous saute au yeux. Toutes les voitures sportives n'ont pas forc�ment un nombre de chevaux �lev�. 

# Densit� des compositions

```{r, warning=FALSE}
g <- ggplot(Cars93, aes(Price, ..count.., fill = AirBags)) +
geom_density(position = "fill")
g +ggtitle("Proportion de airbags pour chaque prix")

```

On retrouve le fait que plus la voiture est ch�re, plus elle est s�re (pr�sence de 2 airbags)
