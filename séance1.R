#Séance 1

library(ggplot2)
library(ggplot2movies)
library(data.table)
library(MASS)
library(plyr)
library(plotly)

#Bar Plot

head(str(Cars93))

str(economics)
#+++++++++++++++++++++++++
# Fonction pour calculer la moyenne et l'écart-type
# pour chaque groupe
#+++++++++++++++++++++++++
# data : une data frame
# varname: le nom de la colonne contenant la variable 
# d'intérêt 
# groupnames : vecteur contenant les noms des colonnes à utiliser
# comme des variables de grouping 
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

  
  datacar <- data_summary(Cars93, varname="Price", 
                      groupnames=c("Cylinders", "Origin"))
  
  datacar

p <- ggplot(datacar, aes(x=Cylinders, y=Price, fill=Origin))

p <- p +   geom_bar(stat="identity", position=position_dodge()) 

p  +geom_errorbar(aes(ymin=Price-sd, ymax=Price+sd), width=.2,position=position_dodge(.9))



#Boxplot

ggplot(Cars93, aes(Type , Price))+ geom_boxplot()


#Violin
library(Hmisc)


p <- ggplot(Cars93, aes(Type, Price))+ geom_violin(draw_quantiles=c(0.25,0.5,0.75))
p
p + stat_summary(fun.data="mean_sdl",fun.args=list(mult=1),geom="crossbar",width=0.2) 

# BeanPlot
library(beanplot)

beanplot(Price  ~Type , data=Cars93)

#Tabplot
library(tabplot)

tableplot(Cars93, select=c(Type,Rev.per.mile,Weight,Wheelbase,Length,Price),sortCol="Price")

#Parallel Coordinates

o <- Cars93[,c(5,13,19,21)]

parcoord(o,col=Cars93$Origin)


#Série temporelle

str(economics)

a <- ggplot()
a <- a + geom_line(data = econorm, aes(x = X1, y = X2,color="red",linetype="dashed"))
a
a <- a + geom_line(data = econorm, aes(x = X1, y = X3,color="blue"))
a

meanpop <- mean(economics$pop)
meanune <- mean(economics$unemploy)
sdpop <- sd(economics$pop)
sdune <- sd(economics$unemploy)
econorm <- cbind(c(1:length(economics$date)),(economics$pop - meanpop)/sdpop,(economics$unemploy - meanune)/sdune)
econorm <- data.frame(econorm)
econorm

library(boot)
library(dygraphs)
ts(econorm$X3)

plot(decompose(drivers)) 
dygraph(drivers) 

str(decompose(drivers)$seasonal)
pie(table((decompose(drivers))$seasonal))

# Bubble chart

g <- ggplot(Cars93,aes(Length, Price)) + geom_point(aes(colour=Origin,size=1.5,shape=AirBags))
g <- g + geom_hline(yintercept=25)
g <- g + geom_vline(xintercept=200)
g

g <- ggplot(Cars93, aes(Weight, Price, fill = Origin))  + geom_point(aes(colour=Origin,size=1.5))+ facet_wrap(~ Type)

library(threejs)
scatterplot3js(Cars93$Cylinders, Cars93$Price, Cars93$Horsepower,color=c("black","steelblue","red")[unclass(Cars93$Origin)])

#Camembert pour Air bags ou les categories de voiture
  
  pie(table(Cars93$AirBags))
pie(table(Cars93$Passengers))

#Multiple: Airbags ettype de graphique
ggplot(Cars93, aes(Type, fill=AirBags)) + geom_bar()

#Waterfall chart il y a un trucqui marche pas je sais pas pourquoi ... 
str(economics)
head(economics)
data=data.frame(label=seq(1,12,1),x=economics$unemploy[1:12])
data
df=data.frame(emploi=c(data$x[1],diff(data$x),data$x[nrow(data)]))
df
df$s=sign(df$emploi)
df$s
df$end=cumsum(df$emploi)

df$end[nrow(df)]=2948
df$end
df$start=c(2948,df$end[-nrow(df)])
df$start
data$label

df$label=c(data$label,"final")
df$label
df$id=seq_along(df$emploi)
df$id


ggplot(df, aes(label, fill = as.factor(s))) +
geom_rect(aes(x = label, xmin = id - 0.475, xmax = id + 0.475, ymin = end, ymax = start)) +
 scale_fill_manual(values=c("red","blue"))

#Sunburst 
install.packages("sunburst")
devtools::install_github("timelyportfolio/sunburstR")
library(sunburstR)
library(data.table)
d1=data.table(cut=gsub("-","_",as.character(diamonds$cut)),
              clarity=gsub("-","_",as.character(diamonds$clarity)),
              color=gsub("-","_",as.character(diamonds$color)),f=1)

d1$label=paste(d1$cut,d1$clarity,d1$color,sep="-")
agg=aggregate(f~label,data=d1,sum)
sb=sunburst(agg,legend = list(w=200,h=20))
sb

#Histogramme
str(Cars93)
hist(Cars93$Length)

#DensitÃ©

d <- density(Cars93$Price)
plot(d, type="n", main="DensitÃ©")
polygon(d, col="lightgray", border="gray")
rug(iris[,1], col="red")

#Donne les differentes categories de voiture par rapport au prix : 
Cars93$Price

ggplot(Cars93, aes(Price, fill = Type)) +
  geom_histogram(binwidth = 5)

#Plusieurs densitÃ©s: donne la repartition du nombre de chevaux par rapport aux types de graphiques

ggplot(Cars93, aes(Horsepower, fill = Type, colour=Type)) +
  geom_density(alpha = 0.05) +
  xlim(0, 200)
