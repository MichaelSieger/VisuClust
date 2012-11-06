# Auswertung der Milch-Daten mit R

# 1. Einlesen der MilchDaten MD (Pfad entsprechend abändern!)
MD <- read.table("d:/Ohmayer/Milch.Dat",dec=",",header=T)
MD

# 2. Deskriptive Statistik und Grafiken
summary(MD[3:6])
par(ask=TRUE)
boxplot(MD[3:6])
hist(MD$Fett)
plot(MD$Fett,MD$Eiweiss)

# 3. Korrelations- und Regressionsanalyse
par(ask=TRUE)
pairs(MD[3:6])
pairs(MD[3:6],panel=panel.smooth,cex=1.2,pch=22,bg="blue")
cor(MD[3:6])
cor.test(MD$Fett,MD$Eiweiss)
Regr <- lm(MD$Fett ~ MD$Eiweiss)
summary(Regr)

# 4. Hauptkomponenten- und Clusteranalyse

# Berechnen der euklidischen Distanzen in DM (DistanzMatrix)
DM <- dist(MD[3:6],method="euclidian")
library(mva)
HK <- princomp(MD[3:6],cor=T,scores=T)
summary(HK,loadings=T)
biplot(HK)

# hierarchische Clusteranalyse (average-linkage)
Cluster1 <- hclust(DM,method="average")
plot(Cluster1)
# kmeans-Verfahren (Annahme 5 Cluster)
Cluster2 <- kmeans(DM,center=5)
Cluster2

# Sammon's nichtlineare Abbldung
library(MASS)
SAM <- sammon(DM)
plot(SAM$points)





