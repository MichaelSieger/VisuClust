# Vorbereitung (Installieren der Packages)
# install.packages("aplpack",dep=T)
install.packages("C:/Users/michi/Documents/linkmapsWorkspace/aplpack_1.2.6.zip",repos=NULL)
install.packages("C:/Users/michi/Documents/linkmapsWorkspace/linkmaps_0.1.zip",repos=NULL)
library(LinkageMaps)

# Auswertung der Milch-Daten (reduziert auf 20 Lebewesen) mit R
# Einlesen der MilchDaten MD (Pfad entsprechend abändern!)
MD <- read.table("C:/Users/michi/documents/linkmapsWorkspace/Milch3.Dat",dec=",",header=T)
MD
par(ask=TRUE)

# Berechnen der standardisierten Daten sowie
# der euklidischen Distanzen in DM (DistanzMatrix)
Dat <- MD[3:6]
sDat <- scale(Dat)
DM <- dist(sDat,method="euclidian")

# Hauptkomponenten- und Clusteranalyse
library(mva)
HK <- princomp(MD[3:6],cor=T,scores=T)
summary(HK,loadings=T)
biplot(HK)
# hierarchische Clusteranalyse (average-linkage)
AL <- hclust(DM,method="average")
AL
plot(AL)

# kmeans-Verfahren (Annahme 6 Cluster)
KM <- kmeans(DM,center=6)
KM

# PAM (Partitioning Around Medoids) aus Package Cluster
library(cluster)
Vertreter <- c(1,8,11,14,15,20)
PAM <- pam(DM,k=6,diss=T,medoids=Vertreter)
PAM
plot(PAM)

# FANNY (Fuzzy Clustering / unscharfe Gruppierung) aus Package Cluster
library(cluster)
FANNY <- fanny(DM,k=6,diss=T)
FANNY
plot(FANNY)
FANNY$silinfo

# Sammon's nichtlineare Abbldung
library(MASS)
SAM <- sammon(DM)
x <- SAM$points[,1]
y <- SAM$points[,2]
namen <- MD[,2]
plot(SAM$points)
text(x,y,namen,pos=1)

# Kerndichteschätzung der Distanzen
# (Gauß-Kerne, Kernbreite: Silverman's Daumenregel)
KDS <- density(DM,kernel="gaussian",adjust=0.25)
plot(KDS)

# Linkage Map
x <- SAM$points[,1]
y <- SAM$points[,2]
namen <- MD[,2]
plot(SAM$points)
text(x,y,namen,pos=1)
DM2 <- as.matrix(DM)
# linkmap(SAM$points, DM2, cluster=KM$cluster)
linkmap(SAM$points, DM, cluster=KM$cluster, labels=namen)
fuzzyColorPlot(Dat,6,SAM$points)







