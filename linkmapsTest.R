library(MASS)

source("~/linkmapsWorkspace/VisuClust/R/linkmaps.R")
MD <- read.table("~/linkmapsWorkspace/Milch.Dat",dec=",",header=TRUE)
D <- dist(MD[3:6])
HK <- princomp(MD[3:6], cor=FALSE, scores=TRUE)
S <- sammon(D, HK$scores[,1:2])
k <- kmeans(D,center=5)
text <- 1:50
LinkageMaps(S$points, D, labels=text, cluster=k$cluster,main="Milch")


