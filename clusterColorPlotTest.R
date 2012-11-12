library(cluster)
library(MASS)
source("~/linkmapsWorkspace/VisuClust/R/FuzzyPlot.R")
MD <- read.table("~/linkmapsWorkspace/Milch.Dat",dec=",",header=TRUE)
HK <- princomp(MD[3:6], cor=FALSE, scores=TRUE)
D <- dist(MD[3:6])
S <- sammon(D, HK$scores[,1:2])
FuzzyPlot(MD[3:6], 5, S$points, labels=1:50)


