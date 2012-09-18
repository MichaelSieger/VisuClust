library(MASS)

source("/home/tamaran/linkmapsWorkspace/linkmaps/R/linkmaps.R")
MD <- read.table("/home/tamaran/linkmapsWorkspace/Milch.Dat",dec=",",header=T)
D <- as.matrix(dist(MD[3:6]))
HK <- princomp(MD[3:6], cor=F, scores=T)
S <- sammon(D, HK$scores[,1:2])
Ds <- as.matrix(dist(S$points))
linkmap(S$points, D, linecolors=c("red","green","blue"), linewidths=c(3,2,1),title="Milch")