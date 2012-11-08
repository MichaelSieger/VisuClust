library(cluster)
library(MASS)
source("~/linkmapsWorkspace/LinkageMaps/R/fuzzy_plot.R")
MD <- read.table("~/linkmapsWorkspace/Milch.Dat",dec=",",header=TRUE)
HK <- princomp(MD[3:6], cor=FALSE, scores=TRUE)
D <- dist(MD[3:6])
clustering = fanny(MD[3:6], 5)
S <- sammon(D, HK$scores[,1:2])
fuzzy_plot(MD[3:6], 5, S$points, labels=1:50)


