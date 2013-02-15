library(VisuClust)
library(cluster)
library(MASS)

data("Milch3")
#data <- Milch3[3:6]
data <- scale(Milch3[3:6])
D <- dist(data, method="euclidian")
HK <- princomp(data, col=FALSE, scores=TRUE)
#S <- sammon(D, HK$scores[,1:2])
S <- sammon(D)

LinkageMaps(S$points, D, linecolors=c("black", "black"), linewidths=c(2,1), maxValue=1)

FuzzyPlot(data, 6, S$points, clusterColors=rep("black", 6), clusterSymbols=rep(20, 6), enableLegend=FALSE)