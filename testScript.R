library(cluster)
library(MASS)

# The example data
data("MilchSmall")
data("MilchBig")

M <- scale(MilchSmall[3:6])
D <- dist(M)
S <- sammon(D)
F <- fanny(D, 6)

FuzzyPlot(S$points, F$membership, labels=MilchSmall[,2],
clusterSymbols=16:21, labelSize=c(0.8,2),xlab="xachse",ylab="yachse",main="titel",cex=2, 
clusterColors=c("red", "green", "magenta", "yellow", "blue", "black"))

K <- kmeans(D,center=5)

LinkageMap(S$points, D, cluster=K$cluster, labels=MilchSmall[,2], maxValue=0.7, legendDigits=4, xlab="xachse",ylab="yachse",main="titel")