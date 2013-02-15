library(cluster)
library(MASS)

source("./VisuClust/R/FuzzyPlot.R")
MilchSmall <- read.table("./VisuClust/data/MilchSmall.tab", dec=".",header=TRUE)

data <- scale(MilchSmall[3:6])
D <- dist(data)
S <- sammon(D)
F <- fanny(data, 6)
cex <- c(0.5, 2)

FuzzyPlot(S$points, F$membership, labels=MilchSmall[,2], clusterColors=c("red", "green", "magenta", "yellow", "blue", "black"), cex=cex)

