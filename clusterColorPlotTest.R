library(cluster)
library(MASS)

source("~/linkmapsWorkspace/VisuClust/R/FuzzyPlot.R")
MilchSmall <- read.table("~/linkmapsWorkspace/VisuClust/data/MilchSmall.tab", dec=".",header=TRUE)

data <- scale(MilchSmall[3:6])
D <- dist(data)
S <- sammon(D)
F <- fanny(data, 6)

FuzzyPlot(S$points, F$membership, labels=MilchSmall[,2], clusterColors=c("red", "green", "magenta", "yellow", "blue", "black"))

