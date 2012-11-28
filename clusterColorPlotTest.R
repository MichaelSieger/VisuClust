library(cluster)
library(MASS)

source("~/linkmapsWorkspace/VisuClust/R/FuzzyPlot.R")
MilchSmall <- read.table("~/linkmapsWorkspace/VisuClust/data/MilchSmall.tab",dec=",",header=TRUE)

data <- scale(MilchSmall[3:6])
D <- dist(data)
S <- sammon(D)

# A very basic example showing the default colors an symbols
FuzzyPlot(data, 6, S$points, labels=MilchSmall[,2], clusterColors=c("red", "green", "magenta", "yellow", "blue", "black"))

