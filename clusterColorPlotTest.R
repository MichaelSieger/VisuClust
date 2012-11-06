library(cluster)
library(MASS)
source("~/linkmapsWorkspace/linkmaps/R/fuzzyColorPlot.R")
MD <- read.table("~/linkmapsWorkspace/Milch.Dat",dec=",",header=TRUE)
HK <- princomp(MD[3:6], cor=FALSE, scores=TRUE)
clustering = fanny(MD[3:6], 5)
S <- sammon(D, HK$scores[,1:2])
fuzzyColorPlot(MD[3:6], 5, S$points)


