pkgname <- "LinkageMaps"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('LinkageMaps')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("fuzzyColorPlot")
### * fuzzyColorPlot

flush(stderr()); flush(stdout())

### Name: fuzzyColorPlot
### Title: Displays a fuzzy color plot
### Aliases: fuzzyColorPlot

### ** Examples


# The example data
MD <- exampleSet1()

library(cluster)
library(MASS)

D = dist(MD[3:6])
HK <- princomp(MD[3:6], cor=FALSE, scores=TRUE)
S <- sammon(D, HK$scores[,1:2])

# A very basic example showing the default colors an symbols
fuzzyColorPlot(MD[3:6], 5, S$points)

# Custom symbols 
fuzzyColorPlot(MD[3:6], 5, S$points, clusterSymbols=c("a","b", "c","d", "e"))

# A Black-White Plot
fuzzyColorPlot(MD[3:6], 5, S$points, clusterColors=rep("black", 5), clusterSymbols=15:19)




cleanEx()
nameEx("linkmap")
### * linkmap

flush(stderr()); flush(stdout())

### Name: linkmap
### Title: Displays a linkage map
### Aliases: linkmap

### ** Examples


library(MASS)

MD <- exampleSet2()

D <- dist(MD[3:6])
HK <- princomp(MD[3:6], cor=FALSE, scores=TRUE)
S <- sammon(D, HK$scores[,1:2])
k <- kmeans(D,center=5)
linkmap(S$points, D, cluster=k$cluster,main="Milch")




### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
