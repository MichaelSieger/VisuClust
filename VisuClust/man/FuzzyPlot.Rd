\name{FuzzyPlot}
\alias{FuzzyPlot}
\title{
Displays a Fuzzy Plot
}
\description{
A Fuzzy Plot is a 2D Scatter Plot that displays the result of Sammon's Nonlinear Mapping together with a fuzzy 
clustering. The values u[i,k] (i=1,...,nPoints; k=1,...,nPoints), which states the membership of the i.th 
observation for the cluster k, can be computed for example with \link{fanny} from the package cluster.

If the slider is on the last position, all cluster are displayed together. The color and the symbol indicates the 
nearest crisp clustering then (max u[i,k], k=1,...,nClusters). The color intensity displays the probability of 
membership to the nearest cluster. 

A single cluster can be selected too by setting the slider in position 1 to nClusters. The color intensity and the 
label size (if labels are set) shows the probability of membership to the selected cluster. 	
}
\usage{
FuzzyPlot(xSammon, probs, clusterColors=rainbow(dim(probs)[2]), clusterSymbols=rep(21,dim(probs)[2]), labels=NULL, 
		labelSize=c(0.6, 1.0), xlab="", ylab="", main="", enableLegend=TRUE, cex=c(0.7, 1.4))
}

\arguments{

\item{xSammon}{
	A matrix with size (nPoints,2) that contains the projected points from Sammon's Nonlinear Mapping.  
	See \link{sammon} (package MASS).
}

\item{probs}{
	A matrix with size (nPoints, nClusters) that contains the probabilities of membership for each cluster. 
	See the membership argument of \link{fanny} (package \link{cluster}).
}

\item{clusterColors}{
	A vector of size nClusters that contains the colors for the clusters. One should only choose colors with a 
	very high intensity. See \link{colors}.
}

\item{clusterSymbols}{
	A vector of size nClusters that contains the symbols for the clusters. Not all symbols can be filled. 
	If this is wanted, one should use the symbols 15-20. See \link{points}. 
}

\item{labels}{
	A string-array with labels for the points (size: nPoints). 
}

\item{labelSize}{
	The size of the labels in the form c(min, max).
}

\item{xlab}{
	A title for the x axis (as described in \link{plot}). 
}
\item{ylab}{
	A title for the y axis (as described in \link{plot}). 
}
\item{main}{
	An overall title for the plot (as described in \link{plot}).
}
\item{enableLegend}{
	Switch legend on/off.
}
\item{cex}{
	Two numerical values in the form c(min,max) giving the amount by which plotting text and symbols should be magnified relative to the 
	default (as described in \link{par}).
}

}

\author{
Michael Sieger <michael.sieger@student.hswt.de>	\cr
Georg Ohmayer <georg.ohmayer@hswt.de>
}

\examples{

library(cluster)
library(MASS)

# The example data
data("MilchSmall")

M <- scale(MilchSmall[3:6])
D <- dist(M)
S <- sammon(D)
F <- fanny(D, 6)

FuzzyPlot(S$points, F$membership, labels=MilchSmall[,2], clusterColors=c("red", "green", "magenta", "yellow", "blue", "black"))


}
