\name{FuzzyPlot}
\alias{FuzzyPlot}
\title{
Displays a fuzzy plot
}
\description{
	A fuzzy plot is a 2D scatterplot that displays the result of Sammon's nonlinear mapping. Additionally a fuzzy clustering is made.
	
	If the slider is on the first position, all cluster are displayed together. 
	The color/symbol indicates the nearest crisp clustering then. 
	The color intensity displays the probability of membership to the nearest cluster.
	
	You can select a single cluster too. The color intensity and the label size (if enabled) shows the probability of membership to the selected cluster.

	
}
\usage{
	FuzzyPlot(X, k, Xs, clusterColors=rainbow(k), clusterSymbols=rep(21,k), labels=NULL, labelsize=c(0.6, 1.0), xlab="", ylab="", main="",
			enableLegend=TRUE)

}

\arguments{

\item{X}{
	A matrix with size (nPoints,nDimensions) that contains the untransformed input data.
}

\item{k}{
    	The number of clusters.
}

\item{Xs}{
    	A matrix with size (nPoints,2) that contains the projected points from Sammon's nonlinear mapping.
}

\item{clusterColors}{
	A vector of size k that contains the colors for the clusters. You should only choose colors with a very high intensity.
	see \link{colors}
}

\item{clusterSymbols}{
	A vector of size k that contains the symbols for the clusters. Not all symbols can be filled. If you want this you should use the symbols 15-20.
	see \link{par}
}

\item{labels}{
	A string-array with labels for the points.
}

\item{labelsize}{
	The size of the labels in the form c(min, max).
}

\item{xlab}{
	As described in \link{plot}.
}
\item{ylab}{
	As described in \link{plot}.
}
\item{main}{
	As described in \link{plot}.
}
\item{enableLegend}{
	Switch legend on/off	
}
}
\references{

}
\author{
Michael Sieger <michael.sieger@student.hswt.de>
Georg Ohmayer <georg.ohmayer@hswt.de>
}

\seealso{

}
\examples{

# The example data
data("MilchSmall")

library(cluster)
library(MASS)

data <- scale(MilchSmall[3:6])
D <- dist(data)
S <- sammon(D)

# A very basic example showing the default colors an symbols
FuzzyPlot(data, 6, S$points, labels=MilchSmall[,2], clusterColors=c("red", "green", "magenta", "yellow", "blue", "black"))

# Custom symbols 
\dontrun{FuzzyPlot(data, 6, S$points, clusterSymbols=c("a","b", "c","d", "e"))}

# A Black-White Plot
\dontrun{FuzzyPlot(data, 6, S$points, clusterColors=rep("black", 5), clusterSymbols=15:19)}


}
