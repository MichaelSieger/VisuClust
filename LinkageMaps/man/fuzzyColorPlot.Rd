\name{fuzzyColorPlot}
\alias{fuzzyColorPlot}
\title{
Displays a fuzzy color plot
}
\description{
	A fuzzy color plot is a 2D scatterplot. 

	The intensity of the color shows the probability of a point that he belongs to the selected cluster.
	You may switch interactivly between the clusters. 

	
}
\usage{
	fuzzyColorPlot(X, k, Xs, clusterColors=rainbow(k), clusterSymbols=rep(21,k), xlab="", ylab="", main="")
}

\arguments{

  \item{X}{
	A matrix with size (nPoints,nDimensions) that contains the input data.
}

  \item{k}{
    	The number of clusters.
}

  \item{Xs}{
    	A matrix with size (nPoints,2) that contains the projected points from sammons nonlinear mapping.
}

  \item{clusterColors}{
	A vector of size k that contains the colors for the clusters. You should only choose colors with a very high intensity.
}

  \item{clusterSymbols}{
	A vector of size k that contains the symbols for the clusters. Not all symbols can be filled. If you want them filled you should use the symbols 15-20.
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
}
\references{

}
\author{
Michael Sieger <michael.sieger@student.hswt.de>
}

\seealso{

}
\examples{

# The example data
data("Milch")

library(cluster)
library(MASS)

D = dist(Milch[3:6])
HK <- princomp(Milch[3:6], cor=FALSE, scores=TRUE)
S <- sammon(D, HK$scores[,1:2])

# A very basic example showing the default colors an symbols
fuzzyColorPlot(Milch[3:6], 5, S$points)

# Custom symbols 
\dontrun{fuzzyColorPlot(Milch[3:6], 5, S$points, clusterSymbols=c("a","b", "c","d", "e"))}

# A Black-White Plot
\dontrun{fuzzyColorPlot(Milch[3:6], 5, S$points, clusterColors=rep("black", 5), clusterSymbols=15:19)}


}
