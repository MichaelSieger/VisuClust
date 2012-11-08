\name{fuzzy_plot}
\alias{fuzzy_plot}
\title{
Displays a fuzzy plot
}
\description{
	A fuzzy plot is a 2D scatterplot that displays the result of sammons nonlinear mapping. Additionally a fuzzy clustering is made.
	
	If the slider is on the first position, all cluster are displayed together. 
	The color/symbol indicates the nearest crisp clustering then. 
	The color intensity displays the probability of membership to the nearest cluster.
	
	You can select a single cluster too. The color intensity shows the probability of membership to the selected cluster.

	
}
\usage{
	fuzzy_plot(X, k, Xs, clusterColors=rainbow(k), clusterSymbols=rep(21,k), labels=NULL, xlab="", ylab="", main="")

}

\arguments{

  \item{X}{
	A matrix with size (nPoints,nDimensions) that contains the untransformed input data.
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
	A vector of size k that contains the symbols for the clusters. Not all symbols can be filled. If you want this you should use the symbols 15-20.
}

\item{labels}{
A string-array with labels for the points
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
fuzzy_plot(Milch[3:6], 5, S$points)

# Custom symbols 
\dontrun{fuzzy_plot(Milch[3:6], 5, S$points, clusterSymbols=c("a","b", "c","d", "e"))}

# A Black-White Plot
\dontrun{fuzzy_plot(Milch[3:6], 5, S$points, clusterColors=rep("black", 5), clusterSymbols=15:19)}


}
