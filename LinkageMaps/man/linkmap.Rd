\name{linkmap}
\alias{linkmap}
\title{
Displays a linkage map
}
\description{
	A Linkage Map is a 2D scatter plot that displays the result of sammons nonlinear mapping. You define a number (possibly one) of ranges. Each point pair whose distance
	is inside of the defined range is connected with a line. You can change the ranges dynamically
	at runtime with sliders.
}
\usage{
linkmap(X, D=dist(X), linetypes=c("solid","dotted"), linecolors=c("red","green"), linewidths=c(1,1),
						 labels = NULL, cluster = NULL, maxValue=0.33, legendDigits = 2, xlab = "", ylab = "", main = "")
}

\arguments{
  \item{X}{
    A matrix with size (n,2) that contains the projected points from sammons nonlinear mapping.
}
  \item{D}{
    A distance matrix for the X argument. 
}
  \item{linetypes}{
    An array of line types. The size must match with line colors and line widths.
    The various line types are described in \link{par}
}
  \item{linecolors}{
    An array of colors. The size must match with line colors and line widths.
}
  \item{linewidths}{
    An array of line widths. The size must match with line colors and line widths.
}
  \item{labels}{
	A string-array with labels for the points
  }
  \item{cluster}{
	A array containing cluster memberships of the points. The point
	membership will be indicated with different colors. The array size
	must match with the number of points.
  }
  \item{maxValue}{
	The maximum value that can be adjusted with the sliders.
  }
  \item{legendDigits}{
    The number of fractional digits to be displayed in the legend.
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

library(MASS)

MD <- exampleSet2()

D <- dist(MD[3:6])
HK <- princomp(MD[3:6], cor=FALSE, scores=TRUE)
S <- sammon(D, HK$scores[,1:2])
k <- kmeans(D,center=5)
linkmap(S$points, D, cluster=k$cluster,main="Milch")

}
