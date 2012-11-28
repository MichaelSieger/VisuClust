\name{LinkageMap}
\alias{LinkageMap}
\title{
Displays a Linkage Map
}
\description{
	A Linkage Map is a 2D scatter plot that displays the result of Sammon's nonlinear mapping. 
	You define a number of ranges (>= 1) for the distance values d[i,j]. 
	Each point pair whose distance is inside of the defined range is connected with a line. 
	You can change the ranges dynamically at runtime  by changing the 
	thresholds t[k] with sliders.

	The ranges are defined by thresholds t[k](1<=k<=n) in the following way \cr
	\bold{range[1]: d[i,j] <= t[1]}		\cr
	\bold{range[k]: t[k-1] < d[i,j] <= t[k]}
}
\usage{
LinkageMap(X, D=dist(X), linetypes=c("solid","dotted"), linecolors=c("red","green"), linewidths=c(1,1),
						 labels = NULL, cluster = NULL, maxValue=0.5, legendDigits = 2, xlab = "", ylab = "", main = "")
}

\arguments{
  \item{X}{
    A matrix with size (n,2) that contains the projected points from Sammon's nonlinear mapping.
}
  \item{D}{
    A distance matrix for the X argument. 
}
  \item{linetypes}{
    An array of line types. The size must match with line colors and line widths.
    The various line types are described in \link{par}
}
  \item{linecolors}{
    An array of colors. The size must match with line types and line widths.
}
  \item{linewidths}{
    An array of line widths. The size must match with line colors and line types.
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
Georg Ohmayer <georg.ohmayer@hswt.de>
}

\seealso{

}
\examples{

library(MASS)

data("MilchSmall")
MD <- scale(MilchSmall[3:6])

D <- dist(MD)
S <- sammon(D)
k <- kmeans(D,center=5)
LinkageMap(S$points, D, cluster=k$cluster, labels=MilchSmall[,2])

}
