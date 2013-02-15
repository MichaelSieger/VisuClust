\name{LinkageMap}
\alias{LinkageMap}
\title{
Displays a Linkage Map
}
\description{
A Linkage Map is a 2D Scatter Plot that displays the result of Sammon's Nonlinear Mapping. A number of ranges (>= 1) 
for the distance values d[i,j] can be defined. Each point pair whose distance is inside of the defined range is connected 
with a line. 

The ranges are defined by thresholds t[k](1<=k<=3) in the following way 
range[1]: d[i,j] <= t[1] 
range[k]: t[k-1] < d[i,j] <= t[k] 
The ranges can be changed dynamically at runtime by changing the thresholds t[k] with sliders. 

A estimation of the density function for the distances d[i,j] (for example with kernel density estimation, see function 
\link{density}) is helpful for finding the ideal thresholds t[k] (=local minima in the density function). 
}
\usage{
LinkageMap(xSammon, dist, lineTypes=c("solid","dotted", "dashed"), lineColors=c("red","green","blue"), 
		lineWidths=c(1,1,1),labels = NULL, cluster = NULL, maxValue=0.5, legendDigits = 2, xlab = "", ylab = "", 
		main = "")
}

\arguments{
  \item{xSammon}{
    A matrix with size (nPoints,2) that contains the projected points from Sammon's Nonlinear Mapping. 
}
  \item{dist}{
   A distance matrix for the given data (to compute with \link{dist}).
}
  \item{lineTypes}{
	An array of line types. The size must match with line colors and line widths. The various line types are described 
	in \link{par}. 

}
  \item{lineColors}{
    	An array of colors. The size must match with line types and line widths. see \link{colors}
}
  \item{lineWidths}{
    	An array of line widths. The size must match with line colors and line types. see \link{par}
}
\item{labels}{
	A string-array with labels for the points (size: nPoints). 
}
\item{cluster}{
	A array containing cluster memberships of the points. The point membership will be indicated with 
	different colors. The array size must match with nPoints.
}
\item{maxValue}{
	maxValue*max(d[i,j]) is the maximum value that can be adjusted with the sliders.
}
\item{legendDigits}{
	The number of fractional digits to be displayed in the legend.
}
\item{xlab}{
	A title for the x axis (as described in plot).
}
\item{ylab}{
	A title for the y axis (as described in plot).
}
\item{main}{
	An overall title for the plot (as described in plot).
}
}
\author{
Michael Sieger <michael.sieger@student.hswt.de>	\cr
Georg Ohmayer <georg.ohmayer@hswt.de>
}
\examples{

library(MASS)

data("MilchSmall")
M <- scale(MilchSmall[3:6])

D <- dist(M)
S <- sammon(D)
K <- kmeans(D,center=5)
LinkageMap(S$points, D, cluster=K$cluster, labels=MilchSmall[,2])

}
