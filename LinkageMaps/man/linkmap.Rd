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

MD <- read.table(text="
Row  Name             Fett Eiweiss Laktose Asche
  1  Mensch                  3,8      1,0       7,0      0,2
  2  Orang-Utan              3,5      1,5       6,0      0,2
  3  Schimpanse              3,7      1,2       7,0      0,2
  5  Pavian                  5,0      1,6       7,3      0,3
  7  Esel                    1,4      2,0       7,4      0,5
  8  Pferd                   1,9      2,5       6,2      0,5
 13  Kamel                   5,4      3,9       5,1      0,7
 21  Rind                    3,7      3,4       4,8      0,7
 27  Ziege                   4,5      2,9       4,1      0,8
 28  Schaf                   7,4      5,5       4,8      1,0
 29  Hund                   12,9      7,9       3,1      1,2
 30  Wolf                    9,6      9,2       3,4      1,2
 32  Schakal                10,5     10,0       3,0      1,2
 36  Braunbaer               22,6      7,9       2,1      1,4
 37  Eisbaer                 33,1     10,9       0,3      1,4
 39  Biber                  11,7      8,1       2,6      1,1
 41  Ratte                  10,3      8,4       2,6      1,3
 42  Maus                   13,1      9,0       3,0      1,3
 48  Buckelwal              33,0     12,5       1,1      1,6
 49  Delphin                33,0      6,8       1,1      0,7
 
", dec=",", header=TRUE)

D <- dist(MD[3:6])
HK <- princomp(MD[3:6], cor=FALSE, scores=TRUE)
S <- sammon(D, HK$scores[,1:2])
k <- kmeans(D,center=5)
linkmap(S$points, D, cluster=k$cluster,main="Milch")

}
