\title{VisuClust}
\name{VisuClust}
\alias{VisuClust}
\docType{package}
\description{
	
The VisuClust package provides functions to display the result of clusterings, if they are either disjoint 
(every observation belongs to one cluster) or fuzzy (for each observation i and cluster k there is a value 
u[i,k](i=1,...,nPoints; k=1,...,nClusters)), which states the membership of the i.th observation for the cluster k, 
where sum(u[i,k],k=1,...,nClusters) = 1). A disjoint clustering can be created with known hierarchical methods, 
if the desired number of clusters is provided, or by using k-means-clustering (for example \link{hclust}, \link{kmeans} 
from the package cluster). A fuzzy clustering can be created for example with fanny from the package \link{cluster}.
 
The basis of the visualization is Sammon's method Nonlinear Mapping (NLM), which displays all observations in a plane so 
that the distances in the multivariate space are approximated best (see \link{sammon} from the package MASS). 
The clusters in the NLM diagram can be indicated by coloring, drawing lines between similar point pairs, and other features. 
	  
	
}

\references{
Will be published in the GIL conference transcript 2012: 
Visualisierung von Clustern in multivariaten Daten unter Einsatz von R 
Georg Ohmayer, Michael Sieger 
}

\author{
Michael Sieger <michael.sieger@student.hswt.de>	\cr
Georg Ohmayer <georg.ohmayer@hswt.de>
}
