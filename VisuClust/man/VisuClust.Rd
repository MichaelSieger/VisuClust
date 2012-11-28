\title{VisuClust}
\name{VisuClust}
\alias{VisuClust}
\docType{package}
\description{
	
	The VisuClust package provides functions to display the result of either disjoint 
	(every observation belongs to one cluster) or fuzzy (for each observation i there is a vector u[ic] with
	c=1,...,m, which states the membership of the i.th observation for the cluster c, where sum(u[ic]) = 1) 
	clusters.

	Disjunct clusterings can be created with known hierarchical methods, if the desired number of clusters m
	is provided. See \link{hclust} \link{kmeans} from the cluster package.

	The basis of the visualization is the method Sammon's Nonlinear Mapping (NLM) which displays all observations
	in a plane so that the distances in the multivariate space are approximated best. The NLM-results can 
	be indicated by coloring the clusters, and by drawing lines between similar pointpairs, and other features.
	  
	
}

\references{
	Will be in the GIL conference transcript 2012:
	Visualisierung von Clustern in multivariaten Daten unter Einsatz von R
		Georg Ohmayer, Michael Sieger
}

\author{
Michael Sieger <michael.sieger@student.hswt.de>
Georg Ohmayer <georg.ohmayer@hswt.de>
}
