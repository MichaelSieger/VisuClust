#Author: Michael Sieger <michael.sieger@student.hswt.de>
#Project responsible: Dr. Georg Ohmayer <georg.ohmayer@hswt.de>
#Copyrights: Hochschule Weihenstephan-Triesdorf

#This file is part of the Linkage Maps package.

#The VisuClust package is free software: you can redistribute it and/or modify
#it under the terms of the GNU Lesser General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.

#The VisuClust package is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU Lesser General Public License for more details.

#You should have received a copy of the GNU Lesser General Public License
#along with the VisuClust package.  If not, see <http://www.gnu.org/licenses/>.




FuzzyPlot <- function(X, k, Xs, clusterColors=rainbow(k), clusterSymbols=rep(21,k), labels=NULL, labelsize=c(0.6, 1.0), xlab="", ylab="", main="",
			enableLegend=TRUE, cex=1.4)
{
	
	library(aplpack)
	library(cluster)
	
	#define variables to prevent them beeing global visible
	fcContext.n = 0
	fcContext.clustering = 0
	fcContext.clusterColorValues = 0
	fcContext.symbols = 0
	fcContext.colors = 0
	fcContext.xRange = 0
	fcContext.yRange = 0
	fcContext.probabilitys = 0
	
	fcContext.init <- function()
	{
		fcContext.xRange <<- max(Xs[,1])-min(Xs[,1])
		fcContext.yRange <<- max(Xs[,2])-min(Xs[,2])
		fcContext.clusterColorValues <<- col2rgb(clusterColors)
		fcContext.clustering <<- fanny(X, k)
		fcContext.n <<- dim(X)[1];
		
		
		slider(fcContext.sliderCallback, 
		sl.names=fcContext.createSliderName(),
		sl.mins=1,
		sl.maxs=(k+1),
		sl.deltas=1, 
		sl.defaults=1,
		prompt=T,
		title="control window")
		
		fcContext.update()
	}
	
	fcContext.createSliderName <- function()
	{
		paste("1-", k, " = cluster, ", k+1, " = all")
	}
	
	fcContext.sliderCallback <- function(...)
	{
		fcContext.update()
	}
	
	fcContext.getSliderValue <- function()
	{
		slider(no=1)
	}

	fcContext.updateColors <- function()
	{
		v <- fcContext.getSliderValue();
		if(v == (k+1))
		{
			fcContext.updateColorsAllCluster()
		}
		else
		{
			fcContext.updateColorsSingleCluster(v)
		}
	}
	
	fcContext.updateSymbols <- function()
	{
		if(fcContext.getSliderValue() == (k+1))
		{
			fcContext.symbols <<- clusterSymbols[fcContext.clustering$clustering]
		}
		else
		{
			fcContext.symbols <<- rep(21, k)
		}
	}
	
	#rgblist: eine [3,n] Matrix mit den Farbanteilen
	#W: eine list mit warscheinlichkeiten für die intensität der farbe
	fcContext.rgbList2color <- function(rgblist, W)
	{
		col <- rep(NA, fcContext.n)
		for(i in 1:fcContext.n)
		{
			col[i] <- rgb(rgblist[1,i]/255, rgblist[2,i]/255, rgblist[3,i]/255, W[i])
		}
		col
	}
	
	fcContext.updateColorsSingleCluster <- function(selCluster)
	{
		fcContext.colors <<- fcContext.rgbList2color(fcContext.clusterColorValues[,rep(selCluster, fcContext.n)], fcContext.probabilitys)
	}
	
	fcContext.updateColorsAllCluster <- function()
	{
		fcContext.colors <<- fcContext.rgbList2color(fcContext.clusterColorValues[,fcContext.clustering$clustering], fcContext.probabilitys)
	}
	
	fcContext.updateProbability <- function()
	{
		sel <- fcContext.getSliderValue()
		if(sel == k+1)
		{			
			fcContext.probabilitys <<- rep(NA, fcContext.n)
			for(i in 1:fcContext.n)
			{
				fcContext.probabilitys[i] <<- max(fcContext.clustering$membership[i,])
			}
		}
		else
		{
			fcContext.probabilitys <<- fcContext.clustering$membership[,sel]
		}
	}
	
	fcContext.drawLegend <- function()
	{
		t <- rep(NA, k)
		for(i in 1:k)
		{
			t[i] <- paste("Cluster", i)
		}
		psize <- par("usr")
		lsize <- legend(0,0,t, horiz=TRUE, plot=FALSE)
		legend(psize[1], psize[4]+lsize$rect$h, t, horiz=TRUE, col="black", pch=clusterSymbols, pt.bg=clusterColors)
	}
	
	fcContext.drawLabels <- function()
	{
		probrange <- labelsize[2]-labelsize[1]
		for(i in 1:fcContext.n)
		{
			text(Xs[i,1], Xs[i, 2], labels=labels[i], adj=c(1.1, 1.1), 
				cex=(probrange*fcContext.probabilitys[i] + labelsize[1])
			)
		}
	}
	
	fcContext.update <- function()
	{
		fcContext.updateProbability()
		fcContext.updateSymbols()
		fcContext.updateColors()
		dev.hold()
		par(xpd=T, ask=F)
		plot(Xs, col="black", bg=fcContext.colors, pch=fcContext.symbols, xlab=xlab, ylab=ylab, main=main, cex=cex)
		if(length(labels) != 0)
		{
			fcContext.drawLabels()
		}
		if(enableLegend)
		{
			fcContext.drawLegend()
		}
		dev.flush()
	}
	
	fcContext.init()
}