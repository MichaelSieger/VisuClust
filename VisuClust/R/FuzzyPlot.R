FuzzyPlot <- function(X, k, Xs, clusterColors=rainbow(k), clusterSymbols=rep(21,k), labels=NULL, xlab="", ylab="", main="")
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
	
	fcContext.init <- function()
	{
		fcContext.xRange <<- max(Xs[,1])-min(Xs[,1])
		fcContext.yRange <<- max(Xs[,2])-min(Xs[,2])
		fcContext.clusterColorValues <<- col2rgb(clusterColors)
		fcContext.clustering <<- fanny(X, k)
		fcContext.n <<- dim(X)[1];
		
		
		slider(fcContext.sliderCallback, 
		sl.names="Cluster",
		sl.mins=1,
		sl.maxs=(k+1),
		sl.deltas=1, 
		sl.defaults=1,
		prompt=T,
		title="control window")
		
		fcContext.update()
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
		if(v == 1)
		{
			fcContext.updateColorsAllCluster()
		}
		else
		{
			fcContext.updateColorsSingleCluster(v-1)
		}
	}
	
	fcContext.updateSymbols <- function()
	{
		if(fcContext.getSliderValue() == 1)
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
		fcContext.colors <<- fcContext.rgbList2color(fcContext.clusterColorValues[,rep(selCluster, fcContext.n)], fcContext.clustering$membership[,selCluster])
	}
	
	fcContext.updateColorsAllCluster <- function()
	{
		strongestMemberships <- rep(NA, fcContext.n)
		for(i in 1:fcContext.n)
		{
			strongestMemberships[i] <- max(fcContext.clustering$membership[i,])
		}
		fcContext.colors <<- fcContext.rgbList2color(fcContext.clusterColorValues[,fcContext.clustering$clustering], 
		strongestMemberships
		)
	}
	
	fcContext.drawLegend <- function()
	{
		t <- rep(NA, k)
		for(i in 1:k)
		{
			t[i] <- paste("Cluster", i)
		}
		legend("topleft", t, col=clusterColors, pch=clusterSymbols, pt.bg=clusterColors)
	}
	
	fcContext.update <- function()
	{
		fcContext.updateSymbols();
		fcContext.updateColors()
		par(xpd=T, ask=F)
		plot(Xs, col=fcContext.colors, bg=fcContext.colors, pch=fcContext.symbols, xlab=xlab, ylab=ylab, main=main)
		if(length(labels) != 0)
		{
			relDist = 0.02
			text(Xs[,1]+fcContext.xRange*relDist,Xs[,2]+fcContext.yRange*relDist,labels)
		}
		fcContext.drawLegend()
	}
	
	fcContext.init()
}