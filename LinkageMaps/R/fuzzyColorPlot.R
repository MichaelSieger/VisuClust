fuzzyColorPlot <- function(X, k, Xs, clusterColors=rainbow(k), clusterSymbols=rep(21,k), xlab="", ylab="", main="")
{
	
	library(aplpack)
	library(cluster)
	
	fcContext.init <- function()
	{
		fcContext.clusterColorValues <<- col2rgb(clusterColors)
		fcContext.clustering <<- fanny(X, k)
		fcContext.n <<- dim(X)[1];
		
		
		slider(fcContext.sliderCallback, 
		sl.names="Cluster",
		sl.mins=1,
		sl.maxs=(k+1),
		sl.deltas=1, 
		sl.defaults=1,
		prompt=T)
		
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
	
	fcContext.update <- function()
	{
		fcContext.updateSymbols();
		fcContext.updateColors()
		par(xpd=T)
		plot(Xs, col=fcContext.colors, bg=fcContext.colors, pch=fcContext.symbols, xlab=xlab, ylab=ylab, main=main)
	}
	
	fcContext.init()
}