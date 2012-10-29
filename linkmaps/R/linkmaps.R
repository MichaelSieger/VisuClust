linkmap <- function(X, D=as.matrix(dist(X)), linetypes=c("solid","dotted"), linecolors=c("red","green"), linewidths=c(1,1),
						 labels = NULL, cluster = NULL, maxValue=0.33, legendDigits = 2, ...){	 
	# checking input
	xdim = dim(X)
	ddim = dim(D)
	llen = length(labels)
	clen = length(cluster)
	if(length(xdim) != 2 || xdim[2] != 2)
	{
		print("The dimension of X must be [n, 2]")
	}
	if(ddim[1] != ddim[2])
	{
		print("The distance matrix is not quadratic")
	}
	if(xdim[1] != ddim[1])
	{
		print("The number of points doesnt match the size of the distance matrix")
	}
	if(length(linetypes) != length(linecolors) || length(linetypes) != length(linewidths))
	{
		print("linetypes, linecolors, and linewidths doesnt contain the same number of elements")
	}
	if(llen != 0 && llen != xdim[1])
	{
		print("The dimension of the label field is wrong")
	}
	if(clen != 0 && clen != xdim[1])
	{
		print("The dimension of the cluster field is wrong")
	}
	if(maxValue <= 0 || maxValue > 1)
	{
		print("maxValue is not between 0 and 1")
	}	

	library(aplpack)

	
	#functions
	maps.format <- function(number){
		round(number, legendDigits)
	}

	maps.createLegendString <- function(v1, v2){
		paste(v1, "<= x <", v2)
	}

	maps.getSliderValues <- function(){
		sliderValues = rep(NA, maps.nLines)
		for(i in 1:maps.nLines){
			sliderValues[i] = slider(no=i)
		}
		sliderValues
	}

	maps.updateGUI <- function(){
		
		sliderValues = maps.getSliderValues()
		moved = (sliderValues != maps.s)
		for(i in 1:maps.nLines)
		{
			if(moved[i])
			{
				if(i != 1)
				{
					for(j in (i-1):maps.nLines)
					{
						if(slider(no=j) > slider(no=i))
						{
							slider(set.no.value=c(j, slider(no=i)))
						}
					}
				}
				if(i != maps.nLines)
				{
					for(j in (i+1):maps.nLines)
					{
						if(slider(no=j) < slider(no=i))
						{
							slider(set.no.value=c(j, slider(no=i)))
						}
					}
				}

				break;
			}
		}
		maps.s <<- sliderValues
	}

	maps.createCircleColors <- function(colors){
		colLen <- length(colors)
		revCol <- rep("black", colLen)
		#for(i in 1:colLen){
		#	revCol[(i+colLen/2)%%colLen] <- colors[i]
		#}
		revCol
	}

	maps.drawLegend <- function(){
		t <- rep(NA, maps.nLines)
		t[1] = maps.createLegendString(0,maps.s[1])
		for(i in 2:maps.nLines){
			t[i] = maps.createLegendString(maps.s[i-1], maps.s[i])
		}
		legend("topleft",  
				t, 
				lwd=linewidths, lty=linetypes, col=linecolors)
	}

	maps.draw <- function(){
		maps.updateGUI()
		dev.hold()
		par(xpd=T, ask=F)
		plot(X, pch=21,col=maps.circleCols[cluster], bg = maps.clusterCols[cluster], ...)
		for(j in 1:maps.n){		#foreach point
			for(i in 1:j){		#foreach point (only one direction)
			      if(i != j)
			      {
				    for(k in 1:maps.nLines){
					index = maps.nLines-k+1
					if((index == 1 && D[i,j] < maps.s[index]) || (index != 1 && D[i,j] < maps.s[index] && D[i,j] >= maps.s[index-1]))
					{
						segments(X[i, 1], X[i,2], X[j,1], X[j,2], lty=linetypes[index], col=linecolors[index], lwd=linewidths[index])
						break;
					}
				    }
			      }
			}
		}
		if(length(labels)!=0)
		{
			relDist = 0.02
			text(X[,1]+xRange*relDist,X[,2]+yRange*relDist,labels)
		}
		maps.drawLegend()
		dev.flush()
	}

	maps.sliderCallback <- function(...){
		maps.draw()
	}

	maps.createSliderNames <- function(){
		res = rep(NA, maps.nLines)
		for(i in 1:maps.nLines){
			res[i] = paste("D", i)
		}
		res
	}

	maps.init <- function()
	{
		maps.nLines <<- length(linetypes)
		maps.n <<- length(X[,1])
		if(length(cluster) == 0)
		{
			cluster <<- rep(1,maps.n)
			maps.clusterCols <<- rgb(0,0,0,0)
			nCluster = 1
		}
		else
		{
			nCluster <<- length(unique(cluster))
			maps.clusterCols <<- rainbow(nCluster)
		}
		maps.circleCols <<- maps.createCircleColors(maps.clusterCols)
		min <- min(D[D !=0])*0.9	#below smallest nonzero distance
		max <- max(D)*maxValue
		maps.s <<- rep(min, maps.nLines)

		slider(maps.sliderCallback, 
				sl.names=maps.createSliderNames(),
				sl.mins=rep(min,maps.nLines*2),
				sl.maxs=rep(max,maps.nLines),
				sl.deltas=rep((max-min)/300,maps.nLines), 
				sl.defaults=rep(0,maps.nLines),
				prompt=T,	
				title="Thresholds")
	}

	#start it
	maps.init()
}




